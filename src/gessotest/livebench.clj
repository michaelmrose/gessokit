(ns gessotest.livebench
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [gesso.live.core :as live])
  (:import
   [java.util.concurrent ConcurrentHashMap])
  )

;; -----------------------------------------------------------------------------
;; Responses
;; -----------------------------------------------------------------------------

(defn json-response
  [data]
  {:status 200
   :headers {"content-type" "application/json; charset=utf-8"}
   :body (json/generate-string data)})

(defn html-response
  [html]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body html})

(defn text-response
  ([status-code message]
   {:status status-code
    :headers {"content-type" "text/plain; charset=utf-8"}
    :body (str message)})
  ([status-code message data]
   (text-response
    status-code
    (str message
         "\n"
         (pr-str data)))))

(defn bad-request
  [message data]
  (text-response 400 message data))

(defn not-found
  [message data]
  (text-response 404 message data))

(defn internal-error
  [message data]
  (text-response 500 message data))

(defn forbidden
  [message]
  (text-response 403 message))

;; -----------------------------------------------------------------------------
;; Token middleware
;; -----------------------------------------------------------------------------

(defn configured-token
  [ctx]
  (or (:gessotest.livebench/dev-token ctx)
      (:gessotest.load/dev-token ctx)
      (System/getenv "GESSOTEST_LOAD_TOKEN")))

(defn bearer-token
  [ctx]
  (some-> (get-in ctx [:headers "authorization"])
          (str/replace #"(?i)^Bearer\s+" "")))

(defn wrap-livebench-token
  [handler]
  (fn [ctx]
    (let [expected (configured-token ctx)
          actual   (bearer-token ctx)]
      (cond
        (str/blank? expected)
        (forbidden "GESSOTEST_LOAD_TOKEN is not configured.")

        (= expected actual)
        (handler ctx)

        :else
        (forbidden "Invalid livebench token.")))))

;; -----------------------------------------------------------------------------
;; State
;; -----------------------------------------------------------------------------

(def default-config
  {:mode :mixed
   :stores 1
   :helpers-per-store 10
   :max-active-customers-per-store 100
   :max-open-tasks-per-store 1000
   :customer-capacity-policy :reject
   :queue-full-policy :reject
   :customer-dispose-retention-ms 5000
   :fragment-ttl-ms 750
   :cache-enabled true
   :singleflight-enabled true
   :coalesce-mode :scope
   :seed 0})

(defn empty-stats
  []
  {:actions {:arrival-attempts 0
             :customers-created 0
             :arrivals-rejected-customer-capacity-full 0

             :request-attempts 0
             :requests-created 0
             :requests-rejected-no-active-customer 0
             :requests-rejected-customer-already-has-task 0
             :requests-rejected-queue-full 0

             :take-attempts 0
             :takes 0
             :takes-no-open-task 0
             :takes-no-available-helper 0

             :disposition-attempts 0
             :dispositions 0
             :dispositions-no-active-task 0}

   :events {:submitted 0
            :accepted 0
            :dropped 0
            :dispatcher-overflows 0}

   :sse {:connections-opened 0
         :connections-closed 0}

   :fragments {:requests 0
               :renders 0
               :cache-hits 0
               :cache-misses 0
               :freshness-window-hits 0
               :stale-version-hits 0
               :version-misses 0
               :singleflight-shares 0
               :singleflight-wait-count 0
               :singleflight-wait-ms-total 0
               :singleflight-wait-ms-max 0
               :singleflight-timeouts 0
               :errors 0}})

(defonce !state
  (atom {:config default-config
         :global-store-seq 0
         :stores {}
         :helpers {}
         :customers {}
         :customer-tombstones {}
         :tasks {}
         :versions {}}))

(defonce !stats
  (atom (empty-stats)))

(defonce !fragment-cache
  (atom {}))

(defonce !fragment-inflight
  (ConcurrentHashMap.))

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(defn now-ms
  []
  (System/currentTimeMillis))

(defn parse-long*
  [x default]
  (try
    (Long/parseLong (str x))
    (catch Exception _
      default)))

(defn parse-bool*
  [x default]
  (cond
    (true? x) true
    (false? x) false
    (nil? x) default
    :else
    (contains? #{"true" "1" "yes" "y" "on"}
               (str/lower-case (str x)))))

(defn parse-keyword*
  [x default]
  (if (nil? x)
    default
    (keyword (str/lower-case (name x)))))

(defn param
  [ctx k]
  (or (get-in ctx [:params k])
      (get-in ctx [:params (name k)])
      (get-in ctx [:query-params k])
      (get-in ctx [:query-params (name k)])
      (get-in ctx [:form-params k])
      (get-in ctx [:form-params (name k)])
      (get-in ctx [:path-params k])
      (get-in ctx [:path-params (name k)])))

(def valid-modes
  #{:customer-lifecycle
    :helper-personal
    :helper-queue
    :mixed})

(def valid-statuses
  #{:completed :cancelled :failed})

(defn valid-mode
  [x]
  (let [m (parse-keyword* x (:mode default-config))]
    (if (valid-modes m) m (:mode default-config))))

(defn valid-policy
  [x]
  (let [p (parse-keyword* x :reject)]
    (if (= p :reject) p :reject)))

(def valid-coalesce-modes
  #{:scope :action :unique})

(defn valid-coalesce-mode
  [x]
  (let [mode (parse-keyword* x (:coalesce-mode default-config))]
    (if (valid-coalesce-modes mode)
      mode
      (:coalesce-mode default-config))))

(defn valid-disposition-status
  [x]
  (let [s (parse-keyword* x :completed)]
    (if (valid-statuses s) s :completed)))

(defn inc-stat!
  [path]
  (swap! !stats update-in path (fnil inc 0)))

(defn add-stat!
  [path n]
  (swap! !stats update-in path (fnil + 0) n))

(defn max-stat!
  [path n]
  (swap! !stats update-in path (fnil max 0) n))

(defn state-update!
  "Atomically update !state.

   f receives old-state and returns [new-state result]."
  [f]
  (loop []
    (let [old @!state
          [new result] (f old)]
      (if (compare-and-set! !state old new)
        result
        (recur)))))

(defn store-id
  [store-idx]
  (str "store-" store-idx))

(defn helper-id
  [store-id helper-idx]
  (str "helper-" store-id "-" helper-idx))

(defn customer-id
  [store-id customer-seq]
  (str "customer-" store-id "-" customer-seq))

(defn task-id
  [store-id task-seq]
  (str "task-" store-id "-" task-seq))

(defn remove-id
  [coll id]
  (vec (remove #(= id %) coll)))

(defn scope-key
  [{:keys [topic id]}]
  [topic id])

(defn scope-version
  [scope]
  (get-in @!state [:versions (scope-key scope)] 0))

(defn bump-scope-version!
  [scope]
  (swap! !state update-in [:versions (scope-key scope)] (fnil inc 0)))

;; -----------------------------------------------------------------------------
;; Config/world setup
;; -----------------------------------------------------------------------------

(defn config-from-ctx
  [ctx]
  {:mode (valid-mode (or (param ctx :mode) (:mode default-config)))

   :stores
   (max 1 (parse-long* (or (param ctx :stores)
                           (:stores default-config))
                       (:stores default-config)))

   :helpers-per-store
   (max 1 (parse-long* (or (param ctx :helpers-per-store)
                           (:helpers-per-store default-config))
                       (:helpers-per-store default-config)))

   :max-active-customers-per-store
   (max 1 (parse-long* (or (param ctx :max-active-customers-per-store)
                           (:max-active-customers-per-store default-config))
                       (:max-active-customers-per-store default-config)))

   :max-open-tasks-per-store
   (max 1 (parse-long* (or (param ctx :max-open-tasks-per-store)
                           (:max-open-tasks-per-store default-config))
                       (:max-open-tasks-per-store default-config)))

   :customer-capacity-policy
   (valid-policy (or (param ctx :customer-capacity-policy)
                     (:customer-capacity-policy default-config)))

   :queue-full-policy
   (valid-policy (or (param ctx :queue-full-policy)
                     (:queue-full-policy default-config)))

   :customer-dispose-retention-ms
   (max 0 (parse-long* (or (param ctx :customer-dispose-retention-ms)
                           (:customer-dispose-retention-ms default-config))
                       (:customer-dispose-retention-ms default-config)))

   :fragment-ttl-ms
   (max 0 (parse-long* (or (param ctx :fragment-ttl-ms)
                           (:fragment-ttl-ms default-config))
                       (:fragment-ttl-ms default-config)))

   :cache-enabled
   (parse-bool* (or (param ctx :cache-enabled)
                    (:cache-enabled default-config))
                (:cache-enabled default-config))

   :singleflight-enabled
   (parse-bool* (or (param ctx :singleflight-enabled)
                    (:singleflight-enabled default-config))
                (:singleflight-enabled default-config))

   :coalesce-mode
   (valid-coalesce-mode (or (param ctx :coalesce-mode)
                            (:coalesce-mode default-config)))

   :seed
   (parse-long* (or (param ctx :seed)
                    (:seed default-config))
                (:seed default-config))})

(defn build-world
  [{:keys [stores helpers-per-store] :as cfg}]
  (let [store-ids (mapv store-id (range stores))]
    {:config cfg
     :global-store-seq 0
     :versions {}
     :customers {}
     :customer-tombstones {}
     :tasks {}

     :stores
     (into {}
           (for [sid store-ids]
             [sid {:store/id sid
                   :store/helper-ids (mapv #(helper-id sid %) (range helpers-per-store))
                   :store/active-customer-ids []
                   :store/open-task-ids []
                   :store/active-task-ids []
                   :store/completed-task-ids []
                   :store/customer-seq 0
                   :store/task-seq 0}]))

     :helpers
     (into {}
           (for [sid store-ids
                 hidx (range helpers-per-store)
                 :let [hid (helper-id sid hidx)]]
             [hid {:helper/id hid
                   :helper/store-id sid
                   :helper/active-task-id nil}]))}))

(defn reset-stats! []
  (clojure.core/reset! !stats (empty-stats)))

(defn reset-fragments! []
  (clojure.core/reset! !fragment-cache {})
  (.clear ^ConcurrentHashMap !fragment-inflight))

(defn reset-route!
  [ctx]
  (let [cfg (config-from-ctx ctx)]
    (clojure.core/reset! !state (build-world cfg))
    (reset-stats!)
    (reset-fragments!)
    (json-response
     {:status :ok
      :config cfg
      :derived {:stores (:stores cfg)
                :helpers (* (:stores cfg) (:helpers-per-store cfg))
                :max-active-customers (* (:stores cfg)
                                         (:max-active-customers-per-store cfg))}})))

;; -----------------------------------------------------------------------------
;; Scopes/live
;; -----------------------------------------------------------------------------

(defn helper-scope
  [helper-id]
  {:topic :humanhelp/helper
   :id helper-id})

(defn customer-scope
  [customer-id]
  {:topic :humanhelp/customer
   :id customer-id})

(defn store-queue-scope
  [store-id]
  {:topic :humanhelp/store-queue
   :id store-id})

(defn live-system
  [ctx]
  (:gesso.live/system ctx))

(defn queue-full?
  [e]
  (or (= "gesso.live dispatcher queue is full." (ex-message e))
      (= :gesso.live.dispatch/queue-full (:gesso.live/error (ex-data e)))
      (some-> (ex-message e)
              (str/includes? "dispatcher queue is full"))))

(defn livebench-coalesce-key
  "Return the dispatcher coalescing key for one dirty scope.

   :scope is the production-realistic default: one pending wakeup per fragment
   scope. Multiple domain changes for the same dirty scope may collapse into one
   useful wakeup.

   :action keeps separate task/action changes distinct, useful for exact
   physical-event accounting runs.

   :unique disables practical coalescing for transport smoke tests."
  [scope data]
  (case (get-in @!state [:config :coalesce-mode] :scope)
    :action
    [:livebench/action
     (scope-key scope)
     (:change/kind data)
     (:task-id data)
     (:customer-id data)
     (:helper-id data)]

    :unique
    [:livebench/unique (random-uuid)]

    :scope
    [:livebench/scope (scope-key scope)]

    [:livebench/scope (scope-key scope)]))

(defn submit-scope!
  [ctx scope data]
  (bump-scope-version! scope)
  (inc-stat! [:events :submitted])
  (try
    (let [dispatch-result
          (live/submit-expanded!
           (live-system ctx)
           ctx
           (assoc scope
                  :change/kind (:change/kind data)
                  :data data)
           {:coalesce-key (livebench-coalesce-key scope data)
            :on-drop (fn [drop-info]
                       (inc-stat! [:events :dropped])
                       (log/debug
                        "Livebench event coalesced or dropped."
                        {:scope scope
                         :data data
                         :drop drop-info}))})]
      (inc-stat! [:events :accepted])
      {:status :accepted
       :scope scope
       :dispatch dispatch-result})
    (catch clojure.lang.ExceptionInfo e
      (if (queue-full? e)
        (do
          (inc-stat! [:events :dropped])
          (inc-stat! [:events :dispatcher-overflows])
          (log/warn "Livebench event dropped because dispatcher queue is full."
                    {:scope scope
                     :data data
                     :ex-data (ex-data e)})
          {:status :dropped
           :reason :dispatcher-full
           :scope scope})
        (throw e)))))

(defn submit-scopes!
  [ctx scopes data]
  (mapv #(submit-scope! ctx % data) scopes))

(defn live-rules
  []
  [{:when-topic :humanhelp/helper
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}

   {:when-topic :humanhelp/customer
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}

   {:when-topic :humanhelp/store-queue
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}])

;; -----------------------------------------------------------------------------
;; Expected fanout
;; -----------------------------------------------------------------------------

(defn expected-events
  [action {:keys [store-id helper-id customer-id]}]
  (let [{:keys [mode helpers-per-store]} (:config @!state)]
    (case mode
      :customer-lifecycle
      (case action
        :customer-arrival 0
        :request 1
        :take 1
        :disposition 1
        0)

      :helper-personal
      (case action
        :customer-arrival 0
        :request 0
        :take (if helper-id 1 0)
        :disposition (if helper-id 1 0)
        0)

      :helper-queue
      (case action
        :customer-arrival 0
        :request helpers-per-store
        :take helpers-per-store
        :disposition 0
        0)

      :mixed
      (case action
        :customer-arrival 0
        :request (+ 1 helpers-per-store)
        :take (+ 2 helpers-per-store)
        :disposition 2
        0)

      0)))

(defn invalidation-scopes
  [action {:keys [store-id helper-id customer-id]}]
  (case action
    :request
    [(customer-scope customer-id)
     (store-queue-scope store-id)]

    :take
    [(helper-scope helper-id)
     (customer-scope customer-id)
     (store-queue-scope store-id)]

    :disposition
    [(helper-scope helper-id)
     (customer-scope customer-id)]

    []))

;; -----------------------------------------------------------------------------
;; Fragment protection
;; -----------------------------------------------------------------------------

(defn fragment-cache-key
  "Return the stable cache/singleflight key for a rendered fragment.

   Important: this intentionally does not include scope-version.

   The fragment cache acts as a freshness window. If a fragment was rendered less
   than :fragment-ttl-ms ago, we may serve it even if the underlying scope
   version advanced. The stored entry records the scope version for stats and
   debugging, but the key itself is stable by fragment identity."
  [kind id _scope]
  [kind id])

(defn fragment-cache-version
  [scope]
  (scope-version scope))

(defn cache-hit
  [k current-version]
  (let [now                         (now-ms)
        {:keys [value expires-at version]} (get @!fragment-cache k)]
    (when (and value (> expires-at now))
      (inc-stat! [:fragments :freshness-window-hits])
      (when (and (some? version)
                 (some? current-version)
                 (not= version current-version))
        (inc-stat! [:fragments :stale-version-hits]))
      value)))

(defn cache-put!
  [k current-version value]
  (let [now (now-ms)
        ttl (get-in @!state [:config :fragment-ttl-ms] 750)]
    (swap! !fragment-cache assoc k {:value value
                                    :version current-version
                                    :stored-at now
                                    :expires-at (+ now ttl)}))
  value)

(defn record-singleflight-wait!
  [started-at-ms]
  (let [elapsed-ms (max 0 (- (now-ms) started-at-ms))]
    (inc-stat! [:fragments :singleflight-wait-count])
    (add-stat! [:fragments :singleflight-wait-ms-total] elapsed-ms)
    (max-stat! [:fragments :singleflight-wait-ms-max] elapsed-ms)
    elapsed-ms))

(defn singleflight-timeout-result
  [k]
  [:timeout
   (ex-info
    "Timed out waiting for livebench fragment render."
    {:key k})])

(defn protected-fragment
  [k current-version render-fn]
  (inc-stat! [:fragments :requests])
  (let [{:keys [cache-enabled singleflight-enabled]} (:config @!state)]
    (if-let [cached (and cache-enabled (cache-hit k current-version))]
      (do
        (inc-stat! [:fragments :cache-hits])
        cached)
      (do
        (inc-stat! [:fragments :cache-misses])
        (inc-stat! [:fragments :version-misses])
        (if-not singleflight-enabled
          (try
            (inc-stat! [:fragments :renders])
            (cache-put! k current-version (render-fn))
            (catch Throwable t
              (inc-stat! [:fragments :errors])
              (throw t)))
          (let [promise-value (promise)
                existing      (.putIfAbsent
                               ^ConcurrentHashMap
                               !fragment-inflight
                               k
                               promise-value)]
            (if (nil? existing)
              ;; Owner path. This request is responsible for rendering and
              ;; delivering the result to any joiners.
              (try
                (inc-stat! [:fragments :renders])
                (let [value (cache-put! k current-version (render-fn))]
                  (deliver promise-value [:ok value])
                  value)
                (catch Throwable t
                  (inc-stat! [:fragments :errors])
                  (deliver promise-value [:error t])
                  (throw t))
                (finally
                  ;; Remove only our own promise. If something very unusual
                  ;; happened during reset/re-entry, do not remove a newer
                  ;; in-flight render for the same key.
                  (.remove
                   ^ConcurrentHashMap
                   !fragment-inflight
                   k
                   promise-value)))

              ;; Joiner path. Another request is already rendering this exact
              ;; fragment key, so wait for that result instead of rendering again.
              (do
                (inc-stat! [:fragments :singleflight-shares])
                (let [started-at-ms (now-ms)
                      [result-kind value]
                      (deref existing
                             5000
                             (singleflight-timeout-result k))]
                  (record-singleflight-wait! started-at-ms)
                  (case result-kind
                    :ok
                    value

                    :timeout
                    (do
                      (inc-stat! [:fragments :singleflight-timeouts])
                      (inc-stat! [:fragments :errors])
                      (throw value))

                    :error
                    (do
                      (inc-stat! [:fragments :errors])
                      (throw value))))))))))))

;; -----------------------------------------------------------------------------
;; Fragment rendering
;; -----------------------------------------------------------------------------

(defn task-row
  [task]
  (str "<li>"
       (:task/id task)
       " "
       (name (:task/status task))
       "</li>"))

(defn tasks-for-helper
  [helper-id n]
  (->> (:tasks @!state)
       vals
       (filter #(= helper-id (:task/helper-id %)))
       (sort-by :task/updated-at >)
       (take n)))

(defn tasks-for-customer
  [customer-id n]
  (->> (:tasks @!state)
       vals
       (filter #(= customer-id (:task/customer-id %)))
       (sort-by :task/updated-at >)
       (take n)))

(defn latest-tasks-for-store
  [store-id n]
  (->> (:tasks @!state)
       vals
       (filter #(= store-id (:task/store-id %)))
       (sort-by :task/updated-at >)
       (take n)))

(defn active-or-tombstone-customer
  [customer-id]
  (or (get-in @!state [:customers customer-id])
      (get-in @!state [:customer-tombstones customer-id])))

(defn helper-personal-fragment-html
  [helper-id]
  (let [helper (get-in @!state [:helpers helper-id])
        tasks  (tasks-for-helper helper-id 10)]
    (str "<section id=\"helper-personal-fragment\">"
         "<h2>Helper " helper-id "</h2>"
         "<p>store=" (:helper/store-id helper) "</p>"
         "<p>active-task=" (or (:helper/active-task-id helper) "") "</p>"
         "<ul>" (apply str (map task-row tasks)) "</ul>"
         "</section>")))

(defn helper-queue-fragment-html
  [store-id]
  (let [store (get-in @!state [:stores store-id])
        tasks (latest-tasks-for-store store-id 10)]
    (str "<section id=\"helper-queue-fragment\">"
         "<h2>Store queue " store-id "</h2>"
         "<p>open=" (count (:store/open-task-ids store)) "</p>"
         "<p>active=" (count (:store/active-task-ids store)) "</p>"
         "<p>completed=" (count (:store/completed-task-ids store)) "</p>"
         "<ul>" (apply str (map task-row tasks)) "</ul>"
         "</section>")))

(defn customer-fragment-html
  [customer-id]
  (let [customer (active-or-tombstone-customer customer-id)
        tasks    (tasks-for-customer customer-id 10)]
    (str "<section id=\"customer-fragment\">"
         "<h2>Customer " customer-id "</h2>"
         "<p>store=" (:customer/store-id customer) "</p>"
         "<p>state=" (name (or (:customer/state customer) :unknown)) "</p>"
         "<p>active-task=" (or (:customer/active-task-id customer) "") "</p>"
         "<ul>" (apply str (map task-row tasks)) "</ul>"
         "</section>")))

(defn fragment-error-response
  [throwable data]
  (inc-stat! [:fragments :errors])
  (log/error throwable "Livebench fragment failed." data)
  (internal-error
   "livebench fragment failed"
   (merge data
          {:error (.getMessage throwable)
           :class (str (class throwable))})))

(defn helper-fragment
  [ctx]
  (try
    (let [helper-id    (param ctx :helper-id)
          watch-queue? (parse-bool* (param ctx :watch-queue) false)
          helper       (get-in @!state [:helpers helper-id])
          store-id     (:helper/store-id helper)]
      (cond
        (str/blank? (str helper-id))
        (bad-request "missing helper-id" {:route :helper-fragment})

        (nil? helper)
        (not-found "unknown helper-id" {:route :helper-fragment
                                         :helper-id helper-id})

        watch-queue?
        (let [scope           (store-queue-scope store-id)
              current-version (fragment-cache-version scope)
              k               (fragment-cache-key :helper-queue store-id scope)]
          (html-response
           (protected-fragment
            k
            current-version
            #(helper-queue-fragment-html store-id))))

        :else
        (let [scope           (helper-scope helper-id)
              current-version (fragment-cache-version scope)
              k               (fragment-cache-key :helper-personal helper-id scope)]
          (html-response
           (protected-fragment
            k
            current-version
            #(helper-personal-fragment-html helper-id))))))
    (catch Throwable t
      (fragment-error-response
       t
       {:route :helper-fragment
        :helper-id (param ctx :helper-id)
        :watch-queue (param ctx :watch-queue)}))))

(defn customer-fragment
  [ctx]
  (try
    (let [customer-id (param ctx :customer-id)
          customer    (active-or-tombstone-customer customer-id)]
      (cond
        (str/blank? (str customer-id))
        (bad-request "missing customer-id" {:route :customer-fragment})

        (nil? customer)
        (not-found "unknown customer-id" {:route :customer-fragment
                                           :customer-id customer-id})

        :else
        (let [scope           (customer-scope customer-id)
              current-version (fragment-cache-version scope)
              k               (fragment-cache-key :customer customer-id scope)]
          (html-response
           (protected-fragment
            k
            current-version
            #(customer-fragment-html customer-id))))))
    (catch Throwable t
      (fragment-error-response
       t
       {:route :customer-fragment
        :customer-id (param ctx :customer-id)}))))

;; -----------------------------------------------------------------------------
;; Streams
;; -----------------------------------------------------------------------------

(defn start-stream
  [ctx scope]
  (inc-stat! [:sse :connections-opened])
  (let [started
        (live/start-sse!
         (live-system ctx)
         scope
         {:flow-options {:relieve? true}
          :sse-options {:on-close (fn [_close-info]
                                    (inc-stat! [:sse :connections-closed]))}})]
    (:response started)))

(defn helper-stream
  [ctx]
  (let [helper-id    (param ctx :helper-id)
        watch-queue? (parse-bool* (param ctx :watch-queue) false)
        helper       (get-in @!state [:helpers helper-id])
        store-id     (:helper/store-id helper)]
    (cond
      (str/blank? (str helper-id))
      (bad-request "missing helper-id" {:route :helper-stream})

      (nil? helper)
      (not-found "unknown helper-id" {:route :helper-stream
                                       :helper-id helper-id})

      :else
      (let [scope (if watch-queue?
                    (store-queue-scope store-id)
                    (helper-scope helper-id))]
        (start-stream ctx scope)))))

(defn customer-stream
  [ctx]
  (let [customer-id (param ctx :customer-id)
        customer    (active-or-tombstone-customer customer-id)]
    (cond
      (str/blank? (str customer-id))
      (bad-request "missing customer-id" {:route :customer-stream})

      (nil? customer)
      (not-found "unknown customer-id" {:route :customer-stream
                                         :customer-id customer-id})

      :else
      (start-stream ctx (customer-scope customer-id)))))

;; -----------------------------------------------------------------------------
;; Action responses
;; -----------------------------------------------------------------------------

(defn action-response
  [{:keys [status action reason message store-id customer-id helper-id task-id expected-events live]}]
  (json-response
   (cond-> {:status status
            :action action
            :expected-events (or expected-events 0)}
     reason (assoc :reason reason)
     message (assoc :message message)
     store-id (assoc :store-id store-id)
     customer-id (assoc :customer-id customer-id)
     helper-id (assoc :helper-id helper-id)
     task-id (assoc :task-id task-id)
     live (assoc :live live))))

(defn controlled-rejection
  [action reason]
  {:status :rejected
   :action action
   :reason reason
   :expected-events 0})

(defn ok-result
  [action data]
  (merge {:status :ok
          :action action}
         data))

;; -----------------------------------------------------------------------------
;; Store/helper selection
;; -----------------------------------------------------------------------------

(defn choose-store-id
  [st explicit-store-id]
  (if explicit-store-id
    [st explicit-store-id]
    (let [n         (count (:stores st))
          idx       (mod (:global-store-seq st) n)
          store-id' (store-id idx)]
      [(update st :global-store-seq inc) store-id'])))

(defn available-helper-id
  [st store-id]
  (some (fn [hid]
          (when-not (get-in st [:helpers hid :helper/active-task-id])
            hid))
        (get-in st [:stores store-id :store/helper-ids])))

(defn helper-available?
  [st helper-id]
  (and (contains? (:helpers st) helper-id)
       (nil? (get-in st [:helpers helper-id :helper/active-task-id]))))

(defn open-task-id
  [st store-id explicit-task-id]
  (or explicit-task-id
      (first (get-in st [:stores store-id :store/open-task-ids]))))

(defn helper-with-active-task-id
  [st store-id]
  (some (fn [hid]
          (when (get-in st [:helpers hid :helper/active-task-id])
            hid))
        (get-in st [:stores store-id :store/helper-ids])))

;; -----------------------------------------------------------------------------
;; Actions
;; -----------------------------------------------------------------------------

(defn arrive-customer!
  [ctx]
  (inc-stat! [:actions :arrival-attempts])
  (let [result
        (state-update!
         (fn [st]
           (let [[st store-id'] (choose-store-id st (param ctx :store-id))
                 cfg           (:config st)
                 active-count  (count (get-in st [:stores store-id' :store/active-customer-ids]))
                 capacity      (:max-active-customers-per-store cfg)]
             (if (>= active-count capacity)
               [st (controlled-rejection :customer-arrival :customer-capacity-full)]
               (let [seq'        (get-in st [:stores store-id' :store/customer-seq])
                     customer-id' (customer-id store-id' seq')
                     customer    {:customer/id customer-id'
                                  :customer/store-id store-id'
                                  :customer/state :active
                                  :customer/active-task-id nil
                                  :customer/arrived-at (now-ms)
                                  :customer/left-at nil}]
                 [(-> st
                      (update-in [:stores store-id' :store/customer-seq] inc)
                      (update-in [:stores store-id' :store/active-customer-ids] conj customer-id')
                      (assoc-in [:customers customer-id'] customer))
                  (ok-result
                   :customer-arrival
                   {:store-id store-id'
                    :customer-id customer-id'
                    :expected-events 0})])))))]
    (if (= :ok (:status result))
      (inc-stat! [:actions :customers-created])
      (inc-stat! [:actions :arrivals-rejected-customer-capacity-full]))
    (action-response result)))

(defn request!
  [ctx]
  (inc-stat! [:actions :request-attempts])
  (let [result
        (state-update!
         (fn [st]
           (let [customer-id' (param ctx :customer-id)
                 customer     (get-in st [:customers customer-id'])]
             (cond
               (nil? customer)
               [st (controlled-rejection :request :no-active-customer)]

               (:customer/active-task-id customer)
               [st (controlled-rejection :request :customer-already-has-task)]

               :else
               (let [store-id'  (:customer/store-id customer)
                     open-count (count (get-in st [:stores store-id' :store/open-task-ids]))
                     max-open   (get-in st [:config :max-open-tasks-per-store])]
                 (if (>= open-count max-open)
                   [st (controlled-rejection :request :queue-full)]
                   (let [seq'    (get-in st [:stores store-id' :store/task-seq])
                         task-id' (task-id store-id' seq')
                         task    {:task/id task-id'
                                  :task/store-id store-id'
                                  :task/customer-id customer-id'
                                  :task/helper-id nil
                                  :task/status :requested
                                  :task/created-at (now-ms)
                                  :task/updated-at (now-ms)
                                  :task/version 0
                                  :task/customer-arrived-at (:customer/arrived-at customer)
                                  :task/customer-left-at nil}
                         ids     {:store-id store-id'
                                  :customer-id customer-id'}
                         expected (expected-events :request ids)]
                     [(-> st
                          (update-in [:stores store-id' :store/task-seq] inc)
                          (assoc-in [:tasks task-id'] task)
                          (assoc-in [:customers customer-id' :customer/active-task-id] task-id')
                          (update-in [:stores store-id' :store/open-task-ids] conj task-id'))
                      (ok-result
                       :request
                       {:store-id store-id'
                        :customer-id customer-id'
                        :task-id task-id'
                        :expected-events expected
                        :ids ids})])))))))]
    (if (= :ok (:status result))
      (do
        (inc-stat! [:actions :requests-created])
        (let [scopes (invalidation-scopes :request (:ids result))
              live   (submit-scopes!
                      ctx
                      scopes
                      {:change/kind :task/requested
                       :task-id (:task-id result)
                       :customer-id (:customer-id result)
                       :store-id (:store-id result)})]
          (action-response (assoc result :live live))))
      (do
        (case (:reason result)
          :no-active-customer
          (inc-stat! [:actions :requests-rejected-no-active-customer])

          :customer-already-has-task
          (inc-stat! [:actions :requests-rejected-customer-already-has-task])

          :queue-full
          (inc-stat! [:actions :requests-rejected-queue-full])

          nil)
        (action-response result)))))

(defn take!
  [ctx]
  (inc-stat! [:actions :take-attempts])
  (let [result
        (state-update!
         (fn [st]
           (let [[st store-id'] (choose-store-id st (param ctx :store-id))
                 task-id'      (open-task-id st store-id' (param ctx :task-id))]
             (if-not task-id'
               [st (controlled-rejection :take :no-open-task)]
               (let [helper-id' (or (param ctx :helper-id)
                                    (available-helper-id st store-id'))]
                 (cond
                   (nil? helper-id')
                   [st (controlled-rejection :take :no-available-helper)]

                   (not (helper-available? st helper-id'))
                   [st (controlled-rejection :take :no-available-helper)]

                   :else
                   (let [task        (get-in st [:tasks task-id'])
                         customer-id' (:task/customer-id task)
                         ids         {:store-id store-id'
                                      :helper-id helper-id'
                                      :customer-id customer-id'}
                         expected    (expected-events :take ids)]
                     [(-> st
                          (assoc-in [:tasks task-id' :task/status] :assigned)
                          (assoc-in [:tasks task-id' :task/helper-id] helper-id')
                          (assoc-in [:tasks task-id' :task/updated-at] (now-ms))
                          (update-in [:tasks task-id' :task/version] (fnil inc 0))
                          (assoc-in [:helpers helper-id' :helper/active-task-id] task-id')
                          (update-in [:stores store-id' :store/open-task-ids] remove-id task-id')
                          (update-in [:stores store-id' :store/active-task-ids] conj task-id'))
                      (ok-result
                       :take
                       {:store-id store-id'
                        :helper-id helper-id'
                        :customer-id customer-id'
                        :task-id task-id'
                        :expected-events expected
                        :ids ids})])))))))]
    (if (= :ok (:status result))
      (do
        (inc-stat! [:actions :takes])
        (let [scopes (invalidation-scopes :take (:ids result))
              live   (submit-scopes!
                      ctx
                      scopes
                      {:change/kind :task/assigned
                       :task-id (:task-id result)
                       :helper-id (:helper-id result)
                       :customer-id (:customer-id result)
                       :store-id (:store-id result)})]
          (action-response (assoc result :live live))))
      (do
        (case (:reason result)
          :no-open-task
          (inc-stat! [:actions :takes-no-open-task])

          :no-available-helper
          (inc-stat! [:actions :takes-no-available-helper])

          nil)
        (action-response result)))))

(defn disposition!
  [ctx]
  (inc-stat! [:actions :disposition-attempts])
  (let [status (valid-disposition-status (or (param ctx :status) "completed"))
        result
        (state-update!
         (fn [st]
           (let [[st store-id'] (choose-store-id st (param ctx :store-id))
                 helper-id'    (or (param ctx :helper-id)
                                   (helper-with-active-task-id st store-id'))
                 task-id'      (or (param ctx :task-id)
                                   (get-in st [:helpers helper-id' :helper/active-task-id]))]
             (if-not task-id'
               [st (controlled-rejection :disposition :no-active-task)]
               (let [task         (get-in st [:tasks task-id'])
                     customer-id' (:task/customer-id task)
                     customer     (get-in st [:customers customer-id'])
                     now          (now-ms)
                     retention    (get-in st [:config :customer-dispose-retention-ms])
                     tombstone    (-> customer
                                      (assoc :customer/state :disposed
                                             :customer/active-task-id nil
                                             :customer/left-at now
                                             :customer/tombstone-expires-at (+ now retention)))
                     ids          {:store-id store-id'
                                   :helper-id helper-id'
                                   :customer-id customer-id'}
                     expected     (expected-events :disposition ids)]
                 [(-> st
                      (assoc-in [:tasks task-id' :task/status] status)
                      (assoc-in [:tasks task-id' :task/updated-at] now)
                      (assoc-in [:tasks task-id' :task/customer-left-at] now)
                      (update-in [:tasks task-id' :task/version] (fnil inc 0))
                      (assoc-in [:helpers helper-id' :helper/active-task-id] nil)
                      (update-in [:stores store-id' :store/active-task-ids] remove-id task-id')
                      (update-in [:stores store-id' :store/completed-task-ids] conj task-id')
                      (update-in [:stores store-id' :store/active-customer-ids] remove-id customer-id')
                      (update :customers dissoc customer-id')
                      (assoc-in [:customer-tombstones customer-id'] tombstone))
                  (ok-result
                   :disposition
                   {:store-id store-id'
                    :helper-id helper-id'
                    :customer-id customer-id'
                    :task-id task-id'
                    :expected-events expected
                    :ids ids})])))))]
    (if (= :ok (:status result))
      (do
        (inc-stat! [:actions :dispositions])
        (let [scopes (invalidation-scopes :disposition (:ids result))
              live   (submit-scopes!
                      ctx
                      scopes
                      {:change/kind :task/dispositioned
                       :task-id (:task-id result)
                       :helper-id (:helper-id result)
                       :customer-id (:customer-id result)
                       :store-id (:store-id result)
                       :status status})]
          (action-response (assoc result :live live))))
      (do
        (inc-stat! [:actions :dispositions-no-active-task])
        (action-response result)))))

;; -----------------------------------------------------------------------------
;; Config/stats
;; -----------------------------------------------------------------------------

(defn safe-ratio
  [numerator denominator]
  (if (pos? denominator)
    (double (/ numerator denominator))
    0.0))

(defn fragment-stats-summary
  [fragments]
  (let [requests                   (:requests fragments 0)
        renders                    (:renders fragments 0)
        cache-hits                 (:cache-hits fragments 0)
        cache-misses               (:cache-misses fragments 0)
        freshness-window-hits      (:freshness-window-hits fragments 0)
        stale-version-hits         (:stale-version-hits fragments 0)
        singleflight-shares        (:singleflight-shares fragments 0)
        singleflight-wait-count    (:singleflight-wait-count fragments 0)
        singleflight-wait-ms-total (:singleflight-wait-ms-total fragments 0)]
    (assoc fragments
           :cache-hit-rate
           (safe-ratio cache-hits requests)

           :cache-miss-rate
           (safe-ratio cache-misses requests)

           :render-rate
           (safe-ratio renders requests)

           :freshness-window-hit-rate
           (safe-ratio freshness-window-hits requests)

           :stale-version-hit-rate
           (safe-ratio stale-version-hits requests)

           :singleflight-share-rate
           (safe-ratio singleflight-shares requests)

           :singleflight-wait-ms-avg
           (safe-ratio singleflight-wait-ms-total
                       singleflight-wait-count))))

(defn config-route
  [_ctx]
  (json-response
   (assoc (:config @!state)
          :live {:relieve-enabled true}
          :routes {:reset "/api/livebench/reset"
                   :config "/api/livebench/config"
                   :stats "/api/livebench/stats"
                   :helper-stream "/api/livebench/stream/helper"
                   :customer-stream "/api/livebench/stream/customer"
                   :helper-fragment "/api/livebench/fragment/helper"
                   :customer-fragment "/api/livebench/fragment/customer"
                   :customer-arrive "/api/livebench/customer/arrive"
                   :request "/api/livebench/request"
                   :take "/api/livebench/take"
                   :disposition "/api/livebench/disposition"})))

(defn stats-route
  [_ctx]
  (let [st                 @!state
        stats              @!stats
        tasks              (vals (:tasks st))
        active-customers   (count (:customers st))
        disposed-customers (count (filter #(some? (:customer/left-at %)) tasks))]
    (json-response
     (merge
      (assoc stats
             :fragments
             (fragment-stats-summary (:fragments stats)))
      {:state {:stores (count (:stores st))
               :helpers (count (:helpers st))
               :active-customers active-customers
               :disposed-customers disposed-customers
               :customer-tombstones (count (:customer-tombstones st))
               :tasks (count tasks)
               :open-tasks (count (filter #(= :requested (:task/status %)) tasks))
               :active-tasks (count (filter #(= :assigned (:task/status %)) tasks))
               :completed-tasks (count (remove #(#{:requested :assigned} (:task/status %)) tasks))}
       :fragment-cache {:entries  (count @!fragment-cache)
                        :inflight (.size ^ConcurrentHashMap !fragment-inflight)}}))))


;; -----------------------------------------------------------------------------
;; Routes
;; -----------------------------------------------------------------------------

(def api-routes
  [["/api/livebench"
    {:middleware [wrap-livebench-token]}

    ["/reset" {:post reset-route!}]
    ["/config" {:get config-route}]
    ["/stats" {:get stats-route}]

    ["/stream/helper" {:get helper-stream}]
    ["/stream/customer" {:get customer-stream}]

    ["/fragment/helper" {:get helper-fragment}]
    ["/fragment/customer" {:get customer-fragment}]

    ["/customer/arrive" {:post arrive-customer!}]
    ["/request" {:post request!}]
    ["/take" {:post take!}]
    ["/disposition" {:post disposition!}]]])

(def module
  {:live-rules (live-rules)
   :api-routes api-routes})
