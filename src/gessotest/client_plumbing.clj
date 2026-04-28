(ns gessotest.client-plumbing
  "Small app-owned browser-client communication plumbing.

   This is not Gesso component code. It is application machinery for talking to
   connected browser clients from many app namespaces without each namespace
   rewriting SSE streams, pending OOB queues, and client targeting.

   It knows about:
   - connected browser clients
   - SSE wakeups
   - pending HTMX out-of-band fragments

   It does not know about:
   - toasts
   - notification semantics
   - REPL demos
   - app domain events"
  (:require
   [gesso.core :as g]
   [gesso.live.htmx :as live-htmx]
   [gesso.live.transport.sse :as sse]
   [gessotest.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/client-plumbing")

(def stream-path
  (str base-path "/stream"))

(def pending-path
  (str base-path "/pending"))

(def wake-event
  "client-oob")

;; -----------------------------------------------------------------------------
;; State
;; -----------------------------------------------------------------------------

;; client-id -> {:id client-id
;;               :user-id user-id
;;               :queue LinkedBlockingQueue
;;               :connected-at millis}
(defonce clients
  (atom {}))

;; client-id -> [hiccup-oob-node hiccup-oob-node ...]
;;
;; Stored values should already be HTMX OOB fragments, e.g.
;;
;;   [:div {:id "some-target" :hx-swap-oob "innerHTML"} ...]
;;
;; This namespace does not care what the OOB fragment represents.
(defonce pending-oob
  (atom {}))

(when-not (map? @clients)
  (reset! clients {}))

(when-not (map? @pending-oob)
  (reset! pending-oob {}))

(defn reset-plumbing!
  []
  (reset! clients {})
  (reset! pending-oob {})
  :reset)

;; -----------------------------------------------------------------------------
;; Request/client helpers
;; -----------------------------------------------------------------------------

(defn new-client-id
  []
  (str (random-uuid)))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn client-id-from-ctx
  [{:keys [params]}]
  (or (request-param params :client-id)
      (new-client-id)))

(defn current-user-id
  "Return the app user id used for client targeting.

   Replace this with the real app convention when this pattern moves out of
   gessotest."
  [ctx]
  (str
   (or (:user/id ctx)
       (get-in ctx [:user :xt/id])
       (get-in ctx [:session :user])
       (get-in ctx [:session :uid])
       "demo-user")))

;; -----------------------------------------------------------------------------
;; Client registry
;; -----------------------------------------------------------------------------

(defn- add-client!
  [client-id user-id queue]
  (swap! clients assoc client-id
         {:id client-id
          :user-id (str user-id)
          :queue queue
          :connected-at (now-ms)}))

(defn- remove-client!
  [client-id queue]
  (swap! clients
         (fn [m]
           ;; Avoid removing a newer connection that reused the same client id.
           (if (= queue (get-in m [client-id :queue]))
             (dissoc m client-id)
             m))))

(defn connected-clients
  []
  (->> @clients
       vals
       (sort-by :connected-at)
       vec))

(defn connected-client-ids
  []
  (mapv :id (connected-clients)))

(defn latest-client-id
  []
  (some-> (connected-clients)
          last
          :id))

(defn clients-for-user
  [user-id]
  (->> @clients
       vals
       (filter #(= (str user-id) (:user-id %)))
       (sort-by :connected-at)
       vec))

(defn pending-counts
  []
  (->> @pending-oob
       (map (fn [[client-id nodes]]
              [client-id (count nodes)]))
       (into {})))

(defn state-summary
  []
  {:clients (connected-clients)
   :pending (pending-counts)})

;; -----------------------------------------------------------------------------
;; SSE stream
;; -----------------------------------------------------------------------------

(defn stream
  "SSE endpoint for connected browser clients.

   This stream sends wakeup events only. The browser receives:

     event: client-oob
     data: {}

   and then HTMX fetches this client's pending OOB fragments."
  [ctx]
  (let [client-id (client-id-from-ctx ctx)
        user-id   (current-user-id ctx)
        queue     (sse/new-queue)]
    (add-client! client-id user-id queue)
    (sse/queue-stream-response
     {:queue queue
      :on-close #(remove-client! client-id queue)})))

;; -----------------------------------------------------------------------------
;; Wakeup
;; -----------------------------------------------------------------------------

(defn wake-client!
  ([client-id]
   (wake-client! client-id wake-event))
  ([client-id event]
   (if-let [queue (get-in @clients [client-id :queue])]
     (do
       (sse/offer! queue {:event event
                          :data "{}"})
       true)
     false)))

(defn wake-user!
  ([user-id]
   (wake-user! user-id wake-event))
  ([user-id event]
   (let [targets (clients-for-user user-id)]
     {:user-id (str user-id)
      :sent (count targets)
      :results
      (mapv
       (fn [{:keys [id]}]
         {:client-id id
          :woke? (wake-client! id event)})
       targets)})))

;; -----------------------------------------------------------------------------
;; Pending OOB mailbox
;; -----------------------------------------------------------------------------

(defn- enqueue-pending-oob!
  [client-id node]
  (swap! pending-oob update client-id (fnil conj []) node))

(defn- enqueue-pending-oobs!
  [client-id nodes]
  (swap! pending-oob update client-id (fnil into []) nodes))

(defn- drain-pending-oob!
  [client-id]
  (loop []
    (let [m     @pending-oob
          nodes (get m client-id [])]
      (if (compare-and-set! pending-oob m (dissoc m client-id))
        nodes
        (recur)))))

(defn send-oob-to-client!
  "Queue one OOB node for one connected client and wake that client.

   The node should normally already contain hx-swap-oob."
  [client-id node]
  (if (get @clients client-id)
    (do
      (enqueue-pending-oob! client-id node)
      {:sent 1
       :woke? (wake-client! client-id)
       :client-id client-id
       :pending (count (get @pending-oob client-id))})
    {:sent 0
     :woke? false
     :client-id client-id
     :error :client-not-connected}))

(defn send-oobs-to-client!
  "Queue multiple OOB nodes for one connected client and wake that client once."
  [client-id nodes]
  (if (get @clients client-id)
    (do
      (enqueue-pending-oobs! client-id (remove nil? nodes))
      {:sent 1
       :woke? (wake-client! client-id)
       :client-id client-id
       :pending (count (get @pending-oob client-id))})
    {:sent 0
     :woke? false
     :client-id client-id
     :error :client-not-connected}))

(defn send-oob-to-user!
  "Queue one OOB node for every connected client belonging to user-id."
  [user-id node]
  (let [targets (clients-for-user user-id)]
    (doseq [{:keys [id]} targets]
      (enqueue-pending-oob! id node))
    {:user-id (str user-id)
     :sent (count targets)
     :results
     (mapv
      (fn [{:keys [id]}]
        {:client-id id
         :woke? (wake-client! id)})
      targets)}))

(defn send-oobs-to-user!
  "Queue multiple OOB nodes for every connected client belonging to user-id."
  [user-id nodes]
  (let [targets (clients-for-user user-id)
        nodes'  (remove nil? nodes)]
    (doseq [{:keys [id]} targets]
      (enqueue-pending-oobs! id nodes'))
    {:user-id (str user-id)
     :sent (count targets)
     :results
     (mapv
      (fn [{:keys [id]}]
        {:client-id id
         :woke? (wake-client! id)})
      targets)}))

(defn broadcast-oob!
  "Queue one OOB node for every connected client.

   This should be explicit and rare in real apps."
  [node]
  (let [targets (connected-clients)]
    (doseq [{:keys [id]} targets]
      (enqueue-pending-oob! id node))
    {:sent (count targets)
     :results
     (mapv
      (fn [{:keys [id]}]
        {:client-id id
         :woke? (wake-client! id)})
      targets)}))

(defn broadcast-oobs!
  "Queue multiple OOB nodes for every connected client."
  [nodes]
  (let [targets (connected-clients)
        nodes'  (remove nil? nodes)]
    (doseq [{:keys [id]} targets]
      (enqueue-pending-oobs! id nodes'))
    {:sent (count targets)
     :results
     (mapv
      (fn [{:keys [id]}]
        {:client-id id
         :woke? (wake-client! id)})
      targets)}))

(defn pending-handler
  "HTMX endpoint triggered by sse:client-oob.

   Drains this connected client's OOB mailbox and returns those fragments."
  [ctx]
  (let [client-id (client-id-from-ctx ctx)
        nodes     (drain-pending-oob! client-id)]
    (if (seq nodes)
      (g/html-response
       (into [:<>] (remove nil?) nodes))
      (g/no-content))))

;; -----------------------------------------------------------------------------
;; Browser hook
;; -----------------------------------------------------------------------------

(defn listener
  "Render one browser-client listener.

   Mount this on pages that need app-owned server-to-client OOB updates."
  [client-id]
  (live-htmx/sse-callback
   {:id (str "client-plumbing-listener-" client-id)
    :stream-url (str stream-path "?client-id=" client-id)
    :event wake-event
    :get (str pending-path "?client-id=" client-id)
    :swap "none"
    :attrs {:data-client-plumbing-listener true}}))

(defn app-listener
  "Convenience listener for app shells that do not need to expose client-id."
  [_ctx]
  (listener (new-client-id)))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending-handler}]]]})
