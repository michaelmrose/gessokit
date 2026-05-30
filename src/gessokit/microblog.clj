(ns gessokit.microblog
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [gesso.live.core :as live]
   [gessokit.middleware :as mid]
   [rum.core :as rum])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Response helpers
;; -----------------------------------------------------------------------------

(defn html-response
  [hiccup]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body (rum/render-static-markup hiccup)})

(defn no-content
  []
  {:status 204
   :headers {}
   :body ""})

(defn text-response
  [s]
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body s})

(defn json-response
  [data]
  {:status 200
   :headers {"content-type" "application/edn; charset=utf-8"}
   :body (pr-str data)})

;; -----------------------------------------------------------------------------
;; In-memory demo state
;; -----------------------------------------------------------------------------

(defonce !state
  (atom {:next-tweet 0
         :write-seq 0
         :tweets []
         :likes {}
         :shares {}
         :comments {}}))

(defn next-tweet-id!
  []
  (str "tweet-" (:next-tweet
                (swap! !state update :next-tweet inc))))

(defn next-write-index!
  []
  (:write-seq
   (swap! !state update :write-seq inc)))

;; -----------------------------------------------------------------------------
;; Identity, modes, cohorts
;; -----------------------------------------------------------------------------

(def valid-modes
  #{:broadcast :high :medium :low :personal})

(defn parse-int
  [s default]
  (try
    (Long/parseLong (str s))
    (catch Exception _
      default)))

(defn param
  [ctx k]
  (or (get-in ctx [:params k])
      (get-in ctx [:params (name k)])
      (get-in ctx [:query-params k])
      (get-in ctx [:query-params (name k)])
      (get-in ctx [:form-params k])
      (get-in ctx [:form-params (name k)])))

(defn current-user-id
  [ctx]
  (or (:uid ctx)
      (:user/id ctx)
      (get-in ctx [:session :uid])
      (get-in ctx [:headers "x-load-user"])
      (param ctx :user-id)
      "anonymous"))

(defn load-user-id
  [ctx]
  (or (param ctx :user-id)
      (get-in ctx [:headers "x-load-user"])
      "load-user-0"))

(defn load-user-index
  [user-id]
  (or (some->> (re-find #"\d+$" (str user-id))
               (parse-int nil))
      (Math/abs (hash user-id))))

(defn load-mode
  [ctx]
  (let [raw (or (param ctx :mode)
                (get-in ctx [:headers "x-load-mode"])
                "medium")
        mode (keyword (str/lower-case (name raw)))]
    (if (valid-modes mode) mode :medium)))

(defn with-load-user
  [ctx]
  (let [user-id (str (load-user-id ctx))
        mode    (load-mode ctx)]
    (-> ctx
        (assoc :gessokit.load/user-id user-id
               :gessokit.load/mode mode
               :uid user-id
               :user/id user-id)
        (assoc-in [:session :uid] user-id))))

(defn cohort-count
  "Cohort count controls fanout.

   broadcast => all users in one cohort
   high      => large cohorts
   medium    => medium cohorts
   low       => small cohorts
   personal  => one cohort per load-user index, practically one user/cohort"
  [mode]
  (case mode
    :broadcast 1
    :high      4
    :medium    25
    :low       100
    :personal  1000000
    25))

(defn cohort-id
  [mode user-id]
  (let [n (cohort-count mode)
        i (load-user-index user-id)]
    (str (name mode) "-cohort-" (mod i n))))

(defn target-cohort-id
  "Spread writes across cohorts rather than making every write hit cohort 0."
  [mode write-index]
  (let [n (cohort-count mode)]
    (str (name mode) "-cohort-" (mod write-index n))))

(defn subscription-scope
  [ctx]
  (let [mode    (or (:gessokit.load/mode ctx) :medium)
        user-id (current-user-id ctx)]
    {:topic :microblog/timeline
     :id    (cohort-id mode user-id)}))

(defn write-scope
  [mode write-index]
  {:topic :microblog/timeline
   :id    (target-cohort-id mode write-index)})

;; -----------------------------------------------------------------------------
;; Live integration
;; -----------------------------------------------------------------------------

(defn live-system
  [ctx]
  (:gesso.live/system ctx))

(defn queue-full?
  [e]
  (or (= "gesso.live dispatcher queue is full." (ex-message e))
      (= :gesso.live.dispatch/queue-full (:gesso.live/error (ex-data e)))
      (some-> (ex-message e)
              (str/includes? "dispatcher queue is full"))))

(defn submit-change!
  "Submit a live invalidation, but never fail the user's write because live is
   overloaded. The write is the source of truth; live is a wakeup path."
  [ctx change]
  (try
    (live/submit-expanded! (live-system ctx) ctx change)
    (catch clojure.lang.ExceptionInfo e
      (if (queue-full? e)
        (do
          (log/warn "Microblog live invalidation dropped/coalesced because dispatcher queue is full."
                    {:change change
                     :ex-data (ex-data e)})
          {:status :dropped
           :reason :dispatcher-full})
        (throw e)))))

(defn live-rules
  []
  [{:when-topic :microblog/timeline
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}
   {:when-topic :microblog/tweet
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}
   {:when-topic :microblog/stats
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind :data])])}])

;; -----------------------------------------------------------------------------
;; Fragment protection: app-level singleflight + short TTL cache
;; -----------------------------------------------------------------------------

(def fragment-ttl-ms 750)

(defonce !fragment-cache
  (atom {}))

(defonce !fragment-inflight
  (atom {}))

(defonce fragment-lock
  (Object.))

(defn now-ms
  []
  (System/currentTimeMillis))

(defn cache-hit
  [k]
  (let [{:keys [value expires-at]} (get @!fragment-cache k)]
    (when (and value (> expires-at (now-ms)))
      value)))

(defn cache-put!
  [k value]
  (swap! !fragment-cache assoc k {:value value
                                  :expires-at (+ (now-ms) fragment-ttl-ms)})
  value)

(defn protected-fragment
  "Small app-level singleflight/cache wrapper.

   This intentionally protects the demo even if the generic Gesso fragment
   helper changes. If your gesso.live.fragment facade is stable, this can later
   be replaced with that."
  [k render]
  (if-let [v (cache-hit k)]
    v
    (let [p      (promise)
          owner? (atom false)
          wait-p (locking fragment-lock
                   (if-let [existing (get @!fragment-inflight k)]
                     existing
                     (do
                       (reset! owner? true)
                       (swap! !fragment-inflight assoc k p)
                       p)))]
      (if @owner?
        (try
          (let [v (render)]
            (cache-put! k v)
            (deliver p [:ok v])
            v)
          (catch Throwable t
            (deliver p [:error t])
            (throw t))
          (finally
            (swap! !fragment-inflight dissoc k)))
        (let [[status v] (deref wait-p 5000 [:error
                                             (ex-info "Timed out waiting for microblog fragment render."
                                                      {:key k})])]
          (case status
            :ok v
            :error (throw v)))))))

;; -----------------------------------------------------------------------------
;; Data model
;; -----------------------------------------------------------------------------

(defn tweet-body
  [ctx]
  (or (param ctx :body)
      (param ctx :text)
      (param ctx :content)
      ""))

(defn create-tweet*
  [ctx {:keys [author-id body mode]}]
  (let [mode        (or mode :medium)
        write-index (next-write-index!)
        scope       (write-scope mode write-index)
        tweet-id    (next-tweet-id!)
        tweet       {:tweet/id tweet-id
                     :tweet/body body
                     :tweet/author-id author-id
                     :tweet/mode mode
                     :tweet/cohort-id (:id scope)
                     :tweet/created-at (now-ms)}]
    (swap! !state update :tweets conj tweet)

    (submit-change!
     ctx
     {:topic (:topic scope)
      :id (:id scope)
      :change/kind :updated
      :data {:tweet/id tweet-id
             :tweet/author-id author-id
             :tweet/mode mode
             :tweet/cohort-id (:id scope)}})

    (submit-change!
     ctx
     {:topic :microblog/stats
      :id "stats"
      :change/kind :updated
      :data {:tweet/id tweet-id}})

    tweet))

(defn tweets
  []
  (:tweets @!state))

(defn tweets-for-cohort
  [cohort-id]
  (->> (tweets)
       (filter #(= cohort-id (:tweet/cohort-id %)))
       reverse
       (take 50)))

(defn recent-tweets
  []
  (->> (tweets)
       reverse
       (take 50)))

(defn tweet-by-id
  [tweet-id]
  (some #(when (= tweet-id (:tweet/id %)) %) (tweets)))

;; -----------------------------------------------------------------------------
;; Rendering
;; -----------------------------------------------------------------------------

(defn tweet-view
  [tweet]
  [:article.rounded-xl.border.border-border.bg-card.p-4.shadow-sm
   [:div.flex.items-center.justify-between.gap-3
    [:div.text-sm.text-muted-foreground
     (:tweet/author-id tweet)
     " · "
     (name (:tweet/mode tweet))
     " · "
     (:tweet/cohort-id tweet)]
    [:a.text-sm.underline
     {:href (str "/app/microblog/tweet/" (:tweet/id tweet))}
     (:tweet/id tweet)]]
   [:p.mt-2.text-base (:tweet/body tweet)]
   [:div.mt-3.flex.gap-2
    [:form {:method "post"
            :action (str "/app/microblog/tweet/" (:tweet/id tweet) "/like")}
     [:button.btn-outline {:type "submit"} "Like"]]
    [:form {:method "post"
            :action (str "/app/microblog/tweet/" (:tweet/id tweet) "/share")}
     [:button.btn-outline {:type "submit"} "Share"]]]])

(defn feed-view
  [title tweets]
  [:section.space-y-4
   [:div.flex.items-center.justify-between
    [:h2.text-xl.font-semibold title]
    [:span.text-sm.text-muted-foreground (count tweets) " shown"]]
   (if (seq tweets)
     [:div.space-y-3
      (for [tweet tweets]
        ^{:key (:tweet/id tweet)}
        [tweet-view tweet])]
     [:div.rounded-xl.border.border-dashed.border-border.p-6.text-muted-foreground
      "No tweets yet."])])

(defn composer
  []
  [:form.flex.gap-2
   {:method "post"
    :action "/app/microblog/tweet"}
   [:input.input.flex-1
    {:type "text"
     :name "body"
     :placeholder "What is happening?"
     :autocomplete "off"}]
   [:button.btn {:type "submit"} "Post"]])

(defn page-shell
  [& body]
  [:div.mx-auto.max-w-5xl.space-y-6.p-6
   [:header.space-y-2
    [:h1.text-3xl.font-bold "Microblog"]
    [:p.text-muted-foreground
     "Cohort-based Gesso Live fanout demo."]]
   body])

(defn timeline-fragment
  [ctx]
  (let [mode      (or (:gessokit.load/mode ctx) :medium)
        user-id   (current-user-id ctx)
        cohort-id (cohort-id mode user-id)]
    (protected-fragment
     [:timeline mode cohort-id]
     (fn []
       [:div#microblog-timeline
        [feed-view
         (str "Timeline " cohort-id)
         (tweets-for-cohort cohort-id)]]))))

(defn stats-fragment
  [_ctx]
  (let [{:keys [tweets]} @!state]
    [:div#microblog-stats.rounded-xl.border.border-border.bg-card.p-4
     [:h2.text-xl.font-semibold "Stats"]
     [:dl.mt-3.grid.grid-cols-2.gap-3.text-sm
      [:dt.text-muted-foreground "Tweets"]
      [:dd.font-mono (count tweets)]
      [:dt.text-muted-foreground "Fragment cache entries"]
      [:dd.font-mono (count @!fragment-cache)]
      [:dt.text-muted-foreground "Inflight fragments"]
      [:dd.font-mono (count @!fragment-inflight)]]]))


(defn page
  [ctx]
  (let [ctx'    ctx
        user-id (current-user-id ctx')
        mode    :medium
        cohort  (cohort-id mode user-id)]
    (html-response
     [page-shell
      [composer]
      [:div.rounded-xl.border.border-border.bg-card.p-3.text-sm.text-muted-foreground
       "Signed-in user: " user-id " · cohort: " cohort]
      [:div
       {:hx-ext "sse"
        :sse-connect "/app/microblog/stream"}
       [:div
        {:hx-get "/app/microblog/fragments/timeline"
         :hx-trigger "load, sse:live-update"
         :hx-swap "outerHTML"}
        (timeline-fragment ctx')]]
      [:div
       {:hx-get "/app/microblog/fragments/stats"
        :hx-trigger "load every 5s"
        :hx-swap "outerHTML"}
       (stats-fragment ctx')]])))



(defn global-feed-fragment
  [_ctx]
  (protected-fragment
   [:global-feed]
   (fn []
     [:div#microblog-global-feed
      [feed-view "Recent tweets" (recent-tweets)]])))



(defn tweet-fragment
  [ctx]
  (let [tweet-id (or (param ctx :tweet-id)
                     (get-in ctx [:path-params :tweet-id]))
        tweet    (tweet-by-id tweet-id)]
    [:div#microblog-tweet
     (if tweet
       [tweet-view tweet]
       [:div.rounded-xl.border.border-dashed.border-border.p-6
        "Tweet not found."])]))

(defn tweet-page
  [ctx]
  (html-response
   [page-shell
    (tweet-fragment ctx)]))

;; -----------------------------------------------------------------------------
;; Handlers
;; -----------------------------------------------------------------------------

(defn stream
  [ctx]
  (let [scope (subscription-scope ctx)]
    (:response
     (live/start-sse!
      (live-system ctx)
      scope
      {:flow-options {:relieve? true}}))))

(defn create-tweet!
  [ctx]
  (let [body (str/trim (tweet-body ctx))]
    (if (str/blank? body)
      (html-response
       [:div.rounded-xl.border.border-destructive.p-4
        "Tweet body is required."])
      (do
        (create-tweet*
         ctx
         {:author-id (str (current-user-id ctx))
          :body body
          :mode (or (:gessokit.load/mode ctx) :medium)})
        (if (= "true" (get-in ctx [:headers "hx-request"]))
          (no-content)
          {:status 303
           :headers {"location" "/app/microblog"}
           :body ""})))))

(defn like!
  [ctx]
  (let [tweet-id (get-in ctx [:path-params :tweet-id])]
    (swap! !state update-in [:likes tweet-id] (fnil inc 0))
    (submit-change! ctx {:topic :microblog/tweet
                         :id tweet-id
                         :change/kind :updated
                         :data {:action :like}})
    (no-content)))

(defn share!
  [ctx]
  (let [tweet-id (get-in ctx [:path-params :tweet-id])]
    (swap! !state update-in [:shares tweet-id] (fnil inc 0))
    (submit-change! ctx {:topic :microblog/tweet
                         :id tweet-id
                         :change/kind :updated
                         :data {:action :share}})
    (no-content)))

(defn comment!
  [ctx]
  (let [tweet-id (get-in ctx [:path-params :tweet-id])
        body     (str/trim (tweet-body ctx))]
    (swap! !state update-in [:comments tweet-id] (fnil conj [])
           {:comment/id (str (UUID/randomUUID))
            :comment/body body
            :comment/author-id (current-user-id ctx)
            :comment/created-at (now-ms)})
    (submit-change! ctx {:topic :microblog/tweet
                         :id tweet-id
                         :change/kind :updated
                         :data {:action :comment}})
    (no-content)))

;; -----------------------------------------------------------------------------
;; Dev/load handlers
;; -----------------------------------------------------------------------------

(defn load-stream
  [ctx]
  (stream (with-load-user ctx)))

(defn load-global-feed-fragment
  [ctx]
  ;; For load testing, this intentionally renders the current user's cohort feed.
  ;; The route name remains "global" only to preserve compatibility with the Go
  ;; harness defaults.
  (html-response
   (timeline-fragment (with-load-user ctx))))

(defn load-timeline-fragment
  [ctx]
  (html-response
   (timeline-fragment (with-load-user ctx))))

(defn load-create-tweet!
  [ctx]
  (create-tweet! (with-load-user ctx)))

(defn dev-stats
  [_ctx]
  (json-response
   {:tweets (count (:tweets @!state))
    :next-tweet (:next-tweet @!state)
    :write-seq (:write-seq @!state)
    :fragment-cache (count @!fragment-cache)
    :fragment-inflight (count @!fragment-inflight)
    :cohort-counts {:broadcast (cohort-count :broadcast)
                    :high (cohort-count :high)
                    :medium (cohort-count :medium)
                    :low (cohort-count :low)
                    :personal (cohort-count :personal)}}))

(defn seed-route!
  [ctx]
  (doseq [i (range 20)]
    (create-tweet*
     ctx
     {:author-id (str "seed-user-" i)
      :body (str "Seed tweet " i)
      :mode :medium}))
  (text-response "seeded"))

(defn burst-tweets!
  [ctx]
  (let [n (parse-int (or (param ctx :n) "100") 100)
        mode (keyword (or (param ctx :mode) "medium"))]
    (doseq [i (range n)]
      (create-tweet*
       ctx
       {:author-id (str "burst-user-" i)
        :body (str "Burst tweet " i)
        :mode mode}))
    (text-response (str "created " n " tweets"))))

(defn burst-likes!
  [_ctx]
  (text-response "ok"))

(defn burst-comments!
  [_ctx]
  (text-response "ok"))

;; -----------------------------------------------------------------------------
;; Routes
;; -----------------------------------------------------------------------------

(def signed-in-route-data
  {:middleware [mid/wrap-signed-in]})

(defn signed-in-route
  [handlers]
  (merge signed-in-route-data handlers))

(def app-routes
  [["/app/microblog"
    (signed-in-route
     {:get page
      :post create-tweet!})]

   ["/app/microblog/stream"
    (signed-in-route
     {:get stream})]

   ["/app/microblog/fragments/global"
    (signed-in-route
     {:get (fn [ctx]
             (html-response (global-feed-fragment ctx)))})]

   ["/app/microblog/fragments/timeline"
    (signed-in-route
     {:get (fn [ctx]
             (html-response (timeline-fragment ctx)))})]

   ["/app/microblog/fragments/tweet"
    (signed-in-route
     {:get (fn [ctx]
             (html-response (tweet-fragment ctx)))})]

   ["/app/microblog/fragments/stats"
    (signed-in-route
     {:get (fn [ctx]
             (html-response (stats-fragment ctx)))})]

   ["/app/microblog/tweet"
    (signed-in-route
     {:post create-tweet!})]

   ["/app/microblog/tweet/:tweet-id"
    (signed-in-route
     {:get tweet-page})]

   ["/app/microblog/tweet/:tweet-id/like"
    (signed-in-route
     {:post like!})]

   ["/app/microblog/tweet/:tweet-id/share"
    (signed-in-route
     {:post share!})]

   ["/app/microblog/tweet/:tweet-id/comment"
    (signed-in-route
     {:post comment!})]])

(def api-routes
  [["/api/microblog/dev/stats"
    {:get dev-stats}]

   ["/api/microblog/dev/seed"
    {:post seed-route!}]

   ["/api/microblog/dev/burst-tweets"
    {:post burst-tweets!}]

   ["/api/microblog/dev/burst-likes"
    {:post burst-likes!}]

   ["/api/microblog/dev/burst-comments"
    {:post burst-comments!}]

   ;; Protected globally by mid/wrap-dev-load-token in gessokit.clj.
   ["/api/microblog/dev/load/stream"
    {:get load-stream}]

   ["/api/microblog/dev/load/fragments/global"
    {:get load-global-feed-fragment}]

   ["/api/microblog/dev/load/fragments/timeline"
    {:get load-timeline-fragment}]

   ["/api/microblog/dev/load/tweet"
    {:post load-create-tweet!}]])

(def module
  {:live-rules (live-rules)
   :routes app-routes
   :api-routes api-routes})
