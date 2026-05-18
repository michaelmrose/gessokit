(ns gessotest.microblog
  "Fake micro-Twitter workload for Gesso Live.

   This load test exercises the entire stack:
   HTMX -> SSE -> Gesso Live (Missionary/Manifold) -> XTDB v2.

   It tests:
   - High fanout vs Low fanout
   - XTDB read-after-write consistency (via transact-and-notify!)
   - Database protection (via singleflight caching)
   - Bursty write traffic and concurrent reads"
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [gesso.live.core :as live]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gessotest.middleware :as mid]
   [gessotest.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Constants
;; -----------------------------------------------------------------------------

(def base-path "/app/microblog")
(def default-user-count 100)
(def default-following-count 20)
(def default-seed-tweet-count 200)
(def max-rendered-tweets 50)

(defn now-ms [] (System/currentTimeMillis))

(defn random-id [prefix]
  (str prefix "-" (random-uuid)))

;; -----------------------------------------------------------------------------
;; XTDB Queries (The Read Model)
;; -----------------------------------------------------------------------------
;; Note: XTDB2 maps Clojure keys like :author-id to SQL columns like author_id

(defn global-feed-tweets [ctx]
  (live.xtdb/q-consistent-from ctx
   "SELECT _id, tweet_id, body, author_id, created_at, like_count, share_count, comment_count
    FROM microblog_tweets
    ORDER BY created_at DESC
    LIMIT ?"
   [max-rendered-tweets]))

(defn timeline-tweets [ctx user-id]
  (live.xtdb/q-consistent-from ctx
   "SELECT t._id, t.tweet_id, t.body, t.author_id, t.created_at, t.like_count, t.share_count, t.comment_count
    FROM microblog_tweets t
    WHERE t.author_id = ?
       OR t.author_id IN (SELECT f.followed_id FROM microblog_follows f WHERE f.follower_id = ?)
    ORDER BY t.created_at DESC
    LIMIT ?"
   [user-id user-id max-rendered-tweets]))

(defn tweet [ctx tweet-id]
  (first
   (live.xtdb/q-consistent-from ctx
    "SELECT _id, tweet_id, body, author_id, created_at, like_count, share_count, comment_count
     FROM microblog_tweets
     WHERE _id = ?"
    [tweet-id])))

(defn comments-for [ctx tweet-id]
  (live.xtdb/q-consistent-from ctx
   "SELECT _id, comment_id, author_id, body, created_at
    FROM microblog_comments
    WHERE tweet_id = ?
    ORDER BY created_at ASC"
   [tweet-id]))

(defn follower-ids [ctx author-id]
  (->> (live.xtdb/q-consistent-from ctx
        "SELECT follower_id FROM microblog_follows WHERE followed_id = ?"
        [author-id])
       (map :follower_id)
       set))

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------


(defn param [ctx k]
  (or (get-in ctx [:params k]) (get-in ctx [:params (name k)])
      (get-in ctx [:query-params k]) (get-in ctx [:query-params (name k)])
      (get-in ctx [:form-params k]) (get-in ctx [:form-params (name k)])
      (get-in ctx [:path-params k]) (get-in ctx [:path-params (name k)])))

(defn parse-long [x default]
  (try (if (some? x) (Long/parseLong (str x)) default) (catch Exception _ default)))

(defn current-user-id [ctx]
  (str (or (param ctx :user-id) (:user/id ctx) (get-in ctx [:user :xt/id])
           (get-in ctx [:session :user]) (get-in ctx [:session :uid]) "user-1")))

(defn live-system [ctx]
  (or (:gesso.live/system ctx)
      (throw (ex-info "microblog requires :gesso.live/system in ctx." {}))))

(defn submit-change!
  [ctx change]
  (live/submit-expanded!
   (live-system ctx)
   ctx
   change
   {:coalesce-key [(:topic change)
                   (or (:id change)
                       (:tweet-id change)
                       (:author-id change)
                       (:user-id change))]}))
;; -----------------------------------------------------------------------------
;; Seed data (XTDB Writes)
;; -----------------------------------------------------------------------------

(declare state-summary)

(defn seed!
  [ctx {:keys [user-count following-count tweet-count]
        :or {user-count default-user-count following-count default-following-count tweet-count default-seed-tweet-count}}]

  ;; 1. Generate Users
  (let [users (mapv (fn [n]
                      (let [id (str "user-" n)]
                        {:xt/id id :user_id id :handle (str "user" n)}))
                    (range 1 (inc user-count)))
        user-ids (mapv :user_id users)

        ;; 2. Generate Follows
        follows (for [idx (range user-count)
                      offset (range 1 (inc following-count))
                      :let [follower (nth user-ids idx)
                            followed (nth user-ids (mod (+ idx offset) user-count))]
                      :when (not= follower followed)]
                  {:xt/id (random-id "follow") :follower_id follower :followed_id followed})

        ;; 3. Generate Tweets
        tweets (mapv (fn [n]
                       (let [author-id (nth user-ids (mod n user-count))
                             id (str "tweet-" n)]
                         {:xt/id id :tweet_id id
                          :body (str "Seed tweet " n " from @" author-id)
                          :author_id author-id :created_at (+ (now-ms) n)
                          :like_count 0 :share_count 0 :comment_count 0}))
                     (range tweet-count))]

    ;; Execute the massive seed transaction
    (live.xtdb/execute-tx-from! ctx
     (concat
      [[:delete-docs :microblog_users]  ;; Optional: wipe old state if re-seeding
       [:delete-docs :microblog_follows]
       [:delete-docs :microblog_tweets]
       [:delete-docs :microblog_comments]]
      (mapv #(into [:put-docs :microblog_users] [%]) users)
      (mapv #(into [:put-docs :microblog_follows] [%]) follows)
      (mapv #(into [:put-docs :microblog_tweets] [%]) tweets))))

  {:status :seeded :stats (state-summary ctx)})

(defn ensure-seeded! [ctx]
  (let [count-res (live.xtdb/q-consistent-from ctx "SELECT COUNT(*) as c FROM microblog_users")]
    (when (or (empty? count-res) (zero? (:c (first count-res))))
      (seed! ctx {}))))

;; -----------------------------------------------------------------------------
;; Live rules
;; -----------------------------------------------------------------------------

(defn tweet-created-invalidations [ctx {:keys [tweet-id author-id]}]
  (let [followers (follower-ids ctx author-id)]
    (vec
     (concat
      [{:topic :microblog/global-feed :id "global" :change/kind :updated}
       {:topic :microblog/profile :id author-id :change/kind :updated}
       {:topic :microblog/tweet :id tweet-id :change/kind :created}]
      (for [follower-id followers]
        {:topic :microblog/timeline :id follower-id :change/kind :updated})))))

(defn tweet-hot-object-invalidations [ctx {:keys [tweet-id author-id user-id]}]
  (vec
   (remove nil?
           [{:topic :microblog/tweet :id tweet-id :change/kind :updated}
            (when author-id {:topic :microblog/notifications :id author-id :change/kind :updated})
            (when user-id {:topic :microblog/profile :id user-id :change/kind :updated})])))

(defn live-rules []
  [{:when-topic :microblog/tweet-created :expand (fn [ctx change] (tweet-created-invalidations ctx change))}
   {:when-topic :microblog/tweet-liked :expand (fn [ctx change] (tweet-hot-object-invalidations ctx change))}
   {:when-topic :microblog/tweet-commented :expand (fn [ctx change] (tweet-hot-object-invalidations ctx change))}
   {:when-topic :microblog/tweet-shared
    :expand (fn [ctx change]
              (let [followers (follower-ids ctx (:user-id change))]
                (vec (concat (tweet-hot-object-invalidations ctx change)
                             (for [follower-id followers]
                               {:topic :microblog/timeline :id follower-id :change/kind :updated})))))}])

;; -----------------------------------------------------------------------------
;; Fragment descriptors
;; -----------------------------------------------------------------------------

(defn stream-url [{:keys [topic id]}]
  (str base-path "/stream?topic=" (java.net.URLEncoder/encode (name topic) "UTF-8")
       "&id=" (java.net.URLEncoder/encode (str id) "UTF-8")))

(defn fragment-url [path params]
  (let [pairs (seq params)]
    (str base-path path
         (when pairs (str "?" (str/join "&" (for [[k v] pairs]
                                              (str (java.net.URLEncoder/encode (name k) "UTF-8") "="
                                                   (java.net.URLEncoder/encode (str v) "UTF-8")))))))))

(defn live-fragment [{:keys [id src subscription swap] :or {swap :innerHTML}}]
  (live/->fragment {:id id :src src :stream-url (stream-url subscription) :subscription subscription :swap swap}))

(defn global-feed-descriptor []
  (live-fragment {:id "microblog-global-feed" :src (fragment-url "/fragments/global" nil)
                  :subscription {:topic :microblog/global-feed :id "global"}}))

(defn timeline-descriptor [user-id]
  (live-fragment {:id (str "microblog-timeline-" user-id) :src (fragment-url "/fragments/timeline" {:user-id user-id})
                  :subscription {:topic :microblog/timeline :id user-id}}))

(defn tweet-descriptor [tweet-id]
  (live-fragment {:id (str "microblog-tweet-" tweet-id) :src (fragment-url "/fragments/tweet" {:tweet-id tweet-id})
                  :subscription {:topic :microblog/tweet :id tweet-id}}))

(defn stats-descriptor []
  (live-fragment {:id "microblog-stats" :src (fragment-url "/fragments/stats" nil)
                  :subscription {:topic :microblog/global-feed :id "global"}}))

;; -----------------------------------------------------------------------------
;; Rendering (Hiccup Views)
;; -----------------------------------------------------------------------------

(defn button-class [] "inline-flex items-center justify-center rounded-lg border border-border bg-background px-3 py-1.5 text-sm font-medium hover:bg-muted")
(defn text-input-class [] "w-full rounded-lg border border-border bg-background px-3 py-2 text-sm")

(defn tweet-card [ctx t]
  (let [tweet-id (:tweet_id t) author-id (:author_id t)]
    [:article {:id (str "tweet-card-" tweet-id) :class "rounded-xl border border-border bg-card p-4 shadow-sm space-y-3"}
     [:div {:class "flex items-start justify-between gap-4"}
      [:div [:div {:class "font-heading font-semibold"} (str "@" author-id)]
       [:div {:class "text-xs text-muted-foreground"} tweet-id]]
      [:a {:href (str base-path "/tweet/" tweet-id) :class "text-sm underline"} "detail"]]
     [:p {:class "font-body leading-body"} (:body t)]
     [:div {:class "flex flex-wrap items-center gap-2 text-sm"}
      (live/post-button ctx (tweet-descriptor tweet-id)
       {:to (str base-path "/tweet/" tweet-id "/like") :label (str "♥ " (:like_count t)) :button-attrs {:class (button-class)}})
      (live/post-button ctx (tweet-descriptor tweet-id)
       {:to (str base-path "/tweet/" tweet-id "/share") :label (str "↻ " (:share_count t)) :button-attrs {:class (button-class)}})
      [:span {:class "rounded-lg bg-muted px-3 py-1.5 text-sm"} "💬 " (:comment_count t)]]]))

(defn tweet-list [ctx tweets']
  (if (seq tweets')
    [:div {:class "space-y-3"}
     (for [t tweets'] ^{:key (:tweet_id t)} (tweet-card ctx t))]
    [:div {:class "rounded-xl border border-dashed border-border p-6 text-center text-muted-foreground"} "No tweets yet."]))

(defn global-feed-fragment [ctx]
  (ensure-seeded! ctx)
  [:section {:class "space-y-3"}
   [:h3 {:class "font-heading text-lg font-semibold"} "Global feed"]
   (tweet-list ctx (global-feed-tweets ctx))])

(defn timeline-fragment [ctx]
  (ensure-seeded! ctx)
  (let [user-id (or (param ctx :user-id) (current-user-id ctx))]
    [:section {:class "space-y-3"}
     [:h3 {:class "font-heading text-lg font-semibold"} "Timeline: " user-id]
     (tweet-list ctx (timeline-tweets ctx user-id))]))

(defn tweet-fragment [ctx]
  (ensure-seeded! ctx)
  (let [tweet-id (param ctx :tweet-id)]
    (if-let [t (tweet ctx tweet-id)]
      [:section {:class "space-y-4"}
       (tweet-card ctx t)
       [:div {:class "rounded-xl border border-border bg-card p-4 space-y-3"}
        [:h3 {:class "font-heading text-lg font-semibold"} "Comments"]
        [:form {:method "post" :hx-post (str base-path "/tweet/" tweet-id "/comment")
                :hx-target (str "#microblog-tweet-" tweet-id) :hx-swap "innerHTML" :hx-sync "closest form:drop" :class "flex gap-2"}
         (live/anti-forgery-input ctx)
         [:input {:name "body" :placeholder "Write a comment" :class (text-input-class)}]
         [:button {:type "submit" :class (button-class)} "Comment"]]
        [:div {:class "space-y-2"}
         (for [c (comments-for ctx tweet-id)]
           ^{:key (:comment_id c)}
           [:div {:class "rounded-lg bg-muted px-3 py-2 text-sm"}
            [:span {:class "font-medium"} (str "@" (:author_id c))] ": " (:body c)])]]]
      [:div {:class "rounded-xl border border-border bg-card p-4"} "Missing tweet " tweet-id])))

(defn stats-fragment [ctx]
  (ensure-seeded! ctx)
  (let [stats (state-summary ctx)]
    [:section {:class "rounded-xl border border-border bg-card p-4"}
     [:h3 {:class "font-heading text-lg font-semibold"} "Microblog stats"]
     [:dl {:class "grid grid-cols-2 gap-2 text-sm"}
      (for [[k v] stats]
        ^{:key k}
        [:<> [:dt {:class "text-muted-foreground"} (name k)] [:dd {:class "font-mono"} (pr-str v)]])]]))

(defn compose-form [ctx]
  [:form {:method "post" :hx-post (str base-path "/tweet") :hx-target "#microblog-global-feed" :hx-swap "innerHTML" :hx-sync "closest form:drop"
          :class "rounded-xl border border-border bg-card p-4 space-y-3"}
   (live/anti-forgery-input ctx)
   [:input {:type "hidden" :name "user-id" :value (current-user-id ctx)}]
   [:textarea {:name "body" :placeholder "What's happening?" :class (str (text-input-class) " min-h-24")}]
   [:div {:class "flex justify-end"} [:button {:type "submit" :class (button-class)} "Post tweet"]]])

(defn page [ctx]
  (ensure-seeded! ctx)
  (let [user-id (current-user-id ctx)]
    (ui/page-shell ctx
      [:section {:class "mx-auto max-w-5xl py-8 space-y-6"}
       [:div {:class "space-y-2"}
        [:h1 {:class "font-heading text-3xl font-bold"} "Microblog Live Load Demo (XTDB Edition)"]
        [:p {:class "text-muted-foreground"} "Testing database scaling, XTDB read-after-write consistency, and singleflight cache protection."]
        [:p {:class "text-sm text-muted-foreground"} "Current synthetic user: " [:code user-id]]]
       (compose-form ctx)
       [:div {:class "grid gap-4 lg:grid-cols-[1fr_1fr]"}
        [:div {:class "space-y-4"} (live/fragment-panel (global-feed-descriptor))]
        [:div {:class "space-y-4"}
         (live/fragment-panel (timeline-descriptor user-id))
         (live/fragment-panel (stats-descriptor))]]])))

(defn tweet-page [ctx]
  (ensure-seeded! ctx)
  (let [tweet-id (param ctx :tweet-id)]
    (ui/page-shell ctx
      [:section {:class "mx-auto max-w-3xl py-8 space-y-6"}
       [:a {:href base-path :class "text-sm underline"} "← Microblog"]
       (live/fragment-panel (tweet-descriptor tweet-id))])))

;; -----------------------------------------------------------------------------
;; Mutations (Route Handlers with transact-and-notify!)
;; -----------------------------------------------------------------------------

(declare global-feed-route)
(declare tweet-route)

(defn create-tweet! [ctx]
  (ensure-seeded! ctx)
  (let [author-id (current-user-id ctx)
        body (str/trim (or (param ctx :body) ""))
        body' (if (str/blank? body) (str "Synthetic tweet at " (now-ms)) body)
        id (random-id "tweet")
        doc {:xt/id id :tweet_id id :body body' :author_id author-id :created_at (now-ms)
             :like_count 0 :share_count 0 :comment_count 0}

        ;; Execute TX and Notify SSE clients in one shot
        result (live/transact-and-notify!
                (live-system ctx) ctx
                {:tx-ops [[:put-docs :microblog_tweets doc]]
                 :change {:topic :microblog/tweet-created :id id :tweet-id id :author-id author-id :change/kind :created}})]

    ;; Use (:ctx result) so the global-feed fetch uses the exact XTDB snapshot-time of the TX we just ran
    (global-feed-route (:ctx result))))

(defn like! [ctx]
  (ensure-seeded! ctx)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        t (tweet ctx tweet-id)
        author-id (:author_id t)

        result (live/transact-and-notify!
                (live-system ctx) ctx
                ;; Warning: In a real app with concurrent likes, you would use an XTDB transaction function here
                ;; to increment safely. For this load test, we're just directly putting the updated doc.
                {:tx-ops [[:put-docs :microblog_tweets (update t :like_count (fnil inc 0))]]
                 :change {:topic :microblog/tweet-liked :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated}})]
    (tweet-route (assoc-in (:ctx result) [:params :tweet-id] tweet-id))))

(defn share! [ctx]
  (ensure-seeded! ctx)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        t (tweet ctx tweet-id)
        author-id (:author_id t)
        share-id (random-id "share")

        result (live/transact-and-notify!
                (live-system ctx) ctx
                {:tx-ops [[:put-docs :microblog_shares {:xt/id share-id :tweet_id tweet-id :user_id user-id}]
                          [:put-docs :microblog_tweets (update t :share_count (fnil inc 0))]]
                 :change {:topic :microblog/tweet-shared :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated}})]
    (tweet-route (assoc-in (:ctx result) [:params :tweet-id] tweet-id))))

(defn comment! [ctx]
  (ensure-seeded! ctx)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        body (str/trim (or (param ctx :body) ""))
        body' (if (str/blank? body) (str "Synthetic comment at " (now-ms)) body)
        t (tweet ctx tweet-id)
        author-id (:author_id t)
        comment-id (random-id "comment")
        c {:xt/id comment-id :comment_id comment-id :tweet_id tweet-id :author_id user-id :body body' :created_at (now-ms)}

        result (live/transact-and-notify!
                (live-system ctx) ctx
                {:tx-ops [[:put-docs :microblog_comments c]
                          [:put-docs :microblog_tweets (update t :comment_count (fnil inc 0))]]
                 :change {:topic :microblog/tweet-commented :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated}})]
    (tweet-route (assoc-in (:ctx result) [:params :tweet-id] tweet-id))))

;; -----------------------------------------------------------------------------
;; SSE / fragments (Route Handlers with XTDB Consistency Caching & Singleflight)
;; -----------------------------------------------------------------------------

(defn run-task-sync
  "Block the current Ring thread until the Missionary task completes."
  [task]
  (let [p (promise)
        _cancel! (task #(deliver p [:ok %]) #(deliver p [:error %]))
        [status val] @p]
    (if (= status :ok) val (throw val))))

(defn stream [ctx]
  (let [topic (keyword (or (param ctx :topic) "microblog/global-feed"))
        id (or (param ctx :id) "global")]
    (:response (live/start-sse! (live-system ctx) {:topic topic :id id} {:flow-options {:relieve? true}}))))

(defn global-feed-route [ctx]
  (run-task-sync
   (live/render-task
    (live-system ctx)
    (live/fragment-key "global-feed" {:consistency-token (:gesso.live/consistency ctx)})
    #(g/html-response (global-feed-fragment ctx))
    {:ttl-ms 500})))

(defn timeline-route [ctx]
  (let [user-id (current-user-id ctx)]
    (run-task-sync
     (live/render-task
      (live-system ctx)
      (live/fragment-key "timeline" {:user-key user-id :consistency-token (:gesso.live/consistency ctx)})
      #(g/html-response (timeline-fragment ctx))
      {:ttl-ms 500}))))

(defn tweet-route [ctx]
  (let [tweet-id (param ctx :tweet-id)]
    (run-task-sync
     (live/render-task
      (live-system ctx)
      (live/fragment-key "tweet" {:user-key tweet-id :consistency-token (:gesso.live/consistency ctx)})
      #(g/html-response (tweet-fragment ctx))
      {:ttl-ms 500}))))

(defn stats-route [ctx]
  (run-task-sync
   (live/render-task
    (live-system ctx)
    (live/fragment-key "stats" {:consistency-token (:gesso.live/consistency ctx)})
    #(g/html-response (stats-fragment ctx))
    {:ttl-ms 500})))

;; -----------------------------------------------------------------------------
;; Dev/load routes
;; -----------------------------------------------------------------------------

(defn state-summary [ctx]
  (let [c (fn [table] (:c (first (live.xtdb/q-consistent-from ctx (str "SELECT COUNT(*) as c FROM " (name table))))))]
    {:user-count (c :microblog_users)
     :tweet-count (c :microblog_tweets)
     :comment-count (c :microblog_comments)
     :follow-count (c :microblog_follows)}))

(defn dev-stats [ctx]
  {:status 200
   :headers {"content-type" "application/edn; charset=utf-8"}
   :body (pr-str {:microblog (state-summary ctx) :live (live/stats (live-system ctx))})})

(defn seed-route! [ctx]
  (let [user-count (parse-long (param ctx :users) default-user-count)
        following-count (parse-long (param ctx :following) default-following-count)
        tweet-count (parse-long (param ctx :tweets) default-seed-tweet-count)]
    (seed! ctx {:user-count user-count :following-count following-count :tweet-count tweet-count})
    (dev-stats ctx)))

(defn burst-tweets! [ctx]
  (ensure-seeded! ctx)
  (let [n (parse-long (param ctx :n) 100)
        author-id (str (or (param ctx :author-id) "user-1"))
        docs (mapv (fn [i]
                     (let [id (random-id "tweet")]
                       {:xt/id id :tweet_id id :body (str "Burst tweet " i " at " (now-ms))
                        :author_id author-id :created_at (+ (now-ms) i)
                        :like_count 0 :share_count 0 :comment_count 0}))
                   (range n))]
    (live.xtdb/execute-tx-from! ctx (mapv #(into [:put-docs :microblog_tweets] [%]) docs))
    ;; Notify
    (doseq [d docs]
      (submit-change! ctx {:topic :microblog/tweet-created :id (:tweet_id d) :tweet-id (:tweet_id d) :author-id author-id :change/kind :created}))
    (dev-stats ctx)))

;; -----------------------------------------------------------------------------
;; Dev/load-test helpers
;; -----------------------------------------------------------------------------

(defn load-param [ctx k]
  (or (get-in ctx [:params k]) (get-in ctx [:params (name k)])
      (get-in ctx [:query-params k]) (get-in ctx [:query-params (name k)])
      (get-in ctx [:form-params k]) (get-in ctx [:form-params (name k)])))

(defn load-user-id [ctx]
  (or (load-param ctx :user-id) (get-in ctx [:headers "x-load-user"]) "load-user-0"))

(defn load-mode [ctx]
  (keyword (or (load-param ctx :mode) (get-in ctx [:headers "x-load-mode"]) "high")))

(defn with-load-user [ctx]
  (let [user-id (str (load-user-id ctx)) mode (load-mode ctx)]
    (-> ctx
        (assoc :gessotest.load/user-id user-id :gessotest.load/mode mode :uid user-id :user/id user-id)
        (assoc-in [:session :uid] user-id))))

(defn load-stream [ctx] (stream (with-load-user ctx)))
(defn load-global-feed-fragment [ctx] (global-feed-route (with-load-user ctx)))
(defn load-timeline-fragment [ctx] (timeline-route (with-load-user ctx)))
(defn load-create-tweet! [ctx] (create-tweet! (with-load-user ctx)))

;; -----------------------------------------------------------------------------
;; Routes
;; -----------------------------------------------------------------------------

(def signed-in-route-data {:middleware [mid/wrap-signed-in]})
(defn signed-in-route [handlers] (merge signed-in-route-data handlers))

(def app-routes
  [["/app/microblog" (signed-in-route {:get page :post create-tweet!})]
   ["/app/microblog/stream" (signed-in-route {:get stream})]
   ["/app/microblog/fragments/global" (signed-in-route {:get global-feed-route})]
   ["/app/microblog/fragments/timeline" (signed-in-route {:get timeline-route})]
   ["/app/microblog/fragments/tweet" (signed-in-route {:get tweet-route})]
   ["/app/microblog/fragments/stats" (signed-in-route {:get stats-route})]
   ["/app/microblog/tweet" (signed-in-route {:post create-tweet!})]
   ["/app/microblog/tweet/:tweet-id" (signed-in-route {:get tweet-page})]
   ["/app/microblog/tweet/:tweet-id/like" (signed-in-route {:post like!})]
   ["/app/microblog/tweet/:tweet-id/share" (signed-in-route {:post share!})]
   ["/app/microblog/tweet/:tweet-id/comment" (signed-in-route {:post comment!})]])

(def api-routes
  [["/api/microblog/dev/stats" {:get dev-stats}]
   ["/api/microblog/dev/seed" {:post seed-route!}]
   ["/api/microblog/dev/burst-tweets" {:post burst-tweets!}]
   ["/api/microblog/dev/load/stream" {:get load-stream}]
   ["/api/microblog/dev/load/fragments/global" {:get load-global-feed-fragment}]
   ["/api/microblog/dev/load/fragments/timeline" {:get load-timeline-fragment}]
   ["/api/microblog/dev/load/tweet" {:post load-create-tweet!}]])

(def module {:live-rules (live-rules) :routes app-routes :api-routes api-routes})
