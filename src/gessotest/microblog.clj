(ns gessotest.microblog
  "Fake micro-Twitter workload for Gesso Live.

   This is intentionally not a real product feature. It is an app-shaped load
   test/demo for:

   - many long-lived SSE clients
   - low fanout vs high fanout
   - hot object updates
   - fragment refresh pressure
   - slow/reconnecting clients
   - bursty write traffic"
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [gesso.live.core :as live]
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

;; -----------------------------------------------------------------------------
;; O(1) Denormalized State
;; -----------------------------------------------------------------------------

(defonce !state
  (atom {:next-id 0
         :users {}
         :tweets {}
         :likes {}         ;; tweet-id -> count
         :shares {}        ;; tweet-id -> count
         :comments {}      ;; tweet-id -> [comments]
         :follows {}       ;; user-id -> set of following
         :followers {}     ;; user-id -> set of followers
         :global-feed ()   ;; list of tweet-ids (max 50)
         :timelines {}     ;; user-id -> list of tweet-ids (max 50)
         :profile-feeds {} ;; user-id -> list of tweet-ids (max 50)
         :created-at (System/currentTimeMillis)}))

(defn now-ms [] (System/currentTimeMillis))

(defn reset-state! []
  (reset! !state
          {:next-id 0
           :users {}
           :tweets {}
           :likes {}
           :shares {}
           :comments {}
           :follows {}
           :followers {}
           :global-feed ()
           :timelines {}
           :profile-feeds {}
           :created-at (now-ms)})
  :reset)

(defn next-id! [prefix]
  (let [n (:next-id (swap! !state update :next-id inc))]
    (str prefix "-" n)))

(defn make-user [n]
  (let [id (str "user-" n)]
    {:xt/id id :user/id id :user/name (str "User " n) :user/handle (str "user" n)}))

(defn user [user-id]
  (get-in @!state [:users user-id]
          {:xt/id user-id :user/id user-id :user/name user-id :user/handle user-id}))

(defn tweet [tweet-id] (get-in @!state [:tweets tweet-id]))

;; O(1) Fast Reads
(defn comments-for [tweet-id] (get-in @!state [:comments tweet-id] []))
(defn like-count [tweet-id] (get-in @!state [:likes tweet-id] 0))
(defn share-count [tweet-id] (get-in @!state [:shares tweet-id] 0))
(defn comment-count [tweet-id] (count (comments-for tweet-id)))

(defn hydrate-tweets [tweet-ids]
  (let [state @!state]
    (keep #(get-in state [:tweets %]) tweet-ids)))

(defn global-feed-tweets [] (hydrate-tweets (:global-feed @!state)))
(defn timeline-tweets [user-id] (hydrate-tweets (get-in @!state [:timelines user-id] ())))
(defn profile-tweets [user-id] (hydrate-tweets (get-in @!state [:profile-feeds user-id] ())))

(defn- push-feed [feed tweet-id]
  (take max-rendered-tweets (conj (or feed ()) tweet-id)))

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

(defn submit-change! [ctx change]
  (live/submit-expanded!
   (live-system ctx) ctx change
   {:coalesce-key [(:topic change) (or (:id change) (:tweet-id change) (:author-id change) (:user-id change))]}))

;; -----------------------------------------------------------------------------
;; Seed data
;; -----------------------------------------------------------------------------

(declare state-summary)

(defn seed!
  [{:keys [user-count following-count tweet-count]
    :or {user-count default-user-count following-count default-following-count tweet-count default-seed-tweet-count}}]
  (reset-state!)
  (let [users' (mapv make-user (range 1 (inc user-count)))
        user-ids (mapv :user/id users')]
    (swap! !state assoc :users (into {} (map (juxt :user/id identity)) users'))

    ;; Build follows
    (let [follow-pairs (for [idx (range user-count)
                             offset (range 1 (inc following-count))
                             :let [follower (nth user-ids idx)
                                   followed (nth user-ids (mod (+ idx offset) user-count))]
                             :when (not= follower followed)]
                         [follower followed])]
      (swap! !state
             (fn [s]
               (reduce (fn [acc [follower followed]]
                         (-> acc
                             (update-in [:follows follower] (fnil conj #{}) followed)
                             (update-in [:followers followed] (fnil conj #{}) follower)))
                       s follow-pairs))))

    ;; Seed tweets
    (doseq [n (range tweet-count)]
      (let [author-id (nth user-ids (mod n user-count))
            id (next-id! "tweet")
            t {:xt/id id :tweet/id id
               :tweet/body (str "Seed tweet " n " from @" (:user/handle (user author-id)))
               :tweet/author-id author-id
               :tweet/created-at (+ (now-ms) n)}]
        (swap! !state
               (fn [s]
                 (let [followers (get-in s [:followers author-id] #{})]
                   (-> s
                       (assoc-in [:tweets id] t)
                       (update :global-feed push-feed id)
                       (update-in [:profile-feeds author-id] push-feed id)
                       (update-in [:timelines author-id] push-feed id) ; see own tweets
                       (as-> s2 (reduce (fn [acc follower-id]
                                          (update-in acc [:timelines follower-id] push-feed id))
                                        s2 followers)))))))))
  {:status :seeded :stats (select-keys (state-summary) [:user-count :tweet-count :follow-count])})

(defn ensure-seeded! []
  (when (empty? (:users @!state)) (seed! {}))
  @!state)

;; -----------------------------------------------------------------------------
;; Live rules
;; -----------------------------------------------------------------------------

(defn tweet-created-invalidations [{:keys [tweet-id author-id]}]
  (let [followers (get-in @!state [:followers author-id] #{})]
    (vec
     (concat
      [{:topic :microblog/global-feed :id "global" :change/kind :updated}
       {:topic :microblog/profile :id author-id :change/kind :updated}
       {:topic :microblog/tweet :id tweet-id :change/kind :created}]
      (for [follower-id followers]
        {:topic :microblog/timeline :id follower-id :change/kind :updated})))))

(defn tweet-hot-object-invalidations [{:keys [tweet-id author-id user-id]}]
  (vec
   (remove nil?
           [{:topic :microblog/tweet :id tweet-id :change/kind :updated}
            (when author-id {:topic :microblog/notifications :id author-id :change/kind :updated})
            (when user-id {:topic :microblog/profile :id user-id :change/kind :updated})])))

(defn live-rules []
  [{:when-topic :microblog/tweet-created :expand (fn [_ change] (tweet-created-invalidations change))}
   {:when-topic :microblog/tweet-liked :expand (fn [_ change] (tweet-hot-object-invalidations change))}
   {:when-topic :microblog/tweet-commented :expand (fn [_ change] (tweet-hot-object-invalidations change))}
   {:when-topic :microblog/tweet-shared
    :expand (fn [_ change]
              (let [followers (get-in @!state [:followers (:user-id change)] #{})]
                (vec (concat (tweet-hot-object-invalidations change)
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

(defn user-label [user-id]
  (let [{:user/keys [name handle]} (user user-id)]
    (str name " @" handle)))

(defn tweet-card [ctx t]
  (let [tweet-id (:tweet/id t) author-id (:tweet/author-id t)]
    [:article {:id (str "tweet-card-" tweet-id) :class "rounded-xl border border-border bg-card p-4 shadow-sm space-y-3"}
     [:div {:class "flex items-start justify-between gap-4"}
      [:div [:div {:class "font-heading font-semibold"} (user-label author-id)]
       [:div {:class "text-xs text-muted-foreground"} tweet-id]]
      [:a {:href (str base-path "/tweet/" tweet-id) :class "text-sm underline"} "detail"]]
     [:p {:class "font-body leading-body"} (:tweet/body t)]
     [:div {:class "flex flex-wrap items-center gap-2 text-sm"}
      (live/post-button ctx (tweet-descriptor tweet-id)
       {:to (str base-path "/tweet/" tweet-id "/like") :label (str "♥ " (like-count tweet-id)) :button-attrs {:class (button-class)}})
      (live/post-button ctx (tweet-descriptor tweet-id)
       {:to (str base-path "/tweet/" tweet-id "/share") :label (str "↻ " (share-count tweet-id)) :button-attrs {:class (button-class)}})
      [:span {:class "rounded-lg bg-muted px-3 py-1.5 text-sm"} "💬 " (comment-count tweet-id)]]]))

(defn tweet-list [ctx tweets']
  (if (seq tweets')
    [:div {:class "space-y-3"}
     (for [t tweets'] ^{:key (:tweet/id t)} (tweet-card ctx t))]
    [:div {:class "rounded-xl border border-dashed border-border p-6 text-center text-muted-foreground"} "No tweets yet."]))

(defn global-feed-fragment [ctx]
  (ensure-seeded!)
  [:section {:class "space-y-3"}
   [:h3 {:class "font-heading text-lg font-semibold"} "Global feed"]
   (tweet-list ctx (global-feed-tweets))])

(defn timeline-fragment [ctx]
  (ensure-seeded!)
  (let [user-id (or (param ctx :user-id) (current-user-id ctx))]
    [:section {:class "space-y-3"}
     [:h3 {:class "font-heading text-lg font-semibold"} "Timeline: " user-id]
     (tweet-list ctx (timeline-tweets user-id))]))

(defn tweet-fragment [ctx]
  (ensure-seeded!)
  (let [tweet-id (param ctx :tweet-id)]
    (if-let [t (tweet tweet-id)]
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
         (for [c (comments-for tweet-id)]
           ^{:key (:comment/id c)}
           [:div {:class "rounded-lg bg-muted px-3 py-2 text-sm"}
            [:span {:class "font-medium"} (user-label (:comment/author-id c))] ": " (:comment/body c)])]]]
      [:div {:class "rounded-xl border border-border bg-card p-4"} "Missing tweet " tweet-id])))

(defn stats-fragment [_ctx]
  (ensure-seeded!)
  (let [stats (state-summary)]
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
  (ensure-seeded!)
  (let [user-id (current-user-id ctx)]
    (ui/page-shell ctx
      [:section {:class "mx-auto max-w-5xl py-8 space-y-6"}
       [:div {:class "space-y-2"}
        [:h1 {:class "font-heading text-3xl font-bold"} "Microblog Live Load Demo"]
        [:p {:class "text-muted-foreground"} "Fake Twitter-shaped workload for fanout, hot objects, slow clients, and reconnect behavior."]
        [:p {:class "text-sm text-muted-foreground"} "Current synthetic user: " [:code user-id]]]
       (compose-form ctx)
       [:div {:class "grid gap-4 lg:grid-cols-[1fr_1fr]"}
        [:div {:class "space-y-4"} (live/fragment-panel (global-feed-descriptor))]
        [:div {:class "space-y-4"}
         (live/fragment-panel (timeline-descriptor user-id))
         (live/fragment-panel (stats-descriptor))]]])))

(defn tweet-page [ctx]
  (ensure-seeded!)
  (let [tweet-id (param ctx :tweet-id)]
    (ui/page-shell ctx
      [:section {:class "mx-auto max-w-3xl py-8 space-y-6"}
       [:a {:href base-path :class "text-sm underline"} "← Microblog"]
       (live/fragment-panel (tweet-descriptor tweet-id))])))

;; -----------------------------------------------------------------------------
;; Mutations (Route Handlers)
;; -----------------------------------------------------------------------------

(declare global-feed-route)
(declare tweet-route)

(defn create-tweet! [ctx]
  (ensure-seeded!)
  (let [author-id (current-user-id ctx)
        body (str/trim (or (param ctx :body) ""))
        body' (if (str/blank? body) (str "Synthetic tweet at " (now-ms)) body)
        id (next-id! "tweet")
        t {:xt/id id :tweet/id id :tweet/body body' :tweet/author-id author-id :tweet/created-at (now-ms)}]
    (swap! !state
           (fn [s]
             (let [followers (get-in s [:followers author-id] #{})]
               (-> s
                   (assoc-in [:tweets id] t)
                   (update :global-feed push-feed id)
                   (update-in [:profile-feeds author-id] push-feed id)
                   (update-in [:timelines author-id] push-feed id)
                   (as-> s2 (reduce (fn [acc follower-id] (update-in acc [:timelines follower-id] push-feed id)) s2 followers))))))
    (submit-change! ctx {:topic :microblog/tweet-created :id id :tweet-id id :author-id author-id :change/kind :created})
    (global-feed-route ctx)))

(defn like! [ctx]
  (ensure-seeded!)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        author-id (:tweet/author-id (tweet tweet-id))]
    (swap! !state (fn [s] (update-in s [:likes tweet-id] (fnil inc 0))))
    (submit-change! ctx {:topic :microblog/tweet-liked :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated})
    (tweet-route (assoc-in ctx [:params :tweet-id] tweet-id))))

(defn share! [ctx]
  (ensure-seeded!)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        author-id (:tweet/author-id (tweet tweet-id))]
    (swap! !state (fn [s] (update-in s [:shares tweet-id] (fnil inc 0))))
    (submit-change! ctx {:topic :microblog/tweet-shared :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated})
    (tweet-route (assoc-in ctx [:params :tweet-id] tweet-id))))

(defn comment! [ctx]
  (ensure-seeded!)
  (let [tweet-id (param ctx :tweet-id)
        user-id (current-user-id ctx)
        body (str/trim (or (param ctx :body) ""))
        comment-id (next-id! "comment")
        author-id (:tweet/author-id (tweet tweet-id))
        c {:xt/id comment-id :comment/id comment-id :comment/tweet-id tweet-id :comment/author-id user-id
           :comment/body (if (str/blank? body) (str "Synthetic comment at " (now-ms)) body) :comment/created-at (now-ms)}]
    (swap! !state (fn [s] (update-in s [:comments tweet-id] (fnil conj []) c)))
    (submit-change! ctx {:topic :microblog/tweet-commented :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated})
    (tweet-route (assoc-in ctx [:params :tweet-id] tweet-id))))

;; -----------------------------------------------------------------------------
;; SSE / fragments (Route Handlers with Caching & Singleflight)
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

(defn state-summary []
  (let [state @!state]
    {:user-count (count (:users state))
     :tweet-count (count (:tweets state))
     :comment-count (count (mapcat val (:comments state)))
     :like-count (reduce + (vals (:likes state)))
     :share-count (reduce + (vals (:shares state)))
     :follow-count (reduce + (map count (vals (:follows state))))
     :latest-client-time (:created-at state)}))

(defn dev-stats [ctx]
  {:status 200
   :headers {"content-type" "application/edn; charset=utf-8"}
   :body (pr-str {:microblog (state-summary) :live (live/stats (live-system ctx))})})

(defn seed-route! [ctx]
  (let [user-count (parse-long (param ctx :users) default-user-count)
        following-count (parse-long (param ctx :following) default-following-count)
        tweet-count (parse-long (param ctx :tweets) default-seed-tweet-count)]
    (seed! {:user-count user-count :following-count following-count :tweet-count tweet-count})
    (submit-change! ctx {:topic :microblog/tweet-created :id "seed" :tweet-id "seed" :author-id "user-1" :change/kind :updated})
    (dev-stats ctx)))

(defn burst-tweets! [ctx]
  (ensure-seeded!)
  (let [n (parse-long (param ctx :n) 100)
        author-id (str (or (param ctx :author-id) "user-1"))]
    (dotimes [i n]
      (let [id (next-id! "tweet")
            t {:xt/id id :tweet/id id :tweet/body (str "Burst tweet " i " at " (now-ms))
               :tweet/author-id author-id :tweet/created-at (+ (now-ms) i)}]
        (swap! !state
               (fn [s]
                 (let [followers (get-in s [:followers author-id] #{})]
                   (-> s
                       (assoc-in [:tweets id] t)
                       (update :global-feed push-feed id)
                       (update-in [:profile-feeds author-id] push-feed id)
                       (update-in [:timelines author-id] push-feed id)
                       (as-> s2 (reduce (fn [acc follower-id] (update-in acc [:timelines follower-id] push-feed id)) s2 followers))))))
        (submit-change! ctx {:topic :microblog/tweet-created :id id :tweet-id id :author-id author-id :change/kind :created})))
    (dev-stats ctx)))

(defn random-tweet-id [] (some-> (seq (keys (:tweets @!state))) rand-nth))

(defn burst-likes! [ctx]
  (ensure-seeded!)
  (let [n (parse-long (param ctx :n) 100)
        tweet-id (or (param ctx :tweet-id) (random-tweet-id) "missing")
        author-id (:tweet/author-id (tweet tweet-id))]
    (dotimes [i n]
      (let [user-id (str "load-user-" i)]
        (swap! !state (fn [s] (update-in s [:likes tweet-id] (fnil inc 0))))
        (submit-change! ctx {:topic :microblog/tweet-liked :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated})))
    (dev-stats ctx)))

(defn burst-comments! [ctx]
  (ensure-seeded!)
  (let [n (parse-long (param ctx :n) 100)
        tweet-id (or (param ctx :tweet-id) (random-tweet-id) "missing")
        author-id (:tweet/author-id (tweet tweet-id))]
    (dotimes [i n]
      (let [user-id (str "load-user-" i)
            comment-id (next-id! "comment")
            c {:xt/id comment-id :comment/id comment-id :comment/tweet-id tweet-id :comment/author-id user-id
               :comment/body (str "Burst comment " i) :comment/created-at (+ (now-ms) i)}]
        (swap! !state (fn [s] (update-in s [:comments tweet-id] (fnil conj []) c)))
        (submit-change! ctx {:topic :microblog/tweet-commented :id tweet-id :tweet-id tweet-id :author-id author-id :user-id user-id :change/kind :updated})))
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
   ["/api/microblog/dev/burst-likes" {:post burst-likes!}]
   ["/api/microblog/dev/burst-comments" {:post burst-comments!}]
   ["/api/microblog/dev/load/stream" {:get load-stream}]
   ["/api/microblog/dev/load/fragments/global" {:get load-global-feed-fragment}]
   ["/api/microblog/dev/load/fragments/timeline" {:get load-timeline-fragment}]
   ["/api/microblog/dev/load/tweet" {:post load-create-tweet!}]])

(def module {:live-rules (live-rules) :routes app-routes :api-routes api-routes})
