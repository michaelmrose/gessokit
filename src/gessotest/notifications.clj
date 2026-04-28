(ns gessotest.notifications
  "App-owned notification demo.

   This namespace demonstrates how an application can use:
   - gesso.live.transport.sse for flushed SSE streams
   - gesso.live.htmx for SSE -> HTMX callback markup
   - gesso.components.toaster for toast rendering

   This is intentionally app-owned. Gesso should not decide what counts as a
   notification, who receives it, whether it is durable, or how unread/read state
   works.

   This demo uses atoms. A real Humanhelp-style app should back important
   notifications with XTDB documents."
  (:require
   [gesso.core :as g]
   [gesso.live.htmx :as live-htmx]
   [gesso.live.transport.sse :as sse]
   [gessotest.middleware :as mid])
  (:import
   [java.time Instant]))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/notifications")

(def stream-path
  (str base-path "/stream"))

(def pending-path
  (str base-path "/pending"))

(def panel-path
  (str base-path "/panel"))

;; -----------------------------------------------------------------------------
;; In-memory demo state
;; -----------------------------------------------------------------------------

;; client-id -> {:id client-id
;;               :user-id user-id
;;               :queue LinkedBlockingQueue
;;               :connected-at millis}
(defonce clients
  (atom {}))

;; notification-id -> notification map
;;
;; This mimics the shape of durable notification docs. In a real app, replace
;; this with XTDB writes and queries.
(defonce notifications
  (atom {}))

;; client-id -> [notification-id ...]
;;
;; This is per-connected-client pending delivery state. It prevents one tab from
;; stealing another tab's pending toast.
(defonce pending-notification-ids
  (atom {}))

;; Defonce shape guards for REPL iteration.
(when-not (map? @clients)
  (reset! clients {}))

(when-not (map? @notifications)
  (reset! notifications {}))

(when-not (map? @pending-notification-ids)
  (reset! pending-notification-ids {}))

;; -----------------------------------------------------------------------------
;; Request/session helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- now
  []
  (Instant/now))

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn- client-id-from-ctx
  [{:keys [params]}]
  (or (request-param params :client-id)
      (str (random-uuid))))

(defn current-user-id
  "Return a stable-ish user id for this demo.

   In a real app, replace this with the app's authenticated user id convention."
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
          :user-id user-id
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
  "Return connected client summaries.

   Useful from the REPL."
  []
  (->> @clients
       vals
       (sort-by :connected-at)
       vec))

(defn connected-client-ids
  []
  (mapv :id (connected-clients)))

(defn latest-client
  []
  (last (connected-clients)))

(defn latest-user-id
  []
  (:user-id (latest-client)))

(defn- clients-for-user
  [user-id]
  (->> @clients
       vals
       (filter #(= (str user-id) (:user-id %)))
       vec))

(defn pending-counts
  []
  (->> @pending-notification-ids
       (map (fn [[client-id ids]]
              [client-id (count ids)]))
       (into {})))

(defn debug-state
  []
  {:clients (connected-clients)
   :pending (pending-counts)
   :notification-count (count @notifications)})

;; -----------------------------------------------------------------------------
;; SSE routes
;; -----------------------------------------------------------------------------

(defn stream
  "SSE endpoint for notification wakeups.

   The event does not carry toast HTML. It only wakes the browser:

     event: notifications-changed
     data: {}

   The browser then fetches /app/notifications/pending?client-id=..."
  [ctx]
  (let [client-id (client-id-from-ctx ctx)
        user-id   (current-user-id ctx)
        queue     (sse/new-queue)]
    (add-client! client-id user-id queue)
    (sse/queue-stream-response
     {:queue queue
      :on-close #(remove-client! client-id queue)})))

;; -----------------------------------------------------------------------------
;; Notification rendering
;; -----------------------------------------------------------------------------

(defn- notification-variant
  [notification]
  (or (:notification/variant notification)
      (case (:notification/type notification)
        :request-created :info
        :request-assigned :info
        :request-claimed :info
        :request-escalated :warning
        :request-failed :danger
        :job-failed :danger
        :job-completed :success
        :success)))

(defn notification->toast
  [notification]
  {:variant (notification-variant notification)
   :title (:notification/title notification)
   :description (:notification/body notification)
   :duration (:notification/duration notification)})

(defn- render-notification-toasts-oob
  [notifications]
  (into
   [:div {:id "app-toaster"
          :hx-swap-oob "beforeend"}]
   (map #(g/toast (notification->toast %)))
   notifications))

(defn- notification-row
  [notification]
  [:li {:class "content-stack-theme"
        :style {:padding "0.75rem 0"
                :border-bottom "1px solid var(--border)"}}
   [:div {:class "font-body text-sm-theme weight-semibold-theme"}
    (:notification/title notification)]

   (when-let [body (:notification/body notification)]
     [:div {:class "font-body text-sm-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
      body])

   [:div {:class "font-body text-xs-theme"
          :style {:color "var(--muted-foreground)"}}
    (str (:notification/status notification)
         " · "
         (:notification/created-at notification))]])

(defn- notifications-for-user
  [user-id]
  (->> @notifications
       vals
       (filter #(= (str user-id) (:notification/user-id %)))
       (sort-by :notification/created-at)
       reverse
       vec))

(defn unread-count
  [ctx]
  (let [user-id (current-user-id ctx)]
    (count
     (filter
      #(and (= user-id (:notification/user-id %))
            (= :unread (:notification/status %)))
      (vals @notifications)))))

(defn bell
  "Small notification bell/panel target.

   Mount this in app chrome if you want a visible notification history control."
  [ctx]
  [:div {:class "cluster-theme"}
   [:button {:type "button"
             :class "btn-outline"
             :hx-get panel-path
             :hx-target "#notification-panel"
             :hx-swap "innerHTML"}
    "Notifications "
    [:span {:id "notification-count"}
     (str (unread-count ctx))]]

   [:div {:id "notification-panel"}]])

(defn panel
  "Render a simple notification history panel."
  [ctx]
  (let [user-id        (current-user-id ctx)
        notifications (take 25 (notifications-for-user user-id))]
    (g/html-response
     [:div {:class "panel-theme radius-lg pad-card content-stack-theme"}
      [:div {:class "title-stack-theme"}
       [:h3 {:class "font-heading text-lg-theme weight-semibold-theme"}
        "Notifications"]

       [:p {:class "font-body text-sm-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
        "Latest notification records for this user."]]

      (if (seq notifications)
        (into [:ul {:class "list-none p-0 m-0"}]
              (map notification-row)
              notifications)
        [:p {:class "font-body text-sm-theme"
             :style {:color "var(--muted-foreground)"}}
         "No notifications yet."])])))

;; -----------------------------------------------------------------------------
;; Pending delivery
;; -----------------------------------------------------------------------------

(defn- enqueue-pending-notification!
  [client-id notification-id]
  (swap! pending-notification-ids update client-id (fnil conj []) notification-id))

(defn- drain-pending-notification-ids!
  [client-id]
  (loop []
    (let [m   @pending-notification-ids
          ids (get m client-id [])]
      (if (compare-and-set! pending-notification-ids m (dissoc m client-id))
        ids
        (recur)))))

(defn- wake-client!
  [client-id]
  (if-let [queue (get-in @clients [client-id :queue])]
    (do
      (sse/offer! queue {:event "notifications-changed"
                         :data "{}"})
      true)
    false))

(defn pending
  "HTMX endpoint triggered by sse:notifications-changed.

   Drains this connected client's pending notification ids, fetches the app-owned
   notification records, and returns OOB toast HTML."
  [ctx]
  (let [client-id       (client-id-from-ctx ctx)
        notification-ids (drain-pending-notification-ids! client-id)
        notification-ms  (keep @notifications notification-ids)]
    (if (seq notification-ms)
      (g/html-response
       (render-notification-toasts-oob notification-ms))
      (g/no-content))))

;; -----------------------------------------------------------------------------
;; Browser hook
;; -----------------------------------------------------------------------------

(defn listener
  "Mount this in the app shell next to the toaster.

   It opens the SSE connection and reacts to notifications-changed by fetching
   pending OOB toast HTML."
  [ctx]
  (let [client-id (str (random-uuid))]
    (live-htmx/sse-callback
     {:id (str "notification-listener-" client-id)
      :stream-url (str stream-path "?client-id=" client-id)
      :event "notifications-changed"
      :get (str pending-path "?client-id=" client-id)
      :swap "none"
      :attrs {:data-notification-listener true}})))

;; -----------------------------------------------------------------------------
;; Notification creation API
;; -----------------------------------------------------------------------------

(defn- normalize-notification
  [user-id notification]
  (let [id (str (or (:xt/id notification)
                    (:notification/id notification)
                    (random-uuid)))]
    (merge
     {:xt/id id
      :notification/id id
      :notification/user-id (str user-id)
      :notification/type :info
      :notification/title "Notification"
      :notification/body nil
      :notification/status :unread
      :notification/created-at (now)}
     notification
     {:xt/id id
      :notification/id id
      :notification/user-id (str user-id)})))

(defn notify-user!
  "Create a notification for one user and wake all connected clients for that
   user.

   In a real app, this function would submit an XTDB transaction instead of
   writing to an atom. The wakeup would likely be triggered by the XTDB listener
   after the notification write."
  [user-id notification]
  (let [notification' (normalize-notification user-id notification)
        id            (:notification/id notification')
        clients       (clients-for-user user-id)]
    (swap! notifications assoc id notification')
    (let [results (mapv
                   (fn [{:keys [id]}]
                     (enqueue-pending-notification! id (:notification/id notification'))
                     {:client-id id
                      :woke? (wake-client! id)})
                   clients)]
      {:notification notification'
       :client-count (count clients)
       :results results})))

(defn notify-latest-user!
  "Create a notification for the most recently connected user's user id.

   Useful from the REPL."
  [notification]
  (if-let [user-id (latest-user-id)]
    (notify-user! user-id notification)
    {:error :no-connected-users}))

(defn notify-sample!
  []
  (notify-latest-user!
   {:notification/type :job-completed
    :notification/variant :success
    :notification/title "Hello from notifications"
    :notification/body "This notification was app-owned, SSE-delivered, HTMX-fetched, and toaster-rendered."
    :notification/duration 5000}))

(defn notify-warning!
  []
  (notify-latest-user!
   {:notification/type :request-escalated
    :notification/variant :warning
    :notification/title "Request escalated"
    :notification/body "This warning came through the app notification pipeline."}))

(defn notify-danger!
  []
  (notify-latest-user!
   {:notification/type :job-failed
    :notification/variant :danger
    :notification/title "Background job failed"
    :notification/body "This danger notification is persistent until dismissed."}))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]
     ["/panel" {:get panel}]]]})
