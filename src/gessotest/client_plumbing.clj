(ns gessotest.client-plumbing
  "Small app-owned browser-client communication adapter.

   This namespace owns app policy and route placement for connected-client OOB
   delivery.

   It does not implement the generic live OOB machinery itself. Gesso owns that
   in gesso.live.oob.

   This namespace decides:
   - where the stream/pending routes live
   - which middleware protects them
   - how this app identifies a user
   - which scopes a connected browser client belongs to
   - app-friendly wrapper names for sending arbitrary OOB fragments."
  (:require
   [gesso.core :as g]
   [gesso.live.oob :as live-oob]
   [gessotest.middleware :as mid])
  (:import
   [java.net URLEncoder]
   [java.nio.charset StandardCharsets]))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/client-plumbing")

(def stream-path
  (str base-path "/stream"))

(def pending-path
  (str base-path "/pending"))

;; -----------------------------------------------------------------------------
;; Request/client helpers
;; -----------------------------------------------------------------------------

(defn new-client-id
  []
  (live-oob/new-client-id))

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

(defn current-client-scopes
  "Return app-defined scopes for the connected browser client.

   Scopes are opaque values to Gesso. App code decides what they mean.

   Examples a real app might include:
     [:user user-id]
     [:store store-id]
     [:request request-id]"
  [ctx]
  (let [user-id (current-user-id ctx)]
    #{[:user user-id]
      [:demo :gessotest]}))

(defn current-client
  [ctx]
  {:client/user-id (current-user-id ctx)
   :client/scopes  (current-client-scopes ctx)})

;; -----------------------------------------------------------------------------
;; Channel
;; -----------------------------------------------------------------------------

(defonce channel
  (live-oob/channel
   {:id :gessotest/client-oob
    :event "client-oob"
    :client current-client}))

(defn reset-plumbing!
  []
  (live-oob/reset-channel! channel))

;; -----------------------------------------------------------------------------
;; URLs and listener markup
;; -----------------------------------------------------------------------------

(defn- url-encode
  [x]
  (URLEncoder/encode (str x) (.name StandardCharsets/UTF_8)))

(defn- append-client-id
  [url client-id]
  (str url "?client-id=" (url-encode client-id)))

(defn stream-url
  [client-id]
  (append-client-id stream-path client-id))

(defn pending-url
  [client-id]
  (append-client-id pending-path client-id))

(defn listener
  "Render one browser-client listener.

   Mount this on pages that need app-owned server-to-client OOB updates."
  [client-id]
  (live-oob/listener
   channel
   {:client/id client-id
    :id (str "client-plumbing-listener-" client-id)
    :stream-url (stream-url client-id)
    :pending-url (pending-url client-id)
    :attrs {:data-client-plumbing-listener true}}))

(defn app-listener
  "Convenience listener for app shells that do not need to expose client-id."
  [_ctx]
  (listener (new-client-id)))

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn stream
  [ctx]
  (let [client-id (client-id-from-ctx ctx)]
    (live-oob/stream-response channel ctx client-id)))

(defn pending
  [ctx]
  (let [client-id (client-id-from-ctx ctx)]
    (if-let [fragment (live-oob/drain-fragment! channel client-id)]
      (g/html-response fragment)
      (g/no-content))))

;; -----------------------------------------------------------------------------
;; Generic sending API
;; -----------------------------------------------------------------------------

(defn send!
  "Send arbitrary OOB fragments to a target.

   Target forms:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]"
  [to & oob-nodes]
  (live-oob/send!
   channel
   {:to to
    :oob oob-nodes}))

(defn send-to-client!
  "Send arbitrary OOB fragments to one connected browser client."
  [client-id & oob-nodes]
  (apply send! [:client client-id] oob-nodes))

(defn send-to-user!
  "Send arbitrary OOB fragments to all connected browser clients for one user."
  [user-id & oob-nodes]
  (apply send! [:user (str user-id)] oob-nodes))

(defn send-to-scope!
  "Send arbitrary OOB fragments to all connected browser clients for one scope."
  [scope & oob-nodes]
  (apply send! [:scope scope] oob-nodes))

(defn broadcast!
  "Send arbitrary OOB fragments to every connected browser client.

   This should be explicit and rare in real apps."
  [& oob-nodes]
  (apply send! :all oob-nodes))

;; -----------------------------------------------------------------------------
;; Introspection
;; -----------------------------------------------------------------------------

(defn connected-clients
  []
  (live-oob/connected-clients channel))

(defn connected-client-ids
  []
  (live-oob/connected-client-ids channel))

(defn latest-client-id
  []
  (live-oob/latest-client-id channel))

(defn pending-counts
  []
  (live-oob/pending-counts channel))

(defn state-summary
  []
  (live-oob/state-summary channel))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]]]})
