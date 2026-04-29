(ns gessotest.client-plumbing
  "App-owned adapter for connected-client OOB delivery.

   This namespace owns app policy:
   - route placement
   - middleware
   - user identity
   - client scopes
   - response wrapping
   - app-friendly send helpers

   Generic connected-client delivery mechanics live in gesso.live.oob."
  (:require
   [gesso.core :as g]
   [gesso.live.oob :as live-oob]
   [gessotest.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; App placement
;; -----------------------------------------------------------------------------

(def endpoint
  {:base-path "/app/client-plumbing"
   :stream-path "/app/client-plumbing/stream"
   :pending-path "/app/client-plumbing/pending"
   :client-id-param :client-id})

;; -----------------------------------------------------------------------------
;; App identity
;; -----------------------------------------------------------------------------

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

(defn current-client
  "Return the app-defined client descriptor for this connected browser.

   Scopes are opaque to Gesso. The app decides what they mean and must only
   register scopes the current user is allowed to receive."
  [ctx]
  (let [user-id (current-user-id ctx)]
    {:client/user-id user-id
     :client/scopes  #{[:user user-id]
                       [:demo :gessotest]}}))

;; -----------------------------------------------------------------------------
;; Channel
;; -----------------------------------------------------------------------------

(defonce channel
  (live-oob/channel
   {:id :gessotest/client-oob
    :event "client-oob"
    :endpoint endpoint
    :client current-client}))

(defn reset-plumbing!
  []
  (live-oob/reset-channel! channel))

;; -----------------------------------------------------------------------------
;; Browser listener
;; -----------------------------------------------------------------------------

(defn new-client-id
  []
  (live-oob/new-client-id))

(defn listener
  "Render the browser-side listener for one connected client."
  ([ctx]
   (live-oob/listener channel ctx))
  ([ctx client-id]
   (live-oob/listener
    channel
    ctx
    {:client/id client-id
     :id (str "client-plumbing-listener-" client-id)
     :attrs {:data-client-plumbing-listener true}})))

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn stream
  [ctx]
  (live-oob/stream-response channel ctx))

(defn pending
  [ctx]
  (if-let [fragment (live-oob/drain-fragment! channel ctx)]
    (g/html-response fragment)
    (g/no-content)))

;; -----------------------------------------------------------------------------
;; App send API
;; -----------------------------------------------------------------------------

(defn send!
  "Send arbitrary complete OOB fragments to a target.

   Target forms:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]

   Fragments should already be HTMX OOB Hiccup, for example:
     (g/oob-inner-html \"notification-count\" \"3\")
     (g/render-toast-oob toast)"
  [to & fragments]
  (live-oob/send!
   channel
   {:to to
    :fragments fragments}))

(defn send-to-client!
  "Send arbitrary OOB fragments to one connected browser client."
  [client-id & fragments]
  (apply live-oob/send-to-client! channel client-id fragments))

(defn send-to-user!
  "Send arbitrary OOB fragments to every connected browser client for user-id."
  [user-id & fragments]
  (apply live-oob/send-to-user! channel (str user-id) fragments))

(defn send-to-scope!
  "Send arbitrary OOB fragments to every connected browser client in scope."
  [scope & fragments]
  (apply live-oob/send-to-scope! channel scope fragments))

(defn broadcast!
  "Send arbitrary OOB fragments to every connected browser client.

   This should be explicit and rare."
  [& fragments]
  (apply live-oob/broadcast! channel fragments))

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
   [[(:base-path endpoint)
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]]]})
