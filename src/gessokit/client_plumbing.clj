(ns gessokit.client-plumbing
  "App-owned adapter for connected-client OOB delivery.

   This namespace owns app policy around connected browser clients:

   - route placement
   - middleware
   - current user identity
   - client scopes
   - response wrapping
   - app-friendly send helpers
   - toast convenience helpers

   Generic connected-client delivery mechanics live in gesso.live.client.

   Human Help uses this for page-global UI effects such as toasts and OOB
   updates. Normal request-list fragments should use model-backed Gesso Live
   fragment streams instead."
  (:require
   [gesso.core :as g]
   [gesso.live.client :as live-client]
   [gessokit.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; App placement
;; -----------------------------------------------------------------------------

(def endpoint
  {:base-path "/app/client-plumbing"
   :stream-path "/app/client-plumbing/stream"
   :pending-path "/app/client-plumbing/pending"
   :client-id-param :client-id})

;; -----------------------------------------------------------------------------
;; Human Help client scopes
;; -----------------------------------------------------------------------------

(def humanhelp-store-id
  "demo-store")

(def humanhelp-store-scope
  "Shared one-store Human Help demo scope for page-global notifications.

   This intentionally mirrors the app plan's one-store simplification without
   making client plumbing depend on gessokit.app.* namespaces."
  [:humanhelp/store humanhelp-store-id])

;; -----------------------------------------------------------------------------
;; App identity
;; -----------------------------------------------------------------------------

(defn current-user-id
  "Return the app user id used for connected-client targeting.

   Biff auth usually gives us a session uid. We intentionally return a string
   because client targeting keys should be stable and easy to serialize/log."
  [ctx]
  (str
   (or (:user/id ctx)
       (:user/email ctx)
       (get-in ctx [:user :xt/id])
       (get-in ctx [:user :email])
       (get-in ctx [:session :uid])
       (get-in ctx [:session :user])
       "demo-user")))

(defn current-user-email
  "Best-effort display email for app UI.

   This namespace does not query XTDB. If the surrounding app wants an exact
   email address, it can attach it to ctx before rendering."
  [ctx]
  (str
   (or (:user/email ctx)
       (get-in ctx [:user :email])
       (get-in ctx [:session :email])
       (get-in ctx [:params :email])
       (get-in ctx [:params "email"])
       (current-user-id ctx))))

(defn current-client
  "Return the app-defined client descriptor for this connected browser.

   Scopes are opaque to Gesso. The app decides what they mean and must only
   register scopes the current user is allowed to receive.

   For the Human Help analogue, all signed-in users may participate in the one
   demo store, so every connected user receives the shared store scope."
  [ctx]
  (let [user-id (current-user-id ctx)]
    {:client/user-id user-id
     :client/scopes  #{[:user user-id]
                       humanhelp-store-scope}}))

;; -----------------------------------------------------------------------------
;; Channel
;; -----------------------------------------------------------------------------

(defonce channel
  (live-client/channel
   {:id :gessokit/client-oob
    :event "client-oob"
    :endpoint endpoint
    :client current-client}))

(defn reset-plumbing!
  "Clear connected clients and pending OOB fragments for this channel.

   Useful at the REPL during development."
  []
  (live-client/reset-channel! channel))

;; -----------------------------------------------------------------------------
;; Browser listener
;; -----------------------------------------------------------------------------

(defn new-client-id
  []
  (live-client/new-client-id))

(defn listener
  "Render the browser-side listener for one connected client.

   Arity 1 generates a new client id.

   Arity 2 uses the supplied client id, which is useful when the page wants to
   display/debug the exact browser client id."
  ([ctx]
   (live-client/listener channel ctx))
  ([ctx client-id]
   (live-client/listener
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
  (live-client/stream-response channel ctx))

(defn pending
  [ctx]
  (if-let [fragment (live-client/drain-fragment! channel ctx)]
    (g/html-response fragment)
    (g/no-content)))

;; -----------------------------------------------------------------------------
;; Generic app send API
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
  (live-client/send!
   channel
   {:to to
    :fragments fragments}))

(defn send-to-client!
  "Send arbitrary OOB fragments to one connected browser client."
  [client-id & fragments]
  (apply live-client/send-to-client! channel client-id fragments))

(defn send-to-user!
  "Send arbitrary OOB fragments to every connected browser client for user-id."
  [user-id & fragments]
  (apply live-client/send-to-user! channel (str user-id) fragments))

(defn send-to-scope!
  "Send arbitrary OOB fragments to every connected browser client in scope."
  [scope & fragments]
  (apply live-client/send-to-scope! channel scope fragments))

(defn broadcast!
  "Send arbitrary OOB fragments to every connected client.

   This should stay explicit and rare."
  [& fragments]
  (apply live-client/broadcast! channel fragments))

;; -----------------------------------------------------------------------------
;; Toast helpers
;; -----------------------------------------------------------------------------

(def default-toast
  {:variant :info
   :title "Live event"
   :description "The page received a live update."})

(defn normalize-toast
  [toast]
  (merge default-toast toast))

(defn toast-oob
  "Render one normalized toast as an HTMX OOB fragment."
  [toast]
  (g/render-toast-oob
   (normalize-toast toast)))

(defn send-toast!
  "Send one toast to an arbitrary connected-client target.

   Target forms are the same as send!:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]"
  [to toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send! to (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-client!
  [client-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-client! client-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-user!
  [user-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-user! user-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-scope!
  [scope toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-scope! scope (toast-oob toast'))
     {:toast toast'})))

(defn broadcast-toast!
  "Broadcast one toast to every connected client.

   This should stay explicit and rare."
  [toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (broadcast! (toast-oob toast'))
     {:toast toast'})))

(defn send-humanhelp-new-request-toast!
  "Convenience helper for the Human Help one-store demo.

   The request list itself may choose not to jump immediately, but connected
   users can still receive a page-global toast."
  [{:keys [request-number title customer-name]}]
  (send-toast-to-scope!
   humanhelp-store-scope
   {:variant :info
    :title "New request received"
    :description
    (str
     (or customer-name "Someone")
     " added request #"
     request-number
     (when title
       (str ": " title)))}))

;; -----------------------------------------------------------------------------
;; Introspection
;; -----------------------------------------------------------------------------

(defn connected-clients
  []
  (live-client/connected-clients channel))

(defn connected-client-ids
  []
  (live-client/connected-client-ids channel))

(defn latest-client-id
  []
  (live-client/latest-client-id channel))

(defn pending-counts
  []
  (live-client/pending-counts channel))

(defn state-summary
  []
  (live-client/state-summary channel))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[(:base-path endpoint)
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]]]})
