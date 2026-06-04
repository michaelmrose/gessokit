(ns gessokit.humanhelp.live
  "Model-backed Gesso Live wiring for the Human Help analogue.

   This namespace owns:
   - the compiled live model
   - live scopes
   - invalidation graph
   - fragment query/render descriptors
   - fragment panel helpers
   - fragment response helpers
   - stream response helpers
   - Human Help change constructors
   - Human Help notification/toast helpers
   - notification helper

   Generic connected-browser/OOB mechanics live in gessokit.client-plumbing."
  (:require
   [gesso.core :as g]
   [gesso.live.core :as live]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.store :as store]
   [gessokit.humanhelp.views :as views]))

;; -----------------------------------------------------------------------------
;; Constants / render context
;; -----------------------------------------------------------------------------

(def store-id
  model/store-id)

(def notification-scope
  "Connected-browser scope used for Human Help page-global notifications.

   For now this targets the generic app-wide connected-client scope. If/when
   client plumbing supports feature-registered scopes, this can become a more
   specific Human Help store scope without changing app handlers."
  client-plumbing/app-scope)

(def ^:private render-options-key
  ::render-options)

(defn- with-render-options
  [ctx render-options]
  (assoc ctx render-options-key (or render-options {})))

(defn- render-options
  [ctx]
  (get ctx render-options-key {}))

(defn- render-user
  [ctx]
  (:user (render-options ctx)))

(defn- render-view-state
  [ctx]
  (:view-state (render-options ctx)))

(defn- normalized-render-view-state
  [ctx]
  (store/normalize-view-state
   (render-view-state ctx)))

;; -----------------------------------------------------------------------------
;; Live scope authorization
;; -----------------------------------------------------------------------------

(defn allow-demo-store?
  "All signed-in users may see the one fake Human Help store.

   Real helper/helpee authorization is intentionally out of scope for the demo."
  [_ctx id]
  (= store-id id))

;; -----------------------------------------------------------------------------
;; Fragment queries
;; -----------------------------------------------------------------------------

(defn request-toolbar-query
  [ctx id]
  (let [view-state (normalized-render-view-state ctx)]
    (merge
     (store/toolbar-data view-state)
     {:ctx ctx
      :store/id id
      :user (render-user ctx)})))

(defn request-list-query
  [ctx id]
  (let [view-state (normalized-render-view-state ctx)]
    (merge
     (store/board-data view-state)
     {:ctx ctx
      :store/id id
      :user (render-user ctx)})))

;; -----------------------------------------------------------------------------
;; Fragment renders
;; -----------------------------------------------------------------------------

(defn request-toolbar-render
  [data]
  (views/request-toolbar-fragment data))

(defn request-list-render
  [data]
  (views/request-list-fragment data))

;; -----------------------------------------------------------------------------
;; Compiled live model
;; -----------------------------------------------------------------------------

(def compiled-live
  (live/compile-live-app
   {:response g/html-response

    :scopes
    {:request-toolbar
     {:topic :humanhelp/request-toolbar
      :id-key :store/id
      :label "Request toolbar"
      :authorized? allow-demo-store?}

     :request-list
     {:topic :humanhelp/request-list
      :id-key :store/id
      :label "Request list"
      :authorized? allow-demo-store?}}

    :graph
    {;; New requests should not make the visible list jump. They wake the
     ;; toolbar so the count/stale/refresh affordance can update. The create
     ;; handler also sends a page-global toast through client plumbing.
     :request/created
     [:request-toolbar]

     ;; Lifecycle changes affect cards that may already be visible, so they wake
     ;; both toolbar and list.
     :request/claimed
     [:request-toolbar
      :request-list]

     :request/unclaimed
     [:request-toolbar
      :request-list]

     :request/taken-over
     [:request-toolbar
      :request-list]

     :request/done
     [:request-toolbar
      :request-list]

     :request/cancelled
     [:request-toolbar
      :request-list]

     ;; A minute tick can update waiting-time labels without any request
     ;; mutation. We can wire the actual timer later.
     :clock/minute
     [:request-list]

     ;; Development/demo reset wakes everything.
     :humanhelp-demo/reset
     [:request-toolbar
      :request-list]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (fn [_id]
               views/request-toolbar-dom-id)
      :query request-toolbar-query
      :render request-toolbar-render
      :swap :outerHTML
      :request-policy {:in-flight :one
                       :queued :last
                       :stale-window-ms 750}}

     :request-list
     {:scope :request-list
      :id-fn (fn [_id]
               views/request-list-dom-id)
      :query request-list-query
      :render request-list-render
      :swap :outerHTML
      :request-policy {:in-flight :one
                       :queued :last
                       :stale-window-ms 750}}}}))

(def live-rules
  "Compiled invalidation rules exported for the app module.

   gessokit.clj collects module :live-rules and passes them into the app-wide
   Gesso Live system."
  (live/model-live-rules compiled-live))

;; -----------------------------------------------------------------------------
;; Fragment URL options
;; -----------------------------------------------------------------------------

(defn fragment-options
  [fragment-name view-state]
  (case fragment-name
    :request-toolbar
    {:fragment-url (routes/request-toolbar-fragment-url view-state)
     :stream-url (routes/request-toolbar-stream-url view-state)}

    :request-list
    {:fragment-url (routes/request-list-fragment-url view-state)
     :stream-url (routes/request-list-stream-url view-state)}

    (throw
     (ex-info "Unknown Human Help live fragment."
              {:fragment fragment-name
               :known-fragments [:request-toolbar :request-list]}))))

;; -----------------------------------------------------------------------------
;; Initial panels
;; -----------------------------------------------------------------------------

(defn request-toolbar-panel
  [view-state]
  (live/model-fragment-panel
   compiled-live
   :request-toolbar
   store-id
   (fragment-options :request-toolbar view-state)))

(defn request-list-panel
  [view-state]
  (live/model-fragment-panel
   compiled-live
   :request-list
   store-id
   (fragment-options :request-list view-state)))

(defn page-panels
  "Return the model-backed live panels needed for the Human Help page."
  [view-state]
  {:request-toolbar-panel (request-toolbar-panel view-state)
   :request-list-panel (request-list-panel view-state)})

;; -----------------------------------------------------------------------------
;; Fragment render / response helpers
;; -----------------------------------------------------------------------------

(defn render-fragment-node
  "Render one Human Help live fragment to a Hiccup node.

   render-options should include:
     :user
     :view-state"
  [ctx fragment-name render-options]
  (live/render-fragment-node
   compiled-live
   (with-render-options ctx render-options)
   fragment-name
   store-id))

(defn render-fragment-response
  "Render one Human Help live fragment as a Ring HTML response.

   render-options should include:
     :user
     :view-state"
  [ctx fragment-name render-options]
  (live/render-fragment-response
   compiled-live
   (with-render-options ctx render-options)
   fragment-name
   store-id))

(defn stream-response
  "Start an SSE stream for one Human Help live fragment.

   Returns the Ring response from gesso.live/start-fragment-stream!.

   render-options should include:
     :user
     :view-state"
  ([live-system ctx fragment-name render-options]
   (stream-response live-system ctx fragment-name render-options nil))
  ([live-system ctx fragment-name render-options options]
   (:response
    (live/start-fragment-stream!
     live-system
     compiled-live
     (with-render-options ctx render-options)
     fragment-name
     store-id
     (merge
      {:flow-options {:relieve? true}}
      options)))))

;; -----------------------------------------------------------------------------
;; Change constructors
;; -----------------------------------------------------------------------------

(defn- store-change-base
  [topic]
  {:topic topic

   ;; Gesso Live's generic source/invalidation routing is keyed by :topic + :id.
   ;; Keep :store/id as domain context, but also provide :id explicitly.
   :id store-id
   :store/id store-id})

(defn request-created-change
  [{:keys [request revision actor]}]
  (merge
   (store-change-base :request/created)
   {:request/id (:request/id request)
    :request/number (:request/number request)
    :request/status (:request/status request)
    :revision revision
    :actor/id (:user/id actor)
    :actor/email (:user/email actor)}))

(defn request-transition-topic
  [action]
  (case action
    :claim
    :request/claimed

    :unclaim
    :request/unclaimed

    :take-over
    :request/taken-over

    :done
    :request/done

    :cancel
    :request/cancelled

    (throw
     (ex-info "Unknown Human Help request transition action."
              {:action action}))))

(defn request-transition-change
  [{:keys [action request previous revision actor]}]
  (merge
   (store-change-base (request-transition-topic action))
   {:request/id (:request/id request)
    :request/number (:request/number request)
    :request/status (:request/status request)
    :previous/status (:request/status previous)
    :action action
    :revision revision
    :actor/id (:user/id actor)
    :actor/email (:user/email actor)}))

(defn minute-tick-change
  []
  (merge
   (store-change-base :clock/minute)
   {:at-ms (model/now-ms)}))

(defn demo-reset-change
  [{:keys [revision actor]}]
  (merge
   (store-change-base :humanhelp-demo/reset)
   {:revision revision
    :actor/id (:user/id actor)
    :actor/email (:user/email actor)}))

;; -----------------------------------------------------------------------------
;; Human Help page-global notifications
;; -----------------------------------------------------------------------------

(defn request-toast-description
  [request]
  (str
   (or (:request/customer-name request) "Someone")
   " added request #"
   (:request/number request)
   (when (model/present? (:request/title request))
     (str ": " (:request/title request)))))

(defn send-new-request-toast!
  "Send a Human Help new-request toast.

   By default this targets the Human Help notification scope. When actor or
   exclude-user-id is supplied, clients owned by that user are excluded. This
   avoids showing the creator both:
   - the local create-success toast
   - the broadcast \"New request received\" toast"
  ([request]
   (send-new-request-toast! request {}))
  ([request {:keys [actor exclude-user-id]}]
   (let [excluded-user-id (or exclude-user-id
                              (:user/id actor))
         toast {:variant :info
                :title "New request received"
                :description (request-toast-description request)}]
     (if excluded-user-id
       (client-plumbing/send-toast-to-scope-except-user!
        notification-scope
        excluded-user-id
        toast)
       (client-plumbing/send-toast-to-scope!
        notification-scope
        toast)))))

(defn send-reset-toast!
  "Send a Human Help demo reset toast."
  []
  (client-plumbing/send-toast-to-scope!
   notification-scope
   {:variant :info
    :title "Demo reset"
    :description "The Human Help request board was reset."}))

(defn send-request-action-error-toast!
  "Send a Human Help request-action error toast.

   App handlers can either return a toast OOB directly through views or call
   this when they need to push the error to connected clients."
  [message]
  (client-plumbing/send-toast-to-scope!
   notification-scope
   {:variant :danger
    :title "Request not updated"
    :description (or message
                     "That request action could not be completed.")}))

;; -----------------------------------------------------------------------------
;; Notification
;; -----------------------------------------------------------------------------

(defn notify!
  "Submit a primary Human Help change to the app-wide Gesso Live system.

   The system already owns compiled live rules through gessokit.clj's module
   collection. This helper keeps callers from depending directly on the lower
   level live/submit-expanded! function."
  [live-system ctx change]
  (live/submit-expanded!
   live-system
   ctx
   change))
