(ns gessokit.humanhelp.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace is now deliberately narrower. It assembles HTTP handlers from:

   - gessokit.humanhelp.data
   - gessokit.humanhelp.live
   - gessokit.humanhelp.model
   - gessokit.humanhelp.routes
   - gessokit.humanhelp.views

   It should not own generic Gesso Live plumbing. Human Help live panels,
   fragment rendering, stream responses, change constructors, and toast helpers
   are delegated to gessokit.humanhelp.live."
  (:require
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.data :as data]
   [gessokit.humanhelp.live :as app-live]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.views :as views]
   [gessokit.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Request boundary helpers
;; -----------------------------------------------------------------------------

(defn- scalar-param-value
  "Normalize a request param value to the scalar value the app expects.

   Repeated browser params can arrive as vectors, for example when a form has
   two fields with the same name:

     [\"\" \"test\"]

   The last submitted value is treated as authoritative. This preserves normal
   form-clearing behavior:

     [\"old\" \"\"] => \"\""
  [x]
  (cond
    (nil? x)
    nil

    (and (sequential? x)
         (not (map? x)))
    (last x)

    :else
    x))

(defn- param
  "Read a Ring/Biff request param by keyword or string key.

   This stays here temporarily because we do not yet have a dedicated request
   boundary/view-state namespace. It supports plain Ring-style maps as well as
   common Reitit match placement."
  [ctx k]
  (scalar-param-value
   (or (get-in ctx [:params k])
       (get-in ctx [:params (name k)])
       (get-in ctx [:form-params k])
       (get-in ctx [:form-params (name k)])
       (get-in ctx [:query-params k])
       (get-in ctx [:query-params (name k)])
       (get-in ctx [:path-params k])
       (get-in ctx [:path-params (name k)])
       (get-in ctx [:reitit.core/match :path-params k])
       (get-in ctx [:reitit.core/match :path-params (name k)]))))

(defn- request-id
  [ctx]
  (param ctx :request-id))

(defn- request-view-state
  "Extract request-board view state from request params.

   Important fields:
   - :search
   - :selected-request-id
   - :visible-revision

   data/normalize-view-state fills defaults against the current persisted data."
  [ctx]
  {:search (or (param ctx :q) "")
   :selected-request-id (param ctx :selected)
   :visible-revision (model/parse-visible-revision
                      (param ctx :visible-revision))})

(defn- create-request-input
  "Extract create-request form input from request params.

   This deliberately uses the HTTP-boundary param helper so repeated browser
   params are normalized before reaching the model parser."
  [ctx]
  (model/parse-create-request-input
   {:title (param ctx :title)
    :area (param ctx :area)
    :details (param ctx :details)
    :customer-name (param ctx :customer-name)}))

;; -----------------------------------------------------------------------------
;; Current user
;; -----------------------------------------------------------------------------

(defn- current-user
  "Return the Human Help demo user descriptor.

   This is intentionally small for this pass. The richer Biff/XTDB email lookup
   from the old app.clj has been moved to app_graveyard.clj for possible later
   extraction into model/user or auth-specific code."
  [ctx]
  {:user/id (client-plumbing/current-user-id ctx)
   :user/email (client-plumbing/current-user-email ctx)})

;; -----------------------------------------------------------------------------
;; Live boundary
;; -----------------------------------------------------------------------------

(defn- live-system
  [ctx]
  (or (:gesso.live/system ctx)
      (throw
       (ex-info "Human Help requires :gesso.live/system in ctx."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn- fragment-render-options
  [ctx]
  {:user (current-user ctx)
   :view-state (request-view-state ctx)})

(defn- render-toolbar-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-toolbar
   {:user (current-user ctx)
    :view-state view-state}))

(defn- render-list-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-list
   {:user (current-user ctx)
    :view-state view-state}))

(defn- board-fragments
  [ctx view-state]
  {:toolbar (render-toolbar-node ctx view-state)
   :request-list (render-list-node ctx view-state)})

(defn- notify!
  [ctx change]
  (app-live/notify!
   (live-system ctx)
   ctx
   change))

;; -----------------------------------------------------------------------------
;; HTML / OOB helpers
;; -----------------------------------------------------------------------------

(defn- html
  [node]
  (g/html-response node))

(defn- board-state-form-oob
  "Render an OOB replacement for the board-state/search form.

   This still lives here temporarily because views.clj does not yet expose a
   single response-level helper for synchronizing board state. It should move
   into views.clj during the next view-state refactor."
  [ctx view-state]
  (g/oob-outer-html
   views/board-state-form-id
   (views/search-control
    {:view-state (data/normalize-view-state ctx view-state)})))

(defn- with-board-state-oob
  [ctx view-state & nodes]
  (apply views/oob-response
         ;; Put board-state first so the browser updates hidden state before
         ;; processing any other OOB fragments from the same response.
         (board-state-form-oob ctx view-state)
         nodes))

;; -----------------------------------------------------------------------------
;; Page data
;; -----------------------------------------------------------------------------

(defn- page-data
  [ctx]
  (let [view-state (data/normalize-view-state
                    ctx
                    (request-view-state ctx))]
    (merge
     {:user (current-user ctx)
      :view-state view-state}
     (app-live/page-panels view-state))))

;; -----------------------------------------------------------------------------
;; Best-effort side effects
;; -----------------------------------------------------------------------------

(defn- send-new-request-toast-safely!
  [request user]
  (try
    (app-live/send-new-request-toast!
     request
     {:actor user})
    (catch Exception e
      (println "[humanhelp] send-new-request-toast! failed"
               {:message (.getMessage e)
                :request/id (:request/id request)}))))

(defn- send-reset-toast-safely!
  []
  (try
    (app-live/send-reset-toast!)
    (catch Exception e
      (println "[humanhelp] send-reset-toast! failed"
               {:message (.getMessage e)}))))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn app-page
  "Render /app."
  [ctx]
  (views/page ctx (page-data ctx)))

;; -----------------------------------------------------------------------------
;; Fragment handlers
;; -----------------------------------------------------------------------------

(defn request-toolbar-fragment
  [ctx]
  (app-live/render-fragment-response
   ctx
   :request-toolbar
   (fragment-render-options ctx)))

(defn request-list-fragment
  [ctx]
  (app-live/render-fragment-response
   ctx
   :request-list
   (fragment-render-options ctx)))

(defn create-request-dialog-fragment
  [ctx]
  (html
   (views/create-request-dialog
    ctx
    {:user (current-user ctx)
     :values {}
     :errors {}
     :open? true})))

;; -----------------------------------------------------------------------------
;; Stream handlers
;; -----------------------------------------------------------------------------

(defn request-toolbar-stream
  [ctx]
  (app-live/stream-response
   (live-system ctx)
   ctx
   :request-toolbar
   (fragment-render-options ctx)))

(defn request-list-stream
  [ctx]
  (app-live/stream-response
   (live-system ctx)
   ctx
   :request-list
   (fragment-render-options ctx)))

;; -----------------------------------------------------------------------------
;; Request creation
;; -----------------------------------------------------------------------------

(defn- create-request-success-response
  [ctx {:keys [request revision view-state]}]
  (let [user        (current-user ctx)
        view-state' (assoc view-state
                            :visible-revision revision
                            :selected-request-id (:request/id request))
        fragments   (board-fragments ctx view-state')]
    (html
     (with-board-state-oob
       ctx
       view-state'
       (views/create-request-success
        ctx
        (merge
         {:user user
          :request request}
         fragments))))))

(defn create-request!
  "Create a new request from the modal dialog.

   Creator behavior:
   - request is created
   - dialog closes
   - visible list refreshes to include the new request

   Other connected users:
   - receive the model-backed :request/created live invalidation, which wakes
     the request toolbar only
   - receive a new-request toast through app-live/send-new-request-toast!
   - their list does not jump until they refresh"
  [ctx]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        input      (create-request-input ctx)
        errors     (model/create-request-errors input)]
    (if (seq errors)
      (html
       (views/create-request-validation-error
        ctx
        {:user user
         :values input
         :errors errors}))

      (let [{:keys [request revision]}
            (data/create-request!
             ctx
             {:user user
              :input input})]

        (notify!
         ctx
         (app-live/request-created-change
          {:request request
           :revision revision
           :actor user}))

        (send-new-request-toast-safely! request user)

        (create-request-success-response
         ctx
         {:request request
          :revision revision
          :view-state view-state})))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(defn refresh-requests!
  "Commit the visible request board to the latest revision."
  [ctx]
  (let [view-state (assoc (request-view-state ctx)
                          :visible-revision
                          (data/latest-revision ctx))]
    (html
     (with-board-state-oob
       ctx
       view-state
       (views/refreshed-request-board-fragments
        (board-fragments ctx view-state))))))

(defn search-requests
  "Render the request list for a search input change."
  [ctx]
  (request-list-fragment ctx))

(defn select-request
  "Render the request list with one selected/expanded card and sync board state.

   The selected request id comes from the route path, not the submitted
   board-state form. The board-state form may still contain the previous
   selected value because stable live wrappers include it during fragment
   refreshes."
  [ctx]
  (let [view-state (assoc (request-view-state ctx)
                          :selected-request-id
                          (request-id ctx))]
    (html
     (with-board-state-oob
       ctx
       view-state
       (render-list-node ctx view-state)))))

;; -----------------------------------------------------------------------------
;; Request lifecycle actions
;; -----------------------------------------------------------------------------

(defn- lifecycle-action!
  "Shared request lifecycle action boundary.

   action is one of:
     :claim
     :unclaim
     :take-over
     :done
     :cancel"
  [ctx action transition-fn]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        id         (request-id ctx)
        result     (transition-fn
                    ctx
                    {:request-id id
                     :user user})]
    (if (= :ok (:status result))
      (let [{:keys [request revision previous]} result]
        (notify!
         ctx
         (app-live/request-transition-change
          {:action action
           :request request
           :previous previous
           :revision revision
           :actor user}))

        (html
         (with-board-state-oob
           ctx
           view-state
           (views/request-lifecycle-result
            (merge
             {:user user
              :action action
              :request request
              :previous previous
              :revision revision
              :view-state view-state}
             (board-fragments ctx view-state))))))

      (html
       (views/request-action-error
        {:user user
         :request-id id
         :action action
         :result result
         :view-state view-state})))))

(defn claim-request!
  [ctx]
  (lifecycle-action!
   ctx
   :claim
   data/claim-request!))

(defn unclaim-request!
  [ctx]
  (lifecycle-action!
   ctx
   :unclaim
   data/unclaim-request!))

(defn take-over-request!
  [ctx]
  (lifecycle-action!
   ctx
   :take-over
   data/take-over-request!))

(defn mark-request-done!
  [ctx]
  (lifecycle-action!
   ctx
   :done
   data/mark-request-done!))

(defn cancel-request!
  [ctx]
  (lifecycle-action!
   ctx
   :cancel
   data/cancel-request!))

;; -----------------------------------------------------------------------------
;; Dev/demo reset
;; -----------------------------------------------------------------------------

(defn reset-demo!
  [ctx]
  (let [user       (current-user ctx)
        result     (data/reset-demo-state! ctx)
        view-state (assoc (request-view-state ctx)
                          :visible-revision
                          (:revision result))]
    (notify!
     ctx
     (app-live/demo-reset-change
      {:revision (:revision result)
       :actor user}))

    (send-reset-toast-safely!)

    (html
     (with-board-state-oob
       ctx
       view-state
       (views/reset-demo-result
        (merge
         {:user user
          :result result
          :view-state view-state}
         (board-fragments ctx view-state)))))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:live-rules app-live/live-rules

   ;; This is still the app-level handler wiring. routes.clj owns the route
   ;; facts/paths/URL builders; app.clj plugs those facts into concrete handler
   ;; functions.
   :routes
   [[routes/base-path
     {:middleware [mid/wrap-signed-in]}

     ;; Main app page
     ["" {:get app-page}]

     ;; Fragments
     [routes/request-toolbar-fragment-route
      {:get request-toolbar-fragment}]

     [routes/request-list-fragment-route
      {:get request-list-fragment}]

     [routes/create-request-dialog-fragment-route
      {:get create-request-dialog-fragment}]

     ;; Streams
     [routes/request-toolbar-stream-route
      {:get request-toolbar-stream}]

     [routes/request-list-stream-route
      {:get request-list-stream}]

     ;; Request creation and visible-list controls
     [routes/create-request-route
      {:post create-request!}]

     [routes/refresh-requests-route
      {:post refresh-requests!}]

     [routes/search-requests-route
      {:get search-requests}]

     [routes/select-request-route
      {:get select-request}]

     ;; Request lifecycle actions
     [routes/claim-request-route
      {:post claim-request!}]

     [routes/unclaim-request-route
      {:post unclaim-request!}]

     [routes/take-over-request-route
      {:post take-over-request!}]

     [routes/done-request-route
      {:post mark-request-done!}]

     [routes/cancel-request-route
      {:post cancel-request!}]

     ;; Dev/demo reset
     [routes/reset-demo-route
      {:post reset-demo!}]]]})
