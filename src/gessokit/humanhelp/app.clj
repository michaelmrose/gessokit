(ns gessokit.humanhelp.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace intentionally stays thin. It wires together:

   - gessokit.humanhelp.domain
   - gessokit.humanhelp.store
   - gessokit.humanhelp.views
   - gessokit.humanhelp.live
   - gessokit.client-plumbing

   The Human Help feature code is isolated under gessokit.humanhelp.* so the
   example app can be removed cleanly from a generated template project."
  (:require
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.domain :as domain]
   [gessokit.humanhelp.live :as app-live]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.store :as store]
   [gessokit.humanhelp.views :as views]
   [gessokit.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------

(defn param
  "Read a Ring/Biff request param by keyword or string key.

   This stays in the HTTP boundary because it is a request-shape concern."
  [ctx k]
  (or (get-in ctx [:params k])
      (get-in ctx [:params (name k)])
      (get-in ctx [:form-params k])
      (get-in ctx [:form-params (name k)])
      (get-in ctx [:query-params k])
      (get-in ctx [:query-params (name k)])
      (get-in ctx [:path-params k])
      (get-in ctx [:path-params (name k)])))

(defn request-id
  [ctx]
  (param ctx :request-id))

(defn current-user
  "Return the Human Help demo user descriptor.

   The demo intentionally does not enforce a real helper/helpee split, but it
   still needs a stable user id/email for ownership, claim display, and OOB
   client targeting."
  [ctx]
  {:user/id (client-plumbing/current-user-id ctx)
   :user/email (client-plumbing/current-user-email ctx)})

(defn live-system
  [ctx]
  (or (:gesso.live/system ctx)
      (throw
       (ex-info "Human Help requires :gesso.live/system in ctx."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn request-view-state
  "Extract request-board view state from request params.

   Important fields:
   - :search
   - :selected-request-id
   - :visible-revision

   :visible-revision controls the 'new data available, click refresh' behavior.
   A nil visible revision is normalized to latest by store/live code for initial
   page loads."
  [ctx]
  {:search (or (param ctx :q) "")
   :selected-request-id (param ctx :selected)
   :visible-revision (domain/parse-visible-revision
                      (param ctx :visible-revision))})

(defn html
  [node]
  (g/html-response node))

;; -----------------------------------------------------------------------------
;; Render helpers
;; -----------------------------------------------------------------------------

(defn page-data
  [ctx]
  (let [user       (current-user ctx)
        view-state (store/normalize-view-state
                    (request-view-state ctx))]
    (merge
     {:user user
      :view-state view-state}
     (app-live/page-panels view-state))))

(defn fragment-render-options
  [ctx]
  {:user (current-user ctx)
   :view-state (request-view-state ctx)})

(defn render-toolbar-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-toolbar
   {:user (current-user ctx)
    :view-state view-state}))

(defn render-list-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-list
   {:user (current-user ctx)
    :view-state view-state}))

(defn board-oob
  [ctx view-state]
  {:toolbar (render-toolbar-node ctx view-state)
   :request-list (render-list-node ctx view-state)})

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

(defn create-request-success-response
  [ctx {:keys [request revision view-state]}]
  (let [user         (current-user ctx)
        view-state'  (assoc view-state :visible-revision revision)
        toolbar      (render-toolbar-node ctx view-state')
        request-list (render-list-node ctx view-state')]
    (html
     (views/create-request-success
      ctx
      {:user user
       :request request
       :toolbar toolbar
       :request-list request-list}))))

(defn create-request!
  "Create a new request from the modal dialog.

   Creator behavior:
   - request is created
   - dialog closes
   - visible list refreshes to include the new request

   Other connected users:
   - receive a toast through Human Help live notification helpers
   - receive toolbar/count/stale indicator through model-backed Live
   - their list does not jump until they refresh"
  [ctx]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        input      (domain/parse-create-request-input (:params ctx))
        errors     (domain/create-request-errors input)]
    (if (seq errors)
      (html
       (views/create-request-validation-error
        ctx
        {:user user
         :values input
         :errors errors}))

      (let [{:keys [request revision]}
            (store/create-request!
             {:user user
              :input input})]

        (app-live/notify!
         (live-system ctx)
         ctx
         (app-live/request-created-change
          {:request request
           :revision revision
           :actor user}))

        (app-live/send-new-request-toast! request)

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
                          (store/latest-revision))]
    (html
     (views/refreshed-request-board-fragments
      ctx
      (board-oob ctx view-state)))))

(defn search-requests
  "Render the request list for a search input change."
  [ctx]
  (request-list-fragment ctx))

(defn select-request
  "Render the request list with one selected/expanded card."
  [ctx]
  (request-list-fragment ctx))

;; -----------------------------------------------------------------------------
;; Request lifecycle actions
;; -----------------------------------------------------------------------------

(defn lifecycle-action!
  "Shared request lifecycle action boundary.

   action is one of:
     :claim
     :unclaim
     :take-over
     :done
     :cancel

   store-fn receives:
     {:request-id ...
      :user ...}

   and returns either:
     {:status :ok ...}

   or:
     {:status :error ...}"
  [ctx action store-fn]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        id         (request-id ctx)
        result     (store-fn {:request-id id
                              :user user})]
    (if (= :ok (:status result))
      (let [{:keys [request revision previous]} result]
        (app-live/notify!
         (live-system ctx)
         ctx
         (app-live/request-transition-change
          {:action action
           :request request
           :previous previous
           :revision revision
           :actor user}))

        (html
         (views/request-lifecycle-result
          ctx
          (merge
           {:user user
            :action action
            :request request
            :previous previous
            :revision revision
            :view-state view-state}
           (board-oob ctx view-state)))))

      (html
       (views/request-action-error
        ctx
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
   store/claim-request!))

(defn unclaim-request!
  [ctx]
  (lifecycle-action!
   ctx
   :unclaim
   store/unclaim-request!))

(defn take-over-request!
  [ctx]
  (lifecycle-action!
   ctx
   :take-over
   store/take-over-request!))

(defn mark-request-done!
  [ctx]
  (lifecycle-action!
   ctx
   :done
   store/mark-request-done!))

(defn cancel-request!
  [ctx]
  (lifecycle-action!
   ctx
   :cancel
   store/cancel-request!))

;; -----------------------------------------------------------------------------
;; Dev/demo reset
;; -----------------------------------------------------------------------------

(defn reset-demo!
  [ctx]
  (let [user       (current-user ctx)
        result     (store/reset-demo-state!)
        view-state (assoc (request-view-state ctx)
                          :visible-revision
                          (:revision result))]
    (app-live/notify!
     (live-system ctx)
     ctx
     (app-live/demo-reset-change
      {:revision (:revision result)
       :actor user}))

    (app-live/send-reset-toast!)

    (html
     (views/reset-demo-result
      ctx
      (merge
       {:user user
        :result result
        :view-state view-state}
       (board-oob ctx view-state))))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:live-rules app-live/live-rules

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
