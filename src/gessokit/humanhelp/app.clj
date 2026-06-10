(ns gessokit.humanhelp.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace intentionally stays thin. It wires together:

   - gessokit.humanhelp.model
   - gessokit.humanhelp.data
   - gessokit.humanhelp.views
   - gessokit.humanhelp.live
   - gessokit.client-plumbing

   The Human Help feature code is isolated under gessokit.humanhelp.* so the
   example app can be removed cleanly from a generated template project."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.data :as data]
   [gessokit.humanhelp.live :as app-live]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.views :as views]
   [gessokit.middleware :as mid])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------

(defn scalar-param-value
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

(defn param
  "Read a Ring/Biff request param by keyword or string key.

   This stays in the HTTP boundary because it is a request-shape concern.

   Supports plain Ring-style maps as well as common Reitit match placement.
   Repeated params are normalized with scalar-param-value."
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

(defn request-id
  [ctx]
  (param ctx :request-id))

;; -----------------------------------------------------------------------------
;; Current user
;; -----------------------------------------------------------------------------

(defn session-uid
  "Return the signed-in user's session id, when present.

   Biff auth commonly stores the user id at [:session :uid]. Other keys are
   included as tolerant fallbacks for tests/dev middleware."
  [ctx]
  (or (get-in ctx [:session :uid])
      (get-in ctx [:session :user])
      (:user/id ctx)
      (get-in ctx [:user :xt/id])))

(defn ->uuid
  [x]
  (cond
    (uuid? x)
    x

    (string? x)
    (try
      (UUID/fromString x)
      (catch Exception _
        nil))

    :else
    nil))

(defn emailish?
  "True when x looks like a displayable email address rather than a UUID/id.

   This is intentionally lightweight. It is not a full email validator; it just
   prevents UUID/session ids from being used as display emails when a real email
   is available elsewhere."
  [x]
  (and (string? x)
       (re-find #"@" x)))

(defn user-email-from-ctx
  "Return a directly attached email from ctx, when one is present.

   These keys cover tests, dev middleware, and common app/user shapes."
  [ctx]
  (some
   (fn [x]
     (when (emailish? x)
       x))
   [(:user/email ctx)
    (:user/email (:user ctx))
    (get-in ctx [:user :email])
    (get-in ctx [:session :email])
    (get-in ctx [:identity :email])
    (get-in ctx [:params :email])
    (get-in ctx [:params "email"])]))

(defn user-email-from-db
  "Look up the signed-in user's real email address from XTDB.

   client-plumbing/current-user-email is intentionally a generic best-effort
   helper. Human Help wants the actual user email for display, so this boundary
   resolves it from the app's :user table when possible."
  [ctx]
  (let [conn (:biff/conn ctx)
        uid  (->uuid (session-uid ctx))]
    (when (and conn uid)
      (try
        (some-> (biffx/q conn
                         {:select [:user/email]
                          :from :user
                          :where [:= :xt/id uid]})
                first
                :user/email)
        (catch Exception _
          nil)))))

(defn current-user-email
  "Return the email Human Help should display.

   Prefer real email values from ctx/session/DB. Only fall back to the generic
   client-plumbing display helper if no email can be found."
  [ctx]
  (or (user-email-from-ctx ctx)
      (user-email-from-db ctx)
      (client-plumbing/current-user-email ctx)))

(defn current-user
  "Return the Human Help demo user descriptor.

   The demo intentionally does not enforce a real helper/helpee split, but it
   still needs:
   - a stable user id for ownership/client targeting
   - the actual email for display and claim labels"
  [ctx]
  {:user/id (client-plumbing/current-user-id ctx)
   :user/email (current-user-email ctx)})

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
   A nil visible revision is normalized to the latest known revision before
   rendering."
  [ctx]
  {:search (or (param ctx :q) "")
   :selected-request-id (param ctx :selected)
   :visible-revision (model/parse-visible-revision
                      (param ctx :visible-revision))})

(defn create-request-input
  "Extract create-request form input from request params.

   This deliberately uses the HTTP-boundary param helper so repeated browser
   params are normalized before reaching the model parser."
  [ctx]
  (model/parse-create-request-input
   {:title (param ctx :title)
    :area (param ctx :area)
    :details (param ctx :details)
    :customer-name (param ctx :customer-name)}))

(defn html
  [node]
  (g/html-response node))

;; -----------------------------------------------------------------------------
;; Board-state OOB
;; -----------------------------------------------------------------------------

(defn board-state-form-oob
  "Render an OOB replacement for the board-state/search form.

   This keeps the hidden selected/visible-revision state in sync after actions
   that change board state outside the search form itself."
  [ctx view-state]
  (g/oob-outer-html
   views/board-state-form-id
   (views/search-control
    ctx
    {:view-state (data/normalize-view-state ctx view-state)})))

(defn with-board-state-oob
  [ctx node view-state]
  (views/oob-response
   ;; Put board-state first so the browser updates the hidden state before
   ;; processing any other OOB fragments from the same response.
   (board-state-form-oob ctx view-state)
   node))

;; -----------------------------------------------------------------------------
;; Render helpers
;; -----------------------------------------------------------------------------

(defn page-data
  [ctx]
  (let [user       (current-user ctx)
        view-state (data/normalize-view-state
                    ctx
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

(defn previous-revision
  [revision]
  (when (number? revision)
    (max 0 (dec revision))))

(defn receiver-view-state-for-new-request
  "Return the receiving browser's view-state for a new-request notification.

   Normally the pending client-plumbing request includes #humanhelp-board-state,
   so the receiving browser supplies its actual visible-revision. If that input
   is missing for any reason, fall back to a definitely-stale visible revision
   rather than rendering a non-glowing toolbar."
  [ctx revision]
  (let [view-state (request-view-state ctx)]
    (data/normalize-view-state
     ctx
     (cond-> view-state
       (nil? (:visible-revision view-state))
       (assoc :visible-revision (previous-revision revision))))))

(defn new-request-client-oob
  "Return a client-plumbing pending fragment function for a newly-created request.

   This runs at drain time using the receiving browser's ctx, so the toolbar is
   rendered using that browser's search/selected/visible-revision state. It also
   sends the new-request toast through the same immediate client-plumbing path."
  [request revision]
  (fn [receiver-ctx]
    (let [view-state (receiver-view-state-for-new-request receiver-ctx revision)
          toolbar    (render-toolbar-node receiver-ctx view-state)]
      (views/oob-response
       (views/replace-toolbar-oob toolbar)
       (g/render-toast-oob
        {:variant :info
         :duration 5000
         :title "New request received"
         :description (app-live/request-toast-description request)})))))

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
;; Best-effort side effects
;; -----------------------------------------------------------------------------

(defn send-new-request-ui-safely!
  [request revision user]
  (try
    (client-plumbing/send-to-scope-except-user!
     app-live/notification-scope
     (:user/id user)
     (new-request-client-oob request revision))
    (catch Exception e
      (println "[humanhelp] send-new-request-ui! failed"
               {:message (.getMessage e)}))))

(defn send-reset-toast-safely!
  []
  (try
    (app-live/send-reset-toast!)
    (catch Exception e
      (println "[humanhelp] send-reset-toast! failed"
               {:message (.getMessage e)}))))

;; -----------------------------------------------------------------------------
;; Request creation
;; -----------------------------------------------------------------------------

(defn create-request-success-response
  [ctx {:keys [request revision view-state]}]
  (let [user         (current-user ctx)
        view-state'  (assoc view-state
                             :visible-revision revision
                             :selected-request-id (:request/id request))
        toolbar      (render-toolbar-node ctx view-state')
        request-list (render-list-node ctx view-state')]
    (html
     (with-board-state-oob
       ctx
       (views/create-request-success
        ctx
        {:user user
         :request request
         :toolbar toolbar
         :request-list request-list})
       view-state'))))

(defn create-request!
  "Create a new request from the modal dialog.

   Creator behavior:
   - request is created
   - dialog closes
   - visible list refreshes to include the new request

   Other connected users:
   - receive one immediate client-plumbing OOB response containing:
     - new-request toast
     - stale request toolbar with glowing refresh affordance
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

        ;; Do not emit the model-backed :request/created invalidation here.
        ;;
        ;; The create POST response already OOB-replaces the creator's toolbar,
        ;; list, dialog, and board-state form at the new visible revision.
        ;;
        ;; Other connected users get the stale-toolbar affordance and toast via
        ;; client plumbing, using the same immediate wake-up path that already
        ;; delivers page-global OOB work.
        (send-new-request-ui-safely! request revision user)

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
       (views/refreshed-request-board-fragments
        (board-oob ctx view-state))
       view-state))))

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
       (render-list-node ctx view-state)
       view-state))))

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

   transition-fn receives ctx and:
     {:request-id ...
      :user ...}

   and returns either:
     {:status :ok ...}

   or:
     {:status :error ...}"
  [ctx action transition-fn]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        id         (request-id ctx)
        result     (transition-fn ctx
                                  {:request-id id
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
         (with-board-state-oob
           ctx
           (views/request-lifecycle-result
            (merge
             {:user user
              :action action
              :request request
              :previous previous
              :revision revision
              :view-state view-state}
             (board-oob ctx view-state)))
           view-state)))

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
    (app-live/notify!
     (live-system ctx)
     ctx
     (app-live/demo-reset-change
      {:revision (:revision result)
       :actor user}))

    (send-reset-toast-safely!)

    (html
     (with-board-state-oob
       ctx
       (views/reset-demo-result
        (merge
         {:user user
          :result result
          :view-state view-state}
         (board-oob ctx view-state)))
       view-state))))

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
