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
   [clojure.string :as str]
   [com.biffweb.experimental :as biffx]
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.domain :as domain]
   [gessokit.humanhelp.live :as app-live]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.store :as store]
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
;; Stable live panels
;; -----------------------------------------------------------------------------

(def live-panel-trigger
  "Events that should refresh a Human Help live panel.

   Important: this trigger belongs on the stable SSE wrapper, not on the
   replaceable inner fragment root. The inner root is replaced by fragment
   responses and therefore must not own the long-lived SSE/fetch behavior."
  "load, pageshow from:window, focus from:window, visibilitychange from:document, online from:window, htmx:sseOpen, sse:live-update")

(defn fragment-dom-id
  [fragment-name]
  (case fragment-name
    :request-toolbar views/request-toolbar-dom-id
    :request-list views/request-list-dom-id
    (throw
     (ex-info "Unknown Human Help fragment DOM id."
              {:fragment fragment-name}))))

(defn fragment-label
  [fragment-name]
  (case fragment-name
    :request-toolbar "humanhelp-request-toolbar-fragment"
    :request-list "humanhelp-request-list-fragment"
    (throw
     (ex-info "Unknown Human Help fragment label."
              {:fragment fragment-name}))))

(defn fragment-url
  [fragment-name]
  (case fragment-name
    :request-toolbar (routes/request-toolbar-fragment-url)
    :request-list (routes/request-list-fragment-url)
    (throw
     (ex-info "Unknown Human Help fragment URL."
              {:fragment fragment-name}))))

(defn stream-url
  [fragment-name]
  (case fragment-name
    :request-toolbar (routes/request-toolbar-stream-url)
    :request-list (routes/request-list-stream-url)
    (throw
     (ex-info "Unknown Human Help stream URL."
              {:fragment fragment-name}))))

(defn live-panel
  "Render a stable Human Help live panel.

   The outer node owns:
   - hx-ext=\"sse\"
   - sse-connect
   - hx-get
   - hx-trigger
   - hx-include

   The inner node owns the fragment DOM id and is the replaceable target.

   Do not bake q/selected/visible-revision into hx-get. The current
   #humanhelp-board-state form is the source of truth for fragment fetches."
  [fragment-name _view-state]
  (let [dom-id (fragment-dom-id fragment-name)]
    [:div {:data-gesso-live-fragment (fragment-label fragment-name)
           :hx-ext "sse"
           :sse-connect (stream-url fragment-name)
           :hx-get (fragment-url fragment-name)
           :hx-include (str "#" views/board-state-form-id)
           :hx-trigger live-panel-trigger
           :hx-target (str "#" dom-id)
           :hx-swap "outerHTML"}
     [:div {:id dom-id}]]))

(defn page-panels
  "Return the stable live panels needed for the Human Help page."
  [view-state]
  {:request-toolbar-panel (live-panel :request-toolbar view-state)
   :request-list-panel (live-panel :request-list view-state)})

(defn board-state-form-oob
  "Render an OOB replacement for the board-state/search form.

   This keeps the hidden selected/visible-revision state in sync after actions
   that change board state outside the search form itself."
  [view-state]
  (g/oob-outer-html
   views/board-state-form-id
   (views/search-control
    {:view-state (store/normalize-view-state view-state)})))

(defn with-board-state-oob
  [node view-state]
  (views/oob-response
   node
   (board-state-form-oob view-state)))

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
     (page-panels view-state))))

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
     (with-board-state-oob
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
     (with-board-state-oob
       (views/refreshed-request-board-fragments
        ctx
        (board-oob ctx view-state))
       view-state))))

(defn search-requests
  "Render the request list for a search input change."
  [ctx]
  (request-list-fragment ctx))

(defn select-request
  "Render the request list with one selected/expanded card and sync board state."
  [ctx]
  (let [view-state (request-view-state ctx)]
    (html
     (with-board-state-oob
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
         (with-board-state-oob
           (views/request-lifecycle-result
            ctx
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
     (with-board-state-oob
       (views/reset-demo-result
        ctx
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
