(ns gessokit.humanhelp.components.request-card.core
  (:require
   [gesso.core :as g]
   [gesso.live.ui :as live]
   [gessokit.humanhelp.components.request-card.attr :as attr]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]))

;; -----------------------------------------------------------------------------
;; Status Pill Rendering
;; -----------------------------------------------------------------------------

(defn status-label
  [request]
  (model/request-status-label request))

(defn status-pill-status
  [request]
  (case (:request/status request)
    :open :waiting
    :claimed :active
    :done :success
    :cancelled :muted
    :destructive))

(defn request-status-pill
  [request]
  (g/status-pill
   {:status (status-pill-status request)
    :text (status-label request)
    :dot? true}))

(defn- confirming-pill
  [request]
  (when (:ui/pending? request)
    (g/status-pill
     {:status :muted
      :text "Confirming…"
      :dot? false
      :attrs
      {:data-humanhelp-request-pending-note true
       :data-humanhelp-request-pending-action
       (some-> (:ui/pending-action request) name)
       :aria-live "polite"
       :aria-label
       (str (or (:ui/pending-label request)
                "Updating…")
            " Awaiting server confirmation.")}})))

;; -----------------------------------------------------------------------------
;; Hidden Board-State Fallback
;; -----------------------------------------------------------------------------

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
             :value value}]))

(defn- true-input
  [name value]
  (when (true? value)
    (hidden-input name "true")))

(defn- created-order-input
  [created-order]
  (when (and (some? created-order)
             (not= :newest created-order))
    (hidden-input routes/created-order-param
                  (name created-order))))

(defn view-state-hidden-inputs
  "Render normalized board view-state as hidden inputs.

   The normal board path should pass :board-state-selector so every action
   includes the stable board-state form directly. These inputs are retained only
   as a compatibility fallback for request-card callers that do not yet supply
   that selector."
  [{:keys [search
           visible-revision
           created-order
           mine-first?
           unclaimed-first?
           show-terminal?]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/visible-revision-param visible-revision)
   (created-order-input created-order)
   (true-input routes/mine-first-param mine-first?)
   (true-input routes/unclaimed-first-param unclaimed-first?)
   (true-input routes/show-terminal-param show-terminal?)])

(defn- fallback-state-id
  [request]
  (str "humanhelp-request-"
       (:request/id request)
       "-action-state"))

(defn- action-state-selector
  [request board-state-selector]
  (or board-state-selector
      (str "#" (fallback-state-id request))))

(defn- fallback-view-state
  [request view-state board-state-selector]
  (when-not board-state-selector
    [:div {:id (fallback-state-id request)
           :data-humanhelp-request-action-state true
           :style {:display "contents"}}
     (view-state-hidden-inputs view-state)]))

;; -----------------------------------------------------------------------------
;; Optimistic State Generation
;; -----------------------------------------------------------------------------

(defn- action-pending-label
  [action]
  (case action
    :claim "Claiming…"
    :take-over "Taking over…"
    :unclaim "Unclaiming…"
    :done "Marking done…"
    :cancel "Canceling…"
    "Updating…"))

(defn- pending-request
  [request action pending-label]
  (assoc request
         :ui/pending? true
         :ui/optimistic? true
         :ui/pending-action action
         :ui/pending-label pending-label
         :ui/disable-actions? true))

(defn- optimistic-request
  [request user action]
  (let [pending-label (action-pending-label action)
        user-id       (:user/id user)
        user-email    (:user/email user)]
    (case action
      :claim
      (assoc (pending-request request action pending-label)
             :request/status :claimed
             :request/claimed-by user-id
             :request/claimed-by-email user-email
             :ui/claimed-by-me? true)

      :take-over
      (assoc (pending-request request action pending-label)
             :request/status :claimed
             :request/claimed-by user-id
             :request/claimed-by-email user-email
             :ui/claimed-by-me? true)

      :unclaim
      (assoc (pending-request request action pending-label)
             :request/status :open
             :request/claimed-by nil
             :request/claimed-by-email nil
             :ui/claimed-by-me? false)

      :done
      (pending-request request action pending-label)

      :cancel
      (pending-request request action pending-label)

      (pending-request request action pending-label))))

;; -----------------------------------------------------------------------------
;; Action Composition
;; -----------------------------------------------------------------------------

(def optimistic-target
  "The request-card element replaced by Gesso's optimistic runtime."
  "closest [data-humanhelp-request-card]")

(defn- action-variant
  [action]
  (case action
    :done :primary
    :claim :primary
    :take-over :primary
    :cancel :outline
    :unclaim :outline
    :default))

(defn- action-button-class
  [action]
  (case (action-variant action)
    :primary "btn-sm-primary"
    :outline "btn-sm-outline"
    "btn-sm"))

(defn- disabled-action-button
  [action]
  (g/button
   {:variant (action-variant action)
    :size :sm
    :text (model/action-label action)
    :attrs
    {:type "button"
     :disabled true
     :aria-disabled "true"
     :data-humanhelp-request-action (name action)}}))

(declare base-request-card)

(defn form-action
  "Render one request action through Gesso Live's post-button helper.

   The actual button owns hx-post and all optimistic protocol attrs. The wrapper
   form exists only for anti-forgery data, while :include adds either the stable
   board-state form or this card's compatibility fallback state."
  [ctx {:keys [to
               text
               action
               request
               user
               view-state
               board-state-selector
               pending-label
               disabled?
               attrs]}]
  (if disabled?
    (disabled-action-button action)
    (live/post-button
     ctx
     {:to to
      :target optimistic-target
      :swap "none"
      :include (action-state-selector request board-state-selector)
      :label text
      :form-attrs
      {:class "inline-flex"
       :data-humanhelp-request-action-form true}
      :button-attrs
      (merge
       {:class (action-button-class action)
        :data-humanhelp-request-action (name action)}
       attrs)
      :optimistic
      {:action action
       :pending-label pending-label
       :content
       (base-request-card
        ctx
        {:request (optimistic-request request user action)
         :user user
         :view-state view-state
         :board-state-selector board-state-selector
         :open? true
         :interactive? false})}})))

(defn action-button
  [ctx request user action view-state board-state-selector]
  (form-action
   ctx
   {:to (routes/action-url (:request/id request) action)
    :text (model/action-label action)
    :action action
    :request request
    :user user
    :view-state view-state
    :board-state-selector board-state-selector
    :pending-label (action-pending-label action)
    :disabled? (or (:ui/pending? request)
                   (:ui/disable-actions? request))}))

;; -----------------------------------------------------------------------------
;; Card Content
;; -----------------------------------------------------------------------------

(defn request-meta
  [request]
  [:div (attr/meta-attrs)
   (request-status-pill request)
   (confirming-pill request)

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text (:request/area request)})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text "·"})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text (str "waiting "
                (model/waiting-label request))})])

(defn request-card-actions
  [ctx request user view-state board-state-selector interactive?]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (if interactive?
         (map #(action-button
                ctx
                request
                user
                %
                view-state
                board-state-selector)
              actions)
         (map disabled-action-button actions))))))

(defn- claimed-by-label
  [request user]
  (cond
    (:ui/claimed-by-me? request)
    "you"

    (= (:request/claimed-by request)
       (:user/id user))
    "you"

    (:request/claimed-by-email request)
    (:request/claimed-by-email request)

    :else
    nil))

(defn request-summary
  [request user open?]
  [:summary (attr/summary-attrs)
   [:div (attr/header-stack-attrs)
    [:h3 (attr/title-attrs)
     (:request/title request)]

    (request-meta request)

    [:div (attr/customer-row-attrs)
     (g/text
      {:as :span
       :variant :small
       :class "weight-medium-theme"
       :text (:request/customer-name request)})

     (when-let [claimed-by
                (claimed-by-label request user)]
       (g/muted-text
        {:as :span
         :class "text-xs-theme leading-body"
         :text (str "claimed by " claimed-by)}))]]

   (g/icon
    "chevron-down"
    {:size :sm
     :class "shrink-0 transition-transform duration-200 ease-in-out"
     :attrs (attr/chevron-attrs open?)})])

(defn request-content
  [ctx request user view-state board-state-selector interactive?]
  (g/accordion-content
   (attr/details-attrs)

   (when (model/present? (:request/details request))
     (g/text
      {:as :p
       :variant :small
       :class "leading-body"
       :text (:request/details request)}))

   (when interactive?
     (fallback-view-state
      request
      view-state
      board-state-selector))

   (request-card-actions
    ctx
    request
    user
    view-state
    board-state-selector
    interactive?)))

;; -----------------------------------------------------------------------------
;; Card Shell
;; -----------------------------------------------------------------------------

(defn- request-item-attrs
  [request open?]
  (merge
   (attr/item-attrs request open?)
   (cond-> {}
     (:ui/pending? request)
     (assoc :data-humanhelp-request-pending "true")

     (:ui/optimistic? request)
     (assoc :data-humanhelp-request-optimistic "true")

     (:ui/pending-action request)
     (assoc :data-humanhelp-request-pending-action
            (name (:ui/pending-action request))))))

(defn- base-request-card
  [ctx {:keys [request
               user
               view-state
               board-state-selector
               open?
               interactive?]
        :or {interactive? true}}]
  (let [view-state (or view-state {})]
    (g/accordion-item
     {:value (:request/id request)
      :open? open?
      :class (attr/item-class request open?)
      :attrs (request-item-attrs request open?)}

     (request-summary request user open?)

     (request-content
      ctx
      request
      user
      view-state
      board-state-selector
      interactive?))))

;; -----------------------------------------------------------------------------
;; Public Card
;; -----------------------------------------------------------------------------

(defn request-card
  "Render a model-backed request accordion row.

   Gesso Live owns optimistic template identity, source attrs, target-local
   synchronization, anti-forgery inclusion, and browser rollback/reconciliation.
   This component still temporarily owns Human Help action selection, routes,
   optimistic domain projection, and presentation mapping; those are the next
   responsibilities to extract once this integration is proven."
  [ctx {:keys [request
               user
               view-state
               board-state-selector
               open?]
        :or {open? false}}]
  (base-request-card
   ctx
   {:request request
    :user user
    :view-state view-state
    :board-state-selector board-state-selector
    :open? open?
    :interactive? true}))
