(ns gessokit.humanhelp.components.request-card.core
  (:require
   [gesso.core :as g]
   [gessokit.humanhelp.components.request-card.attr :as attr]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]))

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

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
             :value value}]))

(defn view-state-hidden-inputs
  "Render full view-state hidden inputs for request action forms.

   Action forms do not have their own visible search input, so they need to
   preserve q/search, selected request id, and visible revision."
  [{:keys [search selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/selected-param selected-request-id)
   (hidden-input routes/visible-revision-param visible-revision)])

(defn form-action
  [ctx {:keys [to text variant size view-state attrs]}]
  [:form
   (attr/action-form-attrs
    {:to to
     :attrs attrs})
   (g/anti-forgery-input ctx)
   (view-state-hidden-inputs view-state)
   (g/button
    {:variant (or variant :default)
     :size (or size :sm)
     :text text
     :attrs {:type "submit"}})])

(defn action-button
  [ctx request action view-state]
  (form-action
   ctx
   {:to (routes/action-url (:request/id request) action)
    :text (model/action-label action)
    :variant (case action
               :done :primary
               :claim :primary
               :take-over :primary
               :cancel :outline
               :unclaim :outline
               :default)
    :view-state view-state}))

(defn card-open?
  [request view-state]
  (= (:request/id request)
     (:selected-request-id view-state)))

(defn request-meta
  [request]
  [:div (attr/meta-attrs)
   (request-status-pill request)

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
     :text (str "waiting " (model/waiting-label request))})])

(defn request-card-actions
  [ctx request user view-state]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (map #(action-button ctx request % view-state))
       actions))))

(defn request-summary
  [request open?]
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

     (when-let [claimed-by (:request/claimed-by-email request)]
       (g/muted-text
        {:as :span
         :class "text-xs-theme leading-body"
         :text (str "claimed by " claimed-by)}))]]

   (g/icon "chevron-down"
           {:size :sm
            :class "shrink-0 transition-transform duration-200 ease-in-out"
            :attrs (attr/chevron-attrs open?)})])

(defn request-content
  [ctx request user view-state]
  (g/accordion-content
   {:class (attr/details-stack-class)}
   (when (model/present? (:request/details request))
     (g/text
      {:as :p
       :variant :small
       :text (:request/details request)}))
   (request-card-actions ctx request user view-state)))

(defn request-card
  [ctx {:keys [request user view-state]}]
  (let [view-state (or view-state {})
        open?      (card-open? request view-state)]
    (g/accordion-item
     {:value (:request/id request)
      :open? open?
      :class (attr/item-class)
      :attrs (attr/item-attrs request open?)}
     (request-summary request open?)
     (request-content ctx request user view-state))))
