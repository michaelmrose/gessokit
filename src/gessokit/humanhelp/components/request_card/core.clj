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

(defn card-selected?
  [request view-state]
  (= (:request/id request)
     (:selected-request-id view-state)))

(defn selection-url
  [request selected? view-state]
  (if selected?
    (routes/clear-selection-url view-state)
    (routes/select-request-url (:request/id request) view-state)))

(defn request-meta
  [request]
  [:div (attr/meta-attrs)
   (request-status-pill request)

   [:span (attr/meta-text-attrs)
    (:request/area request)]

   [:span (attr/meta-text-attrs)
    "·"]

   [:span (attr/meta-text-attrs)
    "waiting "
    (model/waiting-label request)]])

(defn request-card-actions
  [ctx request user view-state]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (map #(action-button ctx request % view-state))
       actions))))

(defn- require-request-list-dom-id
  [request-list-dom-id]
  (or request-list-dom-id
      (throw
       (ex-info "request-card requires :request-list-dom-id"
                {}))))

(defn request-card
  [ctx {:keys [request user view-state request-list-dom-id]}]
  (let [view-state          (or view-state {})
        request-list-dom-id (require-request-list-dom-id request-list-dom-id)
        selected?           (card-selected? request view-state)
        href                (selection-url request selected? view-state)]
    [:article (attr/card-attrs request selected?)
     [:a (attr/selection-link-attrs
          {:href href
           :request-list-dom-id request-list-dom-id})
      [:div (attr/header-row-attrs)
       [:div (attr/title-stack-attrs)
        [:h3 (attr/title-attrs)
         (:request/title request)]
        (request-meta request)]
       (g/icon (if selected? "chevron-up" "chevron-down")
               {:size :sm})]

      [:div (attr/customer-row-attrs)
       [:span (attr/customer-name-attrs)
        (:request/customer-name request)]

       (when-let [claimed-by (:request/claimed-by-email request)]
         [:span (attr/claimed-by-attrs)
          (str "claimed by " claimed-by)])]]

     (when selected?
       [:div (attr/details-stack-attrs)
        (when (model/present? (:request/details request))
          [:p (attr/details-text-attrs)
           (:request/details request)])

        (request-card-actions ctx request user view-state)])]))
