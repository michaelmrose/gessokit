(ns gessokit.humanhelp.components.request-card.core
  "Pure visual request-card component.

   This namespace owns only request-card markup and styling composition.

   It intentionally does not know about:
   - Human Help model rules
   - lifecycle action availability
   - routes
   - board-state parameters
   - Gesso Live optimistic descriptors
   - HTTP context

   Callers provide already-prepared presentation data and action nodes."
  (:require
   [gesso.core :as g]
   [gessokit.humanhelp.components.request-card.attr :as attr]))

;; -----------------------------------------------------------------------------
;; Visual state
;; -----------------------------------------------------------------------------

(defn- visual-request
  [{:keys [id
           request-status
           pending
           optimistic?
           fading-terminal?
           terminal-fade-remaining-ms]}]
  {:request/id id
   :request/status request-status
   :board/fading-terminal? fading-terminal?
   :board/terminal-fade-remaining-ms terminal-fade-remaining-ms
   :ui/pending? (boolean pending)
   :ui/optimistic? (boolean optimistic?)
   :ui/pending-action (:action pending)})

;; -----------------------------------------------------------------------------
;; Pills and metadata
;; -----------------------------------------------------------------------------

(defn status-pill
  [status]
  (when status
    (g/status-pill status)))

(defn pending-pill
  [{:keys [action aria-label attrs] :as pending}]
  (when pending
    (g/status-pill
     (-> pending
         (dissoc :action :aria-label :attrs)
         (assoc
          :attrs
          (merge
           {:data-humanhelp-request-pending-note true
            :data-humanhelp-request-pending-action
            (some-> action name)
            :aria-live "polite"
            :aria-label aria-label}
           attrs))))))

(defn request-meta
  [{:keys [status pending area waiting-text]}]
  [:div (attr/meta-attrs)
   (status-pill status)
   (pending-pill pending)

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text area})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text "·"})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text waiting-text})])

;; -----------------------------------------------------------------------------
;; Summary
;; -----------------------------------------------------------------------------

(defn request-summary
  [{:keys [title
           customer-name
           claimed-by-text
           open?]
    :as props}]
  [:summary (attr/summary-attrs)
   [:div (attr/header-stack-attrs)
    [:h3 (attr/title-attrs)
     title]

    (request-meta props)

    [:div (attr/customer-row-attrs)
     (g/text
      {:as :span
       :variant :small
       :class "weight-medium-theme"
       :text customer-name})

     (when claimed-by-text
       (g/muted-text
        {:as :span
         :class "text-xs-theme leading-body"
         :text claimed-by-text}))]]

   (g/icon
    "chevron-down"
    {:size :sm
     :class "shrink-0 transition-transform duration-200 ease-in-out"
     :attrs (attr/chevron-attrs open?)})])

;; -----------------------------------------------------------------------------
;; Content
;; -----------------------------------------------------------------------------

(defn request-actions
  [actions]
  (when (seq actions)
    (into
     [:div (attr/actions-attrs)]
     actions)))

(defn request-content
  [{:keys [details actions]}]
  (g/accordion-content
   (attr/details-attrs)

   (when details
     (g/text
      {:as :p
       :variant :small
       :class "leading-body"
       :text details}))

   (request-actions actions)))

;; -----------------------------------------------------------------------------
;; Card shell
;; -----------------------------------------------------------------------------

(defn- request-item-options
  [{:keys [id open? attrs] :as props}]
  (let [visual (visual-request props)]
    {:value id
     :open? open?
     :class (attr/item-class visual open?)
     :attrs (merge
             (attr/item-attrs visual open?)
             attrs)}))

(defn request-card
  "Render a request accordion item from prepared presentation props.

   Expected shape:

     {:id ...
      :request-status ...
      :title ...
      :status ...
      :pending ...
      :optimistic? ...
      :fading-terminal? ...
      :terminal-fade-remaining-ms ...
      :area ...
      :waiting-text ...
      :customer-name ...
      :claimed-by-text ...
      :details ...
      :actions [...]
      :open? false}"
  [props]
  (g/accordion-item
   (request-item-options props)
   (request-summary props)
   (request-content props)))
