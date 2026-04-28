(ns gessotest.form-flow-demo.core
  (:require
   [gesso.components.background.patterns :as bg]
   [gesso.components.bars.core :as bars]
   [gesso.core :as g]
   [gesso.util :as util]
   [gessotest.form-flow-demo.bars :as demo-bars]
   [gessotest.form-flow-demo.validation :as validation]
   [gessotest.middleware :as mid]
   [gessotest.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Route paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/pages/form-flow-demo")

(def account-path
  (str base-path "/account"))

(def account-validate-path
  (str base-path "/account/validate"))

(def store-path
  (str base-path "/store"))

(def store-validate-path
  (str base-path "/store/validate"))

(def preferences-path
  (str base-path "/preferences"))

(def preferences-validate-path
  (str base-path "/preferences/validate"))

;; -----------------------------------------------------------------------------
;; Server error response
;; -----------------------------------------------------------------------------

(defn- errors-response
  [errors]
  (if (seq errors)
    (g/html-response (g/render-oob-error-map errors))
    (g/no-content)))

;; -----------------------------------------------------------------------------
;; Layout helpers
;; -----------------------------------------------------------------------------

(defn- section-card
  [& children]
  (into
   [:section {:class "panel-theme radius-lg pad-card content-stack-theme shadow-sm"
              :style {:border "var(--border-width, 1px) solid var(--border)"
                      :background "color-mix(in srgb, var(--card) 96%, var(--background))"
                      :color "var(--card-foreground)"
                      :box-shadow "0 18px 50px color-mix(in srgb, black 22%, transparent)"}}]
   children))

(defn- section-heading
  [title description]
  [:div {:class "title-stack-theme"}
   [:h3 {:class "font-heading text-xl-theme leading-heading tracking-heading weight-semibold-theme"}
    title]
   [:p {:class "font-body text-sm-theme leading-body"
        :style {:color "var(--muted-foreground)"}}
    description]])

(defn- action-row
  [& children]
  (into
   [:div {:class "cluster-theme items-center justify-between"}]
   children))

(defn- muted-note
  [& children]
  (into
   [:p {:class "font-body text-sm-theme leading-body"
        :style {:color "var(--muted-foreground)"}}]
   children))

;; -----------------------------------------------------------------------------
;; Tabs
;; -----------------------------------------------------------------------------

(def steps
  [{:id :account
    :label "Account"}
   {:id :store
    :label "Store"}
   {:id :preferences
    :label "Preferences"}
   {:id :done
    :label "Done"}])

(defn- step-index
  [step-id]
  (or (some (fn [[idx step]]
              (when (= step-id (:id step))
                idx))
            (map-indexed vector steps))
      0))

(defn- tab-state
  [current-id step-id]
  (let [current (step-index current-id)
        idx     (step-index step-id)]
    (cond
      (= idx current) :current
      (< idx current) :complete
      :else :locked)))

(defn- tab-class
  [state]
  (case state
    :current
    "radius-lg px-4 py-2 font-body text-sm-theme weight-medium-theme"

    :complete
    "radius-lg px-4 py-2 font-body text-sm-theme opacity-80"

    :locked
    "radius-lg px-4 py-2 font-body text-sm-theme opacity-60"

    "radius-lg px-4 py-2 font-body text-sm-theme"))

(defn- tab-style
  [state]
  (case state
    :current
    {:background "var(--primary)"
     :color "var(--primary-foreground)"}

    :complete
    {:background "var(--secondary)"
     :color "var(--secondary-foreground)"}

    :locked
    {:background "var(--muted)"
     :color "var(--muted-foreground)"}

    {}))

(defn- tab-node
  [current-id {:keys [id label]}]
  (let [state (tab-state current-id id)]
    [:button {:type "button"
              :role "tab"
              :aria-selected (if (= state :current) "true" "false")
              :aria-current (when (= state :current) "step")
              :disabled true
              :class (tab-class state)
              :style (tab-style state)}
     label]))

(defn- tab-bar
  [current-id]
  [:nav {:role "tablist"
         :aria-label "Form steps"
         :class "cluster-theme"}
   (map #(tab-node current-id %) steps)])

;; -----------------------------------------------------------------------------
;; Account fields and sections
;; -----------------------------------------------------------------------------

(defn- username-field
  [values errors]
  (g/field
   {:label-text "Username"
    :for :username
    :schema validation/account-schema
    :control
    (g/input
     {:type "text"
      :name "username"

      ;; Demo-specific Firefox workaround:
      ;; Firefox's password-manager popup can cover the inline validation
      ;; message when this field is paired with a password field. This is
      ;; intentionally not semantically ideal; remove if this becomes a real
      ;; account/signup form.
      :autocomplete "new-password"
      :spellcheck "false"
      :value (:username values)})
    :description "Try 1demo, admin, support, root, michael, or demo to see server-side errors."
    :error (:username errors)}))

(defn- email-field
  [values errors]
  (g/field
   {:label-text "Email"
    :for :email
    :schema validation/account-schema
    :control (g/input
              {:type "email"
               :name "email"
               :autocomplete "email"
               :value (:email values)})
    :description "Try taken@example.com or used@example.com to see a server-side error."
    :error (:email errors)}))

(defn- password-field
  [_values errors]
  (g/field
   {:label-text "Password"
    :for :password
    :schema validation/account-schema
    :control (g/input
              {:type "password"
               :name "password"
               :autocomplete "new-password"})
    :description "At least 8 characters."
    :error (:password errors)}))

(defn- display-name-field
  [values errors]
  (g/field
   {:label-text "Display name"
    :for :displayName
    :schema validation/account-schema
    :control (g/input
              {:type "text"
               :name "displayName"
               :autocomplete "name"
               :value (:displayName values)})
    :description "Optional."
    :error (:displayName errors)}))

(defn- source-select
  [values]
  [:div {:class "content-stack-theme"}
   [:label {:class "label-theme"
            :for "source"}
    "How did you hear about this?"]
   (g/select
    {:id "source"
     :name "source"
     :value (:source values)
     :placeholder "Choose one"
     :options [{:value "friend" :label "Friend"}
               {:value "search" :label "Search"}
               {:value "other" :label "Other"}]})])

(defn- newsletter-checkbox
  [values]
  [:label {:class "inline-flex items-center gap-inline font-body text-sm-theme"}
   [:input (cond-> {:type "checkbox"
                    :name "newsletter"
                    :value "yes"}
             (util/checked-value? (:newsletter values) "yes")
             (assoc :checked true))]
   [:span "Send occasional updates"]])

(defn- account-primary-section
  [values errors]
  (section-card
   (section-heading
    "Step 1: Account"
    "Client-side rules run immediately. Server-side checks run after a valid field pauses or blurs.")
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (username-field values errors)
    (email-field values errors)]
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (password-field values errors)
    (display-name-field values errors)]))

(defn- account-nested-section
  [values]
  (section-card
   (section-heading
    "Arbitrary nested content"
    "This structure is not inspected by the form component. Named controls submit by normal HTML rules.")
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (source-select values)
    (newsletter-checkbox values)]))

(defn- account-actions
  []
  (action-row
   (muted-note "Only the current step is active. Completing this form advances to Store.")
   [:button {:type "submit"
             :class "btn-primary"}
    "Next"]))

(defn- validating-form-opts
  [post-path validate-path]
  {:post post-path
   :target "#complex-form-flow"
   :swap "outerHTML"
   :validate-url validate-path
   :class "content-stack-theme"
   :attrs {:novalidate true
           :autocomplete "off"}})

(defn account-form
  ([ctx]
   (account-form ctx {} {}))
  ([ctx values errors]
   (g/form ctx
     (validating-form-opts account-path account-validate-path)
     (account-primary-section values errors)
     (account-nested-section values)
     (account-actions))))

;; -----------------------------------------------------------------------------
;; Store fields and sections
;; -----------------------------------------------------------------------------

(defn- store-name-field
  [values errors]
  (g/field
   {:label-text "Store name"
    :for :storeName
    :schema validation/store-schema
    :control (g/input
              {:type "text"
               :name "storeName"
               :value (:storeName values)})
    :description "Try headquarters, corporate, or main office to see a server-side error."
    :error (:storeName errors)}))

(defn- store-type-field
  [values errors]
  (g/field
   {:label-text "Store type"
    :for :storeType
    :schema validation/store-schema
    :control (g/select
              {:name "storeType"
               :value (:storeType values)
               :placeholder "Choose one"
               :options [{:value "retail" :label "Retail"}
                         {:value "grocery" :label "Grocery"}
                         {:value "clinic" :label "Clinic"}
                         {:value "service" :label "Service"}]})
    :description "Required select control."
    :error (:storeType errors)}))

(defn- employee-count-field
  [values errors]
  (g/field
   {:label-text "Team size"
    :for :employeeCount
    :schema validation/store-schema
    :control (g/input
              {:type "number"
               :name "employeeCount"
               :value (:employeeCount values)})
    :description "1 to 500."
    :error (:employeeCount errors)}))

(defn- opening-date-field
  [values errors]
  (g/field
   {:label-text "Opening date"
    :for :openingDate
    :schema validation/store-schema
    :control (g/input
              {:type "date"
               :name "openingDate"
               :value (:openingDate values)})
    :description "Date picker control."
    :error (:openingDate errors)}))

(defn- timezone-field
  [values errors]
  (g/field
   {:label-text "Timezone"
    :for :timezone
    :schema validation/store-schema
    :control (g/select
              {:name "timezone"
               :value (:timezone values)
               :placeholder "Choose one"
               :options [{:value "America/Los_Angeles" :label "Pacific"}
                         {:value "America/Denver" :label "Mountain"}
                         {:value "America/Chicago" :label "Central"}
                         {:value "America/New_York" :label "Eastern"}]})
    :description "Another authored select control."
    :error (:timezone errors)}))

(defn- store-main-section
  [values errors]
  (section-card
   (section-heading
    "Step 2: Store"
    "This step exercises select, number, and date controls inside the same form shell.")
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (store-name-field values errors)
    (store-type-field values errors)]
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (employee-count-field values errors)
    (opening-date-field values errors)]
   (timezone-field values errors)))

(defn- store-actions
  []
  (action-row
   (muted-note "Completing this form advances to Preferences.")
   [:button {:type "submit"
             :class "btn-primary"}
    "Next"]))

(defn store-form
  ([ctx]
   (store-form ctx {} {}))
  ([ctx values errors]
   (g/form ctx
     (validating-form-opts store-path store-validate-path)
     (store-main-section values errors)
     (store-actions))))

;; -----------------------------------------------------------------------------
;; Preferences fields and sections
;; -----------------------------------------------------------------------------

(defn- contact-radio
  [preferred value label]
  [:label {:class "inline-flex items-center gap-inline font-body text-sm-theme"}
   [:input (cond-> {:type "radio"
                    :name "preferredContact"
                    :value value}
             (util/checked-value? preferred value)
             (assoc :checked true))]
   [:span label]])

(defn- preferred-contact-radios
  [values]
  (let [preferred (or (:preferredContact values) "email")]
    [:fieldset {:class "content-stack-theme"}
     [:legend {:class "label-theme"} "Preferred contact method"]
     [:div {:class "cluster-theme"}
      (contact-radio preferred "email" "Email")
      (contact-radio preferred "phone" "Phone")
      (contact-radio preferred "none" "None")]]))

(defn- phone-field
  [values errors]
  (g/field
   {:label-text "Phone"
    :for :phone
    :schema validation/preferences-schema
    :control (g/input
              {:type "tel"
               :name "phone"
               :autocomplete "tel"
               :value (:phone values)})
    :description "Required only if preferred contact is Phone."
    :error (:phone errors)}))

(defn- alert-email-field
  [values errors]
  (g/field
   {:label-text "Alert email"
    :for :alertEmail
    :schema validation/preferences-schema
    :control (g/input
              {:type "email"
               :name "alertEmail"
               :autocomplete "email"
               :value (:alertEmail values)})
    :description "Optional. Try bounce@example.com for a server-side error."
    :error (:alertEmail errors)}))

(defn- send-alerts-checkbox
  [values]
  [:label {:class "inline-flex items-center gap-inline font-body text-sm-theme"}
   [:input (cond-> {:type "checkbox"
                    :name "sendAlerts"
                    :value "yes"}
             (util/checked-value? (:sendAlerts values) "yes")
             (assoc :checked true))]
   [:span "Send urgent alerts"]])

(defn- notes-field
  [values errors]
  (g/field
   {:label-text "Notes"
    :for :notes
    :schema validation/preferences-schema
    :control (g/textarea
              {:name "notes"
               :value (:notes values)})
    :description "Optional. Max 240 characters."
    :error (:notes errors)}))

(defn- preferences-main-section
  [values errors]
  (section-card
   (section-heading
    "Step 3: Preferences"
    "This step includes radio buttons, a toggle-like checkbox, optional fields, and final submit.")
   (preferred-contact-radios values)
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-4"}
    (phone-field values errors)
    (alert-email-field values errors)]
   (send-alerts-checkbox values)
   (notes-field values errors)))

(defn- preferences-actions
  []
  (action-row
   (muted-note "Final submit returns a success message.")
   [:button {:type "submit"
             :class "btn-primary"}
    "Submit"]))

(defn preferences-form
  ([ctx]
   (preferences-form ctx {} {}))
  ([ctx values errors]
   (g/form ctx
     (validating-form-opts preferences-path preferences-validate-path)
     (preferences-main-section values errors)
     (preferences-actions))))

;; -----------------------------------------------------------------------------
;; Flow fragments
;; -----------------------------------------------------------------------------

(def flow-target-id
  "complex-form-flow")

(def initial-step
  :account)

(defn- flow-root
  "Wrap flow content in the stable HTMX target.

   Normal active-step fragments and the final done fragment both replace this
   same DOM node."
  [& children]
  (into
   [:div {:id flow-target-id
          :class "content-stack-theme"}]
   children))

(defn- active-step-form
  "Render the form for the current in-progress step.

   The terminal :done state is intentionally not handled here. Completion uses
   done-fragment, which replaces the whole flow target."
  [ctx current values errors]
  (case current
    :account
    (account-form ctx values errors)

    :store
    (store-form ctx values errors)

    :preferences
    (preferences-form ctx values errors)

    ;; Defensive fallback for unknown states.
    (account-form ctx values errors)))

(defn- flow-intro-card
  [current]
  (section-card
   (section-heading
    "Tabbed form flow"
    "Each tab is a separate form. The active tab advances only after the current form validates and submits.")
   (tab-bar current)))

(defn flow-fragment
  "Render the active form-flow fragment.

   This is the normal fragment for :account, :store, and :preferences. Failed
   validation can re-render the same step with submitted values and errors."
  ([ctx current]
   (flow-fragment ctx current {} {}))
  ([ctx current values errors]
   (flow-root
    (flow-intro-card current)
    (active-step-form ctx current values errors))))

(defn- done-panel
  []
  [:div {:class "panel-theme radius-lg pad-card"
         :style {:border "var(--border-width, 1px) solid var(--border)"
                 :background "color-mix(in srgb, var(--card) 96%, var(--background))"
                 :color "var(--card-foreground)"
                 :box-shadow "0 18px 50px color-mix(in srgb, black 22%, transparent)"}}
   [:p {:class "font-body text-base-theme leading-body"}
    "Success. This page exercised chained forms, disabled tab navigation, local browser validation, server-side OOB validation, arbitrary nested content, and final submission."]])

(defn done-fragment
  "Render the terminal success fragment.

   This replaces the same #complex-form-flow target as flow-fragment, but it is
   no longer an active form step."
  [_ctx]
  (flow-root
   (section-card
    (section-heading
     "Form submitted"
     "The final form submitted successfully. No data was persisted in this demo.")
    (tab-bar :done)
    (done-panel)
    [:a {:href base-path
         :class "btn-outline"}
     "Start over"])))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn- page-content
  [ctx]
  [:div {:class "pad-container content-stack-theme"}
   [:header {:class "title-stack-theme"}
    [:div {:class "font-body text-sm-theme weight-medium-theme tracking-wide-theme uppercase"
           :style {:color "var(--muted-foreground)"}}
     "Gesso demo"]

    [:h1 {:class "font-heading text-3xl-theme leading-heading tracking-heading weight-bold-theme"}
     "Complex Form Flow"]

    [:p {:class "font-body text-base-theme leading-body"
         :style {:color "var(--muted-foreground)"}}
     "A tabbed, route-driven, multi-step form demo using the public Gesso facade: field, form, validation, and HTMX OOB error targets."]]

   (flow-fragment ctx :account)

   (g/scroll-buffer {:size :lg})])

(defn page-view
  [ctx]
  [:div {:class "relative isolate min-h-screen overflow-hidden"}
   (g/background {:light bg/orb-grid-background-light
                  :dark bg/orb-grid-background-light})

   [:div {:class "relative z-10"}
    (bars/bars
     {:brand (demo-bars/brand base-path)
      :sidebar-collapse-at :medium
      :menus (demo-bars/menus base-path)}
     (page-content ctx))]])

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn page
  [ctx]
  (ui/page-shell ctx (page-view ctx)))

(defn account-page
  [ctx]
  (g/html-response (flow-fragment ctx :account)))

(defn store-page
  [ctx]
  (g/html-response (flow-fragment ctx :store)))

(defn preferences-page
  [ctx]
  (g/html-response (flow-fragment ctx :preferences)))

(defn validate-account
  [{:keys [params]}]
  (errors-response
   (validation/server-account-errors
    (validation/account-values params))))

(defn validate-store
  [{:keys [params]}]
  (errors-response
   (validation/server-store-errors
    (validation/store-values params))))

(defn validate-preferences
  [{:keys [params]}]
  (errors-response
   (validation/server-preferences-errors
    (validation/preferences-values params))))

(defn submit-account
  [{:keys [params] :as ctx}]
  (let [values (validation/account-values params)
        errors (validation/submit-account-errors values)]
    (g/html-response
     (if (seq errors)
       (flow-fragment ctx :account values errors)
       (flow-fragment ctx :store)))))

(defn submit-store
  [{:keys [params] :as ctx}]
  (let [values (validation/store-values params)
        errors (validation/submit-store-errors values)]
    (g/html-response
     (if (seq errors)
       (flow-fragment ctx :store values errors)
       (flow-fragment ctx :preferences)))))

(defn submit-preferences
  [{:keys [params] :as ctx}]
  (let [values (validation/preferences-values params)
        errors (validation/submit-preferences-errors values)]
    (g/html-response
     (if (seq errors)
       (flow-fragment ctx :preferences values errors)
       (done-fragment ctx)))))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["" {:get page}]

     ["/account"
      {:get account-page
       :post submit-account}]

     ["/account/validate"
      {:post validate-account}]

     ["/store"
      {:get store-page
       :post submit-store}]

     ["/store/validate"
      {:post validate-store}]

     ["/preferences"
      {:get preferences-page
       :post submit-preferences}]

     ["/preferences/validate"
      {:post validate-preferences}]]]})
