(ns gessotest.form-flow-demo
  (:require
   [clojure.string :as str]
   [gesso.components.background.patterns :as bg]
   [gesso.components.bars.core :as bars]
   [gesso.core :as g]
   [gessotest.middleware :as mid]
   [gessotest.ui :as ui]
   [rum.core :as rum]))

;; -----------------------------------------------------------------------------
;; Responses
;; -----------------------------------------------------------------------------

(defn- html-response
  [body]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body (rum/render-static-markup body)})

(defn- no-content
  []
  {:status 204
   :headers {}
   :body ""})

;; -----------------------------------------------------------------------------
;; Route paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/demo/complex-form-flow")

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
;; Small request/value helpers
;; -----------------------------------------------------------------------------

(defn- param
  [params k]
  (or (get params k)
      (get params (name k))))

(defn- present?
  [x]
  (and (some? x)
       (not (str/blank? (str x)))))

(defn- trim-value
  [x]
  (when (some? x)
    (str/trim (str x))))

(defn- parse-int*
  [x]
  (when (present? x)
    (try
      (Integer/parseInt (str x))
      (catch Exception _
        x))))

(defn- maybe-value
  [attrs v]
  (if (some? v)
    (assoc attrs :value v)
    attrs))

(defn- checked?
  [value expected]
  (= (str value) (str expected)))

(defn- option
  [current value label]
  [:option (cond-> {:value value}
             (= (str current) (str value))
             (assoc :selected true))
   label])

;; -----------------------------------------------------------------------------
;; Demo data and validation helpers
;; -----------------------------------------------------------------------------

(def taken-usernames
  #{"admin" "support" "root" "michael" "demo"})

(def taken-emails
  #{"taken@example.com"
    "used@example.com"})

(def reserved-store-names
  #{"headquarters"
    "corporate"
    "main office"})

(def bounced-alert-emails
  #{"bounce@example.com"
    "invalid-alert@example.com"})

(defn- valid-username-shape?
  [s]
  (boolean
   (and (present? s)
        (<= 3 (count s) 24)
        (re-matches #"^[a-z0-9_-]+$" s))))

(defn- valid-email-shape?
  [s]
  (boolean
   (and (present? s)
        (<= 5 (count s) 120)
        (re-matches #".+@.+" s))))

(defn- valid-phone-shape?
  [s]
  (or (not (present? s))
      (boolean (re-matches #"^[0-9+(). -]{7,32}$" s))))

(defn- date-like?
  [s]
  (boolean
   (and (present? s)
        (re-matches #"\d{4}-\d{2}-\d{2}" s))))

;; -----------------------------------------------------------------------------
;; Submitted values
;; -----------------------------------------------------------------------------

(defn- account-values
  [params]
  {:username    (trim-value (param params :username))
   :email       (trim-value (param params :email))
   :password    (param params :password)
   :displayName (trim-value (param params :displayName))
   :source      (param params :source)
   :newsletter  (param params :newsletter)})

(defn- store-values
  [params]
  {:storeName     (trim-value (param params :storeName))
   :storeType     (param params :storeType)
   :employeeCount (parse-int* (param params :employeeCount))
   :openingDate   (param params :openingDate)
   :timezone      (param params :timezone)})

(defn- preferences-values
  [params]
  {:preferredContact (or (param params :preferredContact) "email")
   :phone            (trim-value (param params :phone))
   :alertEmail       (trim-value (param params :alertEmail))
   :sendAlerts       (param params :sendAlerts)
   :notes            (param params :notes)})

;; -----------------------------------------------------------------------------
;; Server-side demo errors
;; -----------------------------------------------------------------------------

(defn- server-account-errors
  [{:keys [username email]}]
  (cond-> {}
    (and (valid-username-shape? username)
         (contains? taken-usernames (str/lower-case username)))
    (assoc :username "That username is already taken.")

    (and (valid-email-shape? email)
         (contains? taken-emails (str/lower-case email)))
    (assoc :email "That email is already in use.")))

(defn- submit-account-errors
  [{:keys [username email password displayName] :as values}]
  (merge
   (cond
     (not (present? username))
     {:username "Username is required."}

     (< (count username) 3)
     {:username "Username must be at least 3 characters."}

     (> (count username) 24)
     {:username "Username must be at most 24 characters."}

     (not (valid-username-shape? username))
     {:username "Use lowercase letters, numbers, underscores, or hyphens."}

     :else
     {})

   (cond
     (not (present? email))
     {:email "Email is required."}

     (not (valid-email-shape? email))
     {:email "Enter an email address."}

     :else
     {})

   (cond
     (not (present? password))
     {:password "Password is required."}

     (< (count (str password)) 8)
     {:password "Password must be at least 8 characters."}

     :else
     {})

   (when (and (present? displayName)
              (> (count displayName) 80))
     {:displayName "Display name must be at most 80 characters."})

   (server-account-errors values)))

(defn- server-store-errors
  [{:keys [storeName]}]
  (cond-> {}
    (and (present? storeName)
         (contains? reserved-store-names (str/lower-case storeName)))
    (assoc :storeName "That store name is reserved for this demo.")))

(defn- submit-store-errors
  [{:keys [storeName storeType employeeCount openingDate timezone] :as values}]
  (merge
   (cond
     (not (present? storeName))
     {:storeName "Store name is required."}

     (< (count storeName) 2)
     {:storeName "Store name must be at least 2 characters."}

     :else
     {})

   (when-not (present? storeType)
     {:storeType "Choose a store type."})

   (cond
     (not (integer? employeeCount))
     {:employeeCount "Enter a team size."}

     (< employeeCount 1)
     {:employeeCount "Team size must be at least 1."}

     (> employeeCount 500)
     {:employeeCount "Team size must be 500 or less."}

     :else
     {})

   (when-not (date-like? openingDate)
     {:openingDate "Choose an opening date."})

   (when-not (present? timezone)
     {:timezone "Choose a timezone."})

   (server-store-errors values)))

(defn- server-preferences-errors
  [{:keys [alertEmail]}]
  (cond-> {}
    (and (valid-email-shape? alertEmail)
         (contains? bounced-alert-emails (str/lower-case alertEmail)))
    (assoc :alertEmail "That alert email is blocked in this demo.")))

(defn- submit-preferences-errors
  [{:keys [preferredContact phone alertEmail notes] :as values}]
  (merge
   (when (and (= preferredContact "phone")
              (not (present? phone)))
     {:phone "Enter a phone number or choose another contact method."})

   (when-not (valid-phone-shape? phone)
     {:phone "Use digits, spaces, dashes, parentheses, or +."})

   (when (and (present? alertEmail)
              (not (valid-email-shape? alertEmail)))
     {:alertEmail "Enter an email address."})

   (when (and (present? notes)
              (> (count (str notes)) 240))
     {:notes "Notes must be 240 characters or fewer."})

   (server-preferences-errors values)))

;; -----------------------------------------------------------------------------
;; Schemas for field-level browser validation
;; -----------------------------------------------------------------------------

(def account-schema
  [:map
   [:username
    [:and
     [:string {:min 3
               :max 24
               :gesso.html/pattern "^[a-z_\\-]+[0-9]*$"
               :gesso.error/min "Username must be at least 3 characters."
               :gesso.error/max "Username must be at most 24 characters."
               :gesso.error/pattern "Use lowercase letters, numbers, underscores, or hyphens. Cant start with a number"
               :gesso.error/required "Username is required."}]
     [:re #"^[a-z0-9_-]+$"]]]

   [:email
    [:and
     [:string {:min 5
               :max 120
               :gesso.html/pattern ".+@.+"
               :gesso.error/pattern "Enter an email address."
               :gesso.error/required "Email is required."}]
     [:re #".+@.+"]]]

   [:password
    [:string {:min 8
              :max 128
              :gesso.error/min "Password must be at least 8 characters."
              :gesso.error/required "Password is required."}]]

   [:displayName {:optional true}
    [:maybe [:string {:max 80
                      :gesso.error/max "Display name must be at most 80 characters."}]]]])

(def store-schema
  [:map
   [:storeName
    [:string {:min 2
              :max 80
              :gesso.error/min "Store name must be at least 2 characters."
              :gesso.error/max "Store name must be at most 80 characters."
              :gesso.error/required "Store name is required."}]]

   [:storeType
    [:string {:min 1
              :gesso.error/required "Choose a store type."}]]

   [:employeeCount
    [:int {:min 1
           :max 500
           :gesso.error/min "Team size must be at least 1."
           :gesso.error/max "Team size must be 500 or less."
           :gesso.error/required "Team size is required."}]]

   [:openingDate
    [:and
     [:string {:gesso.html/pattern "\\d{4}-\\d{2}-\\d{2}"
               :gesso.error/pattern "Choose an opening date."
               :gesso.error/required "Opening date is required."}]
     [:re #"\d{4}-\d{2}-\d{2}"]]]

   [:timezone
    [:string {:min 1
              :gesso.error/required "Choose a timezone."}]]])

(def preferences-schema
  [:map
   [:phone {:optional true}
    [:maybe
     [:and
      [:string {:max 32
                :gesso.html/pattern "^[0-9+(). -]{7,32}$"
                :gesso.error/pattern "Use digits, spaces, dashes, parentheses, or +."
                :gesso.error/max "Phone number is too long."}]
      [:re #"^[0-9+(). -]{7,32}$"]]]]

   [:alertEmail {:optional true}
    [:maybe
     [:and
      [:string {:max 120
                :gesso.html/pattern ".+@.+"
                :gesso.error/pattern "Enter an email address."
                :gesso.error/max "Alert email is too long."}]
      [:re #".+@.+"]]]]

   [:notes {:optional true}
    [:maybe [:string {:max 240
                      :gesso.error/max "Notes must be 240 characters or fewer."}]]]])

;; -----------------------------------------------------------------------------
;; OOB server error rendering
;; -----------------------------------------------------------------------------

(defn- js-string
  [s]
  (pr-str (str s)))

(defn- reveal-error-script
  [field-id err-id]
  (str
   "(function(){"
   "var err=document.getElementById(" (js-string err-id) ");"
   "if(err){"
   "err.classList.remove('hidden');"
   "err.dataset.serverError='true';"
   "}"
   "var field=document.getElementById(" (js-string field-id) ");"
   "if(field){field.setAttribute('aria-invalid','true');}"
   "})();"))

(defn- field-error-oob
  [field-path message]
  (let [field-id (g/path->field-id field-path)
        err-id   (g/path->err-id field-path)]
    [:div {:id err-id
           :hx-swap-oob "innerHTML"}
     [:span {:data-validation-error-message true
             :style {:color "var(--destructive)"}}
      message]
     [:script {:dangerouslySetInnerHTML
               {:__html (reveal-error-script field-id err-id)}}]]))

(defn- errors-fragment
  [errors]
  (into [:<>]
        (map (fn [[field-path message]]
               (field-error-oob field-path message)))
        errors))

(defn- errors-response
  [errors]
  (if (seq errors)
    (html-response (errors-fragment errors))
    (no-content)))

;; -----------------------------------------------------------------------------
;; Controls and layout helpers
;; -----------------------------------------------------------------------------

(defn- control-class
  [kind]
  (str kind
       " control-theme w-full rounded-lg border-theme bg-background text-foreground "
       "font-body text-base-theme leading-body px-3 py-2 shadow-sm "
       "placeholder:text-muted-foreground "
       "focus:outline-none focus:ring-2 focus:ring-primary focus:ring-offset-1 "
       "focus:ring-offset-background "
       "disabled:cursor-not-allowed disabled:opacity-50"))

(defn- input-control
  [attrs]
  [:input
   (merge
    {:class (control-class "input")}
    attrs)])

(defn- textarea-control
  [attrs value]
  (into
   [:textarea
    (merge
     {:class (str (control-class "textarea")
                  " min-h-28 resize-y")}
     attrs)]
   (when (some? value)
     [value])))

(defn- select-control
  [attrs & children]
  (into
   [:select
    (merge
     {:class (control-class "select")}
     attrs)]
   children))

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
    :schema account-schema
    :control (input-control
              (maybe-value
               {:type "text"
                :name "username"
                :autocomplete "username"}
               (:username values)))
    :description "Try admin, support, root, michael, or demo to see a server-side error."
    :error (:username errors)}))

(defn- email-field
  [values errors]
  (g/field
   {:label-text "Email"
    :for :email
    :schema account-schema
    :control (input-control
              (maybe-value
               {:type "email"
                :name "email"
                :autocomplete "email"}
               (:email values)))
    :description "Try taken@example.com or used@example.com to see a server-side error."
    :error (:email errors)}))

(defn- password-field
  [_values errors]
  (g/field
   {:label-text "Password"
    :for :password
    :schema account-schema
    :control (input-control
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
    :schema account-schema
    :control (input-control
              (maybe-value
               {:type "text"
                :name "displayName"
                :autocomplete "name"}
               (:displayName values)))
    :description "Optional."
    :error (:displayName errors)}))

(defn- source-select
  [values]
  [:div {:class "content-stack-theme"}
   [:label {:class "label-theme"
            :for "source"}
    "How did you hear about this?"]
   (select-control
    {:id "source"
     :name "source"}
    (option (:source values) "" "Choose one")
    (option (:source values) "friend" "Friend")
    (option (:source values) "search" "Search")
    (option (:source values) "other" "Other"))])

(defn- newsletter-checkbox
  [values]
  [:label {:class "inline-flex items-center gap-inline font-body text-sm-theme"}
   [:input (cond-> {:type "checkbox"
                    :name "newsletter"
                    :value "yes"}
             (= "yes" (:newsletter values))
             (assoc :checked true))]
   [:span "Send occasional updates"]])

(defn- account-primary-section
  [values errors]
  (section-card
   (section-heading
    "Step 1: Account"
    "Client-side rules run immediately. Server-side availability checks run after a valid field pauses or blurs.")
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
   :attrs {:novalidate true}})

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
    :schema store-schema
    :control (input-control
              (maybe-value
               {:type "text"
                :name "storeName"}
               (:storeName values)))
    :description "Try headquarters, corporate, or main office to see a server-side error."
    :error (:storeName errors)}))

(defn- store-type-field
  [values errors]
  (g/field
   {:label-text "Store type"
    :for :storeType
    :schema store-schema
    :control (select-control
              {:name "storeType"}
              (option (:storeType values) "" "Choose one")
              (option (:storeType values) "retail" "Retail")
              (option (:storeType values) "grocery" "Grocery")
              (option (:storeType values) "clinic" "Clinic")
              (option (:storeType values) "service" "Service"))
    :description "Required select control."
    :error (:storeType errors)}))

(defn- employee-count-field
  [values errors]
  (g/field
   {:label-text "Team size"
    :for :employeeCount
    :schema store-schema
    :control (input-control
              (maybe-value
               {:type "number"
                :name "employeeCount"}
               (:employeeCount values)))
    :description "1 to 500."
    :error (:employeeCount errors)}))

(defn- opening-date-field
  [values errors]
  (g/field
   {:label-text "Opening date"
    :for :openingDate
    :schema store-schema
    :control (input-control
              (maybe-value
               {:type "date"
                :name "openingDate"}
               (:openingDate values)))
    :description "Date picker control."
    :error (:openingDate errors)}))

(defn- timezone-field
  [values errors]
  (g/field
   {:label-text "Timezone"
    :for :timezone
    :schema store-schema
    :control (select-control
              {:name "timezone"}
              (option (:timezone values) "" "Choose one")
              (option (:timezone values) "America/Los_Angeles" "Pacific")
              (option (:timezone values) "America/Denver" "Mountain")
              (option (:timezone values) "America/Chicago" "Central")
              (option (:timezone values) "America/New_York" "Eastern"))
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
             (checked? preferred value)
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
    :schema preferences-schema
    :control (input-control
              (maybe-value
               {:type "tel"
                :name "phone"
                :autocomplete "tel"}
               (:phone values)))
    :description "Required only if preferred contact is Phone."
    :error (:phone errors)}))

(defn- alert-email-field
  [values errors]
  (g/field
   {:label-text "Alert email"
    :for :alertEmail
    :schema preferences-schema
    :control (input-control
              (maybe-value
               {:type "email"
                :name "alertEmail"
                :autocomplete "email"}
               (:alertEmail values)))
    :description "Optional. Try bounce@example.com for a server-side error."
    :error (:alertEmail errors)}))

(defn- send-alerts-checkbox
  [values]
  [:label {:class "inline-flex items-center gap-inline font-body text-sm-theme"}
   [:input (cond-> {:type "checkbox"
                    :name "sendAlerts"
                    :value "yes"}
             (= "yes" (:sendAlerts values))
             (assoc :checked true))]
   [:span "Send urgent alerts"]])

(defn- notes-field
  [values errors]
  (g/field
   {:label-text "Notes"
    :for :notes
    :schema preferences-schema
    :control (textarea-control
              {:name "notes"}
              (:notes values))
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
;; Flow shell
;; -----------------------------------------------------------------------------

(defn- active-form
  [ctx current values errors]
  (case current
    :account
    (account-form ctx values errors)

    :store
    (store-form ctx values errors)

    :preferences
    (preferences-form ctx values errors)

    :done
    [:div]

    (account-form ctx values errors)))

(defn flow-fragment
  ([ctx current]
   (flow-fragment ctx current {} {}))
  ([ctx current values errors]
   [:div {:id "complex-form-flow"
          :class "content-stack-theme"}
    (section-card
     (section-heading
      "Tabbed form flow"
      "Each tab is a separate form. The active tab advances only after the current form validates and submits.")
     (tab-bar current))
    (active-form ctx current values errors)]))

(defn done-fragment
  [_ctx]
  [:div {:id "complex-form-flow"
         :class "content-stack-theme"}
   (section-card
    (section-heading
     "Form submitted"
     "The final form submitted successfully. No data was persisted in this demo.")
    (tab-bar :done)
    [:div {:class "panel-theme radius-lg pad-card"
           :style {:border "var(--border-width, 1px) solid var(--border)"
                   :background "color-mix(in srgb, var(--card) 96%, var(--background))"
                   :color "var(--card-foreground)"
                   :box-shadow "0 18px 50px color-mix(in srgb, black 22%, transparent)"}}
     [:p {:class "font-body text-base-theme leading-body"}
      "Success. This page exercised chained forms, disabled tab navigation, local browser validation, server-side OOB validation, arbitrary nested content, and final submission."]]
    [:a {:href base-path
         :class "btn-outline"}
     "Start over"])])

;; -----------------------------------------------------------------------------
;; Bars page chrome
;; -----------------------------------------------------------------------------

(defn- demo-link-item
  [text href opts]
  (bars/menu-item
   (merge {:text text
           :href href}
          opts)))

(defn- demo-brand
  []
  [:a {:href base-path
       :class "cluster-theme items-center"
       :style {:color "var(--foreground)"
               :text-decoration "none"}}
   (g/icon "search" {:size :sm})
   [:span {:class "font-heading text-md-theme weight-semibold-theme"}
    "Gesso"]])

(defn- demo-menu
  []
  (bars/menu
   {:label "Demos"
    :icon "inbox"
    :home-region :center
    :priority 80
    :collapse-at :small
    :groups [(bars/menu-group
              {:heading "Demo pages"
               :items [(demo-link-item
                        "Bars demo"
                        "/app/pages/bars-demo"
                        {:icon "inbox"})
                       (demo-link-item
                        "Complex form flow"
                        base-path
                        {:icon "check"
                         :current? true})]})]}))

(defn- account-menu
  []
  (bars/menu
   {:label "Account"
    :icon "check"
    :home-region :rightmost
    :category :account
    :collapse-at :small
    :priority 20
    :groups [(bars/menu-group
              {:heading "Session"
               :items [(demo-link-item "Start over" base-path {:icon "check"})
                       (bars/menu-item {:text "Sign out"
                                        :icon "x"})]})]}))

(defn- sidebar-menu
  []
  (bars/menu
   {:label "Form flow"
    :home-region :sidebar
    :category :demo
    :priority 60
    :groups [(bars/menu-group
              {:heading "Current demo"
               :items [(demo-link-item
                        "Complex form flow"
                        base-path
                        {:icon "check"
                         :current? true})
                       (demo-link-item
                        "Bars demo"
                        "/app/pages/bars-demo"
                        {:icon "inbox"})]})]}))

(defn- demo-menus
  []
  [(demo-menu)
   (account-menu)
   (sidebar-menu)])

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

   (flow-fragment ctx :account)])

(defn page-view
  [ctx]
  [:div {:class "relative isolate min-h-screen overflow-hidden"}
   (g/background {:light bg/orb-grid-background-light
                  :dark bg/orb-grid-background-light})

   [:div {:class "relative z-10"}
    (bars/bars
     {:brand (demo-brand)
      :sidebar-collapse-at :medium
      :menus (demo-menus)}
     (page-content ctx))]])

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn page
  [ctx]
  (ui/page-shell ctx (page-view ctx)))

(defn account-page
  [ctx]
  (html-response (flow-fragment ctx :account)))

(defn store-page
  [ctx]
  (html-response (flow-fragment ctx :store)))

(defn preferences-page
  [ctx]
  (html-response (flow-fragment ctx :preferences)))

(defn validate-account
  [{:keys [params]}]
  (errors-response
   (server-account-errors
    (account-values params))))

(defn validate-store
  [{:keys [params]}]
  (errors-response
   (server-store-errors
    (store-values params))))

(defn validate-preferences
  [{:keys [params]}]
  (errors-response
   (server-preferences-errors
    (preferences-values params))))

(defn submit-account
  [{:keys [params] :as ctx}]
  (let [values (account-values params)
        errors (submit-account-errors values)]
    (html-response
     (if (seq errors)
       (flow-fragment ctx :account values errors)
       (flow-fragment ctx :store)))))

(defn submit-store
  [{:keys [params] :as ctx}]
  (let [values (store-values params)
        errors (submit-store-errors values)]
    (html-response
     (if (seq errors)
       (flow-fragment ctx :store values errors)
       (flow-fragment ctx :preferences)))))

(defn submit-preferences
  [{:keys [params] :as ctx}]
  (let [values (preferences-values params)
        errors (submit-preferences-errors values)]
    (html-response
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
