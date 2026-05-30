(ns gessokit.form-flow-demo.validation
  (:require
   [clojure.string :as str]
   [gesso.util :as util]))

;; -----------------------------------------------------------------------------
;; Demo data
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

;; -----------------------------------------------------------------------------
;; Validation helpers
;; -----------------------------------------------------------------------------

(defn- username-starts-with-number?
  [s]
  (boolean
   (and (util/present-value? s)
        (re-matches #"^[0-9].*" s))))

(defn- valid-username-chars?
  [s]
  (boolean
   (and (util/present-value? s)
        (re-matches #"^[a-z0-9_-]+$" s))))

(defn- valid-username-shape?
  [s]
  (boolean
   (and (util/present-value? s)
        (<= 3 (count s) 24)
        (valid-username-chars? s)
        (not (username-starts-with-number? s)))))

(defn- valid-email-shape?
  [s]
  (boolean
   (and (util/present-value? s)
        (<= 5 (count s) 120)
        (re-matches #".+@.+" s))))

(defn- valid-phone-shape?
  [s]
  (or (not (util/present-value? s))
      (boolean (re-matches #"^[0-9+(). -]{7,32}$" s))))

(defn- date-like?
  [s]
  (boolean
   (and (util/present-value? s)
        (re-matches #"\d{4}-\d{2}-\d{2}" s))))

;; -----------------------------------------------------------------------------
;; Submitted values
;; -----------------------------------------------------------------------------

(defn account-values
  [params]
  {:username    (util/trim-value (util/request-param params :username))
   :email       (util/trim-value (util/request-param params :email))
   :password    (util/request-param params :password)
   :displayName (util/trim-value (util/request-param params :displayName))
   :source      (util/request-param params :source)
   :newsletter  (util/request-param params :newsletter)})

(defn store-values
  [params]
  {:storeName     (util/trim-value (util/request-param params :storeName))
   :storeType     (util/request-param params :storeType)
   :employeeCount (util/parse-int-value (util/request-param params :employeeCount))
   :openingDate   (util/request-param params :openingDate)
   :timezone      (util/request-param params :timezone)})

(defn preferences-values
  [params]
  {:preferredContact (or (util/request-param params :preferredContact) "email")
   :phone            (util/trim-value (util/request-param params :phone))
   :alertEmail       (util/trim-value (util/request-param params :alertEmail))
   :sendAlerts       (util/request-param params :sendAlerts)
   :notes            (util/request-param params :notes)})

;; -----------------------------------------------------------------------------
;; Server-side demo errors
;; -----------------------------------------------------------------------------

(defn server-account-errors
  [{:keys [username email]}]
  (merge
   (when (username-starts-with-number? username)
     {:username "Username cannot start with a number."})

   (cond-> {}
     (and (valid-username-shape? username)
          (contains? taken-usernames (str/lower-case username)))
     (assoc :username "That username is already taken.")

     (and (valid-email-shape? email)
          (contains? taken-emails (str/lower-case email)))
     (assoc :email "That email is already in use."))))

(defn submit-account-errors
  [{:keys [username email password displayName] :as values}]
  (merge
   (cond
     (not (util/present-value? username))
     {:username "Username is required."}

     (< (count username) 3)
     {:username "Username must be at least 3 characters."}

     (> (count username) 24)
     {:username "Username must be at most 24 characters."}

     (username-starts-with-number? username)
     {:username "Username cannot start with a number."}

     (not (valid-username-chars? username))
     {:username "Use lowercase letters, numbers, underscores, or hyphens."}

     :else
     {})

   (cond
     (not (util/present-value? email))
     {:email "Email is required."}

     (not (valid-email-shape? email))
     {:email "Enter an email address."}

     :else
     {})

   (cond
     (not (util/present-value? password))
     {:password "Password is required."}

     (< (count (str password)) 8)
     {:password "Password must be at least 8 characters."}

     :else
     {})

   (when (and (util/present-value? displayName)
              (> (count displayName) 80))
     {:displayName "Display name must be at most 80 characters."})

   (server-account-errors values)))

(defn server-store-errors
  [{:keys [storeName]}]
  (cond-> {}
    (and (util/present-value? storeName)
         (contains? reserved-store-names (str/lower-case storeName)))
    (assoc :storeName "That store name is reserved for this demo.")))

(defn submit-store-errors
  [{:keys [storeName storeType employeeCount openingDate timezone] :as values}]
  (merge
   (cond
     (not (util/present-value? storeName))
     {:storeName "Store name is required."}

     (< (count storeName) 2)
     {:storeName "Store name must be at least 2 characters."}

     :else
     {})

   (when-not (util/present-value? storeType)
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

   (when-not (util/present-value? timezone)
     {:timezone "Choose a timezone."})

   (server-store-errors values)))

(defn server-preferences-errors
  [{:keys [alertEmail]}]
  (cond-> {}
    (and (valid-email-shape? alertEmail)
         (contains? bounced-alert-emails (str/lower-case alertEmail)))
    (assoc :alertEmail "That alert email is blocked in this demo.")))

(defn submit-preferences-errors
  [{:keys [preferredContact phone alertEmail notes] :as values}]
  (merge
   (when (and (= preferredContact "phone")
              (not (util/present-value? phone)))
     {:phone "Enter a phone number or choose another contact method."})

   (when-not (valid-phone-shape? phone)
     {:phone "Use digits, spaces, dashes, parentheses, or +."})

   (when (and (util/present-value? alertEmail)
              (not (valid-email-shape? alertEmail)))
     {:alertEmail "Enter an email address."})

   (when (and (util/present-value? notes)
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
               :gesso.html/pattern "^[a-z0-9_\\-]+$"
               :gesso.error/min "Username must be at least 3 characters."
               :gesso.error/max "Username must be at most 24 characters."
               :gesso.error/pattern "Use lowercase letters, numbers, underscores, or hyphens."
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
                :gesso.html/pattern "^[0-9+\\(\\). \\-]{7,32}$"
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
