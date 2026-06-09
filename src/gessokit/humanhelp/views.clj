(ns gessokit.humanhelp.views
  "Human Help analogue UI.

   This namespace owns Hiccup rendering only.

   It intentionally does not know about:
   - atom state
   - XTDB
   - Gesso Live model compilation
   - Ring route tables

   Data comes in from app/live/store boundary namespaces."
  (:require
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.components.request-card.core :refer [request-card]]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.ui :as ui]))

;; -----------------------------------------------------------------------------
;; DOM ids shared with app/live
;; -----------------------------------------------------------------------------

(def request-toolbar-dom-id
  "humanhelp-request-toolbar-fragment")

(def request-list-dom-id
  "humanhelp-request-list-fragment")

(def create-request-dialog-id
  "humanhelp-create-request-dialog")

(def create-request-dialog-body-id
  "humanhelp-create-request-dialog-body")

(def board-state-form-id
  "humanhelp-board-state")

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn account-email
  [user]
  (:user/email user))

(defn muted
  [text]
  (g/muted-text
   {:as :p
    :class "text-sm-theme leading-body"
    :text text}))

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
             :value value}]))

(defn view-state-hidden-inputs
  "Render full view-state hidden inputs.

   Use this for action forms that do not have their own visible search input.
   Do not use this inside search-control, because that form already has the
   visible search input named q."
  [{:keys [search selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/selected-param selected-request-id)
   (hidden-input routes/visible-revision-param visible-revision)])

(defn board-state-hidden-inputs
  "Render board-state hidden inputs for the search form.

   Deliberately omits q/search because the visible search input is the source
   of truth for q. Rendering both a hidden q and visible q causes repeated
   params such as [\"\" \"test\"]."
  [{:keys [selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/selected-param selected-request-id)
   (hidden-input routes/visible-revision-param visible-revision)])

(defn oob-response
  [& nodes]
  (into [:div {:style {:display "contents"}}]
        (remove nil? nodes)))

(defn sr-only-label
  [for text]
  [:label {:for for
           :class "sr-only"}
   text])

;; -----------------------------------------------------------------------------
;; Page shell bits
;; -----------------------------------------------------------------------------

(defn hero
  []
  [:div {:class "title-stack-theme text-center"}
   (g/page-title
    {:text "Welcome to Human Help."
     :class "text-4xl-theme"})])

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(defn refresh-form
  [ctx view-state stale?]
  [:form {:method "post"
          ;; Keep view-state in the form body only. Do not also put it into the
          ;; action URL, or repeated params can leak into request handling.
          :hx-post (routes/refresh-requests-url)
          :hx-swap "none"
          :class "inline-flex"}
   (g/anti-forgery-input ctx)
   (view-state-hidden-inputs view-state)
   (g/button
    {:variant (if stale? :primary :outline)
     :text "Refresh"
     :attrs {:type "submit"}})])

(defn create-request-button
  []
  (g/button
   {:variant :primary
    :size :icon
    :text "+"
    :attrs {:type "button"
            :aria-label "Create request"
            :onclick (str "document.getElementById('"
                          create-request-dialog-id
                          "').showModal()")}}))

(defn request-toolbar-heading
  [{:keys [open-count pending-open-count]}]
  [:div {:class "content-stack-theme gap-field"}
   (g/section-title
    {:text "Requests"
     :class "text-lg-theme weight-semibold-theme"})

   [:div {:class "cluster-theme items-center"}
    (g/status-pill
     {:status (if (pos? (or open-count 0)) :active :muted)
      :dot? true
      :text "Open"})

    (g/muted-text
     {:as :span
      :class "text-sm-theme leading-body"
      :text (str (or open-count 0) " open")})

    (when (pos? (or pending-open-count 0))
      (g/badge
       {:variant :secondary
        :text (str "+" pending-open-count " new")}))]])

(defn request-toolbar-fragment
  [{:keys [ctx
           view-state
           open-count
           pending-open-count
           stale?
           latest-revision]}]
  (let [view-state (or view-state {})
        stale?     (boolean stale?)]
    [:div {:id request-toolbar-dom-id
           :data-humanhelp-fragment "request-toolbar"
           :data-latest-revision latest-revision
           :class "content-stack-theme"}
     (g/toolbar
      {}
      (g/toolbar-start
       {}
       (request-toolbar-heading
        {:open-count open-count
         :pending-open-count pending-open-count}))

      (g/toolbar-end
       {}
       (refresh-form ctx view-state stale?)
       (create-request-button)))

     (when stale?
       (muted "New request data is available. Refresh when you are ready."))]))

;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(defn search-control
  [{:keys [view-state]}]
  (let [view-state (or view-state {})]
    [:form {:id board-state-form-id
            :method "get"
            ;; Do not bake q/selected/visible-revision into hx-get here.
            ;; The current form controls are the source of truth.
            :hx-get (routes/search-requests-url)
            :hx-target (str "#" request-list-dom-id)
            :hx-swap "outerHTML"
            :hx-trigger "keyup changed delay:250ms from:#humanhelp-search, search from:#humanhelp-search"
            :class "content-stack-theme"}
     (board-state-hidden-inputs view-state)
     (sr-only-label "humanhelp-search" "Search requests")

     [:div {:class "relative"}
      [:span {:class "absolute left-3 top-1/2 -translate-y-1/2"
              :style {:color "var(--muted-foreground)"}}
       (g/icon "search" {:size :sm})]

      (g/input
       {:type "search"
        :id "humanhelp-search"
        :name routes/search-param
        :value (or (:search view-state) "")
        :placeholder "Search by person, request, area, or status"
        :class "text-base-theme w-full"
        :attrs {:style {:padding-left "2.5rem"}}})]]))

;; -----------------------------------------------------------------------------
;; Request list
;; -----------------------------------------------------------------------------

(defn empty-request-list
  [{:keys [view-state]}]
  (g/empty-state
   {:title (if (model/present? (:search view-state))
             "No matching requests"
             "No requests yet")
    :description (if (model/present? (:search view-state))
                   "Try fewer words or a different person, area, request, or status."
                   "Create a request with the plus button to start the demo.")
    :icon (g/empty-state-icon)}))

(defn request-accordion
  [{:keys [ctx user view-state requests]}]
  (apply
   g/accordion
   {:type :single
    :collapsible? true
    :default-value (:selected-request-id view-state)
    :class "shadow-none"
    :attrs {:data-humanhelp-request-accordion true}}
   (map
    (fn [request]
      (request-card
       ctx
       {:request request
        :user user
        :view-state view-state}))
    requests)))

(defn request-list-fragment
  [{:keys [ctx user view-state requests latest-revision]}]
  [:div {:id request-list-dom-id
         :data-humanhelp-fragment "request-list"
         :data-latest-revision latest-revision
         :class "content-stack-theme"}
   (if (seq requests)
     (request-accordion
      {:ctx ctx
       :user user
       :view-state view-state
       :requests requests})
     (empty-request-list {:view-state view-state}))])

;; -----------------------------------------------------------------------------
;; Create request dialog
;; -----------------------------------------------------------------------------

(defn create-field
  [{:keys [id label name value placeholder errors error-key]}]
  (g/field
   {:for id
    :label-text label
    :error (get errors error-key)
    :control
    (g/input
     {:id id
      :name name
      :value value
      :placeholder placeholder})}))

(defn create-textarea-field
  [{:keys [id label name value placeholder errors error-key]}]
  (g/field
   {:for id
    :label-text label
    :error (get errors error-key)
    :control
    (g/textarea
     {:id id
      :name name
      :rows 4
      :value value
      :placeholder placeholder})}))

(defn create-request-dialog-content
  [ctx {:keys [user values errors]}]
  (let [values (or values {})
        errors (or errors {})]
    [:div {:id create-request-dialog-body-id
           :class "pad-card content-stack-theme"}
     [:div {:class "title-stack-theme"}
      (g/section-title
       {:text "Create request"
        :class "text-2xl-theme weight-bold-theme"})

      (g/muted-text
       {:as :p
        :class "text-sm-theme leading-body"
        :text "Everyone can make and service requests in this Human Help analogue."})]

     [:form {:method "post"
             :hx-post (routes/create-request-url)
             :hx-swap "none"
             :class "form-theme"}
      (g/anti-forgery-input ctx)

      (create-field
       {:id "humanhelp-create-customer-name"
        :label "Your name"
        :name "customer-name"
        :value (or (:customer-name values)
                   (account-email user)
                   "")
        :errors errors
        :error-key :customer-name})

      (create-field
       {:id "humanhelp-create-area"
        :label "Area"
        :name "area"
        :value (or (:area values) "")
        :placeholder "Garden"
        :errors errors
        :error-key :area})

      (create-field
       {:id "humanhelp-create-title"
        :label "Request"
        :name "title"
        :value (or (:title values) "")
        :placeholder "Need help finding a rake"
        :errors errors
        :error-key :title})

      (create-textarea-field
       {:id "humanhelp-create-details"
        :label "Details"
        :name "details"
        :value (or (:details values) "")
        :placeholder "Add item, aisle, or context."
        :errors errors
        :error-key :details})

      [:div {:class "cluster-theme justify-end"}
       (g/button
        {:variant :outline
         :text "Cancel"
         :attrs {:type "button"
                 :onclick (str "document.getElementById('"
                               create-request-dialog-id
                               "').close()")}})
       (g/button
        {:variant :primary
         :text "Create"
         :attrs {:type "submit"}})]]]))

(defn create-request-dialog
  [ctx {:keys [open?] :as opts}]
  [:dialog
   (cond-> {:id create-request-dialog-id
            :class "radius-xl border-theme shadow-xl"
            :style {:border-style "solid"
                    :border-color "var(--border)"
                    :background "var(--card)"
                    :color "var(--card-foreground)"
                    :max-width "min(34rem, calc(100vw - 2rem))"
                    :width "100%"
                    :padding "0"}}
     open? (assoc :open true))
   (create-request-dialog-content ctx opts)])

(defn create-request-dialog-fragment
  [ctx {:keys [user values errors open?]}]
  (create-request-dialog
   ctx
   {:user user
    :values values
    :errors errors
    :open? open?}))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn board-card
  [{:keys [view-state request-toolbar-panel request-list-panel]}]
  (g/card
   {:class "shadow-lg"
    :attrs {:data-humanhelp-board true}}
   (g/card-content
    {:class "content-stack-theme"}
    (or request-toolbar-panel
        [:div {:id request-toolbar-dom-id}
         (muted "Request toolbar loading...")])

    (search-control {:view-state view-state})

    (or request-list-panel
        [:div {:id request-list-dom-id}
         (muted "Request list loading...")]))))

(defn page
  [ctx {:keys [user
               view-state
               request-toolbar-panel
               request-list-panel]}]
  (ui/page-shell
   ctx
   {:user user}

   (client-plumbing/listener ctx)

   [:div {:class "relative isolate min-h-screen"
          :style {:background "var(--background)"
                  :color "var(--foreground)"}}
    [:div {:class "mx-auto w-full max-w-2xl px-4 py-8 sm:px-6 lg:px-8"}
     [:div {:class "content-stack-theme gap-section"}
      (hero)

      (board-card
       {:view-state view-state
        :request-toolbar-panel request-toolbar-panel
        :request-list-panel request-list-panel})

      (create-request-dialog
       ctx
       {:user user
        :values {}
        :errors {}
        :open? false})]]]))

;; -----------------------------------------------------------------------------
;; OOB / action result views
;; -----------------------------------------------------------------------------

(defn replace-toolbar-oob
  [toolbar]
  (g/oob-outer-html request-toolbar-dom-id toolbar))

(defn replace-request-list-oob
  [request-list]
  (g/oob-outer-html request-list-dom-id request-list))

(defn replace-dialog-oob
  [dialog]
  (g/oob-outer-html create-request-dialog-id dialog))

(defn fragments-oob
  [{:keys [toolbar request-list]}]
  (oob-response
   (when toolbar
     (replace-toolbar-oob toolbar))
   (when request-list
     (replace-request-list-oob request-list))))

(defn create-request-validation-error
  [ctx {:keys [user values errors]}]
  (replace-dialog-oob
   (create-request-dialog
    ctx
    {:user user
     :values values
     :errors errors
     :open? true})))

(defn create-request-success
  [ctx {:keys [user request toolbar request-list]}]
  (oob-response
   (replace-dialog-oob
    (create-request-dialog
     ctx
     {:user user
      :values {}
      :errors {}
      :open? false}))
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :success
     :duration 5000
     :title "Request created"
     :description (if request
                    (str "Request #"
                         (:request/number request)
                         " is now on the board.")
                    "The request is now on the board.")})))

(defn refreshed-request-board-fragments
  [{:keys [toolbar request-list]}]
  (fragments-oob
   {:toolbar toolbar
    :request-list request-list}))

(defn request-lifecycle-result
  [{:keys [action request toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (when (and action request)
     (g/render-toast-oob
      {:variant :success
       :duration 2500
       :title (model/action-label action)
       :description (model/action-result-message action request)}))))

(defn request-action-error
  [{:keys [result]}]
  (g/render-toast-oob
   {:variant :danger
    :duration 7000
    :title "Request not updated"
    :description (or (get-in result [:error :message])
                     (:message result)
                     (:reason result)
                     "That request action could not be completed.")}))

(defn reset-demo-result
  [{:keys [toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :info
     :duration 5000
     :title "Demo reset"
     :description "The Human Help request board was reset."})))
