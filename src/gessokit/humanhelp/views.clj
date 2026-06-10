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
   [gessokit.humanhelp.components.refresh-button.core :refer [refresh-button]]
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
;; Stable board-state selectors
;; -----------------------------------------------------------------------------

(defn board-state-selector
  []
  (str "#" board-state-form-id))

(defn selected-state-input-selector
  []
  (str (board-state-selector)
       " input[name="
       routes/selected-param
       "]"))

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

   Use this for forms that do not have their own visible search input.
   Do not use this inside search-control, because that form already has the
   visible search input named q."
  [{:keys [search selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/selected-param selected-request-id)
   (hidden-input routes/visible-revision-param visible-revision)])

(defn board-state-hidden-inputs
  "Render board-state hidden inputs for the search/board-state form.

   Deliberately omits q/search because the visible search input is the source
   of truth for q. Rendering both a hidden q and visible q causes repeated
   params such as [\"\" \"test\"].

   The selected input is always rendered, even when blank, because the generic
   Gesso accordion state-sync script needs a stable input to write into."
  [{:keys [selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/selected-param (or selected-request-id ""))
   (hidden-input routes/visible-revision-param visible-revision)])

(defn oob-response
  [& nodes]
  (into [:div {:style {:display "contents"}}]
        (remove nil? nodes)))

;; -----------------------------------------------------------------------------
;; Page shell bits
;; -----------------------------------------------------------------------------

(defn hero
  []
  [:div {:class "title-stack-theme text-center"}
   (g/page-title
    {:text "Human Help Fast."
     :class "text-3xl-theme py-6"})])

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

(defn create-request-form
  [ctx {:keys [user values errors]}]
  (let [values (or values {})
        errors (or errors {})]
    (g/form
     ctx
     {:post (routes/create-request-url)
      :swap "none"
      :attrs {:hx-include (board-state-selector)}}
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
       :rows 4
       :value (or (:details values) "")
       :placeholder "Add item, aisle, or context."
       :errors errors
       :error-key :details})

     (g/group
      {:align :end}
      (g/dialog-close
       {:text "Cancel"})
      (g/button
       {:variant :primary
        :text "Create"
        :attrs {:type "submit"}})))))

(defn create-request-dialog-body
  [ctx opts]
  [:div {:id create-request-dialog-body-id}
   (create-request-form ctx opts)])

(defn create-request-button
  []
  (g/dialog-trigger
   {:class "btn-icon-primary"
    :attrs {:aria-label "Create request"}}
   "+"))

(defn create-request-dialog
  [ctx {:keys [open?] :as opts}]
  (g/dialog
   {:open? open?
    :attrs {:id create-request-dialog-id}}
   (create-request-button)
   (g/dialog-overlay)
   (g/dialog-content
    {:title "Create request"
     :description "Everyone can make and service requests in this Human Help analogue."
     :body [(create-request-dialog-body ctx opts)]})))

(defn create-request-dialog-fragment
  [ctx {:keys [user values errors open?]}]
  (create-request-dialog
   ctx
   {:user user
    :values values
    :errors errors
    :open? open?}))

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(defn refresh-form
  [ctx view-state stale?]
  (g/form
    ctx
    {:post (routes/refresh-requests-url)
     :swap "none"
     :inline? true
     :attrs {:hx-include (board-state-selector)}}
    (refresh-button {:stale? stale?})))

(defn request-toolbar-heading
  [{:keys [open-count pending-open-count]}]
  [:div {:class "content-stack-theme gap-field"}
   (g/section-title
    {:text "Requests"
     :class "text-lg-theme weight-semibold-theme"})

   (g/group
    {}
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
        :text (str "+" pending-open-count " new")})))])


(defn request-toolbar-fragment
  [{:keys [ctx
           user
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
      {:start [(request-toolbar-heading
                {:open-count open-count
                 :pending-open-count pending-open-count})]
       :end [(g/group
              {:orientation :vertical
               :wrap? false
               :class "gap-field"
               :attrs {:style {:align-items "flex-end"}}}
              (refresh-form ctx view-state stale?)
              (create-request-dialog
               ctx
               {:user user
                :values {}
                :errors {}
                :open? false}))]})

     (when stale?
       (muted "New request data is available. Refresh when you are ready."))]))


;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(defn search-control
  ([opts]
   (search-control nil opts))
  ([ctx {:keys [view-state]}]
   (let [view-state (or view-state {})]
     (g/form
      ctx
      {:get (routes/search-requests-url)
       :target (str "#" request-list-dom-id)
       :swap "outerHTML"
       :trigger "keyup changed delay:250ms from:#humanhelp-search, search from:#humanhelp-search"
       :class "content-stack-theme"
       :attrs {:id board-state-form-id}}
      (board-state-hidden-inputs view-state)
      (g/field
       {:for "humanhelp-search"
        :label-text "Search requests"
        :control
        (g/input
         {:type "search"
          :id "humanhelp-search"
          :name routes/search-param
          :value (or (:search view-state) "")
          :placeholder "Search by person, request, area, or status"})})))))

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
    :state-input (selected-state-input-selector)
    :state-name routes/selected-param
    :state-include? true
    :class "content-stack-theme shadow-none"
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
;; Page
;; -----------------------------------------------------------------------------

(defn board-card
  [ctx {:keys [view-state request-toolbar-panel request-list-panel]}]
  (g/card
   {:class "shadow-lg"
    :attrs {:data-humanhelp-board true}}
   (g/card-content
    {:class "content-stack-theme"}
    (or request-toolbar-panel
        [:div {:id request-toolbar-dom-id}
         (muted "Request toolbar loading...")])

    (search-control ctx {:view-state view-state})

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

   (client-plumbing/listener
     ctx
     {:trigger-attrs {:hx-include (board-state-selector)}})

   (ui/container
    [:div {:class "content-stack-theme gap-section"}
     (hero)

     (board-card
      ctx
      {:view-state view-state
       :request-toolbar-panel request-toolbar-panel
       :request-list-panel request-list-panel})])))

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
