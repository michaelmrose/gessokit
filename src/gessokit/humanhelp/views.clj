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
   [clojure.string :as str]
   [gesso.core :as g]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.domain :as domain]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.ui :as ui]))

;; -----------------------------------------------------------------------------
;; DOM ids shared with live.clj
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

(defn user-email
  [user]
  (or (:user/email user)
      (:user/id user)
      "demo-user"))

(defn status-label
  [request]
  (domain/request-status-label request))

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

(defn muted
  [text]
  [:p {:class "font-body text-sm-theme leading-body"
       :style {:color "var(--muted-foreground)"}}
   text])

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
             :value value}]))

(defn view-state-hidden-inputs
  [{:keys [search selected-request-id visible-revision]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/selected-param selected-request-id)
   (hidden-input routes/visible-revision-param visible-revision)])

(defn oob-response
  [& nodes]
  (into [:div {:style {:display "contents"}}]
        (remove nil? nodes)))

(defn attr-style-control
  []
  {:border-style "solid"
   :border-color "var(--border)"
   :background "var(--background)"
   :color "var(--foreground)"})

(defn sr-only-label
  [for text]
  [:label {:for for
           :class "sr-only"}
   text])

;; -----------------------------------------------------------------------------
;; Form/action helpers
;; -----------------------------------------------------------------------------

(defn form-action
  [ctx {:keys [to text variant size view-state attrs]}]
  [:form
   (merge
    {:method "post"
     :hx-post to
     :hx-swap "none"
     :class "inline-flex"}
    attrs)
   (g/anti-forgery-input ctx)
   (view-state-hidden-inputs view-state)
   (g/button
    {:variant (or variant :default)
     :size (or size :sm)
     :text text
     :attrs {:type "submit"}})])

(defn action-button
  [ctx request user action view-state]
  (form-action
   ctx
   {:to (routes/action-url (:request/id request) action)
    :text (domain/action-label action)
    :variant (case action
               :done :primary
               :claim :primary
               :take-over :primary
               :cancel :outline
               :unclaim :outline
               :default)
    :view-state view-state}))

;; -----------------------------------------------------------------------------
;; App bar
;; -----------------------------------------------------------------------------

(defn brand
  []
  [:a {:href routes/base-path
       :class "cluster-theme items-center"
       :style {:color "var(--foreground)"
               :text-decoration "none"}}
   (g/icon "hand-heart" {:size :sm})
   [:span {:class "font-heading text-md-theme leading-heading tracking-heading weight-semibold-theme"}
    "Human Help"]])

(defn logout-form
  []
  [:form {:method "post"
          :action "/auth/signout"}
   [:button {:type "submit"
             :class "w-full text-left font-body text-sm-theme leading-body"
             :style {:color "var(--foreground)"
                     :background "transparent"
                     :border "0"
                     :padding "0"}}
    "Log out"]])

(defn user-menu
  [user]
  [:details {:class "relative"}
   [:summary {:class "inline-flex cursor-pointer list-none items-center gap-inline control-theme radius-md border-theme font-body text-sm-theme weight-medium-theme"
              :style {:border-style "solid"
                      :border-color "var(--border)"
                      :background "var(--card)"
                      :color "var(--card-foreground)"}}
    [:span {:class "truncate max-w-[18rem]"}
     (user-email user)]
    (g/icon "chevron-down" {:size :sm})]

   [:div {:class "absolute right-0 z-50 mt-2 min-w-56 radius-md border-theme pad-panel shadow-lg"
          :style {:border-style "solid"
                  :border-color "var(--border)"
                  :background "var(--popover, var(--card))"
                  :color "var(--popover-foreground, var(--card-foreground))"}}
    [:div {:class "content-stack-theme"}
     [:div {:class "font-body text-xs-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
      "Signed in as"]
     [:div {:class "font-body text-sm-theme leading-body weight-medium-theme break-all"}
      (user-email user)]
     [:div {:class "border-t border-theme"
            :style {:border-color "var(--border)"}}]
     (logout-form)]]])

(defn app-bar
  [ctx user]
  [:div {:data-bars-root true
         :data-bars-open "false"
         :data-bars-has-sidebar "false"
         :data-bars-sidebar-collapse-at "medium"
         :data-bars-has-hamburger-md "false"
         :data-bars-has-hamburger-sm "false"
         :class "min-w-0"}
   [:header {:data-bars-topbar true}
    [:div {:data-bars-brand true
           :class "min-w-0"}
     (brand)]

    [:nav {:data-bars-segment "leftmost"
           :class "min-w-0"}]

    [:nav {:data-bars-segment "center"
           :class "min-w-0"}]

    [:nav {:data-bars-segment "rightmost"
           :class "min-w-0"}
     [:div {:class "cluster-theme items-center justify-end"}
      (ui/theme-dialog ctx {:trigger-label? false})
      (user-menu user)]]]])

;; -----------------------------------------------------------------------------
;; Page shell bits
;; -----------------------------------------------------------------------------

(defn hero
  []
  [:div {:class "title-stack-theme text-center"}
   [:h1 {:class "font-heading leading-heading tracking-heading text-4xl-theme weight-bold-theme"}
    "Welcome to Human Help."]])

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(defn refresh-button-class
  [stale?]
  (str "inline-flex items-center justify-center gap-inline control-theme radius-md border-theme "
       "font-body text-sm-theme weight-medium-theme "
       (when stale? "shadow-lg")))

(defn refresh-button-style
  [stale?]
  (if stale?
    {:border-style "solid"
     :border-color "var(--primary)"
     :background "var(--primary)"
     :color "var(--primary-foreground)"}
    {:border-style "solid"
     :border-color "var(--border)"
     :background "var(--card)"
     :color "var(--card-foreground)"}))

(defn refresh-form
  [ctx view-state stale?]
  [:form {:method "post"
          :hx-post (routes/refresh-requests-url view-state)
          :hx-swap "none"
          :class "inline-flex"}
   (g/anti-forgery-input ctx)
   (view-state-hidden-inputs view-state)
   [:button {:type "submit"
             :class (refresh-button-class stale?)
             :style (refresh-button-style stale?)}
    "Refresh"]])

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
     [:div {:class "toolbar-theme justify-between"}
      [:div {:class "cluster-theme items-center"}
       [:div {:class "title-stack-theme gap-field"}
        [:h2 {:class "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
         "Requests"]
        [:div {:class "cluster-theme items-center"}
         (g/status-pill
          {:status (if (pos? (or open-count 0)) :active :muted)
           :dot? true
           :text "Open"})

         [:span {:class "font-body text-sm-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
          (str (or open-count 0) " open")]

         (when (pos? (or pending-open-count 0))
           (g/badge
            {:variant :secondary
             :text (str "+" pending-open-count " new")}))]]]

      [:div {:class "cluster-theme items-center justify-end"}
       (refresh-form ctx view-state stale?)

       [:button {:type "button"
                 :aria-label "Create request"
                 :class "btn-primary"
                 :onclick (str "document.getElementById('"
                               create-request-dialog-id
                               "').showModal()")}
        "+"]]]

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
            :hx-get (routes/search-requests-url view-state)
            :hx-target (str "#" request-list-dom-id)
            :hx-swap "outerHTML"
            :hx-trigger "keyup changed delay:250ms from:#humanhelp-search, search from:#humanhelp-search"
            :class "content-stack-theme"}
     (view-state-hidden-inputs view-state)
     (sr-only-label "humanhelp-search" "Search requests")

     [:div {:class "relative"}
      [:span {:class "absolute left-3 top-1/2 -translate-y-1/2"
              :style {:color "var(--muted-foreground)"}}
       (g/icon "search" {:size :sm})]

      [:input {:type "search"
               :id "humanhelp-search"
               :name routes/search-param
               :value (or (:search view-state) "")
               :placeholder "Search by person, request, area, or status"
               :class "control-theme radius-lg border-theme font-body text-base-theme w-full"
               :style (assoc (attr-style-control)
                             :padding-left "2.5rem")}]]]))

;; -----------------------------------------------------------------------------
;; Request cards
;; -----------------------------------------------------------------------------

(defn card-selected?
  [request view-state]
  (= (:request/id request)
     (:selected-request-id view-state)))

(defn request-card-style
  [selected?]
  {:border-style "solid"
   :border-color (if selected?
                   "var(--primary)"
                   "var(--border)")
   :background "var(--background)"
   :color "var(--foreground)"
   :box-shadow (when selected?
                 "0 0 0 3px color-mix(in srgb, var(--primary) 24%, transparent)")})

(defn request-meta
  [request]
  [:div {:class "cluster-theme items-center"}
   (request-status-pill request)

   [:span {:class "font-body text-xs-theme"
           :style {:color "var(--muted-foreground)"}}
    (:request/area request)]

   [:span {:class "font-body text-xs-theme"
           :style {:color "var(--muted-foreground)"}}
    "·"]

   [:span {:class "font-body text-xs-theme"
           :style {:color "var(--muted-foreground)"}}
    "waiting "
    (domain/waiting-label request)]])

(defn request-card-actions
  [ctx request user view-state]
  (let [actions (domain/available-actions request user)]
    (when (seq actions)
      (into
       [:div {:class "cluster-theme items-center justify-end"}]
       (map #(action-button ctx request user % view-state))
       actions))))

(defn request-card
  [ctx {:keys [request user view-state]}]
  (let [selected? (card-selected? request view-state)]
    [:article {:id (str "humanhelp-request-" (:request/id request))
               :data-humanhelp-request-card true
               :class "radius-xl border-theme pad-card content-stack-theme transition-all"
               :style (request-card-style selected?)}
     [:a {:href (if selected?
                  (routes/clear-selection-url view-state)
                  (routes/select-request-url (:request/id request) view-state))
          :hx-get (if selected?
                    (routes/clear-selection-url view-state)
                    (routes/select-request-url (:request/id request) view-state))
          :hx-target (str "#" request-list-dom-id)
          :hx-swap "outerHTML"
          :class "block content-stack-theme"
          :style {:color "inherit"
                  :text-decoration "none"}}
      [:div {:class "cluster-theme items-start justify-between"}
       [:div {:class "content-stack-theme gap-field"}
        [:h3 {:class "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
         (:request/title request)]
        (request-meta request)]
       (g/icon (if selected? "chevron-up" "chevron-down")
               {:size :sm})]

      [:div {:class "cluster-theme items-center"}
       [:span {:class "font-body text-sm-theme leading-body weight-medium-theme"}
        (:request/customer-name request)]

       (when-let [claimed-by (:request/claimed-by-email request)]
         [:span {:class "font-body text-xs-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
          "claimed by "
          claimed-by])]]

     (when selected?
       [:div {:class "content-stack-theme"}
        (when (domain/present? (:request/details request))
          [:p {:class "font-body text-sm-theme leading-body"}
           (:request/details request)])

        (request-card-actions ctx request user view-state)])]))

(defn empty-request-list
  [{:keys [view-state]}]
  (g/empty-state
   {:title (if (domain/present? (:search view-state))
             "No matching requests"
             "No requests yet")
    :description (if (domain/present? (:search view-state))
                   "Try fewer words or a different person, area, request, or status."
                   "Create a request with the plus button to start the demo.")
    :icon (g/empty-state-icon)}))

(defn request-list-fragment
  [{:keys [ctx user view-state requests latest-revision]}]
  [:div {:id request-list-dom-id
         :data-humanhelp-fragment "request-list"
         :data-latest-revision latest-revision
         :class "content-stack-theme"}
   (if (seq requests)
     [:div {:class "content-stack-theme"}
      (for [request requests]
        (request-card
         ctx
         {:request request
          :user user
          :view-state view-state}))]
     (empty-request-list {:view-state view-state}))])

;; -----------------------------------------------------------------------------
;; Create request dialog
;; -----------------------------------------------------------------------------

(defn field-error
  [errors k]
  (when-let [error (get errors k)]
    [:p {:class "font-body text-sm-theme leading-body"
         :style {:color "var(--destructive)"}}
     error]))

(defn create-request-dialog-content
  [ctx {:keys [user values errors]}]
  (let [values (or values {})
        errors (or errors {})]
    [:div {:id create-request-dialog-body-id
           :class "pad-card content-stack-theme"}
     [:div {:class "title-stack-theme"}
      [:h2 {:class "font-heading text-2xl-theme leading-heading tracking-heading weight-bold-theme"}
       "Create request"]

      [:p {:class "font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "Everyone can make and service requests in this Human Help analogue."]]

     [:form {:method "post"
             :hx-post (routes/create-request-url)
             :hx-swap "none"
             :class "form-theme"}
      (g/anti-forgery-input ctx)

      [:label {:class "content-stack-theme gap-field"}
       [:span {:class "font-heading text-sm-theme weight-semibold-theme"}
        "Your name"]
       [:input {:name "customer-name"
                :value (or (:customer-name values)
                           (user-email user))
                :class "control-theme radius-md border-theme font-body text-sm-theme"
                :style (attr-style-control)}]
       (field-error errors :customer-name)]

      [:label {:class "content-stack-theme gap-field"}
       [:span {:class "font-heading text-sm-theme weight-semibold-theme"}
        "Area"]
       [:input {:name "area"
                :value (or (:area values) "")
                :placeholder "Garden"
                :class "control-theme radius-md border-theme font-body text-sm-theme"
                :style (attr-style-control)}]
       (field-error errors :area)]

      [:label {:class "content-stack-theme gap-field"}
       [:span {:class "font-heading text-sm-theme weight-semibold-theme"}
        "Request"]
       [:input {:name "title"
                :value (or (:title values) "")
                :placeholder "Need help finding a rake"
                :class "control-theme radius-md border-theme font-body text-sm-theme"
                :style (attr-style-control)}]
       (field-error errors :title)]

      [:label {:class "content-stack-theme gap-field"}
       [:span {:class "font-heading text-sm-theme weight-semibold-theme"}
        "Details"]
       [:textarea {:name "details"
                   :rows 4
                   :placeholder "Add item, aisle, or context."
                   :class "control-theme radius-md border-theme font-body text-sm-theme"
                   :style (attr-style-control)}
        (or (:details values) "")]
       (field-error errors :details)]

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
  [:dialog (cond-> {:id create-request-dialog-id
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

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn page
  [ctx {:keys [user
               view-state
               request-toolbar-panel
               request-list-panel]}]
  (ui/page-shell
   ctx

   (client-plumbing/listener ctx)

   [:div {:class "relative isolate min-h-screen"
          :style {:background "var(--background)"
                  :color "var(--foreground)"}}
    (app-bar ctx user)

    [:main {:class "mx-auto w-full max-w-2xl px-4 py-8 sm:px-6 lg:px-8"}
     [:div {:class "content-stack-theme gap-section"}
      (hero)

      [:section {:class "radius-2xl border-theme pad-card content-stack-theme shadow-lg"
                 :style {:border-style "solid"
                         :border-color "var(--border)"
                         :background "var(--card)"
                         :color "var(--card-foreground)"}}
       (or request-toolbar-panel
           [:div {:id request-toolbar-dom-id}
            (muted "Request toolbar loading...")])

       (search-control {:view-state view-state})

       (or request-list-panel
           [:div {:id request-list-dom-id}
            (muted "Request list loading...")])]

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

(defn create-request-dialog-fragment
  [ctx {:keys [user values errors open?]}]
  (create-request-dialog
   ctx
   {:user user
    :values values
    :errors errors
    :open? open?}))

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
     :title "Request created"
     :description (if request
                    (str "Request #"
                         (:request/number request)
                         " is now on the board.")
                    "The request is now on the board.")})))

(defn refreshed-request-board-fragments
  [_ctx {:keys [toolbar request-list]}]
  (fragments-oob
   {:toolbar toolbar
    :request-list request-list}))

(defn request-lifecycle-result
  [_ctx {:keys [action request toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (when (and action request)
     (g/render-toast-oob
      {:variant :success
       :title (domain/action-label action)
       :description (domain/action-result-message action request)}))))

(defn request-action-error
  [_ctx {:keys [result]}]
  (g/render-toast-oob
   {:variant :danger
    :title "Request not updated"
    :description (or (get-in result [:error :message])
                     (:message result)
                     (:reason result)
                     "That request action could not be completed.")}))

(defn reset-demo-result
  [_ctx {:keys [toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :info
     :title "Demo reset"
     :description "The Human Help request board was reset."})))
