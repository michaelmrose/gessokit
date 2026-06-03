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
   [gessokit.ui :as ui])
  (:import
   [java.net URLEncoder]))

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
;; Forward declarations
;; -----------------------------------------------------------------------------

(declare search-control)
(declare create-request-dialog)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn user-email
  [user]
  (or (:user/email user)
      (:user/id user)
      "demo-user"))

(defn user-display-name
  [user]
  (let [email (user-email user)]
    (if (str/includes? email "@")
      (first (str/split email #"@"))
      email)))

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

(defn route
  [path]
  (str routes/base-path path))

(defn encode
  [x]
  (URLEncoder/encode (str x) "UTF-8"))

(defn q
  [params]
  (let [pairs (for [[k v] params
                    :when (some? v)
                    :let [s (str v)]
                    :when (not (str/blank? s))]
                (str (encode (name k))
                     "="
                     (encode s)))]
    (when (seq pairs)
      (str "?" (str/join "&" pairs)))))

(defn with-query
  [path params]
  (str path (or (q params) "")))

(defn visible-revision
  [view-state]
  (:visible-revision view-state))

(defn selected-request-id
  [view-state]
  (:selected-request-id view-state))

(defn search-value
  [view-state]
  (or (:search view-state) ""))

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
;; Brand / top bar
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

(defn account-dropdown
  [user]
  (g/dropdown-menu
   {}
   (g/dropdown-menu-trigger
    {:class "inline-flex max-w-[22rem] items-center justify-center gap-inline control-theme radius-md border-theme font-body text-sm-theme weight-medium-theme"
     :attrs {:aria-label "Account menu"}}
    [:span {:class "truncate"}
     (user-email user)]
    [:span {:aria-hidden "true"
            :style {:color "var(--muted-foreground)"}}
     "▾"])

   (g/dropdown-menu-content
    {:align :end}
    (g/dropdown-menu-label
     {:text (user-email user)})
    (g/dropdown-menu-separator)
    (g/dropdown-menu-item
     {:attrs {:onclick "window.location.href='/auth/signout'"}}
     [:span "Log out"]))))

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
      (account-dropdown user)]]]])

(defn hero
  []
  [:div {:class "title-stack-theme text-center"}
   [:h1 {:class "font-heading leading-heading tracking-heading text-4xl-theme weight-bold-theme"}
    "Welcome to Human Help."]])

;; -----------------------------------------------------------------------------
;; Request board state / search
;; -----------------------------------------------------------------------------

(defn search-url
  [view-state]
  (with-query
    (route routes/search-requests-route)
    {:visible-revision (visible-revision view-state)}))

(defn search-control
  [{:keys [view-state]}]
  [:form {:id board-state-form-id
          :method "get"
          :hx-get (search-url view-state)
          :hx-target (str "#" request-list-dom-id)
          :hx-swap "outerHTML"
          :hx-trigger "keyup changed delay:250ms from:#humanhelp-search, search from:#humanhelp-search"
          :class "content-stack-theme"}
   (hidden-input "visible-revision" (visible-revision view-state))
   (sr-only-label "humanhelp-search" "Search requests")

   [:div {:class "relative"}
    [:span {:class "absolute left-3 top-1/2 -translate-y-1/2"
            :style {:color "var(--muted-foreground)"}}
     (g/icon "search" {:size :sm})]

    [:input {:type "search"
             :id "humanhelp-search"
             :name "q"
             :value (search-value view-state)
             :placeholder "Search by person, request, area, or status"
             :class "control-theme radius-lg border-theme font-body text-base-theme w-full"
             :style (assoc (attr-style-control)
                           :padding-left "2.5rem")}]]])

;; -----------------------------------------------------------------------------
;; Request toolbar fragment
;; -----------------------------------------------------------------------------

(defn request-toolbar-fragment
  [{:keys [view-state latest-revision visible-revision stale? open-count pending-open-count]}]
  [:div {:id request-toolbar-dom-id
         :class "content-stack-theme"}
   [:div {:class "toolbar-theme justify-between"}
    [:div {:class "cluster-theme items-center"}
     (g/status-pill
      {:status (if (pos? open-count) :active :muted)
       :dot? true
       :text (str open-count " open")})

     (when (pos? pending-open-count)
       (g/badge
        {:variant :secondary
         :text (str pending-open-count " new")}))

     (when stale?
       (g/badge
        {:variant :outline
         :text "New data available"}))]

    [:div {:class "cluster-theme items-center justify-end"}
     (when stale?
       [:form {:method "post"
               :hx-post (route routes/refresh-requests-route)
               :hx-target "body"
               :hx-swap "beforeend"
               :class "inline-flex"}
        (hidden-input "visible-revision" latest-revision)
        (hidden-input "q" (search-value view-state))
        (g/button
         {:variant :primary
          :size :sm
          :text "Refresh"
          :attrs {:type "submit"}})])

     [:button {:type "button"
               :class "btn-primary"
               :onclick (str "document.getElementById('"
                             create-request-dialog-id
                             "').showModal()")}
      "Create request"]]]

   (when stale?
     (muted
      (str "Your board is pinned at revision "
           visible-revision
           ". Refresh to include newer requests.")))])

;; -----------------------------------------------------------------------------
;; Request cards / actions
;; -----------------------------------------------------------------------------

(defn action-url
  [request action]
  (let [id (:request/id request)]
    (case action
      :claim
      (route (str "/requests/" id "/claim"))

      :unclaim
      (route (str "/requests/" id "/unclaim"))

      :take-over
      (route (str "/requests/" id "/take-over"))

      :done
      (route (str "/requests/" id "/done"))

      :cancel
      (route (str "/requests/" id "/cancel"))

      (route (str "/requests/" id "/" (name action))))))

(defn select-url
  [request view-state]
  (with-query
    (route (str "/requests/" (:request/id request) "/select"))
    {:visible-revision (visible-revision view-state)
     :q (search-value view-state)
     :selected (:request/id request)}))

(defn action-variant
  [action]
  (case action
    :claim :primary
    :take-over :primary
    :done :primary
    :unclaim :outline
    :cancel :outline
    :outline))

(defn action-button
  [ctx request action view-state]
  [:form {:method "post"
          :hx-post (action-url request action)
          :hx-target "body"
          :hx-swap "beforeend"
          :hx-sync "closest [data-humanhelp-request-card]:drop"
          :class "inline-flex"}
   (g/anti-forgery-input ctx)
   (hidden-input "visible-revision" (visible-revision view-state))
   (hidden-input "q" (search-value view-state))
   (hidden-input "selected" (selected-request-id view-state))

   (g/button
    {:variant (action-variant action)
     :size :sm
     :text (domain/action-label action)
     :attrs {:type "submit"}})])

(defn request-actions
  [ctx request user view-state]
  (let [actions (domain/available-actions request user)]
    (if (seq actions)
      (into
       [:div {:class "cluster-theme items-center justify-end"}]
       (map #(action-button ctx request % view-state))
       actions)

      [:div {:class "cluster-theme items-center justify-end"}
       (g/badge
        {:variant :outline
         :text "No actions"})])))

(defn request-metadata
  [request]
  (str "Request #"
       (:request/number request)
       " · "
       (:request/customer-name request)
       " · "
       (:request/area request)
       " · "
       (domain/waiting-label request)
       " ago"))

(defn request-card
  [{:keys [ctx user view-state selected? request]}]
  [:article {:id (str "request-" (:request/id request))
             :data-humanhelp-request-card true
             :class "radius-xl border-theme pad-panel content-stack-theme shadow-sm"
             :style {:border-style "solid"
                     :border-color (if selected?
                                     "var(--ring)"
                                     "var(--border)")
                     :background "var(--card)"
                     :color "var(--card-foreground)"}}
   [:div {:class "toolbar-theme justify-between"}
    [:div {:class "title-stack-theme"}
     [:div {:class "cluster-theme items-center"}
      (request-status-pill request)

      (when-let [claimed-by (:request/claimed-by-email request)]
        (g/badge
         {:variant :outline
          :text (str "Claimed by " claimed-by)}))]

     [:h2 {:class "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
      (:request/title request)]

     [:p {:class "font-body text-sm-theme leading-body"
          :style {:color "var(--muted-foreground)"}}
      (request-metadata request)]]

    [:a {:href (select-url request view-state)
         :hx-get (select-url request view-state)
         :hx-target (str "#" request-list-dom-id)
         :hx-swap "outerHTML"
         :class "font-body text-sm-theme"
         :style {:color "var(--primary)"
                 :text-decoration "none"}}
     (if selected? "Collapse" "Details")]]

   (when (or selected?
             (seq (:request/details request)))
     [:div {:class "content-stack-theme"}
      (when (seq (:request/details request))
        [:p {:class "font-body text-sm-theme leading-body"}
         (:request/details request)])

      (when selected?
        [:div {:class "radius-md pad-row"
               :style {:background "var(--muted)"}}
         [:p {:class "font-body text-sm-theme leading-body"
              :style {:color "var(--muted-foreground)"}}
          "This expanded state is driven by normal request params and fragment rendering."]])])

   (request-actions ctx request user view-state)])

(defn request-list-empty
  [{:keys [view-state]}]
  (let [search (search-value view-state)]
    (g/empty-state
     {:icon (g/empty-state-icon)
      :title (if (str/blank? search)
               "No requests yet"
               "No matching requests")
      :description (if (str/blank? search)
                     "Create a request to see the live board update."
                     "Try a broader search, or clear the search field.")})))

(defn request-list-fragment
  [{:keys [ctx user view-state requests stale?] :as data}]
  [:div {:id request-list-dom-id
         :class "content-stack-theme"}
   (when stale?
     [:div {:class "radius-lg border-theme pad-panel"
            :style {:border-style "solid"
                    :border-color "var(--border)"
                    :background "var(--muted)"}}
      (muted "New requests are available. Use Refresh to commit them into this visible board.")])

   (if (seq requests)
     [:div {:class "list-theme"}
      (for [request requests]
        (request-card
         {:ctx ctx
          :user user
          :view-state view-state
          :request request
          :selected? (= (:request/id request)
                        (selected-request-id view-state))}))]
     (request-list-empty data))])

;; -----------------------------------------------------------------------------
;; Create request dialog
;; -----------------------------------------------------------------------------

(defn field-error
  [errors k]
  (when-let [error (get errors k)]
    [:p {:class "font-body text-sm-theme leading-body"
         :style {:color "var(--destructive)"}}
     error]))

(defn input-label
  [text]
  [:span {:class "font-heading text-sm-theme weight-semibold-theme"}
   text])

(defn text-input
  [{:keys [name value placeholder type]}]
  [:input {:name name
           :type (or type "text")
           :value (or value "")
           :placeholder placeholder
           :class "control-theme radius-md border-theme font-body text-sm-theme"
           :style (attr-style-control)}])

(defn create-request-form
  [ctx {:keys [user values errors]}]
  (let [values (or values {})
        errors (or errors {})]
    [:form {:method "post"
            :hx-post (route routes/create-request-route)
            :hx-swap "none"
            :class "form-theme"}
     (g/anti-forgery-input ctx)

     [:label {:class "content-stack-theme gap-field"}
      (input-label "Your name")
      (text-input
       {:name "customer-name"
        :value (or (:customer-name values)
                   (user-display-name user))
        :placeholder "Avery"})
      (field-error errors :customer-name)]

     [:label {:class "content-stack-theme gap-field"}
      (input-label "Area")
      (text-input
       {:name "area"
        :value (:area values)
        :placeholder "Garden"})
      (field-error errors :area)]

     [:label {:class "content-stack-theme gap-field"}
      (input-label "Request")
      (text-input
       {:name "title"
        :value (:title values)
        :placeholder "Need help finding a rake"})
      (field-error errors :title)]

     [:label {:class "content-stack-theme gap-field"}
      (input-label "Details")
      [:textarea {:name "details"
                  :rows 4
                  :placeholder "Add item, aisle, or context."
                  :class "control-theme radius-md border-theme font-body text-sm-theme"
                  :style (attr-style-control)}
       (or (:details values) "")]
      (field-error errors :details)]

     [:div {:class "cluster-theme justify-end"}
      [:button {:type "button"
                :onclick (str "document.getElementById('"
                              create-request-dialog-id
                              "').close()")
                :class "btn-outline"}
       "Cancel"]

      [:button {:type "submit"
                :class "btn-primary"}
       "Create"]]]))

(defn create-request-dialog-body
  [ctx {:keys [user values errors open?]}]
  [:div {:id create-request-dialog-body-id
         :class "pad-card content-stack-theme"}
   [:div {:class "title-stack-theme"}
    [:h2 {:class "font-heading text-2xl-theme leading-heading tracking-heading weight-bold-theme"}
     "Create request"]

    [:p {:class "font-body text-sm-theme leading-body"
         :style {:color "var(--muted-foreground)"}}
     "Everyone can make and service requests in this Human Help analogue."]]

   (when (seq errors)
     (g/alert
      {:variant :destructive
       :title "Check the request"
       :content "Fix the highlighted fields and try again."}))

   (create-request-form
    ctx
    {:user user
     :values values
     :errors errors
     :open? open?})])

(defn create-request-dialog
  [ctx {:keys [user values errors open?]}]
  [:dialog {:id create-request-dialog-id
            :class "radius-xl border-theme shadow-xl"
            :open (when open? true)
            :style {:border-style "solid"
                    :border-color "var(--border)"
                    :background "var(--card)"
                    :color "var(--card-foreground)"
                    :max-width "min(34rem, calc(100vw - 2rem))"
                    :width "100%"
                    :padding "0"}}
   (create-request-dialog-body
    ctx
    {:user user
     :values (or values {})
     :errors (or errors {})
     :open? open?})])

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn page
  "Render /app."
  [ctx {:keys [user view-state request-toolbar-panel request-list-panel]}]
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
;; OOB helpers
;; -----------------------------------------------------------------------------

(defn close-dialog-oob
  []
  [:div {:hx-swap-oob "beforeend:body"}
   [:script
    (str "document.getElementById('"
         create-request-dialog-id
         "')?.close();")]])

(defn replace-dialog-body-oob
  [body]
  (g/oob-outer-html create-request-dialog-body-id body))

(defn replace-toolbar-oob
  [toolbar]
  (g/oob-outer-html request-toolbar-dom-id toolbar))

(defn replace-request-list-oob
  [request-list]
  (g/oob-outer-html request-list-dom-id request-list))

(defn fragments-oob
  [{:keys [toolbar request-list]}]
  [:<>
   (when toolbar
     (replace-toolbar-oob toolbar))
   (when request-list
     (replace-request-list-oob request-list))])

;; -----------------------------------------------------------------------------
;; Create request responses
;; -----------------------------------------------------------------------------

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
  (replace-dialog-body-oob
   (create-request-dialog-body
    ctx
    {:user user
     :values values
     :errors errors
     :open? true})))

(defn create-request-success
  [_ctx {:keys [request toolbar request-list]}]
  [:<>
   (close-dialog-oob)
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :success
     :title "Request created"
     :description (str "Request #"
                       (:request/number request)
                       " is now on the board.")})])

;; -----------------------------------------------------------------------------
;; Request-board interaction responses
;; -----------------------------------------------------------------------------

(defn refreshed-request-board-fragments
  [_ctx {:keys [toolbar request-list]}]
  (fragments-oob
   {:toolbar toolbar
    :request-list request-list}))

(defn request-lifecycle-result
  [_ctx {:keys [action request toolbar request-list]}]
  [:<>
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :success
     :title (domain/action-label action)
     :description (domain/action-result-message action request)})])

(defn request-action-error
  [_ctx {:keys [action result request-id]}]
  (g/render-toast-oob
   {:variant :danger
    :title (str "Could not " (str/lower-case (domain/action-label action)))
    :description (or (:message result)
                     (:reason result)
                     (str "Request " request-id " could not be updated."))}))

(defn reset-demo-result
  [_ctx {:keys [toolbar request-list]}]
  [:<>
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :info
     :title "Demo reset"
     :description "The Human Help demo state was reset."})])
