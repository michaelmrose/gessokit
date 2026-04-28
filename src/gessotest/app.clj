(ns gessotest.app

  (:require
   [cheshire.core :as cheshire]
   [com.biffweb :as biff]
   [com.biffweb.experimental :as biffx]
   [gessotest.middleware :as mid]
   [gessotest.settings :as settings]
   [gessotest.ui :as ui]
   [ring.websocket :as ws]
   [rum.core :as rum]
   [tick.core :as tick]
   [gesso.core :as gs :refer :all ]
   [gessotest.page-examples :refer :all]
   [gessotest.bars-demo :refer [bars-demo-page]]
   [gesso.live.app :as live.app]
   [gessotest.shared-counter :as shared-counter]
   [gessotest.simple-shared-counter :as simple-shared-counter]
   [gessotest.form-flow-demo.core :as form-flow-demo]
   [gessotest.toaster-demo :as toaster-demo]
   ))

(defn- section-heading
  [title description]
  [:div {:class "text-center space-y-2"}
   [:h2 {:class "font-heading leading-heading tracking-heading text-2xl font-semibold"}
    title]
   [:p {:class "font-body leading-body text-muted-foreground"} description]])

(defn- hero-section []
  [:header {:class "mx-auto max-w-3xl text-center py-6 space-y-3"}
   [:div {:class "font-body text-sm font-medium uppercase tracking-[0.2em] text-muted-foreground"}
    "Component Demo"]
   [:h1 {:class "font-heading leading-heading tracking-heading text-4xl font-bold"}
    "Gesso Component Library"]
   [:p {:class "font-body leading-body text-muted-foreground text-base-theme md:text-lg-theme"}
    "Basecoat structures wrapped in Hiccup-friendly components, tested inside a real Biff app."]])

(defn- core-surfaces-section []
  [:section {:class "gap-section space-y-6"}
   (section-heading
    "Core Surfaces"
    "Cards, actions, statuses, and alerts — the basic building blocks for most application screens.")
   [:div {:class "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-8"}
    (card
     {:title "User Profile"
      :description "A basic surface for profile or account information."
      :content
      [:div {:class "gap-stack space-y-3 font-body leading-body"}
       [:p "Because this card lives in a grid inside the page container, it keeps a comfortable readable width."]
       [:div {:class "flex flex-wrap gap-2"}
        (badge {:variant :secondary :text "Active"})
        (badge {:variant :outline :text "Verified"})]]
      :footer
      [:div {:class "flex gap-2"}
       (button {:variant :outline :text "Cancel"})
       (button {:variant :primary :text "Edit Profile"})]})

    (card
     {:title "Status"
      :description "Small feedback primitives for queue and workflow state."
      :content
      [:div {:class "gap-stack space-y-4 font-body leading-body"}
       [:div {:class "flex flex-wrap gap-2"}
        (badge {:variant :secondary :text "Waiting"})
        (badge {:variant :outline :text "Claimed"})
        (badge {:variant :primary :text "In Progress"})
        (badge {:variant :destructive :text "Blocked"})]
       [:p {:class "font-body leading-body text-sm text-muted-foreground"}
        "Badges work well for request lifecycles, employee dashboards, and admin views."]]
      :footer
      [:div {:class "flex gap-2"}
       (button {:variant :outline :text "Back"})
       (button {:variant :secondary :text "Save"})]})

    (card
     {:title "Alerts"
      :description "Inline messaging for success and error states."
      :content
      [:div {:class "gap-stack space-y-4"}
       (alert {:title "Saved"
               :content "Your settings were saved successfully."})
       (alert {:variant :destructive
               :title "Something went wrong"
               :content "Please check the form and try again."})]})]])

(defn- form-controls-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Form Controls"
    "A more realistic composition showing fields grouped into clear sections with stronger hierarchy.")

   (card
    {:title "Profile Form"
     :description "Testing label, field, input, textarea, select, checkbox, switch, and radio group."
     :content
     [:form {:class "gap-section space-y-8"}

      [:section {:class "gap-stack space-y-5"}
       [:div {:class "space-y-1"}
        [:h3 {:class "font-heading leading-heading tracking-heading text-lg font-semibold"}
         "Identity"]
        [:p {:class "font-body leading-body text-sm text-muted-foreground"}
         "Basic account information and authentication fields."]]

       [:div {:class "gap-stack space-y-5"}
        (field
         {:label-text "Display name"
          :for "demo-display-name"
          :required? true
          :control (input {:id "demo-display-name"
                           :name "display-name"
                           :placeholder "Jane Doe"
                           :required? true})
          :description "This is the public name shown to other users."})

        (field
         {:label-text "Email"
          :for "demo-email"
          :control (input {:type "email"
                           :id "demo-email"
                           :name "email"
                           :placeholder "jane@example.com"})})

        (field
         {:label-text "Password"
          :for "demo-password"
          :control (input {:type "password"
                           :id "demo-password"
                           :name "password"
                           :placeholder "Enter a password"})
          :description "Password input uses the same base input component for now."})]]

      [:hr {:class "border-border"}]

      [:section {:class "gap-stack space-y-5"}
       [:div {:class "space-y-1"}
        [:h3 {:class "font-heading leading-heading tracking-heading text-lg font-semibold"}
         "Request Details"]
        [:p {:class "font-body leading-body text-sm text-muted-foreground"}
         "Structured fields suited to Humanhelp-style requests and task flows."]]

       [:div {:class "gap-stack space-y-5"}
        (field
         {:label-text "Request category"
          :for "demo-category"
          :control (select
                    {:id "demo-category"
                     :name "category"
                     :placeholder "Choose a category"
                     :options [{:value "rides" :label "Rides"}
                               {:value "groceries" :label "Groceries"}
                               {:value "companionship" :label "Companionship"}
                               {:value "other" :label "Other"}]})
          :description "Native select for now; richer custom select can come later."})

        (field
         {:label-text "Notes"
          :for "demo-notes"
          :control (textarea {:id "demo-notes"
                              :name "notes"
                              :rows 4
                              :placeholder "Add any helpful details..."})
          :description "Textarea uses the Basecoat textarea styling."})]]

      [:hr {:class "border-border"}]

      [:section {:class "gap-stack space-y-5"}
       [:div {:class "space-y-1"}
        [:h3 {:class "font-heading leading-heading tracking-heading text-lg font-semibold"}
         "Preferences"]
        [:p {:class "font-body leading-body text-sm text-muted-foreground"}
         "Examples of boolean and single-choice controls for notifications and settings."]]

       [:div {:class "gap-stack space-y-4"}
        [:div {:class "flex items-center gap-3 rounded-md px-3 py-2 hover:bg-accent/40"}
         (checkbox {:id "demo-terms"
                    :name "terms"
                    :checked true})
         (label {:for "demo-terms"
                 :text "I agree to receive service updates"})]

        [:div {:class "radius-lg border-theme flex items-center justify-between rounded-lg border p-4"}
         [:div {:class "space-y-1"}
          [:div {:class "font-heading leading-heading font-medium"} "Marketing emails"]
          [:p {:class "font-body leading-body text-sm text-muted-foreground"}
           "Receive occasional updates about new features."]]
         (switch {:id "demo-marketing"
                  :name "marketing"})]

        [:div {:class "space-y-3"}
         [:div {:class "space-y-1"}
          [:div {:class "font-heading leading-heading font-medium"} "Notify me about…"]
          [:p {:class "font-body leading-body text-sm text-muted-foreground"}
           "This is still a plain radio group for now; later this would be a good candidate for radio cards or segmented controls."]]
         (radio-group
          {:name "notify"
           :orientation :vertical
           :options [{:value "all" :label "All new messages" :checked true}
                     {:value "mentions" :label "Direct messages and mentions"}
                     {:value "nothing" :label "Nothing"}]})]]]]
     :footer
     [:div {:class "flex justify-end gap-2"}
      (button {:variant :outline :text "Cancel"})
      (button {:variant :primary :text "Submit"})]})])

(defn- disclosure-patterns-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Disclosure Patterns"
    "Accordion variants showing single-open, multiple-open, and concise map-driven usage.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Single, not collapsible"
      :description "Good for a focused FAQ or settings group where one section should always remain open."
      :content
      (accordion
       {:type :single
        :default-index 0
        :collapsible? false}
       (fn [{:keys [id title body]}]
         {:value id
          :title title
          :content body})
       (map-indexed
        (fn [i [title body]]
          {:id (inc i)
           :title title
           :body body})
        [["How layout works" "The outer page provides the container, and the inner grid controls card sizing."]
         ["Why Basecoat" "It gives us a solid structural baseline while Tailwind handles layout."]
         ["Why Gesso" "Gesso wraps those patterns in ergonomic Hiccup-friendly components."]]))})

    (card
     {:title "Multiple, default open values"
      :description "Useful when several sections should remain visible at once."
      :content
      (accordion
       {:type :multiple
        :collapsible? false
        :default-values [:item-1 :item-2]}
       (fn [{:keys [id title body]}]
         {:value id
          :title title
          :content body})
       [{:id :item-1 :title "One" :body [:p "A"]}
        {:id :item-2 :title "Two" :body [:p "B"]}])})

    (card
     {:title "Simple map form"
      :description "The shortest route when you just want to supply a few items."
      :content
      (accordion
       {:items [{:title "How does the layout work?"
                 :content [:p "We use a combination of a max-width container in ui/page and a CSS grid in the app function."]
                 :open? true}
                {:title "Are these native elements?"
                 :content [:p "Yes. The accordion uses HTML details and summary, styled by Basecoat and enhanced with a little hyperscript."]
                 :open? true}
                {:title "Can I use short-form maps?"
                 :content [:p "Absolutely. Most components support a map-based short form for cleaner code."]}]})})]])

(defn- tabs-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Tabs"
    "Layered sections of content shown one at a time, with a restrained Radix-inspired presentation.")

   [:div {:class "gap-section space-y-6"}

    (card
     {:title "Basic tabs"
      :description "A simple two-panel tab set with form content."
      :content
      (tabs
       {:default-value :account
        :id "demo-tabs-basic"}

       (tabs-list
        (tabs-trigger {:value :account} "Account")
        (tabs-trigger {:value :password} "Password"))

       (tabs-content
        {:value :account}
        [:div {:class "gap-stack space-y-5"}
         [:p {:class "font-body leading-body text-sm text-muted-foreground"}
          "Make changes to your account here. Click save when you're done."]
         [:div {:class "gap-stack space-y-4"}
          (field
           {:label-text "Name"
            :for "tabs-account-name"
            :control (input {:id "tabs-account-name"
                             :value "Pedro Duarte"})})
          (field
           {:label-text "Username"
            :for "tabs-account-username"
            :control (input {:id "tabs-account-username"
                             :value "@peduarte"})})]
         [:div {:class "flex justify-end"}
          (button {:variant :primary :text "Save changes"})]])

       (tabs-content
        {:value :password}
        [:div {:class "gap-stack space-y-5"}
         [:p {:class "font-body leading-body text-sm text-muted-foreground"}
          "Change your password here. After saving, you'll be logged out."]
         [:div {:class "gap-stack space-y-4"}
          (field
           {:label-text "Current password"
            :for "tabs-current-password"
            :control (input {:id "tabs-current-password"
                             :type "password"})})
          (field
           {:label-text "New password"
            :for "tabs-new-password"
            :control (input {:id "tabs-new-password"
                             :type "password"})})
          (field
           {:label-text "Confirm password"
            :for "tabs-confirm-password"
            :control (input {:id "tabs-confirm-password"
                             :type "password"})})]
         [:div {:class "flex justify-end"}
          (button {:variant :primary :text "Change password"})]]))})

    (card
     {:title "Three tabs"
      :description "Useful for compact settings and dashboard sections."
      :content
      (tabs
       {:default-value :overview
        :id "demo-tabs-three"}

       (tabs-list
        (tabs-trigger {:value :overview} "Overview")
        (tabs-trigger {:value :activity} "Activity")
        (tabs-trigger {:value :access} "Access"))

       (tabs-content
        {:value :overview}
        [:div {:class "gap-stack space-y-3"}
         [:p {:class "font-body leading-body text-sm text-muted-foreground"}
          "Overview content can stay light and summary-oriented."]
         [:div {:class "flex flex-wrap gap-2"}
          (badge {:variant :secondary :text "Healthy"})
          (badge {:variant :outline :text "12 Members"})
          (badge {:variant :outline :text "3 Pending"})]])

       (tabs-content
        {:value :activity}
        [:div {:class "gap-stack space-y-3"}
         [:p {:class "font-body leading-body text-sm text-muted-foreground"}
          "Recent activity, audit notes, or timeline-style information fits well here."]
         (alert
          {:title "Last update"
           :content "The workspace settings were updated 2 hours ago."})])

       (tabs-content
        {:value :access}
        [:div {:class "gap-stack space-y-4"}
         [:p {:class "font-body leading-body text-sm text-muted-foreground"}
          "Permission-related controls are a natural tab use case."]
         [:div {:class "radius-lg border-theme flex items-center justify-between rounded-lg border p-4"}
          [:div {:class "space-y-1"}
           [:div {:class "font-heading leading-heading font-medium"} "Allow external invites"]
           [:p {:class "font-body leading-body text-sm text-muted-foreground"}
            "Let members invite collaborators from outside the organization."]]
          (switch {:id "tabs-allow-invites"
                   :name "allow-invites"})]]))})]])

(defn- dialogs-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Dialogs"
    "Modal surfaces for confirmations, editing flows, and focused tasks.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Simple short form"
      :description "A dialog assembled from the root short form."
      :content
      (dialog
       {:trigger "Open dialog"
        :title "Edit profile"
        :description "Make changes to your profile here."
        :body
        [:div {:class "gap-stack space-y-4"}
         [:p {:class "font-body leading-body"} "This is the simplest way to use the dialog component."]
         (field
          {:label-text "Display name"
           :for "dialog-display-name"
           :control (input {:id "dialog-display-name"
                            :name "dialog-display-name"
                            :placeholder "Jane Doe"})})]
        :footer
        [(dialog-close {:text "Cancel"})
         (button {:variant :primary :text "Save changes"})]})})

    (card
     {:title "More complex dialog"
      :description ""
      :content
      (dialog
       {:trigger "Edit profile"
        :title "Edit profile"
        :description "Update account details."
        :body
        [:form {:class "gap-section space-y-6"}
         (field
          {:label-text "Display name"
           :for "dialog-name"
           :control (input {:id "dialog-name"
                            :name "dialog-name"
                            :placeholder "Jane Doe"})})
         (field
          {:label-text "Email"
           :for "dialog-email"
           :control (input {:type "email"
                            :id "dialog-email"
                            :name "dialog-email"
                            :placeholder "jane@example.com"})})]
        :footer
        [(dialog-close {:text "Cancel"})
         (button {:variant :primary :text "Save"})]})})

    (card
     {:title "Composed dialog"
      :description "The lower-level pieces can also be composed directly."
      :content
      (dialog {}
              (dialog-trigger {:text "Open composed dialog"})
              (dialog-overlay)
              (dialog-content {}
                              (dialog-header
                               (dialog-title {} "Delete request")
                               (dialog-description {}
                                                   "This action cannot be undone. This will permanently remove the request."))
                              (dialog-body
                               [:div {:class "gap-stack space-y-3"}
                                [:p {:class "font-body leading-body"}
                                 "Use the composed form when more control over layout is needed."]
                                (alert {:variant :destructive
                                        :title "Warning"
                                        :content "Deleted requests cannot be recovered."})])
                              (dialog-footer
                               (dialog-close {:text "Cancel"})
                               (button {:variant :destructive
                                        :text "Delete"}))))})]])

(defn- dropdown-menus-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Dropdown Menus"
    "Compact floating menus for actions, shortcuts, and grouped command lists.")

   [:div {:class "gap-section space-y-6"}

    (card
     {:title "Simple short form"
      :description "The quickest way to render a trigger plus a few menu items."
      :content
      (dropdown-menu
       {:trigger "Open menu"
        :items [{:text "Profile"}
                {:text "Settings"}
                {:separator? true}
                {:text "Sign out"}]})})

    (card
     {:title "Composed menu"
      :description "Use the lower-level pieces for labels, separators, and right-side hints."
      :content
      (dropdown-menu {}
                     (dropdown-menu-trigger
                      {:class "btn-outline"}
                      "Actions")
                     (dropdown-menu-content {}
                                            (dropdown-menu-label {:text "Account"})
                                            (dropdown-menu-item {}
                                                                [:span "Profile"]
                                                                (dropdown-menu-right-slot {} "⌘P"))
                                            (dropdown-menu-item {}
                                                                [:span "Billing"]
                                                                (dropdown-menu-right-slot {} "⌘B"))
                                            (dropdown-menu-separator)
                                            (dropdown-menu-label {:text "Session"})
                                            (dropdown-menu-item {:disabled? true}
                                                                [:span "Switch workspace"])
                                            (dropdown-menu-item {}
                                                                [:span "Sign out"])))})]])
(defn- typography-section []
  [:section {:class "section-theme max-w-4xl mx-auto"}
   [:div {:class "space-y-2"}
    (heading {:level 2 :text "Typography"})
    (muted-text
     {:as :p
      :text "A realistic sample of heading, lead, body, muted, small, and label roles under the active typography theme."})]

   (card
    {:title "Article preview"
     :description "A more realistic hierarchy test than listing heading variants one after another."
     :content
     [:article {:class "panel-theme"}
      (page-title {:text "Human help, fast."})

      (text
       {:variant :lead
        :text "Customers should be able to request help quickly, see that someone is coming, and keep moving without friction."})

      (heading
       {:level 3
        :text "Why hierarchy matters"})

      (text
       {:variant :body
        :text "A typography system should not rely on size alone. Good hierarchy also comes from line-height, weight, contrast, and spacing. If those things are working, a page feels easier to scan even before you consciously notice why."})

      (muted-text
       {:as :p
        :text "Updated 2 hours ago · Seattle flagship · 3 staff online"})

      (text
       {:variant :small
        :text "Small text is useful for secondary metadata and compact explanatory copy."})

      [:div {:class "cluster-theme items-center"}
       (label-text {:text "Status"})
       (badge {:variant :outline :text "Active"})]]

     :footer
     [:div {:class "cluster-theme justify-end"}
      (button {:variant :outline :text "Secondary"})
      (button {:variant :primary :text "Primary"})]})])




(defn- empty-states-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Empty States"
    "Reusable placeholders for empty lists, filtered views, and waiting regions.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Simple empty state"
      :description "Short-form usage with an optional decorative icon, title, description, and one action."
      :content
      (empty-state
        {:icon (empty-state-icon)
        :title "No waiting requests"
        :description "New requests will appear here as customers ask for help."
        :action (button {:variant :outline :text "Refresh"})})})

    (card
     {:title "Composed empty state"
      :description "Long-form usage with explicit subcomponents and multiple actions."
      :content
      (empty-state {}
                   [:div {:class "flex items-center justify-center"
                          :style {:color "var(--muted-foreground)"}}
                    (empty-state-icon)
                    ]
                   (empty-state-title "No search results")
                   (empty-state-description
                    "Try a broader query or clear your current filters.")
                   (empty-state-actions
                    (button {:variant :outline :text "Clear filters"})
                    (button {:variant :primary :text "Create request"})))})]])


(defn- icons-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Icons"
    "Inline SVG icons resolved by name from the current icon search paths, with theme sizes and optional explicit sizing.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Theme sizes"
      :description "The icon component supports xs, sm, md, lg, xl, and 2xl semantic sizes."
      :content
      [:div {:class "cluster-theme items-center"}
       (icon "search" {:size :xs})
       (icon "search" {:size :sm})
       (icon "search" {:size :md})
       (icon "search" {:size :lg})
       (icon "search" {:size :xl})
       (icon "search" {:size :2xl})]})

    (card
     {:title "Different icons"
      :description "Any icon name available in the current project or bundled fallback set can be rendered the same way."
      :content
      [:div {:class "cluster-theme items-center"}
       (icon "search" {:size :lg})
       (icon "inbox" {:size :lg})
       (icon "alert-triangle" {:size :lg})
       (icon "check" {:size :lg})
       (icon "x" {:size :lg})
       (icon "chevron-down" {:size :lg})]})

    (card
     {:title "Explicit sizing"
      :description "You can override semantic sizes with a square :size or explicit :width and :height."
      :content
      [:div {:class "cluster-theme items-center"}
       (icon "inbox" {:size "1.5rem"})
       (icon "inbox" {:size 28})
       (icon "inbox" {:size "28px"})
       (icon "alert-triangle" {:width "4.25rem" :height "4.75rem"})
       (icon "search" {:width "2rem" :height "1.25rem"})]})

    (card
     {:title "Inside controls and text"
      :description "Icons compose naturally with buttons and other inline UI."
      :content
      [:div {:class "cluster-theme items-center"}
       (button {}
               (icon "search" {:size :sm})
               [:span "Search"])
       (button {:variant :outline}
               (icon "inbox" {:size :sm})
               [:span "Inbox"])
       [:div {:class "cluster-theme items-center"}
        (icon "check" {:size :sm})
        (text {:variant :muted
               :as :span
               :text "Synced"})]]})

    (card
     {:title "Accessible title"
      :description "Icons are decorative by default. Supplying :title exposes the icon as an image with a title."
      :content
      [:div {:class "cluster-theme items-center"}
       (icon "check" {:title "Success"})
       (icon "alert-triangle" {:title "Warning"})
       (icon "search" {:title "Search"})]})]])

(defn- status-pills-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Status Pills"
    "Compact inline state indicators for queues, ownership, and live-updating task state.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Semantic statuses"
      :description "Core semantic variants for neutral, informative, successful, warning, and destructive states."
      :content
      [:div {:class "cluster-theme items-center"}
       (status-pill {:status :default})
       (status-pill {:status :muted})
       (status-pill {:status :info})
       (status-pill {:status :success})
       (status-pill {:status :warning})
       (status-pill {:status :destructive})]})

    (card
     {:title "Friendly aliases"
      :description "Aliases map common workflow states onto the semantic variants."
      :content
      [:div {:class "cluster-theme items-center"}
       (status-pill {:status :waiting :dot? true})
       (status-pill {:status :active})
       (status-pill {:status :claimed})
       (status-pill {:status :complete :icon "check"})
       (status-pill {:status :cancelled})
       (status-pill {:status :error :icon "alert-triangle"})]})

    (card
     {:title "Explicit labels"
      :description "You can override the default label or use long-form content."
      :content
      [:div {:class "cluster-theme items-center"}
       (status-pill {:status :info :text "Yours"})
       (status-pill {:status :warning :text "Needs review" :icon "alert-triangle"})
       (status-pill {:status :success}
                    "Synced") ]})]])

(defn- groups-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Groups"
    "A generic grouping primitive for related actions, pills, and inline controls, with optional attached styling.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Default group"
      :description "A simple wrapping cluster of related actions."
      :content
      (group {}
             (button {:variant :outline :text "Cancel"})
             (button {:variant :primary :text "Save"})
             (button {:variant :ghost :text "Duplicate"}))})

    (card
     {:title "Aligned group"
      :description "Groups can align their contents without needing ad hoc wrapper divs."
      :content
      (group {:align :end}
             (button {:variant :outline :text "Back"})
             (button {:variant :primary :text "Continue"}))})

    (card
     {:title "Attached buttons"
      :description "Attached groups visually join adjacent controls into a single compound control."
      :content
      (group {:attached? true}
             (button {:variant :outline :text "Day"})
             (button {:variant :outline :text "Week"})
             (button {:variant :outline :text "Month"}))})

    (card
     {:title "Attached vertical group"
      :description "Attachment also works vertically for stacked segmented controls."
      :content
      (group {:attached? true
              :orientation :vertical
              :class "max-w-xs"}
             (button {:variant :outline :text "Profile"})
             (button {:variant :outline :text "Notifications"})
             (button {:variant :outline :text "Security"}))})

    (card
     {:title "Mixed content"
      :description "Groups are not limited to buttons and can be used for any related inline UI."
      :content
      (group {}
             (status-pill {:status :success :icon "check"})
             (status-pill {:status :warning :dot? true})
             [:span {:class "font-body text-sm-theme leading-body"} "3 items selected"])})]])


(defn- section-blocks-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Section Blocks"
    "Structured page sections with optional description, actions, and body content, without forcing a card-like surface.")

   [:div {:class "gap-section space-y-6"}
    (section-block
      {:title "Waiting requests"
       :description "Requests that have not yet been claimed."
       :actions [(button {:variant :outline :text "Refresh"})]
       :content [[:div {:class "panel-theme"}
                  (status-pill {:status :waiting :dot? true})
                  (text {:as :p
                         :variant :muted
                         :text "No requests are currently waiting."})]]})

    card
    (section-block {}
                   (section-block-header {}
                                         [:div {:class "flex flex-col gap-field"}
                                          (section-block-title "Active work")
                                          (section-block-description
                                            "Items currently assigned to the employee.")]
                                         (section-block-actions
                                           (button {:variant :outline :text "Filter"})
                                           (button {:variant :primary :text "Create"})))
                   (section-block-content
                     [:div {:class "panel-theme"}
                      (group {}
                             (status-pill {:status :active})
                             (status-pill {:status :claimed :text "Yours"}))
                      (text {:as :p
                             :variant :muted
                             :text "This area can hold lists, cards, metrics, or live-updating fragments."})]))]])

(defn- toolbars-section []
  [:section {:class "gap-section space-y-6 max-w-4xl mx-auto"}
   (section-heading
    "Toolbars"
    "Local control rows for search, filters, status, and actions, with optional center and spacer regions.")

   [:div {:class "gap-section space-y-6"}
    (card
     {:title "Short-form toolbar"
      :description "A compact controls row with start, center, and end regions."
      :content
      (toolbar
       {:start [(button {:variant :outline :text "Refresh"})
                (button {:variant :outline :text "Filter"})]
        :center [(status-pill {:status :active})
                 (status-pill {:status :claimed :text "Yours"})]
        :end [(button {:variant :primary :text "Create request"})]})})

    (card
     {:title "Toolbar with spacer"
      :description "The spacer pushes trailing actions away without hand-written flex filler divs."
      :content
      (toolbar {}
               (toolbar-start
                (button {:variant :outline :text "Back"})
                (button {:variant :outline :text "Duplicate"}))
               (toolbar-spacer)
               (toolbar-end
                (button {:variant :outline :text "Cancel"})
                (button {:variant :primary :text "Save"})))})

    (card
     {:title "Centered controls"
      :description "The center region works well for view switches or contextual controls."
      :content
      (toolbar {}
               (toolbar-start
                (text {:variant :muted
                       :as :span
                       :text "Showing 24 results"}))
               (toolbar-center
                (group {:attached? true}
                       (button {:variant :outline :text "Day"})
                       (button {:variant :outline :text "Week"})
                       (button {:variant :outline :text "Month"})))
               (toolbar-end
                (button {:variant :outline :text "Export"})))})

    (card
     {:title "Wrapping toolbar"
      :description "Toolbars wrap naturally on narrow widths while preserving clear region structure."
      :content
      (toolbar {}
               (toolbar-start
                (button {:variant :outline :text "All"})
                (button {:variant :outline :text "Open"})
                (button {:variant :outline :text "Closed"}))
               (toolbar-center
                (status-pill {:status :waiting :dot? true})
                (status-pill {:status :success :icon "check"}))
               (toolbar-end
                (button {:variant :outline :text "Reset"})
                (button {:variant :primary :text "Apply"})))})]])

(def ^:private page-demo-links
  [{:slug "focused"
    :title "Focused"
    :description "A centered main work area with flexible side gutters and one continuous surface."}
   {:slug "wide-focused"
    :title "Wide focused"
    :description "A wider central lane for denser screens that still want one coherent plane."}
   {:slug "sidebar-main"
    :title "Sidebar + main"
    :description "A left rail for navigation or filters with a main work surface."}
   {:slug "main-rail"
    :title "Main + rail"
    :description "A main work area with a right-hand contextual rail."}
   {:slug "three-column"
    :title "Three column"
    :description "Left, main, and right regions all active at once."}
   {:slug "full"
    :title "Full"
    :description "A broad full-width page for dashboards and dense operational screens."}
   {:slug "custom-layout"
    :title "Custom layout"
    :description "An example that uses :layout directly for an editorial-style page with a left rail, a lead surface, and a wide supporting band."}
{:slug "bars-demo"
    :title "Bars demo"
    :description "A full-page demo of the bars navigation shell, including topbar, sidebar, and hamburger behavior."}
{:slug "form-flow-demo"
 :title "Form flow demo"
 :description "A multi-step form demo with fields, client validation, server validation, HTMX OOB errors, and chained form progression."}
   ])

(defn- pages-section []
  [:section {:class "gap-section space-y-6 max-w-3xl mx-auto"}
   (section-heading
    "Pages"
    "Real routed page demos for the page component family. These links should open full pages rather than tiny in-gallery previews.")

   [:div {:class "panel-theme pad-card radius-lg"
          :style {:border (str "var(--border-width, 1px) solid var(--border)")
                  :background "var(--card)"
                  :color "var(--card-foreground)"}}
    [:ul {:class "list-theme"}
     (for [{:keys [slug title description]} page-demo-links]
       [:li {:key slug
             :class "panel-theme"}
        [:a {:href (str "/app/pages/" slug)
             :class "font-body text-base-theme leading-body weight-semibold-theme"
             :style {:color "var(--primary)"}}
         title]
        [:p {:class "font-body text-sm-theme leading-body"
             :style {:color "var(--muted-foreground)"}}
         description]])]]])

(defn app [ctx]
  (ui/page
   ctx
   [:div {:class "gap-section space-y-14 font-body leading-body"}
    (hero-section)
    (core-surfaces-section)
    (form-controls-section)
    (disclosure-patterns-section)
    (dialogs-section)
    (dropdown-menus-section)
    (tabs-section)
    (typography-section)
    (empty-states-section)
    (icons-section)
    (status-pills-section)
    (groups-section)
    (section-blocks-section)
    (toolbars-section)
    (pages-section)
    (shared-counter/section)
    (simple-shared-counter/section )
    (toaster-demo/section)
    (scroll-buffer {:size :lg})
    ]))

(defn set-foo [{:keys [session params] :as ctx}]
  (biffx/submit-tx ctx
                   [[:patch-docs :user {:xt/id (:uid session)
                                        :user/foo (:foo params)}]])
  {:status 303
   :headers {"location" "/app"}})

(defn bar-form [{:keys [value]}]
  (biff/form
   {:hx-post "/app/set-bar"
    :hx-swap "outerHTML"}
   [:label.block {:for "bar"} "Bar: "
    [:span.font-mono (pr-str value)]]
   [:.h-1]
   [:.flex
    [:input.w-full#bar {:type "text" :name "bar" :value value}]
    [:.w-3]
    [:button.btn {:type "submit"} "Update"]]
   [:.h-1]
   [:.text-sm.text-gray-600
    "This demonstrates updating a value with HTMX."]))

(defn set-bar [{:keys [session params] :as ctx}]
  (time (biffx/submit-tx ctx
                         [[:patch-docs :user {:xt/id (:uid session) :user/bar (:bar params)}]]))
  (biff/render (bar-form {:value (:bar params)})))

(defn message [{:msg/keys [content sent-at]}]
  [:.mt-3 {:_ "init send newMessage to #message-header"}
   [:.text-gray-600 (tick/format "dd MMM yyyy HH:mm:ss" sent-at)]
   [:div content]])

(defn notify-clients [{:keys [gessotest/chat-clients]} record]
  (when (= "msg" (:biff.xtdb/table record))
    (let [html (rum/render-static-markup
                [:div#messages {:hx-swap-oob "afterbegin"}
                 (message record)])]
      (doseq [ws @chat-clients]
        (ws/send ws html)))))

(defn send-message [{:keys [session] :as ctx} {:keys [text]}]
  (let [{:keys [content]} (cheshire/parse-string text true)]
    (biffx/submit-tx ctx
                     [[:put-docs :msg {:xt/id (random-uuid)
                                       :msg/user (:uid session)
                                       :msg/content content
                                       :msg/sent-at (tick/zoned-date-time)}]])))

(defn chat [{:keys [biff/conn]}]
  (let [messages (biffx/q conn
                          {:select [:msg/content :msg/sent-at]
                           :from :msg
                           :where [:>= :msg/sent-at (tick/<< (tick/now)
                                                             (tick/of-minutes 10))]})]
    [:div {:hx-ext "ws" :ws-connect "/app/chat"}
     [:form.mb-0 {:ws-send true
                  :_ "on submit set value of #message to ''"}
      [:label.block {:for "message"} "Write a message"]
      [:.h-1]
      [:textarea.w-full#message {:name "content"}]
      [:.h-1]
      [:.text-sm.text-gray-600
       "Sign in with an incognito window to have a conversation with yourself."]
      [:.h-2]
      [:div [:button.btn {:type "submit"} "Send message"]]]
     [:.h-6]
     [:div#message-header
      {:_ "on newMessage put 'Messages sent in the past 10 minutes:' into me"}
      (if (empty? messages)
        "No messages yet."
        "Messages sent in the past 10 minutes:")]
     [:div#messages
      (map message (sort-by :msg/sent-at #(compare %2 %1) messages))]]))

(defn ws-handler [{:keys [gessotest/chat-clients] :as ctx}]
  {:status 101
   :headers {"upgrade" "websocket"
             "connection" "upgrade"}
   ::ws/listener {:on-open (fn [ws]
                             (swap! chat-clients conj ws))
                  :on-message (fn [ws text-message]
                                (send-message ctx {:ws ws :text text-message}))
                  :on-close (fn [ws _status-code _reason]
                              (swap! chat-clients disj ws))}})

(def about-page
  (ui/page
   {:base/title (str "About " settings/app-name)}
   [:p {:class "font-body leading-body"}
    "This app was made with "
    [:a.link {:href "https://biffweb.com"} "Biff"] "."]))

(defn echo [{:keys [params]}]
  {:status 200
   :headers {"content-type" "application/json"}
   :body params})

(def live-system
  (live.app/simple-system
   {:configs [shared-counter/live-config]}))

(def module
  {:static {"/about/" about-page}
   :routes [["/app" {:middleware [mid/wrap-signed-in
                                  (:middleware live-system)]}
             ["" {:get app}]
             ["/set-foo" {:post set-foo}]
             ["/set-bar" {:post set-bar}]
             ["/chat" {:get ws-handler}]
             ["/gesso/live/stream" {:get (:sse-handler live-system)}]

             ;; Original Counter
             ["/demo/shared-counter/fragment" {:get shared-counter/fragment}]
             ["/demo/shared-counter/increment" {:post shared-counter/increment!}]
             ["/demo/shared-counter/decrement" {:post shared-counter/decrement!}]

             ;; Simple Counter
             ["/demo/simple-shared-counter/fragment" {:get simple-shared-counter/fragment}]
             ["/demo/simple-shared-counter/increment" {:post simple-shared-counter/increment!}]
             ["/demo/simple-shared-counter/decrement" {:post simple-shared-counter/decrement!}]

             ["/pages" {}
              ["/focused" {:get page-focused}]
              ["/bars-demo" {:get bars-demo-page}]
              ["/wide-focused" {:get page-wide-focused}]
              ["/sidebar-main" {:get page-sidebar-main}]
              ["/main-rail" {:get page-main-rail}]
              ["/three-column" {:get page-three-column}]
              ["/full" {:get page-full}]
              ["/custom-layout" {:get page-custom-layout}]]]]
   :api-routes [["/api/echo" {:post echo}]]
   :on-tx notify-clients})
