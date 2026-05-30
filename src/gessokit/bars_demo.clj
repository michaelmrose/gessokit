(ns gessokit.bars-demo
  (:require
   [gesso.components.bars.core :as bars]
   [gesso.core :refer [button card group icon status-pill text background]]
   [gesso.components.background.patterns :as bg]
   [gessokit.ui :as ui]))

(defn- demo-link-item
  ([text]
   (demo-link-item text {}))
  ([text opts]
   (bars/menu-item
    (merge {:text text
            :href "/app/pages/bars-demo"}
           opts))))

(defn- dashboard-menu []
  (bars/menu
   {:home-region :leftmost
    :priority 100
    :groups [(bars/menu-group
              {:items [(demo-link-item "Dashboard" {:current? true})]})]}))

(defn- browse-menu []
  (bars/menu
   {:label "Browse"
    :icon "inbox"
    :home-region :center
    :priority 80
    :collapse-at :small
    :groups [(bars/menu-group
              {:heading "Queues"
               :items [(demo-link-item "Waiting" {:icon "alert-triangle"})
                       (demo-link-item "Active" {:icon "check"})
                       (demo-link-item "Completed" {:icon "check"})]})
             (bars/menu-group
              {:heading "Explore"
               :items [(demo-link-item "Saved searches" {:icon "search"})
                       (demo-link-item "Inbox" {:icon "inbox"})]})]}))

(defn- account-menu []
  (bars/menu
   {:label "Account"
    :icon "check"
    :home-region :rightmost
    :category :account
    :collapse-at :small
    :priority 20
    :groups [(bars/menu-group
              {:heading "Session"
               :items [(demo-link-item "Profile" {:icon "check"})
                       (demo-link-item "Settings" {:icon "search"})
                       (bars/menu-item {:text "Sign out"
                                        :icon "x"})]})]}))

(defn- operations-menu []
  (bars/menu
   {:label "Operations"
    :home-region :sidebar
    :category :operations
    :priority 60
    :groups [(bars/menu-group
              {:heading "Queues"
               :items [(demo-link-item "Waiting" {:icon "alert-triangle"})
                       (demo-link-item "Active" {:icon "check"})
                       (demo-link-item "Completed" {:icon "check"})]})
             (bars/menu-group
              {:heading "Tools"
               :items [(demo-link-item "Schedule" {:icon "search"})
                       (demo-link-item "Coverage" {:icon "inbox"})]})]}))

(defn- demo-menus []
  [
   ;; (dashboard-menu)
   (browse-menu)
   (browse-menu)
   (browse-menu)
   (account-menu)
   (operations-menu)])

(defn- demo-brand []
  [:a {:href "/app/pages/bars-demo"
       :class "cluster-theme items-center"
       :style {:color "var(--foreground)"
               :text-decoration "none"}}
   (icon "search" {:size :sm})
   [:span {:class "font-heading text-md-theme weight-semibold-theme"}
    "Gesso"]])

(defn- intro-card []
  (card
   {:title "Bars demo"
    :description "This route exists only to load the current bars component in a real gessokit page."
    :content
    [:div {:class "content-stack-theme"}
     (text
      {:variant :body
       :text "The topbar now demonstrates both direct single-item menus and richer click-open dropdown menus."})
     (text
      {:variant :muted
       :text "Resize the browser to verify the large, medium, and small behaviors while also checking dropdown triggers and item icons."})]}))

(defn- behavior-card []
  (card
   {:title "What to look for"
    :description "Manual test checklist."
    :content
    [:div {:class "content-stack-theme"}
     (text
      {:variant :body
       :text "Large: Dashboard should render directly, while Browse and Account should render as dropdown triggers in the topbar."})
     (text
      {:variant :body
       :text "Medium: Account should collapse into the hamburger, while Browse should remain in the topbar."})
     (text
      {:variant :body
       :text "Small: opening the hamburger should replace the page content below the topbar."})]}))

(defn- filler-card []
  (card
   {:title "Dummy actions"
    :description "Simple filler content so layout changes are easy to notice."
    :content
    [:div {:class "content-stack-theme"}
     (button {:variant :primary :text "Primary action"})
     (button {:variant :outline :text "Secondary action"})
     (button {:variant :ghost :text "Tertiary action"})]}))

(defn- notes-card []
  (card
   {:title "Notes"
    :description "Current scope of this test page."
    :content
    [:div {:class "content-stack-theme"}
     (text
      {:variant :body
       :text "This page is testing the bars component exactly as it exists now."})
     (text
      {:variant :body
       :text "Topbar dropdown support is now exercised here, but measured overflow logic is still not part of the implementation."})]}))

(defn- status-row []
  (group {:class "cluster-theme"}
         (status-pill {:status :active :text "Topbar"})
         (status-pill {:status :info :text "Sidebar"})
         (status-pill {:status :warning :text "Hamburger"})))


(defn- demo-content []
  [:div {:class "relative isolate overflow-hidden"}
   [:div {:class "relative z-10 pad-container py-8"}
    [:div {:class "section-theme max-w-5xl mx-auto"}
     (intro-card)
     (status-row)
     [:div {:class "grid grid-cols-1 xl:grid-cols-2 gap-6"}
      (behavior-card)
      (filler-card)]
     (notes-card)]]])

(defn bars-demo-page
  [ctx]
  (ui/page-shell
   ctx
   (bars/bars
    {:brand (demo-brand)
     :sidebar-collapse-at :medium
     :menus (demo-menus)}
    (demo-content))))

(defn bars-demo-page
  [ctx]
  (ui/page-shell
   ctx
   [:div {:class "relative isolate min-h-screen overflow-hidden"}
    (background {:light bg/orb-grid-background-light
                 :dark bg/orb-grid-background-light})

    [:div {:class "relative z-10"}
     (bars/bars
      {:brand (demo-brand)
       :sidebar-collapse-at :medium
       :menus (demo-menus)}
      (demo-content))]]))
