(ns gessokit.form-flow-demo.bars
  (:require
   [gesso.components.bars.core :as bars]
   [gesso.core :as g]))

(defn- demo-link-item
  [text href opts]
  (bars/menu-item
   (merge {:text text
           :href href}
          opts)))

(defn brand
  [base-path]
  [:a {:href base-path
       :class "cluster-theme items-center"
       :style {:color "var(--foreground)"
               :text-decoration "none"}}
   (g/icon "search" {:size :sm})
   [:span {:class "font-heading text-md-theme weight-semibold-theme"}
    "Gesso"]])

(defn- demo-menu
  [base-path]
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
  [base-path]
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
  [base-path]
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
                         :current? true})]})]}))

#_(defn- sidebar-menu
  [base-path]
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

(defn menus
  [base-path]
  [(demo-menu base-path)
   (account-menu base-path)
   (sidebar-menu base-path)])
