(ns gessokit.page-examples
  (:require
   [gessokit.ui :as ui]
   [gesso.core :as gs :refer :all ]
   ))
(defn- page-demo-nav []
  (group {}
         [:a {:href "/app"
              :class "font-body text-sm-theme leading-body weight-medium-theme"
              :style {:color "var(--primary)"}}
          "Back to gallery"]
         [:a {:href "/app#pages"
              :class "font-body text-sm-theme leading-body weight-medium-theme"
              :style {:color "var(--primary)"}}
          "Back to pages section"]))

(defn- page-demo-header
  [title description]
  [:div {:class "section-theme"}
   (page-demo-nav)
   [:header {:class "stack-theme"}
    [:h1 {:class "font-heading text-3xl-theme leading-tight-theme tracking-tight-theme weight-bold-theme"}
     title]
    [:p {:class "font-body text-base-theme leading-body"
         :style {:color "var(--muted-foreground)"}}
     description]]])

(defn- plain-surface
  [& children]
  (into
   [:section {:class "panel-theme pad-card radius-lg"
              :style {:border (str "var(--border-width, 1px) solid var(--border)")
                      :background "var(--card)"
                      :color "var(--card-foreground)"}}]
   children))

(defn- rail-block
  [title & body]
  (into
   [:section {:class "stack-theme"}
    [:h2 {:class "font-heading text-lg-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
     title]]
   body))

(defn- content-block
  [title & body]
  (into
   [:section {:class "stack-theme"}
    [:h2 {:class "font-heading text-xl-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
     title]]
   body))

(defn page-focused
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :focused}
         (page-main
          (page-surface
           (page-demo-header
            "Focused page"
            "A centered page with one continuous work surface.")
           (toolbar {}
                    (toolbar-start
                     (status-pill {:status :active})
                     (status-pill {:status :claimed :text "Yours"}))
                    (toolbar-end
                     (button {:variant :outline :text "Refresh"})
                     (button {:variant :primary :text "Create"})))
           (content-block
            "Primary content"
            [:p {:class "font-body text-base-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Use this for settings pages, detail pages, and forms where the page should read like one coherent sheet."]
            (empty-state
             {:title "No active items"
              :description "This layout keeps attention centered on the main work area."
              :action (button {:variant :outline :text "Refresh"})})))))))

(defn page-wide-focused
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :wide-focused}
         (page-main
          (page-surface
           (page-demo-header
            "Wide focused page"
            "A wider centered lane for denser layouts that still want one main plane.")
           (toolbar {}
                    (toolbar-start
                     (button {:variant :outline :text "Back"})
                     (button {:variant :outline :text "Duplicate"}))
                    (toolbar-center
                     (group {:attached? true}
                            (button {:variant :outline :text "Day"})
                            (button {:variant :outline :text "Week"})
                            (button {:variant :outline :text "Month"})))
                    (toolbar-end
                     (button {:variant :primary :text "Save"})))
           [:section {:class "grid gap-panel md:grid-cols-2"}
            (plain-surface
             (content-block
              "Left block"
              [:p {:class "font-body text-sm-theme leading-body"
                   :style {:color "var(--muted-foreground)"}}
               "A wide focused page can comfortably hold denser two-up content without becoming a true app shell."]))
            (plain-surface
             (content-block
              "Right block"
              [:p {:class "font-body text-sm-theme leading-body"
                   :style {:color "var(--muted-foreground)"}}
               "This works well for broad settings, detail, and operational pages."]))])))))

(defn page-sidebar-main
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :sidebar-main
          :collapse-policy

          ;; shorter or longer version

          #_{:md {:keep [:left :main]
                :stack-order [:left :main ]}
           :sm {:keep [:main]
                :drop [:left ]
                :stack-order [:main ]}}

          {:md {:keep [:left :main] }
           :sm {:keep [:main]}}
          }
         (page-left
          [:div {:class "section-theme"}
           (rail-block
            "Sidebar"
            [:p {:class "font-body text-sm-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Use this region for navigation, saved views, or filters."]
            [:nav {:class "list-theme"}
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "Overview"]
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "Assigned"]
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "Completed"]])])
         (page-main
          (page-surface
           (page-demo-header
            "Sidebar + main"
            "A left rail for navigation or filters with a main work surface.")
           (toolbar {}
                    (toolbar-start
                     (button {:variant :outline :text "Filter"})
                     (button {:variant :outline :text "Sort"}))
                    (toolbar-end
                     (button {:variant :primary :text "Create"})))
           (content-block
            "Main content"
            [:p {:class "font-body text-base-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "This is a strong default for admin screens and list/detail workflows where persistent side controls matter."]))))))

(defn page-main-rail
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :main-rail
          :collapse-policy
          {:md {:keep [:main :right]
                :drop [:left]
                :stack-order [:main :right :left]}
           :sm {:keep [:main]
                :drop [:left :right]
                :stack-order [:main :right :left]}}}
         (page-main
          (page-surface
           (page-demo-header
            "Main + rail"
            "A dominant main work area with a contextual right rail.")
           (content-block
            "Main work area"
            [:p {:class "font-body text-base-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "The central region remains dominant, while the rail stays clearly secondary."]
            (group {}
                   (status-pill {:status :active})
                   (status-pill {:status :warning :dot? true})
                   (status-pill {:status :success :icon "check"})))))
         (page-right
          [:div {:class "section-theme"}
           (rail-block
            "Right rail"
            [:p {:class "font-body text-sm-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Good for summaries, activity, assignment details, or compact actions."]
            (group {:orientation :vertical}
                   (button {:variant :outline :text "Refresh"})
                   (button {:variant :outline :text "Escalate"})
                   (button {:variant :primary :text "Complete"})))]))))

(defn page-three-column
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :three-column
          :collapse-policy
          {:md {:keep [:left :right :main]
                :stack-order [ :main :left :right]}
           :sm {:keep [:main]
                :drop [:left :right]
                :stack-order [:main :left :right]}}}
         (page-left
          [:div {:class "section-theme"}
           (rail-block
            "Left"
            [:p {:class "font-body text-sm-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Navigation, filters, or saved views."]
            [:nav {:class "list-theme"}
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "Queue"]
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "Active"]
             [:a {:href "#"
                  :style {:color "var(--primary)"}} "History"]])])
         (page-main
          (page-surface
           (page-demo-header
            "Three-column page"
            "A true app-style layout where all three regions are active.")
           (content-block
            "Main content"
            [:p {:class "font-body text-base-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Use this when both side regions are first-class parts of the workflow, not just decorative gutters."])))
         (page-right
          [:div {:class "section-theme"}
           (rail-block
            "Right"
            [:p {:class "font-body text-sm-theme leading-body"
                 :style {:color "var(--muted-foreground)"}}
             "Inspectors, activity, or contextual details."]
            (group {:orientation :vertical}
                   (status-pill {:status :claimed :text "Yours"})
                   (status-pill {:status :warning :dot? true})
                   (status-pill {:status :success :icon "check"})))]))))


(defn page-full
  [ctx]
  (ui/page-shell
   ctx
   (page {:variant :full}
         (page-main
          [:div {:class "section-theme pad-container"}
           (page-demo-header
            "Full page"
            "A broad layout for dashboards and denser operational screens.")
           (toolbar {}
                    (toolbar-start
                     (button {:variant :outline :text "Refresh"})
                     (button {:variant :outline :text "Filter"})
                     (button {:variant :outline :text "Export"}))
                    (toolbar-spacer)
                    (toolbar-end
                     (button {:variant :primary :text "Create"})))
           [:section {:class "grid gap-panel md:grid-cols-2 xl:grid-cols-3"}
            (plain-surface
             (content-block
              "Summary A"
              [:p {:class "font-body text-sm-theme leading-body"
                   :style {:color "var(--muted-foreground)"}}
               "A broad page can still use smaller surfaced blocks inside the full-width layout."]))
            (plain-surface
             (content-block
              "Summary B"
              [:p {:class "font-body text-sm-theme leading-body"
                   :style {:color "var(--muted-foreground)"}}
               "This variant is for genuinely wide screens, not just a slightly larger centered column."]))
            (plain-surface
             (content-block
              "Summary C"
              [:p {:class "font-body text-sm-theme leading-body"
                   :style {:color "var(--muted-foreground)"}}
               "Use it when the screen truly benefits from a broader layout."]))]]))))


(defn page-custom-layout
  [ctx]
  (ui/page-shell
   ctx
   (page
    {:layout
     {:default {:areas [[:left :main :main]
                        [:left :right :right]]
                :columns ["15rem" "minmax(0,1fr)" "minmax(0,1fr)"]
                :show [:left :main :right]}
      :md {:areas [[:main]
                   [:right]]
           :columns ["minmax(0,1fr)"]
           :show [:main :right]}
      :sm {:areas [[:main]]
           :columns ["minmax(0,1fr)"]
           :show [:main]}}}

    (page-left
     [:div {:class "section-theme"}
      (rail-block
       "Sections"
       [:p {:class "font-body text-sm-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
        "This left rail stays present on large screens, then drops away on small screens."]
       [:nav {:class "list-theme"}
        [:a {:href "#"
             :style {:color "var(--primary)"}} "Top stories"]
        [:a {:href "#"
             :style {:color "var(--primary)"}} "Analysis"]
        [:a {:href "#"
             :style {:color "var(--primary)"}} "Markets"]
        [:a {:href "#"
             :style {:color "var(--primary)"}} "Opinion"]])
      (plain-surface
       [:div {:class "stack-theme"}
        [:h3 {:class "font-heading text-md-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
         "At a glance"]
        (group {:orientation :vertical}
               (status-pill {:status :active})
               (status-pill {:status :warning :dot? true})
               (status-pill {:status :success :icon "check"}))])])

    (page-main
     (page-surface
      (page-demo-header
       "Custom layout page"
       "This page uses :layout directly instead of a built-in variant. The main story surface sits above a wide supporting region, while a left rail anchors navigation.")
      (toolbar {}
               (toolbar-start
                (button {:variant :outline :text "Refresh"})
                (button {:variant :outline :text "Save"}))
               (toolbar-end
                (button {:variant :primary :text "Create"})))
      [:section {:class "section-theme"}
       [:div {:class "stack-theme"}
        [:h2 {:class "font-heading text-2xl-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
         "Lead story"]
        [:p {:class "font-body text-base-theme leading-body"
             :style {:color "var(--muted-foreground)"}}
         "The main region spans two columns on large screens, giving it enough room to feel like a real feature area rather than another small card."]
        [:p {:class "font-body text-base-theme leading-body"}
         "Use :layout when the built-in page variants are close, but not quite the page you want. It is especially helpful for editorial or dashboard-like arrangements that are still made from the same left, main, and right regions."]]]))

    (page-right
     [:section {:class "section-theme"}
      [:div {:class "grid gap-panel md:grid-cols-2"}
       (plain-surface
        [:div {:class "stack-theme"}
         [:h3 {:class "font-heading text-lg-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
          "Supporting block A"]
         [:p {:class "font-body text-sm-theme leading-body"
              :style {:color "var(--muted-foreground)"}}
          "On large screens this wide lower band sits beneath the lead story and gives the page a more editorial feel."]])
       (plain-surface
        [:div {:class "stack-theme"}
         [:h3 {:class "font-heading text-lg-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}
          "Supporting block B"]
         [:p {:class "font-body text-sm-theme leading-body"
              :style {:color "var(--muted-foreground)"}}
          "At medium widths it drops below the main story, and on small screens it disappears so the page stays focused."]])]]))))
