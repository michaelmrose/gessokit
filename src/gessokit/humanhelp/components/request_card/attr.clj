(ns gessokit.humanhelp.components.request-card.attr)

(defn- compact-style
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn card-surface
  []
  "color-mix(in srgb, var(--background) 82%, var(--card) 18%)")

(defn card-style
  []
  (compact-style
   {:position "relative"
    :border-style "solid"
    :border-width "1px"
    :border-color "color-mix(in srgb, var(--border) 80%, transparent)"
    :background (card-surface)
    :color "var(--foreground)"
    :box-shadow "var(--shadow-sm)"}))

(defn item-class
  []
  "radius-xl border-theme transition-all")

(defn item-attrs
  [request _open?]
  {:id (str "humanhelp-request-" (:request/id request))
   :data-humanhelp-request-card true
   :style (card-style)})

(defn summary-attrs
  []
  {:class "cursor-pointer w-full list-none flex items-start justify-between gap-inline outline-none"
   :data-humanhelp-request-summary true
   :style {:padding "1.25rem 1.25rem 0.75rem"
           :background "transparent"
           :color "var(--foreground)"
           :font-weight 500
           :box-shadow "none"}})

(defn header-stack-attrs
  []
  {:class "content-stack-theme gap-field min-w-0"})

(defn title-attrs
  []
  {:class "font-heading text-md-theme leading-heading tracking-heading weight-semibold-theme min-w-0"
   :style {:color "var(--foreground)"}})

(defn meta-attrs
  []
  {:class "cluster-theme items-center"
   :style {:color "var(--muted-foreground)"}})

(defn customer-row-attrs
  []
  {:class "cluster-theme items-center"
   :style {:color "var(--foreground)"}})

(defn chevron-attrs
  [_open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:color "var(--muted-foreground)"
           :opacity "0.9"
           :transform "rotate(0deg)"}})

(defn details-attrs
  []
  {:class "content-stack-theme"
   :attrs {:style {:padding "0 1.25rem 1.25rem"
                   :background "transparent"
                   :border-top "0"
                   :color "var(--foreground)"}}})

(defn actions-attrs
  []
  {:class "cluster-theme items-center justify-end"
   :style {:padding-top "0.875rem"
           :background "transparent"
           :border-top "0"}})

(defn action-form-attrs
  [{:keys [to attrs]}]
  (merge
   {:method "post"
    :hx-post to
    :hx-swap "none"
    :class "inline-flex"}
   attrs))
