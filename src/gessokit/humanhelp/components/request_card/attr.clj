(ns gessokit.humanhelp.components.request-card.attr)

(defn- compact-style
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- card-surface
  []
  "color-mix(in srgb, var(--card) 46%, var(--background))")

(defn card-style
  [open?]
  (compact-style
   {:border-style "solid"
    :border-width "1px"
    :border-color (if open?
                    "color-mix(in srgb, var(--primary) 48%, var(--border))"
                    "color-mix(in srgb, var(--border) 82%, transparent)")
    :background (card-surface)
    :color "var(--foreground)"
    :box-shadow (if open?
                  (str "0 0 0 1px color-mix(in srgb, var(--primary) 18%, transparent), "
                       "0 10px 24px color-mix(in srgb, black 22%, transparent)")
                  "0 6px 18px color-mix(in srgb, black 14%, transparent)")}))

(defn item-class
  []
  "radius-xl transition-all overflow-hidden")

(defn item-attrs
  [request open?]
  {:id (str "humanhelp-request-" (:request/id request))
   :data-humanhelp-request-card true
   :style (card-style open?)})

(defn summary-attrs
  []
  {:class "cursor-pointer w-full list-none pad-row flex items-start justify-between gap-inline outline-none"
   :data-humanhelp-request-summary true
   :style {:background "transparent"
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
   :style {:color "var(--muted-foreground)"}})

(defn chevron-attrs
  [open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:color "var(--muted-foreground)"
           :transform (if open?
                        "rotate(180deg)"
                        "rotate(0deg)")}})

(defn details-attrs
  []
  {:class "content-stack-theme pad-row"
   :style {:background "transparent"
           :border-top "1px solid color-mix(in srgb, var(--border) 72%, transparent)"}})

(defn actions-attrs
  []
  {:class "cluster-theme items-center justify-end"
   :style {:padding-top "var(--space-field)"
           :border-top "1px solid color-mix(in srgb, var(--border) 58%, transparent)"}})

(defn action-form-attrs
  [{:keys [to attrs]}]
  (merge
   {:method "post"
    :hx-post to
    :hx-swap "none"
    :class "inline-flex"}
   attrs))
