(ns gessokit.humanhelp.components.request-card.attr)

(defn- compact-style
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn selected-ring-color
  []
  ;; In cosmicnight:
  ;;   --primary / --ring = purple
  ;;   --accent = muted violet surface
  ;;   --chart-4 = cyan/teal
  "var(--chart-4)")

(defn card-surface
  []
  ;; Darker than --card, but still theme-derived.
  ;; In dark cosmicnight, --card is too light/violet for these request rows.
  "color-mix(in srgb, var(--background) 82%, var(--card) 18%)")

(defn card-border-color
  [open?]
  (if open?
    (selected-ring-color)
    "color-mix(in srgb, var(--border) 80%, transparent)"))

(defn selected-card-shadow
  []
  (let [ring (selected-ring-color)]
    (str
     ;; visible even if parent clips outside shadows
     "inset 0 0 0 1px color-mix(in srgb, " ring " 96%, white 4%), "
     "inset 0 0 0 4px color-mix(in srgb, " ring " 24%, transparent), "

     ;; subtle outside glow, nice when not clipped
     "0 0 0 1px color-mix(in srgb, " ring " 70%, transparent), "
     "0 0 22px color-mix(in srgb, " ring " 30%, transparent), "

     "var(--shadow-lg)")))

(defn card-style
  [open?]
  (compact-style
   {:border-style "solid"
    :border-width (if open? "2px" "1px")
    :border-color (card-border-color open?)
    :background (card-surface)
    :color "var(--foreground)"
    :box-shadow (if open?
                  (selected-card-shadow)
                  "var(--shadow-sm)")}))

(defn item-class
  []
  ;; Keep border-theme/radius, but do not use overflow-hidden here.
  ;; The selected ring is mostly inset, but clipping still makes the whole card
  ;; feel flatter.
  "radius-xl border-theme transition-all")

(defn item-attrs
  [request open?]
  {:id (str "humanhelp-request-" (:request/id request))
   :data-humanhelp-request-card true
   :data-humanhelp-request-selected (when open? "true")
   :style (card-style open?)})

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
  [open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:color (if open?
                    (selected-ring-color)
                    "var(--muted-foreground)")
           :opacity (if open? "1" "0.9")
           :transform (if open?
                        "rotate(180deg)"
                        "rotate(0deg)")}})

(defn details-attrs
  []
  ;; g/accordion-content needs raw HTML attrs under :attrs.
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
