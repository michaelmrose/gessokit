(ns gessokit.humanhelp.components.request-card.attr)

(defn card-style
  [open?]
  {:border-style "solid"
   :border-color (if open?
                   "var(--primary)"
                   "var(--border)")
   :background "var(--background)"
   :color "var(--foreground)"
   :box-shadow (when open?
                 "0 0 0 3px color-mix(in srgb, var(--primary) 24%, transparent)")})

(defn item-class
  []
  "radius-xl border-theme transition-all")

(defn item-attrs
  [request open?]
  {:id (str "humanhelp-request-" (:request/id request))
   :data-humanhelp-request-card true
   :style (card-style open?)})

(defn summary-attrs
  []
  {:class "cursor-pointer w-full list-none pad-row flex items-start justify-between gap-inline outline-none"
   :data-humanhelp-request-summary true
   :style {:background "var(--secondary)"
           :color "var(--primary)"
           :font-weight 600
           :box-shadow "inset 0 -1px 0 0 color-mix(in srgb, var(--foreground) 16%, transparent)"}})

(defn header-stack-attrs
  []
  {:class "content-stack-theme gap-field min-w-0"})

(defn title-attrs
  []
  {:class "font-heading text-md-theme leading-heading tracking-heading weight-semibold-theme min-w-0"})

(defn meta-attrs
  []
  {:class "cluster-theme items-center"})

(defn customer-row-attrs
  []
  {:class "cluster-theme items-center"})

(defn chevron-attrs
  [open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:transform (if open?
                        "rotate(180deg)"
                        "rotate(0deg)")}})

(defn details-stack-class
  []
  "content-stack-theme")

(defn actions-attrs
  []
  {:class "cluster-theme items-center justify-end"})

(defn action-form-attrs
  [{:keys [to attrs]}]
  (merge
   {:method "post"
    :hx-post to
    :hx-swap "none"
    :class "inline-flex"}
   attrs))
