(ns gessokit.humanhelp.components.request-card.attr)

(defn- compact-style
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn card-style
  [open?]
  (compact-style
   {:border-style "solid"
    :border-width "1px"
    :border-color (if open?
                    "color-mix(in srgb, var(--primary) 72%, var(--border))"
                    "color-mix(in srgb, var(--border) 72%, transparent)")
    :background "color-mix(in srgb, var(--background) 88%, black)"
    :color "var(--foreground)"
    :box-shadow (if open?
                  (str "0 0 0 3px color-mix(in srgb, var(--primary) 34%, transparent), "
                       "0 18px 44px color-mix(in srgb, black 34%, transparent)")
                  "0 10px 26px color-mix(in srgb, black 18%, transparent)") }))

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
  {:class "cursor-pointer w-full list-none flex items-start justify-between gap-inline outline-none"
   :data-humanhelp-request-summary true
   :style {:padding "1.75rem 1.75rem 1rem"
           :background "transparent"
           :color "var(--foreground)"
           :font-weight 500
           :box-shadow "none"}})

(defn header-stack-attrs
  []
  {:class "content-stack-theme min-w-0"
   :style {:gap "0.85rem"}})

(defn title-attrs
  []
  {:class "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme min-w-0"
   :style {:color "var(--foreground)"}})

(defn meta-attrs
  []
  {:class "cluster-theme items-center"
   :style {:gap "1rem"
           :color "var(--muted-foreground)"}})

(defn customer-row-attrs
  []
  {:class "cluster-theme items-center"
   :style {:color "var(--foreground)"}})

(defn chevron-attrs
  [open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:color "var(--foreground)"
           :opacity 0.9
           :transform (if open?
                        "rotate(180deg)"
                        "rotate(0deg)")}})

(defn details-attrs
  []
  {:class "content-stack-theme"
   :attrs {:style {:padding "0 1.75rem 1.75rem"
                   :background "transparent"
                   :border-top "0"
                   :color "var(--foreground)"}}})


(defn actions-attrs
  []
  {:class "cluster-theme items-center justify-end"
   :style {:padding-top "1rem"
           :border-top "0"}})

(defn action-form-attrs
  [{:keys [to attrs]}]
  (merge
   {:method "post"
    :hx-post to
    :hx-swap "none"
    :class "inline-flex"}
   attrs))
