(ns gessokit.humanhelp.components.request-card.attr)

(defn card-style
  [selected?]
  {:border-style "solid"
   :border-color (if selected?
                   "var(--primary)"
                   "var(--border)")
   :background "var(--background)"
   :color "var(--foreground)"
   :box-shadow (when selected?
                 "0 0 0 3px color-mix(in srgb, var(--primary) 24%, transparent)")})

(defn card-attrs
  [request selected?]
  {:id (str "humanhelp-request-" (:request/id request))
   :data-humanhelp-request-card true
   :class "radius-xl border-theme pad-card content-stack-theme transition-all"
   :style (card-style selected?)})

(defn selection-link-attrs
  [{:keys [href request-list-dom-id]}]
  {:href href
   :hx-get href
   :hx-target (str "#" request-list-dom-id)
   :hx-swap "outerHTML"
   :class "block content-stack-theme"
   :style {:color "inherit"
           :text-decoration "none"}})

(defn header-row-attrs
  []
  {:class "cluster-theme items-start justify-between"})

(defn title-stack-attrs
  []
  {:class "content-stack-theme gap-field"})

(defn title-attrs
  []
  {:class "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"})

(defn meta-attrs
  []
  {:class "cluster-theme items-center"})

(defn meta-text-attrs
  []
  {:class "font-body text-xs-theme"
   :style {:color "var(--muted-foreground)"}})

(defn customer-row-attrs
  []
  {:class "cluster-theme items-center"})

(defn customer-name-attrs
  []
  {:class "font-body text-sm-theme leading-body weight-medium-theme"})

(defn claimed-by-attrs
  []
  {:class "font-body text-xs-theme leading-body"
   :style {:color "var(--muted-foreground)"}})

(defn details-stack-attrs
  []
  {:class "content-stack-theme"})

(defn details-text-attrs
  []
  {:class "font-body text-sm-theme leading-body"})

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
