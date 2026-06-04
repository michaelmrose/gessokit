(ns gessokit.ui
  (:require
   [clojure.java.io :as io]
   [com.biffweb :as biff]
   [gesso.core :as g]
   [gessokit.settings :as settings]
   [ring.util.response :as ring-response]
   [rum.core :as rum]))

;; -----------------------------------------------------------------------------
;; Theme defaults
;; -----------------------------------------------------------------------------

(def default-theme
  {:color-theme "cosmicnight"
   :density "default"
   :typography "ui"
   :shape "default"})

(def default-mode
  :dark)

(def axis-specs
  [{:axis :color-theme
    :attr "data-color-theme"
    :label "Color"
    :description "Choose the app color palette."}

   {:axis :density
    :attr "data-density"
    :label "Density"
    :description "Adjust spacing, control size, and layout rhythm."}

   {:axis :typography
    :attr "data-typography"
    :label "Typography"
    :description "Choose the body and heading type system."}

   {:axis :shape
    :attr "data-shape"
    :label "Shape"
    :description "Adjust border radius and component softness."}])

;; -----------------------------------------------------------------------------
;; Static assets
;; -----------------------------------------------------------------------------

(defn static-path
  [path]
  (if-some [last-modified (some-> (io/resource (str "public" path))
                                  ring-response/resource-data
                                  :last-modified
                                  (.getTime))]
    (str path "?t=" last-modified)
    path))

;; -----------------------------------------------------------------------------
;; Theme discovery
;; -----------------------------------------------------------------------------

(defn- theme-css-resources
  []
  (keep io/resource
        ["public/gesso/themes.css"
         "public/gesso/app-themes.css"]))

(defn- options-from-css
  [css attr]
  (let [pattern (re-pattern
                 (str "html(?:\\.dark)?\\["
                      (java.util.regex.Pattern/quote attr)
                      "~=\"([^\"]+)\"\\]"))]
    (->> (re-seq pattern css)
         (map second)
         distinct
         sort
         vec)))

(defn discovered-theme-options
  "Discover available theme axis values from bundled/app-generated CSS.

   This preserves the useful behavior from the old theme testing bar: when new
   app themes are generated into public/gesso/app-themes.css, they appear in the
   UI without hand-editing Clojure."
  []
  (let [css-blobs (map slurp (theme-css-resources))]
    (reduce
     (fn [m {:keys [axis attr]}]
       (let [discovered (->> css-blobs
                             (mapcat #(options-from-css % attr))
                             distinct
                             sort
                             vec)
             fallback   (some-> (get default-theme axis) vector)]
         (assoc m axis (or (not-empty discovered)
                           fallback
                           []))))
     {}
     axis-specs)))

(defn theme-state
  "Return the server-rendered theme state for ctx.

   The theme dialog applies changes client-side to document.documentElement.
   These values are the initial render defaults."
  [ctx]
  {:color-theme (or (:color-theme ctx)
                    (:data-color-theme ctx)
                    (:color-theme default-theme))
   :density (or (:density ctx)
                (:data-density ctx)
                (:density default-theme))
   :typography (or (:typography ctx)
                   (:data-typography default-theme)
                   (:typography default-theme))
   :shape (or (:shape ctx)
              (:data-shape ctx)
              (:shape default-theme))
   :mode (or (:mode ctx)
             (:data-color-theme-mode ctx)
             default-mode)})

(defn- mode-token
  [mode]
  (cond
    (keyword? mode) (name mode)
    (nil? mode) (name default-mode)
    :else (str mode)))

(defn- select-option
  [selected opt]
  [:option
   (cond-> {:value opt}
     (= (str selected) (str opt))
     (assoc :selected true))
   opt])

(defn- theme-select-id
  [id-prefix axis]
  (str id-prefix "-" (name axis)))

(defn- theme-select
  [{:keys [axis attr label description options selected id-prefix]}]
  (let [id (theme-select-id id-prefix axis)]
    [:label {:class "content-stack-theme gap-field"}
     [:span {:class "font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
      label]

     [:span {:class "font-body text-xs-theme leading-body"
             :style {:color "var(--muted-foreground)"}}
      description]

     [:select
      {:id id
       :name (name axis)
       :class "control-theme radius-md border-theme font-body text-sm-theme"
       :style {:border-style "solid"
               :border-color "var(--border)"
               :background "var(--background)"
               :color "var(--foreground)"}
       :onchange (str "document.documentElement.setAttribute('"
                      attr
                      "', this.value)")}
      (for [opt options]
        (select-option selected opt))]]))

(defn- mode-select
  [{:keys [selected id-prefix]}]
  [:label {:class "content-stack-theme gap-field"}
   [:span {:class "font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
    "Mode"]

   [:span {:class "font-body text-xs-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
    "Switch between light, dark, or your browser preference."]

   [:select
    {:id (str id-prefix "-mode")
     :name "mode"
     :class "control-theme radius-md border-theme font-body text-sm-theme"
     :style {:border-style "solid"
             :border-color "var(--border)"
             :background "var(--background)"
             :color "var(--foreground)"}
     :onchange
     "var root = document.documentElement;
      root.setAttribute('data-color-theme-mode', this.value);
      if (this.value === 'dark') {
        root.classList.add('dark');
      } else {
        if (this.value === 'light') {
          root.classList.remove('dark');
        } else {
          if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
            root.classList.add('dark');
          } else {
            root.classList.remove('dark');
          }
        }
      }"}
    (for [opt ["dark" "light" "system"]]
      (select-option (mode-token selected) opt))]])

;; -----------------------------------------------------------------------------
;; Theme dialog
;; -----------------------------------------------------------------------------

(defn theme-dialog
  "Render a theme button plus modal dialog.

   Intended placement: the center area of the Human Help app bar.

   This keeps theme discovery in gessokit.ui, while app-specific navigation and
   bar layout stay in the app/view namespaces.

   Options:
     :trigger-label?
       Whether the trigger button includes the text label \"Theme\".

     :id
       Dialog id. Defaults to \"gessokit-theme-dialog\".

     :id-prefix
       Prefix for internal select ids. Defaults to :id."
  ([ctx]
   (theme-dialog ctx {}))
  ([ctx {:keys [trigger-label? id id-prefix]
         :or {trigger-label? true}}]
   (let [dialog-id     (or id "gessokit-theme-dialog")
         id-prefix'    (or id-prefix dialog-id)
         state         (theme-state ctx)
         theme-options (discovered-theme-options)]
     [:<>
      [:button
       {:type "button"
        :class "inline-flex items-center justify-center gap-inline control-theme radius-md border-theme font-body text-sm-theme weight-medium-theme"
        :style {:border-style "solid"
                :border-color "var(--border)"
                :background "var(--card)"
                :color "var(--card-foreground)"}
        :aria-label "Theme settings"
        :onclick (str "document.getElementById('"
                      dialog-id
                      "').showModal()")}
       (g/icon "palette" {:size :sm})
       (when trigger-label?
         [:span "Theme"])]

      [:dialog
       {:id dialog-id
        :class "radius-xl border-theme shadow-xl"
        :style {:border-style "solid"
                :border-color "var(--border)"
                :background "var(--card)"
                :color "var(--card-foreground)"
                :max-width "min(34rem, calc(100vw - 2rem))"
                :width "100%"
                :padding "0"}}
       [:div {:class "pad-card content-stack-theme"}
        [:div {:class "title-stack-theme"}
         [:h2 {:class "font-heading text-2xl-theme leading-heading tracking-heading weight-bold-theme"}
          "Theme"]

         [:p {:class "font-body text-sm-theme leading-body"
              :style {:color "var(--muted-foreground)"}}
          "Explore the generated Gesso theme dimensions. Changes apply to this page immediately."]]

        [:div {:class "form-theme"}
         (for [{:keys [axis attr label description] :as spec} axis-specs]
           (theme-select
            (assoc spec
                   :axis axis
                   :attr attr
                   :label label
                   :description description
                   :id-prefix id-prefix'
                   :options (get theme-options axis)
                   :selected (get state axis))))

         (mode-select
          {:selected (:mode state)
           :id-prefix id-prefix'})]

        [:div {:class "cluster-theme justify-end"}
         [:form {:method "dialog"}
          (g/button
           {:variant :outline
            :text "Done"
            :attrs {:type "submit"}})]]]]])))

;; -----------------------------------------------------------------------------
;; Base page shell
;; -----------------------------------------------------------------------------

(defn base
  [{:keys [::recaptcha] :as ctx} & body]
  (let [{:keys [color-theme density typography shape mode]} (theme-state ctx)]
    (apply
     biff/base-html
     (-> ctx
         (merge
          (g/theme {:color-theme color-theme
                    :density density
                    :typography typography
                    :shape shape}
                   mode))
         (update :base/head
                 (fn [head]
                   (concat
                    head
                    [[:script {:src (static-path "/js/gesso-theme.js")
                               :defer true}]
                     [:link {:rel "stylesheet"
                             :href (static-path "/css/main.css")}]
                     [:link {:rel "stylesheet"
                             :href "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.11/dist/basecoat.cdn.min.css"}]
                     [:link {:rel "stylesheet"
                             :href (static-path "/gesso/themes.css")}]
                     (when (io/resource "public/gesso/app-themes.css")
                       [:link {:rel "stylesheet"
                               :href (static-path "/gesso/app-themes.css")}])
                     [:link {:rel "icon"
                             :href "/favicon.ico"
                             :sizes "any"}]

                     [:script {:src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.11/dist/js/all.min.js"
                               :defer true}]
                     [:script {:src (static-path "/js/main.js")
                               :defer true}]
                     [:script {:src "https://unpkg.com/htmx.org@2.0.7"}]
                     [:script {:src "https://cdn.jsdelivr.net/npm/htmx-ext-sse@2.2.4"}]
                     [:script {:src "https://unpkg.com/htmx-ext-ws@2.0.2/ws.js"}]
                     [:script {:src "https://unpkg.com/hyperscript.org@0.9.14"}]
                     (when recaptcha
                       [:script {:src "https://www.google.com/recaptcha/api.js"
                                 :async "async"
                                 :defer "defer"}])])))
         (merge
          #:base{:title settings/app-name
                 :lang "en-US"
                 :icon "/img/glider.png"
                 :description (str settings/app-name " Description")
                 :image "https://clojure.org/images/clojure-logo-120b.png"}))
     body)))

(defn container
  [& children]
  (into [:div {:class "w-full max-w-4xl mx-auto px-4 sm:px-6 lg:px-8"}]
        children))

(defn page
  "Centered standard page shell.

   This does not render the old always-visible theme testing bar. App pages that
   want theme controls should place (theme-dialog ctx) where it belongs,
   usually in the app bar."
  [ctx & body]
  (base ctx
        [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
         [:main {:class "flex-grow py-10"}
          (apply container body)]

         (g/toaster {:id "app-toaster"
                     :position :bottom-right})]))

(defn page-shell
  "Full-width app shell.

   Use this for application layouts that render their own bars, page grids, and
   content surfaces."
  [ctx & body]
  (base ctx
        [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
         (into [:main {:class "flex-grow"}]
               body)

         (g/toaster {:id "app-toaster"
                     :position :bottom-right})]))

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn on-error
  [{:keys [status] :as ctx}]
  {:status status
   :headers {"content-type" "text/html"}
   :body (rum/render-static-markup
          (page
           ctx
           [:h1 {:class "font-heading text-2xl-theme leading-heading tracking-heading weight-semibold-theme"}
            (if (= status 404)
              "Page not found."
              "Something went wrong.")]))})
