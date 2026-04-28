(ns gessotest.ui
  (:require [clojure.java.io :as io]
            [gessotest.settings :as settings]
            [com.biffweb :as biff]
            [ring.util.response :as ring-response]
            [rum.core :as rum]
            [gesso.core :refer :all]))

(def default-theme
  {:color-theme "cosmicnight"
   :density "default"
   :typography "ui"
   :shape "default"})

(def default-mode :dark)

(def ^:private axis-specs
  [{:axis :color-theme
    :attr "data-color-theme"
    :label "Color"}

   {:axis :density
    :attr "data-density"
    :label "Density"}

   {:axis :typography
    :attr "data-typography"
    :label "Typography"}

   {:axis :shape
    :attr "data-shape"
    :label "Shape"}])

(defn static-path [path]
  (if-some [last-modified (some-> (io/resource (str "public" path))
                                  ring-response/resource-data
                                  :last-modified
                                  (.getTime))]
    (str path "?t=" last-modified)
    path))

(defn- theme-css-resources []
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

(defn- discovered-theme-options []
  (let [css-blobs (map slurp (theme-css-resources))]
    (reduce
     (fn [m {:keys [axis attr]}]
       (let [discovered (->> css-blobs
                             (mapcat #(options-from-css % attr))
                             distinct
                             sort
                             vec)
             fallback   (some-> (get default-theme axis) vector)]
         (assoc m axis (or (not-empty discovered) fallback []))))
     {}
     axis-specs)))

(defn- theme-state
  [ctx]
  {:color-theme (or (:color-theme ctx) (:data-color-theme ctx) (:color-theme default-theme))
   :density (or (:density ctx) (:data-density ctx) (:density default-theme))
   :typography (or (:typography ctx) (:data-typography ctx) (:typography default-theme))
   :shape (or (:shape ctx) (:data-shape ctx) (:shape default-theme))
   :mode (or (:mode ctx) (:data-color-theme-mode ctx) default-mode)})

(defn- theme-select
  [{:keys [label attr options selected]}]
  [:label {:class "flex items-center gap-2 text-sm font-body leading-body"}
   [:span {:class "text-foreground/80"} label]
   [:select
    {:class "select control-theme rounded-lg border-theme bg-background text-foreground"
     :_ (str "on change set document.documentElement's @" attr " to my value")}
    (for [opt options]
      [:option
       (cond-> {:value opt}
         (= opt selected) (assoc :selected true))
       opt])]])

(defn- theme-testing-bar
  [{:keys [theme-options color-theme density typography shape]}]
  [:div {:class "w-full border-b border-border bg-card text-card-foreground"}
   [:div {:class "mx-auto flex w-full max-w-6xl flex-wrap items-center gap-3 px-4 py-3 sm:px-6 lg:px-8"}
    (button
     {:text "Toggle dark/light"
      :variant :outline
      :attrs
      {:type "button"
       :_ "on click
             if document.documentElement matches .dark
               remove .dark from document.documentElement
               set document.documentElement's @data-color-theme-mode to 'light'
             else
               add .dark to document.documentElement
               set document.documentElement's @data-color-theme-mode to 'dark'
             end"}})

    (theme-select
     {:label "Color"
      :attr "data-color-theme"
      :options (:color-theme theme-options)
      :selected color-theme})

    (theme-select
     {:label "Density"
      :attr "data-density"
      :options (:density theme-options)
      :selected density})

    (theme-select
     {:label "Typography"
      :attr "data-typography"
      :options (:typography theme-options)
      :selected typography})

    (theme-select
     {:label "Shape"
      :attr "data-shape"
      :options (:shape theme-options)
      :selected shape})]])

(defn base [{:keys [::recaptcha] :as ctx} & body]
  (let [{:keys [color-theme density typography shape mode]} (theme-state ctx)]
    (apply
     biff/base-html
     (-> ctx
         (merge
          (theme {:color-theme color-theme
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
  "The standard app layout shell.
   Includes a theme testing bar and centers the main content."
  [ctx & body]
  (let [theme-options (discovered-theme-options)]
    (base ctx
          [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
           (theme-testing-bar
            (merge default-theme
                   {:theme-options theme-options}))

           [:main {:class "flex-grow py-10"}
            (apply container body)]

           (toaster {:id "app-toaster"
                     :position :bottom-right})])))

(defn page-shell
  "Like page, but does not force the centered container.
   Reuses the existing theme testing bar and lets callers supply their own
   full-page layout."
  [ctx & body]
  (let [theme-options (discovered-theme-options)]
    (base ctx
          [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
           (theme-testing-bar
            (merge default-theme
                   {:theme-options theme-options}))

           (into [:main {:class "flex-grow py-10"}]
                 body)

           (toaster {:id "app-toaster"
                     :position :bottom-right})])))

(defn on-error [{:keys [status] :as ctx}]
  {:status status
   :headers {"content-type" "text/html"}
   :body (rum/render-static-markup
          (page
           ctx
           [:h1 {:class "font-heading text-2xl"}
            (if (= status 404)
              "Page not found."
              "Something went wrong.")]))})
