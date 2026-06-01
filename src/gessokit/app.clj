(ns gessokit.app

  (:require
   [cheshire.core :as cheshire]
   [com.biffweb :as biff]
   [com.biffweb.experimental :as biffx]
   [gessokit.middleware :as mid]
   [gessokit.settings :as settings]
   [gessokit.ui :as ui]
   [ring.websocket :as ws]
   [rum.core :as rum]
   [tick.core :as tick]
   [gesso.core :as gs :refer :all ]
   ))

(defn- section-heading
  [title description]
  [:div {:class "text-center space-y-2"}
   [:h2 {:class "font-heading leading-heading tracking-heading text-2xl font-semibold"}
    title]
   [:p {:class "font-body leading-body text-muted-foreground"} description]])



(defn app [ctx]
  (ui/page
   ctx
   [:div {:class "gap-section space-y-14 font-body leading-body"}
    (scroll-buffer {:size :lg})
    ]))



(def about-page
  (ui/page
   {:base/title (str "About " settings/app-name)}
   [:p {:class "font-body leading-body"}
    "This app was made with "
    [:a.link {:href "https://biffweb.com"} "Biff"] "."]))

(defn echo [{:keys [params]}]
  {:status 200
   :headers {"content-type" "application/json"}
   :body params})

(def module
  {:static {"/about/" about-page}
   :routes [["/app" {:middleware [mid/wrap-signed-in]} ["" {:get app}]]]
   :api-routes [["/api/echo" {:post echo}]]})
