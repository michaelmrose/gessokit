(ns gessotest.toaster-demo
  (:require
   [gesso.core :as g]
   [gessotest.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/demo/toasts")

(def success-path
  (str base-path "/success"))

(def info-path
  (str base-path "/info"))

(def warning-path
  (str base-path "/warning"))

(def danger-path
  (str base-path "/danger"))

(def persistent-path
  (str base-path "/persistent"))

;; -----------------------------------------------------------------------------
;; Demo section
;; -----------------------------------------------------------------------------

(defn section
  []
  [:section {:class "panel-theme radius-lg pad-card content-stack-theme"}
   [:div {:class "title-stack-theme"}
    [:h2 {:class "font-heading text-xl-theme leading-heading tracking-heading weight-semibold-theme"}
     "Toaster"]

    [:p {:class "font-body text-sm-theme leading-body"
         :style {:color "var(--muted-foreground)"}}
     "Server-rendered HTMX responses append toast notifications into the page-shell toaster."]]

   [:div {:class "cluster-theme"}
    [:button {:type "button"
              :class "btn-primary"
              :hx-get success-path
              :hx-swap "none"}
     "Success toast"]

    [:button {:type "button"
              :class "btn-outline"
              :hx-get info-path
              :hx-swap "none"}
     "Info toast"]

    [:button {:type "button"
              :class "btn-outline"
              :hx-get warning-path
              :hx-swap "none"}
     "Warning toast"]

    [:button {:type "button"
              :class "btn-outline"
              :hx-get danger-path
              :hx-swap "none"}
     "Danger toast"]

    [:button {:type "button"
              :class "btn-outline"
              :hx-get persistent-path
              :hx-swap "none"}
     "Persistent toast"]]])

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn success
  [_ctx]
  (g/html-response
   (g/render-toast-oob
    {:variant :success
     :title "Saved"
     :description "This success toast auto-dismisses after four seconds."
     :duration 4000})))

(defn info
  [_ctx]
  (g/html-response
   (g/render-toast-oob
    {:variant :info
     :title "New event"
     :description "This is a server-rendered informational toast."
     :duration 5000})))

(defn warning
  [_ctx]
  (g/html-response
   (g/render-toast-oob
    {:variant :warning
     :title "Check this"
     :description "Warnings are persistent by default in this demo."})))

(defn danger
  [_ctx]
  (g/html-response
   (g/render-toast-oob
    {:variant :danger
     :title "Could not submit"
     :description "Danger toasts should usually remain visible until dismissed."})))

(defn persistent
  [_ctx]
  (g/html-response
   (g/render-toast-oob
    {:variant :default
     :title "Persistent toast"
     :description "This toast has no duration. It stays until manually dismissed."})))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["/success" {:get success}]
     ["/info" {:get info}]
     ["/warning" {:get warning}]
     ["/danger" {:get danger}]
     ["/persistent" {:get persistent}]]]})
