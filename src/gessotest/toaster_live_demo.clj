(ns gessotest.toaster-live-demo
  "Tiny demo of app-owned client plumbing + Gesso toaster.

   This namespace owns the toast-specific demo behavior. The shared browser
   client plumbing lives in gessotest.client-plumbing."
  (:require
   [gesso.core :as g]
   [gessotest.client-plumbing :as plumbing]))

(defn toast-oob
  [toast]
  (g/render-toast-oob
   (merge {:variant :info
           :title "Live event"
           :description "This toast was sent through app client plumbing."}
          toast)))

(defn send-toast-to-client!
  [client-id toast]
  (merge
   (plumbing/send-oob-to-client! client-id (toast-oob toast))
   {:toast toast}))

(defn send-toast-to-latest-client!
  [toast]
  (if-let [client-id (plumbing/latest-client-id)]
    (send-toast-to-client! client-id toast)
    {:sent 0
     :woke? false
     :error :no-connected-clients}))

(defn broadcast-toast!
  [toast]
  (merge
   (plumbing/broadcast-oob! (toast-oob toast))
   {:toast toast}))

(defn send-sample-toast!
  []
  (send-toast-to-latest-client!
   {:variant :success
    :title "Hello from toaster demo"
    :description "SSE woke HTMX, HTMX fetched pending OOB HTML, and the toaster appended it."
    :duration 5000}))

(defn send-warning-toast!
  []
  (send-toast-to-latest-client!
   {:variant :warning
    :title "External event"
    :description "This warning was sent through the toaster demo."}))

(defn send-danger-toast!
  []
  (send-toast-to-latest-client!
   {:variant :danger
    :title "Server-side problem"
    :description "This danger toast was sent through the toaster demo."}))

(defn section
  []
  (let [client-id (plumbing/new-client-id)]
    [:section {:class "panel-theme radius-lg pad-card content-stack-theme"}
     [:div {:class "title-stack-theme"}
      [:h2 {:class "font-heading text-xl-theme leading-heading tracking-heading weight-semibold-theme"}
       "Live toaster"]

      [:p {:class "font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "This section uses shared app client plumbing to connect one browser client over SSE and send server-triggered toasts into the page-shell toaster."]

      [:p {:class "font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "Client id: "
       [:code client-id]]]

     (plumbing/listener client-id)

     [:div {:class "content-stack-theme"}
      [:p {:class "font-body text-sm-theme leading-body"}
       "REPL examples:"]

      [:pre {:class "panel-theme radius-md pad-card overflow-auto text-sm-theme"}
       "(require '[gessotest.toaster-live-demo :as toaster-live])\n"
       "(require '[gessotest.client-plumbing :as plumbing])\n\n"
       "(plumbing/state-summary)\n"
       "(toaster-live/send-sample-toast!)\n\n"
       "(toaster-live/send-toast-to-client!\n"
       "  \"" client-id "\"\n"
       "  {:variant :warning\n"
       "   :title \"Targeted SSE toast\"\n"
       "   :description \"Only this connected client should receive this.\"})\n\n"
       "(toaster-live/broadcast-toast!\n"
       "  {:variant :info\n"
       "   :title \"Broadcast\"\n"
       "   :description \"Every connected client receives this.\"})"]]]))
