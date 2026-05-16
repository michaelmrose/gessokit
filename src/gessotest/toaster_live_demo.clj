(ns gessotest.toaster-live-demo
  "Tiny demo of app-owned client plumbing + Gesso toaster.

   This namespace owns only toast-specific demo behavior.

   The shared browser-client delivery adapter lives in
   gessotest.client-plumbing. That adapter owns app policy such as routes,
   middleware, current-user-id, and client scopes, while gesso.live.client owns
   the generic connected-client delivery mechanics."
  (:require
   [gesso.core :as g]
   [gessotest.client-plumbing :as plumbing]))

;; -----------------------------------------------------------------------------
;; Toast OOB helpers
;; -----------------------------------------------------------------------------

(def default-toast
  {:variant :info
   :title "Live event"
   :description "This toast was sent through app client plumbing."})

(defn normalize-toast
  [toast]
  (merge default-toast toast))

(defn toast-oob
  "Render one normalized toast as an HTMX OOB fragment."
  [toast]
  (g/render-toast-oob
   (normalize-toast toast)))

;; -----------------------------------------------------------------------------
;; Demo send helpers
;; -----------------------------------------------------------------------------

(defn send-toast-to-client!
  "Send one toast to a specific connected browser client."
  [client-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (plumbing/send-to-client! client-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-latest-client!
  "Send one toast to the most recently connected browser client."
  [toast]
  (if-let [client-id (plumbing/latest-client-id)]
    (send-toast-to-client! client-id toast)
    {:sent 0
     :woke 0
     :woke? false
     :error :no-connected-clients
     :toast (normalize-toast toast)}))

(defn send-toast-to-user!
  "Send one toast to all connected browser clients for user-id."
  [user-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (plumbing/send-to-user! user-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-scope!
  "Send one toast to all connected browser clients in scope."
  [scope toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (plumbing/send-to-scope! scope (toast-oob toast'))
     {:toast toast'})))

(defn broadcast-toast!
  "Broadcast one toast to every connected browser client.

   This should stay explicit and rare."
  [toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (plumbing/broadcast! (toast-oob toast'))
     {:toast toast'})))

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

(defn send-scope-toast!
  []
  (send-toast-to-scope!
   [:demo :gessotest]
   {:variant :info
    :title "Scoped toast"
    :description "Every connected client registered in [:demo :gessotest] should receive this."}))

;; -----------------------------------------------------------------------------
;; Demo UI
;; -----------------------------------------------------------------------------

(defn repl-examples
  [client-id]
  [:pre {:class "panel-theme radius-md pad-card overflow-auto text-sm-theme"}
   "(require '[gessotest.toaster-live-demo :as toaster-live])\n"
   "(require '[gessotest.client-plumbing :as plumbing])\n\n"

   ";; Inspect connected clients and pending OOB queues\n"
   "(plumbing/state-summary)\n\n"

   ";; Send to the most recently connected client\n"
   "(toaster-live/send-sample-toast!)\n\n"

   ";; Send directly to this displayed client\n"
   "(toaster-live/send-toast-to-client!\n"
   "  \"" client-id "\"\n"
   "  {:variant :warning\n"
   "   :title \"Targeted SSE toast\"\n"
   "   :description \"Only this connected client should receive this.\"})\n\n"

   ";; Send to the demo scope registered by gessotest.client-plumbing\n"
   "(toaster-live/send-toast-to-scope!\n"
   "  [:demo :gessotest]\n"
   "  {:variant :info\n"
   "   :title \"Scoped toast\"\n"
   "   :description \"Every demo-scoped client receives this.\"})\n\n"

   ";; Broadcast to every connected client\n"
   "(toaster-live/broadcast-toast!\n"
   "  {:variant :info\n"
   "   :title \"Broadcast\"\n"
   "   :description \"Every connected client receives this.\"})"])

(defn section
  "Render the live toaster demo.

   The zero-arity form is kept for existing call sites. Prefer passing ctx when
   available so this mirrors normal app usage."
  ([]
   (section nil))
  ([ctx]
   (let [client-id (plumbing/new-client-id)]
     [:section {:class "panel-theme radius-lg pad-card content-stack-theme"}
      [:div {:class "title-stack-theme"}
       [:h2 {:class "font-heading text-xl-theme leading-heading tracking-heading weight-semibold-theme"}
        "Live toaster"]

       [:p {:class "font-body text-sm-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
        "This section connects one browser client over SSE, then uses app-owned client plumbing to send server-triggered OOB toast fragments into the page-shell toaster."]

       [:p {:class "font-body text-sm-theme leading-body"
            :style {:color "var(--muted-foreground)"}}
        "Client id: "
        [:code client-id]]]

      ;; Important: pass both ctx and the displayed client id.
      ;; Calling (plumbing/listener client-id) would treat client-id as ctx and
      ;; generate a different connected client id.
      (plumbing/listener ctx client-id)

      [:div {:class "content-stack-theme"}
       [:p {:class "font-body text-sm-theme leading-body"}
        "REPL examples:"]

       (repl-examples client-id)]])))
