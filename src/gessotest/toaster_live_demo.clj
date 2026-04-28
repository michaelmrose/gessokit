(ns gessotest.toaster-live-demo
  (:require
   [clojure.java.io :as io]
   [gesso.core :as g]
   [gessotest.middleware :as mid]
   [ring.core.protocols :as ring-protocols])
  (:import
   [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(def base-path
  "/app/demo/live-toasts")

(def stream-path
  (str base-path "/stream"))

(def pending-path
  (str base-path "/pending"))

;; -----------------------------------------------------------------------------
;; In-memory demo state
;; -----------------------------------------------------------------------------

;; Demo-only process-local subscriber registry.
;;
;; Shape:
;;   client-id -> {:id client-id
;;                 :queue LinkedBlockingQueue
;;                 :connected-at millis}
;;
;; This proves the browser/SSE/toaster pipeline. Production notification delivery
;; should key by authenticated user/session/scope and should usually derive
;; important notifications from XTDB writes.
(defonce subscribers
  (atom {}))

;; Demo-only process-local pending toast storage.
;;
;; Shape:
;;   client-id -> [toast toast ...]
;;
;; This prevents one client from stealing another client's toast and prevents
;; accidental broadcast. Production notification history should be backed by
;; durable application state, usually XTDB.
(defonce pending-toasts
  (atom {}))

;; Defonce shape guards for REPL-driven iteration while this demo evolves.
(when-not (map? @subscribers)
  (reset! subscribers {}))

(when-not (map? @pending-toasts)
  (reset! pending-toasts {}))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn- client-id-from-ctx
  [{:keys [params]}]
  (or (request-param params :client-id)
      (str (random-uuid))))

(defn- add-subscriber!
  [client-id queue]
  (swap! subscribers assoc client-id
         {:id client-id
          :queue queue
          :connected-at (now-ms)}))

(defn- remove-subscriber!
  [client-id queue]
  (swap! subscribers
         (fn [m]
           ;; Avoid removing a newer connection that reused the same client id.
           (if (= queue (get-in m [client-id :queue]))
             (dissoc m client-id)
             m))))

(defn connected-clients
  "Return currently connected demo client ids.

   Useful from the REPL:

     (gessotest.toaster-live-demo/connected-clients)"
  []
  (->> @subscribers
       vals
       (sort-by :connected-at)
       (map :id)
       vec))

(defn connected-client-summaries
  "Return connected demo clients with connection metadata."
  []
  (->> @subscribers
       vals
       (sort-by :connected-at)
       vec))

(defn latest-client-id
  "Return the most recently connected demo client id."
  []
  (->> @subscribers
       vals
       (sort-by :connected-at)
       last
       :id))

(defn pending-counts
  "Return pending toast counts by client id.

   Useful from the REPL:

     (gessotest.toaster-live-demo/pending-counts)"
  []
  (->> @pending-toasts
       (map (fn [[client-id toasts]]
              [client-id (count toasts)]))
       (into {})))

(defn subscriber-debug
  "Return a compact view of connected clients and pending toast counts."
  []
  {:connected (connected-clients)
   :pending   (pending-counts)})

;; -----------------------------------------------------------------------------
;; SSE helpers
;; -----------------------------------------------------------------------------

(defn- write-sse-event!
  [writer event-name data]
  (.write writer (str "event: " event-name "\n"))
  (.write writer (str "data: " data "\n\n"))
  (.flush writer))

(defn- write-sse-comment!
  [writer comment]
  (.write writer (str ": " comment "\n\n"))
  (.flush writer))

(defn- stream-body
  [client-id queue]
  (reify ring-protocols/StreamableResponseBody
    (write-body-to-stream [_body _response output-stream]
      (with-open [writer (io/writer output-stream)]
        (try
          (write-sse-event! writer "open" "{}")

          (loop []
            (let [event (.poll queue 15 TimeUnit/SECONDS)]
              (if event
                (write-sse-event! writer (:event event) "{}")
                (write-sse-comment! writer "keepalive"))
              (recur)))

          (catch Exception _e
            ;; Usually client disconnect / broken pipe.
            nil)

          (finally
            (remove-subscriber! client-id queue)))))))

(defn stream
  "SSE endpoint.

   Sends tiny wakeup events. The browser reacts to `sse:toast` by fetching
   pending toast HTML for this specific client id.

   Uses Ring's StreamableResponseBody so tiny SSE messages are flushed directly
   to the HTTP output stream."
  [ctx]
  (let [client-id (client-id-from-ctx ctx)
        queue     (LinkedBlockingQueue.)]
    (add-subscriber! client-id queue)
    {:status 200
     :headers {"content-type" "text/event-stream; charset=utf-8"
               "cache-control" "no-cache, no-transform"
               "connection" "keep-alive"
               "x-accel-buffering" "no"}
     :body (stream-body client-id queue)}))

;; -----------------------------------------------------------------------------
;; Pending toast rendering
;; -----------------------------------------------------------------------------

(defn- enqueue-pending-toast!
  [client-id toast]
  (swap! pending-toasts update client-id (fnil conj []) toast))

(defn- drain-pending-toasts!
  [client-id]
  (loop []
    (let [m      @pending-toasts
          toasts (get m client-id [])]
      (if (compare-and-set! pending-toasts m (dissoc m client-id))
        toasts
        (recur)))))

(defn- render-toasts-oob
  [toasts]
  (into
   [:div {:id "app-toaster"
          :hx-swap-oob "beforeend"}]
   (map g/toast)
   toasts))

(defn pending
  "HTMX fetch endpoint triggered by `sse:toast`.

   Returns an OOB append containing only this client's pending demo toasts."
  [ctx]
  (let [client-id (client-id-from-ctx ctx)
        toasts    (drain-pending-toasts! client-id)]
    (if (seq toasts)
      (g/html-response (render-toasts-oob toasts))
      (g/no-content))))

;; -----------------------------------------------------------------------------
;; Browser hook
;; -----------------------------------------------------------------------------

(defn listener
  "Browser-side SSE listener.

   The SSE event is a wakeup:

     event: toast
     data: {}

   The parent owns the SSE connection. The child owns the HTMX callback
   triggered by sse:toast.

   Requires:
     - the HTMX SSE extension is loaded
     - (g/toaster {:id \"app-toaster\"}) exists on the page"
  [client-id]
  [:div {:id (str "live-toast-listener-" client-id)
         :data-live-toast-listener true
         :hx-ext "sse"
         :sse-connect (str stream-path "?client-id=" client-id)
         :aria-hidden "true"}
   [:div {:data-live-toast-trigger true
          :hx-get (str pending-path "?client-id=" client-id)
          :hx-trigger "sse:toast"
          :hx-swap "none"}]])

(defn section
  []
  (let [client-id (str (random-uuid))]
    [:section {:class "panel-theme radius-lg pad-card content-stack-theme"}
     [:div {:class "title-stack-theme"}
      [:h2 {:class "font-heading text-xl-theme leading-heading tracking-heading weight-semibold-theme"}
       "Live toaster"]

      [:p {:class "font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "This section connects to an SSE stream. Trigger a toast from the REPL; the SSE event wakes HTMX, which fetches this client’s pending OOB toast HTML."]

      [:p {:class "font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "Client id: "
       [:code client-id]]]

     (listener client-id)

     [:div {:class "content-stack-theme"}
      [:p {:class "font-body text-sm-theme leading-body"}
       "REPL examples:"]

      [:pre {:class "panel-theme radius-md pad-card overflow-auto text-sm-theme"}
       "(require '[gessotest.toaster-live-demo :as live-toasts])\n\n"
       "(live-toasts/connected-clients)\n"
       "(live-toasts/pending-counts)\n"
       "(live-toasts/subscriber-debug)\n\n"
       "(live-toasts/publish-sample!)\n\n"
       "(live-toasts/publish-toast-to!\n"
       "  \"" client-id "\"\n"
       "  {:variant :warning\n"
       "   :title \"Targeted SSE toast\"\n"
       "   :description \"Only this connected client should receive this.\"})"]]]))

;; -----------------------------------------------------------------------------
;; REPL API
;; -----------------------------------------------------------------------------

(defn- wake-client!
  [client-id]
  (if-let [queue (get-in @subscribers [client-id :queue])]
    (do
      (.offer queue {:event "toast"})
      true)
    false))

(defn publish-toast-to!
  "Publish one toast to one connected demo client.

   Example:

     (publish-toast-to!
      \"client-id-from-page\"
      {:variant :success
       :title \"Hello\"
       :description \"This went to one browser tab.\"
       :duration 5000})"
  [client-id toast]
  (if (get @subscribers client-id)
    (let [toast' (merge {:variant :info
                         :title "Live event"
                         :description "This toast was triggered from the REPL."}
                        toast)
          _      (enqueue-pending-toast! client-id toast')
          woke?  (wake-client! client-id)]
      {:sent 1
       :woke? woke?
       :client-id client-id
       :pending (count (get @pending-toasts client-id))
       :toast toast'})
    {:sent 0
     :woke? false
     :client-id client-id
     :error :client-not-connected}))

(defn publish-toast-to-latest!
  "Publish one toast to the most recently connected demo client."
  [toast]
  (if-let [client-id (latest-client-id)]
    (publish-toast-to! client-id toast)
    {:sent 0
     :woke? false
     :error :no-connected-clients}))

(defn broadcast-toast!
  "Publish one toast to all connected demo clients.

   This is intentionally explicit. In production, broadcasting to every logged-in
   user should be rare; most notifications should be scoped by user, store,
   request, role, etc."
  [toast]
  (let [toast'     (merge {:variant :info
                           :title "Broadcast live event"
                           :description "This toast was broadcast to all connected demo clients."}
                          toast)
        client-ids (connected-clients)
        results    (mapv
                    (fn [client-id]
                      (enqueue-pending-toast! client-id toast')
                      {:client-id client-id
                       :woke? (wake-client! client-id)})
                    client-ids)]
    {:sent (count client-ids)
     :results results
     :toast toast'}))

(defn publish-sample!
  "Publish a sample toast to the most recently connected demo client."
  []
  (publish-toast-to-latest!
   {:variant :success
    :title "Hello from the REPL"
    :description "SSE woke HTMX, HTMX fetched pending OOB toast HTML, and the toaster appended it."
    :duration 5000}))

(defn publish-warning!
  []
  (publish-toast-to-latest!
   {:variant :warning
    :title "External event"
    :description "This warning was triggered by SSE and fetched as OOB HTML."}))

(defn publish-danger!
  []
  (publish-toast-to-latest!
   {:variant :danger
    :title "Server-side problem"
    :description "This danger toast was triggered by SSE and fetched as OOB HTML."}))

;; -----------------------------------------------------------------------------
;; Biff module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[base-path
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]]]})
