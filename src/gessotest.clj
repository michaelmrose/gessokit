(ns gessotest
  (:require
   [aleph.http :as aleph]
   [clojure.test :as test]
   [clojure.tools.logging :as log]
   [clojure.tools.namespace.repl :as tn-repl]
   [com.biffweb :as biff]
   [com.biffweb.experimental :as biffx]
   [com.biffweb.experimental.auth :as biff-auth]
   [gesso.live.core :as live]
   [gessotest.app :as app]
   [gessotest.client-plumbing :as client-plumbing]
   [gessotest.email :as email]
   [gessotest.form-flow-demo.core :as form-flow-demo]
   [gessotest.home :as home]
   [gessotest.middleware :as mid]
   [gessotest.microblog :as microblog]
   [gessotest.livebench :as livebench]
   [gessotest.schema :as schema]
   [gessotest.simple-shared-counter :as simple-shared-counter]
   [gessotest.toaster-demo :as toaster-demo]
   [gessotest.ui :as ui]
   [gessotest.worker :as worker]
   [malli.core :as malc]
   [malli.registry :as malr]
   [nrepl.cmdline :as nrepl-cmd])
  (:gen-class))

(def modules
  [app/module
   (biff-auth/module {})
   home/module
   schema/module
   worker/module
   form-flow-demo/module
   toaster-demo/module
   simple-shared-counter/module
   client-plumbing/module
   microblog/module
   livebench/module
   ])

(def routes
  [["" {:middleware [mid/wrap-site-defaults]}
    (keep :routes modules)]
   ["" {:middleware [mid/wrap-api-defaults
                     mid/wrap-dev-load-token]}
    (keep :api-routes modules)]])

(def handler
  (-> (biff/reitit-handler {:routes routes})
      mid/wrap-base-defaults))

(def static-pages
  (apply biff/safe-merge (map :static modules)))

(defn generate-assets!
  [_ctx]
  (biff/export-rum static-pages "target/resources/public")
  (biff/delete-old-files {:dir "target/resources/public"
                          :exts [".html"]}))

(defn on-save
  [ctx]
  (biff/add-libs ctx)
  (biff/eval-files! ctx)
  (generate-assets! ctx)
  (test/run-all-tests #"gessotest.*-test"))

(def malli-opts
  {:registry (malr/composite-registry
              malc/default-registry
              (apply biff/safe-merge (keep :schema modules)))})

(def initial-system
  {:biff/modules #'modules
   :biff/send-email #'email/send-email
   :biff/handler #'handler
   :biff/malli-opts #'malli-opts
   :biff.beholder/on-save #'on-save
   :biff.middleware/on-error #'ui/on-error
   :biff.xtdb.listener/tables ["user" "msg"]
   :gessotest.load/dev-token (System/getenv "GESSOTEST_LOAD_TOKEN")})

(defonce system
  (atom {}))

;; -----------------------------------------------------------------------------
;; Gesso Live system component
;; -----------------------------------------------------------------------------

(defn gesso-live-rules
  []
  (vec
   (concat
    [{:when-topic :demo-counter
      :expand (fn [_ctx change]
                [{:topic (:topic change)
                  :id (:id change)
                  :change/kind (:change/kind change)}])}]
    (mapcat :live-rules modules))))

(defn use-gesso-live
  "Biff-style component that creates the app-wide Gesso Live system.

   The resulting live system is attached to ctx as :gesso.live/system so route
   handlers can use the gesso.live.core facade without each feature namespace
   creating its own local singleton."
  [ctx]
  (let [live-system (live/create
                     {:rules (gesso-live-rules)
                      ;; Indexed source routing is enabled by gesso.live.source.
                      ;; This window enables source-level coalescing before fanout:
                      ;; Source-level leading/trailing per-scope throttling.
                      ;; The first invalidation for a scope wakes immediately;
                      ;; repeated invalidations within 1000ms collapse to one
                      ;; trailing wakeup.
                      :source-options {:coalesce-window-ms 1000}
                      :dispatch-options {:threads 4
                                         :queue-size 50000
                                         :on-overflow :coalesce}
                      :fragment-options {:ttl-ms 1000}})]
    (log/info "Gesso Live system started.")
    (-> ctx
        (assoc :gesso.live/system live-system)
        (update :biff/stop
                (fnil conj [])
                (fn stop-gesso-live []
                  (log/info "Stopping Gesso Live system.")
                  (live/close! live-system))))))

;; -----------------------------------------------------------------------------
;; Aleph HTTP server component
;; -----------------------------------------------------------------------------

(defn- parse-port
  [port]
  (cond
    (integer? port)
    port

    (string? port)
    (Long/parseLong port)

    :else
    (long port)))

;; Cleanly shut down our custom pool


(defn use-aleph
  "Temporary Biff-style component that starts an Aleph/Netty HTTP server.

   This replaces biff/use-jetty in this app's component list.

   Each Ring request is merged with the current system ctx before being passed
   to :biff/handler, matching the important app-facing behavior expected by
   ordinary Biff handlers."
  [{:biff/keys [host port handler]
    :or {host "0.0.0.0"
         port 8080}
    :as ctx}]
  (when-not handler
    (throw
     (ex-info "Cannot start Aleph server without :biff/handler."
              {:ctx-keys (set (keys ctx))})))
  (let [port'    (parse-port port)
        handler' (fn [request]
                   (handler (merge ctx request)))

        ;; 1. Use Netty's built-in factory to handle thread naming and daemon status
        thread-factory (io.netty.util.concurrent.DefaultThreadFactory. "aleph-worker" true)

        ;; 2. Rock-solid standard JVM thread pool.
        worker-pool (java.util.concurrent.ThreadPoolExecutor.
                     200  ;; core pool size
                     200  ;; maximum pool size
                     60   ;; keep-alive time
                     java.util.concurrent.TimeUnit/SECONDS
                     (java.util.concurrent.LinkedBlockingQueue. 50000) ;; The massive queue
                     thread-factory)

        server   (aleph/start-server
                  handler'
                  {:host host
                   :port port'
                   ;; Tell Aleph to use our custom massive queue
                   :executor worker-pool})]
    (log/info "ALEPH SERVER STARTED" {:host host
                                      :port port'
                                      :server server})
    (update ctx :biff/stop
            (fnil conj [])
            (fn stop-aleph []
              (log/info "STOPPING ALEPH SERVER" {:host host
                                                 :port port'})
              (.close server)
              (.shutdown worker-pool)))))

(def components
  [biff/use-aero-config
   biffx/use-xtdb2
   biff/use-queues
   biffx/use-xtdb2-listener
   biff/use-htmx-refresh
   use-gesso-live
   use-aleph
   biff/use-chime
   biff/use-beholder])

(defn start
  []
  (let [new-system (reduce (fn [system component]
                             (log/info "starting:" (str component))
                             (component system))
                           initial-system
                           components)]
    (reset! system new-system)
    (generate-assets! new-system)
    (log/info "System started.")
    (log/info "Go to" (:biff/base-url new-system))
    (when-not (:gessotest.load/dev-token new-system)
      (log/warn "GESSOTEST_LOAD_TOKEN is not set; /api/microblog/dev/load routes will reject requests."))
    new-system))

(defn -main
  []
  (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone "UTC"))
  (let [{:keys [biff.nrepl/args]} (start)]
    (apply nrepl-cmd/-main args)))

(defn refresh
  []
  (doseq [f (:biff/stop @system)]
    (log/info "stopping:" (str f))
    (f))
  (tn-repl/refresh :after `start)
  :done)
