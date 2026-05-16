(ns gessotest.simple-shared-counter
  (:require
   [gesso.live.core :as live]
   [gessotest.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Synced value
;; -----------------------------------------------------------------------------

(def counter-id
  "global-shared-counter")

(def counter-subscription
  {:topic :demo-counter
   :id counter-id})

(def counter
  (live/->synced
   {:table :demo_counters
    :id counter-id
    :col :demo/value
    :topic :demo-counter
    :default 0}))

(def counter-fragment
  (live/->fragment
   {:id "simple-shared-counter-fragment"
    :src "/app/demo/simple-shared-counter/fragment"
    :stream-url "/app/demo/simple-shared-counter/stream"
    :subscription counter-subscription
    :swap :innerHTML}))

;; -----------------------------------------------------------------------------
;; Temporary local live system for functional testing
;;
;; Once this demo works, this should probably be lifted into the main gessotest
;; system/component setup instead of living as a local defonce.
;; -----------------------------------------------------------------------------

(defn live-rules
  []
  [{:when-topic :demo-counter
    :expand (fn [_ctx change]
              [{:topic (:topic change)
                :id (:id change)
                :change/kind (:change/kind change)}])}])

(defonce !live-system
  (atom nil))

(defn live-system
  []
  (or @!live-system
      (let [system (live/create
                    {:rules (live-rules)
                     :dispatch-options {:threads 1
                                        :queue-size 64
                                        :on-overflow :coalesce}
                     :fragment-options {:ttl-ms 1000}})]
        (reset! !live-system system)
        system)))

(defn close-live-system!
  []
  (when-let [system @!live-system]
    (live/close! system)
    (reset! !live-system nil))
  :closed)

(defn wrap-live-system
  [handler]
  (fn [ctx]
    (handler (assoc ctx :gesso.live/system (live-system)))))

;; -----------------------------------------------------------------------------
;; UI
;; -----------------------------------------------------------------------------

(defn button-class
  []
  "inline-flex h-11 w-11 items-center justify-center radius-xl border-theme font-heading text-xl-theme weight-semibold-theme")

(defn counter-button
  [ctx {:keys [to label]}]
  (live/post-button
   ctx
   counter-fragment
   {:to to
    :label label
    :button-attrs {:class (button-class)
                   :style {:border-style "solid"
                           :border-color "var(--border)"
                           :background "var(--background)"
                           :color "var(--foreground)"}}}))

(defn fragment
  [ctx]
  (let [n (live/live-read ctx counter)]
    [:section {:class "mx-auto max-w-3xl py-6"}
     [:div {:class "radius-xl border-theme pad-card content-stack-theme shadow-sm"
            :style {:border-style "solid"
                    :border-color "var(--border)"
                    :background "var(--card)"
                    :color "var(--card-foreground)"}}

      [:div {:class "title-stack-theme"
             :style {:text-align "center"}}
       [:div {:class "font-body text-sm-theme weight-medium-theme tracking-wide-theme uppercase"
              :style {:color "var(--muted-foreground)"}}
        "Live Demo"]
       [:h2 {:class "font-heading leading-heading tracking-heading text-2xl-theme weight-bold-theme"}
        "Simple Shared Counter"]
       [:p {:class "font-body leading-body text-base-theme"
            :style {:color "var(--muted-foreground)"}}
        "Powered by live/->synced, live/live-read, and live/live-swap!."]]

      [:div {:class "flex items-center justify-center gap-4"}
       (counter-button
        ctx
        {:to "/app/demo/simple-shared-counter/decrement"
         :label "−"})

       [:div {:class "min-w-28 radius-xl px-6 py-4 text-center"
              :style {:background "var(--muted)"}}
        [:div {:class "font-body text-xs-theme tracking-wide-theme uppercase"
               :style {:color "var(--muted-foreground)"}}
         "Value"]
        [:div {:class "font-heading text-3xl-theme weight-bold-theme"}
         n]]

       (counter-button
        ctx
        {:to "/app/demo/simple-shared-counter/increment"
         :label "+"})]

      [:p {:class "text-center font-body text-sm-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
       "Updates are persisted and pushed live to all viewers."]]]))

(defn section
  []
  (live/fragment-panel counter-fragment))

;; -----------------------------------------------------------------------------
;; HTTP handlers
;; -----------------------------------------------------------------------------

(defn stream
  [_ctx]
  (:response
   (live/start-sse!
    (live-system)
    counter-subscription
    {:flow-options {:relieve? true}})))

(defn increment!
  [ctx]
  (let [result (live/live-swap!
                ctx
                counter
                inc
                {:data {:reason :increment}})]
    (fragment (:ctx result))))

(defn decrement!
  [ctx]
  (let [result (live/live-swap!
                ctx
                counter
                dec
                {:data {:reason :decrement}})]
    (fragment (:ctx result))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:routes [["/app/demo/simple-shared-counter"
             {:middleware [mid/wrap-signed-in
                           wrap-live-system]}
             ["/stream" {:get stream}]
             ["/fragment" {:get fragment}]
             ["/increment" {:post increment!}]
             ["/decrement" {:post decrement!}]]]})
