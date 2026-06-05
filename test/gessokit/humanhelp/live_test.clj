(ns gessokit.humanhelp.live-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.live.core :as live]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.data :as data]
   [gessokit.humanhelp.live :as hh-live]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.views :as views]
   [xtdb.node :as xtn]))

;; -----------------------------------------------------------------------------
;; XTDB fixture
;; -----------------------------------------------------------------------------

(defonce !ctx
  (atom nil))

(defn ctx
  []
  (or @!ctx
      (throw
       (ex-info "live-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx {:anti-forgery-token "test-token"
                  :biff/node node
                  :biff/conn node
                  :xtdb/node node})
    (try
      (f)
      (finally
        (reset! !ctx nil)))))

(defn reset-data-fixture
  [f]
  (data/reset-demo-state! (ctx))
  (try
    (f)
    (finally
      (data/reset-demo-state! (ctx)))))

(use-fixtures :once xtdb-fixture)
(use-fixtures :each reset-data-fixture)

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def user-owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def user-helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def view-state
  {:search "garden"
   :selected-request-id nil
   :visible-revision 3})

(defn latest-view-state
  []
  {:search ""
   :selected-request-id nil
   :visible-revision (data/latest-revision (ctx))})

(defn render-options
  [overrides]
  (merge
   {:user user-owner
    :view-state (latest-view-state)}
   overrides))

(defn valid-input
  [overrides]
  (merge
   {:title "Need help finding a rake"
    :area "Garden"
    :details "Looking for a sturdy rake for leaves."
    :customer-name "Jon"}
   overrides))

(defn request
  [overrides]
  (merge
   {:request/id "hh-req-99"
    :request/number 99
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1000000
    :request/updated-at-ms 1000000
    :request/created-revision 4
    :request/updated-revision 4}
   overrides))

;; -----------------------------------------------------------------------------
;; Hiccup / response helpers
;; -----------------------------------------------------------------------------

(defn hiccup-branch?
  [x]
  (and (sequential? x)
       (not (string? x))
       (not (map? x))))

(defn hiccup-seq
  [x]
  (tree-seq hiccup-branch? seq x))

(defn node?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (filter node? (hiccup-seq tree))))

(defn all-text
  [tree]
  (apply str
         (filter string? (hiccup-seq tree))))

(defn contains-text?
  [tree text]
  (boolean
   (re-find
    (re-pattern (java.util.regex.Pattern/quote text))
    (all-text tree))))

(defn response-body
  [response]
  (:body response))

(defn body-contains?
  [response text]
  (boolean
   (re-find
    (re-pattern (java.util.regex.Pattern/quote text))
    (or (response-body response) ""))))

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (= "text/html; charset=utf-8"
          (get-in response [:headers "content-type"]))
       (string? (:body response))))

(defn scope-key
  [scope]
  [(:topic scope) (:id scope)])

(defn scope-keys
  [scopes]
  (mapv scope-key scopes))

;; -----------------------------------------------------------------------------
;; Ctx/render-option assertions
;; -----------------------------------------------------------------------------

(defn assert-render-ctx
  [actual-ctx expected-render-options]
  (is (= (:anti-forgery-token (ctx))
         (:anti-forgery-token actual-ctx)))
  (is (= (:biff/node (ctx))
         (:biff/node actual-ctx)))
  (is (= expected-render-options
         (#'hh-live/render-options actual-ctx))))

;; -----------------------------------------------------------------------------
;; Constants / exported values
;; -----------------------------------------------------------------------------

(deftest constants-test
  (testing "store id is shared with domain"
    (is (= model/store-id hh-live/store-id)))

  (testing "notification scope is currently the generic app-wide client scope"
    (is (= client-plumbing/app-scope hh-live/notification-scope))))

(deftest compiled-live-and-rules-test
  (testing "compiled-live is present and marked compiled"
    (is hh-live/compiled-live)
    (is (true? (:compiled? hh-live/compiled-live))))

  (testing "live-rules are exported as stable data"
    (is (seq hh-live/live-rules))
    (is (coll? hh-live/live-rules)))

  (testing "live-rules contain the expected Human Help topics"
    (let [topics (set (map :when-topic hh-live/live-rules))]
      (doseq [topic [:request/created
                     :request/claimed
                     :request/unclaimed
                     :request/taken-over
                     :request/done
                     :request/cancelled
                     :clock/minute
                     :humanhelp-demo/reset]]
        (is (contains? topics topic)
            (str "Missing live rule for " topic))))))

;; -----------------------------------------------------------------------------
;; Scope authorization
;; -----------------------------------------------------------------------------

(deftest allow-demo-store-test
  (testing "the demo store is authorized"
    (is (true? (hh-live/allow-demo-store? (ctx) model/store-id))))

  (testing "any other store id is rejected"
    (is (false? (hh-live/allow-demo-store? (ctx) "other-store")))
    (is (false? (hh-live/allow-demo-store? (ctx) nil)))))

(deftest compiled-scope-test
  (testing "request toolbar scope uses the Human Help toolbar topic"
    (is (= {:topic :humanhelp/request-toolbar
            :id model/store-id
            :gesso.live/scope :request-toolbar
            :gesso.live/scope-label "Request toolbar"}
           (live/live-scope
            hh-live/compiled-live
            :request-toolbar
            model/store-id))))

  (testing "request list scope uses the Human Help list topic"
    (is (= {:topic :humanhelp/request-list
            :id model/store-id
            :gesso.live/scope :request-list
            :gesso.live/scope-label "Request list"}
           (live/live-scope
            hh-live/compiled-live
            :request-list
            model/store-id)))))

;; -----------------------------------------------------------------------------
;; Query functions
;; -----------------------------------------------------------------------------

(deftest request-toolbar-query-test
  (testing "query merges toolbar data with ctx, store id, and render user"
    (let [opts {:user user-owner
                :view-state {:search ""
                             :visible-revision (data/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-toolbar-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (= model/store-id (:store/id result)))
      (is (= user-owner (:user result)))
      (is (= (data/latest-revision (ctx)) (:latest-revision result)))
      (is (= (data/open-request-count (ctx)) (:open-count result)))
      (is (contains? result :pending-open-count))
      (is (contains? result :stale?))
      (is (map? (:view-state result)))))

  (testing "query respects visible revision supplied in render options"
    (let [visible-before (data/latest-revision (ctx))]
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "New pending request"})})
      (let [opts {:user user-owner
                  :view-state {:search ""
                               :visible-revision visible-before}}
            render-ctx (#'hh-live/with-render-options (ctx) opts)
            result (hh-live/request-toolbar-query render-ctx model/store-id)]
        (assert-render-ctx (:ctx result) opts)
        (is (true? (:stale? result)))
        (is (= 1 (:pending-open-count result)))))))

(deftest request-list-query-test
  (testing "query merges board data with ctx, store id, and render user"
    (let [opts {:user user-owner
                :view-state {:search ""
                             :visible-revision (data/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-list-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (= model/store-id (:store/id result)))
      (is (= user-owner (:user result)))
      (is (= (data/latest-revision (ctx)) (:latest-revision result)))
      (is (vector? (:requests result)))
      (is (map? (:view-state result)))))

  (testing "query respects search"
    (data/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Front doors"
               :customer-name "Mina"})})
    (let [opts {:user user-owner
                :view-state {:search "purple mina seasonal"
                             :visible-revision (data/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-list-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (some #(= "Need a purple snow shovel" (:request/title %))
                (:requests result))))))

(deftest request-list-query-visible-revision-test
  (testing "query preserves stale-list semantics for newly-created requests"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Hidden from stale query"})})
          stale-opts {:user user-owner
                      :view-state {:search ""
                                   :visible-revision visible-before}}
          fresh-opts {:user user-owner
                      :view-state {:search ""
                                   :visible-revision revision}}
          stale-result (hh-live/request-list-query
                        (#'hh-live/with-render-options (ctx) stale-opts)
                        model/store-id)
          fresh-result (hh-live/request-list-query
                        (#'hh-live/with-render-options (ctx) fresh-opts)
                        model/store-id)]
      (is (true? (:stale? stale-result)))
      (is (not (contains? (set (map :request/id (:requests stale-result)))
                          (:request/id request))))
      (is (contains? (set (map :request/id (:requests fresh-result)))
                     (:request/id request))))))

;; -----------------------------------------------------------------------------
;; Render functions
;; -----------------------------------------------------------------------------

(deftest request-toolbar-render-test
  (testing "render returns toolbar root"
    (let [latest (data/latest-revision (ctx))
          node (hh-live/request-toolbar-render
                {:ctx (ctx)
                 :user user-owner
                 :view-state {:search ""
                              :visible-revision latest}
                 :open-count 2
                 :pending-open-count 0
                 :stale? false
                 :latest-revision latest})]
      (is (= views/request-toolbar-dom-id (:id (attrs node))))
      (is (= "request-toolbar"
             (:data-humanhelp-fragment (attrs node))))
      (is (contains-text? node "Requests")))))

(deftest request-list-render-test
  (testing "render returns request list root"
    (let [latest (data/latest-revision (ctx))
          node (hh-live/request-list-render
                {:ctx (ctx)
                 :user user-owner
                 :view-state {:search ""
                              :visible-revision latest}
                 :requests (data/all-requests (ctx))
                 :latest-revision latest})]
      (is (= views/request-list-dom-id (:id (attrs node))))
      (is (= "request-list"
             (:data-humanhelp-fragment (attrs node)))))))

;; -----------------------------------------------------------------------------
;; Fragment URL options
;; -----------------------------------------------------------------------------

(deftest fragment-options-test
  (let [state {:search "garden rake"
               :selected-request-id "hh-req-1"
               :visible-revision 3}]
    (testing "request toolbar fragment options use route builders"
      (is (= {:fragment-url (routes/request-toolbar-fragment-url state)
              :stream-url (routes/request-toolbar-stream-url state)}
             (hh-live/fragment-options :request-toolbar state))))

    (testing "request list fragment options use route builders"
      (is (= {:fragment-url (routes/request-list-fragment-url state)
              :stream-url (routes/request-list-stream-url state)}
             (hh-live/fragment-options :request-list state))))

    (testing "unknown fragments throw useful ex-info"
      (try
        (hh-live/fragment-options :missing-fragment state)
        (is false "Expected fragment-options to throw")
        (catch clojure.lang.ExceptionInfo e
          (is (re-find #"Unknown Human Help live fragment"
                       (ex-message e)))
          (is (= :missing-fragment (:fragment (ex-data e))))
          (is (= [:request-toolbar :request-list]
                 (:known-fragments (ex-data e)))))))))

;; -----------------------------------------------------------------------------
;; Initial panel helpers
;; -----------------------------------------------------------------------------

(deftest panel-helper-test
  (testing "request-toolbar-panel delegates to model-fragment-panel with correct args"
    (let [calls (atom [])]
      (with-redefs [live/model-fragment-panel
                    (fn [& args]
                      (swap! calls conj args)
                      [:panel :toolbar])]
        (is (= [:panel :toolbar]
               (hh-live/request-toolbar-panel view-state)))
        (is (= 1 (count @calls)))
        (let [[compiled fragment-name id opts] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-toolbar fragment-name))
          (is (= model/store-id id))
          (is (= (hh-live/fragment-options :request-toolbar view-state)
                 opts))))))

  (testing "request-list-panel delegates to model-fragment-panel with correct args"
    (let [calls (atom [])]
      (with-redefs [live/model-fragment-panel
                    (fn [& args]
                      (swap! calls conj args)
                      [:panel :list])]
        (is (= [:panel :list]
               (hh-live/request-list-panel view-state)))
        (is (= 1 (count @calls)))
        (let [[compiled fragment-name id opts] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-list fragment-name))
          (is (= model/store-id id))
          (is (= (hh-live/fragment-options :request-list view-state)
                 opts)))))))

(deftest page-panels-test
  (testing "page-panels returns both panel entries"
    (with-redefs [hh-live/request-toolbar-panel
                  (fn [state]
                    [:toolbar-panel state])

                  hh-live/request-list-panel
                  (fn [state]
                    [:list-panel state])]
      (is (= {:request-toolbar-panel [:toolbar-panel view-state]
              :request-list-panel [:list-panel view-state]}
             (hh-live/page-panels view-state))))))

;; -----------------------------------------------------------------------------
;; Fragment render / response / stream helpers
;; -----------------------------------------------------------------------------

(deftest render-fragment-node-helper-test
  (testing "render-fragment-node delegates to live/render-fragment-node"
    (let [calls (atom [])
          opts {:user user-owner
                :view-state view-state}]
      (with-redefs [live/render-fragment-node
                    (fn [& args]
                      (swap! calls conj args)
                      [:rendered-fragment])]
        (is (= [:rendered-fragment]
               (hh-live/render-fragment-node
                (ctx)
                :request-toolbar
                opts)))
        (is (= 1 (count @calls)))
        (let [[compiled ctx' fragment-name id] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-toolbar fragment-name))
          (is (= model/store-id id))
          (is (= opts (#'hh-live/render-options ctx')))
          (is (= (ctx) (dissoc ctx' ::hh-live/render-options))))))))

(deftest render-fragment-response-helper-test
  (testing "render-fragment-response delegates to live/render-fragment-response"
    (let [calls (atom [])
          opts {:user user-owner
                :view-state view-state}]
      (with-redefs [live/render-fragment-response
                    (fn [& args]
                      (swap! calls conj args)
                      {:status 200
                       :body "ok"})]
        (is (= {:status 200
                :body "ok"}
               (hh-live/render-fragment-response
                (ctx)
                :request-list
                opts)))
        (is (= 1 (count @calls)))
        (let [[compiled ctx' fragment-name id] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-list fragment-name))
          (is (= model/store-id id))
          (is (= opts (#'hh-live/render-options ctx'))))))))

(deftest stream-response-helper-test
  (testing "stream-response delegates to live/start-fragment-stream! and returns :response"
    (let [calls (atom [])
          opts {:user user-owner
                :view-state view-state}
          live-system ::live-system]
      (with-redefs [live/start-fragment-stream!
                    (fn [& args]
                      (swap! calls conj args)
                      {:response {:status 200
                                  :headers {"content-type" "text/event-stream"}
                                  :body ::stream-body}
                       :other :ignored})]
        (is (= {:status 200
                :headers {"content-type" "text/event-stream"}
                :body ::stream-body}
               (hh-live/stream-response
                live-system
                (ctx)
                :request-toolbar
                opts)))
        (is (= 1 (count @calls)))
        (let [[live-system' compiled ctx' fragment-name id options] (first @calls)]
          (is (= live-system live-system'))
          (is (= hh-live/compiled-live compiled))
          (is (= :request-toolbar fragment-name))
          (is (= model/store-id id))
          (is (= opts (#'hh-live/render-options ctx')))
          (is (= {:flow-options {:relieve? true}}
                 options))))))

  (testing "stream-response merges custom options over defaults"
    (let [calls (atom [])]
      (with-redefs [live/start-fragment-stream!
                    (fn [& args]
                      (swap! calls conj args)
                      {:response {:status 200}})]
        (hh-live/stream-response
         ::live-system
         (ctx)
         :request-list
         {:user user-owner
          :view-state view-state}
         {:flow-options {:relieve? false
                         :extra true}
          :custom true})
        (let [[_live-system _compiled _ctx _fragment _id options] (first @calls)]
          (is (= {:flow-options {:relieve? false
                                  :extra true}
                  :custom true}
                 options)))))))

;; -----------------------------------------------------------------------------
;; Invalidation graph expansion
;; -----------------------------------------------------------------------------

(deftest request-created-expansion-test
  (testing "created requests wake the toolbar only"
    (let [r (request {:request/id "hh-req-4"
                      :request/number 4
                      :request/status :open})
          change (hh-live/request-created-change
                  {:request r
                   :revision 4
                   :actor user-owner})
          scopes (live/expand-change hh-live/compiled-live (ctx) change)]
      (is (= [[:humanhelp/request-toolbar model/store-id]]
             (scope-keys scopes))))))

(deftest request-transition-expansion-test
  (testing "lifecycle transitions wake toolbar and list"
    (let [current (request {:request/id "hh-req-4"
                            :request/number 4
                            :request/status :claimed})
          previous (request {:request/id "hh-req-4"
                             :request/number 4
                             :request/status :open})
          change (hh-live/request-transition-change
                  {:action :claim
                   :request current
                   :previous previous
                   :revision 5
                   :actor user-helper})
          scopes (live/expand-change hh-live/compiled-live (ctx) change)]
      (is (= [[:humanhelp/request-toolbar model/store-id]
              [:humanhelp/request-list model/store-id]]
             (scope-keys scopes))))))

(deftest minute-tick-expansion-test
  (testing "minute ticks wake request-list only"
    (let [scopes (live/expand-change
                  hh-live/compiled-live
                  (ctx)
                  (hh-live/minute-tick-change))]
      (is (= [[:humanhelp/request-list model/store-id]]
             (scope-keys scopes))))))

(deftest demo-reset-expansion-test
  (testing "demo reset wakes toolbar and list"
    (let [change (hh-live/demo-reset-change
                  {:revision 3
                   :actor user-owner})
          scopes (live/expand-change hh-live/compiled-live (ctx) change)]
      (is (= [[:humanhelp/request-toolbar model/store-id]
              [:humanhelp/request-list model/store-id]]
             (scope-keys scopes))))))

;; -----------------------------------------------------------------------------
;; Change constructors
;; -----------------------------------------------------------------------------

(deftest request-created-change-test
  (let [r (request {:request/id "hh-req-4"
                    :request/number 4
                    :request/status :open})
        change (hh-live/request-created-change
                {:request r
                 :revision 4
                 :actor user-owner})]
    (is (= :request/created (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= "hh-req-4" (:request/id change)))
    (is (= 4 (:request/number change)))
    (is (= :open (:request/status change)))
    (is (= 4 (:revision change)))
    (is (= "user-owner" (:actor/id change)))
    (is (= "owner@example.com" (:actor/email change)))))

(deftest request-transition-topic-test
  (testing "known actions map to change topics"
    (is (= :request/claimed (hh-live/request-transition-topic :claim)))
    (is (= :request/unclaimed (hh-live/request-transition-topic :unclaim)))
    (is (= :request/taken-over (hh-live/request-transition-topic :take-over)))
    (is (= :request/done (hh-live/request-transition-topic :done)))
    (is (= :request/cancelled (hh-live/request-transition-topic :cancel))))

  (testing "unknown actions throw"
    (try
      (hh-live/request-transition-topic :explode)
      (is false "Expected request-transition-topic to throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"Unknown Human Help request transition action"
                     (ex-message e)))
        (is (= :explode (:action (ex-data e))))))))

(deftest request-transition-change-test
  (let [r (request {:request/id "hh-req-4"
                    :request/number 4
                    :request/status :claimed})
        previous (request {:request/id "hh-req-4"
                           :request/number 4
                           :request/status :open})
        change (hh-live/request-transition-change
                {:action :claim
                 :request r
                 :previous previous
                 :revision 5
                 :actor user-helper})]
    (is (= :request/claimed (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= "hh-req-4" (:request/id change)))
    (is (= 4 (:request/number change)))
    (is (= :claimed (:request/status change)))
    (is (= :open (:previous/status change)))
    (is (= :claim (:action change)))
    (is (= 5 (:revision change)))
    (is (= "user-helper" (:actor/id change)))
    (is (= "helper@example.com" (:actor/email change)))))

(deftest minute-tick-change-test
  (let [before (System/currentTimeMillis)
        change (hh-live/minute-tick-change)
        after (System/currentTimeMillis)]
    (is (= :clock/minute (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (integer? (:at-ms change)))
    (is (<= before (:at-ms change) after))))

(deftest demo-reset-change-test
  (let [change (hh-live/demo-reset-change
                {:revision 9
                 :actor user-owner})]
    (is (= :humanhelp-demo/reset (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= 9 (:revision change)))
    (is (= "user-owner" (:actor/id change)))
    (is (= "owner@example.com" (:actor/email change)))))

;; -----------------------------------------------------------------------------
;; Human Help notifications
;; -----------------------------------------------------------------------------

(deftest request-toast-description-test
  (testing "description includes customer, number, and title"
    (is (= "Jon added request #4: Need a rake"
           (hh-live/request-toast-description
            {:request/customer-name "Jon"
             :request/number 4
             :request/title "Need a rake"}))))

  (testing "missing customer name falls back to Someone"
    (is (= "Someone added request #4: Need a rake"
           (hh-live/request-toast-description
            {:request/customer-name nil
             :request/number 4
             :request/title "Need a rake"}))))

  (testing "missing or blank title omits colon suffix"
    (is (= "Jon added request #4"
           (hh-live/request-toast-description
            {:request/customer-name "Jon"
             :request/number 4
             :request/title nil})))
    (is (= "Jon added request #4"
           (hh-live/request-toast-description
            {:request/customer-name "Jon"
             :request/number 4
             :request/title ""})))))

(deftest send-new-request-toast-test
  (testing "without actor, toast is sent to the whole notification scope"
    (let [scope-calls (atom [])
          except-calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [scope toast]
                      (swap! scope-calls conj {:scope scope
                                               :toast toast})
                      {:sent 1
                       :toast toast})

                    client-plumbing/send-toast-to-scope-except-user!
                    (fn [& args]
                      (swap! except-calls conj args)
                      {:sent 0})]
        (let [result (hh-live/send-new-request-toast!
                      {:request/customer-name "Jon"
                       :request/number 4
                       :request/title "Need a rake"})]
          (is (= 1 (:sent result)))
          (is (= 1 (count @scope-calls)))
          (is (empty? @except-calls))
          (is (= hh-live/notification-scope
                 (:scope (first @scope-calls))))
          (is (= {:variant :info
                  :title "New request received"
                  :description "Jon added request #4: Need a rake"}
                 (:toast (first @scope-calls))))))))

  (testing "with actor, toast excludes that user's connected clients"
    (let [scope-calls (atom [])
          except-calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [& args]
                      (swap! scope-calls conj args)
                      {:sent 0})

                    client-plumbing/send-toast-to-scope-except-user!
                    (fn [scope excluded-user-id toast]
                      (swap! except-calls conj {:scope scope
                                                :excluded-user-id excluded-user-id
                                                :toast toast})
                      {:sent 1
                       :toast toast})]
        (let [result (hh-live/send-new-request-toast!
                      {:request/customer-name "Jon"
                       :request/number 4
                       :request/title "Need a rake"}
                      {:actor user-owner})]
          (is (= 1 (:sent result)))
          (is (empty? @scope-calls))
          (is (= 1 (count @except-calls)))
          (is (= hh-live/notification-scope
                 (:scope (first @except-calls))))
          (is (= "user-owner"
                 (:excluded-user-id (first @except-calls))))
          (is (= {:variant :info
                  :title "New request received"
                  :description "Jon added request #4: Need a rake"}
                 (:toast (first @except-calls)))))))))

(deftest send-reset-toast-test
  (let [calls (atom [])]
    (with-redefs [client-plumbing/send-toast-to-scope!
                  (fn [scope toast]
                    (swap! calls conj {:scope scope
                                       :toast toast})
                    {:sent 1
                     :toast toast})]
      (let [result (hh-live/send-reset-toast!)]
        (is (= 1 (:sent result)))
        (is (= 1 (count @calls)))
        (is (= hh-live/notification-scope (:scope (first @calls))))
        (is (= {:variant :info
                :title "Demo reset"
                :description "The Human Help request board was reset."}
               (:toast (first @calls))))))))

(deftest send-request-action-error-toast-test
  (testing "explicit message"
    (let [calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [scope toast]
                      (swap! calls conj {:scope scope
                                         :toast toast})
                      {:sent 1
                       :toast toast})]
        (hh-live/send-request-action-error-toast! "Nope.")
        (is (= hh-live/notification-scope
               (:scope (first @calls))))
        (is (= {:variant :danger
                :title "Request not updated"
                :description "Nope."}
               (:toast (first @calls)))))))

  (testing "fallback message"
    (let [calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [scope toast]
                      (swap! calls conj {:scope scope
                                         :toast toast})
                      {:sent 1
                       :toast toast})]
        (hh-live/send-request-action-error-toast! nil)
        (is (= "That request action could not be completed."
               (get-in (first @calls) [:toast :description])))))))

;; -----------------------------------------------------------------------------
;; notify!
;; -----------------------------------------------------------------------------

(deftest notify-test
  (testing "notify! delegates to live/submit-expanded!"
    (let [calls (atom [])
          live-system ::live-system
          change {:topic :request/created
                  :id model/store-id
                  :store/id model/store-id}]
      (with-redefs [live/submit-expanded!
                    (fn [& args]
                      (swap! calls conj args)
                      {:submitted true})]
        (is (= {:submitted true}
               (hh-live/notify! live-system (ctx) change)))
        (is (= [[live-system (ctx) change]]
               @calls))))))

;; -----------------------------------------------------------------------------
;; Integration-ish render checks without transport
;; -----------------------------------------------------------------------------

(deftest render-fragment-node-integration-test
  (testing "actual toolbar fragment render returns expected root"
    (let [node (hh-live/render-fragment-node
                (ctx)
                :request-toolbar
                {:user user-owner
                 :view-state {:search ""
                              :visible-revision (data/latest-revision (ctx))}})]
      (is (= views/request-toolbar-dom-id (:id (attrs node))))
      (is (contains-text? node "Requests"))))

  (testing "actual request list fragment render returns expected root"
    (let [node (hh-live/render-fragment-node
                (ctx)
                :request-list
                {:user user-owner
                 :view-state {:search ""
                              :visible-revision (data/latest-revision (ctx))}})]
      (is (= views/request-list-dom-id (:id (attrs node))))
      (is (or (contains-text? node "Need help")
              (contains-text? node "No requests"))))))

(deftest render-fragment-response-integration-test
  (testing "actual toolbar response is an HTML Ring response"
    (let [response (hh-live/render-fragment-response
                    (ctx)
                    :request-toolbar
                    {:user user-owner
                     :view-state {:search ""
                                  :visible-revision (data/latest-revision (ctx))}})]
      (is (html-response? response))
      (is (body-contains? response views/request-toolbar-dom-id))))

  (testing "actual list response is an HTML Ring response"
    (let [response (hh-live/render-fragment-response
                    (ctx)
                    :request-list
                    {:user user-owner
                     :view-state {:search ""
                                  :visible-revision (data/latest-revision (ctx))}})]
      (is (html-response? response))
      (is (body-contains? response views/request-list-dom-id)))))

(deftest render-fragment-response-stale-toolbar-integration-test
  (testing "toolbar fragment rendered through live layer reports stale new-request state"
    (let [visible-before (data/latest-revision (ctx))]
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "Live stale toolbar target"})})
      (let [response (hh-live/render-fragment-response
                      (ctx)
                      :request-toolbar
                      {:user user-helper
                       :view-state {:search ""
                                    :visible-revision visible-before}})]
        (is (html-response? response))
        (is (body-contains? response views/request-toolbar-dom-id))
        (is (body-contains? response "+1 new"))
        (is (body-contains? response "New request data is available"))))))

(deftest render-fragment-response-stale-list-integration-test
  (testing "list fragment rendered through live layer does not reveal new requests behind old visible revision"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Hidden live list target"})})
          stale-response (hh-live/render-fragment-response
                          (ctx)
                          :request-list
                          {:user user-helper
                           :view-state {:search ""
                                        :visible-revision visible-before}})
          fresh-response (hh-live/render-fragment-response
                          (ctx)
                          :request-list
                          {:user user-helper
                           :view-state {:search ""
                                        :visible-revision revision}})]
      (is (html-response? stale-response))
      (is (html-response? fresh-response))
      (is (not (body-contains? stale-response (:request/title request))))
      (is (body-contains? fresh-response (:request/title request))))))
