(ns gessokit.humanhelp.data-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gessokit.humanhelp.data :as data]
   [gessokit.humanhelp.model :as model]
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
       (ex-info "data-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx {:biff/node node
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
;; Users / input fixtures
;; -----------------------------------------------------------------------------

(def user-owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def user-helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def user-other
  {:user/id "user-other"
   :user/email "other@example.com"})

(defn valid-input
  [overrides]
  (merge
   {:title "Need help finding a rake"
    :area "Garden"
    :details "Looking for a sturdy rake for leaves."
    :customer-name "Jon"}
   overrides))

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(def required-request-keys
  #{:request/id
    :request/number
    :request/store-id
    :request/title
    :request/area
    :request/customer-user-id
    :request/status
    :request/created-at-ms
    :request/updated-at-ms
    :request/created-revision
    :request/updated-revision})

(def required-event-keys
  #{:event/id
    :event/store-id
    :event/kind
    :event/message
    :event/request-id
    :event/at-ms
    :event/revision})

(def valid-statuses
  #{:open :claimed :done :cancelled})

(def active-statuses
  #{:open :claimed})

(defn without-nil-vals
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn request-shape-valid?
  [request]
  (and
   (map? request)
   (every? #(contains? request %) required-request-keys)
   (string? (:request/id request))
   (pos-int? (:request/number request))
   (= model/store-id (:request/store-id request))
   (string? (:request/title request))
   (string? (:request/area request))
   (contains? valid-statuses (:request/status request))
   (integer? (:request/created-at-ms request))
   (integer? (:request/updated-at-ms request))
   (integer? (:request/created-revision request))
   (integer? (:request/updated-revision request))))

(defn event-shape-valid?
  [event]
  (and
   (map? event)
   (every? #(contains? event %) required-event-keys)
   (string? (:event/id event))
   (= model/store-id (:event/store-id event))
   (keyword? (:event/kind event))
   (string? (:event/message event))
   (string? (:event/request-id event))
   (integer? (:event/at-ms event))
   (integer? (:event/revision event))))

(defn active-request?
  [request]
  (contains? active-statuses (:request/status request)))

(defn request-ids
  [requests]
  (mapv :request/id requests))

(defn request-titles
  [requests]
  (set (map :request/title requests)))

(defn requests-by-id
  [requests]
  (into {}
        (map (juxt :request/id identity))
        requests))

(defn find-request-by-title
  [title]
  (first
   (filter #(= title (:request/title %))
           (data/all-requests (ctx)))))

(defn find-event
  [{:keys [kind request-id action]}]
  (first
   (filter
    (fn [event]
      (and
       (or (nil? kind)
           (= kind (:event/kind event)))
       (or (nil? request-id)
           (= request-id (:event/request-id event)))
       (or (nil? action)
           (= action (:event/action event)))))
    (data/all-events (ctx)))))

(defn seeded-open-request
  []
  (first
   (filter #(= :open (:request/status %))
           (data/all-requests (ctx)))))

(defn seeded-claimed-request
  []
  (first
   (filter #(= :claimed (:request/status %))
           (data/all-requests (ctx)))))

(defn seeded-terminal-request
  []
  (first
   (filter #(contains? #{:done :cancelled}
                       (:request/status %))
           (data/all-requests (ctx)))))

(defn create-open-request!
  [overrides]
  (:request
   (data/create-request!
    (ctx)
    {:user user-owner
     :input (valid-input overrides)})))

(defn owner-user-for
  [request]
  {:user/id (:request/customer-user-id request)
   :user/email (or (:request/customer-name request)
                   (:request/customer-user-id request))})

(defn claimer-user-for
  [request]
  {:user/id (:request/claimed-by request)
   :user/email (:request/claimed-by-email request)})

;; -----------------------------------------------------------------------------
;; Reset / seeded state
;; -----------------------------------------------------------------------------

(deftest reset-demo-state-test
  (testing "reset returns the deterministic demo state"
    (let [result (data/reset-demo-state! (ctx))]
      (is (= :ok (:status result)))
      (is (= 3 (:revision result)))
      (is (= 3 (data/latest-revision (ctx))))
      (is (= 3 (count (data/all-requests (ctx)))))
      (is (= 3 (count (data/all-events (ctx)))))))

  (testing "reset restores the exact seeded collection after mutations"
    (let [initial {:revision (data/latest-revision (ctx))
                   :requests (data/all-requests (ctx))
                   :events (data/all-events (ctx))}]
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "Temporary request"})})

      (is (not= initial
                {:revision (data/latest-revision (ctx))
                 :requests (data/all-requests (ctx))
                 :events (data/all-events (ctx))}))

      (data/reset-demo-state! (ctx))

      (is (= initial
             {:revision (data/latest-revision (ctx))
              :requests (data/all-requests (ctx))
              :events (data/all-events (ctx))}))))

  (testing "seeded requests and events have valid shapes"
    (is (every? request-shape-valid? (data/all-requests (ctx))))
    (is (every? event-shape-valid? (data/all-events (ctx)))))

  (testing "seeded request ids and numbers are unique"
    (let [requests (data/all-requests (ctx))
          ids (request-ids requests)
          numbers (mapv :request/number requests)]
      (is (= (count ids) (count (set ids))))
      (is (= (count numbers) (count (set numbers))))))

  (testing "seeded state includes open, claimed, and terminal examples"
    (let [statuses (set (map :request/status (data/all-requests (ctx))))]
      (is (contains? statuses :open))
      (is (contains? statuses :claimed))
      (is (some statuses [:done :cancelled])))))

(deftest latest-revision-test
  (testing "latest revision is integer and non-negative"
    (is (integer? (data/latest-revision (ctx))))
    (is (not (neg? (data/latest-revision (ctx))))))

  (testing "pure reads do not advance the revision"
    (let [before (data/latest-revision (ctx))]
      (data/all-requests (ctx))
      (data/all-events (ctx))
      (data/recent-events (ctx))
      (data/open-request-count (ctx))
      (data/summary (ctx))
      (data/board-data (ctx)
                       {:search ""
                        :visible-revision before})
      (data/toolbar-data (ctx)
                         {:search ""
                          :visible-revision before})
      (is (= before (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Read APIs
;; -----------------------------------------------------------------------------

(deftest all-requests-test
  (testing "all-requests returns sorted request maps"
    (let [requests (data/all-requests (ctx))]
      (is (vector? requests))
      (is (every? map? requests))
      (is (= (sort (map :request/number requests))
             (map :request/number requests)))))

  (testing "all-requests does not expose duplicate ids"
    (let [ids (request-ids (data/all-requests (ctx)))]
      (is (= (count ids) (count (set ids)))))))

(deftest request-by-id-test
  (testing "existing ids return the same maps found in all-requests"
    (doseq [request (data/all-requests (ctx))]
      (is (= request
             (data/request-by-id (ctx) (:request/id request))))))

  (testing "missing or blank ids return nil"
    (is (nil? (data/request-by-id (ctx) "missing-request-id")))
    (is (nil? (data/request-by-id (ctx) nil)))
    (is (nil? (data/request-by-id (ctx) "")))))

(deftest events-test
  (testing "all-events returns event maps newest-first"
    (let [events (data/all-events (ctx))]
      (is (vector? events))
      (is (every? event-shape-valid? events))
      (is (= (sort-by :event/at-ms > events)
             events))))

  (testing "recent-events returns the requested prefix"
    (let [events (data/all-events (ctx))]
      (is (= (take 2 events)
             (seq (data/recent-events (ctx) 2))))
      (is (= (take 20 events)
             (seq (data/recent-events (ctx))))))))

(deftest summary-test
  (testing "summary reflects current persisted request state"
    (let [requests (data/all-requests (ctx))
          summary (data/summary (ctx))]
      (is (= model/store-id (:store/id summary)))
      (is (= model/store-name (:store/name summary)))
      (is (= (data/latest-revision (ctx)) (:revision summary)))
      (is (= (count requests) (:total summary)))
      (is (= (count (filter active-request? requests))
             (:open summary)))
      (is (= (frequencies (map :request/status requests))
             (:by-status summary)))
      (is (fn? (:pending-open summary))))))

;; -----------------------------------------------------------------------------
;; View-state normalization
;; -----------------------------------------------------------------------------

(deftest normalize-view-state-test
  (testing "nil view-state normalizes to a complete usable shape"
    (is (= {:search ""
            :selected-request-id nil
            :visible-revision (data/latest-revision (ctx))}
           (data/normalize-view-state (ctx) nil))))

  (testing "blank search normalizes to an empty string"
    (doseq [search [nil "" "   "]]
      (is (= ""
             (:search
              (data/normalize-view-state
               (ctx)
               {:search search}))))))

  (testing "search is trimmed"
    (is (= "garden"
           (:search
            (data/normalize-view-state
             (ctx)
             {:search "  garden  "}))))
    (is (= "garden rake"
           (:search
            (data/normalize-view-state
             (ctx)
             {:search "  garden rake  "})))))

  (testing "blank selected request id normalizes to nil"
    (doseq [selected [nil "" "   "]]
      (is (nil?
           (:selected-request-id
            (data/normalize-view-state
             (ctx)
             {:selected-request-id selected}))))))

  (testing "selected request id is trimmed and preserved"
    (is (= "hh-req-1"
           (:selected-request-id
            (data/normalize-view-state
             (ctx)
             {:selected-request-id "  hh-req-1  "})))))

  (testing "visible revision defaults to latest only when absent"
    (let [latest (data/latest-revision (ctx))]
      (is (= latest
             (:visible-revision
              (data/normalize-view-state (ctx) nil))))
      (is (= latest
             (:visible-revision
              (data/normalize-view-state (ctx) {}))))
      (is (= 1
             (:visible-revision
              (data/normalize-view-state
               (ctx)
               {:visible-revision 1})))))))

;; -----------------------------------------------------------------------------
;; Creation
;; -----------------------------------------------------------------------------

(deftest create-request-success-test
  (testing "create-request! appends a new open request"
    (let [before-revision (data/latest-revision (ctx))
          before-count (count (data/all-requests (ctx)))
          result (data/create-request!
                  (ctx)
                  {:user user-owner
                   :input (valid-input
                           {:title "Need help finding gloves"
                            :area "Hardware"
                            :details "Large work gloves"
                            :customer-name "Avery"})})
          request (:request result)]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= (:revision result) (data/latest-revision (ctx))))
      (is (= (inc before-count) (count (data/all-requests (ctx)))))

      (is (request-shape-valid? request))
      (is (= :open (:request/status request)))
      (is (= model/store-id (:request/store-id request)))
      (is (= "Need help finding gloves" (:request/title request)))
      (is (= "Hardware" (:request/area request)))
      (is (= "Large work gloves" (:request/details request)))
      (is (= "Avery" (:request/customer-name request)))
      (is (= (:user/id user-owner) (:request/customer-user-id request)))
      (is (nil? (:request/claimed-by request)))
      (is (nil? (:request/claimed-by-email request)))
      (is (= (:revision result) (:request/created-revision request)))
      (is (= (:revision result) (:request/updated-revision request)))
      (is (= (without-nil-vals request)
             (data/request-by-id (ctx) (:request/id request)))))))

(deftest create-request-numbering-test
  (testing "created request numbers and ids are unique and monotonic"
    (let [r1 (:request
              (data/create-request!
               (ctx)
               {:user user-owner
                :input (valid-input {:title "First new request"})}))
          r2 (:request
              (data/create-request!
               (ctx)
               {:user user-owner
                :input (valid-input {:title "Second new request"})}))]
      (is (not= (:request/id r1) (:request/id r2)))
      (is (not= (:request/number r1) (:request/number r2)))
      (is (< (:request/number r1) (:request/number r2)))
      (is (< (:request/created-revision r1)
             (:request/created-revision r2))))))

(deftest create-request-default-customer-name-test
  (testing "customer-name falls back to user email when omitted"
    (let [result (data/create-request!
                  (ctx)
                  {:user user-owner
                   :input (valid-input
                           {:title "Need help"
                            :area "Paint"
                            :details nil
                            :customer-name nil})})
          request (:request result)]
      (is (= :ok (:status result)))
      (is (= "Need help" (:request/title request)))
      (is (= "Paint" (:request/area request)))
      (is (nil? (:request/details request)))
      (is (= "owner@example.com" (:request/customer-name request))))))

(deftest create-request-does-not-validate-test
  (testing "data layer assumes model/app validation happened before create"
    (let [result (data/create-request!
                  (ctx)
                  {:user user-owner
                   :input {:title ""
                           :area ""
                           :details nil
                           :customer-name nil}})]
      ;; This deliberately documents current layering. If data becomes
      ;; defensive later, this test should change.
      (is (= :ok (:status result)))
      (is (= "" (get-in result [:request :request/title])))
      (is (= "" (get-in result [:request :request/area]))))))

(deftest create-request-event-test
  (testing "create-request! records a request-created event"
    (let [{:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Event target"})})
          event (find-event {:kind :request/created
                             :request-id (:request/id request)})]
      (is event)
      (is (= revision (:event/revision event)))
      (is (= (:request/id request) (:event/request-id event)))
      (is (nil? (:event/action event))))))

;; -----------------------------------------------------------------------------
;; Counts / toolbar data
;; -----------------------------------------------------------------------------

(deftest open-request-count-test
  (testing "open-request-count matches active statuses in all requests"
    (let [expected (count (filter active-request?
                                  (data/all-requests (ctx))))]
      (is (= expected (data/open-request-count (ctx))))))

  (testing "creating an open request increments open-request-count"
    (let [before (data/open-request-count (ctx))]
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "New active request"})})
      (is (= (inc before) (data/open-request-count (ctx)))))))

(deftest toolbar-data-test
  (testing "toolbar-data exposes the values needed by the toolbar fragment"
    (let [latest (data/latest-revision (ctx))
          toolbar (data/toolbar-data
                   (ctx)
                   {:search ""
                    :visible-revision latest})]
      (is (= model/store-id (:store/id toolbar)))
      (is (= model/store-name (:store/name toolbar)))
      (is (= latest (:latest-revision toolbar)))
      (is (= latest (:visible-revision toolbar)))
      (is (= latest (get-in toolbar [:view-state :visible-revision])))
      (is (= (data/open-request-count (ctx)) (:open-count toolbar)))
      (is (= 0 (:pending-open-count toolbar)))
      (is (false? (:stale? toolbar)))))

  (testing "toolbar-data reports stale board and pending open requests"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request]} (data/create-request!
                             (ctx)
                             {:user user-owner
                              :input (valid-input
                                      {:title "Pending new request"})})
          toolbar (data/toolbar-data
                   (ctx)
                   {:search ""
                    :visible-revision visible-before})]
      (is (= (data/latest-revision (ctx)) (:latest-revision toolbar)))
      (is (true? (:stale? toolbar)))
      (is (= 1 (:pending-open-count toolbar)))
      (is (= (data/open-request-count (ctx)) (:open-count toolbar)))
      (is (= :open (:request/status request)))))

  (testing "terminal newly-created requests do not count as pending open or stale"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request]} (data/create-request!
                             (ctx)
                             {:user user-owner
                              :input (valid-input {:title "Soon terminal"})})]
      (data/cancel-request!
       (ctx)
       {:request-id (:request/id request)
        :user user-owner})
      (let [toolbar (data/toolbar-data
                     (ctx)
                     {:search ""
                      :visible-revision visible-before})]
        (is (false? (:stale? toolbar)))
        (is (= 0 (:pending-open-count toolbar)))))))

;; -----------------------------------------------------------------------------
;; Board data / stale visibility semantics
;; -----------------------------------------------------------------------------

(deftest board-data-shape-test
  (testing "board-data exposes the stable fields needed by request-list"
    (let [latest (data/latest-revision (ctx))
          view-state {:search ""
                      :visible-revision latest}
          board (data/board-data (ctx) view-state)]
      (is (map? board))
      (is (= model/store-id (:store/id board)))
      (is (= model/store-name (:store/name board)))
      (is (= latest (:latest-revision board)))
      (is (= latest (:visible-revision board)))
      (is (= (data/open-request-count (ctx)) (:open-count board)))
      (is (= 0 (:pending-open-count board)))
      (is (false? (:stale? board)))
      (is (vector? (:requests board)))
      (is (map? (:view-state board)))
      (is (= latest
             (get-in board [:view-state :visible-revision]))))))

(deftest board-data-new-request-visibility-test
  (testing "new requests are hidden from an older visible revision"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Hidden until refresh"})})
          stale-board (data/board-data
                       (ctx)
                       {:search ""
                        :visible-revision visible-before})
          fresh-board (data/board-data
                       (ctx)
                       {:search ""
                        :visible-revision revision})]
      (is (true? (:stale? stale-board)))
      (is (not (contains? (set (request-ids (:requests stale-board)))
                          (:request/id request))))
      (is (contains? (set (request-ids (:requests fresh-board)))
                     (:request/id request))))))

(deftest board-data-existing-request-update-visibility-test
  (testing "already-visible requests remain visible after lifecycle updates"
    (let [open-request (seeded-open-request)
          visible-before (data/latest-revision (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          stale-board (data/board-data
                       (ctx)
                       {:search ""
                        :visible-revision visible-before})
          updated-card (first
                        (filter #(= (:request/id open-request)
                                    (:request/id %))
                                (:requests stale-board)))]
      (is (= :ok (:status result)))
      (is updated-card)
      (is (= :claimed (:request/status updated-card)))
      (is (= (:user/id user-helper)
             (:request/claimed-by updated-card))))))

(deftest board-data-search-test
  (testing "search terms match collectively across customer, title, area, and details"
    (data/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (data/latest-revision (ctx))
          board (data/board-data
                 (ctx)
                 {:search "mina purple seasonal front"
                  :visible-revision latest})]
      (is (contains? (request-titles (:requests board))
                     "Need a purple snow shovel"))))

  (testing "missing terms exclude requests"
    (data/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (data/latest-revision (ctx))
          board (data/board-data
                 (ctx)
                 {:search "mina purple seasonal unicorn"
                  :visible-revision latest})]
      (is (not (contains? (request-titles (:requests board))
                          "Need a purple snow shovel"))))))

(deftest board-data-search-respects-visible-revision-test
  (testing "search cannot reveal a new request hidden behind an older revision"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Need chartreuse grout"
                                                :area "Tile"
                                                :details "Rare color"
                                                :customer-name "Mina"})})
          stale-board (data/board-data
                       (ctx)
                       {:search "chartreuse grout mina"
                        :visible-revision visible-before})
          fresh-board (data/board-data
                       (ctx)
                       {:search "chartreuse grout mina"
                        :visible-revision revision})]
      (is (not (contains? (set (request-ids (:requests stale-board)))
                          (:request/id request))))
      (is (contains? (set (request-ids (:requests fresh-board)))
                     (:request/id request))))))

(deftest board-data-selection-test
  (testing "selected request id is preserved in normalized view state"
    (let [some-request (first (data/all-requests (ctx)))
          board (data/board-data
                 (ctx)
                 {:search ""
                  :selected-request-id (:request/id some-request)
                  :visible-revision (data/latest-revision (ctx))})]
      (is (= (:request/id some-request)
             (get-in board [:view-state :selected-request-id])))))

  (testing "selected request id may refer to a request not currently visible"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request]} (data/create-request!
                             (ctx)
                             {:user user-owner
                              :input (valid-input {:title "Not yet visible"})})
          board (data/board-data
                 (ctx)
                 {:search ""
                  :selected-request-id (:request/id request)
                  :visible-revision visible-before})]
      (is (= (:request/id request)
             (get-in board [:view-state :selected-request-id])))
      (is (not (contains? (set (request-ids (:requests board)))
                          (:request/id request)))))))

;; -----------------------------------------------------------------------------
;; Lower-level board helpers
;; -----------------------------------------------------------------------------

(deftest board-helper-test
  (testing "board-requests delegates visible filtering"
    (let [visible-before (data/latest-revision (ctx))
          {:keys [request revision]} (data/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Visible helper target"})})]
      (is (not (contains? (set (request-ids
                                (data/board-requests
                                 (ctx)
                                 {:search ""
                                  :visible-revision visible-before})))
                          (:request/id request))))
      (is (contains? (set (request-ids
                           (data/board-requests
                            (ctx)
                            {:search ""
                             :visible-revision revision})))
                     (:request/id request))))))

(deftest stale-helper-test
  (testing "board-stale? compares visible revision to latest"
    (let [visible-before (data/latest-revision (ctx))]
      (is (false? (data/board-stale? (ctx) visible-before)))
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "Stale helper target"})})
      (is (true? (data/board-stale? (ctx) visible-before)))
      (is (false? (data/board-stale? (ctx) (data/latest-revision (ctx))))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: claim
;; -----------------------------------------------------------------------------

(deftest claim-request-success-test
  (testing "non-owner can claim an open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= open-request (:previous result)))
      (is (= updated (:request result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :claimed (:request/status updated)))
      (is (= (:user/id user-helper) (:request/claimed-by updated)))
      (is (= (:user/email user-helper) (:request/claimed-by-email updated)))
      (is (= (:revision result) (:request/updated-revision updated))))))

(deftest claim-request-event-test
  (testing "successful claim records a claimed event with action metadata"
    (let [open-request (seeded-open-request)
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          event (find-event {:kind :request/claimed
                             :request-id (:request/id open-request)
                             :action :claim})]
      (is (= :ok (:status result)))
      (is event)
      (is (= (:revision result) (:event/revision event))))))

(deftest claim-request-error-test
  (testing "owner cannot claim own open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          before-requests (data/all-requests (ctx))
          before-events (data/all-events (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))
      (is (= before-requests (data/all-requests (ctx))))
      (is (= before-events (data/all-events (ctx))))))

  (testing "missing request id returns error and does not advance revision"
    (let [before-revision (data/latest-revision (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: unclaim
;; -----------------------------------------------------------------------------

(deftest unclaim-request-success-test
  (testing "claimer can unclaim a claimed request"
    (let [open-request (seeded-open-request)
          claim (data/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-unclaim (data/latest-revision (ctx))
          result (data/unclaim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-unclaim) (:revision result)))
      (is (= :open (:request/status updated)))
      (is (nil? (:request/claimed-by updated)))
      (is (nil? (:request/claimed-by-email updated)))
      (is (find-event {:kind :request/unclaimed
                       :request-id (:request/id open-request)
                       :action :unclaim})))))

(deftest unclaim-request-error-test
  (testing "non-claimer cannot unclaim"
    (let [claimed-request (seeded-claimed-request)
          before-revision (data/latest-revision (ctx))
          result (data/unclaim-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))))

  (testing "missing request id returns error"
    (let [before-revision (data/latest-revision (ctx))
          result (data/unclaim-request!
                  (ctx)
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: take over
;; -----------------------------------------------------------------------------

(deftest take-over-request-success-test
  (testing "another user can take over a claimed request"
    (let [claimed-request (seeded-claimed-request)
          before-revision (data/latest-revision (ctx))
          result (data/take-over-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user user-other})
          updated (data/request-by-id (ctx) (:request/id claimed-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :claimed (:request/status updated)))
      (is (= (:user/id user-other) (:request/claimed-by updated)))
      (is (= (:user/email user-other) (:request/claimed-by-email updated)))
      (is (find-event {:kind :request/taken-over
                       :request-id (:request/id claimed-request)
                       :action :take-over})))))

(deftest take-over-request-error-test
  (testing "current claimer cannot take over their own claim"
    (let [claimed-request (seeded-claimed-request)
          claimer (claimer-user-for claimed-request)
          before-revision (data/latest-revision (ctx))
          result (data/take-over-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user claimer})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))))

  (testing "open request cannot be taken over"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/take-over-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: done
;; -----------------------------------------------------------------------------

(deftest mark-request-done-success-test
  (testing "owner can mark an open request done"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated)))
      (is (= (:revision result) (:request/updated-revision updated)))
      (is (find-event {:kind :request/done
                       :request-id (:request/id open-request)
                       :action :done}))))

  (testing "claimer can mark a claimed request done"
    (let [open-request (create-open-request!
                        {:title "Claimed request to mark done"})
          claim (data/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (data/latest-revision (ctx))
          result (data/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated))))))

(deftest mark-request-done-error-test
  (testing "unrelated user cannot mark open request done"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))))

  (testing "terminal request cannot be marked done again"
    (let [terminal (seeded-terminal-request)
          before-revision (data/latest-revision (ctx))
          result (data/mark-request-done!
                  (ctx)
                  {:request-id (:request/id terminal)
                   :user user-owner})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: cancel
;; -----------------------------------------------------------------------------

(deftest cancel-request-success-test
  (testing "owner can cancel an open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated)))
      (is (find-event {:kind :request/cancelled
                       :request-id (:request/id open-request)
                       :action :cancel}))))

  (testing "claimer can cancel a claimed request"
    (let [open-request (create-open-request!
                        {:title "Claimed request to cancel"})
          claim (data/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated))))))

(deftest cancel-request-error-test
  (testing "unrelated user cannot cancel an open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))))

  (testing "terminal request cannot be cancelled again"
    (let [terminal (seeded-terminal-request)
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id terminal)
                   :user user-owner})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Mutation invariants
;; -----------------------------------------------------------------------------

(deftest successful-transition-invariants-test
  (testing "successful transition updates only the targeted request"
    (let [open-request (seeded-open-request)
          before-requests (requests-by-id (data/all-requests (ctx)))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          after-requests (requests-by-id (data/all-requests (ctx)))]
      (is (= :ok (:status result)))
      (doseq [[id before] before-requests
              :when (not= id (:request/id open-request))]
        (is (= before (get after-requests id))
            (str "Untargeted request changed: " id))))))

(deftest failed-transition-invariants-test
  (testing "failed transition does not change requests, events, or revision"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          before-requests (data/all-requests (ctx))
          before-events (data/all-events (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))
      (is (= before-requests (data/all-requests (ctx))))
      (is (= before-events (data/all-events (ctx)))))))

(deftest revision-monotonicity-test
  (testing "successful mutations advance revision by exactly one each"
    (let [r0 (data/latest-revision (ctx))
          created (:request
                   (data/create-request!
                    (ctx)
                    {:user user-owner
                     :input (valid-input {:title "Revision test"})}))
          r1 (data/latest-revision (ctx))
          claim-result (data/claim-request!
                        (ctx)
                        {:request-id (:request/id created)
                         :user user-helper})
          r2 (data/latest-revision (ctx))
          done-result (data/mark-request-done!
                       (ctx)
                       {:request-id (:request/id created)
                        :user user-helper})
          r3 (data/latest-revision (ctx))]
      (is (= (inc r0) r1))
      (is (= :ok (:status claim-result)))
      (is (= (inc r1) r2))
      (is (= :ok (:status done-result)))
      (is (= (inc r2) r3)))))

(deftest reset-after-many-mutations-test
  (testing "reset cleans up created and mutated demo data"
    (let [initial-requests (data/all-requests (ctx))
          initial-events (data/all-events (ctx))
          initial-revision (data/latest-revision (ctx))
          created (:request
                   (data/create-request!
                    (ctx)
                    {:user user-owner
                     :input (valid-input {:title "Request to disappear"})}))]
      (data/claim-request!
       (ctx)
       {:request-id (:request/id created)
        :user user-helper})
      (data/mark-request-done!
       (ctx)
       {:request-id (:request/id created)
        :user user-helper})

      (is (not= initial-revision (data/latest-revision (ctx))))
      (is (some #(= (:request/id created) (:request/id %))
                (data/all-requests (ctx))))

      (data/reset-demo-state! (ctx))

      (is (= initial-revision (data/latest-revision (ctx))))
      (is (= initial-requests (data/all-requests (ctx))))
      (is (= initial-events (data/all-events (ctx))))
      (is (nil? (data/request-by-id (ctx) (:request/id created)))))))
