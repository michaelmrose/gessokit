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
                  :biff/conn node})
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
;; Generic assertions
;; -----------------------------------------------------------------------------

(def required-request-keys
  #{:request/id
    :request/number
    :request/store-id
    :request/title
    :request/area
    :request/details
    :request/customer-user-id
    :request/customer-name
    :request/status
    :request/claimed-by
    :request/claimed-by-email
    :request/created-at-ms
    :request/updated-at-ms
    :request/created-revision
    :request/updated-revision})

(def valid-statuses
  #{:open :claimed :done :cancelled})

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

(defn active-status?
  [request]
  (contains? #{:open :claimed} (:request/status request)))

(defn request-ids
  [requests]
  (mapv :request/id requests))

(defn request-titles
  [requests]
  (set (map :request/title requests)))

(defn find-request-by-title
  [title]
  (first
   (filter #(= title (:request/title %))
           (data/all-requests (ctx)))))

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
  "Create a fresh open request for tests that need independent lifecycle setup
   inside a single deftest."
  [overrides]
  (:request
   (data/create-request!
    (ctx)
    {:user user-owner
     :input (valid-input overrides)})))

(defn owner-user-for
  [request]
  {:user/id (:request/customer-user-id request)
   :user/email (or (:request/customer-email request)
                   (:request/customer-name request)
                   (:request/customer-user-id request))})

(defn claimer-user-for
  [request]
  {:user/id (:request/claimed-by request)
   :user/email (:request/claimed-by-email request)})

;; -----------------------------------------------------------------------------
;; Reset / initial state
;; -----------------------------------------------------------------------------

(deftest reset-demo-state-test
  (testing "reset returns a result map with status and revision"
    (let [result (data/reset-demo-state! (ctx))]
      (is (= :ok (:status result)))
      (is (integer? (:revision result)))
      (is (= (:revision result) (data/latest-revision (ctx))))))

  (testing "reset restores deterministic seeded request collection"
    (let [first-reset (do
                        (data/reset-demo-state! (ctx))
                        {:revision (data/latest-revision (ctx))
                         :requests (data/all-requests (ctx))})
          _ (data/create-request!
             (ctx)
             {:user user-owner
              :input (valid-input {:title "Temporary request"})})
          second-reset (do
                         (data/reset-demo-state! (ctx))
                         {:revision (data/latest-revision (ctx))
                          :requests (data/all-requests (ctx))})]
      (is (= first-reset second-reset))))

  (testing "seeded requests have complete request shape"
    (let [requests (data/all-requests (ctx))]
      (is (seq requests))
      (is (every? request-shape-valid? requests))))

  (testing "seeded request ids are unique"
    (let [ids (request-ids (data/all-requests (ctx)))]
      (is (= (count ids) (count (set ids))))))

  (testing "seeded request numbers are unique"
    (let [numbers (mapv :request/number (data/all-requests (ctx)))]
      (is (= (count numbers) (count (set numbers))))))

  (testing "seeded state includes active request examples"
    (let [statuses (set (map :request/status (data/all-requests (ctx))))]
      (is (contains? statuses :open))
      (is (or (contains? statuses :claimed)
              (contains? statuses :done)
              (contains? statuses :cancelled))))))

(deftest latest-revision-test
  (testing "latest revision is integer and non-negative"
    (is (integer? (data/latest-revision (ctx))))
    (is (not (neg? (data/latest-revision (ctx))))))

  (testing "latest revision is stable across pure reads"
    (let [before (data/latest-revision (ctx))]
      (data/all-requests (ctx))
      (data/open-request-count (ctx))
      (data/board-data (ctx)
                       {:search ""
                        :visible-revision before})
      (data/toolbar-data (ctx)
                         {:search ""
                          :visible-revision before})
      (is (= before (data/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; all-requests / request-by-id
;; -----------------------------------------------------------------------------

(deftest all-requests-test
  (testing "all-requests returns a sequential collection"
    (is (sequential? (data/all-requests (ctx)))))

  (testing "all-requests returns request maps"
    (is (every? map? (data/all-requests (ctx)))))

  (testing "all-requests does not expose duplicate ids"
    (let [ids (request-ids (data/all-requests (ctx)))]
      (is (= (count ids) (count (set ids)))))))

(deftest request-by-id-test
  (testing "existing ids return exact stored requests"
    (doseq [request (data/all-requests (ctx))]
      (is (= request
             (data/request-by-id (ctx) (:request/id request))))))

  (testing "missing ids return nil"
    (is (nil? (data/request-by-id (ctx) "missing-request-id")))
    (is (nil? (data/request-by-id (ctx) nil)))
    (is (nil? (data/request-by-id (ctx) "")))))

;; -----------------------------------------------------------------------------
;; normalize-view-state
;; -----------------------------------------------------------------------------

(deftest normalize-view-state-test
  (testing "nil view-state normalizes to a usable map"
    (let [view-state (data/normalize-view-state (ctx) nil)]
      (is (map? view-state))
      (is (contains? view-state :search))
      (is (contains? view-state :selected-request-id))
      (is (contains? view-state :visible-revision))))

  (testing "blank search normalizes to an empty string"
    (is (= "" (:search (data/normalize-view-state (ctx) {:search nil}))))
    (is (= "" (:search (data/normalize-view-state (ctx) {:search ""}))))
    (is (= "" (:search (data/normalize-view-state (ctx) {:search "   "})))))

  (testing "search is trimmed"
    (is (= "garden"
           (:search
            (data/normalize-view-state
             (ctx)
             {:search "  garden  "})))))

  (testing "selected request id is preserved when present"
    (is (= "hh-req-1"
           (:selected-request-id
            (data/normalize-view-state
             (ctx)
             {:selected-request-id "hh-req-1"})))))

  (testing "visible revision defaults to latest when missing"
    (is (= (data/latest-revision (ctx))
           (:visible-revision
            (data/normalize-view-state (ctx) nil))))
    (is (= (data/latest-revision (ctx))
           (:visible-revision
            (data/normalize-view-state (ctx) {})))))

  (testing "visible revision is preserved when supplied"
    (is (= 1
           (:visible-revision
            (data/normalize-view-state
             (ctx)
             {:visible-revision 1}))))))

;; -----------------------------------------------------------------------------
;; create-request!
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
      (is (= request
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

(deftest create-request-minimal-input-test
  (testing "details and customer-name may be nil"
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
      (is (or (nil? (:request/customer-name request))
              (string? (:request/customer-name request)))))))

(deftest create-request-does-not-validate-test
  (testing "data layer assumes model/app validation happened before create"
    (let [result (data/create-request!
                  (ctx)
                  {:user user-owner
                   :input {:title ""
                           :area ""
                           :details nil
                           :customer-name nil}})]
      ;; This documents the current layering expectation. If we later decide
      ;; data should validate defensively, this test should change.
      (is (= :ok (:status result)))
      (is (= "" (get-in result [:request :request/title])))
      (is (= "" (get-in result [:request :request/area]))))))

;; -----------------------------------------------------------------------------
;; Counts / toolbar data
;; -----------------------------------------------------------------------------

(deftest open-request-count-test
  (testing "open-request-count matches active statuses in all requests"
    (let [expected (count (filter active-status?
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
    (let [toolbar (data/toolbar-data
                   (ctx)
                   {:search ""
                    :visible-revision (data/latest-revision (ctx))})]
      (is (= (data/latest-revision (ctx)) (:latest-revision toolbar)))
      (is (= (data/open-request-count (ctx)) (:open-count toolbar)))
      (is (= 0 (:pending-open-count toolbar)))
      (is (false? (:stale? toolbar)))))

  (testing "toolbar-data reports stale board and pending open requests"
    (let [visible-before (data/latest-revision (ctx))]
      (data/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "Pending new request"})})
      (let [toolbar (data/toolbar-data
                     (ctx)
                     {:search ""
                      :visible-revision visible-before})]
        (is (= (data/latest-revision (ctx)) (:latest-revision toolbar)))
        (is (true? (:stale? toolbar)))
        (is (= 1 (:pending-open-count toolbar)))
        (is (= (data/open-request-count (ctx)) (:open-count toolbar))))))

  (testing "terminal newly-created requests do not count as pending open"
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
        (is (true? (:stale? toolbar)))
        (is (= 0 (:pending-open-count toolbar)))))))

;; -----------------------------------------------------------------------------
;; Board data / visibility
;; -----------------------------------------------------------------------------

(deftest board-data-shape-test
  (testing "board-data exposes stable fields"
    (let [view-state {:search ""
                      :visible-revision (data/latest-revision (ctx))}
          board (data/board-data (ctx) view-state)]
      (is (map? board))
      (is (= (data/latest-revision (ctx)) (:latest-revision board)))
      (is (= (data/open-request-count (ctx)) (:open-count board)))
      (is (= 0 (:pending-open-count board)))
      (is (false? (:stale? board)))
      (is (vector? (:requests board)))
      (is (map? (:view-state board)))
      (is (= (:visible-revision view-state)
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
  (testing "an already-visible request remains visible after lifecycle updates"
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
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= :claimed (:request/status updated)))
      (is (contains? (set (request-ids (:requests stale-board)))
                     (:request/id open-request))))))

(deftest board-data-search-test
  (testing "search filters requests across fields"
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
                 {:search "mina purple seasonal"
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

(deftest claim-request-error-test
  (testing "owner cannot claim own open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))
      (is (= open-request
             (data/request-by-id (ctx) (:request/id open-request))))))

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
      (is (nil? (:request/claimed-by-email updated))))))

(deftest unclaim-request-error-test
  (testing "non-claimer cannot unclaim"
    (let [claimed-request (seeded-claimed-request)]
      (when claimed-request
        (let [before-revision (data/latest-revision (ctx))
              result (data/unclaim-request!
                      (ctx)
                      {:request-id (:request/id claimed-request)
                       :user user-other})]
          (is (= :error (:status result)))
          (is (= before-revision (data/latest-revision (ctx))))))))

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
    (let [claimed-request (or (seeded-claimed-request)
                              (:request
                               (data/claim-request!
                                (ctx)
                                {:request-id (:request/id (seeded-open-request))
                                 :user user-helper})))
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
      (is (= (:user/email user-other) (:request/claimed-by-email updated))))))

(deftest take-over-request-error-test
  (testing "current claimer cannot take over their own claim"
    (let [claimed-request (or (seeded-claimed-request)
                              (:request
                               (data/claim-request!
                                (ctx)
                                {:request-id (:request/id (seeded-open-request))
                                 :user user-helper})))
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
  (testing "owner can mark open request done"
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
      (is (= (:revision result) (:request/updated-revision updated)))))

  (testing "claimer can mark claimed request done"
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
    (let [terminal (seeded-terminal-request)]
      (when terminal
        (let [before-revision (data/latest-revision (ctx))
              result (data/mark-request-done!
                      (ctx)
                      {:request-id (:request/id terminal)
                       :user user-owner})]
          (is (= :error (:status result)))
          (is (= before-revision (data/latest-revision (ctx)))))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: cancel
;; -----------------------------------------------------------------------------

(deftest cancel-request-success-test
  (testing "owner can cancel open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (data/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated)))))

  (testing "claimer can cancel claimed request"
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
  (testing "unrelated user cannot cancel open request"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          result (data/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))))

  (testing "terminal request cannot be cancelled again"
    (let [terminal (seeded-terminal-request)]
      (when terminal
        (let [before-revision (data/latest-revision (ctx))
              result (data/cancel-request!
                      (ctx)
                      {:request-id (:request/id terminal)
                       :user user-owner})]
          (is (= :error (:status result)))
          (is (= before-revision (data/latest-revision (ctx)))))))))

;; -----------------------------------------------------------------------------
;; Mutation invariants
;; -----------------------------------------------------------------------------

(deftest successful-transition-invariants-test
  (testing "successful transition updates only the targeted request"
    (let [open-request (seeded-open-request)
          before-requests-by-id (into {}
                                      (map (juxt :request/id identity))
                                      (data/all-requests (ctx)))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          after-requests-by-id (into {}
                                     (map (juxt :request/id identity))
                                     (data/all-requests (ctx)))]
      (is (= :ok (:status result)))
      (doseq [[id before] before-requests-by-id
              :when (not= id (:request/id open-request))]
        (is (= before (get after-requests-by-id id))
            (str "Untargeted request changed: " id))))))

(deftest failed-transition-invariants-test
  (testing "failed transition does not change requests or revision"
    (let [open-request (seeded-open-request)
          before-revision (data/latest-revision (ctx))
          before-requests (data/all-requests (ctx))
          result (data/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (data/latest-revision (ctx))))
      (is (= before-requests (data/all-requests (ctx)))))))

(deftest revision-monotonicity-test
  (testing "successful mutations advance revision by one each"
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

;; -----------------------------------------------------------------------------
;; Reset after mutations
;; -----------------------------------------------------------------------------

(deftest reset-after-many-mutations-test
  (testing "reset cleans up created and mutated data"
    (let [initial-requests (data/all-requests (ctx))
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
      (is (nil? (data/request-by-id (ctx) (:request/id created)))))))
