(ns gessokit.humanhelp.store-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gessokit.humanhelp.domain :as domain]
   [gessokit.humanhelp.store :as store]))

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

(defn reset-store-fixture
  [f]
  (store/reset-demo-state!)
  (try
    (f)
    (finally
      (store/reset-demo-state!))))

(use-fixtures :each reset-store-fixture)

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
   (= domain/store-id (:request/store-id request))
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
           (store/all-requests))))

(defn seeded-open-request
  []
  (first
   (filter #(= :open (:request/status %))
           (store/all-requests))))

(defn seeded-claimed-request
  []
  (first
   (filter #(= :claimed (:request/status %))
           (store/all-requests))))

(defn seeded-terminal-request
  []
  (first
   (filter #(contains? #{:done :cancelled}
                       (:request/status %))
           (store/all-requests))))

(defn create-open-request!
  "Create a fresh open request for tests that need independent lifecycle setup
   inside a single deftest."
  [overrides]
  (:request
   (store/create-request!
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
    (let [result (store/reset-demo-state!)]
      (is (= :ok (:status result)))
      (is (integer? (:revision result)))
      (is (= (:revision result) (store/latest-revision)))))

  (testing "reset restores deterministic seeded request collection"
    (let [first-reset (do
                        (store/reset-demo-state!)
                        {:revision (store/latest-revision)
                         :requests (store/all-requests)})
          _ (store/create-request!
             {:user user-owner
              :input (valid-input {:title "Temporary request"})})
          second-reset (do
                         (store/reset-demo-state!)
                         {:revision (store/latest-revision)
                          :requests (store/all-requests)})]
      (is (= first-reset second-reset))))

  (testing "seeded requests have complete request shape"
    (let [requests (store/all-requests)]
      (is (seq requests))
      (is (every? request-shape-valid? requests))))

  (testing "seeded request ids are unique"
    (let [ids (request-ids (store/all-requests))]
      (is (= (count ids) (count (set ids))))))

  (testing "seeded request numbers are unique"
    (let [numbers (mapv :request/number (store/all-requests))]
      (is (= (count numbers) (count (set numbers))))))

  (testing "seeded state includes active request examples"
    (let [statuses (set (map :request/status (store/all-requests)))]
      (is (contains? statuses :open))
      (is (or (contains? statuses :claimed)
              (contains? statuses :done)
              (contains? statuses :cancelled))))))

(deftest latest-revision-test
  (testing "latest revision is integer and non-negative"
    (is (integer? (store/latest-revision)))
    (is (not (neg? (store/latest-revision)))))

  (testing "latest revision is stable across pure reads"
    (let [before (store/latest-revision)]
      (store/all-requests)
      (store/open-request-count)
      (store/board-data {:search ""
                         :visible-revision before})
      (store/toolbar-data {:search ""
                           :visible-revision before})
      (is (= before (store/latest-revision))))))

;; -----------------------------------------------------------------------------
;; all-requests / request-by-id
;; -----------------------------------------------------------------------------

(deftest all-requests-test
  (testing "all-requests returns a sequential collection"
    (is (sequential? (store/all-requests))))

  (testing "all-requests returns request maps"
    (is (every? map? (store/all-requests))))

  (testing "all-requests does not expose duplicate ids"
    (let [ids (request-ids (store/all-requests))]
      (is (= (count ids) (count (set ids)))))))

(deftest request-by-id-test
  (testing "existing ids return exact stored requests"
    (doseq [request (store/all-requests)]
      (is (= request (store/request-by-id (:request/id request))))))

  (testing "missing ids return nil"
    (is (nil? (store/request-by-id "missing-request-id")))
    (is (nil? (store/request-by-id nil)))
    (is (nil? (store/request-by-id "")))))

;; -----------------------------------------------------------------------------
;; normalize-view-state
;; -----------------------------------------------------------------------------

(deftest normalize-view-state-test
  (testing "nil view-state normalizes to a usable map"
    (let [view-state (store/normalize-view-state nil)]
      (is (map? view-state))
      (is (contains? view-state :search))
      (is (contains? view-state :selected-request-id))
      (is (contains? view-state :visible-revision))))

  (testing "blank search normalizes to an empty string"
    (is (= "" (:search (store/normalize-view-state {:search nil}))))
    (is (= "" (:search (store/normalize-view-state {:search ""}))))
    (is (= "" (:search (store/normalize-view-state {:search "   "}))))))

  (testing "search is trimmed"
    (is (= "garden"
           (:search
            (store/normalize-view-state {:search "  garden  "})))))

  (testing "selected request id is preserved when present"
    (is (= "hh-req-1"
           (:selected-request-id
            (store/normalize-view-state
             {:selected-request-id "hh-req-1"})))))

  (testing "visible revision defaults to latest when missing"
    (is (= (store/latest-revision)
           (:visible-revision
            (store/normalize-view-state nil))))
    (is (= (store/latest-revision)
           (:visible-revision
            (store/normalize-view-state {})))))

  (testing "visible revision is preserved when supplied"
    (is (= 1
           (:visible-revision
            (store/normalize-view-state {:visible-revision 1})))))

;; -----------------------------------------------------------------------------
;; create-request!
;; -----------------------------------------------------------------------------

(deftest create-request-success-test
  (testing "create-request! appends a new open request"
    (let [before-revision (store/latest-revision)
          before-count (count (store/all-requests))
          result (store/create-request!
                  {:user user-owner
                   :input (valid-input
                           {:title "Need help finding gloves"
                            :area "Hardware"
                            :details "Large work gloves"
                            :customer-name "Avery"})})
          request (:request result)]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= (:revision result) (store/latest-revision)))
      (is (= (inc before-count) (count (store/all-requests))))

      (is (request-shape-valid? request))
      (is (= :open (:request/status request)))
      (is (= domain/store-id (:request/store-id request)))
      (is (= "Need help finding gloves" (:request/title request)))
      (is (= "Hardware" (:request/area request)))
      (is (= "Large work gloves" (:request/details request)))
      (is (= "Avery" (:request/customer-name request)))
      (is (= (:user/id user-owner) (:request/customer-user-id request)))
      (is (nil? (:request/claimed-by request)))
      (is (nil? (:request/claimed-by-email request)))
      (is (= (:revision result) (:request/created-revision request)))
      (is (= (:revision result) (:request/updated-revision request)))
      (is (= request (store/request-by-id (:request/id request)))))))

(deftest create-request-numbering-test
  (testing "created request numbers and ids are unique and monotonic"
    (let [r1 (:request
              (store/create-request!
               {:user user-owner
                :input (valid-input {:title "First new request"})}))
          r2 (:request
              (store/create-request!
               {:user user-owner
                :input (valid-input {:title "Second new request"})}))]
      (is (not= (:request/id r1) (:request/id r2)))
      (is (not= (:request/number r1) (:request/number r2)))
      (is (< (:request/number r1) (:request/number r2)))
      (is (< (:request/created-revision r1)
             (:request/created-revision r2))))))

(deftest create-request-minimal-input-test
  (testing "details and customer-name may be nil"
    (let [result (store/create-request!
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
  (testing "store layer assumes domain/app validation happened before create"
    (let [result (store/create-request!
                  {:user user-owner
                   :input {:title ""
                           :area ""
                           :details nil
                           :customer-name nil}})]
      ;; This documents the current layering expectation. If we later decide
      ;; store should validate defensively, this test should change.
      (is (= :ok (:status result)))
      (is (= "" (get-in result [:request :request/title])))
      (is (= "" (get-in result [:request :request/area]))))))

;; -----------------------------------------------------------------------------
;; Counts / toolbar data
;; -----------------------------------------------------------------------------

(deftest open-request-count-test
  (testing "open-request-count matches active statuses in all requests"
    (let [expected (count (filter active-status? (store/all-requests)))]
      (is (= expected (store/open-request-count)))))

  (testing "creating an open request increments open-request-count"
    (let [before (store/open-request-count)]
      (store/create-request!
       {:user user-owner
        :input (valid-input {:title "New active request"})})
      (is (= (inc before) (store/open-request-count))))))

(deftest toolbar-data-test
  (testing "toolbar-data exposes the values needed by the toolbar fragment"
    (let [data (store/toolbar-data {:search ""
                                    :visible-revision (store/latest-revision)})]
      (is (= (store/latest-revision) (:latest-revision data)))
      (is (= (store/open-request-count) (:open-count data)))
      (is (= 0 (:pending-open-count data)))
      (is (false? (:stale? data)))))

  (testing "toolbar-data reports stale board and pending open requests"
    (let [visible-before (store/latest-revision)]
      (store/create-request!
       {:user user-owner
        :input (valid-input {:title "Pending new request"})})
      (let [data (store/toolbar-data {:search ""
                                      :visible-revision visible-before})]
        (is (= (store/latest-revision) (:latest-revision data)))
        (is (true? (:stale? data)))
        (is (= 1 (:pending-open-count data)))
        (is (= (store/open-request-count) (:open-count data))))))

  (testing "terminal newly-created requests do not count as pending open"
    (let [visible-before (store/latest-revision)
          {:keys [request]} (store/create-request!
                             {:user user-owner
                              :input (valid-input {:title "Soon terminal"})})]
      (store/cancel-request!
       {:request-id (:request/id request)
        :user user-owner})
      (let [data (store/toolbar-data {:search ""
                                      :visible-revision visible-before})]
        (is (true? (:stale? data)))
        (is (= 0 (:pending-open-count data)))))))

;; -----------------------------------------------------------------------------
;; Board data / visibility
;; -----------------------------------------------------------------------------

(deftest board-data-shape-test
  (testing "board-data exposes stable fields"
    (let [view-state {:search ""
                      :visible-revision (store/latest-revision)}
          data (store/board-data view-state)]
      (is (map? data))
      (is (= (store/latest-revision) (:latest-revision data)))
      (is (= (store/open-request-count) (:open-count data)))
      (is (= 0 (:pending-open-count data)))
      (is (false? (:stale? data)))
      (is (vector? (:requests data)))
      (is (map? (:view-state data)))
      (is (= (:visible-revision view-state)
             (get-in data [:view-state :visible-revision]))))))

(deftest board-data-new-request-visibility-test
  (testing "new requests are hidden from an older visible revision"
    (let [visible-before (store/latest-revision)
          {:keys [request revision]} (store/create-request!
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Hidden until refresh"})})
          stale-board (store/board-data {:search ""
                                         :visible-revision visible-before})
          fresh-board (store/board-data {:search ""
                                         :visible-revision revision})]
      (is (true? (:stale? stale-board)))
      (is (not (contains? (set (request-ids (:requests stale-board)))
                          (:request/id request))))
      (is (contains? (set (request-ids (:requests fresh-board)))
                     (:request/id request))))))

(deftest board-data-existing-request-update-visibility-test
  (testing "an already-visible request remains visible after lifecycle updates"
    (let [open-request (seeded-open-request)
          visible-before (store/latest-revision)
          result (store/claim-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          stale-board (store/board-data {:search ""
                                         :visible-revision visible-before})
          updated (store/request-by-id (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= :claimed (:request/status updated)))
      (is (contains? (set (request-ids (:requests stale-board)))
                     (:request/id open-request))))))

(deftest board-data-search-test
  (testing "search filters requests across fields"
    (store/create-request!
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (store/latest-revision)
          board (store/board-data {:search "mina purple seasonal"
                                   :visible-revision latest})]
      (is (contains? (request-titles (:requests board))
                     "Need a purple snow shovel"))))

  (testing "missing terms exclude requests"
    (store/create-request!
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (store/latest-revision)
          board (store/board-data {:search "mina purple seasonal unicorn"
                                   :visible-revision latest})]
      (is (not (contains? (request-titles (:requests board))
                          "Need a purple snow shovel"))))))

(deftest board-data-selection-test
  (testing "selected request id is preserved in normalized view state"
    (let [some-request (first (store/all-requests))
          data (store/board-data {:search ""
                                  :selected-request-id (:request/id some-request)
                                  :visible-revision (store/latest-revision)})]
      (is (= (:request/id some-request)
             (get-in data [:view-state :selected-request-id])))))

  (testing "selected request id may refer to a request not currently visible"
    (let [visible-before (store/latest-revision)
          {:keys [request]} (store/create-request!
                             {:user user-owner
                              :input (valid-input {:title "Not yet visible"})})
          data (store/board-data {:search ""
                                  :selected-request-id (:request/id request)
                                  :visible-revision visible-before})]
      (is (= (:request/id request)
             (get-in data [:view-state :selected-request-id])))
      (is (not (contains? (set (request-ids (:requests data)))
                          (:request/id request)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: claim
;; -----------------------------------------------------------------------------

(deftest claim-request-success-test
  (testing "non-owner can claim an open request"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/claim-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (store/request-by-id (:request/id open-request))]
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
          before-revision (store/latest-revision)
          result (store/claim-request!
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision)))
      (is (= open-request (store/request-by-id (:request/id open-request))))))

  (testing "missing request id returns error and does not advance revision"
    (let [before-revision (store/latest-revision)
          result (store/claim-request!
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: unclaim
;; -----------------------------------------------------------------------------

(deftest unclaim-request-success-test
  (testing "claimer can unclaim a claimed request"
    (let [open-request (seeded-open-request)
          claim (store/claim-request!
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-unclaim (store/latest-revision)
          result (store/unclaim-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (store/request-by-id (:request/id open-request))]
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
        (let [before-revision (store/latest-revision)
              result (store/unclaim-request!
                      {:request-id (:request/id claimed-request)
                       :user user-other})]
          (is (= :error (:status result)))
          (is (= before-revision (store/latest-revision)))))))

  (testing "missing request id returns error"
    (let [before-revision (store/latest-revision)
          result (store/unclaim-request!
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: take over
;; -----------------------------------------------------------------------------

(deftest take-over-request-success-test
  (testing "another user can take over a claimed request"
    (let [claimed-request (or (seeded-claimed-request)
                              (:request
                               (store/claim-request!
                                {:request-id (:request/id (seeded-open-request))
                                 :user user-helper})))
          before-revision (store/latest-revision)
          result (store/take-over-request!
                  {:request-id (:request/id claimed-request)
                   :user user-other})
          updated (store/request-by-id (:request/id claimed-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :claimed (:request/status updated)))
      (is (= (:user/id user-other) (:request/claimed-by updated)))
      (is (= (:user/email user-other) (:request/claimed-by-email updated))))))

(deftest take-over-request-error-test
  (testing "current claimer cannot take over their own claim"
    (let [claimed-request (or (seeded-claimed-request)
                              (:request
                               (store/claim-request!
                                {:request-id (:request/id (seeded-open-request))
                                 :user user-helper})))
          claimer (claimer-user-for claimed-request)
          before-revision (store/latest-revision)
          result (store/take-over-request!
                  {:request-id (:request/id claimed-request)
                   :user claimer})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision)))))

  (testing "open request cannot be taken over"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/take-over-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: done
;; -----------------------------------------------------------------------------

(deftest mark-request-done-success-test
  (testing "owner can mark open request done"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/mark-request-done!
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (store/request-by-id (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated)))
      (is (= (:revision result) (:request/updated-revision updated)))))

  (testing "claimer can mark claimed request done"
    (let [open-request (create-open-request!
                        {:title "Claimed request to mark done"})
          claim (store/claim-request!
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (store/latest-revision)
          result (store/mark-request-done!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (store/request-by-id (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated))))))

(deftest mark-request-done-error-test
  (testing "unrelated user cannot mark open request done"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/mark-request-done!
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision)))))

  (testing "terminal request cannot be marked done again"
    (let [terminal (seeded-terminal-request)]
      (when terminal
        (let [before-revision (store/latest-revision)
              result (store/mark-request-done!
                      {:request-id (:request/id terminal)
                       :user user-owner})]
          (is (= :error (:status result)))
          (is (= before-revision (store/latest-revision))))))))

;; -----------------------------------------------------------------------------
;; Lifecycle transitions: cancel
;; -----------------------------------------------------------------------------

(deftest cancel-request-success-test
  (testing "owner can cancel open request"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/cancel-request!
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (store/request-by-id (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated)))))

  (testing "claimer can cancel claimed request"
    (let [open-request (create-open-request!
                        {:title "Claimed request to cancel"})
          claim (store/claim-request!
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (store/latest-revision)
          result (store/cancel-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (store/request-by-id (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated))))))

(deftest cancel-request-error-test
  (testing "unrelated user cannot cancel open request"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          result (store/cancel-request!
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision)))))

  (testing "terminal request cannot be cancelled again"
    (let [terminal (seeded-terminal-request)]
      (when terminal
        (let [before-revision (store/latest-revision)
              result (store/cancel-request!
                      {:request-id (:request/id terminal)
                       :user user-owner})]
          (is (= :error (:status result)))
          (is (= before-revision (store/latest-revision))))))))

;; -----------------------------------------------------------------------------
;; Mutation invariants
;; -----------------------------------------------------------------------------

(deftest successful-transition-invariants-test
  (testing "successful transition updates only the targeted request"
    (let [open-request (seeded-open-request)
          before-requests-by-id (into {}
                                      (map (juxt :request/id identity))
                                      (store/all-requests))
          result (store/claim-request!
                  {:request-id (:request/id open-request)
                   :user user-helper})
          after-requests-by-id (into {}
                                     (map (juxt :request/id identity))
                                     (store/all-requests))]
      (is (= :ok (:status result)))
      (doseq [[id before] before-requests-by-id
              :when (not= id (:request/id open-request))]
        (is (= before (get after-requests-by-id id))
            (str "Untargeted request changed: " id))))))

(deftest failed-transition-invariants-test
  (testing "failed transition does not change requests or revision"
    (let [open-request (seeded-open-request)
          before-revision (store/latest-revision)
          before-requests (store/all-requests)
          result (store/claim-request!
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (store/latest-revision)))
      (is (= before-requests (store/all-requests))))))

(deftest revision-monotonicity-test
  (testing "successful mutations advance revision by one each"
    (let [r0 (store/latest-revision)
          created (:request
                   (store/create-request!
                    {:user user-owner
                     :input (valid-input {:title "Revision test"})}))
          r1 (store/latest-revision)
          claim-result (store/claim-request!
                        {:request-id (:request/id created)
                         :user user-helper})
          r2 (store/latest-revision)
          done-result (store/mark-request-done!
                       {:request-id (:request/id created)
                        :user user-helper})
          r3 (store/latest-revision)]
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
    (let [initial-requests (store/all-requests)
          initial-revision (store/latest-revision)
          created (:request
                   (store/create-request!
                    {:user user-owner
                     :input (valid-input {:title "Request to disappear"})}))]
      (store/claim-request!
       {:request-id (:request/id created)
        :user user-helper})
      (store/mark-request-done!
       {:request-id (:request/id created)
        :user user-helper})
      (is (not= initial-revision (store/latest-revision)))
      (is (some #(= (:request/id created) (:request/id %))
                (store/all-requests)))

      (store/reset-demo-state!)

      (is (= initial-revision (store/latest-revision)))
      (is (= initial-requests (store/all-requests)))
      (is (nil? (store/request-by-id (:request/id created)))))))
