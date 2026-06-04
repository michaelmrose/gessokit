(ns gessokit.humanhelp.model-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gessokit.humanhelp.model :as model]
   [malli.core :as m]))

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

(defn req
  [overrides]
  (merge
   {:request/id "hh-req-1"
    :request/number 1
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for bark and leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1000000
    :request/updated-at-ms 1000000
    :request/created-revision 1
    :request/updated-revision 1}
   overrides))

(defn valid-input
  [overrides]
  (merge
   {:title "Need help finding a rake"
    :area "Garden"
    :details "Looking for a sturdy rake."
    :customer-name "Jon"}
   overrides))

(defn ids
  [requests]
  (mapv :request/id requests))

;; -----------------------------------------------------------------------------
;; Constants and schemas
;; -----------------------------------------------------------------------------

(deftest constants-test
  (is (= "demo-store" model/store-id))
  (is (= "Human Help" model/store-name))
  (is (= #{:open :claimed :done :cancelled} model/statuses))
  (is (= #{:open :claimed} model/open-statuses))
  (is (= #{:done :cancelled} model/terminal-statuses))
  (is (= [:claim :unclaim :take-over :done :cancel]
         model/lifecycle-actions))
  (doseq [limit [model/request-title-max
                 model/request-area-max
                 model/request-details-max
                 model/request-customer-name-max]]
    (is (integer? limit))
    (is (pos? limit))))

(deftest schemas-test
  (is (true? (m/validate model/user-schema user-owner)))
  (is (false? (m/validate model/user-schema {:user/id "x"})))
  (is (true? (m/validate model/request-schema (req {}))))
  (is (false? (m/validate model/request-schema
                          (dissoc (req {}) :request/id)))))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(deftest present-test
  (is (false? (model/present? nil)))
  (is (false? (model/present? "")))
  (is (false? (model/present? "   ")))
  (is (true? (model/present? "x")))
  (is (true? (model/present? "  x  ")))
  (is (true? (model/present? 0)))
  (is (true? (model/present? false))))

(deftest trim-value-test
  (is (nil? (model/trim-value nil)))
  (is (= "" (model/trim-value "")))
  (is (= "" (model/trim-value "   ")))
  (is (= "abc" (model/trim-value "  abc  ")))
  (is (= "123" (model/trim-value 123))))

(deftest blank->nil-test
  (is (nil? (model/blank->nil nil)))
  (is (nil? (model/blank->nil "")))
  (is (nil? (model/blank->nil "   ")))
  (is (= "x" (model/blank->nil "x")))
  (is (= "  x  " (model/blank->nil "  x  "))))

(deftest request-param-test
  (let [params {:title "keyword title"
                "area" "string area"}]
    (is (= "keyword title" (model/request-param params :title)))
    (is (= "string area" (model/request-param params :area)))
    (is (nil? (model/request-param params :missing)))))

(deftest parse-long-value-test
  (is (nil? (model/parse-long-value nil)))
  (is (nil? (model/parse-long-value "")))
  (is (nil? (model/parse-long-value "   ")))
  (is (nil? (model/parse-long-value "abc")))
  (is (nil? (model/parse-long-value "1.5")))
  (is (= 0 (model/parse-long-value "0")))
  (is (= 42 (model/parse-long-value "42")))
  (is (= -2 (model/parse-long-value "-2")))
  (is (= 7 (model/parse-long-value 7))))

(deftest parse-visible-revision-test
  (is (nil? (model/parse-visible-revision nil)))
  (is (nil? (model/parse-visible-revision "")))
  (is (nil? (model/parse-visible-revision "abc")))
  (is (= 3 (model/parse-visible-revision "3")))
  (is (= 3 (model/parse-visible-revision 3))))

(deftest now-ms-test
  (let [before (System/currentTimeMillis)
        n (model/now-ms)
        after (System/currentTimeMillis)]
    (is (integer? n))
    (is (<= before n after))))

(deftest compact-map-test
  (is (= {:a 1
          :c false
          :d ""}
         (model/compact-map
          {:a 1
           :b nil
           :c false
           :d ""}))))

(deftest normalize-token-test
  (is (nil? (model/normalize-token nil)))
  (is (= "" (model/normalize-token "")))
  (is (= "garden" (model/normalize-token " Garden ")))
  (is (= ":open" (model/normalize-token :open))))

(deftest label-helpers-test
  (is (= "" (model/sentence-case "")))
  (is (= "" (model/sentence-case nil)))
  (is (= "Open" (model/sentence-case "open")))
  (is (= "Open" (model/labelize :open)))
  (is (= "Take over" (model/labelize :take-over)))
  (is (= "Already done" (model/labelize "already-done"))))

;; -----------------------------------------------------------------------------
;; Users
;; -----------------------------------------------------------------------------

(deftest user-accessors-test
  (is (= "user-owner" (model/user-id user-owner)))
  (is (= "owner@example.com" (model/user-email user-owner)))
  (is (nil? (model/user-id {:user/email "fallback@example.com"})))
  (is (= 123 (model/user-id {:user/id 123})))
  (is (nil? (model/user-email {:user/id "user-only"}))))

(deftest same-user-test
  (is (true? (model/same-user? "1" "1")))
  (is (true? (model/same-user? 1 "1")))
  (is (true? (model/same-user? :a ":a")))
  (is (false? (model/same-user? "1" "2")))
  (is (false? (model/same-user? nil "x"))))

;; -----------------------------------------------------------------------------
;; Create-request input parsing and validation
;; -----------------------------------------------------------------------------

(deftest parse-create-request-input-test
  (let [parsed (model/parse-create-request-input
                {"title" "  Need a rake  "
                 "area" " Garden "
                 "details" " Near aisle 4 "
                 "customer-name" " Jon "})]
    (is (= {:title "Need a rake"
            :area "Garden"
            :details "Near aisle 4"
            :customer-name "Jon"}
           parsed)))

  (let [parsed (model/parse-create-request-input
                {:title " Need gloves "
                 :area " Hardware "
                 :details " Large work gloves "
                 :customer-name " Avery "})]
    (is (= {:title "Need gloves"
            :area "Hardware"
            :details "Large work gloves"
            :customer-name "Avery"}
           parsed)))

  (let [parsed (model/parse-create-request-input
                {:title " Need a rake "
                 :area " Garden "
                 :details "   "
                 :customer-name ""})]
    (is (= {:title "Need a rake"
            :area "Garden"
            :details nil
            :customer-name nil}
           parsed)))

  (let [parsed (model/parse-create-request-input
                {:title "Need paint"
                 :area "Paint"
                 :name " Morgan "})]
    (is (= "Morgan" (:customer-name parsed)))))

(deftest create-request-errors-test
  (is (nil? (model/create-request-errors (valid-input {}))))

  (let [errors (model/create-request-errors
                (valid-input {:title ""
                              :area "   "}))]
    (is (= "A short request is required."
           (:title errors)))
    (is (= "Choose or describe an area of the store."
           (:area errors))))

  (let [errors (model/create-request-errors
                (valid-input
                 {:title (apply str (repeat (inc model/request-title-max) "x"))
                  :area (apply str (repeat (inc model/request-area-max) "x"))
                  :details (apply str (repeat (inc model/request-details-max) "x"))
                  :customer-name (apply str (repeat (inc model/request-customer-name-max) "x"))}))]
    (is (= (str "Use " model/request-title-max " characters or fewer.")
           (:title errors)))
    (is (= (str "Use " model/request-area-max " characters or fewer.")
           (:area errors)))
    (is (= (str "Use " model/request-details-max " characters or fewer.")
           (:details errors)))
    (is (= (str "Use " model/request-customer-name-max " characters or fewer.")
           (:customer-name errors)))))

(deftest valid-create-request-input-test
  (let [normal-input (valid-input {})
        blank-area-input (valid-input {:area "   "})
        empty-title-input (valid-input {:title ""})
        optional-nil-input (valid-input {:details nil
                                         :customer-name nil})
        blank-area-errors (model/create-request-errors blank-area-input)]

    (is (true? (model/valid-create-request-input? normal-input)))

    (testing "blank strings currently satisfy the raw Malli predicate"
      (is (true? (model/valid-create-request-input? blank-area-input)))
      (is (= "Choose or describe an area of the store."
             (:area blank-area-errors))))

    (is (false? (model/valid-create-request-input? empty-title-input)))

    (is (true? (model/valid-create-request-input? optional-nil-input)))))

;; -----------------------------------------------------------------------------
;; Request model helpers
;; -----------------------------------------------------------------------------

(deftest request-state-predicates-test
  (is (true? (model/request-open? (req {:request/status :open}))))
  (is (true? (model/request-open? (req {:request/status :claimed}))))
  (is (false? (model/request-open? (req {:request/status :done}))))
  (is (false? (model/request-open? (req {:request/status :cancelled}))))

  (is (false? (model/request-terminal? (req {:request/status :open}))))
  (is (false? (model/request-terminal? (req {:request/status :claimed}))))
  (is (true? (model/request-terminal? (req {:request/status :done}))))
  (is (true? (model/request-terminal? (req {:request/status :cancelled})))))

(deftest request-ownership-test
  (let [r (req {:request/customer-user-id "user-owner"
                :request/status :claimed
                :request/claimed-by "user-helper"
                :request/claimed-by-email "helper@example.com"})]
    (is (true? (model/request-owner? user-owner r)))
    (is (false? (model/request-owner? user-helper r)))
    (is (true? (model/request-claimed? r)))
    (is (true? (model/request-claimed-by-user? user-helper r)))
    (is (false? (model/request-claimed-by-user? user-owner r)))
    (is (true? (model/request-claimed-by-other? user-owner r)))
    (is (false? (model/request-claimed-by-other? user-helper r)))))

(deftest request-labels-test
  (is (= "Open" (model/request-status-label (req {:request/status :open}))))
  (is (= "Claimed" (model/request-status-label (req {:request/status :claimed}))))
  (is (= "Done" (model/request-status-label (req {:request/status :done}))))
  (is (= "Cancelled" (model/request-status-label (req {:request/status :cancelled})))))

(deftest request-counts-and-revisions-test
  (let [requests [(req {:request/id "open"
                        :request/status :open
                        :request/updated-revision 2})
                  (req {:request/id "claimed"
                        :request/status :claimed
                        :request/updated-revision 8})
                  (req {:request/id "done"
                        :request/status :done
                        :request/updated-revision 4})]]
    (is (= 2 (model/open-request-count requests)))
    (is (= 8 (model/newest-revision requests)))
    (is (= 0 (model/newest-revision [])))))

;; -----------------------------------------------------------------------------
;; Visibility and board freshness
;; -----------------------------------------------------------------------------

(deftest revision-visibility-test
  (let [existing (req {:request/id "existing"
                       :request/created-revision 1
                       :request/updated-revision 10})
        newly-created (req {:request/id "new"
                            :request/created-revision 4
                            :request/updated-revision 4})]
    (is (true? (model/request-visible-at-revision? 3 existing)))
    (is (false? (model/request-visible-at-revision? 3 newly-created)))
    (is (true? (model/request-visible-at-revision? 4 newly-created)))
    (is (true? (model/request-visible-at-revision? nil newly-created)))))

(deftest board-stale-test
  (is (false? (model/board-stale? nil 10)))
  (is (true? (model/board-stale? 9 10)))
  (is (false? (model/board-stale? 10 10)))
  (is (false? (model/board-stale? 11 10))))

(deftest pending-open-request-count-test
  (let [requests [(req {:request/id "old-open"
                        :request/status :open
                        :request/created-revision 1})
                  (req {:request/id "old-claimed"
                        :request/status :claimed
                        :request/created-revision 1})
                  (req {:request/id "new-open"
                        :request/status :open
                        :request/created-revision 4})
                  (req {:request/id "new-claimed"
                        :request/status :claimed
                        :request/created-revision 5})
                  (req {:request/id "new-done"
                        :request/status :done
                        :request/created-revision 6})
                  (req {:request/id "new-cancelled"
                        :request/status :cancelled
                        :request/created-revision 7})]]
    (is (= 2 (model/pending-open-request-count requests 3)))
    (is (= 0 (model/pending-open-request-count requests nil)))
    (is (= 0 (model/pending-open-request-count requests 5)))))

;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(deftest parse-search-test
  (is (= [] (model/parse-search nil)))
  (is (= [] (model/parse-search "")))
  (is (= [] (model/parse-search "   ")))
  (is (= ["jon" "rake" "garden"]
         (model/parse-search " Jon   rake Garden ")))
  (is (= ["jon" "rake"]
         (model/parse-search "jon jon rake"))))

(deftest request-search-fields-test
  (let [r (req {:request/status :claimed
                :request/claimed-by-email "helper@example.com"})]
    (is (= ["1"
            "Need help finding a rake"
            "Garden"
            "Looking for a sturdy rake for bark and leaves."
            "Jon"
            "helper@example.com"
            "Claimed"]
           (model/request-search-fields r)))
    (is (str/includes? (model/request-search-text r)
                       "helper@example.com"))))

(deftest request-matches-search-test
  (let [r (req {})]
    (is (true? (model/request-matches-search? r nil)))
    (is (true? (model/request-matches-search? r "")))
    (is (true? (model/request-matches-search? r "jon rake garden")))
    (is (true? (model/request-matches-search? r "jon bark rake garden")))
    (is (true? (model/request-matches-search? r "JON GARDEN")))
    (is (true? (model/request-matches-search? r "rak gar jon")))
    (is (false? (model/request-matches-search? r "jon purple rake garden")))
    (is (false? (model/request-matches-search? r "rak gar xyz")))))

(deftest filter-requests-test
  (let [r1 (req {:request/id "r1"
                 :request/title "Need a rake"
                 :request/area "Garden"
                 :request/created-revision 1})
        r2 (req {:request/id "r2"
                 :request/title "Need blue paint"
                 :request/area "Paint"
                 :request/created-revision 2})
        r3 (req {:request/id "r3"
                 :request/title "Need mulch"
                 :request/area "Garden"
                 :request/created-revision 5})]
    (is (= ["r1"]
           (ids (model/filter-requests
                 [r1 r2 r3]
                 {:search "garden"
                  :visible-revision 3}))))
    (is (= ["r1" "r3"]
           (ids (model/filter-requests
                 [r1 r2 r3]
                 {:search "garden"
                  :visible-revision nil}))))))

;; -----------------------------------------------------------------------------
;; Sorting and visible board
;; -----------------------------------------------------------------------------

(deftest sort-requests-for-board-test
  (let [open-old (req {:request/id "open-old"
                       :request/status :open
                       :request/updated-at-ms 1000})
        open-new (req {:request/id "open-new"
                       :request/status :open
                       :request/updated-at-ms 3000})
        claimed (req {:request/id "claimed"
                      :request/status :claimed
                      :request/updated-at-ms 2000})
        done (req {:request/id "done"
                   :request/status :done
                   :request/updated-at-ms 5000})
        cancelled (req {:request/id "cancelled"
                        :request/status :cancelled
                        :request/updated-at-ms 6000})]
    (is (= ["open-new" "open-old" "claimed" "done" "cancelled"]
           (ids (model/sort-requests-for-board
                 [cancelled done open-new claimed open-old]))))))

(deftest visible-board-requests-test
  (let [r1 (req {:request/id "r1"
                 :request/title "Need rake"
                 :request/area "Garden"
                 :request/status :open
                 :request/updated-at-ms 1000
                 :request/created-revision 1})
        r2 (req {:request/id "r2"
                 :request/title "Need paint"
                 :request/area "Paint"
                 :request/status :open
                 :request/updated-at-ms 2000
                 :request/created-revision 2})
        r3 (req {:request/id "r3"
                 :request/title "Need mulch"
                 :request/area "Garden"
                 :request/status :done
                 :request/updated-at-ms 3000
                 :request/created-revision 3})
        r4 (req {:request/id "r4"
                 :request/title "Need gloves"
                 :request/area "Garden"
                 :request/status :open
                 :request/updated-at-ms 4000
                 :request/created-revision 4})]
    (is (= ["r1" "r3"]
           (ids (model/visible-board-requests
                 [r1 r2 r3 r4]
                 {:search "garden"
                  :visible-revision 3}))))
    (is (= ["r4" "r1" "r3"]
           (ids (model/visible-board-requests
                 [r1 r2 r3 r4]
                 {:search "garden"
                  :visible-revision 4}))))))

;; -----------------------------------------------------------------------------
;; Time labels
;; -----------------------------------------------------------------------------

(deftest elapsed-minutes-test
  (is (= 0 (model/elapsed-minutes 1000 1000)))
  (is (= 0 (model/elapsed-minutes 1000 59999)))
  (is (= 1 (model/elapsed-minutes 1000 61000)))
  (is (= 17 (model/elapsed-minutes 0 (* 17 60000))))
  (is (= 0 (model/elapsed-minutes 10000 1000))))

(deftest waiting-label-test
  (let [now 10000000]
    (is (= "just now"
           (model/waiting-label (req {:request/created-at-ms now}) now)))
    (is (= "just now"
           (model/waiting-label (req {:request/created-at-ms (- now 59000)}) now)))
    (is (= "1 min"
           (model/waiting-label (req {:request/created-at-ms (- now 60000)}) now)))
    (is (= "17 min"
           (model/waiting-label (req {:request/created-at-ms (- now (* 17 60000))}) now)))
    (is (= "59 min"
           (model/waiting-label (req {:request/created-at-ms (- now (* 59 60000))}) now)))
    (is (= "1 hr"
           (model/waiting-label (req {:request/created-at-ms (- now (* 60 60000))}) now)))
    (is (= "2 hr"
           (model/waiting-label (req {:request/created-at-ms (- now (* 125 60000))}) now)))))

;; -----------------------------------------------------------------------------
;; Available actions and errors
;; -----------------------------------------------------------------------------

(deftest available-actions-test
  (is (= [:done :cancel]
         (model/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-owner)))

  (is (= [:claim]
         (model/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-helper)))

  (is (= [:done :unclaim :cancel]
         (model/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-helper)))

  (is (= [:take-over]
         (model/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-owner)))

  (is (= []
         (model/available-actions
          (req {:request/status :done})
          user-owner)))

  (is (= []
         (model/available-actions
          (req {:request/status :cancelled})
          user-owner))))

(deftest action-available-test
  (let [open-owned (req {:request/status :open
                         :request/customer-user-id "user-owner"})]
    (is (true? (model/action-available? open-owned user-owner :done)))
    (is (false? (model/action-available? open-owned user-owner :claim)))))

(deftest transition-error-test
  (is (= :humanhelp/request-not-found
         (:error/type (model/transition-error nil :claim user-helper))))

  (is (= :humanhelp/unknown-action
         (:error/type (model/transition-error (req {}) :explode user-helper))))

  (is (= :humanhelp/request-closed
         (:error/type
          (model/transition-error
           (req {:request/status :done})
           :claim
           user-helper))))

  (is (= :humanhelp/action-not-allowed
         (:error/type
          (model/transition-error
           (req {:request/status :open
                 :request/customer-user-id "user-owner"})
           :claim
           user-owner)))))

;; -----------------------------------------------------------------------------
;; Transitions
;; -----------------------------------------------------------------------------

(deftest transition-claim-test
  (let [result (model/transition-request
                (req {:request/status :open
                      :request/customer-user-id "user-owner"})
                :claim
                user-helper
                {:now-ms 2000000
                 :revision 4})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :claimed (:request/status request')))
    (is (= "user-helper" (:request/claimed-by request')))
    (is (= "helper@example.com" (:request/claimed-by-email request')))
    (is (= 2000000 (:request/updated-at-ms request')))
    (is (= 4 (:request/updated-revision request')))
    (is (= :open (get-in result [:previous :request/status])))))

(deftest transition-unclaim-test
  (let [result (model/transition-request
                (req {:request/status :claimed
                      :request/claimed-by "user-helper"
                      :request/claimed-by-email "helper@example.com"})
                :unclaim
                user-helper
                {:now-ms 2000000
                 :revision 5})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :open (:request/status request')))
    (is (nil? (:request/claimed-by request')))
    (is (nil? (:request/claimed-by-email request')))
    (is (= 5 (:request/updated-revision request')))))

(deftest transition-take-over-test
  (let [result (model/transition-request
                (req {:request/status :claimed
                      :request/claimed-by "user-helper"
                      :request/claimed-by-email "helper@example.com"})
                :take-over
                user-other
                {:now-ms 2000000
                 :revision 6})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :claimed (:request/status request')))
    (is (= "user-other" (:request/claimed-by request')))
    (is (= "other@example.com" (:request/claimed-by-email request')))))

(deftest transition-done-test
  (let [owned-open (req {:request/status :open
                         :request/customer-user-id "user-owner"})
        claimed-by-helper (req {:request/status :claimed
                                :request/claimed-by "user-helper"})]
    (is (= :ok
           (:status (model/transition-request
                     owned-open
                     :done
                     user-owner
                     {:revision 7}))))
    (is (= :ok
           (:status (model/transition-request
                     claimed-by-helper
                     :done
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (model/transition-request
                     owned-open
                     :done
                     user-helper
                     {:revision 9}))))))

(deftest transition-cancel-test
  (let [owned-open (req {:request/status :open
                         :request/customer-user-id "user-owner"})
        claimed-by-helper (req {:request/status :claimed
                                :request/claimed-by "user-helper"})]
    (is (= :ok
           (:status (model/transition-request
                     owned-open
                     :cancel
                     user-owner
                     {:revision 7}))))
    (is (= :ok
           (:status (model/transition-request
                     claimed-by-helper
                     :cancel
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (model/transition-request
                     owned-open
                     :cancel
                     user-helper
                     {:revision 9}))))))

(deftest transition-rejection-test
  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :open
                         :request/customer-user-id "user-owner"})
                   :claim
                   user-owner
                   {:revision 4}))))

  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :unclaim
                   user-other
                   {:revision 4}))))

  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :take-over
                   user-helper
                   {:revision 4}))))

  (doseq [status [:done :cancelled]]
    (is (= :error
           (:status (model/transition-request
                     (req {:request/status status})
                     :claim
                     user-helper
                     {:revision 4}))))))

(deftest transition-default-timestamp-and-revision-test
  (let [before (System/currentTimeMillis)
        result (model/transition-request
                (req {:request/status :open
                      :request/customer-user-id "user-owner"
                      :request/updated-revision 11})
                :done
                user-owner)
        after (System/currentTimeMillis)
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= 11 (:request/updated-revision request')))
    (is (<= before (:request/updated-at-ms request') after))))

;; -----------------------------------------------------------------------------
;; Patch helpers and action messages
;; -----------------------------------------------------------------------------

(deftest patch-helper-test
  (is (= {:request/status :claimed
          :request/claimed-by "user-helper"
          :request/claimed-by-email "helper@example.com"}
         (model/claim-fields user-helper)))

  (is (= {:request/status :open
          :request/claimed-by nil
          :request/claimed-by-email nil}
         (model/clear-claim-fields)))

  (is (= {:request/status :done}
         (model/terminal-fields :done))))

(deftest action-label-test
  (is (= "Claim" (model/action-label :claim)))
  (is (= "Unclaim" (model/action-label :unclaim)))
  (is (= "Take over" (model/action-label :take-over)))
  (is (= "Done" (model/action-label :done)))
  (is (= "Cancel" (model/action-label :cancel)))
  (is (= "Custom action" (model/action-label :custom-action))))

(deftest action-result-message-test
  (let [r (req {:request/number 42})]
    (is (= "Claimed request #42."
           (model/action-result-message :claim r)))
    (is (= "Unclaimed request #42."
           (model/action-result-message :unclaim r)))
    (is (= "Took over request #42."
           (model/action-result-message :take-over r)))
    (is (= "Marked request #42 done."
           (model/action-result-message :done r)))
    (is (= "Cancelled request #42."
           (model/action-result-message :cancel r)))
    (is (= "Updated request #42."
           (model/action-result-message :whatever r)))))
