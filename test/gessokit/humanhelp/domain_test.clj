(ns gessokit.humanhelp.domain-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gessokit.humanhelp.domain :as domain]
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
    :request/store-id domain/store-id
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
  (is (= "demo-store" domain/store-id))
  (is (= "Human Help" domain/store-name))
  (is (= #{:open :claimed :done :cancelled} domain/statuses))
  (is (= #{:open :claimed} domain/open-statuses))
  (is (= #{:done :cancelled} domain/terminal-statuses))
  (is (= [:claim :unclaim :take-over :done :cancel]
         domain/lifecycle-actions))
  (doseq [limit [domain/request-title-max
                 domain/request-area-max
                 domain/request-details-max
                 domain/request-customer-name-max]]
    (is (integer? limit))
    (is (pos? limit))))

(deftest schemas-test
  (is (true? (m/validate domain/user-schema user-owner)))
  (is (false? (m/validate domain/user-schema {:user/id "x"})))
  (is (true? (m/validate domain/request-schema (req {}))))
  (is (false? (m/validate domain/request-schema
                          (dissoc (req {}) :request/id)))))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(deftest present-test
  (is (false? (domain/present? nil)))
  (is (false? (domain/present? "")))
  (is (false? (domain/present? "   ")))
  (is (true? (domain/present? "x")))
  (is (true? (domain/present? "  x  ")))
  (is (true? (domain/present? 0)))
  (is (true? (domain/present? false))))

(deftest trim-value-test
  (is (nil? (domain/trim-value nil)))
  (is (= "" (domain/trim-value "")))
  (is (= "" (domain/trim-value "   ")))
  (is (= "abc" (domain/trim-value "  abc  ")))
  (is (= "123" (domain/trim-value 123))))

(deftest blank->nil-test
  (is (nil? (domain/blank->nil nil)))
  (is (nil? (domain/blank->nil "")))
  (is (nil? (domain/blank->nil "   ")))
  (is (= "x" (domain/blank->nil "x")))
  (is (= "  x  " (domain/blank->nil "  x  "))))

(deftest request-param-test
  (let [params {:title "keyword title"
                "area" "string area"}]
    (is (= "keyword title" (domain/request-param params :title)))
    (is (= "string area" (domain/request-param params :area)))
    (is (nil? (domain/request-param params :missing)))))

(deftest parse-long-value-test
  (is (nil? (domain/parse-long-value nil)))
  (is (nil? (domain/parse-long-value "")))
  (is (nil? (domain/parse-long-value "   ")))
  (is (nil? (domain/parse-long-value "abc")))
  (is (nil? (domain/parse-long-value "1.5")))
  (is (= 0 (domain/parse-long-value "0")))
  (is (= 42 (domain/parse-long-value "42")))
  (is (= -2 (domain/parse-long-value "-2")))
  (is (= 7 (domain/parse-long-value 7))))

(deftest parse-visible-revision-test
  (is (nil? (domain/parse-visible-revision nil)))
  (is (nil? (domain/parse-visible-revision "")))
  (is (nil? (domain/parse-visible-revision "abc")))
  (is (= 3 (domain/parse-visible-revision "3")))
  (is (= 3 (domain/parse-visible-revision 3))))

(deftest now-ms-test
  (let [before (System/currentTimeMillis)
        n (domain/now-ms)
        after (System/currentTimeMillis)]
    (is (integer? n))
    (is (<= before n after))))

(deftest compact-map-test
  (is (= {:a 1
          :c false
          :d ""}
         (domain/compact-map
          {:a 1
           :b nil
           :c false
           :d ""}))))

(deftest normalize-token-test
  (is (nil? (domain/normalize-token nil)))
  (is (= "" (domain/normalize-token "")))
  (is (= "garden" (domain/normalize-token " Garden ")))
  (is (= ":open" (domain/normalize-token :open))))

(deftest label-helpers-test
  (is (= "" (domain/sentence-case "")))
  (is (= "" (domain/sentence-case nil)))
  (is (= "Open" (domain/sentence-case "open")))
  (is (= "Open" (domain/labelize :open)))
  (is (= "Take over" (domain/labelize :take-over)))
  (is (= "Already done" (domain/labelize "already-done"))))

;; -----------------------------------------------------------------------------
;; Users
;; -----------------------------------------------------------------------------

(deftest user-accessors-test
  (is (= "user-owner" (domain/user-id user-owner)))
  (is (= "owner@example.com" (domain/user-email user-owner)))
  (is (nil? (domain/user-id {:user/email "fallback@example.com"})))
  (is (= 123 (domain/user-id {:user/id 123})))
  (is (nil? (domain/user-email {:user/id "user-only"}))))

(deftest same-user-test
  (is (true? (domain/same-user? "1" "1")))
  (is (true? (domain/same-user? 1 "1")))
  (is (true? (domain/same-user? :a ":a")))
  (is (false? (domain/same-user? "1" "2")))
  (is (false? (domain/same-user? nil "x"))))

;; -----------------------------------------------------------------------------
;; Create-request input parsing and validation
;; -----------------------------------------------------------------------------

(deftest parse-create-request-input-test
  (let [parsed (domain/parse-create-request-input
                {"title" "  Need a rake  "
                 "area" " Garden "
                 "details" " Near aisle 4 "
                 "customer-name" " Jon "})]
    (is (= {:title "Need a rake"
            :area "Garden"
            :details "Near aisle 4"
            :customer-name "Jon"}
           parsed)))

  (let [parsed (domain/parse-create-request-input
                {:title " Need gloves "
                 :area " Hardware "
                 :details " Large work gloves "
                 :customer-name " Avery "})]
    (is (= {:title "Need gloves"
            :area "Hardware"
            :details "Large work gloves"
            :customer-name "Avery"}
           parsed)))

  (let [parsed (domain/parse-create-request-input
                {:title " Need a rake "
                 :area " Garden "
                 :details "   "
                 :customer-name ""})]
    (is (= {:title "Need a rake"
            :area "Garden"
            :details nil
            :customer-name nil}
           parsed)))

  (let [parsed (domain/parse-create-request-input
                {:title "Need paint"
                 :area "Paint"
                 :name " Morgan "})]
    (is (= "Morgan" (:customer-name parsed)))))

(deftest create-request-errors-test
  (is (nil? (domain/create-request-errors (valid-input {}))))

  (let [errors (domain/create-request-errors
                (valid-input {:title ""
                              :area "   "}))]
    (is (= "A short request is required."
           (:title errors)))
    (is (= "Choose or describe an area of the store."
           (:area errors))))

  (let [errors (domain/create-request-errors
                (valid-input
                 {:title (apply str (repeat (inc domain/request-title-max) "x"))
                  :area (apply str (repeat (inc domain/request-area-max) "x"))
                  :details (apply str (repeat (inc domain/request-details-max) "x"))
                  :customer-name (apply str (repeat (inc domain/request-customer-name-max) "x"))}))]
    (is (= (str "Use " domain/request-title-max " characters or fewer.")
           (:title errors)))
    (is (= (str "Use " domain/request-area-max " characters or fewer.")
           (:area errors)))
    (is (= (str "Use " domain/request-details-max " characters or fewer.")
           (:details errors)))
    (is (= (str "Use " domain/request-customer-name-max " characters or fewer.")
           (:customer-name errors)))))

;; (deftest valid-create-request-input-test
;;   (is (true? (domain/valid-create-request-input? (valid-input {}))))

;;   (testing "blank strings currently satisfy the raw Malli predicate"
;;     (is (true? (domain/valid-create-request-input?
;;                 (valid-input {:area "   "}))))
;;     (is (= "Choose or describe an area of the store."
;;            (:area (domain/create-request-errors
;;                    (valid-input {:area "   "))))))

;;   (is (false? (domain/valid-create-request-input?
;;                (valid-input {:title ""}))))

;;   (is (true? (domain/valid-create-request-input?
;;               (valid-input {:details nil
;;                             :customer-name nil})))))

(deftest valid-create-request-input-test
  (let [normal-input (valid-input {})
        blank-area-input (valid-input {:area "   "})
        empty-title-input (valid-input {:title ""})
        optional-nil-input (valid-input {:details nil
                                         :customer-name nil})
        blank-area-errors (domain/create-request-errors blank-area-input)]

    (is (true? (domain/valid-create-request-input? normal-input)))

    (testing "blank strings currently satisfy the raw Malli predicate"
      (is (true? (domain/valid-create-request-input? blank-area-input)))
      (is (= "Choose or describe an area of the store."
             (:area blank-area-errors))))

    (is (false? (domain/valid-create-request-input? empty-title-input)))

    (is (true? (domain/valid-create-request-input? optional-nil-input)))))

;; -----------------------------------------------------------------------------
;; Request model helpers
;; -----------------------------------------------------------------------------

(deftest request-state-predicates-test
  (is (true? (domain/request-open? (req {:request/status :open}))))
  (is (true? (domain/request-open? (req {:request/status :claimed}))))
  (is (false? (domain/request-open? (req {:request/status :done}))))
  (is (false? (domain/request-open? (req {:request/status :cancelled}))))

  (is (false? (domain/request-terminal? (req {:request/status :open}))))
  (is (false? (domain/request-terminal? (req {:request/status :claimed}))))
  (is (true? (domain/request-terminal? (req {:request/status :done}))))
  (is (true? (domain/request-terminal? (req {:request/status :cancelled})))))

(deftest request-ownership-test
  (let [r (req {:request/customer-user-id "user-owner"
                :request/status :claimed
                :request/claimed-by "user-helper"
                :request/claimed-by-email "helper@example.com"})]
    (is (true? (domain/request-owner? user-owner r)))
    (is (false? (domain/request-owner? user-helper r)))
    (is (true? (domain/request-claimed? r)))
    (is (true? (domain/request-claimed-by-user? user-helper r)))
    (is (false? (domain/request-claimed-by-user? user-owner r)))
    (is (true? (domain/request-claimed-by-other? user-owner r)))
    (is (false? (domain/request-claimed-by-other? user-helper r)))))

(deftest request-labels-test
  (is (= "Open" (domain/request-status-label (req {:request/status :open}))))
  (is (= "Claimed" (domain/request-status-label (req {:request/status :claimed}))))
  (is (= "Done" (domain/request-status-label (req {:request/status :done}))))
  (is (= "Cancelled" (domain/request-status-label (req {:request/status :cancelled})))))

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
    (is (= 2 (domain/open-request-count requests)))
    (is (= 8 (domain/newest-revision requests)))
    (is (= 0 (domain/newest-revision [])))))

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
    (is (true? (domain/request-visible-at-revision? 3 existing)))
    (is (false? (domain/request-visible-at-revision? 3 newly-created)))
    (is (true? (domain/request-visible-at-revision? 4 newly-created)))
    (is (true? (domain/request-visible-at-revision? nil newly-created)))))

(deftest board-stale-test
  (is (false? (domain/board-stale? nil 10)))
  (is (true? (domain/board-stale? 9 10)))
  (is (false? (domain/board-stale? 10 10)))
  (is (false? (domain/board-stale? 11 10))))

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
    (is (= 2 (domain/pending-open-request-count requests 3)))
    (is (= 0 (domain/pending-open-request-count requests nil)))
    (is (= 0 (domain/pending-open-request-count requests 5)))))

;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(deftest parse-search-test
  (is (= [] (domain/parse-search nil)))
  (is (= [] (domain/parse-search "")))
  (is (= [] (domain/parse-search "   ")))
  (is (= ["jon" "rake" "garden"]
         (domain/parse-search " Jon   rake Garden ")))
  (is (= ["jon" "rake"]
         (domain/parse-search "jon jon rake"))))

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
           (domain/request-search-fields r)))
    (is (str/includes? (domain/request-search-text r)
                       "helper@example.com"))))

(deftest request-matches-search-test
  (let [r (req {})]
    (is (true? (domain/request-matches-search? r nil)))
    (is (true? (domain/request-matches-search? r "")))
    (is (true? (domain/request-matches-search? r "jon rake garden")))
    (is (true? (domain/request-matches-search? r "jon bark rake garden")))
    (is (true? (domain/request-matches-search? r "JON GARDEN")))
    (is (true? (domain/request-matches-search? r "rak gar jon")))
    (is (false? (domain/request-matches-search? r "jon purple rake garden")))
    (is (false? (domain/request-matches-search? r "rak gar xyz")))))

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
           (ids (domain/filter-requests
                 [r1 r2 r3]
                 {:search "garden"
                  :visible-revision 3}))))
    (is (= ["r1" "r3"]
           (ids (domain/filter-requests
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
           (ids (domain/sort-requests-for-board
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
           (ids (domain/visible-board-requests
                 [r1 r2 r3 r4]
                 {:search "garden"
                  :visible-revision 3}))))
    (is (= ["r4" "r1" "r3"]
           (ids (domain/visible-board-requests
                 [r1 r2 r3 r4]
                 {:search "garden"
                  :visible-revision 4}))))))

;; -----------------------------------------------------------------------------
;; Time labels
;; -----------------------------------------------------------------------------

(deftest elapsed-minutes-test
  (is (= 0 (domain/elapsed-minutes 1000 1000)))
  (is (= 0 (domain/elapsed-minutes 1000 59999)))
  (is (= 1 (domain/elapsed-minutes 1000 61000)))
  (is (= 17 (domain/elapsed-minutes 0 (* 17 60000))))
  (is (= 0 (domain/elapsed-minutes 10000 1000))))

(deftest waiting-label-test
  (let [now 10000000]
    (is (= "just now"
           (domain/waiting-label (req {:request/created-at-ms now}) now)))
    (is (= "just now"
           (domain/waiting-label (req {:request/created-at-ms (- now 59000)}) now)))
    (is (= "1 min"
           (domain/waiting-label (req {:request/created-at-ms (- now 60000)}) now)))
    (is (= "17 min"
           (domain/waiting-label (req {:request/created-at-ms (- now (* 17 60000))}) now)))
    (is (= "59 min"
           (domain/waiting-label (req {:request/created-at-ms (- now (* 59 60000))}) now)))
    (is (= "1 hr"
           (domain/waiting-label (req {:request/created-at-ms (- now (* 60 60000))}) now)))
    (is (= "2 hr"
           (domain/waiting-label (req {:request/created-at-ms (- now (* 125 60000))}) now)))))

;; -----------------------------------------------------------------------------
;; Available actions and errors
;; -----------------------------------------------------------------------------

(deftest available-actions-test
  (is (= [:done :cancel]
         (domain/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-owner)))

  (is (= [:claim]
         (domain/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-helper)))

  (is (= [:done :unclaim :cancel]
         (domain/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-helper)))

  (is (= [:take-over]
         (domain/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-owner)))

  (is (= []
         (domain/available-actions
          (req {:request/status :done})
          user-owner)))

  (is (= []
         (domain/available-actions
          (req {:request/status :cancelled})
          user-owner))))

(deftest action-available-test
  (let [open-owned (req {:request/status :open
                         :request/customer-user-id "user-owner"})]
    (is (true? (domain/action-available? open-owned user-owner :done)))
    (is (false? (domain/action-available? open-owned user-owner :claim)))))

(deftest transition-error-test
  (is (= :humanhelp/request-not-found
         (:error/type (domain/transition-error nil :claim user-helper))))

  (is (= :humanhelp/unknown-action
         (:error/type (domain/transition-error (req {}) :explode user-helper))))

  (is (= :humanhelp/request-closed
         (:error/type
          (domain/transition-error
           (req {:request/status :done})
           :claim
           user-helper))))

  (is (= :humanhelp/action-not-allowed
         (:error/type
          (domain/transition-error
           (req {:request/status :open
                 :request/customer-user-id "user-owner"})
           :claim
           user-owner)))))

;; -----------------------------------------------------------------------------
;; Transitions
;; -----------------------------------------------------------------------------

(deftest transition-claim-test
  (let [result (domain/transition-request
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
  (let [result (domain/transition-request
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
  (let [result (domain/transition-request
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
           (:status (domain/transition-request
                     owned-open
                     :done
                     user-owner
                     {:revision 7}))))
    (is (= :ok
           (:status (domain/transition-request
                     claimed-by-helper
                     :done
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (domain/transition-request
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
           (:status (domain/transition-request
                     owned-open
                     :cancel
                     user-owner
                     {:revision 7}))))
    (is (= :ok
           (:status (domain/transition-request
                     claimed-by-helper
                     :cancel
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (domain/transition-request
                     owned-open
                     :cancel
                     user-helper
                     {:revision 9}))))))

(deftest transition-rejection-test
  (is (= :error
         (:status (domain/transition-request
                   (req {:request/status :open
                         :request/customer-user-id "user-owner"})
                   :claim
                   user-owner
                   {:revision 4}))))

  (is (= :error
         (:status (domain/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :unclaim
                   user-other
                   {:revision 4}))))

  (is (= :error
         (:status (domain/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :take-over
                   user-helper
                   {:revision 4}))))

  (doseq [status [:done :cancelled]]
    (is (= :error
           (:status (domain/transition-request
                     (req {:request/status status})
                     :claim
                     user-helper
                     {:revision 4}))))))

(deftest transition-default-timestamp-and-revision-test
  (let [before (System/currentTimeMillis)
        result (domain/transition-request
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
         (domain/claim-fields user-helper)))

  (is (= {:request/status :open
          :request/claimed-by nil
          :request/claimed-by-email nil}
         (domain/clear-claim-fields)))

  (is (= {:request/status :done}
         (domain/terminal-fields :done))))

(deftest action-label-test
  (is (= "Claim" (domain/action-label :claim)))
  (is (= "Unclaim" (domain/action-label :unclaim)))
  (is (= "Take over" (domain/action-label :take-over)))
  (is (= "Done" (domain/action-label :done)))
  (is (= "Cancel" (domain/action-label :cancel)))
  (is (= "Custom action" (domain/action-label :custom-action))))

(deftest action-result-message-test
  (let [r (req {:request/number 42})]
    (is (= "Claimed request #42."
           (domain/action-result-message :claim r)))
    (is (= "Unclaimed request #42."
           (domain/action-result-message :unclaim r)))
    (is (= "Took over request #42."
           (domain/action-result-message :take-over r)))
    (is (= "Marked request #42 done."
           (domain/action-result-message :done r)))
    (is (= "Cancelled request #42."
           (domain/action-result-message :cancel r)))
    (is (= "Updated request #42."
           (domain/action-result-message :whatever r)))))
