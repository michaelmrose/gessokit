(ns gessokit.humanhelp.flow-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gessokit.humanhelp.app :as app]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.live :as hh-live]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.store :as store]
   [gessokit.humanhelp.views :as views]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def base-ctx
  {:anti-forgery-token "test-token"
   :gesso.live/system ::live-system
   :user/id "owner"
   :user/email "owner@example.com"
   :session {:uid "owner"
             :email "owner@example.com"}})

(def helper-ctx
  (assoc base-ctx
         :user/id "helper"
         :user/email "helper@example.com"
         :session {:uid "helper"
                   :email "helper@example.com"}))

(def other-ctx
  (assoc base-ctx
         :user/id "other"
         :user/email "other@example.com"
         :session {:uid "other"
                   :email "other@example.com"}))

(defn reset-store-fixture
  [f]
  (store/reset-demo-state!)
  (try
    (f)
    (finally
      (store/reset-demo-state!))))

(use-fixtures :each reset-store-fixture)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn response-body
  [response]
  (:body response))

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (= "text/html; charset=utf-8"
          (get-in response [:headers "content-type"]))
       (string? (:body response))))

(defn body-contains?
  [response text]
  (str/includes? (or (response-body response) "") text))

(defn oob-response-for?
  [response dom-id]
  (and (body-contains? response (str "id=\"" dom-id "\""))
       (body-contains? response "hx-swap-oob=\"outerHTML\"")))

(defn ctx-with-params
  [ctx params]
  (assoc ctx :params params))

(defn ctx-with-request-id
  [ctx request-id]
  (assoc ctx :path-params {:request-id request-id}))

(defn create-params
  [overrides]
  (merge
   {"title" "Need help finding a rake"
    "area" "Garden"
    "details" "Looking for a sturdy rake for leaves."
    "customer-name" "Jon"}
   overrides))

(defn request-by-title
  [title]
  (first
   (filter #(= title (:request/title %))
           (store/all-requests))))

(defn request-status
  [request-id]
  (:request/status (store/request-by-id request-id)))

(defn request-claimer
  [request-id]
  (:request/claimed-by (store/request-by-id request-id)))

(defn request-titles
  [requests]
  (set (map :request/title requests)))

(defn board-titles
  [view-state]
  (request-titles
   (:requests (store/board-data view-state))))

(defn notify-recorder
  [calls]
  (fn [& args]
    (swap! calls conj args)
    {:submitted true}))

(defn request-toast-recorder
  [calls]
  (fn [request & _opts]
    (swap! calls conj request)
    {:sent 1}))

(defn reset-toast-recorder
  [calls]
  (fn []
    (swap! calls inc)
    {:sent 1}))

(defn stub-toolbar
  [_ctx _view-state]
  [:div {:id views/request-toolbar-dom-id} "toolbar"])

(defn stub-list
  [_ctx _view-state]
  [:div {:id views/request-list-dom-id} "list"])

(defmacro with-app-render-stubs
  [& body]
  `(with-redefs [app/render-toolbar-node stub-toolbar
                 app/render-list-node stub-list]
     ~@body))

(defmacro with-app-side-effect-recorders
  [notify-calls request-toasts & body]
  `(with-redefs [hh-live/notify! (notify-recorder ~notify-calls)
                 hh-live/send-new-request-toast! (request-toast-recorder ~request-toasts)]
     ~@body))

(defn create-request-through-app!
  [ctx params]
  (let [notify-calls (atom [])
        request-toasts (atom [])]
    (with-app-render-stubs
      (with-app-side-effect-recorders notify-calls request-toasts
        {:response (app/create-request! (ctx-with-params ctx params))
         :notify-calls @notify-calls
         :request-toasts @request-toasts}))))

(defn lifecycle-through-app!
  [handler ctx request-id]
  (let [notify-calls (atom [])]
    (with-app-render-stubs
      (with-redefs [hh-live/notify! (notify-recorder notify-calls)]
        {:response (handler (ctx-with-request-id ctx request-id))
         :notify-calls @notify-calls}))))

(defn refresh-through-app!
  [ctx params]
  (with-app-render-stubs
    (app/refresh-requests! (ctx-with-params ctx params))))

;; -----------------------------------------------------------------------------
;; Initial state flow
;; -----------------------------------------------------------------------------

(deftest initial-state-flow-test
  (testing "reset seed state is usable for the demo board"
    (let [latest (store/latest-revision)
          toolbar (store/toolbar-data {:search ""
                                       :visible-revision latest})
          board (store/board-data {:search ""
                                   :visible-revision latest})]
      (is (= 3 latest))
      (is (= latest (:latest-revision toolbar)))
      (is (= latest (:latest-revision board)))
      (is (seq (:requests board)))
      (is (= (store/open-request-count) (:open-count toolbar)))
      (is (= (store/open-request-count) (:open-count board)))
      (is (false? (:stale? toolbar)))
      (is (false? (:stale? board))))))

(deftest initial-page-flow-test
  (testing "app page data can be built from a normal signed-in ctx"
    (let [data (app/page-data base-ctx)]
      (is (= {:user/id "owner"
              :user/email "owner@example.com"}
             (:user data)))
      (is (map? (:view-state data)))
      (is (:request-toolbar-panel data))
      (is (:request-list-panel data)))))

;; -----------------------------------------------------------------------------
;; Create request flow
;; -----------------------------------------------------------------------------

(deftest create-request-valid-flow-test
  (testing "valid app create mutates store, emits live change, sends toast, returns OOB"
    (let [before-revision (store/latest-revision)
          title "Need help finding purple gloves"
          result (create-request-through-app!
                  base-ctx
                  (create-params
                   {"title" title
                    "area" "Garden"
                    "details" "Large purple gloves"
                    "customer-name" "Avery"}))
          response (:response result)
          created (request-by-title title)]
      (is (html-response? response))
      (is created)
      (is (= (inc before-revision) (store/latest-revision)))
      (is (= :open (:request/status created)))
      (is (= "owner" (:request/customer-user-id created)))
      (is (= "Avery" (:request/customer-name created)))

      (is (= 1 (count (:notify-calls result))))
      (let [[live-system ctx change] (first (:notify-calls result))]
        (is (= ::live-system live-system))
        (is (= :request/created (:topic change)))
        (is (= (:request/id created) (:request/id change)))
        (is (= model/store-id (:store/id change)))
        (is (= "owner" (:actor/id change)))
        (is (= "owner@example.com" (:actor/email change)))
        (is (= ctx (ctx-with-params
                    base-ctx
                    (create-params
                     {"title" title
                      "area" "Garden"
                      "details" "Large purple gloves"
                      "customer-name" "Avery"})))))

      (is (= [created] (:request-toasts result)))
      (is (oob-response-for? response views/request-toolbar-dom-id))
      (is (oob-response-for? response views/request-list-dom-id))
      (is (oob-response-for? response views/create-request-dialog-id)))))

(deftest create-request-invalid-flow-test
  (testing "invalid app create does not mutate, notify, or toast"
    (let [before-revision (store/latest-revision)
          before-requests (store/all-requests)
          result (create-request-through-app!
                  base-ctx
                  {"title" ""
                   "area" ""})
          response (:response result)]
      (is (html-response? response))
      (is (= before-revision (store/latest-revision)))
      (is (= before-requests (store/all-requests)))
      (is (empty? (:notify-calls result)))
      (is (empty? (:request-toasts result)))
      (is (oob-response-for? response views/create-request-dialog-id))
      (is (body-contains? response "Create request")))))

;; -----------------------------------------------------------------------------
;; Two-viewer stale/refresh semantics
;; -----------------------------------------------------------------------------

(deftest two-viewer-create-refresh-flow-test
  (testing "other viewer gets stale board semantics until refresh"
    (let [viewer-b-visible-revision (store/latest-revision)
          title "Need help loading cedar mulch"
          create-result (create-request-through-app!
                         base-ctx
                         (create-params
                          {"title" title
                           "area" "Garden"
                           "details" "Five bags near entrance"
                           "customer-name" "Mina"}))
          latest (store/latest-revision)
          stale-toolbar (store/toolbar-data
                         {:search ""
                          :visible-revision viewer-b-visible-revision})
          stale-board (store/board-data
                       {:search ""
                        :visible-revision viewer-b-visible-revision})
          fresh-board (store/board-data
                       {:search ""
                        :visible-revision latest})]
      (is (html-response? (:response create-result)))
      (is (= (inc viewer-b-visible-revision) latest))

      (is (true? (:stale? stale-toolbar)))
      (is (pos? (:pending-open-count stale-toolbar)))
      (is (not (contains? (request-titles (:requests stale-board))
                          title)))
      (is (contains? (request-titles (:requests fresh-board))
                     title))

      (let [refresh-response (refresh-through-app!
                              helper-ctx
                              {"visible-revision" (str viewer-b-visible-revision)})]
        (is (html-response? refresh-response))
        (is (oob-response-for? refresh-response views/request-toolbar-dom-id))
        (is (oob-response-for? refresh-response views/request-list-dom-id))))))

(deftest create-refresh-with-search-flow-test
  (testing "refresh preserves search while advancing visible revision"
    (let [old-revision (store/latest-revision)
          title "Need help finding a blue snow shovel"]
      (create-request-through-app!
       base-ctx
       (create-params
        {"title" title
         "area" "Seasonal"
         "details" "Blue shovel near front"
         "customer-name" "Nora"}))

      (let [stale-matching (store/board-data
                            {:search "blue shovel nora"
                             :visible-revision old-revision})
            fresh-matching (store/board-data
                            {:search "blue shovel nora"
                             :visible-revision (store/latest-revision)})]
        (is (not (contains? (request-titles (:requests stale-matching))
                            title)))
        (is (contains? (request-titles (:requests fresh-matching))
                       title))))))

;; -----------------------------------------------------------------------------
;; Search flow
;; -----------------------------------------------------------------------------

(deftest search-flow-test
  (testing "search terms match collectively across customer, title, area, details"
    (let [title "Need help finding a long-handled rake"]
      (create-request-through-app!
       base-ctx
       (create-params
        {"title" title
         "area" "Garden"
         "details" "Customer is comparing bark tools"
         "customer-name" "Jon"}))

      (let [latest (store/latest-revision)]
        (is (contains? (board-titles {:search "jon rake garden"
                                      :visible-revision latest})
                       title))
        (is (contains? (board-titles {:search "jon bark rake garden"
                                      :visible-revision latest})
                       title))
        (is (not (contains? (board-titles {:search "jon bark rake garden purple"
                                           :visible-revision latest})
                            title)))))))

(deftest search-handler-flow-test
  (testing "search handler renders only the list fragment response"
    (let [response (app/search-requests
                    (ctx-with-params
                     base-ctx
                     {"q" "garden"
                      "visible-revision" (str (store/latest-revision))}))]
      (is (html-response? response))
      (is (body-contains? response views/request-list-dom-id))
      (is (not (body-contains? response views/request-toolbar-dom-id))))))

;; -----------------------------------------------------------------------------
;; Selection flow
;; -----------------------------------------------------------------------------

(deftest select-request-flow-test
  (testing "select handler expands a visible card by setting selected request id"
    (let [request (first (store/all-requests))
          ctx (ctx-with-request-id base-ctx (:request/id request))
          ctx (assoc ctx
                     :params {"selected" (:request/id request)
                              "visible-revision" (str (store/latest-revision))})
          response (app/select-request ctx)]
      (is (html-response? response))
      (is (body-contains? response views/request-list-dom-id))
      (is (body-contains? response (:request/title request)))
      (is (or (body-contains? response (:request/details request))
              (not (model/present? (:request/details request))))))))

;; -----------------------------------------------------------------------------
;; Lifecycle flows
;; -----------------------------------------------------------------------------

(deftest claim-unclaim-flow-test
  (testing "helper can claim an open request and then unclaim it"
    (let [created-title "Need help picking paint brushes"
          create-result (create-request-through-app!
                         base-ctx
                         (create-params
                          {"title" created-title
                           "area" "Paint"
                           "details" "Two inch angled brush"
                           "customer-name" "Pat"}))
          request-id (:request/id (request-by-title created-title))]
      (is (html-response? (:response create-result)))

      (let [claim-result (lifecycle-through-app!
                          app/claim-request!
                          helper-ctx
                          request-id)]
        (is (html-response? (:response claim-result)))
        (is (= :claimed (request-status request-id)))
        (is (= "helper" (request-claimer request-id)))
        (is (= :request/claimed
               (:topic (nth (first (:notify-calls claim-result)) 2)))))

      (let [unclaim-result (lifecycle-through-app!
                            app/unclaim-request!
                            helper-ctx
                            request-id)]
        (is (html-response? (:response unclaim-result)))
        (is (= :open (request-status request-id)))
        (is (nil? (request-claimer request-id)))
        (is (= :request/unclaimed
               (:topic (nth (first (:notify-calls unclaim-result)) 2))))))))

(deftest take-over-flow-test
  (testing "another user can take over a claimed request"
    (let [created-title "Need help loading plywood"
          _create-result (create-request-through-app!
                          base-ctx
                          (create-params
                           {"title" created-title
                            "area" "Lumber"
                            "details" "Three sheets"
                            "customer-name" "Robin"}))
          request-id (:request/id (request-by-title created-title))]
      (let [claim-result (lifecycle-through-app!
                          app/claim-request!
                          helper-ctx
                          request-id)]
        (is (html-response? (:response claim-result)))
        (is (= :claimed (request-status request-id)))
        (is (= "helper" (request-claimer request-id)))
        (is (= :request/claimed
               (:topic (nth (first (:notify-calls claim-result)) 2)))))

      (let [take-over-result (lifecycle-through-app!
                              app/take-over-request!
                              other-ctx
                              request-id)]
        (is (html-response? (:response take-over-result)))
        (is (= :claimed (request-status request-id)))
        (is (= "other" (request-claimer request-id)))
        (is (= :request/taken-over
               (:topic (nth (first (:notify-calls take-over-result)) 2))))))))

(deftest done-flow-test
  (testing "owner can mark their own open request done"
    (let [created-title "Need help finding caulk"
          _create-result (create-request-through-app!
                          base-ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "White kitchen caulk"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title created-title))
          done-result (lifecycle-through-app!
                       app/mark-request-done!
                       base-ctx
                       request-id)]
      (is (html-response? (:response done-result)))
      (is (= :done (request-status request-id)))
      (is (= :request/done
             (:topic (nth (first (:notify-calls done-result)) 2)))))))

(deftest cancel-flow-test
  (testing "owner can cancel their own open request"
    (let [created-title "Need help finding return desk"
          _create-result (create-request-through-app!
                          base-ctx
                          (create-params
                           {"title" created-title
                            "area" "Customer service"
                            "details" "Wrong receipt"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title created-title))
          cancel-result (lifecycle-through-app!
                         app/cancel-request!
                         base-ctx
                         request-id)]
      (is (html-response? (:response cancel-result)))
      (is (= :cancelled (request-status request-id)))
      (is (= :request/cancelled
             (:topic (nth (first (:notify-calls cancel-result)) 2)))))))

(deftest forbidden-action-flow-test
  (testing "owner cannot claim their own open request"
    (let [created-title "Need help finding nails"
          _create-result (create-request-through-app!
                          base-ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "Finish nails"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title created-title))
          result (lifecycle-through-app!
                  app/claim-request!
                  base-ctx
                  request-id)]
      (is (html-response? (:response result)))
      (is (= :open (request-status request-id)))
      (is (empty? (:notify-calls result)))
      (is (body-contains? (:response result) "Request not updated")))))

(deftest missing-request-action-flow-test
  (testing "missing request action returns an error response and does not notify"
    (let [result (lifecycle-through-app!
                  app/claim-request!
                  helper-ctx
                  "missing-request")]
      (is (html-response? (:response result)))
      (is (empty? (:notify-calls result)))
      (is (body-contains? (:response result) "Request not updated")))))

;; -----------------------------------------------------------------------------
;; Reset flow
;; -----------------------------------------------------------------------------

(deftest reset-flow-test
  (testing "reset removes created requests, resets revision, emits reset change and toast"
    (let [created-title "Temporary reset target"]
      (create-request-through-app!
       base-ctx
       (create-params
        {"title" created-title
         "area" "Garden"
         "details" "Should disappear"
         "customer-name" "Temp"}))

      (is (request-by-title created-title))
      (is (> (store/latest-revision) 3))

      (let [notify-calls (atom [])
            reset-toasts (atom 0)]
        (with-app-render-stubs
          (with-redefs [hh-live/notify! (notify-recorder notify-calls)
                        hh-live/send-reset-toast! (reset-toast-recorder reset-toasts)]
            (let [response (app/reset-demo! base-ctx)]
              (is (html-response? response))
              (is (= 3 (store/latest-revision)))
              (is (nil? (request-by-title created-title)))
              (is (= 1 @reset-toasts))
              (is (= 1 (count @notify-calls)))
              (is (= :humanhelp-demo/reset
                     (:topic (nth (first @notify-calls) 2))))
              (is (oob-response-for? response views/request-toolbar-dom-id))
              (is (oob-response-for? response views/request-list-dom-id)))))))))

;; -----------------------------------------------------------------------------
;; Route/view/live wiring flow
;; -----------------------------------------------------------------------------

(deftest fragment-url-flow-test
  (testing "live fragment options agree with route builders"
    (let [state {:search "garden"
                 :selected-request-id "hh-req-1"
                 :visible-revision 3}
          toolbar-options (hh-live/fragment-options :request-toolbar state)
          list-options (hh-live/fragment-options :request-list state)]
      (is (= (routes/request-toolbar-fragment-url state)
             (:fragment-url toolbar-options)))
      (is (= (routes/request-toolbar-stream-url state)
             (:stream-url toolbar-options)))
      (is (= (routes/request-list-fragment-url state)
             (:fragment-url list-options)))
      (is (= (routes/request-list-stream-url state)
             (:stream-url list-options))))))

(deftest rendered-fragment-flow-test
  (testing "toolbar/list render through live layer using current store data"
    (let [latest (store/latest-revision)
          toolbar-response (hh-live/render-fragment-response
                            base-ctx
                            :request-toolbar
                            {:user {:user/id "owner"
                                    :user/email "owner@example.com"}
                             :view-state {:search ""
                                          :visible-revision latest}})
          list-response (hh-live/render-fragment-response
                         base-ctx
                         :request-list
                         {:user {:user/id "owner"
                                 :user/email "owner@example.com"}
                          :view-state {:search ""
                                       :visible-revision latest}})]
      (is (html-response? toolbar-response))
      (is (html-response? list-response))
      (is (body-contains? toolbar-response views/request-toolbar-dom-id))
      (is (body-contains? list-response views/request-list-dom-id)))))
