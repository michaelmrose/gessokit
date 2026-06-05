(ns gessokit.humanhelp.flow-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gessokit.humanhelp.app :as app]
   [gessokit.humanhelp.data :as data]
   [gessokit.humanhelp.live :as hh-live]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.views :as views]
   [xtdb.node :as xtn]))

;; -----------------------------------------------------------------------------
;; XTDB fixture
;; -----------------------------------------------------------------------------

(defonce !ctx-base
  (atom nil))

(defn ctx-base
  []
  (or @!ctx-base
      (throw
       (ex-info "flow-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx-base {:biff/node node
                       :biff/conn node
                       :xtdb/node node})
    (try
      (f)
      (finally
        (reset! !ctx-base nil)))))

(defn base-ctx
  []
  (merge
   (ctx-base)
   {:anti-forgery-token "test-token"
    :gesso.live/system ::live-system
    :user/id "owner"
    :user/email "owner@example.com"
    :session {:uid "owner"
              :email "owner@example.com"}}))

(defn helper-ctx
  []
  (assoc (base-ctx)
         :user/id "helper"
         :user/email "helper@example.com"
         :session {:uid "helper"
                   :email "helper@example.com"}))

(defn other-ctx
  []
  (assoc (base-ctx)
         :user/id "other"
         :user/email "other@example.com"
         :session {:uid "other"
                   :email "other@example.com"}))

(defn reset-data-fixture
  [f]
  (data/reset-demo-state! (base-ctx))
  (try
    (f)
    (finally
      (data/reset-demo-state! (base-ctx)))))

(use-fixtures :once xtdb-fixture)
(use-fixtures :each reset-data-fixture)

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
  (boolean
   (str/includes? (or (response-body response) "") text)))

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

(defn ctx-with-selected-request
  [ctx request-id]
  (assoc ctx
         :path-params {:request-id request-id}
         :params {"selected" request-id}))

(defn create-params
  [overrides]
  (merge
   {"title" "Need help finding a rake"
    "area" "Garden"
    "details" "Looking for a sturdy rake for leaves."
    "customer-name" "Jon"}
   overrides))

(defn request-by-title
  [ctx title]
  (first
   (filter #(= title (:request/title %))
           (data/all-requests ctx))))

(defn request-status
  [ctx request-id]
  (:request/status (data/request-by-id ctx request-id)))

(defn request-claimer
  [ctx request-id]
  (:request/claimed-by (data/request-by-id ctx request-id)))

(defn request-titles
  [requests]
  (set (map :request/title requests)))

(defn without-nil-vals
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn board-titles
  [ctx view-state]
  (request-titles
   (:requests (data/board-data ctx view-state))))

(defn open-seed-request
  [ctx]
  (first
   (filter #(= :open (:request/status %))
           (data/all-requests ctx))))

(defn owner-ctx-for
  [ctx request]
  (assoc ctx
         :user/id (:request/customer-user-id request)
         :user/email (str (:request/customer-user-id request)
                          "@example.com")
         :session {:uid (:request/customer-user-id request)
                   :email (str (:request/customer-user-id request)
                               "@example.com")}))

(defn notify-recorder
  [calls]
  (fn [& args]
    (swap! calls conj args)
    {:submitted true}))

(defn request-toast-recorder
  [calls]
  (fn [request user]
    (swap! calls conj {:request request
                       :user user})
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
                 app/send-new-request-toast-safely!
                 (request-toast-recorder ~request-toasts)]
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
    (let [ctx (base-ctx)
          latest (data/latest-revision ctx)
          toolbar (data/toolbar-data
                   ctx
                   {:search ""
                    :visible-revision latest})
          board (data/board-data
                 ctx
                 {:search ""
                  :visible-revision latest})]
      (is (= 3 latest))
      (is (= latest (:latest-revision toolbar)))
      (is (= latest (:latest-revision board)))
      (is (seq (:requests board)))
      (is (= (data/open-request-count ctx) (:open-count toolbar)))
      (is (= (data/open-request-count ctx) (:open-count board)))
      (is (false? (:stale? toolbar)))
      (is (false? (:stale? board))))))

(deftest initial-page-flow-test
  (testing "app page data can be built from a normal signed-in ctx"
    (let [ctx (base-ctx)
          page-data (app/page-data ctx)]
      (is (= {:user/id "owner"
              :user/email "owner@example.com"}
             (:user page-data)))
      (is (map? (:view-state page-data)))
      (is (:request-toolbar-panel page-data))
      (is (:request-list-panel page-data))
      (is (= (data/latest-revision ctx)
             (get-in page-data [:view-state :visible-revision]))))))

(deftest initial-live-panel-flow-test
  (testing "page data uses stable app-owned live wrappers"
    (let [page-data (app/page-data (base-ctx))
          toolbar-panel (:request-toolbar-panel page-data)
          list-panel (:request-list-panel page-data)]
      (is (= "sse" (get-in toolbar-panel [1 :hx-ext])))
      (is (= "sse" (get-in list-panel [1 :hx-ext])))
      (is (str/includes? (get-in toolbar-panel [1 :hx-trigger])
                         "sse:live-update"))
      (is (str/includes? (get-in list-panel [1 :hx-trigger])
                         "sse:live-update"))
      (is (= (str "#" views/board-state-form-id)
             (get-in toolbar-panel [1 :hx-include])))
      (is (= (str "#" views/board-state-form-id)
             (get-in list-panel [1 :hx-include]))))))

;; -----------------------------------------------------------------------------
;; Create request flow
;; -----------------------------------------------------------------------------

(deftest create-request-valid-flow-test
  (testing "valid app create mutates store, emits live change, sends toast, and returns OOB"
    (let [ctx (base-ctx)
          before-revision (data/latest-revision ctx)
          title "Need help finding purple gloves"
          params (create-params
                  {"title" title
                   "area" "Garden"
                   "details" "Large purple gloves"
                   "customer-name" "Avery"})
          result (create-request-through-app! ctx params)
          response (:response result)
          created (request-by-title ctx title)]
      (is (html-response? response))
      (is created)
      (is (= (inc before-revision) (data/latest-revision ctx)))
      (is (= :open (:request/status created)))
      (is (= "owner" (:request/customer-user-id created)))
      (is (= "Avery" (:request/customer-name created)))

      (is (= 1 (count (:notify-calls result))))
      (let [[live-system ctx' change] (first (:notify-calls result))]
        (is (= ::live-system live-system))
        (is (= (ctx-with-params ctx params) ctx'))
        (is (= :request/created (:topic change)))
        (is (= model/store-id (:id change)))
        (is (= model/store-id (:store/id change)))
        (is (= (:request/id created) (:request/id change)))
        (is (= "owner" (:actor/id change)))
        (is (= "owner@example.com" (:actor/email change))))

      (is (= 1 (count (:request-toasts result))))
      (let [{:keys [request user]} (first (:request-toasts result))]
        (is (= created (without-nil-vals request)))
        (is (= {:user/id "owner"
                :user/email "owner@example.com"}
               user)))

      (is (oob-response-for? response views/request-toolbar-dom-id))
      (is (oob-response-for? response views/request-list-dom-id))
      (is (oob-response-for? response views/create-request-dialog-id))
      (is (oob-response-for? response views/board-state-form-id)))))

(deftest create-request-invalid-flow-test
  (testing "invalid app create does not mutate, notify, or toast"
    (let [ctx (base-ctx)
          before-revision (data/latest-revision ctx)
          before-requests (data/all-requests ctx)
          result (create-request-through-app!
                  ctx
                  {"title" ""
                   "area" ""})
          response (:response result)]
      (is (html-response? response))
      (is (= before-revision (data/latest-revision ctx)))
      (is (= before-requests (data/all-requests ctx)))
      (is (empty? (:notify-calls result)))
      (is (empty? (:request-toasts result)))
      (is (oob-response-for? response views/create-request-dialog-id))
      (is (not (oob-response-for? response views/request-toolbar-dom-id)))
      (is (not (oob-response-for? response views/request-list-dom-id)))
      (is (body-contains? response "Create request")))))

;; -----------------------------------------------------------------------------
;; Two-viewer stale/refresh semantics
;; -----------------------------------------------------------------------------

(deftest two-viewer-create-refresh-flow-test
  (testing "other viewer gets stale board semantics until refresh"
    (let [owner (base-ctx)
          helper (helper-ctx)
          viewer-b-visible-revision (data/latest-revision helper)
          title "Need help loading cedar mulch"
          create-result (create-request-through-app!
                         owner
                         (create-params
                          {"title" title
                           "area" "Garden"
                           "details" "Five bags near entrance"
                           "customer-name" "Mina"}))
          latest (data/latest-revision helper)
          stale-toolbar (data/toolbar-data
                         helper
                         {:search ""
                          :visible-revision viewer-b-visible-revision})
          stale-board (data/board-data
                       helper
                       {:search ""
                        :visible-revision viewer-b-visible-revision})
          fresh-board (data/board-data
                       helper
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
                              helper
                              {"visible-revision"
                               (str viewer-b-visible-revision)})]
        (is (html-response? refresh-response))
        (is (oob-response-for? refresh-response
                               views/request-toolbar-dom-id))
        (is (oob-response-for? refresh-response
                               views/request-list-dom-id))
        (is (oob-response-for? refresh-response
                               views/board-state-form-id))
        (is (body-contains?
             refresh-response
             (str "name=\"visible-revision\" value=\""
                  latest
                  "\"")))))))

(deftest create-refresh-with-search-flow-test
  (testing "refresh preserves search while advancing visible revision"
    (let [ctx (base-ctx)
          old-revision (data/latest-revision ctx)
          title "Need help finding a blue snow shovel"]
      (create-request-through-app!
       ctx
       (create-params
        {"title" title
         "area" "Seasonal"
         "details" "Blue shovel near front"
         "customer-name" "Nora"}))

      (let [stale-matching (data/board-data
                            ctx
                            {:search "blue shovel nora"
                             :visible-revision old-revision})
            fresh-matching (data/board-data
                            ctx
                            {:search "blue shovel nora"
                             :visible-revision (data/latest-revision ctx)})]
        (is (not (contains? (request-titles (:requests stale-matching))
                            title)))
        (is (contains? (request-titles (:requests fresh-matching))
                       title))))))

(deftest created-request-does-not-jump-other-viewers-list-flow-test
  (testing "a stale request-list fragment does not reveal a newly-created request"
    (let [owner (base-ctx)
          helper (helper-ctx)
          visible-before (data/latest-revision helper)
          title "Need help finding lime green twine"]
      (create-request-through-app!
       owner
       (create-params
        {"title" title
         "area" "Garden"
         "details" "Small spool"
         "customer-name" "Mina"}))

      (let [stale-response (app/request-list-fragment
                            (ctx-with-params
                             helper
                             {"q" ""
                              "visible-revision" (str visible-before)}))
            toolbar-response (app/request-toolbar-fragment
                              (ctx-with-params
                               helper
                               {"q" ""
                                "visible-revision" (str visible-before)}))]
        (is (html-response? stale-response))
        (is (html-response? toolbar-response))
        (is (not (body-contains? stale-response title)))
        (is (body-contains? toolbar-response "+1 new"))
        (is (body-contains? toolbar-response
                            "New request data is available"))))))

;; -----------------------------------------------------------------------------
;; Search flow
;; -----------------------------------------------------------------------------

(deftest search-flow-test
  (testing "search terms match collectively across customer, title, area, and details"
    (let [ctx (base-ctx)
          title "Need help finding a long-handled rake"]
      (create-request-through-app!
       ctx
       (create-params
        {"title" title
         "area" "Garden"
         "details" "Customer is comparing bark tools"
         "customer-name" "Jon"}))

      (let [latest (data/latest-revision ctx)]
        (is (contains? (board-titles ctx
                                     {:search "jon rake garden"
                                      :visible-revision latest})
                       title))
        (is (contains? (board-titles ctx
                                     {:search "jon bark rake garden"
                                      :visible-revision latest})
                       title))
        (is (not (contains? (board-titles
                              ctx
                              {:search "jon bark rake garden purple"
                               :visible-revision latest})
                             title)))))))

(deftest search-handler-flow-test
  (testing "search handler renders only the list fragment response"
    (let [ctx (base-ctx)
          response (app/search-requests
                    (ctx-with-params
                     ctx
                     {"q" "garden"
                      "visible-revision"
                      (str (data/latest-revision ctx))}))]
      (is (html-response? response))
      (is (body-contains? response views/request-list-dom-id))
      (is (not (body-contains? response views/request-toolbar-dom-id))))))

(deftest search-handler-respects-visible-revision-flow-test
  (testing "search does not bypass stale visible revision filtering"
    (let [ctx (base-ctx)
          visible-before (data/latest-revision ctx)
          title "Need help locating chartreuse grout"]
      (create-request-through-app!
       ctx
       (create-params
        {"title" title
         "area" "Tile"
         "details" "Chartreuse only"
         "customer-name" "Mina"}))

      (let [stale-response (app/search-requests
                            (ctx-with-params
                             ctx
                             {"q" "chartreuse grout mina"
                              "visible-revision" (str visible-before)}))
            fresh-response (app/search-requests
                            (ctx-with-params
                             ctx
                             {"q" "chartreuse grout mina"
                              "visible-revision"
                              (str (data/latest-revision ctx))}))]
        (is (html-response? stale-response))
        (is (html-response? fresh-response))
        (is (not (body-contains? stale-response title)))
        (is (body-contains? fresh-response title))))))

;; -----------------------------------------------------------------------------
;; Selection flow
;; -----------------------------------------------------------------------------

(deftest select-request-flow-test
  (testing "select handler expands a visible card by setting selected request id"
    (let [ctx (base-ctx)
          request (first (data/all-requests ctx))
          ctx' (-> ctx
                   (ctx-with-request-id (:request/id request))
                   (assoc :params {"selected" (:request/id request)
                                   "visible-revision"
                                   (str (data/latest-revision ctx))}))
          response (app/select-request ctx')]
      (is (html-response? response))
      (is (body-contains? response views/request-list-dom-id))
      (is (body-contains? response (:request/title request)))
      (is (or (body-contains? response (:request/details request))
              (not (model/present? (:request/details request)))))
      (is (oob-response-for? response views/board-state-form-id))
      (is (body-contains?
           response
           (str "name=\"selected\" value=\""
                (:request/id request)
                "\""))))))

;; -----------------------------------------------------------------------------
;; Lifecycle flows
;; -----------------------------------------------------------------------------

(deftest claim-unclaim-flow-test
  (testing "helper can claim an open request and then unclaim it"
    (let [owner (base-ctx)
          helper (helper-ctx)
          created-title "Need help picking paint brushes"
          create-result (create-request-through-app!
                         owner
                         (create-params
                          {"title" created-title
                           "area" "Paint"
                           "details" "Two inch angled brush"
                           "customer-name" "Pat"}))
          request-id (:request/id (request-by-title owner created-title))]
      (is (html-response? (:response create-result)))

      (let [claim-result (lifecycle-through-app!
                          app/claim-request!
                          helper
                          request-id)]
        (is (html-response? (:response claim-result)))
        (is (= :claimed (request-status helper request-id)))
        (is (= "helper" (request-claimer helper request-id)))
        (is (= 1 (count (:notify-calls claim-result))))
        (is (= :request/claimed
               (:topic (nth (first (:notify-calls claim-result)) 2))))
        (is (oob-response-for? (:response claim-result)
                               views/request-toolbar-dom-id))
        (is (oob-response-for? (:response claim-result)
                               views/request-list-dom-id))
        (is (oob-response-for? (:response claim-result)
                               views/board-state-form-id)))

      (let [unclaim-result (lifecycle-through-app!
                            app/unclaim-request!
                            helper
                            request-id)]
        (is (html-response? (:response unclaim-result)))
        (is (= :open (request-status helper request-id)))
        (is (nil? (request-claimer helper request-id)))
        (is (= 1 (count (:notify-calls unclaim-result))))
        (is (= :request/unclaimed
               (:topic (nth (first (:notify-calls unclaim-result)) 2))))))))

(deftest take-over-flow-test
  (testing "another user can take over a claimed request"
    (let [owner (base-ctx)
          helper (helper-ctx)
          other (other-ctx)
          created-title "Need help loading plywood"
          _create-result (create-request-through-app!
                          owner
                          (create-params
                           {"title" created-title
                            "area" "Lumber"
                            "details" "Three sheets"
                            "customer-name" "Robin"}))
          request-id (:request/id (request-by-title owner created-title))]
      (let [claim-result (lifecycle-through-app!
                          app/claim-request!
                          helper
                          request-id)]
        (is (html-response? (:response claim-result)))
        (is (= :claimed (request-status helper request-id)))
        (is (= "helper" (request-claimer helper request-id)))
        (is (= :request/claimed
               (:topic (nth (first (:notify-calls claim-result)) 2)))))

      (let [take-over-result (lifecycle-through-app!
                              app/take-over-request!
                              other
                              request-id)]
        (is (html-response? (:response take-over-result)))
        (is (= :claimed (request-status other request-id)))
        (is (= "other" (request-claimer other request-id)))
        (is (= :request/taken-over
               (:topic (nth (first (:notify-calls take-over-result)) 2))))))))

(deftest done-flow-test
  (testing "owner can mark their own open request done"
    (let [ctx (base-ctx)
          created-title "Need help finding caulk"
          _create-result (create-request-through-app!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "White kitchen caulk"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          done-result (lifecycle-through-app!
                       app/mark-request-done!
                       ctx
                       request-id)]
      (is (html-response? (:response done-result)))
      (is (= :done (request-status ctx request-id)))
      (is (= :request/done
             (:topic (nth (first (:notify-calls done-result)) 2)))))))

(deftest cancel-flow-test
  (testing "owner can cancel their own open request"
    (let [ctx (base-ctx)
          created-title "Need help finding return desk"
          _create-result (create-request-through-app!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Customer service"
                            "details" "Wrong receipt"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          cancel-result (lifecycle-through-app!
                         app/cancel-request!
                         ctx
                         request-id)]
      (is (html-response? (:response cancel-result)))
      (is (= :cancelled (request-status ctx request-id)))
      (is (= :request/cancelled
             (:topic (nth (first (:notify-calls cancel-result)) 2)))))))

(deftest forbidden-action-flow-test
  (testing "owner cannot claim their own open request"
    (let [ctx (base-ctx)
          created-title "Need help finding nails"
          _create-result (create-request-through-app!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "Finish nails"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          result (lifecycle-through-app!
                  app/claim-request!
                  ctx
                  request-id)]
      (is (html-response? (:response result)))
      (is (= :open (request-status ctx request-id)))
      (is (empty? (:notify-calls result)))
      (is (body-contains? (:response result) "Request not updated")))))

(deftest missing-request-action-flow-test
  (testing "missing request action returns an error response and does not notify"
    (let [result (lifecycle-through-app!
                  app/claim-request!
                  (helper-ctx)
                  "missing-request")]
      (is (html-response? (:response result)))
      (is (empty? (:notify-calls result)))
      (is (body-contains? (:response result) "Request not updated")))))

(deftest lifecycle-auto-refresh-visible-request-flow-test
  (testing "a lifecycle live refresh can update an already-visible request at the old visible revision"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          open-request (open-seed-request ctx)
          visible-before (data/latest-revision ctx)
          request-id (:request/id open-request)
          claim-result (lifecycle-through-app!
                        app/claim-request!
                        helper
                        request-id)]
      (is (html-response? (:response claim-result)))
      (is (= :claimed (request-status helper request-id)))

      ;; This mirrors the browser's automatic fragment GET after
      ;; sse:live-update. The visible revision is still the older revision,
      ;; but this request was already visible then, so the refreshed card should
      ;; show the updated lifecycle state.
      (let [fragment-response (app/request-list-fragment
                               (assoc helper
                                      :params {"q" ""
                                               "selected" request-id
                                               "visible-revision"
                                               (str visible-before)}))]
        (is (html-response? fragment-response))
        (is (body-contains? fragment-response views/request-list-dom-id))
        (is (body-contains? fragment-response (:request/title open-request)))
        (is (body-contains? fragment-response "claimed by helper@example.com"))))))

;; -----------------------------------------------------------------------------
;; Reset flow
;; -----------------------------------------------------------------------------

(deftest reset-flow-test
  (testing "reset removes created requests, resets revision, emits reset change and toast"
    (let [ctx (base-ctx)
          created-title "Temporary reset target"]
      (create-request-through-app!
       ctx
       (create-params
        {"title" created-title
         "area" "Garden"
         "details" "Should disappear"
         "customer-name" "Temp"}))

      (is (request-by-title ctx created-title))
      (is (> (data/latest-revision ctx) 3))

      (let [notify-calls (atom [])
            reset-toasts (atom 0)]
        (with-app-render-stubs
          (with-redefs [hh-live/notify! (notify-recorder notify-calls)
                        app/send-reset-toast-safely!
                        (reset-toast-recorder reset-toasts)]
            (let [response (app/reset-demo! ctx)]
              (is (html-response? response))
              (is (= 3 (data/latest-revision ctx)))
              (is (nil? (request-by-title ctx created-title)))
              (is (= 1 @reset-toasts))
              (is (= 1 (count @notify-calls)))

              (let [[live-system ctx' change] (first @notify-calls)]
                (is (= ::live-system live-system))
                (is (= ctx ctx'))
                (is (= :humanhelp-demo/reset (:topic change)))
                (is (= model/store-id (:id change)))
                (is (= model/store-id (:store/id change)))
                (is (= 3 (:revision change))))

              (is (oob-response-for? response views/request-toolbar-dom-id))
              (is (oob-response-for? response views/request-list-dom-id))
              (is (oob-response-for? response views/board-state-form-id)))))))))

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
    (let [ctx (base-ctx)
          latest (data/latest-revision ctx)
          toolbar-response (hh-live/render-fragment-response
                            ctx
                            :request-toolbar
                            {:user {:user/id "owner"
                                    :user/email "owner@example.com"}
                             :view-state {:search ""
                                          :visible-revision latest}})
          list-response (hh-live/render-fragment-response
                         ctx
                         :request-list
                         {:user {:user/id "owner"
                                 :user/email "owner@example.com"}
                          :view-state {:search ""
                                       :visible-revision latest}})]
      (is (html-response? toolbar-response))
      (is (html-response? list-response))
      (is (body-contains? toolbar-response views/request-toolbar-dom-id))
      (is (body-contains? list-response views/request-list-dom-id)))))

(deftest rendered-fragment-stale-refresh-flow-test
  (testing "live layer renders stale toolbar but keeps list behind old visible revision"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          visible-before (data/latest-revision ctx)
          title "Need help finding copper pipe"]
      (create-request-through-app!
       ctx
       (create-params
        {"title" title
         "area" "Plumbing"
         "details" "Half inch copper"
         "customer-name" "Nora"}))

      (let [toolbar-response (hh-live/render-fragment-response
                              helper
                              :request-toolbar
                              {:user {:user/id "helper"
                                      :user/email "helper@example.com"}
                               :view-state {:search ""
                                            :visible-revision visible-before}})
            list-response (hh-live/render-fragment-response
                           helper
                           :request-list
                           {:user {:user/id "helper"
                                   :user/email "helper@example.com"}
                            :view-state {:search ""
                                         :visible-revision visible-before}})]
        (is (html-response? toolbar-response))
        (is (html-response? list-response))
        (is (body-contains? toolbar-response "+1 new"))
        (is (not (body-contains? list-response title)))))))
