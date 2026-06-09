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
   [gessokit.middleware :as mid]
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
;; Route dispatch helpers
;; -----------------------------------------------------------------------------

(defn route-pairs
  [route-tree]
  (filter
   (fn [x]
     (and (vector? x)
          (string? (first x))
          (map? (second x))))
   (tree-seq
    (fn [x]
      (and (sequential? x)
           (not (string? x))))
    seq
    route-tree)))

(defn route-strings
  [route-tree]
  (set
   (filter string?
           (tree-seq
            (fn [x]
              (and (sequential? x)
                   (not (string? x))))
            seq
            route-tree))))

(defn route-map-for
  [route]
  (second
   (first
    (filter #(= route (first %))
            (route-pairs (:routes app/module))))))

(defn route-handler
  [method route]
  (or (get (route-map-for route) method)
      (throw
       (ex-info "No mounted Human Help route handler."
                {:method method
                 :route route
                 :route-map (route-map-for route)}))))

(defn ctx-with-route-data
  [ctx {:keys [params path-params]}]
  (cond-> ctx
    (some? params)
    (assoc :params params)

    (some? path-params)
    (assoc :path-params path-params
           :reitit.core/match {:path-params path-params})))

(defn endpoint!
  ([method route ctx]
   (endpoint! method route ctx {}))
  ([method route ctx route-data]
   ((route-handler method route)
    (ctx-with-route-data ctx route-data))))

;; -----------------------------------------------------------------------------
;; Response helpers
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
   (str/includes? (or (response-body response) "")
                  (str text))))

(defn response-full-page?
  [response]
  (let [body (or (response-body response) "")]
    (or (str/includes? body "<body")
        (str/includes? body "<main")
        (str/includes? body "data-bars-root")
        (str/includes? body "min-h-screen flex flex-col")
        (str/includes? body "Welcome to Human Help."))))

(defn assert-html-response
  [response]
  (is (html-response? response)
      (pr-str {:status (:status response)
               :headers (:headers response)
               :body-prefix (subs (or (:body response) "")
                                  0
                                  (min 240 (count (or (:body response) ""))))})))

(defn assert-not-full-page-response
  [response]
  (is (not (response-full-page? response))
      "Fragment/action endpoint returned page-shell/full-page-looking HTML."))

(defn assert-fragment-or-oob-response
  [response]
  (assert-html-response response)
  (assert-not-full-page-response response))

(defn oob-response-for?
  [response dom-id]
  (and (body-contains? response (str "id=\"" dom-id "\""))
       (body-contains? response "hx-swap-oob=\"outerHTML\"")))

(defn assert-oob
  [response dom-id]
  (is (oob-response-for? response dom-id)
      (str "Missing OOB replacement for #" dom-id)))

(defn assert-not-oob
  [response dom-id]
  (is (not (oob-response-for? response dom-id))
      (str "Unexpected OOB replacement for #" dom-id)))

(defn assert-fragment-root
  [response dom-id]
  (assert-fragment-or-oob-response response)
  (is (body-contains? response (str "id=\"" dom-id "\""))))

(defn terminal-action-url-present?
  [response request-id]
  (some
   #(body-contains? response (routes/action-url request-id %))
   [:claim :unclaim :take-over :done :cancel]))

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------

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

(defn request-claimer-email
  [ctx request-id]
  (:request/claimed-by-email (data/request-by-id ctx request-id)))

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

(defn latest-view-state
  [ctx]
  {:search ""
   :selected-request-id nil
   :visible-revision (data/latest-revision ctx)})

(defn open-seed-request
  [ctx]
  (first
   (filter #(= :open (:request/status %))
           (data/all-requests ctx))))

(defn selected-params
  [ctx request-id]
  {"q" ""
   "selected" request-id
   "visible-revision" (str (data/latest-revision ctx))})

;; -----------------------------------------------------------------------------
;; Side-effect recorders
;; -----------------------------------------------------------------------------

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

(defmacro with-notify-recorder
  [notify-calls & body]
  `(with-redefs [hh-live/notify! (notify-recorder ~notify-calls)]
     ~@body))

(defmacro with-create-side-effect-recorders
  [notify-calls request-toasts & body]
  `(with-redefs [hh-live/notify! (notify-recorder ~notify-calls)
                 app/send-new-request-toast-safely!
                 (request-toast-recorder ~request-toasts)]
     ~@body))

(defmacro with-reset-side-effect-recorders
  [notify-calls reset-toasts & body]
  `(with-redefs [hh-live/notify! (notify-recorder ~notify-calls)
                 app/send-reset-toast-safely!
                 (reset-toast-recorder ~reset-toasts)]
     ~@body))

(defn create-request-through-route!
  [ctx params]
  (let [notify-calls (atom [])
        request-toasts (atom [])]
    (with-create-side-effect-recorders notify-calls request-toasts
      {:response (endpoint!
                  :post
                  routes/create-request-route
                  ctx
                  {:params params})
       :notify-calls @notify-calls
       :request-toasts @request-toasts})))

(defn lifecycle-through-route!
  [route ctx request-id]
  (let [notify-calls (atom [])]
    (with-notify-recorder notify-calls
      {:response (endpoint!
                  :post
                  route
                  ctx
                  {:path-params {:request-id request-id}
                   :params (selected-params ctx request-id)})
       :notify-calls @notify-calls})))

(defn refresh-through-route!
  [ctx params]
  (endpoint!
   :post
   routes/refresh-requests-route
   ctx
   {:params params}))

(def lifecycle-routes
  {:claim routes/claim-request-route
   :unclaim routes/unclaim-request-route
   :take-over routes/take-over-request-route
   :done routes/done-request-route
   :cancel routes/cancel-request-route})

(def expected-change-topic
  {:claim :request/claimed
   :unclaim :request/unclaimed
   :take-over :request/taken-over
   :done :request/done
   :cancel :request/cancelled})

(defn lifecycle-action-through-route!
  [action ctx request-id]
  (lifecycle-through-route!
   (get lifecycle-routes action)
   ctx
   request-id))

(defn first-change-topic
  [result]
  (:topic (nth (first (:notify-calls result)) 2)))

;; -----------------------------------------------------------------------------
;; Module/route wiring
;; -----------------------------------------------------------------------------

(deftest module-route-shape-test
  (testing "module exports live rules and mounts under the Human Help base path"
    (is (= hh-live/live-rules (:live-rules app/module)))
    (let [route-tree (first (:routes app/module))]
      (is (= routes/base-path (first route-tree)))
      (is (= {:middleware [mid/wrap-signed-in]}
             (second route-tree)))))

  (testing "all expected app routes are mounted"
    (let [strings (route-strings (:routes app/module))]
      (doseq [route [routes/base-path
                     routes/request-toolbar-fragment-route
                     routes/request-list-fragment-route
                     routes/create-request-dialog-fragment-route
                     routes/request-toolbar-stream-route
                     routes/request-list-stream-route
                     routes/create-request-route
                     routes/refresh-requests-route
                     routes/search-requests-route
                     routes/select-request-route
                     routes/claim-request-route
                     routes/unclaim-request-route
                     routes/take-over-request-route
                     routes/done-request-route
                     routes/cancel-request-route
                     routes/reset-demo-route]]
        (is (contains? strings route)
            (str "Missing route: " route)))))

  (testing "mounted handlers are the app boundary functions"
    (is (= {:get app/app-page}
           (route-map-for "")))
    (is (= {:get app/request-toolbar-fragment}
           (route-map-for routes/request-toolbar-fragment-route)))
    (is (= {:get app/request-list-fragment}
           (route-map-for routes/request-list-fragment-route)))
    (is (= {:get app/create-request-dialog-fragment}
           (route-map-for routes/create-request-dialog-fragment-route)))
    (is (= {:post app/create-request!}
           (route-map-for routes/create-request-route)))
    (is (= {:post app/refresh-requests!}
           (route-map-for routes/refresh-requests-route)))
    (is (= {:get app/search-requests}
           (route-map-for routes/search-requests-route)))
    (is (= {:get app/select-request}
           (route-map-for routes/select-request-route)))
    (is (= {:post app/claim-request!}
           (route-map-for routes/claim-request-route)))
    (is (= {:post app/unclaim-request!}
           (route-map-for routes/unclaim-request-route)))
    (is (= {:post app/take-over-request!}
           (route-map-for routes/take-over-request-route)))
    (is (= {:post app/mark-request-done!}
           (route-map-for routes/done-request-route)))
    (is (= {:post app/cancel-request!}
           (route-map-for routes/cancel-request-route)))
    (is (= {:post app/reset-demo!}
           (route-map-for routes/reset-demo-route)))))

;; -----------------------------------------------------------------------------
;; Initial page/fragment flow
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
      (is (nil? (get-in toolbar-panel [1 :sse-swap])))
      (is (nil? (get-in list-panel [1 :sse-swap])))
      (is (str/includes? (get-in toolbar-panel [1 :hx-trigger])
                         "sse:live-update"))
      (is (str/includes? (get-in list-panel [1 :hx-trigger])
                         "sse:live-update"))
      (is (= (str "#" views/board-state-form-id)
             (get-in toolbar-panel [1 :hx-include])))
      (is (= (str "#" views/board-state-form-id)
             (get-in list-panel [1 :hx-include])))
      (is (= (str "#" views/request-toolbar-dom-id)
             (get-in toolbar-panel [1 :hx-target])))
      (is (= (str "#" views/request-list-dom-id)
             (get-in list-panel [1 :hx-target]))))))

(deftest mounted-fragment-endpoint-flow-test
  (testing "toolbar fragment endpoint returns only toolbar HTML"
    (let [ctx (base-ctx)
          response (endpoint!
                    :get
                    routes/request-toolbar-fragment-route
                    ctx
                    {:params {"q" ""
                              "visible-revision"
                              (str (data/latest-revision ctx))}})]
      (assert-fragment-root response views/request-toolbar-dom-id)
      (is (body-contains? response "Requests"))
      (is (not (body-contains? response views/request-list-dom-id)))))

  (testing "request list fragment endpoint returns only list HTML"
    (let [ctx (base-ctx)
          response (endpoint!
                    :get
                    routes/request-list-fragment-route
                    ctx
                    {:params {"q" ""
                              "visible-revision"
                              (str (data/latest-revision ctx))}})]
      (assert-fragment-root response views/request-list-dom-id)
      (is (not (body-contains? response views/request-toolbar-dom-id)))))

  (testing "create dialog fragment endpoint returns only dialog HTML"
    (let [response (endpoint!
                    :get
                    routes/create-request-dialog-fragment-route
                    (base-ctx))]
      (assert-fragment-root response views/create-request-dialog-id)
      (is (body-contains? response "Create request"))
      (is (body-contains? response "open")))))

;; -----------------------------------------------------------------------------
;; Create request flow
;; -----------------------------------------------------------------------------

(deftest create-request-valid-flow-test
  (testing "valid mounted create route mutates store, emits live change, sends toast, and returns OOB"
    (let [ctx (base-ctx)
          before-revision (data/latest-revision ctx)
          before-count (count (data/all-requests ctx))
          title "Need help finding purple gloves"
          params (create-params
                  {"title" title
                   "area" "Garden"
                   "details" "Large purple gloves"
                   "customer-name" "Avery"})
          result (create-request-through-route! ctx params)
          response (:response result)
          created (request-by-title ctx title)]
      (assert-fragment-or-oob-response response)
      (is created)
      (is (= (inc before-count) (count (data/all-requests ctx))))
      (is (= (inc before-revision) (data/latest-revision ctx)))
      (is (= :open (:request/status created)))
      (is (= "owner" (:request/customer-user-id created)))
      (is (= "Avery" (:request/customer-name created)))
      (is (= "Garden" (:request/area created)))
      (is (= "Large purple gloves" (:request/details created)))

      (is (= 1 (count (:notify-calls result))))
      (let [[live-system ctx' change] (first (:notify-calls result))]
        (is (= ::live-system live-system))
        (is (= (assoc ctx :params params) ctx'))
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

      (assert-oob response views/request-toolbar-dom-id)
      (assert-oob response views/request-list-dom-id)
      (assert-oob response views/create-request-dialog-id)
      (assert-oob response views/board-state-form-id)
      (is (body-contains? response "Request created")))))

(deftest create-request-repeated-param-flow-test
  (testing "mounted create route uses the final scalar value for repeated params"
    (let [ctx (base-ctx)
          result (create-request-through-route!
                  ctx
                  {"title" ["Old title" "Final title"]
                   "area" ["Old area" "Final area"]
                   "details" ["Old details" "Final details"]
                   "customer-name" ["Old customer" "Final customer"]})
          response (:response result)
          created (request-by-title ctx "Final title")]
      (assert-fragment-or-oob-response response)
      (is created)
      (is (= "Final area" (:request/area created)))
      (is (= "Final details" (:request/details created)))
      (is (= "Final customer" (:request/customer-name created)))
      (is (nil? (request-by-title ctx "Old title"))))))

(deftest create-request-invalid-flow-test
  (testing "invalid mounted create route does not mutate, notify, or toast"
    (let [ctx (base-ctx)
          before-revision (data/latest-revision ctx)
          before-requests (data/all-requests ctx)
          result (create-request-through-route!
                  ctx
                  {"title" ""
                   "area" ""
                   "details" ""
                   "customer-name" ""})
          response (:response result)]
      (assert-fragment-or-oob-response response)
      (is (= before-revision (data/latest-revision ctx)))
      (is (= before-requests (data/all-requests ctx)))
      (is (empty? (:notify-calls result)))
      (is (empty? (:request-toasts result)))
      (assert-oob response views/create-request-dialog-id)
      (assert-not-oob response views/request-toolbar-dom-id)
      (assert-not-oob response views/request-list-dom-id)
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
          create-result (create-request-through-route!
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
      (assert-fragment-or-oob-response (:response create-result))
      (is (= (inc viewer-b-visible-revision) latest))

      (is (true? (:stale? stale-toolbar)))
      (is (pos? (:pending-open-count stale-toolbar)))
      (is (not (contains? (request-titles (:requests stale-board))
                          title)))
      (is (contains? (request-titles (:requests fresh-board))
                     title))

      (let [refresh-response (refresh-through-route!
                              helper
                              {"q" ""
                               "visible-revision"
                               (str viewer-b-visible-revision)})]
        (assert-fragment-or-oob-response refresh-response)
        (assert-oob refresh-response views/request-toolbar-dom-id)
        (assert-oob refresh-response views/request-list-dom-id)
        (assert-oob refresh-response views/board-state-form-id)
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
      (create-request-through-route!
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
                             :visible-revision (data/latest-revision ctx)})
            refresh-response (refresh-through-route!
                              ctx
                              {"q" "blue shovel nora"
                               "visible-revision" (str old-revision)})]
        (is (not (contains? (request-titles (:requests stale-matching))
                            title)))
        (is (contains? (request-titles (:requests fresh-matching))
                       title))
        (assert-fragment-or-oob-response refresh-response)
        (is (body-contains? refresh-response "blue shovel nora"))
        (is (body-contains? refresh-response title))))))

(deftest created-request-does-not-jump-other-viewers-list-flow-test
  (testing "a stale request-list fragment does not reveal a newly-created request"
    (let [owner (base-ctx)
          helper (helper-ctx)
          visible-before (data/latest-revision helper)
          title "Need help finding lime green twine"]
      (create-request-through-route!
       owner
       (create-params
        {"title" title
         "area" "Garden"
         "details" "Small spool"
         "customer-name" "Mina"}))

      (let [stale-response (endpoint!
                            :get
                            routes/request-list-fragment-route
                            helper
                            {:params {"q" ""
                                      "visible-revision"
                                      (str visible-before)}})
            toolbar-response (endpoint!
                              :get
                              routes/request-toolbar-fragment-route
                              helper
                              {:params {"q" ""
                                        "visible-revision"
                                        (str visible-before)}})]
        (assert-fragment-root stale-response views/request-list-dom-id)
        (assert-fragment-root toolbar-response views/request-toolbar-dom-id)
        (is (not (body-contains? stale-response title)))
        (is (body-contains? toolbar-response "+1 new"))
        (is (body-contains? toolbar-response
                            "New request data is available"))))))

;; -----------------------------------------------------------------------------
;; Search flow
;; -----------------------------------------------------------------------------

(deftest search-data-flow-test
  (testing "search terms match collectively across customer, title, area, and details"
    (let [ctx (base-ctx)
          title "Need help finding a long-handled rake"]
      (create-request-through-route!
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
  (testing "mounted search route renders only the list fragment response"
    (let [ctx (base-ctx)
          response (endpoint!
                    :get
                    routes/search-requests-route
                    ctx
                    {:params {"q" "garden"
                              "visible-revision"
                              (str (data/latest-revision ctx))}})]
      (assert-fragment-root response views/request-list-dom-id)
      (is (not (body-contains? response views/request-toolbar-dom-id))))))

(deftest search-handler-repeated-param-flow-test
  (testing "mounted search route uses the final q value when params repeat"
    (let [ctx (base-ctx)
          title "Need help finding black pipe insulation"]
      (create-request-through-route!
       ctx
       (create-params
        {"title" title
         "area" "Plumbing"
         "details" "Black foam"
         "customer-name" "Mina"}))

      (let [response (endpoint!
                      :get
                      routes/search-requests-route
                      ctx
                      {:params {"q" ["not-present" "black pipe mina"]
                                "visible-revision"
                                (str (data/latest-revision ctx))}})]
        (assert-fragment-root response views/request-list-dom-id)
        (is (body-contains? response title))))))

(deftest search-handler-respects-visible-revision-flow-test
  (testing "search does not bypass stale visible revision filtering"
    (let [ctx (base-ctx)
          visible-before (data/latest-revision ctx)
          title "Need help locating chartreuse grout"]
      (create-request-through-route!
       ctx
       (create-params
        {"title" title
         "area" "Tile"
         "details" "Chartreuse only"
         "customer-name" "Mina"}))

      (let [stale-response (endpoint!
                            :get
                            routes/search-requests-route
                            ctx
                            {:params {"q" "chartreuse grout mina"
                                      "visible-revision"
                                      (str visible-before)}})
            fresh-response (endpoint!
                            :get
                            routes/search-requests-route
                            ctx
                            {:params {"q" "chartreuse grout mina"
                                      "visible-revision"
                                      (str (data/latest-revision ctx))}})]
        (assert-fragment-root stale-response views/request-list-dom-id)
        (assert-fragment-root fresh-response views/request-list-dom-id)
        (is (not (body-contains? stale-response title)))
        (is (body-contains? fresh-response title))))))

;; -----------------------------------------------------------------------------
;; Selection flow
;; -----------------------------------------------------------------------------

(deftest select-request-flow-test
  (testing "mounted select route updates selected hidden board state and returns list + board-state OOB"
    (let [ctx (base-ctx)
          request (first (data/all-requests ctx))
          request-id (:request/id request)
          response (endpoint!
                    :get
                    routes/select-request-route
                    ctx
                    {:path-params {:request-id request-id}
                     :params {"selected" request-id
                              "visible-revision"
                              (str (data/latest-revision ctx))}})]
      (assert-fragment-or-oob-response response)
      (is (body-contains? response views/request-list-dom-id))
      (is (body-contains? response (:request/title request)))
      (assert-oob response views/board-state-form-id)
      (is (body-contains?
           response
           (str "name=\"selected\" value=\""
                request-id
                "\""))))))

;; -----------------------------------------------------------------------------
;; Lifecycle flows
;; -----------------------------------------------------------------------------

(deftest claim-unclaim-flow-test
  (testing "helper can claim an open request and then unclaim it through mounted routes"
    (let [owner (base-ctx)
          helper (helper-ctx)
          created-title "Need help picking paint brushes"
          create-result (create-request-through-route!
                         owner
                         (create-params
                          {"title" created-title
                           "area" "Paint"
                           "details" "Two inch angled brush"
                           "customer-name" "Pat"}))
          request-id (:request/id (request-by-title owner created-title))]
      (assert-fragment-or-oob-response (:response create-result))

      (let [claim-result (lifecycle-action-through-route!
                          :claim
                          helper
                          request-id)
            response (:response claim-result)]
        (assert-fragment-or-oob-response response)
        (is (= :claimed (request-status helper request-id)))
        (is (= "helper" (request-claimer helper request-id)))
        (is (= "helper@example.com" (request-claimer-email helper request-id)))
        (is (= 1 (count (:notify-calls claim-result))))
        (is (= :request/claimed (first-change-topic claim-result)))
        (assert-oob response views/request-toolbar-dom-id)
        (assert-oob response views/request-list-dom-id)
        (assert-oob response views/board-state-form-id)
        (is (body-contains? response "Claim")))

      (let [unclaim-result (lifecycle-action-through-route!
                            :unclaim
                            helper
                            request-id)
            response (:response unclaim-result)]
        (assert-fragment-or-oob-response response)
        (is (= :open (request-status helper request-id)))
        (is (nil? (request-claimer helper request-id)))
        (is (nil? (request-claimer-email helper request-id)))
        (is (= 1 (count (:notify-calls unclaim-result))))
        (is (= :request/unclaimed (first-change-topic unclaim-result)))
        (assert-oob response views/request-toolbar-dom-id)
        (assert-oob response views/request-list-dom-id)
        (assert-oob response views/board-state-form-id)
        (is (body-contains? response "Unclaim"))))))

(deftest take-over-flow-test
  (testing "another user can take over a claimed request through mounted routes"
    (let [owner (base-ctx)
          helper (helper-ctx)
          other (other-ctx)
          created-title "Need help loading plywood"
          _create-result (create-request-through-route!
                          owner
                          (create-params
                           {"title" created-title
                            "area" "Lumber"
                            "details" "Three sheets"
                            "customer-name" "Robin"}))
          request-id (:request/id (request-by-title owner created-title))]
      (let [claim-result (lifecycle-action-through-route!
                          :claim
                          helper
                          request-id)]
        (assert-fragment-or-oob-response (:response claim-result))
        (is (= :claimed (request-status helper request-id)))
        (is (= "helper" (request-claimer helper request-id)))
        (is (= :request/claimed (first-change-topic claim-result))))

      (let [take-over-result (lifecycle-action-through-route!
                              :take-over
                              other
                              request-id)
            response (:response take-over-result)]
        (assert-fragment-or-oob-response response)
        (is (= :claimed (request-status other request-id)))
        (is (= "other" (request-claimer other request-id)))
        (is (= "other@example.com" (request-claimer-email other request-id)))
        (is (= 1 (count (:notify-calls take-over-result))))
        (is (= :request/taken-over (first-change-topic take-over-result)))
        (assert-oob response views/request-toolbar-dom-id)
        (assert-oob response views/request-list-dom-id)
        (assert-oob response views/board-state-form-id)
        (is (body-contains? response "Take over"))))))

(deftest done-flow-test
  (testing "owner can mark their own open request done through mounted route"
    (let [ctx (base-ctx)
          created-title "Need help finding caulk"
          _create-result (create-request-through-route!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "White kitchen caulk"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          done-result (lifecycle-action-through-route!
                       :done
                       ctx
                       request-id)
          response (:response done-result)]
      (assert-fragment-or-oob-response response)
      (is (= :done (request-status ctx request-id)))
      (is (= :request/done (first-change-topic done-result)))
      (assert-oob response views/request-toolbar-dom-id)
      (assert-oob response views/request-list-dom-id)
      (assert-oob response views/board-state-form-id)
      (is (body-contains? response "Done"))
      (is (contains? (board-titles ctx (latest-view-state ctx))
                     created-title))
      (is (body-contains? response created-title))
      (is (not (terminal-action-url-present? response request-id))))))

(deftest claimed-request-done-flow-test
  (testing "claimer can mark a claimed request done through mounted route"
    (let [owner (base-ctx)
          helper (helper-ctx)
          created-title "Need help carrying mulch"
          _create-result (create-request-through-route!
                          owner
                          (create-params
                           {"title" created-title
                            "area" "Garden"
                            "details" "Three bags"
                            "customer-name" "Ira"}))
          request-id (:request/id (request-by-title owner created-title))]
      (lifecycle-action-through-route! :claim helper request-id)
      (let [done-result (lifecycle-action-through-route!
                         :done
                         helper
                         request-id)]
        (assert-fragment-or-oob-response (:response done-result))
        (is (= :done (request-status helper request-id)))
        (is (= :request/done (first-change-topic done-result)))))))

(deftest cancel-flow-test
  (testing "owner can cancel their own open request through mounted route"
    (let [ctx (base-ctx)
          created-title "Need help finding return desk"
          _create-result (create-request-through-route!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Customer service"
                            "details" "Wrong receipt"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          cancel-result (lifecycle-action-through-route!
                         :cancel
                         ctx
                         request-id)
          response (:response cancel-result)]
      (assert-fragment-or-oob-response response)
      (is (= :cancelled (request-status ctx request-id)))
      (is (= :request/cancelled (first-change-topic cancel-result)))
      (assert-oob response views/request-toolbar-dom-id)
      (assert-oob response views/request-list-dom-id)
      (assert-oob response views/board-state-form-id)
      (is (body-contains? response "Cancel"))
      (is (contains? (board-titles ctx (latest-view-state ctx))
                     created-title))
      (is (body-contains? response created-title))
      (is (not (terminal-action-url-present? response request-id))))))

(deftest lifecycle-action-topic-matrix-flow-test
  (testing "each successful mounted lifecycle route emits the expected topic"
    (doseq [action [:claim :unclaim :take-over :done :cancel]]
      (let [owner (base-ctx)
            helper (helper-ctx)
            other (other-ctx)
            title (str "Matrix request " (name action))
            _create-result (create-request-through-route!
                            owner
                            (create-params
                             {"title" title
                              "area" "Matrix"
                              "details" "Matrix details"
                              "customer-name" "Casey"}))
            request-id (:request/id (request-by-title owner title))
            result (case action
                     :claim
                     (lifecycle-action-through-route! :claim helper request-id)

                     :unclaim
                     (do
                       (lifecycle-action-through-route! :claim helper request-id)
                       (lifecycle-action-through-route! :unclaim helper request-id))

                     :take-over
                     (do
                       (lifecycle-action-through-route! :claim helper request-id)
                       (lifecycle-action-through-route! :take-over other request-id))

                     :done
                     (lifecycle-action-through-route! :done owner request-id)

                     :cancel
                     (lifecycle-action-through-route! :cancel owner request-id))]
        (assert-fragment-or-oob-response (:response result))
        (is (= (expected-change-topic action)
               (first-change-topic result))
            (str "wrong topic for " action))))))

(deftest forbidden-action-flow-test
  (testing "owner cannot claim their own open request through mounted route"
    (let [ctx (base-ctx)
          created-title "Need help finding nails"
          _create-result (create-request-through-route!
                          ctx
                          (create-params
                           {"title" created-title
                            "area" "Hardware"
                            "details" "Finish nails"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx created-title))
          result (lifecycle-action-through-route!
                  :claim
                  ctx
                  request-id)
          response (:response result)]
      (assert-fragment-or-oob-response response)
      (is (= :open (request-status ctx request-id)))
      (is (empty? (:notify-calls result)))
      (is (body-contains? response "Request not updated"))
      (assert-not-oob response views/request-toolbar-dom-id)
      (assert-not-oob response views/request-list-dom-id))))

(deftest missing-request-action-flow-test
  (testing "missing request action returns an error response and does not notify"
    (let [result (lifecycle-action-through-route!
                  :claim
                  (helper-ctx)
                  "missing-request")
          response (:response result)]
      (assert-fragment-or-oob-response response)
      (is (empty? (:notify-calls result)))
      (is (body-contains? response "Request not updated")))))

(deftest terminal-request-action-flow-test
  (testing "terminal requests cannot be claimed or unclaimed"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          title "Need help with terminal action"
          _create-result (create-request-through-route!
                          ctx
                          (create-params
                           {"title" title
                            "area" "Hardware"
                            "details" "Terminal"
                            "customer-name" "Dana"}))
          request-id (:request/id (request-by-title ctx title))]
      (lifecycle-action-through-route! :done ctx request-id)

      (doseq [action [:claim :unclaim :take-over :cancel]]
        (let [result (lifecycle-action-through-route!
                      action
                      helper
                      request-id)]
          (assert-fragment-or-oob-response (:response result))
          (is (= :done (request-status helper request-id)))
          (is (empty? (:notify-calls result)))
          (is (body-contains? (:response result)
                              "Request not updated")))))))

(deftest lifecycle-auto-refresh-visible-request-flow-test
  (testing "a lifecycle live refresh can update an already-visible request at the old visible revision"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          open-request (open-seed-request ctx)
          visible-before (data/latest-revision ctx)
          request-id (:request/id open-request)
          claim-result (lifecycle-action-through-route!
                        :claim
                        helper
                        request-id)]
      (assert-fragment-or-oob-response (:response claim-result))
      (is (= :claimed (request-status helper request-id)))

      ;; This mirrors the browser's automatic fragment GET after
      ;; sse:live-update. The visible revision is still the older revision,
      ;; but this request was already visible then, so the refreshed card should
      ;; show the updated lifecycle state.
      (let [fragment-response (endpoint!
                               :get
                               routes/request-list-fragment-route
                               helper
                               {:params {"q" ""
                                         "selected" request-id
                                         "visible-revision"
                                         (str visible-before)}})]
        (assert-fragment-root fragment-response views/request-list-dom-id)
        (is (body-contains? fragment-response (:request/title open-request)))
        (is (body-contains? fragment-response "claimed by helper@example.com"))))))

;; -----------------------------------------------------------------------------
;; Reset flow
;; -----------------------------------------------------------------------------

(deftest reset-flow-test
  (testing "mounted reset route removes created requests, resets revision, emits reset change and toast"
    (let [ctx (base-ctx)
          created-title "Temporary reset target"]
      (create-request-through-route!
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
        (with-reset-side-effect-recorders notify-calls reset-toasts
          (let [response (endpoint!
                          :post
                          routes/reset-demo-route
                          ctx)]
            (assert-fragment-or-oob-response response)
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

            (assert-oob response views/request-toolbar-dom-id)
            (assert-oob response views/request-list-dom-id)
            (assert-oob response views/board-state-form-id)
            (is (body-contains? response "Demo reset"))))))))

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
      (assert-fragment-root toolbar-response views/request-toolbar-dom-id)
      (assert-fragment-root list-response views/request-list-dom-id))))

(deftest rendered-fragment-stale-refresh-flow-test
  (testing "live layer renders stale toolbar but keeps list behind old visible revision"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          visible-before (data/latest-revision ctx)
          title "Need help finding copper pipe"]
      (create-request-through-route!
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
        (assert-fragment-root toolbar-response views/request-toolbar-dom-id)
        (assert-fragment-root list-response views/request-list-dom-id)
        (is (body-contains? toolbar-response "+1 new"))
        (is (not (body-contains? list-response title)))))))

(deftest no-fragment-action-endpoint-returns-full-page-flow-test
  (testing "fragment and action endpoints never accidentally return the whole page shell"
    (let [ctx (base-ctx)
          helper (helper-ctx)
          title "Need help with full page guard"
          _create-result (create-request-through-route!
                          ctx
                          (create-params
                           {"title" title
                            "area" "Testing"
                            "details" "Guard"
                            "customer-name" "Guard"}))
          request-id (:request/id (request-by-title ctx title))
          responses [(endpoint!
                      :get
                      routes/request-toolbar-fragment-route
                      ctx
                      {:params {"visible-revision"
                                (str (data/latest-revision ctx))}})

                     (endpoint!
                      :get
                      routes/request-list-fragment-route
                      ctx
                      {:params {"visible-revision"
                                (str (data/latest-revision ctx))}})

                     (endpoint!
                      :get
                      routes/create-request-dialog-fragment-route
                      ctx)

                     (endpoint!
                      :get
                      routes/search-requests-route
                      ctx
                      {:params {"q" "guard"
                                "visible-revision"
                                (str (data/latest-revision ctx))}})

                     (refresh-through-route!
                      ctx
                      {"visible-revision"
                       (str (data/latest-revision ctx))})

                     (:response
                      (lifecycle-action-through-route!
                       :claim
                       helper
                       request-id))]]
      (doseq [response responses]
        (assert-fragment-or-oob-response response)))))
