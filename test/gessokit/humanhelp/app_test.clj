(ns gessokit.humanhelp.app-test
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
       (ex-info "app-test ctx has not been initialized."
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
;; Fixtures
;; -----------------------------------------------------------------------------

(def view-state
  {:search "garden"
   :selected-request-id "hh-req-1"
   :visible-revision 2})

;; -----------------------------------------------------------------------------
;; Hiccup helpers
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

(defn children
  [node]
  (when (vector? node)
    (let [xs (rest node)]
      (if (map? (first xs))
        (rest xs)
        xs))))

(defn text-nodes
  [tree]
  (filter string? (hiccup-seq tree)))

(defn contains-text?
  [tree text]
  (boolean
   (some #(str/includes? % text)
         (text-nodes tree))))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (filter node? (hiccup-seq tree))))

;; -----------------------------------------------------------------------------
;; Response helpers
;; -----------------------------------------------------------------------------

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (= "text/html; charset=utf-8"
          (get-in response [:headers "content-type"]))
       (string? (:body response))))

(defn body-contains?
  [response text]
  (boolean
   (str/includes? (or (:body response) "") text)))

(defn response-oob?
  [response dom-id]
  (and (body-contains? response (str "id=\"" dom-id "\""))
       (body-contains? response "hx-swap-oob=\"outerHTML\"")))

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

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------

(defn valid-create-params
  [overrides]
  (merge
   {"title" "Need help finding a rake"
    "area" "Garden"
    "details" "Looking for a sturdy rake for leaves."
    "customer-name" "Jon"}
   overrides))

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

(defn open-seed-request
  []
  (first
   (filter #(= :open (:request/status %))
           (data/all-requests (base-ctx)))))

(defn request-by-title
  [title]
  (first
   (filter #(= title (:request/title %))
           (data/all-requests (base-ctx)))))

(defn owner-ctx-for
  [request]
  (assoc (base-ctx)
         :user/id (:request/customer-user-id request)
         :user/email (str (:request/customer-user-id request)
                          "@example.com")
         :session {:uid (:request/customer-user-id request)
                   :email (str (:request/customer-user-id request)
                               "@example.com")}))

(defn recording-notify
  [calls]
  (fn [& args]
    (swap! calls conj args)
    {:submitted true}))

;; -----------------------------------------------------------------------------
;; Param and identity
;; -----------------------------------------------------------------------------

(deftest scalar-param-value-test
  (testing "nil and scalar values are preserved"
    (is (nil? (app/scalar-param-value nil)))
    (is (= "garden" (app/scalar-param-value "garden"))))

  (testing "repeated values use the last submitted browser value"
    (is (= "garden" (app/scalar-param-value ["" "garden"])))
    (is (= "" (app/scalar-param-value ["garden" ""])))
    (is (= "second" (app/scalar-param-value ["first" "second"])))))

(deftest param-test
  (testing "reads params from common request locations"
    (is (= "params-k" (app/param {:params {:x "params-k"}} :x)))
    (is (= "params-s" (app/param {:params {"x" "params-s"}} :x)))
    (is (= "form-k" (app/param {:form-params {:x "form-k"}} :x)))
    (is (= "form-s" (app/param {:form-params {"x" "form-s"}} :x)))
    (is (= "query-k" (app/param {:query-params {:x "query-k"}} :x)))
    (is (= "query-s" (app/param {:query-params {"x" "query-s"}} :x)))
    (is (= "path-k" (app/param {:path-params {:x "path-k"}} :x)))
    (is (= "path-s" (app/param {:path-params {"x" "path-s"}} :x)))
    (is (= "match-k"
           (app/param
            {:reitit.core/match {:path-params {:x "match-k"}}}
            :x)))
    (is (= "match-s"
           (app/param
            {:reitit.core/match {:path-params {"x" "match-s"}}}
            :x))))

  (testing "normalizes repeated params"
    (is (= "garden"
           (app/param {:params {"q" ["" "garden"]}} :q)))
    (is (= ""
           (app/param {:params {"q" ["garden" ""]}} :q)))))

(deftest request-id-test
  (is (= "hh-req-1"
         (app/request-id {:path-params {:request-id "hh-req-1"}})))
  (is (= "hh-req-2"
         (app/request-id {:path-params {"request-id" "hh-req-2"}})))
  (is (= "hh-req-3"
         (app/request-id
          {:reitit.core/match
           {:path-params {:request-id "hh-req-3"}}}))))

(deftest current-user-test
  (testing "current-user prefers the signed-in ctx identity"
    (is (= {:user/id "owner"
            :user/email "owner@example.com"}
           (app/current-user (base-ctx)))))

  (testing "current-user can use session-only identity"
    (is (= {:user/id "session-user"
            :user/email "session@example.com"}
           (app/current-user
            (merge
             (ctx-base)
             {:session {:uid "session-user"
                        :email "session@example.com"}}))))))

(deftest live-system-test
  (is (= ::live-system
         (app/live-system (base-ctx))))

  (try
    (app/live-system {:a 1 :b 2})
    (is false "Expected live-system to throw")
    (catch clojure.lang.ExceptionInfo e
      (is (str/includes? (ex-message e)
                         "Human Help requires :gesso.live/system"))
      (is (= #{:a :b} (:ctx-keys (ex-data e)))))))

(deftest request-view-state-test
  (is (= {:search "garden"
          :selected-request-id "hh-req-1"
          :visible-revision 3}
         (app/request-view-state
          {:params {"q" "garden"
                    "selected" "hh-req-1"
                    "visible-revision" "3"}})))

  (is (= "garden"
         (:search
          (app/request-view-state
           {:params {"q" ["" "garden"]}}))))

  (is (= ""
         (:search
          (app/request-view-state
           {:params {"q" ["garden" ""]}}))))

  (is (= "" (:search (app/request-view-state {}))))

  (is (nil?
       (:visible-revision
        (app/request-view-state
         {:params {"visible-revision" "not-a-number"}})))))

;; -----------------------------------------------------------------------------
;; Stable live panels
;; -----------------------------------------------------------------------------

(deftest live-panel-test
  (testing "request list panel keeps SSE/fetch trigger on stable wrapper"
    (let [node (app/live-panel :request-list)
          a (attrs node)
          inner (first (children node))]
      (is (= :div (first node)))
      (is (= "humanhelp-request-list-fragment"
             (:data-gesso-live-fragment a)))
      (is (= "sse" (:hx-ext a)))
      (is (= (routes/request-list-stream-url)
             (:sse-connect a)))
      (is (= (routes/request-list-fragment-url)
             (:hx-get a)))
      (is (= (str "#" views/board-state-form-id)
             (:hx-include a)))
      (is (= (str "#" views/request-list-dom-id)
             (:hx-target a)))
      (is (= "outerHTML" (:hx-swap a)))
      (is (str/includes? (:hx-trigger a) "sse:live-update"))
      (is (str/includes? (:hx-trigger a) "htmx:sseOpen"))
      (is (not (str/includes? (:hx-get a) "?")))
      (is (= views/request-list-dom-id
             (:id (attrs inner))))))

  (testing "request toolbar panel keeps SSE/fetch trigger on stable wrapper"
    (let [node (app/live-panel :request-toolbar)
          a (attrs node)
          inner (first (children node))]
      (is (= "humanhelp-request-toolbar-fragment"
             (:data-gesso-live-fragment a)))
      (is (= "sse" (:hx-ext a)))
      (is (= (routes/request-toolbar-stream-url)
             (:sse-connect a)))
      (is (= (routes/request-toolbar-fragment-url)
             (:hx-get a)))
      (is (= (str "#" views/board-state-form-id)
             (:hx-include a)))
      (is (= (str "#" views/request-toolbar-dom-id)
             (:hx-target a)))
      (is (= "outerHTML" (:hx-swap a)))
      (is (str/includes? (:hx-trigger a) "sse:live-update"))
      (is (str/includes? (:hx-trigger a) "htmx:sseOpen"))
      (is (not (str/includes? (:hx-get a) "?")))
      (is (= views/request-toolbar-dom-id
             (:id (attrs inner)))))))

(deftest live-panel-unknown-fragment-test
  (testing "unknown live panel fragments throw useful ex-info"
    (try
      (app/live-panel :missing-fragment)
      (is false "Expected live-panel to throw")
      (catch clojure.lang.ExceptionInfo e
        (is (str/includes? (ex-message e)
                           "Unknown Human Help fragment DOM id"))
        (is (= :missing-fragment (:fragment (ex-data e))))))))

(deftest page-panels-test
  (let [panels (app/page-panels)]
    (is (find-by-id (:request-toolbar-panel panels)
                    views/request-toolbar-dom-id))
    (is (find-by-id (:request-list-panel panels)
                    views/request-list-dom-id))))

(deftest page-data-test
  (let [ctx (assoc (base-ctx)
                   :params {"q" "garden"
                            "selected" "hh-req-1"
                            "visible-revision" "2"})
        page-data (app/page-data ctx)
        state (:view-state page-data)
        toolbar-panel (:request-toolbar-panel page-data)
        list-panel (:request-list-panel page-data)]
    (is (= {:user/id "owner"
            :user/email "owner@example.com"}
           (:user page-data)))
    (is (= "garden" (:search state)))
    (is (= "hh-req-1" (:selected-request-id state)))
    (is (= 2 (:visible-revision state)))

    (is (= (routes/request-toolbar-stream-url)
           (:sse-connect (attrs toolbar-panel))))
    (is (= (routes/request-toolbar-fragment-url)
           (:hx-get (attrs toolbar-panel))))
    (is (= (str "#" views/board-state-form-id)
           (:hx-include (attrs toolbar-panel))))

    (is (= (routes/request-list-stream-url)
           (:sse-connect (attrs list-panel))))
    (is (= (routes/request-list-fragment-url)
           (:hx-get (attrs list-panel))))
    (is (= (str "#" views/board-state-form-id)
           (:hx-include (attrs list-panel))))))

(deftest page-data-default-visible-revision-test
  (testing "page-data normalizes missing visible revision to current latest"
    (let [ctx (assoc (base-ctx) :params {})
          page-data (app/page-data ctx)]
      (is (= (data/latest-revision ctx)
             (get-in page-data [:view-state :visible-revision]))))))

;; -----------------------------------------------------------------------------
;; Board-state OOB
;; -----------------------------------------------------------------------------

(deftest board-state-form-oob-test
  (let [ctx (base-ctx)
        node (app/board-state-form-oob ctx view-state)
        a (attrs node)]
    (is (= :form (first node)))
    (is (= views/board-state-form-id (:id a)))
    (is (= "outerHTML" (:hx-swap-oob a)))
    (is (= (routes/search-requests-url) (:hx-get a)))
    (is (find-by-id node "humanhelp-search"))))

(deftest with-board-state-oob-test
  (let [ctx (base-ctx)
        node (app/with-board-state-oob
              ctx
              [:div {:id "payload"} "payload"]
              view-state)]
    (is (find-by-id node "payload"))
    (is (find-by-id node views/board-state-form-id))))

;; -----------------------------------------------------------------------------
;; Render helpers
;; -----------------------------------------------------------------------------

(deftest fragment-render-options-test
  (let [ctx (assoc (base-ctx)
                   :params {"q" "garden"
                            "selected" "hh-req-1"
                            "visible-revision" "3"})
        opts (app/fragment-render-options ctx)]
    (is (= {:user/id "owner"
            :user/email "owner@example.com"}
           (:user opts)))
    (is (= {:search "garden"
            :selected-request-id "hh-req-1"
            :visible-revision 3}
           (:view-state opts)))))

(deftest render-toolbar-node-test
  (let [ctx (base-ctx)
        node (app/render-toolbar-node
              ctx
              {:search ""
               :visible-revision (data/latest-revision ctx)})]
    (is (= views/request-toolbar-dom-id
           (:id (attrs node))))
    (is (contains-text? node "Requests"))))

(deftest render-list-node-test
  (let [ctx (base-ctx)
        node (app/render-list-node
              ctx
              {:search ""
               :visible-revision (data/latest-revision ctx)})]
    (is (= views/request-list-dom-id
           (:id (attrs node))))
    (is (or (contains-text? node "Need help finding a rake")
            (contains-text? node "No matching requests")))))

(deftest board-oob-test
  (let [ctx (base-ctx)
        state {:search ""
               :visible-revision (data/latest-revision ctx)}
        result (app/board-oob ctx state)]
    (is (= views/request-toolbar-dom-id
           (:id (attrs (:toolbar result)))))
    (is (= views/request-list-dom-id
           (:id (attrs (:request-list result)))))))

;; -----------------------------------------------------------------------------
;; Page and fragment handlers
;; -----------------------------------------------------------------------------

(deftest app-page-test
  (let [node (app/app-page (base-ctx))]
    (is (vector? node))
    (is (find-by-id node views/request-toolbar-dom-id))
    (is (find-by-id node views/request-list-dom-id))
    (is (find-by-id node views/board-state-form-id))
    (is (find-by-id node "app-toaster"))
    (is (contains-text? node "Human Help"))))

(deftest request-toolbar-fragment-test
  (let [response (app/request-toolbar-fragment (base-ctx))]
    (is (html-response? response))
    (is (body-contains? response views/request-toolbar-dom-id))
    (is (body-contains? response "Requests"))))

(deftest request-list-fragment-test
  (let [response (app/request-list-fragment (base-ctx))]
    (is (html-response? response))
    (is (body-contains? response views/request-list-dom-id))
    (is (body-contains? response "Need help finding a rake"))))

(deftest request-list-fragment-stale-visibility-test
  (testing "fragment handler does not reveal newly-created requests behind old visible revision"
    (let [ctx (base-ctx)
          visible-before (data/latest-revision ctx)
          {:keys [request revision]} (data/create-request!
                                      ctx
                                      {:user {:user/id "creator"
                                              :user/email "creator@example.com"}
                                       :input {:title "Hidden app fragment target"
                                               :area "Garden"
                                               :details nil
                                               :customer-name "Creator"}})
          stale-response (app/request-list-fragment
                          (assoc ctx
                                 :params {"q" ""
                                          "visible-revision" (str visible-before)}))
          fresh-response (app/request-list-fragment
                          (assoc ctx
                                 :params {"q" ""
                                          "visible-revision" (str revision)}))]
      (is (html-response? stale-response))
      (is (html-response? fresh-response))
      (is (not (body-contains? stale-response (:request/title request))))
      (is (body-contains? fresh-response (:request/title request))))))

(deftest create-request-dialog-fragment-test
  (let [response (app/create-request-dialog-fragment (base-ctx))]
    (is (html-response? response))
    (is (body-contains? response views/create-request-dialog-id))
    (is (body-contains? response "Create request"))
    (is (body-contains? response "owner@example.com"))))

;; -----------------------------------------------------------------------------
;; Stream handlers
;; -----------------------------------------------------------------------------

(deftest request-toolbar-stream-test
  (let [calls (atom [])]
    (with-redefs [hh-live/stream-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body ::stream})]
      (let [ctx (base-ctx)]
        (is (= {:status 200
                :body ::stream}
               (app/request-toolbar-stream ctx)))
        (let [[live-system ctx' fragment opts] (first @calls)]
          (is (= ::live-system live-system))
          (is (= ctx ctx'))
          (is (= :request-toolbar fragment))
          (is (= (app/fragment-render-options ctx) opts)))))))

(deftest request-list-stream-test
  (let [calls (atom [])]
    (with-redefs [hh-live/stream-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body ::stream})]
      (let [ctx (base-ctx)]
        (is (= {:status 200
                :body ::stream}
               (app/request-list-stream ctx)))
        (let [[live-system ctx' fragment opts] (first @calls)]
          (is (= ::live-system live-system))
          (is (= ctx ctx'))
          (is (= :request-list fragment))
          (is (= (app/fragment-render-options ctx) opts)))))))

;; -----------------------------------------------------------------------------
;; Request creation
;; -----------------------------------------------------------------------------

(deftest create-request-validation-error-test
  (let [notified (atom [])
        toasted (atom [])]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  app/send-new-request-toast-safely!
                  (fn [& args]
                    (swap! toasted conj args)
                    {:sent 1})]
      (let [ctx (assoc (base-ctx)
                       :params {"title" ""
                                "area" ""})
            before-revision (data/latest-revision ctx)
            response (app/create-request! ctx)]
        (is (html-response? response))
        (is (= before-revision (data/latest-revision ctx)))
        (is (response-oob? response views/create-request-dialog-id))
        (is (body-contains? response "Create request"))
        (is (empty? @notified))
        (is (empty? @toasted))))))

(deftest create-request-success-response-test
  (let [ctx (base-ctx)
        before-revision (data/latest-revision ctx)
        {:keys [request revision]} (data/create-request!
                                    ctx
                                    {:user (app/current-user ctx)
                                     :input {:title "Need created response target"
                                             :area "Garden"
                                             :details nil
                                             :customer-name "Avery"}})
        response (app/create-request-success-response
                  ctx
                  {:request request
                   :revision revision
                   :view-state {:search ""
                                :visible-revision before-revision}})]
    (is (html-response? response))
    (is (response-oob? response views/request-toolbar-dom-id))
    (is (response-oob? response views/request-list-dom-id))
    (is (response-oob? response views/create-request-dialog-id))
    (is (response-oob? response views/board-state-form-id))
    (is (body-contains? response "Request created"))
    (is (body-contains? response "Need created response target"))
    (is (body-contains? response
                        (str "name=\"visible-revision\" value=\""
                             revision
                             "\"")))))

(deftest create-request-success-test
  (let [notified (atom [])
        toasted (atom [])]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  app/send-new-request-toast-safely!
                  (fn [request user]
                    (swap! toasted conj {:request request
                                         :user user})
                    {:sent 1})]
      (let [ctx (assoc (base-ctx)
                       :params (valid-create-params
                                {"title" "Need gloves"
                                 "area" "Garden"
                                 "details" "Large gloves"
                                 "customer-name" "Avery"}))
            before-revision (data/latest-revision ctx)
            response (app/create-request! ctx)]
        (is (html-response? response))
        (is (= (inc before-revision) (data/latest-revision ctx)))
        (is (= 1 (count @notified)))
        (is (= 1 (count @toasted)))

        (let [{:keys [request user]} (first @toasted)]
          (is (= "Need gloves" (:request/title request)))
          (is (= "Garden" (:request/area request)))
          (is (= "Large gloves" (:request/details request)))
          (is (= "Avery" (:request/customer-name request)))
          (is (= (app/current-user ctx) user)))

        (let [[live-system ctx' change] (first @notified)]
          (is (= ::live-system live-system))
          (is (= ctx ctx'))
          (is (= :request/created (:topic change)))
          (is (= model/store-id (:id change)))
          (is (= model/store-id (:store/id change)))
          (is (= "owner" (:actor/id change)))
          (is (= "owner@example.com" (:actor/email change))))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (response-oob? response views/create-request-dialog-id))
        (is (response-oob? response views/board-state-form-id))
        (is (body-contains? response "Need gloves"))))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(deftest refresh-requests-test
  (let [ctx (base-ctx)
        old-visible (data/latest-revision ctx)]
    (data/create-request!
     ctx
     {:user {:user/id "creator"
             :user/email "creator@example.com"}
      :input {:title "New hidden request"
              :area "Garden"
              :details nil
              :customer-name "Creator"}})

    (let [latest (data/latest-revision ctx)
          response (app/refresh-requests!
                    (assoc ctx
                           :params {"q" "garden"
                                    "visible-revision" (str old-visible)}))]
      (is (html-response? response))
      (is (response-oob? response views/request-toolbar-dom-id))
      (is (response-oob? response views/request-list-dom-id))
      (is (response-oob? response views/board-state-form-id))
      (is (body-contains? response "New hidden request"))
      (is (body-contains? response
                          (str "name=\"visible-revision\" value=\""
                               latest
                               "\""))))))

(deftest search-requests-test
  (let [ctx (base-ctx)
        response (app/search-requests
                  (assoc ctx
                         :params {"q" "rake"
                                  "visible-revision"
                                  (str (data/latest-revision ctx))}))]
    (is (html-response? response))
    (is (body-contains? response views/request-list-dom-id))
    (is (body-contains? response "Need help finding a rake"))
    (is (not (body-contains? response "Can someone help load soil?")))))

(deftest search-requests-clear-test
  (let [ctx (base-ctx)
        response (app/search-requests
                  (assoc ctx
                         :params {"q" ""
                                  "visible-revision"
                                  (str (data/latest-revision ctx))}))]
    (is (html-response? response))
    (is (body-contains? response "Need help finding a rake"))
    (is (body-contains? response "Can someone help load soil?"))))

(deftest select-request-test
  (let [ctx (base-ctx)
        response (app/select-request
                  (assoc
                   (ctx-with-selected-request ctx "hh-req-1")
                   :params {"selected" "old-selection"
                            "visible-revision"
                            (str (data/latest-revision ctx))}))]
    (is (html-response? response))
    (is (body-contains? response views/request-list-dom-id))
    (is (body-contains? response "Looking for a sturdy rake for bark and leaves."))
    (is (response-oob? response views/board-state-form-id))
    (is (body-contains? response "name=\"selected\" value=\"hh-req-1\""))))

;; -----------------------------------------------------------------------------
;; Lifecycle actions
;; -----------------------------------------------------------------------------

(deftest lifecycle-action-success-test
  (let [open-request (open-seed-request)
        notified (atom [])]
    (with-redefs [hh-live/notify! (recording-notify notified)]
      (let [ctx (ctx-with-request-id
                 (helper-ctx)
                 (:request/id open-request))
            response (app/lifecycle-action! ctx :claim data/claim-request!)
            updated (data/request-by-id ctx (:request/id open-request))]
        (is (html-response? response))
        (is (= :claimed (:request/status updated)))
        (is (= "helper" (:request/claimed-by updated)))
        (is (= 1 (count @notified)))

        (let [[live-system ctx' change] (first @notified)]
          (is (= ::live-system live-system))
          (is (= ctx ctx'))
          (is (= :request/claimed (:topic change)))
          (is (= model/store-id (:id change)))
          (is (= model/store-id (:store/id change)))
          (is (= (:request/id open-request) (:request/id change)))
          (is (= :claim (:action change))))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (response-oob? response views/board-state-form-id))))))

(deftest lifecycle-action-error-test
  (let [open-request (open-seed-request)
        notified (atom [])
        owner-ctx (owner-ctx-for open-request)]
    (with-redefs [hh-live/notify! (recording-notify notified)]
      (let [ctx (ctx-with-request-id owner-ctx (:request/id open-request))
            response (app/lifecycle-action!
                      ctx
                      :claim
                      data/claim-request!)]
        (is (html-response? response))
        (is (body-contains? response "Request not updated"))
        (is (empty? @notified))
        (is (= open-request
               (data/request-by-id ctx (:request/id open-request))))))))

(defn delegated-action
  [handler]
  (with-redefs [app/lifecycle-action!
                (fn [ctx action transition-fn]
                  {:ctx ctx
                   :action action
                   :transition-fn transition-fn})]
    (let [ctx (base-ctx)]
      (handler ctx))))

(deftest lifecycle-specific-handlers-test
  (is (= {:ctx (base-ctx)
          :action :claim
          :transition-fn data/claim-request!}
         (delegated-action app/claim-request!)))

  (is (= {:ctx (base-ctx)
          :action :unclaim
          :transition-fn data/unclaim-request!}
         (delegated-action app/unclaim-request!)))

  (is (= {:ctx (base-ctx)
          :action :take-over
          :transition-fn data/take-over-request!}
         (delegated-action app/take-over-request!)))

  (is (= {:ctx (base-ctx)
          :action :done
          :transition-fn data/mark-request-done!}
         (delegated-action app/mark-request-done!)))

  (is (= {:ctx (base-ctx)
          :action :cancel
          :transition-fn data/cancel-request!}
         (delegated-action app/cancel-request!))))

;; -----------------------------------------------------------------------------
;; Reset demo
;; -----------------------------------------------------------------------------

(deftest reset-demo-test
  (let [ctx (base-ctx)]
    (data/create-request!
     ctx
     {:user {:user/id "creator"
             :user/email "creator@example.com"}
      :input {:title "Temporary request"
              :area "Garden"
              :details nil
              :customer-name "Creator"}})

    (is (> (data/latest-revision ctx) 3))

    (let [notified (atom [])
          reset-toasts (atom 0)]
      (with-redefs [hh-live/notify! (recording-notify notified)
                    app/send-reset-toast-safely!
                    (fn []
                      (swap! reset-toasts inc)
                      {:sent 1})]
        (let [response (app/reset-demo! ctx)]
          (is (html-response? response))
          (is (= 3 (data/latest-revision ctx)))
          (is (nil? (request-by-title "Temporary request")))
          (is (= 1 (count @notified)))
          (is (= 1 @reset-toasts))

          (let [[live-system ctx' change] (first @notified)]
            (is (= ::live-system live-system))
            (is (= ctx ctx'))
            (is (= :humanhelp-demo/reset (:topic change)))
            (is (= model/store-id (:id change)))
            (is (= model/store-id (:store/id change)))
            (is (= 3 (:revision change))))

          (is (response-oob? response views/request-toolbar-dom-id))
          (is (response-oob? response views/request-list-dom-id))
          (is (response-oob? response views/board-state-form-id)))))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(deftest module-test
  (is (= hh-live/live-rules (:live-rules app/module)))

  (let [route-tree (first (:routes app/module))]
    (is (= routes/base-path (first route-tree)))
    (is (= {:middleware [mid/wrap-signed-in]}
           (second route-tree))))

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

(deftest module-handler-shape-test
  (let [route-tree (first (:routes app/module))]
    (is (some #(= [routes/create-request-route
                   {:post app/create-request!}]
                 %)
              route-tree))
    (is (some #(= [routes/refresh-requests-route
                   {:post app/refresh-requests!}]
                 %)
              route-tree))
    (is (some #(= [routes/request-list-fragment-route
                   {:get app/request-list-fragment}]
                 %)
              route-tree))
    (is (some #(= [routes/request-toolbar-stream-route
                   {:get app/request-toolbar-stream}]
                 %)
              route-tree))
    (is (some #(= [routes/request-list-stream-route
                   {:get app/request-list-stream}]
                 %)
              route-tree))))
