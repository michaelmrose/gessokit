(ns gessokit.humanhelp.app-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gessokit.humanhelp.app :as app]
   [gessokit.humanhelp.domain :as domain]
   [gessokit.humanhelp.live :as hh-live]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.store :as store]
   [gessokit.humanhelp.views :as views]
   [gessokit.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def base-ctx
  {:anti-forgery-token "test-token"
   :gesso.live/system ::live-system
   :user/id "user-owner"
   :user/email "owner@example.com"
   :session {:uid "session-user"
             :email "session@example.com"}})

(def helper-ctx
  (assoc base-ctx
         :user/id "user-helper"
         :user/email "helper@example.com"))

(def view-state
  {:search "garden"
   :selected-request-id nil
   :visible-revision 3})

(defn reset-store-fixture
  [f]
  (store/reset-demo-state!)
  (try
    (f)
    (finally
      (store/reset-demo-state!))))

(use-fixtures :each reset-store-fixture)

(defn valid-params
  [overrides]
  (merge
   {"title" "Need help finding a rake"
    "area" "Garden"
    "details" "Looking for a sturdy rake for leaves."
    "customer-name" "Jon"}
   overrides))

(defn open-seed-request
  []
  (first
   (filter #(= :open (:request/status %))
           (store/all-requests))))

(defn owner-ctx-for
  [request]
  (assoc base-ctx
         :user/id (:request/customer-user-id request)
         :user/email (str (:request/customer-user-id request)
                          "@example.com")))

(defn ctx-with-request-id
  [ctx request-id]
  (assoc ctx :path-params {:request-id request-id}))

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

(defn contains-body?
  [response s]
  (str/includes? (or (response-body response) "") s))

(defn response-oob?
  [response id]
  (and (contains-body? response (str "id=\"" id "\""))
       (contains-body? response "hx-swap-oob=\"outerHTML\"")))

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
;; Test stubs
;; -----------------------------------------------------------------------------

(defn stub-toolbar
  [_ctx _view-state]
  [:div {:id views/request-toolbar-dom-id} "toolbar"])

(defn stub-list
  [_ctx _view-state]
  [:div {:id views/request-list-dom-id} "list"])

(defn recording-notify
  [calls]
  (fn [& args]
    (swap! calls conj args)
    {:submitted true}))

(defn recording-request-toast
  [calls]
  (fn [request]
    (swap! calls conj request)
    {:sent 1}))

;; -----------------------------------------------------------------------------
;; Request param helpers
;; -----------------------------------------------------------------------------

(deftest param-test
  (testing "param reads keyword and string keys from all supported locations"
    (is (= "params-k" (app/param {:params {:x "params-k"}} :x)))
    (is (= "params-s" (app/param {:params {"x" "params-s"}} :x)))
    (is (= "form-k" (app/param {:form-params {:x "form-k"}} :x)))
    (is (= "form-s" (app/param {:form-params {"x" "form-s"}} :x)))
    (is (= "query-k" (app/param {:query-params {:x "query-k"}} :x)))
    (is (= "query-s" (app/param {:query-params {"x" "query-s"}} :x)))
    (is (= "path-k" (app/param {:path-params {:x "path-k"}} :x)))
    (is (= "path-s" (app/param {:path-params {"x" "path-s"}} :x))))

  (testing "param precedence is params, form, query, path"
    (is (= "params"
           (app/param
            {:params {:x "params"}
             :form-params {:x "form"}
             :query-params {:x "query"}
             :path-params {:x "path"}}
            :x)))
    (is (= "form"
           (app/param
            {:form-params {:x "form"}
             :query-params {:x "query"}
             :path-params {:x "path"}}
            :x)))
    (is (= "query"
           (app/param
            {:query-params {:x "query"}
             :path-params {:x "path"}}
            :x)))
    (is (= "path"
           (app/param
            {:path-params {:x "path"}}
            :x)))))

(deftest request-id-test
  (is (= "hh-req-1"
         (app/request-id {:path-params {:request-id "hh-req-1"}})))
  (is (= "hh-req-2"
         (app/request-id {:path-params {"request-id" "hh-req-2"}}))))

;; -----------------------------------------------------------------------------
;; User / live system / view state
;; -----------------------------------------------------------------------------

(deftest current-user-test
  (is (= {:user/id "user-owner"
          :user/email "owner@example.com"}
         (app/current-user base-ctx)))

  (is (= {:user/id "session-user"
          :user/email "session@example.com"}
         (app/current-user
          {:session {:uid "session-user"
                     :email "session@example.com"}}))))

(deftest live-system-test
  (is (= ::live-system
         (app/live-system base-ctx)))

  (try
    (app/live-system {:a 1 :b 2})
    (is false "Expected app/live-system to throw")
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

  (is (= "" (:search (app/request-view-state {}))))

  (is (nil?
       (:visible-revision
        (app/request-view-state
         {:params {"visible-revision" "not-a-number"}}))))

  (is (= 7
         (:visible-revision
          (app/request-view-state
           {:params {:visible-revision "7"}})))))

(deftest html-test
  (let [response (app/html [:div {:id "x"} "Hello"])]
    (is (html-response? response))
    (is (contains-body? response "Hello"))
    (is (contains-body? response "id=\"x\""))))

;; -----------------------------------------------------------------------------
;; Render helpers
;; -----------------------------------------------------------------------------

(deftest page-data-test
  (with-redefs [hh-live/page-panels
                (fn [state]
                  {:request-toolbar-panel [:toolbar-panel state]
                   :request-list-panel [:list-panel state]})]
    (let [ctx (assoc base-ctx
                     :params {"q" "garden"
                              "selected" "hh-req-1"
                              "visible-revision" "2"})
          data (app/page-data ctx)
          state (:view-state data)]
      (is (= {:user/id "user-owner"
              :user/email "owner@example.com"}
             (:user data)))
      (is (= "garden" (:search state)))
      (is (= "hh-req-1" (:selected-request-id state)))
      (is (= 2 (:visible-revision state)))
      (is (= [:toolbar-panel state]
             (:request-toolbar-panel data)))
      (is (= [:list-panel state]
             (:request-list-panel data))))))

(deftest fragment-render-options-test
  (let [ctx (assoc base-ctx
                   :params {"q" "garden"
                            "selected" "hh-req-1"
                            "visible-revision" "3"})
        opts (app/fragment-render-options ctx)]
    (is (= {:user/id "user-owner"
            :user/email "owner@example.com"}
           (:user opts)))
    (is (= {:search "garden"
            :selected-request-id "hh-req-1"
            :visible-revision 3}
           (:view-state opts)))))

(deftest render-toolbar-node-test
  (let [calls (atom [])]
    (with-redefs [hh-live/render-fragment-node
                  (fn [& args]
                    (swap! calls conj args)
                    [:toolbar])]
      (is (= [:toolbar]
             (app/render-toolbar-node base-ctx view-state)))
      (let [[ctx fragment opts] (first @calls)]
        (is (= base-ctx ctx))
        (is (= :request-toolbar fragment))
        (is (= view-state (:view-state opts)))
        (is (= {:user/id "user-owner"
                :user/email "owner@example.com"}
               (:user opts)))))))

(deftest render-list-node-test
  (let [calls (atom [])]
    (with-redefs [hh-live/render-fragment-node
                  (fn [& args]
                    (swap! calls conj args)
                    [:list])]
      (is (= [:list]
             (app/render-list-node base-ctx view-state)))
      (let [[ctx fragment opts] (first @calls)]
        (is (= base-ctx ctx))
        (is (= :request-list fragment))
        (is (= view-state (:view-state opts)))
        (is (= {:user/id "user-owner"
                :user/email "owner@example.com"}
               (:user opts)))))))

(deftest board-oob-test
  (with-redefs [app/render-toolbar-node
                (fn [ctx state]
                  [:toolbar ctx state])
                app/render-list-node
                (fn [ctx state]
                  [:list ctx state])]
    (is (= {:toolbar [:toolbar base-ctx view-state]
            :request-list [:list base-ctx view-state]}
           (app/board-oob base-ctx view-state)))))

;; -----------------------------------------------------------------------------
;; Page and fragment handlers
;; -----------------------------------------------------------------------------

(deftest app-page-test
  (let [calls (atom [])]
    (with-redefs [app/page-data
                  (fn [ctx]
                    {:page-data true
                     :ctx ctx})
                  views/page
                  (fn [& args]
                    (swap! calls conj args)
                    [:page])]
      (is (= [:page] (app/app-page base-ctx)))
      (is (= [[base-ctx {:page-data true
                         :ctx base-ctx}]]
             @calls)))))

(deftest request-toolbar-fragment-test
  (let [calls (atom [])]
    (with-redefs [hh-live/render-fragment-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body "toolbar"})]
      (is (= {:status 200
              :body "toolbar"}
             (app/request-toolbar-fragment base-ctx)))
      (let [[ctx fragment opts] (first @calls)]
        (is (= base-ctx ctx))
        (is (= :request-toolbar fragment))
        (is (= (app/fragment-render-options base-ctx) opts))))))

(deftest request-list-fragment-test
  (let [calls (atom [])]
    (with-redefs [hh-live/render-fragment-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body "list"})]
      (is (= {:status 200
              :body "list"}
             (app/request-list-fragment base-ctx)))
      (let [[ctx fragment opts] (first @calls)]
        (is (= base-ctx ctx))
        (is (= :request-list fragment))
        (is (= (app/fragment-render-options base-ctx) opts))))))

(deftest create-request-dialog-fragment-test
  (let [response (app/create-request-dialog-fragment base-ctx)]
    (is (html-response? response))
    (is (contains-body? response views/create-request-dialog-id))
    (is (contains-body? response views/create-request-dialog-body-id))
    (is (contains-body? response "Create request"))
    (is (contains-body? response "owner@example.com"))))

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
      (is (= {:status 200
              :body ::stream}
             (app/request-toolbar-stream base-ctx)))
      (let [[live-system ctx fragment opts] (first @calls)]
        (is (= ::live-system live-system))
        (is (= base-ctx ctx))
        (is (= :request-toolbar fragment))
        (is (= (app/fragment-render-options base-ctx) opts))))))

(deftest request-list-stream-test
  (let [calls (atom [])]
    (with-redefs [hh-live/stream-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body ::stream})]
      (is (= {:status 200
              :body ::stream}
             (app/request-list-stream base-ctx)))
      (let [[live-system ctx fragment opts] (first @calls)]
        (is (= ::live-system live-system))
        (is (= base-ctx ctx))
        (is (= :request-list fragment))
        (is (= (app/fragment-render-options base-ctx) opts))))))

;; -----------------------------------------------------------------------------
;; Create request
;; -----------------------------------------------------------------------------

(deftest create-request-validation-error-test
  (let [notified (atom [])
        toasted (atom [])]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  hh-live/send-new-request-toast! (recording-request-toast toasted)]
      (let [response (app/create-request!
                      (assoc base-ctx
                             :params {"title" ""
                                      "area" ""}))]
        (is (html-response? response))
        (is (response-oob? response views/create-request-dialog-id))
        (is (contains-body? response "Create request"))
        (is (empty? @notified))
        (is (empty? @toasted))
        (is (= 3 (store/latest-revision)))))))

(deftest create-request-success-response-test
  (with-redefs [app/render-toolbar-node stub-toolbar
                app/render-list-node stub-list]
    (let [response (app/create-request-success-response
                    base-ctx
                    {:request {:request/id "hh-req-4"}
                     :revision 4
                     :view-state view-state})]
      (is (html-response? response))
      (is (response-oob? response views/request-toolbar-dom-id))
      (is (response-oob? response views/request-list-dom-id))
      (is (response-oob? response views/create-request-dialog-id))
      (is (contains-body? response "toolbar"))
      (is (contains-body? response "list")))))

(deftest create-request-success-test
  (let [notified (atom [])
        toasted (atom [])
        before-revision (store/latest-revision)
        params (valid-params
                {"title" "Need gloves"
                 "area" "Garden"
                 "details" "Large gloves"
                 "customer-name" "Avery"})
        request-ctx (assoc base-ctx :params params)]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  hh-live/send-new-request-toast! (recording-request-toast toasted)
                  app/render-toolbar-node stub-toolbar
                  app/render-list-node stub-list]
      (let [response (app/create-request! request-ctx)
            created (first @toasted)
            notification (first @notified)
            [live-system ctx change] notification]

        (is (html-response? response))
        (is (= (inc before-revision) (store/latest-revision)))

        (is (= 1 (count @notified)))
        (is (= 1 (count @toasted)))

        (is (= "Need gloves" (:request/title created)))
        (is (= "Garden" (:request/area created)))
        (is (= "Large gloves" (:request/details created)))
        (is (= "Avery" (:request/customer-name created)))

        (is (= ::live-system live-system))
        (is (= request-ctx ctx))
        (is (= :request/created (:topic change)))
        (is (= domain/store-id (:store/id change)))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (response-oob? response views/create-request-dialog-id))))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(deftest refresh-requests-test
  (store/create-request!
   {:user {:user/id "creator"
           :user/email "creator@example.com"}
    :input {:title "New hidden request"
            :area "Garden"
            :details nil
            :customer-name "Creator"}})

  (let [latest (store/latest-revision)
        seen (atom [])]
    (with-redefs [app/render-toolbar-node
                  (fn [_ctx state]
                    (swap! seen conj [:toolbar state])
                    (stub-toolbar nil state))
                  app/render-list-node
                  (fn [_ctx state]
                    (swap! seen conj [:list state])
                    (stub-list nil state))]
      (let [response (app/refresh-requests!
                      (assoc base-ctx
                             :params {"q" "garden"
                                      "visible-revision" "3"}))]
        (is (html-response? response))
        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (every? #(= latest (get-in % [1 :visible-revision]))
                    @seen))))))

(deftest search-requests-test
  (with-redefs [app/request-list-fragment
                (fn [ctx]
                  {:status 200
                   :body (str "searched:" (get-in ctx [:params "q"]))})]
    (is (= {:status 200
            :body "searched:garden"}
           (app/search-requests
            (assoc base-ctx :params {"q" "garden"}))))))

(deftest select-request-test
  (with-redefs [app/request-list-fragment
                (fn [ctx]
                  {:status 200
                   :body (str "selected:"
                              (get-in ctx [:path-params :request-id]))})]
    (is (= {:status 200
            :body "selected:hh-req-1"}
           (app/select-request
            (ctx-with-request-id base-ctx "hh-req-1"))))))

;; -----------------------------------------------------------------------------
;; Lifecycle actions
;; -----------------------------------------------------------------------------

(deftest lifecycle-action-success-test
  (let [open-request (open-seed-request)
        notified (atom [])]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  app/render-toolbar-node stub-toolbar
                  app/render-list-node stub-list]
      (let [ctx (ctx-with-request-id helper-ctx (:request/id open-request))
            response (app/lifecycle-action! ctx :claim store/claim-request!)]
        (is (html-response? response))
        (is (= :claimed
               (:request/status
                (store/request-by-id (:request/id open-request)))))
        (is (= 1 (count @notified)))

        (let [[live-system ctx' change] (first @notified)]
          (is (= ::live-system live-system))
          (is (= ctx ctx'))
          (is (= :request/claimed (:topic change))))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))))))

(deftest lifecycle-action-error-test
  (let [open-request (open-seed-request)
        notified (atom [])
        ctx (owner-ctx-for open-request)]
    (with-redefs [hh-live/notify! (recording-notify notified)]
      (let [response (app/lifecycle-action!
                      (ctx-with-request-id ctx (:request/id open-request))
                      :claim
                      store/claim-request!)]
        (is (html-response? response))
        (is (contains-body? response "Request not updated"))
        (is (empty? @notified))
        (is (= open-request
               (store/request-by-id (:request/id open-request))))))))

(defn delegated-action
  [handler]
  (with-redefs [app/lifecycle-action!
                (fn [ctx action store-fn]
                  {:ctx ctx
                   :action action
                   :store-fn store-fn})]
    (handler base-ctx)))

(deftest lifecycle-specific-handlers-test
  (is (= {:ctx base-ctx
          :action :claim
          :store-fn store/claim-request!}
         (delegated-action app/claim-request!)))

  (is (= {:ctx base-ctx
          :action :unclaim
          :store-fn store/unclaim-request!}
         (delegated-action app/unclaim-request!)))

  (is (= {:ctx base-ctx
          :action :take-over
          :store-fn store/take-over-request!}
         (delegated-action app/take-over-request!)))

  (is (= {:ctx base-ctx
          :action :done
          :store-fn store/mark-request-done!}
         (delegated-action app/mark-request-done!)))

  (is (= {:ctx base-ctx
          :action :cancel
          :store-fn store/cancel-request!}
         (delegated-action app/cancel-request!))))

;; -----------------------------------------------------------------------------
;; Reset demo
;; -----------------------------------------------------------------------------

(deftest reset-demo-test
  (store/create-request!
   {:user {:user/id "creator"
           :user/email "creator@example.com"}
    :input {:title "Temporary request"
            :area "Garden"
            :details nil
            :customer-name "Creator"}})

  (is (> (store/latest-revision) 3))

  (let [notified (atom [])
        reset-toasts (atom 0)]
    (with-redefs [hh-live/notify! (recording-notify notified)
                  hh-live/send-reset-toast! (fn []
                                              (swap! reset-toasts inc)
                                              {:sent 1})
                  app/render-toolbar-node stub-toolbar
                  app/render-list-node stub-list]
      (let [response (app/reset-demo! base-ctx)]
        (is (html-response? response))
        (is (= 3 (store/latest-revision)))
        (is (= 1 (count @notified)))
        (is (= 1 @reset-toasts))

        (let [[live-system ctx change] (first @notified)]
          (is (= ::live-system live-system))
          (is (= base-ctx ctx))
          (is (= :humanhelp-demo/reset (:topic change)))
          (is (= 3 (:revision change))))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))))))

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
              route-tree))))
