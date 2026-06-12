(ns gessokit.humanhelp.views-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gessokit.client-plumbing :as client-plumbing]
   [gessokit.humanhelp.model :as model]
   [gessokit.humanhelp.routes :as routes]
   [gessokit.humanhelp.views :as views]
   [gessokit.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def ctx
  {:anti-forgery-token "test-token"})

(def owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def other-user
  {:user/id "user-other"
   :user/email "other@example.com"})

(def view-state
  {:search "garden"
   :selected-request-id nil
   :visible-revision 3})

(def selected-view-state
  {:search "garden"
   :selected-request-id "hh-req-1"
   :visible-revision 3})

(defn request
  [overrides]
  (merge
   {:request/id "hh-req-1"
    :request/number 1
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1780471110000
    :request/updated-at-ms 1780471110000
    :request/created-revision 1
    :request/updated-revision 1}
   overrides))

(def open-request
  (request {}))

(def claimed-request
  (request
   {:request/id "hh-req-2"
    :request/number 2
    :request/title "Can someone help load soil?"
    :request/details "Six heavy bags near the entrance to the garden center."
    :request/customer-user-id "seed-user-2"
    :request/customer-name "Avery"
    :request/status :claimed
    :request/claimed-by "user-helper"
    :request/claimed-by-email "helper@example.com"
    :request/created-revision 2
    :request/updated-revision 2}))

(def done-request
  (request
   {:request/id "hh-req-3"
    :request/number 3
    :request/title "Question about returns"
    :request/details "Customer needed the return window checked."
    :request/customer-user-id "seed-user-3"
    :request/customer-name "Sam"
    :request/status :done
    :request/claimed-by "user-helper"
    :request/claimed-by-email "helper@example.com"
    :request/created-revision 3
    :request/updated-revision 3}))

(def cancelled-request
  (request
   {:request/id "hh-req-4"
    :request/number 4
    :request/title "Cancelled request"
    :request/status :cancelled
    :request/created-revision 4
    :request/updated-revision 4}))

;; -----------------------------------------------------------------------------
;; Hiccup inspection helpers
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

(defn element?
  [x tag]
  (and (node? x)
       (= tag (first x))))

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

(defn exact-text?
  [tree text]
  (boolean
   (some #(= % text)
         (text-nodes tree))))

(defn find-elements
  [tree tag]
  (filter #(element? % tag)
          (hiccup-seq tree)))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (filter node? (hiccup-seq tree))))

(defn find-first
  [tree pred]
  (some
   (fn [node]
     (when (pred node)
       node))
   (filter node? (hiccup-seq tree))))

(defn forms
  [tree]
  (find-elements tree :form))

(defn inputs
  [tree]
  (find-elements tree :input))

(defn buttons
  [tree]
  (find-elements tree :button))

(defn dialogs
  [tree]
  (find-elements tree :dialog))

(defn details-elements
  [tree]
  (find-elements tree :details))

(defn input-by-name
  [tree name]
  (some
   (fn [node]
     (when (= name (:name (attrs node)))
       node))
   (inputs tree)))

(defn form-by-hx-post
  [tree url]
  (some
   (fn [node]
     (when (= url (:hx-post (attrs node)))
       node))
   (forms tree)))

(defn oob-node?
  [node]
  (contains? (attrs node) :hx-swap-oob))

(defn oob-by-id
  [tree id]
  (some
   (fn [node]
     (when (and (= id (:id (attrs node)))
                (oob-node? node))
       node))
   (filter node? (hiccup-seq tree))))

(defn hidden-input-value
  [tree name]
  (:value (attrs (input-by-name tree name))))

(defn hx-posts
  [tree]
  (set
   (keep #(some-> % attrs :hx-post)
         (forms tree))))

;; -----------------------------------------------------------------------------
;; Constants and small helpers
;; -----------------------------------------------------------------------------

(deftest dom-id-constants-test
  (testing "fragment and dialog DOM ids are stable non-blank strings"
    (doseq [id [views/request-toolbar-dom-id
                views/request-list-dom-id
                views/create-request-dialog-id
                views/create-request-dialog-body-id
                views/board-state-form-id]]
      (is (string? id))
      (is (not (str/blank? id)))))

  (testing "fragment and dialog ids are distinct"
    (is (= 5
           (count
            (set [views/request-toolbar-dom-id
                  views/request-list-dom-id
                  views/create-request-dialog-id
                  views/create-request-dialog-body-id
                  views/board-state-form-id]))))))

(deftest user-email-test
  (is (= "owner@example.com"
         (views/user-email owner)))
  (is (= "user-owner"
         (views/user-email {:user/id "user-owner"})))
  (is (= "demo-user"
         (views/user-email {})))
  (is (= "demo-user"
         (views/user-email nil))))

(deftest muted-test
  (let [node (views/muted "Quiet text")]
    (is (= :p (first node)))
    (is (contains-text? node "Quiet text"))
    (is (str/includes? (:class (attrs node)) "text-sm-theme"))
    (is (= "var(--muted-foreground)"
           (get-in (attrs node) [:style :color])))))

(deftest hidden-input-test
  (testing "nil values produce no node"
    (is (nil? (views/hidden-input "q" nil))))

  (testing "non-nil values produce hidden inputs"
    (let [node (views/hidden-input "q" "garden")]
      (is (= :input (first node)))
      (is (= "hidden" (:type (attrs node))))
      (is (= "q" (:name (attrs node))))
      (is (= "garden" (:value (attrs node))))))

  (testing "blank strings are concrete values and are preserved"
    (let [node (views/hidden-input "q" "")]
      (is (= "" (:value (attrs node)))))))

(deftest view-state-hidden-inputs-test
  (let [node (views/view-state-hidden-inputs
              {:search "garden"
               :selected-request-id "hh-req-1"
               :visible-revision 3})]
    (is (= "garden" (hidden-input-value node routes/search-param)))
    (is (= "hh-req-1" (hidden-input-value node routes/selected-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))))

(deftest view-state-hidden-inputs-preserves-blank-search-test
  (let [node (views/view-state-hidden-inputs
              {:search ""
               :selected-request-id nil
               :visible-revision 3})]
    (is (= "" (hidden-input-value node routes/search-param)))
    (is (nil? (input-by-name node routes/selected-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))))

(deftest board-state-hidden-inputs-test
  (testing "board-state form omits q because visible search input owns q"
    (let [node (views/board-state-hidden-inputs
                {:search "garden"
                 :selected-request-id "hh-req-1"
                 :visible-revision 3})]
      (is (nil? (input-by-name node routes/search-param)))
      (is (= "hh-req-1" (hidden-input-value node routes/selected-param)))
      (is (= 3 (hidden-input-value node routes/visible-revision-param))))))

(deftest oob-response-test
  (let [node (views/oob-response
              nil
              [:div {:id "one"} "one"]
              nil
              [:div {:id "two"} "two"])]
    (is (= :div (first node)))
    (is (= "contents" (get-in (attrs node) [:style :display])))
    (is (find-by-id node "one"))
    (is (find-by-id node "two"))
    (is (= 2 (count (children node))))))

;; -----------------------------------------------------------------------------
;; App bar
;; -----------------------------------------------------------------------------

(deftest brand-test
  (let [node (views/brand)]
    (is (= :a (first node)))
    (is (= routes/base-path (:href (attrs node))))
    (is (contains-text? node "Human Help"))))

(deftest logout-form-test
  (let [node (views/logout-form)]
    (is (= :form (first node)))
    (is (= "post" (:method (attrs node))))
    (is (= "/auth/signout" (:action (attrs node))))
    (is (contains-text? node "Log out"))))

(deftest user-menu-test
  (let [node (views/user-menu owner)]
    (is (= :details (first node)))
    (is (contains-text? node "Signed in as"))
    (is (contains-text? node "owner@example.com"))
    (is (contains-text? node "Log out"))
    (is (some #(= :summary (first %))
              (filter node? (hiccup-seq node))))))

(deftest app-bar-test
  (with-redefs [ui/theme-dialog
                (fn [_ctx opts]
                  [:theme-dialog opts])]
    (let [node (views/app-bar ctx owner)]
      (is (contains-text? node "Human Help"))
      (is (contains-text? node "owner@example.com"))
      (is (contains-text? node "Log out")))))

;; -----------------------------------------------------------------------------
;; Page shell
;; -----------------------------------------------------------------------------

(deftest hero-test
  (let [node (views/hero)]
    (is (contains-text? node "Welcome to Human Help."))
    (is (some #(= :h1 (first %))
              (filter node? (hiccup-seq node))))))

(deftest page-test
  (testing "page composes shell, listener, toolbar, search, list, and dialog"
    (with-redefs [ui/page-shell
                  (fn [ctx opts & body]
                    (into [:page-shell {:ctx ctx
                                        :opts opts}]
                          body))

                  client-plumbing/listener
                  (fn [& args]
                    [:listener {:id "client-listener"
                                :args args}])]
      (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar panel"]
            request-list [:div {:id views/request-list-dom-id} "list panel"]
            node (views/page
                  ctx
                  {:user owner
                   :view-state view-state
                   :request-toolbar-panel toolbar
                   :request-list-panel request-list})]
        (is (= :page-shell (first node)))
        (is (= ctx (:ctx (attrs node))))
        (is (= {:user owner} (:opts (attrs node))))

        (is (find-by-id node "client-listener"))
        (is (find-by-id node views/request-toolbar-dom-id))
        (is (find-by-id node views/request-list-dom-id))
        (is (find-by-id node views/create-request-dialog-id))
        (is (find-by-id node views/board-state-form-id))

        (is (contains-text? node "Welcome to Human Help."))
        (is (contains-text? node "toolbar panel"))
        (is (contains-text? node "list panel"))))))

(deftest page-fallback-panels-test
  (with-redefs [ui/page-shell
                (fn [_ctx _opts & body]
                  (into [:page-shell] body))

                client-plumbing/listener
                (fn [& _args]
                  [:listener {:id "client-listener"}])]
    (let [node (views/page
                ctx
                {:user owner
                 :view-state view-state})]
      (is (find-by-id node views/request-toolbar-dom-id))
      (is (find-by-id node views/request-list-dom-id))
      (is (contains-text? node "Request toolbar loading"))
      (is (contains-text? node "Request list loading")))))

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(deftest refresh-button-class-test
  (testing "fresh button has baseline classes"
    (let [class (views/refresh-button-class false)]
      (is (str/includes? class "inline-flex"))
      (is (str/includes? class "control-theme"))
      (is (not (str/includes? class "shadow-lg")))))

  (testing "stale button gets highlight class"
    (is (str/includes? (views/refresh-button-class true)
                       "shadow-lg"))))

(deftest refresh-button-style-test
  (is (= "var(--card)"
         (:background (views/refresh-button-style false))))
  (is (= "var(--primary)"
         (:background (views/refresh-button-style true)))))

(deftest refresh-form-test
  (let [node (views/refresh-form ctx view-state false)]
    (is (= :form (first node)))
    (is (= "post" (:method (attrs node))))
    (is (= (routes/refresh-requests-url)
           (:hx-post (attrs node))))
    (is (= "none" (:hx-swap (attrs node))))
    (is (input-by-name node "__anti-forgery-token"))
    (is (= "garden" (hidden-input-value node routes/search-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))
    (is (contains-text? node "Refresh"))))

(deftest request-toolbar-fragment-fresh-test
  (let [node (views/request-toolbar-fragment
              {:ctx ctx
               :user owner
               :view-state view-state
               :open-count 2
               :pending-open-count 0
               :stale? false
               :latest-revision 3})]
    (is (= views/request-toolbar-dom-id (:id (attrs node))))
    (is (= "request-toolbar"
           (:data-humanhelp-fragment (attrs node))))
    (is (= 3 (:data-latest-revision (attrs node))))
    (is (contains-text? node "Requests"))
    (is (contains-text? node "Open"))
    (is (contains-text? node "2 open"))
    (is (not (contains-text? node "New request data is available")))

    (is (form-by-hx-post node (routes/refresh-requests-url)))
    (is (find-first
         node
         #(and (= :button (first %))
               (= "Create request" (:aria-label (attrs %)))
               (str/includes? (or (:onclick (attrs %)) "")
                              views/create-request-dialog-id))))))

(deftest request-toolbar-fragment-stale-test
  (let [node (views/request-toolbar-fragment
              {:ctx ctx
               :user owner
               :view-state view-state
               :open-count 3
               :pending-open-count 1
               :stale? true
               :latest-revision 4})]
    (is (= 4 (:data-latest-revision (attrs node))))
    (is (contains-text? node "+1 new"))
    (is (contains-text? node "New request data is available. Refresh when you are ready."))
    (is (contains-text? node "Refresh"))))

;; -----------------------------------------------------------------------------
;; Search control
;; -----------------------------------------------------------------------------

(deftest search-control-test
  (let [node (views/search-control {:view-state view-state})]
    (is (= :form (first node)))
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= "get" (:method (attrs node))))
    (is (= (routes/search-requests-url)
           (:hx-get (attrs node))))
    (is (= (str "#" views/request-list-dom-id)
           (:hx-target (attrs node))))
    (is (= "outerHTML" (:hx-swap (attrs node))))
    (is (str/includes? (:hx-trigger (attrs node))
                       "keyup changed delay:250ms"))
    (is (str/includes? (:hx-trigger (attrs node))
                       "search from:#humanhelp-search"))

    (testing "search form has one q input, and it is the visible search input"
      (is (= 1
             (count
              (filter #(= routes/search-param (:name (attrs %)))
                      (inputs node)))))
      (let [search-input (find-by-id node "humanhelp-search")]
        (is search-input)
        (is (= routes/search-param (:name (attrs search-input))))
        (is (= "search" (:type (attrs search-input))))
        (is (= "garden" (:value (attrs search-input))))))

    (testing "board-state hidden inputs are present, but hidden q is not"
      (is (nil? (input-by-name
                 (first (children node))
                 routes/search-param)))
      (is (= 3 (hidden-input-value node routes/visible-revision-param))))))

(deftest search-control-empty-search-test
  (let [node (views/search-control
              {:view-state {:search nil
                            :selected-request-id nil
                            :visible-revision 3}})
        search-input (find-by-id node "humanhelp-search")]
    (is (= "" (:value (attrs search-input))))))

;; -----------------------------------------------------------------------------
;; Request list
;; -----------------------------------------------------------------------------

(deftest empty-request-list-test
  (testing "empty without search prompts creation"
    (let [node (views/empty-request-list
                {:view-state {:search ""}})]
      (is (contains-text? node "No requests yet"))
      (is (contains-text? node "Create a request"))))

  (testing "empty with search prompts narrowing search"
    (let [node (views/empty-request-list
                {:view-state {:search "purple unicorn"}})]
      (is (contains-text? node "No matching requests"))
      (is (contains-text? node "Try fewer words")))))

(deftest request-list-fragment-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user owner
               :view-state view-state
               :requests [open-request claimed-request]
               :latest-revision 3})]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (= "request-list"
           (:data-humanhelp-fragment (attrs node))))
    (is (= 3 (:data-latest-revision (attrs node))))
    (is (find-by-id node "humanhelp-request-hh-req-1"))
    (is (find-by-id node "humanhelp-request-hh-req-2"))
    (is (contains-text? node "Need help finding a rake"))
    (is (contains-text? node "Can someone help load soil?"))))

(deftest request-list-fragment-selected-card-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user owner
               :view-state selected-view-state
               :requests [open-request]
               :latest-revision 3})]
    (is (contains-text? node "Looking for a sturdy rake for leaves."))
    (is (contains? (hx-posts node)
                   (routes/action-url (:request/id open-request) :done)))
    (is (contains? (hx-posts node)
                   (routes/action-url (:request/id open-request) :cancel)))))

(deftest request-list-fragment-claimed-card-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user helper
               :view-state (assoc view-state
                                   :selected-request-id
                                   (:request/id claimed-request))
               :requests [claimed-request]
               :latest-revision 3})]
    (is (contains-text? node "Can someone help load soil?"))
    (is (contains? (hx-posts node)
                   (routes/action-url (:request/id claimed-request) :done)))
    (is (contains? (hx-posts node)
                   (routes/action-url (:request/id claimed-request) :unclaim)))
    (is (contains? (hx-posts node)
                   (routes/action-url (:request/id claimed-request) :cancel)))))

(deftest request-list-fragment-empty-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user owner
               :view-state {:search "notfound"}
               :requests []
               :latest-revision 3})]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (contains-text? node "No matching requests"))))

;; -----------------------------------------------------------------------------
;; Create request dialog
;; -----------------------------------------------------------------------------

(deftest field-error-test
  (testing "missing field error produces no node"
    (is (nil? (views/field-error {} :title))))

  (testing "present field error renders destructive text"
    (let [node (views/field-error {:title "Title required."} :title)]
      (is (= :p (first node)))
      (is (contains-text? node "Title required."))
      (is (= "var(--destructive)"
             (get-in (attrs node) [:style :color]))))))

(deftest create-request-dialog-content-test
  (let [node (views/create-request-dialog-content
              ctx
              {:user owner
               :values {:customer-name "Avery"
                        :area "Paint"
                        :title "Need blue paint"
                        :details "Exterior paint"}
               :errors {}})]
    (is (= views/create-request-dialog-body-id (:id (attrs node))))
    (is (contains-text? node "Create request"))
    (is (contains-text? node "Everyone can make and service requests"))

    (let [form (first (forms node))]
      (is (= "post" (:method (attrs form))))
      (is (= (routes/create-request-url)
             (:hx-post (attrs form))))
      (is (= "none" (:hx-swap (attrs form))))
      (is (input-by-name form "__anti-forgery-token")))

    (is (= "Avery" (:value (attrs (input-by-name node "customer-name")))))
    (is (= "Paint" (:value (attrs (input-by-name node "area")))))
    (is (= "Need blue paint" (:value (attrs (input-by-name node "title")))))
    (is (contains-text? node "Exterior paint"))

    (is (some #(and (= :button (first %))
                    (= "button" (:type (attrs %)))
                    (str/includes? (or (:onclick (attrs %)) "")
                                   ".close()"))
              (buttons node)))
    (is (some #(and (= :button (first %))
                    (= "submit" (:type (attrs %))))
              (buttons node)))))

(deftest create-request-dialog-content-default-customer-test
  (let [node (views/create-request-dialog-content
              ctx
              {:user owner
               :values {}
               :errors {}})]
    (is (= "owner@example.com"
           (:value (attrs (input-by-name node "customer-name")))))))

(deftest create-request-dialog-content-errors-test
  (let [node (views/create-request-dialog-content
              ctx
              {:user owner
               :values {:customer-name ""
                        :area ""
                        :title ""
                        :details ""}
               :errors {:customer-name "Name too long."
                        :area "Area required."
                        :title "Title required."
                        :details "Details too long."}})]
    (is (contains-text? node "Name too long."))
    (is (contains-text? node "Area required."))
    (is (contains-text? node "Title required."))
    (is (contains-text? node "Details too long."))))

(deftest create-request-dialog-test
  (testing "closed dialog omits open attr"
    (let [node (views/create-request-dialog
                ctx
                {:user owner
                 :values {}
                 :errors {}
                 :open? false})]
      (is (= :dialog (first node)))
      (is (= views/create-request-dialog-id (:id (attrs node))))
      (is (nil? (:open (attrs node))))
      (is (find-by-id node views/create-request-dialog-body-id))))

  (testing "open dialog has open attr"
    (let [node (views/create-request-dialog
                ctx
                {:user owner
                 :values {}
                 :errors {}
                 :open? true})]
      (is (= :dialog (first node)))
      (is (= views/create-request-dialog-id (:id (attrs node))))
      (is (true? (:open (attrs node)))))))

(deftest create-request-dialog-fragment-test
  (let [node (views/create-request-dialog-fragment
              ctx
              {:user owner
               :values {:title "Need help"}
               :errors {}
               :open? true})]
    (is (= :dialog (first node)))
    (is (= views/create-request-dialog-id (:id (attrs node))))
    (is (true? (:open (attrs node))))
    (is (contains-text? node "Create request"))))

;; -----------------------------------------------------------------------------
;; OOB helpers
;; -----------------------------------------------------------------------------

(deftest replace-toolbar-oob-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        node (views/replace-toolbar-oob toolbar)]
    (is (= views/request-toolbar-dom-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (contains-text? node "toolbar"))))

(deftest replace-request-list-oob-test
  (let [request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/replace-request-list-oob request-list)]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (contains-text? node "list"))))

(deftest replace-dialog-oob-test
  (let [dialog [:dialog {:id views/create-request-dialog-id} "dialog"]
        node (views/replace-dialog-oob dialog)]
    (is (= views/create-request-dialog-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (contains-text? node "dialog"))))

(deftest fragments-oob-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/fragments-oob
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "toolbar"))
    (is (contains-text? node "list"))))

(deftest replace-board-state-oob-test
  (let [node (views/replace-board-state-oob ctx view-state)]
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (= (routes/search-requests-url)
           (:hx-get (attrs node))))
    (is (= "garden"
           (:value (attrs (find-by-id node "humanhelp-search")))))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))))

(deftest with-board-state-oob-test
  (let [payload [:div {:id "payload"} "payload"]
        node (views/with-board-state-oob ctx view-state payload)
        kids (children node)
        board-state-oob (first kids)]
    (is (= :div (first node)))
    (is (= "contents" (get-in (attrs node) [:style :display])))
    (is (= views/board-state-form-id (:id (attrs board-state-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs board-state-oob))))
    (is (find-by-id node "payload"))
    (is (contains-text? node "payload"))))

;; -----------------------------------------------------------------------------
;; OOB result views
;; -----------------------------------------------------------------------------

(deftest create-request-validation-error-test
  (let [node (views/create-request-validation-error
              ctx
              {:user owner
               :values {:title ""
                        :area ""}
               :errors {:title "Title required."
                        :area "Area required."}})
        dialog (oob-by-id node views/create-request-dialog-id)]
    (is dialog)
    (is (= "outerHTML" (:hx-swap-oob (attrs dialog))))
    (is (true? (:open (attrs dialog))))
    (is (contains-text? node "Title required."))
    (is (contains-text? node "Area required."))))

(deftest create-request-success-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/create-request-success
              ctx
              {:user owner
               :request open-request
               :toolbar toolbar
               :request-list request-list})
        toolbar-oob (oob-by-id node views/request-toolbar-dom-id)
        list-oob (oob-by-id node views/request-list-dom-id)
        dialog-oob (oob-by-id node views/create-request-dialog-id)]
    (is toolbar-oob)
    (is list-oob)
    (is dialog-oob)
    (is (= "outerHTML" (:hx-swap-oob (attrs toolbar-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs list-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs dialog-oob))))
    (is (nil? (:open (attrs dialog-oob))))
    (is (contains-text? node "toolbar"))
    (is (contains-text? node "request list"))
    (is (contains-text? node "Request created"))
    (is (contains-text? node "Request #1 is now on the board."))))

(deftest refreshed-request-board-fragments-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/refreshed-request-board-fragments
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "toolbar"))
    (is (contains-text? node "list"))))

(deftest request-lifecycle-result-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/request-lifecycle-result
              {:action :claim
               :request claimed-request
               :toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "Claim"))
    (is (contains-text? node "Claimed request #2."))))

(deftest request-lifecycle-result-without-toast-data-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/request-lifecycle-result
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (not (contains-text? node "Claimed request")))))

(deftest request-action-error-test
  (testing "specific nested error message is rendered"
    (let [node (views/request-action-error
                {:result {:status :error
                          :error {:message "Cannot claim this request."}}})]
      (is (contains-text? node "Request not updated"))
      (is (contains-text? node "Cannot claim this request."))))

  (testing "fallback message is rendered"
    (let [node (views/request-action-error
                {:result {:status :error}})]
      (is (contains-text? node "Request not updated"))
      (is (contains-text? node "That request action could not be completed.")))))

(deftest reset-demo-result-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/reset-demo-result
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "Demo reset"))
    (is (contains-text? node "The Human Help request board was reset."))))
