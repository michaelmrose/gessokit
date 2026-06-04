(ns gessokit.humanhelp.views-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.core :as g]
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
    :request/created-at-ms (model/now-ms)
    :request/updated-at-ms (model/now-ms)
    :request/created-revision 1
    :request/updated-revision 1}
   overrides))

(def open-request
  (request {}))

(def claimed-request
  (request
   {:request/status :claimed
    :request/claimed-by "user-helper"
    :request/claimed-by-email "helper@example.com"}))

(def done-request
  (request
   {:request/status :done
    :request/claimed-by "user-helper"
    :request/claimed-by-email "helper@example.com"}))

(def cancelled-request
  (request
   {:request/status :cancelled}))

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

(defn anchors
  [tree]
  (find-elements tree :a))

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

(defn anchor-by-hx-get
  [tree url]
  (some
   (fn [node]
     (when (= url (:hx-get (attrs node)))
       node))
   (anchors tree)))

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

;; -----------------------------------------------------------------------------
;; Constants and tiny helpers
;; -----------------------------------------------------------------------------

(deftest dom-id-constants-test
  (testing "fragment and dialog DOM ids are stable strings"
    (doseq [id [views/request-toolbar-dom-id
                views/request-list-dom-id
                views/create-request-dialog-id
                views/create-request-dialog-body-id
                views/board-state-form-id]]
      (is (string? id))
      (is (not (str/blank? id)))))

  (testing "fragment ids are distinct"
    (is (= 5
           (count
            (set [views/request-toolbar-dom-id
                  views/request-list-dom-id
                  views/create-request-dialog-id
                  views/create-request-dialog-body-id
                  views/board-state-form-id]))))))

(deftest user-email-test
  (is (= "owner@example.com"
         (views/user-email {:user/id "user-owner"
                            :user/email "owner@example.com"})))
  (is (= "user-owner"
         (views/user-email {:user/id "user-owner"})))
  (is (= "demo-user"
         (views/user-email {})))
  (is (= "demo-user"
         (views/user-email nil))))

(deftest status-label-and-pill-status-test
  (testing "status label delegates to domain labels"
    (is (= "Open" (views/status-label open-request)))
    (is (= "Claimed" (views/status-label claimed-request)))
    (is (= "Done" (views/status-label done-request)))
    (is (= "Cancelled" (views/status-label cancelled-request))))

  (testing "request statuses map to visual pill statuses"
    (is (= :waiting (views/status-pill-status open-request)))
    (is (= :active (views/status-pill-status claimed-request)))
    (is (= :success (views/status-pill-status done-request)))
    (is (= :muted (views/status-pill-status cancelled-request)))
    (is (= :destructive
           (views/status-pill-status
            (assoc open-request :request/status :mystery))))))

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

  (testing "blank strings are still preserved because they are concrete values"
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
;; Form/action helpers
;; -----------------------------------------------------------------------------

(deftest form-action-test
  (let [node (views/form-action
              ctx
              {:to "/app/requests/hh-req-1/claim"
               :text "Claim"
               :variant :primary
               :size :sm
               :view-state view-state
               :attrs {:data-test "claim-form"}})]
    (is (= :form (first node)))
    (is (= "post" (:method (attrs node))))
    (is (= "/app/requests/hh-req-1/claim" (:hx-post (attrs node))))
    (is (= "none" (:hx-swap (attrs node))))
    (is (= "inline-flex" (:class (attrs node))))
    (is (= "claim-form" (:data-test (attrs node))))
    (is (input-by-name node "__anti-forgery-token"))
    (is (= "garden" (hidden-input-value node routes/search-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))
    (is (contains-text? node "Claim"))))

(deftest action-button-test
  (testing "action-button posts to the action URL"
    (doseq [action [:claim :unclaim :take-over :done :cancel]]
      (let [node (views/action-button
                  ctx
                  open-request
                  helper
                  action
                  view-state)
            expected-url (routes/action-url (:request/id open-request)
                                            action)]
        (is (= expected-url (:hx-post (attrs node))))
        (is (contains-text? node (model/action-label action)))))))

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
    (is (contains-text? node "owner@example.com"))
    (is (contains-text? node "Signed in as"))
    (is (contains-text? node "Log out"))
    (is (some #(= :summary (first %))
              (filter node? (hiccup-seq node))))))

;; -----------------------------------------------------------------------------
;; Page shell
;; -----------------------------------------------------------------------------

(deftest hero-test
  (let [node (views/hero)]
    (is (contains-text? node "Welcome to Human Help."))
    (is (some #(= :h1 (first %))
              (filter node? (hiccup-seq node))))))

(deftest page-test
  (testing "page composes page shell, listener, bar, panels, search, and dialog"
    (with-redefs [ui/page-shell
                  (fn [_ctx & body]
                    (into [:page-shell] body))

                  client-plumbing/listener
                  (fn [_ctx]
                    [:listener {:id "client-listener"}])

                  ui/theme-dialog
                  (fn [_ctx opts]
                    [:theme-dialog opts])

                  g/bars
                  (fn [opts & children]
                    (into [:bars opts] children))]
      (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar panel"]
            request-list [:div {:id views/request-list-dom-id} "list panel"]
            node (views/page
                  ctx
                  {:user owner
                   :view-state view-state
                   :request-toolbar-panel toolbar
                   :request-list-panel request-list})]
        (is (= :page-shell (first node)))
        (is (find-by-id node "client-listener"))
        (is (find-by-id node views/request-toolbar-dom-id))
        (is (find-by-id node views/request-list-dom-id))
        (is (find-by-id node views/create-request-dialog-id))
        (is (find-by-id node views/board-state-form-id))
        (is (contains-text? node "Welcome to Human Help."))
        (is (contains-text? node "toolbar panel"))
        (is (contains-text? node "list panel"))
        (is (contains-text? node "owner@example.com"))))))

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(deftest refresh-button-class-test
  (testing "refresh button always has baseline classes"
    (let [class (views/refresh-button-class false)]
      (is (str/includes? class "inline-flex"))
      (is (str/includes? class "control-theme"))
      (is (str/includes? class "border-theme"))
      (is (not (str/includes? class "shadow-lg")))))

  (testing "stale refresh button gets extra highlight class"
    (is (str/includes? (views/refresh-button-class true)
                       "shadow-lg"))))


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
    (is (not (contains-text? node "New request data is available")))

    (let [refresh-form (form-by-hx-post
                        node
                        (routes/refresh-requests-url))]
      (is refresh-form)
      (is (= "post" (:method (attrs refresh-form))))
      (is (= "none" (:hx-swap (attrs refresh-form))))
      (is (= "garden"
             (hidden-input-value refresh-form routes/search-param)))
      (is (= 3
             (hidden-input-value refresh-form
                                 routes/visible-revision-param))))

    (let [plus-button (find-first
                       node
                       #(and (= :button (first %))
                             (= "Create request" (:aria-label (attrs %)))))]
      (is plus-button)
      (is (str/includes? (:onclick (attrs plus-button))
                         views/create-request-dialog-id))
      (is (str/includes? (:onclick (attrs plus-button))
                         ".showModal()")))))



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
    (let [refresh-button (first (buttons node))]
      (is (= "var(--primary)"
             (get-in (attrs refresh-button) [:style :border-color])))
      (is (= "var(--primary)"
             (get-in (attrs refresh-button) [:style :background]))))))

;; -----------------------------------------------------------------------------
;; Search control
;; -----------------------------------------------------------------------------

(deftest search-control-test
  (let [node (views/search-control {:view-state view-state})]
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= "get" (:method (attrs node))))
    (is (= (routes/search-requests-url)
           (:hx-get (attrs node))))
    (is (= (str "#" views/request-list-dom-id)
           (:hx-target (attrs node))))
    (is (= "outerHTML" (:hx-swap (attrs node))))
    (is (str/includes? (:hx-trigger (attrs node))
                       "delay:250ms"))

    ;; The search form must not duplicate q in both a hidden input and the
    ;; visible search input. The visible search input is the only q source.
    (is (= 1
           (count
            (filter #(= routes/search-param (:name (attrs %)))
                    (inputs node)))))

    (let [search-input (find-by-id node "humanhelp-search")]
      (is search-input)
      (is (= routes/search-param (:name (attrs search-input))))
      (is (= "search" (:type (attrs search-input))))
      (is (= "garden" (:value (attrs search-input)))))))

(deftest search-control-empty-search-test
  (let [node (views/search-control
              {:view-state {:search nil
                            :selected-request-id nil
                            :visible-revision 3}})
        search-input (find-by-id node "humanhelp-search")]
    (is (= "" (:value (attrs search-input))))))

;; -----------------------------------------------------------------------------
;; Request cards
;; -----------------------------------------------------------------------------

(deftest card-selected-test
  (is (true?
       (views/card-selected?
        open-request
        {:selected-request-id (:request/id open-request)})))
  (is (false?
       (views/card-selected?
        open-request
        {:selected-request-id "something-else"})))
  (is (false?
       (views/card-selected?
        open-request
        {:selected-request-id nil}))))

(deftest request-card-style-test
  (testing "unselected cards use normal border"
    (let [style (views/request-card-style false)]
      (is (= "var(--border)" (:border-color style)))
      (is (nil? (:box-shadow style)))))

  (testing "selected cards use primary border and shadow"
    (let [style (views/request-card-style true)]
      (is (= "var(--primary)" (:border-color style)))
      (is (string? (:box-shadow style))))))

(deftest request-meta-test
  (let [node (views/request-meta open-request)]
    (is (contains-text? node "Garden"))
    (is (contains-text? node "waiting"))))

(deftest request-card-unselected-test
  (let [node (views/request-card
              ctx
              {:request open-request
               :user helper
               :view-state view-state})
        expected-select-url (routes/select-request-url
                             (:request/id open-request)
                             view-state)]
    (is (= :article (first node)))
    (is (= (str "humanhelp-request-" (:request/id open-request))
           (:id (attrs node))))
    (is (contains-text? node "Need help finding a rake"))
    (is (contains-text? node "Jon"))
    (is (contains-text? node "Garden"))

    (let [card-link (anchor-by-hx-get node expected-select-url)]
      (is card-link)
      (is (= expected-select-url (:href (attrs card-link))))
      (is (= (str "#" views/request-list-dom-id)
             (:hx-target (attrs card-link))))
      (is (= "outerHTML" (:hx-swap (attrs card-link)))))

    (testing "unselected card does not render details/actions"
      (is (not (contains-text? node "Looking for a sturdy rake for leaves.")))
      (is (empty? (forms node))))))

(deftest request-card-selected-owner-actions-test
  (let [selected-state (assoc view-state
                              :selected-request-id
                              (:request/id open-request))
        node (views/request-card
              ctx
              {:request open-request
               :user owner
               :view-state selected-state})]
    (is (contains-text? node "Looking for a sturdy rake for leaves."))
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id open-request) :done)))
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id open-request) :cancel)))
    (is (not
         (form-by-hx-post
          node
          (routes/action-url (:request/id open-request) :claim))))))

(deftest request-card-selected-helper-actions-test
  (let [selected-state (assoc view-state
                              :selected-request-id
                              (:request/id open-request))
        node (views/request-card
              ctx
              {:request open-request
               :user helper
               :view-state selected-state})]
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id open-request) :claim)))
    (is (not
         (form-by-hx-post
          node
          (routes/action-url (:request/id open-request) :done))))))

(deftest request-card-selected-claimer-actions-test
  (let [selected-state (assoc view-state
                              :selected-request-id
                              (:request/id claimed-request))
        node (views/request-card
              ctx
              {:request claimed-request
               :user helper
               :view-state selected-state})]
    (is (contains-text? node "claimed by helper@example.com"))
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id claimed-request) :done)))
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id claimed-request) :unclaim)))
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id claimed-request) :cancel)))))

(deftest request-card-selected-take-over-actions-test
  (let [selected-state (assoc view-state
                              :selected-request-id
                              (:request/id claimed-request))
        node (views/request-card
              ctx
              {:request claimed-request
               :user other-user
               :view-state selected-state})]
    (is (form-by-hx-post
         node
         (routes/action-url (:request/id claimed-request) :take-over)))
    (is (not
         (form-by-hx-post
          node
          (routes/action-url (:request/id claimed-request) :unclaim))))))

(deftest request-card-terminal-actions-test
  (doseq [request [done-request cancelled-request]]
    (let [selected-state (assoc view-state
                                :selected-request-id
                                (:request/id request))
          node (views/request-card
                ctx
                {:request request
                 :user owner
                 :view-state selected-state})]
      (is (empty? (forms node))
          (str "Terminal request should not render forms: "
               (:request/status request))))))

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
    (is (= "request-list" (:data-humanhelp-fragment (attrs node))))
    (is (= 3 (:data-latest-revision (attrs node))))
    (is (find-by-id node "humanhelp-request-hh-req-1"))
    (is (contains-text? node "Need help finding a rake"))))

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
  (testing "missing error returns nil"
    (is (nil? (views/field-error {} :title))))

  (testing "present error renders text"
    (let [node (views/field-error {:title "Title is required."} :title)]
      (is (= :p (first node)))
      (is (contains-text? node "Title is required."))
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
      (is (= (routes/create-request-url) (:hx-post (attrs form))))
      (is (= "none" (:hx-swap (attrs form))))
      (is (input-by-name form "__anti-forgery-token")))

    (is (= "Avery" (:value (attrs (input-by-name node "customer-name")))))
    (is (= "Paint" (:value (attrs (input-by-name node "area")))))
    (is (= "Need blue paint" (:value (attrs (input-by-name node "title")))))
    (is (contains-text? node "Exterior paint"))

    (is (some #(and (= :button (first %))
                    (= "button" (:type (attrs %)))
                    (str/includes? (:onclick (attrs %))
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
      (is (true? (:open (attrs node)))))))

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
    (is (contains-text? node "request list"))))

(deftest refreshed-request-board-fragments-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/refreshed-request-board-fragments
              ctx
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
              ctx
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))))

(deftest request-action-error-test
  (testing "specific message is rendered"
    (let [node (views/request-action-error
                ctx
                {:result {:status :error
                          :error {:message "Cannot claim this request."}}})]
      (is (contains-text? node "Request not updated"))
      (is (contains-text? node "Cannot claim this request."))))

  (testing "fallback message is rendered"
    (let [node (views/request-action-error
                ctx
                {:result {:status :error}})]
      (is (contains-text? node "Request not updated"))
      (is (contains-text? node "That request action could not be completed.")))))

(deftest reset-demo-result-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/reset-demo-result
              ctx
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "Demo reset"))
    (is (contains-text? node "The Human Help request board was reset."))))
