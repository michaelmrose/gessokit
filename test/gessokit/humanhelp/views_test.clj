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

(def uuid-user
  {:user/id "aef38467-ef20-4f58-b9ab-f9c0fb4c9bb2"
   :user/email "aef38467-ef20-4f58-b9ab-f9c0fb4c9bb2"})

(def helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

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

(defn button-by-aria-label
  [tree label]
  (find-first
   tree
   #(and (= :button (first %))
         (= label (:aria-label (attrs %))))))

#_(defn dialog-node
  [tree]
  (find-first tree #(= :dialog (first %))))

#_(defn open-dialog?
  [tree]
  (true? (:open (attrs (dialog-node tree)))))

(defn dialog-root
  [tree]
  (find-first tree #(true? (:data-dialog-root (attrs %)))))

(defn dialog-content
  [tree]
  (find-first tree #(= "dialog" (:role (attrs %)))))

(defn open-dialog?
  [tree]
  (let [root (dialog-root tree)
        content (dialog-content tree)]
    (and (= "true" (:data-dialog-open (attrs root)))
         (not (true? (:hidden (attrs content)))))))

;; -----------------------------------------------------------------------------
;; Stable DOM / board-state contract
;; -----------------------------------------------------------------------------

(deftest stable-dom-id-test
  (doseq [id [views/request-toolbar-dom-id
              views/request-list-dom-id
              views/create-request-dialog-id
              views/create-request-dialog-body-id
              views/board-state-form-id]]
    (is (string? id))
    (is (not (str/blank? id))))

  (is (= 5
         (count
          (set [views/request-toolbar-dom-id
                views/request-list-dom-id
                views/create-request-dialog-id
                views/create-request-dialog-body-id
                views/board-state-form-id])))))

(deftest board-state-selector-test
  (is (= (str "#" views/board-state-form-id)
         (views/board-state-selector)))

  (is (= (str "#" views/board-state-form-id
              " input[name="
              routes/selected-param
              "]")
         (views/selected-state-input-selector))))

(deftest account-email-test
  (testing "real email is displayable"
    (is (= "owner@example.com"
           (views/account-email owner))))

  (testing "ids and UUID-like strings are not display emails"
    (is (nil? (views/account-email {:user/id "user-owner"})))
    (is (nil? (views/account-email uuid-user)))
    (is (nil? (views/account-email {})))
    (is (nil? (views/account-email nil)))))

(deftest hidden-input-contract-test
  (testing "ordinary hidden-input omits nil but preserves concrete blank"
    (is (nil? (views/hidden-input "q" nil)))

    (let [node (views/hidden-input "q" "")]
      (is (= :input (first node)))
      (is (= "hidden" (:type (attrs node))))
      (is (= "q" (:name (attrs node))))
      (is (= "" (:value (attrs node))))))

  (testing "hidden-input-present always renders a stable input"
    (let [node (views/hidden-input-present routes/selected-param nil)]
      (is (= :input (first node)))
      (is (= "hidden" (:type (attrs node))))
      (is (= routes/selected-param (:name (attrs node))))
      (is (= "" (:value (attrs node)))))))

(deftest board-state-hidden-inputs-test
  (let [node (views/board-state-hidden-inputs
              {:search "garden"
               :selected-request-id nil
               :visible-revision 3})]
    (testing "q/search is deliberately omitted because the visible search input owns q"
      (is (nil? (input-by-name node routes/search-param))))

    (testing "selected request input is stable even when blank"
      (is (= "" (hidden-input-value node routes/selected-param))))

    (testing "visible revision is preserved"
      (is (= 3 (hidden-input-value node routes/visible-revision-param))))))

(deftest full-view-state-hidden-inputs-test
  (let [node (views/view-state-hidden-inputs
              {:search "garden"
               :selected-request-id "hh-req-1"
               :visible-revision 3})]
    (is (= "garden" (hidden-input-value node routes/search-param)))
    (is (= "hh-req-1" (hidden-input-value node routes/selected-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))))

;; -----------------------------------------------------------------------------
;; Page composition
;; -----------------------------------------------------------------------------

(deftest page-composition-test
  (testing "page passes user to shell and installs the app listener with board-state include"
    (with-redefs [ui/page-shell
                  (fn [ctx opts & body]
                    (into [:page-shell {:ctx ctx
                                        :opts opts}]
                          body))

                  ui/container
                  (fn [& body]
                    (into [:container] body))

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
                   :request-list-panel request-list})
            listener (find-by-id node "client-listener")]
        (is (= :page-shell (first node)))
        (is (= ctx (:ctx (attrs node))))
        (is (= {:user owner} (:opts (attrs node))))

        (is listener)
        (is (= ctx (first (:args (attrs listener)))))
        (is (= {:trigger-attrs {:hx-include (views/board-state-selector)}}
               (second (:args (attrs listener)))))

        (is (find-by-id node views/request-toolbar-dom-id))
        (is (find-by-id node views/request-list-dom-id))
        (is (find-by-id node views/board-state-form-id))
        (is (contains-text? node "Human Help Fast."))
        (is (contains-text? node "toolbar panel"))
        (is (contains-text? node "list panel"))))))

(deftest board-card-fallback-test
  (let [node (views/board-card
              ctx
              {:view-state view-state})]
    (is (find-by-id node views/request-toolbar-dom-id))
    (is (find-by-id node views/request-list-dom-id))
    (is (find-by-id node views/board-state-form-id))
    (is (contains-text? node "Request toolbar loading"))
    (is (contains-text? node "Request list loading"))))

;; -----------------------------------------------------------------------------
;; HTMX form wiring
;; -----------------------------------------------------------------------------

(deftest search-control-contract-test
  (let [node (views/search-control ctx {:view-state view-state})]
    (is (= :form (first node)))
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= (routes/search-requests-url) (:hx-get (attrs node))))
    (is (= (str "#" views/request-list-dom-id) (:hx-target (attrs node))))
    (is (= "outerHTML" (:hx-swap (attrs node))))

    (testing "search form has exactly one q input"
      (is (= 1
             (count
              (filter #(= routes/search-param (:name (attrs %)))
                      (inputs node))))))

    (testing "the q input is the visible search input"
      (let [search-input (find-by-id node "humanhelp-search")]
        (is search-input)
        (is (= routes/search-param (:name (attrs search-input))))
        (is (= "search" (:type (attrs search-input))))
        (is (= "garden" (:value (attrs search-input))))))

    (testing "board state travels with search submissions"
      (is (= "" (hidden-input-value node routes/selected-param)))
      (is (= 3 (hidden-input-value node routes/visible-revision-param))))))

(deftest refresh-form-contract-test
  (let [node (views/refresh-form ctx view-state true)]
    (is (= :form (first node)))
    (is (= (routes/refresh-requests-url) (:hx-post (attrs node))))
    (is (= "none" (:hx-swap (attrs node))))
    (is (= (views/board-state-selector) (:hx-include (attrs node))))
    (is (input-by-name node "__anti-forgery-token"))

    (let [button (button-by-aria-label
                  node
                  "Refresh requests. New request data is available.")]
      (is button)
      (is (= "submit" (:type (attrs button))))
      (is (= "New requests received" (:title (attrs button)))))))

(deftest create-request-form-contract-test
  (let [node (views/create-request-form
              ctx
              {:user owner
               :values {:customer-name "Avery"
                        :area "Paint"
                        :title "Need blue paint"
                        :details "Exterior paint"}
               :errors {}})]
    (is (= :form (first node)))
    (is (= (routes/create-request-url) (:hx-post (attrs node))))
    (is (= "none" (:hx-swap (attrs node))))
    (is (= (views/board-state-selector) (:hx-include (attrs node))))
    (is (input-by-name node "__anti-forgery-token"))

    (is (= "Avery" (:value (attrs (input-by-name node "customer-name")))))
    (is (= "Paint" (:value (attrs (input-by-name node "area")))))
    (is (= "Need blue paint" (:value (attrs (input-by-name node "title")))))
    (is (contains-text? node "Exterior paint"))))

(deftest create-request-form-default-customer-test
  (testing "real email can default the customer-name field"
    (let [node (views/create-request-form
                ctx
                {:user owner
                 :values {}
                 :errors {}})]
      (is (= "owner@example.com"
             (:value (attrs (input-by-name node "customer-name")))))))

  (testing "UUID-looking values must not default the customer-name field"
    (let [node (views/create-request-form
                ctx
                {:user uuid-user
                 :values {}
                 :errors {}})]
      (is (= ""
             (:value (attrs (input-by-name node "customer-name"))))))))

;; -----------------------------------------------------------------------------
;; Fragment contracts
;; -----------------------------------------------------------------------------

(deftest request-toolbar-fragment-contract-test
  (let [fresh (views/request-toolbar-fragment
               {:ctx ctx
                :user owner
                :view-state view-state
                :open-count 2
                :pending-open-count 0
                :stale? false
                :latest-revision 3})
        stale (views/request-toolbar-fragment
               {:ctx ctx
                :user owner
                :view-state view-state
                :open-count 3
                :pending-open-count 1
                :stale? true
                :latest-revision 4})]
    (testing "fresh toolbar has stable fragment identity and refresh/create controls"
      (is (= views/request-toolbar-dom-id (:id (attrs fresh))))
      (is (= "request-toolbar" (:data-humanhelp-fragment (attrs fresh))))
      (is (= 3 (:data-latest-revision (attrs fresh))))
      (is (form-by-hx-post fresh (routes/refresh-requests-url)))
      (is (button-by-aria-label fresh "Create request")))

    (testing "stale toolbar advertises pending data"
      (is (= 4 (:data-latest-revision (attrs stale))))
      (is (contains-text? stale "+1 new"))
      (is (contains-text? stale "New request data is available")))))

(deftest request-list-fragment-contract-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user owner
               :view-state selected-view-state
               :requests [open-request claimed-request]
               :latest-revision 3})]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (= "request-list" (:data-humanhelp-fragment (attrs node))))
    (is (= 3 (:data-latest-revision (attrs node))))

    (is (find-by-id node "humanhelp-request-hh-req-1"))
    (is (find-by-id node "humanhelp-request-hh-req-2"))
    (is (contains-text? node "Need help finding a rake"))
    (is (contains-text? node "Can someone help load soil?"))

    (testing "selected/open request exposes lifecycle action forms"
      (is (contains? (hx-posts node)
                     (routes/action-url (:request/id open-request) :done)))
      (is (contains? (hx-posts node)
                     (routes/action-url (:request/id open-request) :cancel))))))

(deftest request-list-empty-state-test
  (testing "empty board without search encourages creation"
    (let [node (views/request-list-fragment
                {:ctx ctx
                 :user owner
                 :view-state {:search ""}
                 :requests []
                 :latest-revision 3})]
      (is (= views/request-list-dom-id (:id (attrs node))))
      (is (contains-text? node "No requests yet"))
      (is (contains-text? node "Create a request"))))

  (testing "empty board with search encourages narrowing search"
    (let [node (views/request-list-fragment
                {:ctx ctx
                 :user owner
                 :view-state {:search "purple unicorn"}
                 :requests []
                 :latest-revision 3})]
      (is (= views/request-list-dom-id (:id (attrs node))))
      (is (contains-text? node "No matching requests"))
      (is (contains-text? node "Try fewer words")))))

;; -----------------------------------------------------------------------------
;; Dialog contract
;; -----------------------------------------------------------------------------

(deftest create-request-dialog-contract-test
  (testing "closed dialog has stable id, body, and create form"
    (let [node (views/create-request-dialog
                ctx
                {:user owner
                 :values {}
                 :errors {}
                 :open? false})]
      (is (find-by-id node views/create-request-dialog-id))
      (is (not (open-dialog? node)))
      (is (find-by-id node views/create-request-dialog-body-id))
      (is (form-by-hx-post node (routes/create-request-url)))))

  (testing "validation dialog renders open with errors"
    (let [node (views/create-request-dialog
                ctx
                {:user owner
                 :values {:title ""
                          :area ""}
                 :errors {:title "Title required."
                          :area "Area required."}
                 :open? true})]
      (is (find-by-id node views/create-request-dialog-id))
      (is (open-dialog? node))
      (is (contains-text? node "Title required."))
      (is (contains-text? node "Area required.")))))

;; -----------------------------------------------------------------------------
;; OOB contracts
;; -----------------------------------------------------------------------------

(deftest direct-oob-helper-contract-test
  (let [toolbar (views/replace-toolbar-oob
                 [:div {:id views/request-toolbar-dom-id} "toolbar"])
        request-list (views/replace-request-list-oob
                      [:div {:id views/request-list-dom-id} "list"])
        dialog (views/replace-dialog-oob
                [:div {:id views/create-request-dialog-id} "dialog"])]
    (is (= views/request-toolbar-dom-id (:id (attrs toolbar))))
    (is (= views/request-list-dom-id (:id (attrs request-list))))
    (is (= views/create-request-dialog-id (:id (attrs dialog))))

    (is (= "outerHTML" (:hx-swap-oob (attrs toolbar))))
    (is (= "outerHTML" (:hx-swap-oob (attrs request-list))))
    (is (= "outerHTML" (:hx-swap-oob (attrs dialog))))))

(deftest board-state-oob-contract-test
  (let [node (views/replace-board-state-oob ctx view-state)]
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (= (routes/search-requests-url) (:hx-get (attrs node))))
    (is (= "garden" (:value (attrs (find-by-id node "humanhelp-search")))))
    (is (= "" (hidden-input-value node routes/selected-param)))
    (is (= 3 (hidden-input-value node routes/visible-revision-param)))))

(deftest with-board-state-oob-contract-test
  (let [payload [:div {:id "payload"} "payload"]
        node (views/with-board-state-oob ctx view-state payload)
        kids (children node)
        board-state-oob (first kids)]
    (is (= :div (first node)))
    (is (= "contents" (get-in (attrs node) [:style :display])))

    (testing "board-state OOB comes first so later OOB fragments see current state"
      (is (= views/board-state-form-id (:id (attrs board-state-oob))))
      (is (= "outerHTML" (:hx-swap-oob (attrs board-state-oob)))))

    (is (find-by-id node "payload"))
    (is (contains-text? node "payload"))))

(deftest create-validation-error-oob-contract-test
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
    (is (open-dialog? dialog))
    (is (contains-text? node "Title required."))
    (is (contains-text? node "Area required."))))

(deftest create-success-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/create-request-success
              ctx
              {:user owner
               :request open-request
               :toolbar toolbar
               :request-list request-list})]
    (testing "successful create replaces the board fragments"
      (let [toolbar-oob (oob-by-id node views/request-toolbar-dom-id)
            list-oob (oob-by-id node views/request-list-dom-id)]
        (is toolbar-oob)
        (is list-oob)
        (is (= "outerHTML" (:hx-swap-oob (attrs toolbar-oob))))
        (is (= "outerHTML" (:hx-swap-oob (attrs list-oob))))))

    (testing "successful create does not emit a separate dialog OOB"
      ;; The app-level create response closes/resets the dialog via the refreshed
      ;; toolbar fragment, which contains the closed create dialog in normal app
      ;; rendering. This view helper only owns board-fragment OOB + toast.
      (is (nil? (oob-by-id node views/create-request-dialog-id))))

    (testing "successful create includes a useful toast"
      (is (contains-text? node "Request created"))
      (is (contains-text? node "Request #1 is now on the board.")))))

(deftest lifecycle-and-reset-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        lifecycle-node (views/request-lifecycle-result
                        {:action :claim
                         :request claimed-request
                         :toolbar toolbar
                         :request-list request-list})
        reset-node (views/reset-demo-result
                    {:toolbar toolbar
                     :request-list request-list})]
    (testing "lifecycle result replaces both board fragments and toasts"
      (is (oob-by-id lifecycle-node views/request-toolbar-dom-id))
      (is (oob-by-id lifecycle-node views/request-list-dom-id))
      (is (contains-text? lifecycle-node "Claimed request #2.")))

    (testing "reset result replaces both board fragments and toasts"
      (is (oob-by-id reset-node views/request-toolbar-dom-id))
      (is (oob-by-id reset-node views/request-list-dom-id))
      (is (contains-text? reset-node "Demo reset"))
      (is (contains-text? reset-node "The Human Help request board was reset.")))))

(deftest request-action-error-oob-contract-test
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
