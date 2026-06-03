(ns gessokit.humanhelp.routes-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gessokit.humanhelp.routes :as routes])
  (:import
   [java.net URLDecoder]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn decode
  [s]
  (URLDecoder/decode (str s) "UTF-8"))

(defn split-url
  [url]
  (let [[path query] (str/split (str url) #"\?" 2)]
    {:path path
     :query query}))

(defn path-of
  [url]
  (:path (split-url url)))

(defn query-part
  [url-or-query]
  (let [s (str url-or-query)
        query (:query (split-url s))]
    (cond
      query
      query

      (str/starts-with? s "?")
      (subs s 1)

      :else
      s)))

(defn parse-query
  [url-or-query]
  (let [query (query-part url-or-query)]
    (if (str/blank? query)
      {}
      (reduce
       (fn [m pair]
         (let [[k v] (str/split pair #"=" 2)
               k' (decode k)
               v' (decode (or v ""))]
           (update m k' (fnil conj []) v')))
       {}
       (remove str/blank? (str/split query #"&"))))))

(defn query-value
  [url-or-query k]
  (first (get (parse-query url-or-query) (name k))))

(defn query-values
  [url-or-query k]
  (get (parse-query url-or-query) (name k)))

(defn has-query?
  [url]
  (some? (:query (split-url url))))

(defn no-query?
  [url]
  (not (has-query? url)))

(defn route-path-has-placeholder?
  [path]
  (boolean
   (or (str/includes? (str path) ":")
       (str/includes? (str path) "{")
       (str/includes? (str path) "}"))))

(defn request-id-from-action-url
  [url]
  (let [path (path-of url)
        prefix "/app/requests/"
        action-start (str/last-index-of path "/")]
    (decode
     (subs path
           (count prefix)
           action-start))))

(defn request-id-from-select-url
  [url]
  (let [path (path-of url)
        prefix "/app/requests/"
        suffix "/select"]
    (decode
     (subs path
           (count prefix)
           (- (count path) (count suffix))))))

;; -----------------------------------------------------------------------------
;; Constants and route fragments
;; -----------------------------------------------------------------------------

(deftest public-route-constants-test
  (testing "base path is the mounted app path"
    (is (= "/app" routes/base-path)))

  (testing "store id is stable"
    (is (= "demo-store" routes/store-id)))

  (testing "query parameter names are stable"
    (is (= "q" routes/search-param))
    (is (= "selected" routes/selected-param))
    (is (= "visible-revision" routes/visible-revision-param)))

  (testing "route fragments are relative route fragments"
    (doseq [route [routes/request-toolbar-fragment-route
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
      (is (string? route))
      (is (str/starts-with? route "/"))
      (is (not (str/starts-with? route routes/base-path))
          (str "Route fragment should not include base path: " route)))))

;; -----------------------------------------------------------------------------
;; path
;; -----------------------------------------------------------------------------

(deftest path-test
  (testing "path joins base path and relative route fragments"
    (is (= "/app/fragments/request-toolbar"
           (routes/path routes/request-toolbar-fragment-route)))
    (is (= "/app/fragments/requests"
           (routes/path routes/request-list-fragment-route)))
    (is (= "/app/fragments/create-request-dialog"
           (routes/path routes/create-request-dialog-fragment-route)))
    (is (= "/app/streams/request-toolbar"
           (routes/path routes/request-toolbar-stream-route)))
    (is (= "/app/streams/requests"
           (routes/path routes/request-list-stream-route)))
    (is (= "/app/demo/reset"
           (routes/path routes/reset-demo-route))))

  (testing "path does not double slash"
    (doseq [route [routes/request-toolbar-fragment-route
                   routes/request-list-fragment-route
                   routes/create-request-dialog-fragment-route
                   routes/request-toolbar-stream-route
                   routes/request-list-stream-route
                   routes/create-request-route
                   routes/refresh-requests-route
                   routes/search-requests-route
                   routes/reset-demo-route]]
      (is (not (str/includes? (routes/path route) "//"))
          (str "Path contains double slash: " (routes/path route))))))

;; -----------------------------------------------------------------------------
;; query-string and with-query
;; -----------------------------------------------------------------------------

(deftest query-string-empty-test
  (testing "nil and empty maps produce nil query strings"
    (is (nil? (routes/query-string nil)))
    (is (nil? (routes/query-string {}))))

  (testing "nil, blank, and empty sequential values are omitted"
    (is (nil? (routes/query-string {:q nil})))
    (is (nil? (routes/query-string {:q ""})))
    (is (nil? (routes/query-string {:q "   "})))
    (is (nil? (routes/query-string {:tag []})))
    (is (nil? (routes/query-string {:tag [nil "" "   "]}))))

  (testing "with-query turns nil query strings into a bare URL"
    (is (= "/app" (routes/with-query "/app" nil)))
    (is (= "/app" (routes/with-query "/app" {})))
    (is (= "/app" (routes/with-query "/app" {:q ""})))))

(deftest query-string-scalar-test
  (testing "scalar values are encoded and decoded correctly"
    (let [qs (routes/query-string {:q "jon rake/garden"
                                   :visible-revision 3
                                   :selected "hh-req-1"})]
      (is (str/starts-with? qs "?"))
      (is (= "jon rake/garden" (query-value qs :q)))
      (is (= "3" (query-value qs :visible-revision)))
      (is (= "hh-req-1" (query-value qs :selected)))))

  (testing "keyword and symbol values are stringified with str"
    (let [qs (routes/query-string {:status :open
                                   :thing 'hello})]
      (is (= ":open" (query-value qs :status)))
      (is (= "hello" (query-value qs :thing))))))

(deftest query-string-sequential-test
  (testing "sequential values are emitted as repeated params"
    (let [qs (routes/query-string {:tag ["garden" "paint" nil "" "  "]})]
      (is (= ["garden" "paint"] (query-values qs :tag)))))

  (testing "mixed scalar and sequential values both survive"
    (let [qs (routes/query-string {:q "garden"
                                   :tag ["a" "b"]
                                   :visible-revision 9})]
      (is (= "garden" (query-value qs :q)))
      (is (= ["a" "b"] (query-values qs :tag)))
      (is (= "9" (query-value qs :visible-revision))))))

(deftest with-query-test
  (testing "with-query appends query strings"
    (let [url (routes/with-query "/app" {:q "garden"
                                         :visible-revision 3})]
      (is (= "/app" (path-of url)))
      (is (= "garden" (query-value url :q)))
      (is (= "3" (query-value url :visible-revision)))))

  (testing "with-query leaves URL untouched when there are no params"
    (is (= "/app" (routes/with-query "/app" nil)))
    (is (= "/app" (routes/with-query "/app" {})))
    (is (= "/app" (routes/with-query "/app" {:q ""})))))

;; -----------------------------------------------------------------------------
;; View-state query
;; -----------------------------------------------------------------------------

(deftest view-state-query-test
  (testing "view-state-query maps internal keys to URL param names"
    (is (= {"q" "garden"
            "selected" "hh-req-1"
            "visible-revision" 3}
           (routes/view-state-query
            {:search "garden"
             :selected-request-id "hh-req-1"
             :visible-revision 3}))))

  (testing "nil view state destructures to nil values"
    (is (= {"q" nil
            "selected" nil
            "visible-revision" nil}
           (routes/view-state-query nil)))))

;; -----------------------------------------------------------------------------
;; Page URLs
;; -----------------------------------------------------------------------------

(def full-view-state
  {:search "garden rake"
   :selected-request-id "hh-req-1"
   :visible-revision 3})

(def blank-view-state
  {:search ""
   :selected-request-id nil
   :visible-revision nil})

(deftest page-url-test
  (testing "page-url without view state is just the base path"
    (is (= routes/base-path (routes/page-url)))
    (is (= routes/base-path (routes/page-url nil)))
    (is (= routes/base-path (routes/page-url {})))
    (is (= routes/base-path (routes/page-url blank-view-state))))

  (testing "page-url includes normalized view-state query params"
    (let [url (routes/page-url full-view-state)]
      (is (= routes/base-path (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param))))))

;; -----------------------------------------------------------------------------
;; Fragment URLs
;; -----------------------------------------------------------------------------

(deftest fragment-url-test
  (testing "toolbar fragment URL"
    (let [url (routes/request-toolbar-fragment-url full-view-state)]
      (is (= "/app/fragments/request-toolbar" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "request list fragment URL"
    (let [url (routes/request-list-fragment-url full-view-state)]
      (is (= "/app/fragments/requests" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "create request dialog fragment URL"
    (is (= "/app/fragments/create-request-dialog"
           (routes/create-request-dialog-fragment-url))))

  (testing "empty view state omits query params"
    (is (no-query? (routes/request-toolbar-fragment-url blank-view-state)))
    (is (no-query? (routes/request-list-fragment-url blank-view-state)))))

;; -----------------------------------------------------------------------------
;; Stream URLs
;; -----------------------------------------------------------------------------

(deftest stream-url-test
  (testing "toolbar stream URL"
    (let [url (routes/request-toolbar-stream-url full-view-state)]
      (is (= "/app/streams/request-toolbar" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "request list stream URL"
    (let [url (routes/request-list-stream-url full-view-state)]
      (is (= "/app/streams/requests" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "empty view state omits query params"
    (is (no-query? (routes/request-toolbar-stream-url blank-view-state)))
    (is (no-query? (routes/request-list-stream-url blank-view-state)))))

;; -----------------------------------------------------------------------------
;; Request creation and list controls
;; -----------------------------------------------------------------------------

(deftest request-control-url-test
  (testing "create URL"
    (is (= "/app/requests" (routes/create-request-url))))

  (testing "refresh URL"
    (let [url (routes/refresh-requests-url full-view-state)]
      (is (= "/app/requests/refresh" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "search URL"
    (let [url (routes/search-requests-url full-view-state)]
      (is (= "/app/requests/search" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "empty view state omits query params"
    (is (= "/app/requests/refresh"
           (routes/refresh-requests-url blank-view-state)))
    (is (= "/app/requests/search"
           (routes/search-requests-url blank-view-state)))))

;; -----------------------------------------------------------------------------
;; Request route substitution and action URLs
;; -----------------------------------------------------------------------------

(deftest request-route-substitution-test
  (testing "request-route substitutes the request-id placeholder"
    (let [path (routes/request-route routes/claim-request-route "hh-req-1")]
      (is (= "/requests/hh-req-1/claim" path))
      (is (not (route-path-has-placeholder? path)))))

  (testing "request-route encodes unsafe path segment characters"
    (let [request-id "id with spaces/slash?and=query"
          path (routes/request-route routes/claim-request-route request-id)
          encoded-id (second (re-find #"^/requests/(.*)/claim$" path))]
      (is (str/starts-with? path "/requests/"))
      (is (str/ends-with? path "/claim"))
      (is (= request-id (decode encoded-id))))))

(deftest lifecycle-action-url-test
  (testing "individual lifecycle URL builders use the mounted /app path"
    (is (= "/app/requests/hh-req-1/claim"
           (routes/claim-request-url "hh-req-1")))
    (is (= "/app/requests/hh-req-1/unclaim"
           (routes/unclaim-request-url "hh-req-1")))
    (is (= "/app/requests/hh-req-1/take-over"
           (routes/take-over-request-url "hh-req-1")))
    (is (= "/app/requests/hh-req-1/done"
           (routes/done-request-url "hh-req-1")))
    (is (= "/app/requests/hh-req-1/cancel"
           (routes/cancel-request-url "hh-req-1")))))

(deftest action-url-test
  (testing "known action URLs use the mounted /app path"
    (is (= "/app/requests/hh-req-1/claim"
           (routes/action-url "hh-req-1" :claim)))
    (is (= "/app/requests/hh-req-1/unclaim"
           (routes/action-url "hh-req-1" :unclaim)))
    (is (= "/app/requests/hh-req-1/take-over"
           (routes/action-url "hh-req-1" :take-over)))
    (is (= "/app/requests/hh-req-1/done"
           (routes/action-url "hh-req-1" :done)))
    (is (= "/app/requests/hh-req-1/cancel"
           (routes/action-url "hh-req-1" :cancel))))

  (testing "known action URLs encode request id"
    (let [request-id "id with spaces/slash?and=query"
          url (routes/action-url request-id :claim)]
      (is (= request-id (request-id-from-action-url url)))))

  (testing "unknown actions throw with useful data"
    (try
      (routes/action-url "hh-req-1" :explode)
      (is false "Expected action-url to throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"Unknown Human Help request action"
                     (ex-message e)))
        (is (= "hh-req-1" (:request-id (ex-data e))))
        (is (= :explode (:action (ex-data e))))))))

;; -----------------------------------------------------------------------------
;; Selection URLs
;; -----------------------------------------------------------------------------

(deftest select-request-url-test
  (testing "select-request-url points at the select route"
    (let [url (routes/select-request-url "hh-req-1" full-view-state)]
      (is (= "/app/requests/hh-req-1/select" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "hh-req-1" (query-value url routes/selected-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))))

  (testing "select-request-url encodes request id in path and query"
    (let [request-id "id with spaces/slash"
          url (routes/select-request-url request-id blank-view-state)]
      (is (= request-id (request-id-from-select-url url)))
      (is (= request-id (query-value url routes/selected-param))))))

(deftest clear-selection-url-test
  (testing "clear-selection-url removes selected-request-id but keeps search and revision"
    (let [url (routes/clear-selection-url full-view-state)]
      (is (= "/app/fragments/requests" (path-of url)))
      (is (= "garden rake" (query-value url routes/search-param)))
      (is (= "3" (query-value url routes/visible-revision-param)))
      (is (nil? (query-value url routes/selected-param)))))

  (testing "clear-selection-url with empty view state has no query"
    (is (= "/app/fragments/requests"
           (routes/clear-selection-url blank-view-state)))))

;; -----------------------------------------------------------------------------
;; Dev/demo URLs
;; -----------------------------------------------------------------------------

(deftest reset-demo-url-test
  (is (= "/app/demo/reset" (routes/reset-demo-url))))

;; -----------------------------------------------------------------------------
;; Route table compatibility expectations
;; -----------------------------------------------------------------------------

(deftest route-fragment-compatibility-test
  (testing "fragment URL builders agree with route fragments"
    (is (= (routes/path routes/request-toolbar-fragment-route)
           (path-of (routes/request-toolbar-fragment-url full-view-state))))
    (is (= (routes/path routes/request-list-fragment-route)
           (path-of (routes/request-list-fragment-url full-view-state)))))

  (testing "stream URL builders agree with route fragments"
    (is (= (routes/path routes/request-toolbar-stream-route)
           (path-of (routes/request-toolbar-stream-url full-view-state))))
    (is (= (routes/path routes/request-list-stream-route)
           (path-of (routes/request-list-stream-url full-view-state)))))

  (testing "control URL builders agree with route fragments"
    (is (= (routes/path routes/create-request-route)
           (routes/create-request-url)))
    (is (= (routes/path routes/refresh-requests-route)
           (path-of (routes/refresh-requests-url full-view-state))))
    (is (= (routes/path routes/search-requests-route)
           (path-of (routes/search-requests-url full-view-state))))
    (is (= (routes/path routes/reset-demo-route)
           (routes/reset-demo-url)))))

(deftest route-fragments-have-expected-placeholders-test
  (testing "request-specific routes contain request-id placeholders before substitution"
    (doseq [route [routes/select-request-route
                   routes/claim-request-route
                   routes/unclaim-request-route
                   routes/take-over-request-route
                   routes/done-request-route
                   routes/cancel-request-route]]
      (is (route-path-has-placeholder? route)
          (str "Expected placeholder in route: " route))))

  (testing "non-request-specific routes do not contain placeholders"
    (doseq [route [routes/request-toolbar-fragment-route
                   routes/request-list-fragment-route
                   routes/create-request-dialog-fragment-route
                   routes/request-toolbar-stream-route
                   routes/request-list-stream-route
                   routes/create-request-route
                   routes/refresh-requests-route
                   routes/search-requests-route
                   routes/reset-demo-route]]
      (is (not (route-path-has-placeholder? route))
          (str "Unexpected placeholder in route: " route)))))
