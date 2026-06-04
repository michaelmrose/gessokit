(ns gessokit.client-plumbing-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.client :as live-client]
   [gessokit.client-plumbing :as plumbing]
   [gessokit.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def base-ctx
  {:anti-forgery-token "test-token"
   :user/id "user-1"
   :user/email "user1@example.com"
   :session {:uid "session-user"
             :email "session@example.com"}})

(def session-only-ctx
  {:session {:uid "session-user"
             :email "session@example.com"}})

(def email-only-session-ctx
  {:session {:email "session@example.com"}})

(def anonymous-ctx
  {})

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (= "text/html; charset=utf-8"
          (get-in response [:headers "content-type"]))
       (string? (:body response))))

(defn no-content-response?
  [response]
  (and (= 204 (:status response))
       (= "" (:body response))))

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

(defn route-map-for
  [route-tree route]
  (second
   (first
    (filter #(= route (first %))
            (route-pairs route-tree)))))

(defn recording-fn
  [calls return-value]
  (fn [& args]
    (swap! calls conj args)
    return-value))

;; -----------------------------------------------------------------------------
;; Constants / public shape
;; -----------------------------------------------------------------------------

(deftest constants-test
  (testing "app-scope is stable, concrete data"
    (is (some? plumbing/app-scope))
    (is (or (keyword? plumbing/app-scope)
            (vector? plumbing/app-scope)
            (string? plumbing/app-scope))))

  (testing "new-client-id returns unique non-blank strings"
    (let [a (plumbing/new-client-id)
          b (plumbing/new-client-id)]
      (is (string? a))
      (is (string? b))
      (is (not (str/blank? a)))
      (is (not (str/blank? b)))
      (is (not= a b)))))

;; -----------------------------------------------------------------------------
;; User identity helpers
;; -----------------------------------------------------------------------------

(deftest current-user-id-test
  (testing "explicit :user/id wins"
    (is (= "user-1"
           (plumbing/current-user-id base-ctx))))

  (testing "session uid is fallback"
    (is (= "session-user"
           (plumbing/current-user-id session-only-ctx))))

  (testing "session email can act as fallback identity"
    (is (= "session@example.com"
           (plumbing/current-user-id email-only-session-ctx))))

  (testing "anonymous context still produces a non-blank identity"
    (let [id (plumbing/current-user-id anonymous-ctx)]
      (is (string? id))
      (is (not (str/blank? id))))))

(deftest current-user-email-test
  (testing "explicit :user/email wins"
    (is (= "user1@example.com"
           (plumbing/current-user-email base-ctx))))

  (testing "session email is fallback"
    (is (= "session@example.com"
           (plumbing/current-user-email session-only-ctx))))

  (testing "session uid can act as fallback email-ish display value"
    (is (= "session-user"
           (plumbing/current-user-email
            {:session {:uid "session-user"}}))))

  (testing "anonymous context still produces a non-blank display value"
    (let [email (plumbing/current-user-email anonymous-ctx)]
      (is (string? email))
      (is (not (str/blank? email))))))

;; -----------------------------------------------------------------------------
;; Listener
;; -----------------------------------------------------------------------------

(deftest listener-default-test
  (testing "one-arity listener delegates to live-client/listener"
    (let [calls (atom [])]
      (with-redefs [live-client/listener
                    (recording-fn calls [:listener])]
        (is (= [:listener]
               (plumbing/listener base-ctx)))
        (is (= 1 (count @calls)))
        (let [[channel ctx] (first @calls)]
          (is (some? channel))
          (is (= base-ctx ctx)))))))

(deftest listener-explicit-client-id-test
  (testing "two-arity listener supplies stable client descriptor"
    (let [calls (atom [])
          client-id "client-123"]
      (with-redefs [live-client/listener
                    (recording-fn calls [:listener])]
        (is (= [:listener]
               (plumbing/listener base-ctx client-id)))
        (is (= 1 (count @calls)))
        (let [[channel ctx options] (first @calls)]
          (is (some? channel))
          (is (= base-ctx ctx))
          (is (= client-id (:client/id options)))
          (is (= (str "client-plumbing-listener-" client-id)
                 (:id options)))
          (is (= true
                 (get-in options [:attrs :data-client-plumbing-listener]))))))))

;; -----------------------------------------------------------------------------
;; Stream and pending handlers
;; -----------------------------------------------------------------------------

(deftest stream-test
  (testing "stream delegates to live-client/stream-response"
    (let [calls (atom [])
          response {:status 200
                    :headers {"content-type" "text/event-stream"}
                    :body ::stream}]
      (with-redefs [live-client/stream-response
                    (recording-fn calls response)]
        (is (= response
               (plumbing/stream base-ctx)))
        (is (= 1 (count @calls)))
        (let [[channel ctx] (first @calls)]
          (is (some? channel))
          (is (= base-ctx ctx)))))))

(deftest pending-with-fragment-test
  (testing "pending drains one fragment and returns an HTML response"
    (let [calls (atom [])
          fragment [:div {:id "toast"} "Toast"]]
      (with-redefs [live-client/drain-fragment!
                    (recording-fn calls fragment)]
        (let [response (plumbing/pending base-ctx)]
          (is (html-response? response))
          (is (str/includes? (:body response) "Toast"))
          (is (str/includes? (:body response) "id=\"toast\""))
          (is (= 1 (count @calls)))
          (let [[channel ctx] (first @calls)]
            (is (some? channel))
            (is (= base-ctx ctx))))))))

(deftest pending-without-fragment-test
  (testing "pending returns 204 when there is nothing to drain"
    (let [calls (atom [])]
      (with-redefs [live-client/drain-fragment!
                    (recording-fn calls nil)]
        (let [response (plumbing/pending base-ctx)]
          (is (no-content-response? response))
          (is (= 1 (count @calls))))))))

;; -----------------------------------------------------------------------------
;; Send API
;; -----------------------------------------------------------------------------

(deftest send-all-test
  (testing "send! delegates :all target to live-client/send!"
    (let [calls (atom [])
          fragment [:div "Hello"]
          result {:sent 2
                  :woke 2
                  :woke? true
                  :target :all
                  :fragment-count 1}]
      (with-redefs [live-client/send!
                    (recording-fn calls result)]
        (is (= result
               (plumbing/send! :all fragment)))
        (is (= 1 (count @calls)))
        (let [[channel request] (first @calls)]
          (is (some? channel))
          (is (= :all (:to request)))
          (is (= [fragment] (vec (:fragments request)))))))))

(deftest send-client-test
  (testing "send! delegates [:client id] target to live-client/send!"
    (let [calls (atom [])
          fragment [:div "Hello"]
          target [:client "client-1"]
          result {:sent 1
                  :woke 1
                  :woke? true
                  :target target
                  :fragment-count 1}]
      (with-redefs [live-client/send!
                    (recording-fn calls result)]
        (is (= result
               (plumbing/send! target fragment)))
        (is (= 1 (count @calls)))
        (let [[channel request] (first @calls)]
          (is (some? channel))
          (is (= target (:to request)))
          (is (= [fragment] (vec (:fragments request)))))))))

(deftest send-user-test
  (testing "send! delegates [:user id] target to live-client/send!"
    (let [calls (atom [])
          fragment [:div "Hello"]
          target [:user "user-1"]
          result {:sent 1
                  :woke 1
                  :woke? true
                  :target target
                  :fragment-count 1}]
      (with-redefs [live-client/send!
                    (recording-fn calls result)]
        (is (= result
               (plumbing/send! target fragment)))
        (is (= 1 (count @calls)))
        (let [[channel request] (first @calls)]
          (is (some? channel))
          (is (= target (:to request)))
          (is (= [fragment] (vec (:fragments request)))))))))

(deftest send-scope-test
  (testing "send! delegates [:scope scope] target to live-client/send!"
    (let [calls (atom [])
          fragment [:div "Hello"]
          scope [:demo :scope]
          target [:scope scope]
          result {:sent 3
                  :woke 3
                  :woke? true
                  :target target
                  :fragment-count 1}]
      (with-redefs [live-client/send!
                    (recording-fn calls result)]
        (is (= result
               (plumbing/send! target fragment)))
        (is (= 1 (count @calls)))
        (let [[channel request] (first @calls)]
          (is (some? channel))
          (is (= target (:to request)))
          (is (= [fragment] (vec (:fragments request)))))))))

(deftest send-unknown-target-test
  (testing "unknown targets are delegated to live-client/send! and its error is propagated"
    (let [target [:nope "x"]]
      (with-redefs [live-client/send!
                    (fn [_channel request]
                      (throw
                       (ex-info "Unsupported gesso.live client delivery target."
                                {:request request})))]
        (try
          (plumbing/send! target [:div "Hello"])
          (is false "Expected send! to throw for unknown target")
          (catch clojure.lang.ExceptionInfo e
            (is (str/includes? (ex-message e)
                               "Unsupported gesso.live client delivery target"))
            (is (= target (get-in (ex-data e) [:request :to])))))))))

(deftest send-convenience-functions-test
  (testing "send-to-client! delegates to live-client/send-to-client!"
    (let [calls (atom [])
          fragment [:div "Hello"]]
      (with-redefs [live-client/send-to-client!
                    (recording-fn calls {:sent 1})]
        (is (= {:sent 1}
               (plumbing/send-to-client! "client-1" fragment)))
        (is (= 1 (count @calls)))
        (is (= "client-1" (second (first @calls))))
        (is (= fragment (nth (first @calls) 2))))))

  (testing "send-to-user! delegates to live-client/send-to-user!"
    (let [calls (atom [])
          fragment [:div "Hello"]]
      (with-redefs [live-client/send-to-user!
                    (recording-fn calls {:sent 1})]
        (is (= {:sent 1}
               (plumbing/send-to-user! "user-1" fragment)))
        (is (= 1 (count @calls)))
        (is (= "user-1" (second (first @calls))))
        (is (= fragment (nth (first @calls) 2))))))

  (testing "send-to-scope! delegates to live-client/send-to-scope!"
    (let [calls (atom [])
          fragment [:div "Hello"]
          scope [:demo :scope]]
      (with-redefs [live-client/send-to-scope!
                    (recording-fn calls {:sent 2})]
        (is (= {:sent 2}
               (plumbing/send-to-scope! scope fragment)))
        (is (= 1 (count @calls)))
        (is (= scope (second (first @calls))))
        (is (= fragment (nth (first @calls) 2))))))

  (testing "broadcast! delegates to live-client/broadcast!"
    (let [calls (atom [])
          fragment [:div "Hello"]]
      (with-redefs [live-client/broadcast!
                    (recording-fn calls {:sent 9})]
        (is (= {:sent 9}
               (plumbing/broadcast! fragment)))
        (is (= 1 (count @calls)))
        (is (= fragment (second (first @calls))))))))

;; -----------------------------------------------------------------------------
;; Toast helpers
;; -----------------------------------------------------------------------------

(deftest send-toast-to-scope-test
  (testing "send-toast-to-scope! sends a rendered toast fragment to a scope and returns normalized toast"
    (let [calls (atom [])
          scope [:demo :scope]
          toast {:variant :info
                 :title "Hello"
                 :description "World"}
          normalized (plumbing/normalize-toast toast)]
      (with-redefs [live-client/send-to-scope!
                    (recording-fn calls {:sent 1})]
        (is (= {:sent 1
                :toast normalized}
               (plumbing/send-toast-to-scope! scope toast)))
        (is (= 1 (count @calls)))

        (let [[channel scope' fragment] (first @calls)]
          (is (some? channel))
          (is (= scope scope'))
          (is (vector? fragment)))))))

(deftest broadcast-toast-test
  (testing "broadcast-toast! broadcasts a rendered toast fragment and returns normalized toast"
    (let [calls (atom [])
          toast {:variant :info
                 :title "Hello"
                 :description "World"}
          normalized (plumbing/normalize-toast toast)]
      (with-redefs [live-client/broadcast!
                    (recording-fn calls {:sent 5})]
        (is (= {:sent 5
                :toast normalized}
               (plumbing/broadcast-toast! toast)))
        (is (= 1 (count @calls)))

        (let [[channel fragment] (first @calls)]
          (is (some? channel))
          (is (vector? fragment)))))))

;; -----------------------------------------------------------------------------
;; Introspection helpers
;; -----------------------------------------------------------------------------

(deftest connected-client-ids-test
  (let [calls (atom [])
        ids ["client-1" "client-2"]]
    (with-redefs [live-client/connected-client-ids
                  (recording-fn calls ids)]
      (is (= ids (plumbing/connected-client-ids)))
      (is (= 1 (count @calls))))))

(deftest latest-client-id-test
  (let [calls (atom [])]
    (with-redefs [live-client/latest-client-id
                  (recording-fn calls "client-2")]
      (is (= "client-2" (plumbing/latest-client-id)))
      (is (= 1 (count @calls))))))

(deftest pending-counts-test
  (let [calls (atom [])
        counts {"client-1" 2
                "client-2" 0}]
    (with-redefs [live-client/pending-counts
                  (recording-fn calls counts)]
      (is (= counts (plumbing/pending-counts)))
      (is (= 1 (count @calls))))))

(deftest state-summary-test
  (let [calls (atom [])
        summary {:connected-client-count 2
                 :pending-total 3}]
    (with-redefs [live-client/state-summary
                  (recording-fn calls summary)]
      (is (= summary (plumbing/state-summary)))
      (is (= 1 (count @calls))))))

(deftest reset-plumbing-public-surface-test
  (testing "client plumbing may expose a reset helper, but it must not require a nonexistent live-client/reset! var"
    (let [reset-var (ns-resolve 'gessokit.client-plumbing 'reset-plumbing!)]
      (when reset-var
        (is (fn? @reset-var))))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(deftest module-shape-test
  (testing "module exposes routes"
    (is (map? plumbing/module))
    (is (vector? (:routes plumbing/module)))
    (is (seq (:routes plumbing/module))))

  (testing "module is mounted under a client-plumbing path"
    (let [root-route (first (:routes plumbing/module))
          base-path (first root-route)]
      (is (string? base-path))
      (is (str/includes? base-path "client-plumbing"))))

  (testing "module uses signed-in middleware"
    (let [root-route (first (:routes plumbing/module))]
      (is (= {:middleware [mid/wrap-signed-in]}
             (second root-route))))))

(deftest module-routes-test
  (let [routes (:routes plumbing/module)
        strings (route-strings routes)]
    (testing "stream and pending routes are present"
      (is (contains? strings "/stream"))
      (is (contains? strings "/pending")))

    (testing "stream route points at stream handler"
      (is (= {:get plumbing/stream}
             (route-map-for routes "/stream"))))

    (testing "pending route points at pending handler"
      (is (= {:get plumbing/pending}
             (route-map-for routes "/pending"))))))
