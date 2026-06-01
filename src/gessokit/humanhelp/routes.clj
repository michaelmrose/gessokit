(ns gessokit.humanhelp.routes
  "Route constants and URL builders for the Human Help analogue.

   This namespace is intentionally small and dependency-light.

   It exists so:
   - views can generate hx-get/hx-post URLs
   - live can generate fragment and stream URLs
   - app.clj can define the matching Reitit route table

   without duplicating string literals across the feature."
  (:require
   [clojure.string :as str])
  (:import
   [java.net URLEncoder]
   [java.nio.charset StandardCharsets]))

;; -----------------------------------------------------------------------------
;; Base
;; -----------------------------------------------------------------------------

(def base-path
  "/app")

(def store-id
  "demo-store")

;; -----------------------------------------------------------------------------
;; Query/view-state parameter names
;; -----------------------------------------------------------------------------

(def search-param
  "q")

(def selected-param
  "selected")

(def visible-revision-param
  "visible-revision")

;; -----------------------------------------------------------------------------
;; Relative route fragments for Reitit nesting under base-path
;; -----------------------------------------------------------------------------

(def request-toolbar-fragment-route
  "/fragments/request-toolbar")

(def request-list-fragment-route
  "/fragments/requests")

(def create-request-dialog-fragment-route
  "/fragments/create-request-dialog")

(def request-toolbar-stream-route
  "/streams/request-toolbar")

(def request-list-stream-route
  "/streams/requests")

(def create-request-route
  "/requests")

(def refresh-requests-route
  "/requests/refresh")

(def search-requests-route
  "/requests/search")

(def select-request-route
  "/requests/:request-id/select")

(def claim-request-route
  "/requests/:request-id/claim")

(def unclaim-request-route
  "/requests/:request-id/unclaim")

(def take-over-request-route
  "/requests/:request-id/take-over")

(def done-request-route
  "/requests/:request-id/done")

(def cancel-request-route
  "/requests/:request-id/cancel")

(def reset-demo-route
  "/demo/reset")

;; -----------------------------------------------------------------------------
;; URL helpers
;; -----------------------------------------------------------------------------

(defn path
  "Return an absolute app path from a relative route fragment.

   Example:
     (path request-list-fragment-route)
     => \"/app/fragments/requests\""
  [relative-route]
  (str base-path relative-route))

(defn- encode
  [x]
  (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- present?
  [x]
  (and (some? x)
       (not (str/blank? (str x)))))

(defn query-string
  "Build a URL query string from a map.

   Nil and blank values are omitted. Sequential values produce repeated keys."
  [params]
  (let [pairs
        (mapcat
         (fn [[k v]]
           (cond
             (nil? v)
             []

             (and (string? v) (str/blank? v))
             []

             (sequential? v)
             (for [item v
                   :when (present? item)]
               [(name k) item])

             :else
             [[(name k) v]]))
         params)]
    (when (seq pairs)
      (str "?"
           (str/join
            "&"
            (for [[k v] pairs]
              (str (encode k) "=" (encode v))))))))

(defn with-query
  [url params]
  (str url (or (query-string params) "")))

(defn view-state-query
  "Return query params shared by request-board fragment URLs.

   Expected view-state keys:
     :search
     :selected-request-id
     :visible-revision"
  [{:keys [search selected-request-id visible-revision]}]
  {search-param search
   selected-param selected-request-id
   visible-revision-param visible-revision})

(defn request-route
  "Substitute request-id into a relative request route."
  [relative-route request-id]
  (str/replace relative-route
               ":request-id"
               (encode request-id)))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn page-url
  ([]
   base-path)
  ([view-state]
   (with-query base-path (view-state-query view-state))))

;; -----------------------------------------------------------------------------
;; Fragment URLs
;; -----------------------------------------------------------------------------

(defn request-toolbar-fragment-url
  ([]
   (path request-toolbar-fragment-route))
  ([view-state]
   (with-query
    (request-toolbar-fragment-url)
    (view-state-query view-state))))

(defn request-list-fragment-url
  ([]
   (path request-list-fragment-route))
  ([view-state]
   (with-query
    (request-list-fragment-url)
    (view-state-query view-state))))

(defn create-request-dialog-fragment-url
  []
  (path create-request-dialog-fragment-route))

;; -----------------------------------------------------------------------------
;; Stream URLs
;; -----------------------------------------------------------------------------

(defn request-toolbar-stream-url
  ([]
   (path request-toolbar-stream-route))
  ([view-state]
   (with-query
    (request-toolbar-stream-url)
    (view-state-query view-state))))

(defn request-list-stream-url
  ([]
   (path request-list-stream-route))
  ([view-state]
   (with-query
    (request-list-stream-url)
    (view-state-query view-state))))

;; -----------------------------------------------------------------------------
;; Request creation / list controls
;; -----------------------------------------------------------------------------

(defn create-request-url
  []
  (path create-request-route))

(defn refresh-requests-url
  ([]
   (path refresh-requests-route))
  ([view-state]
   (with-query
    (refresh-requests-url)
    (view-state-query view-state))))

(defn search-requests-url
  ([]
   (path search-requests-route))
  ([view-state]
   (with-query
    (search-requests-url)
    (view-state-query view-state))))

(defn select-request-url
  ([request-id]
   (path (request-route select-request-route request-id)))
  ([request-id view-state]
   (with-query
    (select-request-url request-id)
    (view-state-query
     (assoc view-state :selected-request-id request-id)))))

(defn clear-selection-url
  [view-state]
  (with-query
   (request-list-fragment-url)
   (view-state-query
    (assoc view-state :selected-request-id nil))))

;; -----------------------------------------------------------------------------
;; Request lifecycle action URLs
;; -----------------------------------------------------------------------------

(defn claim-request-url
  [request-id]
  (path (request-route claim-request-route request-id)))

(defn unclaim-request-url
  [request-id]
  (path (request-route unclaim-request-route request-id)))

(defn take-over-request-url
  [request-id]
  (path (request-route take-over-request-route request-id)))

(defn done-request-url
  [request-id]
  (path (request-route done-request-route request-id)))

(defn cancel-request-url
  [request-id]
  (path (request-route cancel-request-route request-id)))

(defn action-url
  [request-id action]
  (case action
    :claim
    (claim-request-url request-id)

    :unclaim
    (unclaim-request-url request-id)

    :take-over
    (take-over-request-url request-id)

    :done
    (done-request-url request-id)

    :cancel
    (cancel-request-url request-id)

    (throw
     (ex-info "Unknown Human Help request action."
              {:request-id request-id
               :action action}))))

;; -----------------------------------------------------------------------------
;; Dev/demo
;; -----------------------------------------------------------------------------

(defn reset-demo-url
  []
  (path reset-demo-route))
