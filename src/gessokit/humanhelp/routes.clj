(ns gessokit.humanhelp.routes
  "Route constants, route specs, route binding, and URL builders for the Human
   Help analogue.

   This namespace is intentionally dependency-light.

   It exists so:
   - views can generate hx-get/hx-post URLs
   - live can generate fragment and stream URLs
   - app.clj can bind route ids to concrete handler functions

   routes.clj owns route facts.
   app.clj owns handler functions."
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

;; Compatibility only. Prefer gessokit.humanhelp.model/store-id for domain code.
(def store-id
  "demo-store")

;; -----------------------------------------------------------------------------
;; Route param names
;; -----------------------------------------------------------------------------

(def request-id-param
  "request-id")

;; -----------------------------------------------------------------------------
;; Query/view-state parameter names
;; -----------------------------------------------------------------------------

(def search-param
  "q")

(def selected-param
  "selected")

(def visible-revision-param
  "visible-revision")

;; Later:
;; (def show-terminal-param
;;   "show-terminal")

;; -----------------------------------------------------------------------------
;; Route ids
;; -----------------------------------------------------------------------------

(def page-id
  :humanhelp/page)

(def request-toolbar-fragment-id
  :humanhelp/request-toolbar-fragment)

(def request-list-fragment-id
  :humanhelp/request-list-fragment)

(def create-request-dialog-fragment-id
  :humanhelp/create-request-dialog-fragment)

(def request-toolbar-stream-id
  :humanhelp/request-toolbar-stream)

(def request-list-stream-id
  :humanhelp/request-list-stream)

(def create-request-id
  :humanhelp/create-request)

(def refresh-requests-id
  :humanhelp/refresh-requests)

(def search-requests-id
  :humanhelp/search-requests)

(def select-request-id
  :humanhelp/select-request)

(def claim-request-id
  :humanhelp/claim-request)

(def unclaim-request-id
  :humanhelp/unclaim-request)

(def take-over-request-id
  :humanhelp/take-over-request)

(def done-request-id
  :humanhelp/done-request)

(def cancel-request-id
  :humanhelp/cancel-request)

(def reset-demo-id
  :humanhelp/reset-demo)

;; -----------------------------------------------------------------------------
;; Relative route fragments for Reitit nesting under base-path
;; -----------------------------------------------------------------------------

(def page-route
  "")

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
;; Route specs
;; -----------------------------------------------------------------------------

(def route-specs
  [{:id page-id
    :method :get
    :route page-route}

   {:id request-toolbar-fragment-id
    :method :get
    :route request-toolbar-fragment-route}

   {:id request-list-fragment-id
    :method :get
    :route request-list-fragment-route}

   {:id create-request-dialog-fragment-id
    :method :get
    :route create-request-dialog-fragment-route}

   {:id request-toolbar-stream-id
    :method :get
    :route request-toolbar-stream-route}

   {:id request-list-stream-id
    :method :get
    :route request-list-stream-route}

   {:id create-request-id
    :method :post
    :route create-request-route}

   {:id refresh-requests-id
    :method :post
    :route refresh-requests-route}

   {:id search-requests-id
    :method :get
    :route search-requests-route}

   {:id select-request-id
    :method :get
    :route select-request-route}

   {:id claim-request-id
    :method :post
    :route claim-request-route}

   {:id unclaim-request-id
    :method :post
    :route unclaim-request-route}

   {:id take-over-request-id
    :method :post
    :route take-over-request-route}

   {:id done-request-id
    :method :post
    :route done-request-route}

   {:id cancel-request-id
    :method :post
    :route cancel-request-route}

   {:id reset-demo-id
    :method :post
    :route reset-demo-route}])

(def route-spec-by-id
  (into {}
        (map (juxt :id identity))
        route-specs))

(defn route-spec
  [route-id]
  (or (get route-spec-by-id route-id)
      (throw
       (ex-info "Unknown Human Help route id."
                {:route-id route-id
                 :known-route-ids (set (keys route-spec-by-id))}))))

;; -----------------------------------------------------------------------------
;; Handler binding
;; -----------------------------------------------------------------------------

(defn- handler-for!
  [handlers {:keys [id method route] :as spec}]
  (or (get handlers id)
      (throw
       (ex-info "Missing Human Help route handler."
                {:route-id id
                 :method method
                 :route route
                 :spec spec
                 :handler-ids (set (keys handlers))}))))

(defn- route-entry
  [handlers {:keys [method route] :as spec}]
  [route {method (handler-for! handlers spec)}])

(defn route-table
  "Return a Reitit route table for Human Help.

   handlers is a map of route id -> handler function.

   Example:

     (routes/route-table
       {routes/page-id app-page
        routes/request-list-fragment-id request-list-fragment}
       {:middleware [mid/wrap-signed-in]})

   routes.clj owns route facts. app.clj owns the handler map and middleware."
  ([handlers]
   (route-table handlers nil))
  ([handlers {:keys [middleware]}]
   [[base-path
     (cond-> {}
       (seq middleware)
       (assoc :middleware middleware))

     (route-entry handlers (route-spec page-id))
     (route-entry handlers (route-spec request-toolbar-fragment-id))
     (route-entry handlers (route-spec request-list-fragment-id))
     (route-entry handlers (route-spec create-request-dialog-fragment-id))
     (route-entry handlers (route-spec request-toolbar-stream-id))
     (route-entry handlers (route-spec request-list-stream-id))
     (route-entry handlers (route-spec create-request-id))
     (route-entry handlers (route-spec refresh-requests-id))
     (route-entry handlers (route-spec search-requests-id))
     (route-entry handlers (route-spec select-request-id))
     (route-entry handlers (route-spec claim-request-id))
     (route-entry handlers (route-spec unclaim-request-id))
     (route-entry handlers (route-spec take-over-request-id))
     (route-entry handlers (route-spec done-request-id))
     (route-entry handlers (route-spec cancel-request-id))
     (route-entry handlers (route-spec reset-demo-id))]]))

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
  (URLEncoder/encode
   (str x)
   (.name StandardCharsets/UTF_8)))

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
