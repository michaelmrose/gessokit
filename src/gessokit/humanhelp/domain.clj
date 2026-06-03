(ns gessokit.humanhelp.domain
  "Pure Human Help domain rules.

   This namespace owns:
   - request statuses
   - Malli schemas
   - submitted input normalization/validation
   - search matching
   - visible revision helpers
   - request transition rules

   It intentionally does not know about:
   - Ring/Biff ctx
   - atoms or XTDB
   - Gesso Live
   - Hiccup/UI
   - routes"
  (:require
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me]))

;; -----------------------------------------------------------------------------
;; Constants
;; -----------------------------------------------------------------------------

(def store-id
  "demo-store")

(def store-name
  "Human Help")

(def statuses
  #{:open
    :claimed
    :done
    :cancelled})

(def open-statuses
  "Statuses counted as open requests.

   The plan defines open requests as requests which are neither cancelled nor
   done."
  #{:open
    :claimed})

(def terminal-statuses
  #{:done
    :cancelled})

(def lifecycle-actions
  [:claim
   :unclaim
   :take-over
   :done
   :cancel])

(def request-title-max
  160)

(def request-area-max
  80)

(def request-details-max
  1200)

(def request-customer-name-max
  80)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn present?
  [x]
  (and (some? x)
       (not (str/blank? (str x)))))

(defn trim-value
  [x]
  (when (some? x)
    (str/trim (str x))))

(defn blank->nil
  [x]
  (when (present? x)
    x))

(defn request-param
  "Read a param map by keyword or string key.

   This helper is pure and can be used by app.clj before passing normalized
   values into the rest of the domain."
  [params k]
  (or (get params k)
      (get params (name k))))

(defn parse-long-value
  [x]
  (when (present? x)
    (try
      (Long/parseLong (str x))
      (catch Exception _
        nil))))

(defn parse-visible-revision
  "Parse a visible revision token from request params.

   nil means no committed visible revision was supplied. The caller may treat
   nil as \"latest\" for initial page loads."
  [x]
  (parse-long-value x))

(defn now-ms
  []
  (System/currentTimeMillis))

(defn compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn normalize-token
  [x]
  (some-> x str str/trim str/lower-case))

(defn sentence-case
  [x]
  (let [s (name (or x ""))]
    (if (str/blank? s)
      ""
      (str (str/upper-case (subs s 0 1))
           (subs s 1)))))

(defn labelize
  [x]
  (-> (name (or x ""))
      (str/replace #"-" " ")
      sentence-case))

;; -----------------------------------------------------------------------------
;; Users
;; -----------------------------------------------------------------------------

(def user-schema
  [:map
   [:user/id string?]
   [:user/email string?]])

(defn user-id
  [user]
  (:user/id user))

(defn user-email
  [user]
  (:user/email user))

(defn same-user?
  [a b]
  (= (str a) (str b)))

;; -----------------------------------------------------------------------------
;; Request input validation
;; -----------------------------------------------------------------------------

(def create-request-input-schema
  [:map
   [:title
    [:string
     {:min 1
      :max request-title-max}]]

   [:area
    [:string
     {:min 1
      :max request-area-max}]]

   [:details
    {:optional true}
    [:maybe
     [:string
      {:max request-details-max}]]]

   [:customer-name
    {:optional true}
    [:maybe
     [:string
      {:max request-customer-name-max}]]]])

(defn parse-create-request-input
  "Normalize submitted create-request params.

   Expected fields:
     title
       The user's request/query.

     area
       The imaginary store area/location.

     details
       Optional additional detail.

     customer-name
       Optional display name. The app can fall back to user email."
  [params]
  {:title (or (trim-value (request-param params :title)) "")
   :area (or (trim-value (request-param params :area)) "")
   :details (blank->nil (trim-value (request-param params :details)))
   :customer-name (blank->nil
                   (trim-value
                    (or (request-param params :customer-name)
                        (request-param params :name))))})

(defn malli-humanized-errors
  [schema value]
  (when-let [explanation (m/explain schema value)]
    (me/humanize explanation)))

(defn first-error
  [errors k]
  (some-> (get errors k) first))

(defn create-request-errors
  "Return field-keyed validation errors for normalized request input.

   Malli is the validation source of truth, but this function supplies nicer
   demo-specific wording for the obvious cases."
  [input]
  (let [humanized (or (malli-humanized-errors create-request-input-schema input)
                     {})]
    (not-empty
     (compact-map
      {:title
       (cond
         (str/blank? (:title input))
         "A short request is required."

         (< request-title-max (count (:title input)))
         (str "Use " request-title-max " characters or fewer.")

         :else
         (first-error humanized :title))

       :area
       (cond
         (str/blank? (:area input))
         "Choose or describe an area of the store."

         (< request-area-max (count (:area input)))
         (str "Use " request-area-max " characters or fewer.")

         :else
         (first-error humanized :area))

       :details
       (cond
         (and (:details input)
              (< request-details-max (count (:details input))))
         (str "Use " request-details-max " characters or fewer.")

         :else
         (first-error humanized :details))

       :customer-name
       (cond
         (and (:customer-name input)
              (< request-customer-name-max (count (:customer-name input))))
         (str "Use " request-customer-name-max " characters or fewer.")

         :else
         (first-error humanized :customer-name))}))))

(defn valid-create-request-input?
  [input]
  (m/validate create-request-input-schema input))

;; -----------------------------------------------------------------------------
;; Request model
;; -----------------------------------------------------------------------------

(def request-schema
  [:map
   [:request/id string?]
   [:request/number int?]
   [:request/store-id string?]
   [:request/title string?]
   [:request/area string?]
   [:request/details {:optional true} [:maybe string?]]

   [:request/customer-user-id string?]
   [:request/customer-name string?]

   [:request/status [:enum :open :claimed :done :cancelled]]

   [:request/claimed-by {:optional true} [:maybe string?]]
   [:request/claimed-by-email {:optional true} [:maybe string?]]

   [:request/created-at-ms int?]
   [:request/updated-at-ms int?]

   ;; Revision at which the request first became visible data.
   [:request/created-revision int?]

   ;; Latest revision touching this request.
   [:request/updated-revision int?]])

(defn request-open?
  [request]
  (contains? open-statuses (:request/status request)))

(defn request-terminal?
  [request]
  (contains? terminal-statuses (:request/status request)))

(defn request-owner?
  [user request]
  (same-user? (user-id user)
              (:request/customer-user-id request)))

(defn request-claimed?
  [request]
  (= :claimed (:request/status request)))

(defn request-claimed-by-user?
  [user request]
  (and (request-claimed? request)
       (same-user? (user-id user)
                   (:request/claimed-by request))))

(defn request-claimed-by-other?
  [user request]
  (and (request-claimed? request)
       (present? (:request/claimed-by request))
       (not (request-claimed-by-user? user request))))

(defn request-status-label
  [request]
  (labelize (:request/status request)))

(defn open-request-count
  [requests]
  (count (filter request-open? requests)))

(defn newest-revision
  [requests]
  (reduce
   max
   0
   (map #(or (:request/updated-revision %) 0) requests)))

(defn request-visible-at-revision?
  "True when request should appear in a list committed to visible-revision.

   We intentionally use created-revision, not updated-revision. This allows
   existing visible cards to update in place while preventing brand-new requests
   from suddenly jumping into the list until the user clicks refresh."
  [visible-revision request]
  (or (nil? visible-revision)
      (<= (or (:request/created-revision request) 0)
          visible-revision)))

(defn board-stale?
  "Return true when latest-revision is newer than visible-revision.

   nil visible-revision is treated as not stale; callers generally use nil for
   initial page loads where the visible board should start at latest."
  [visible-revision latest-revision]
  (and (some? visible-revision)
       (< visible-revision latest-revision)))

(defn pending-open-request-count
  "Count open requests created after the visible revision."
  [requests visible-revision]
  (if (nil? visible-revision)
    0
    (count
     (filter
      (fn [request]
        (and (request-open? request)
             (< visible-revision
                (or (:request/created-revision request) 0))))
      requests))))

;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(defn parse-search
  "Split a search string into lowercase search terms.

   All terms must be present as substrings somewhere in the request's searchable
   fields."
  [s]
  (->> (str/split (str (or s "")) #"\s+")
       (map normalize-token)
       (remove str/blank?)
       distinct
       vec))

(defn request-search-fields
  [request]
  [(str (:request/number request))
   (:request/title request)
   (:request/area request)
   (:request/details request)
   (:request/customer-name request)
   (:request/claimed-by-email request)
   (request-status-label request)])

(defn request-search-text
  [request]
  (->> (request-search-fields request)
       (remove nil?)
       (str/join " ")
       str/lower-case))

(defn request-matches-search?
  [request search]
  (let [terms (if (sequential? search)
                search
                (parse-search search))
        haystack (request-search-text request)]
    (every? #(str/includes? haystack %) terms)))

(defn filter-requests
  "Filter requests by search and visible revision.

   view-state keys:
     :search
     :visible-revision"
  [requests {:keys [search visible-revision]}]
  (->> requests
       (filter #(request-visible-at-revision? visible-revision %))
       (filter #(request-matches-search? % search))
       vec))

(defn sort-requests-for-board
  "Sort request cards for the board.

   Open requests come first, then claimed requests, then terminal requests. Newer
   items within each status group come first."
  [requests]
  (let [rank {:open 0
              :claimed 1
              :done 2
              :cancelled 3}]
    (->> requests
         (sort-by (fn [request]
                    [(get rank (:request/status request) 99)
                     (- (or (:request/updated-at-ms request) 0))]))
         vec)))

(defn visible-board-requests
  [requests view-state]
  (sort-requests-for-board
   (filter-requests requests view-state)))

;; -----------------------------------------------------------------------------
;; Time labels
;; -----------------------------------------------------------------------------

(defn elapsed-minutes
  ([started-at-ms]
   (elapsed-minutes started-at-ms (now-ms)))
  ([started-at-ms now-ms]
   (max 0
        (quot (- now-ms started-at-ms)
              60000))))

(defn waiting-label
  ([request]
   (waiting-label request (now-ms)))
  ([request now-ms]
   (let [mins (elapsed-minutes (:request/created-at-ms request) now-ms)]
     (cond
       (< mins 1) "just now"
       (= mins 1) "1 min"
       (< mins 60) (str mins " min")
       (= (quot mins 60) 1) "1 hr"
       :else (str (quot mins 60) " hr")))))

;; -----------------------------------------------------------------------------
;; Request actions / transitions
;; -----------------------------------------------------------------------------

(defn available-actions
  "Return ordered lifecycle actions available to user for request.

   Demo rules from the plan:

   - a new/open request by yourself can be marked done or cancelled
   - a new/open request by another person can be claimed
   - a claimed request by yourself can be unclaimed, done, or cancelled
   - a claimed request by another person can be taken over
   - done/cancelled requests have no actions"
  [request user]
  (case (:request/status request)
    :open
    (if (request-owner? user request)
      [:done :cancel]
      [:claim])

    :claimed
    (if (request-claimed-by-user? user request)
      [:done :unclaim :cancel]
      [:take-over])

    :done
    []

    :cancelled
    []

    []))

(defn action-available?
  [request user action]
  (boolean
   (some #{action}
         (available-actions request user))))

(defn transition-error
  [request action user]
  (cond
    (nil? request)
    {:error/type :humanhelp/request-not-found
     :message "Request not found."}

    (not (some #{action} lifecycle-actions))
    {:error/type :humanhelp/unknown-action
     :message "Unknown request action."
     :action action
     :valid-actions lifecycle-actions}

    (request-terminal? request)
    {:error/type :humanhelp/request-closed
     :message "This request is already closed."
     :request/status (:request/status request)
     :action action}

    (not (action-available? request user action))
    {:error/type :humanhelp/action-not-allowed
     :message "That action is not available for this request."
     :request/status (:request/status request)
     :action action
     :available-actions (available-actions request user)}

    :else
    nil))

(defn claim-fields
  [user]
  {:request/status :claimed
   :request/claimed-by (user-id user)
   :request/claimed-by-email (user-email user)})

(defn clear-claim-fields
  []
  {:request/status :open
   :request/claimed-by nil
   :request/claimed-by-email nil})

(defn terminal-fields
  [status]
  {:request/status status})

(defn transition-request
  "Apply a lifecycle transition to a request.

   Returns:
     {:status :ok
      :previous ...
      :request ...}

   or:
     {:status :error
      :error ...}

   Options:
     :now-ms
     :revision"
  ([request action user]
   (transition-request request action user {}))
  ([request action user opts]
   (let [{supplied-now-ms :now-ms
          revision :revision} opts]
     (if-let [error (transition-error request action user)]
       {:status :error
        :error error
        :request request}

       (let [now-ms'   (or supplied-now-ms (now-ms))
             revision' (or revision
                           (:request/updated-revision request)
                           (:request/created-revision request)
                           0)
             patch
             (case action
               :claim
               (claim-fields user)

               :unclaim
               (clear-claim-fields)

               :take-over
               (claim-fields user)

               :done
               (terminal-fields :done)

               :cancel
               (terminal-fields :cancelled))

             request' (merge request
                             patch
                             {:request/updated-at-ms now-ms'
                              :request/updated-revision revision'})]
         {:status :ok
          :previous request
          :request request'})))))

(defn action-label
  [action]
  (case action
    :claim "Claim"
    :unclaim "Unclaim"
    :take-over "Take over"
    :done "Done"
    :cancel "Cancel"
    (labelize action)))

(defn action-result-message
  [action request]
  (case action
    :claim
    (str "Claimed request #" (:request/number request) ".")

    :unclaim
    (str "Unclaimed request #" (:request/number request) ".")

    :take-over
    (str "Took over request #" (:request/number request) ".")

    :done
    (str "Marked request #" (:request/number request) " done.")

    :cancel
    (str "Cancelled request #" (:request/number request) ".")

    (str "Updated request #" (:request/number request) ".")))
