(ns gessokit.humanhelp.model
  "Human Help domain, view-state, and XTDB-backed persistence boundary.

   This namespace now owns the removable Human Help demo's core model/service
   behavior:

   - request statuses
   - Malli schemas
   - submitted input normalization/validation
   - search matching
   - visible revision helpers
   - request transition rules
   - seed/reset state
   - XTDB read/write helpers
   - request/event persistence
   - board-data and toolbar-data

   It intentionally does not know about:

   - Gesso Live
   - Hiccup/UI
   - routes
   - client plumbing

   It does know about Ring/Biff ctx because XTDB/Biff connection and node handles
   are carried there."
  (:require
   [clojure.string :as str]
   [com.biffweb.experimental :as biffx]
   [malli.core :as m]
   [malli.error :as me]
   [xtdb.api :as xt]))

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
;; XTDB tables / ids
;; -----------------------------------------------------------------------------

(def request-table
  :humanhelp_requests)

(def event-table
  :humanhelp_events)

(def store-table
  :humanhelp_stores)

(def store-doc-id
  (str "humanhelp-store/" store-id))

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
;; Search / visible board requests
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

   This preserves the current pre-filter-refactor behavior: open requests first,
   then claimed requests, then terminal requests. Newer updated items within
   each status group come first.

   The upcoming visibility/filter feature will intentionally change this to
   strict newest-created-first ordering."
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

;; -----------------------------------------------------------------------------
;; Demo ids
;; -----------------------------------------------------------------------------

(defn request-id
  [n]
  (str "hh-req-" n))

(defn event-id
  [n]
  (str "hh-event-" n))

;; -----------------------------------------------------------------------------
;; Seed data
;; -----------------------------------------------------------------------------

(def seed-now-ms
  "Fixed demo clock used only for seeded/reset state.

   Keeping this fixed makes reset-demo-state! deterministic, which is useful for
   tests and for reasoning about the demo store. Runtime-created requests and
   lifecycle events still use now-ms."
  1780471110000)

(defn seeded-request
  [{:keys [number
           title
           area
           details
           customer-user-id
           customer-name
           status
           claimed-by
           claimed-by-email
           created-offset-ms
           revision]}]
  (let [created-at (- seed-now-ms (or created-offset-ms 0))
        id         (request-id number)
        revision'  (or revision number)]
    {:request/id id
     :request/number number
     :request/store-id store-id
     :request/title title
     :request/area area
     :request/details details
     :request/customer-user-id customer-user-id
     :request/customer-name customer-name
     :request/status status
     :request/claimed-by claimed-by
     :request/claimed-by-email claimed-by-email
     :request/created-at-ms created-at
     :request/updated-at-ms created-at
     :request/created-revision revision'
     :request/updated-revision revision'}))

(defn seeded-event
  [{:keys [number kind message request-id created-offset-ms revision]}]
  {:event/id (event-id number)
   :event/store-id store-id
   :event/kind kind
   :event/message message
   :event/request-id request-id
   :event/at-ms (- seed-now-ms (or created-offset-ms 0))
   :event/revision (or revision number)})

(defn initial-state
  []
  (let [r1 (seeded-request
            {:number 1
             :title "Need help finding a rake"
             :area "Garden"
             :details "Looking for a sturdy rake for bark and leaves."
             :customer-user-id "seed-user-1"
             :customer-name "Jon"
             :status :open
             :claimed-by nil
             :claimed-by-email nil
             :created-offset-ms (* 9 60000)
             :revision 1})

        r2 (seeded-request
            {:number 2
             :title "Can someone help load soil?"
             :area "Garden"
             :details "Six heavy bags near the entrance to the garden center."
             :customer-user-id "seed-user-2"
             :customer-name "Avery"
             :status :claimed
             :claimed-by "seed-helper-1"
             :claimed-by-email "helper@example.com"
             :created-offset-ms (* 17 60000)
             :revision 2})

        r3 (seeded-request
            {:number 3
             :title "Question about returns"
             :area "Customer service"
             :details "Customer needed the return window checked."
             :customer-user-id "seed-user-3"
             :customer-name "Sam"
             :status :done
             :claimed-by "seed-helper-1"
             :claimed-by-email "helper@example.com"
             :created-offset-ms (* 48 60000)
             :revision 3})]
    {:revision 3
     :next-request-number 4
     :next-event-number 4
     :requests {(:request/id r1) r1
                (:request/id r2) r2
                (:request/id r3) r3}
     :events [(seeded-event
               {:number 1
                :kind :request/created
                :message "Jon requested help finding a rake in Garden."
                :request-id (:request/id r1)
                :created-offset-ms (* 9 60000)
                :revision 1})
              (seeded-event
               {:number 2
                :kind :request/claimed
                :message "helper@example.com claimed Avery's soil request."
                :request-id (:request/id r2)
                :created-offset-ms (* 16 60000)
                :revision 2})
              (seeded-event
               {:number 3
                :kind :request/done
                :message "Sam's return question was marked done."
                :request-id (:request/id r3)
                :created-offset-ms (* 44 60000)
                :revision 3})]}))

;; -----------------------------------------------------------------------------
;; Context / XTDB helpers
;; -----------------------------------------------------------------------------

(defn queryable-from-ctx
  [ctx]
  (or (:biff/conn ctx)
      (:biff/db ctx)
      (:biff/node ctx)
      (:xtdb/node ctx)
      (throw
       (ex-info "Human Help model requires :biff/conn, :biff/db, :biff/node, or :xtdb/node for reads."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn tx-connectable-from-ctx
  [ctx]
  (or (:biff/node ctx)
      (:xtdb/node ctx)
      (:biff/conn ctx)
      (throw
       (ex-info "Human Help model requires :biff/node, :xtdb/node, or :biff/conn for writes."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn q
  [ctx query]
  (biffx/q (queryable-from-ctx ctx) query))

(defn execute-tx!
  [ctx tx-ops]
  (let [tx-ops (vec (remove nil? tx-ops))]
    (when (seq tx-ops)
      (xt/execute-tx
       (tx-connectable-from-ctx ctx)
       tx-ops))))

;; -----------------------------------------------------------------------------
;; XTDB doc conversion
;; -----------------------------------------------------------------------------

(def request-fields
  [:xt/id
   :request/id
   :request/number
   :request/store-id
   :request/title
   :request/area
   :request/details
   :request/customer-user-id
   :request/customer-name
   :request/status
   :request/claimed-by
   :request/claimed-by-email
   :request/created-at-ms
   :request/updated-at-ms
   :request/created-revision
   :request/updated-revision])

(def event-fields
  [:xt/id
   :event/id
   :event/store-id
   :event/kind
   :event/message
   :event/request-id
   :event/action
   :event/at-ms
   :event/revision])

(def store-fields
  [:xt/id
   :store/id
   :store/name
   :store/revision
   :store/next-request-number
   :store/next-event-number])

(defn request->doc
  [request]
  (assoc request :xt/id (:request/id request)))

(defn doc->request
  [doc]
  (dissoc doc :xt/id))

(defn event->doc
  [event]
  (assoc event :xt/id (:event/id event)))

(defn doc->event
  [doc]
  (dissoc doc :xt/id))

(defn state->store-doc
  [state]
  {:xt/id store-doc-id
   :store/id store-id
   :store/name store-name
   :store/revision (:revision state)
   :store/next-request-number (:next-request-number state)
   :store/next-event-number (:next-event-number state)})

(defn put-doc-ops
  [table docs]
  (mapv
   (fn [doc]
     [:put-docs table doc])
   docs))

(defn delete-doc-ops
  [table ids]
  (mapv
   (fn [id]
     [:delete-docs table id])
   ids))

(defn doc-ids
  [docs]
  (set (keep :xt/id docs)))

;; -----------------------------------------------------------------------------
;; Low-level queries
;; -----------------------------------------------------------------------------

(defn store-docs
  [ctx]
  (q ctx
     {:select store-fields
      :from store-table
      :where [:= :store/id store-id]}))

(defn store-meta-doc
  [ctx]
  (first (store-docs ctx)))

(defn request-docs
  [ctx]
  (q ctx
     {:select request-fields
      :from request-table
      :where [:= :request/store-id store-id]}))

(defn event-docs
  [ctx]
  (q ctx
     {:select event-fields
      :from event-table
      :where [:= :event/store-id store-id]}))

;; -----------------------------------------------------------------------------
;; Whole-state persistence
;; -----------------------------------------------------------------------------

(defn state-docs
  [state]
  {:store-docs [(state->store-doc state)]
   :request-docs (mapv request->doc
                       (vals (:requests state)))
   :event-docs (mapv event->doc
                     (:events state))})

(defn replace-doc-ops
  "Build ops that make the Human Help XTDB tables match state for this demo
   store id.

   This is intentionally simple and demo-oriented. It re-puts the desired docs
   and deletes old Human Help docs that no longer exist in the desired state.
   That preserves atom-store semantics such as keeping only the most recent
   events."
  [ctx state]
  (let [{desired-store-docs :store-docs
         desired-request-docs :request-docs
         desired-event-docs :event-docs} (state-docs state)

        existing-store-docs   (store-docs ctx)
        existing-request-docs (request-docs ctx)
        existing-event-docs   (event-docs ctx)

        desired-store-ids   (doc-ids desired-store-docs)
        desired-request-ids (doc-ids desired-request-docs)
        desired-event-ids   (doc-ids desired-event-docs)

        delete-store-ids   (remove desired-store-ids
                                    (doc-ids existing-store-docs))
        delete-request-ids (remove desired-request-ids
                                    (doc-ids existing-request-docs))
        delete-event-ids   (remove desired-event-ids
                                    (doc-ids existing-event-docs))]
    (concat
     (delete-doc-ops store-table delete-store-ids)
     (delete-doc-ops request-table delete-request-ids)
     (delete-doc-ops event-table delete-event-ids)
     (put-doc-ops store-table desired-store-docs)
     (put-doc-ops request-table desired-request-docs)
     (put-doc-ops event-table desired-event-docs))))

(defn persist-state!
  [ctx state]
  (execute-tx! ctx (replace-doc-ops ctx state)))

(defn seed-state!
  [ctx]
  (let [new-state (initial-state)]
    (persist-state! ctx new-state)
    {:status :ok
     :revision (:revision new-state)
     :state new-state}))

(defn ensure-seeded!
  "Seed the demo store when no Human Help store metadata document exists.

   This makes first load usable after a fresh database. reset-demo-state! still
   performs an explicit replace with the deterministic seed state."
  [ctx]
  (when-not (store-meta-doc ctx)
    (seed-state! ctx)))

;; -----------------------------------------------------------------------------
;; State helpers
;; -----------------------------------------------------------------------------

(defn state
  [ctx]
  (ensure-seeded! ctx)
  (let [meta-doc (store-meta-doc ctx)
        requests (->> (request-docs ctx)
                      (map doc->request)
                      (sort-by :request/number)
                      vec)
        events   (->> (event-docs ctx)
                      (map doc->event)
                      (sort-by :event/at-ms >)
                      vec)]
    {:revision (:store/revision meta-doc)
     :next-request-number (:store/next-request-number meta-doc)
     :next-event-number (:store/next-event-number meta-doc)
     :requests (into {}
                     (map (juxt :request/id identity))
                     requests)
     :events events}))

(defn latest-revision
  [ctx]
  (:revision (state ctx)))

(defn next-revision
  [state]
  (inc (or (:revision state) 0)))

(defn all-requests
  [ctx]
  (->> (:requests (state ctx))
       vals
       (sort-by :request/number)
       vec))

(defn request-by-id
  [ctx request-id]
  (get-in (state ctx) [:requests request-id]))

(defn all-events
  [ctx]
  (->> (:events (state ctx))
       (sort-by :event/at-ms >)
       vec))

(defn recent-events
  ([ctx]
   (recent-events ctx 20))
  ([ctx n]
   (take n (all-events ctx))))

(defn board-requests
  "Return visible request cards for a view-state."
  [ctx view-state]
  (visible-board-requests
   (all-requests ctx)
   view-state))

(defn summary
  [ctx]
  (let [requests (all-requests ctx)]
    {:store/id store-id
     :store/name store-name
     :revision (latest-revision ctx)
     :total (count requests)
     :open (open-request-count requests)
     :pending-open (fn [visible-revision]
                     (pending-open-request-count
                      requests
                      visible-revision))
     :by-status (frequencies (map :request/status requests))}))

;; -----------------------------------------------------------------------------
;; State update helpers
;; -----------------------------------------------------------------------------

(defn update-state!
  "Read current XTDB-backed state, compute a new state/result pair, persist the
   new state, then return result.

   f receives old-state and must return:

     [new-state result]

   This keeps the old atom-backed result shapes but deliberately does not yet
   implement optimistic retry/precondition logic for contended concurrent writes.
   That is acceptable for this removable demo analogue; it can be tightened
   later if the example needs to demonstrate concurrent write handling."
  [ctx f]
  (let [old-state    (state ctx)
        [new result] (f old-state)]
    (when-not (= old-state new)
      (persist-state! ctx new))
    result))

(defn add-event
  [state kind message data]
  (let [number (:next-event-number state)
        event  (merge
                {:event/id (event-id number)
                 :event/store-id store-id
                 :event/kind kind
                 :event/message message
                 :event/at-ms (now-ms)
                 :event/revision (:revision state)}
                data)]
    (-> state
        (update :next-event-number inc)
        (update :events
                (fn [events]
                  (->> (conj (vec events) event)
                       (sort-by :event/at-ms >)
                       (take 50)
                       vec))))))

(defn bump-revision
  [state]
  (update state :revision inc))

(defn assoc-request
  [state request]
  (assoc-in state [:requests (:request/id request)] request))

;; -----------------------------------------------------------------------------
;; Creation
;; -----------------------------------------------------------------------------

(defn create-request-state
  [state {:keys [user input]}]
  (let [revision      (next-revision state)
        number        (:next-request-number state)
        id            (request-id number)
        now           (now-ms)
        customer-name (or (:customer-name input)
                          (user-email user)
                          (user-id user))
        request       {:request/id id
                       :request/number number
                       :request/store-id store-id
                       :request/title (:title input)
                       :request/area (:area input)
                       :request/details (:details input)
                       :request/customer-user-id (user-id user)
                       :request/customer-name customer-name
                       :request/status :open
                       :request/claimed-by nil
                       :request/claimed-by-email nil
                       :request/created-at-ms now
                       :request/updated-at-ms now
                       :request/created-revision revision
                       :request/updated-revision revision}]
    [(-> state
         bump-revision
         (update :next-request-number inc)
         (assoc-request request)
         (add-event
          :request/created
          (str customer-name
               " requested help in "
               (:request/area request)
               ": "
               (:request/title request))
          {:event/request-id id}))
     {:status :ok
      :request request
      :revision revision}]))

(defn create-request!
  "Create a request from already-normalized and validated input.

   Args:
     ctx
     {:user ...
      :input ...}

   Returns:
     {:status :ok
      :request ...
      :revision ...}"
  [ctx {:keys [user input] :as args}]
  (when-not user
    (throw
     (ex-info "create-request! requires :user."
              {:args args})))
  (when-not input
    (throw
     (ex-info "create-request! requires :input."
              {:args args})))
  (update-state!
   ctx
   #(create-request-state % args)))

;; -----------------------------------------------------------------------------
;; Transitions
;; -----------------------------------------------------------------------------

(defn transition-event-kind
  [action]
  (case action
    :claim :request/claimed
    :unclaim :request/unclaimed
    :take-over :request/taken-over
    :done :request/done
    :cancel :request/cancelled))

(defn transition-message
  [action user request]
  (let [email (or (user-email user)
                  (user-id user))]
    (case action
      :claim
      (str email
           " claimed request #"
           (:request/number request)
           ".")

      :unclaim
      (str email
           " unclaimed request #"
           (:request/number request)
           ".")

      :take-over
      (str email
           " took over request #"
           (:request/number request)
           ".")

      :done
      (str email
           " marked request #"
           (:request/number request)
           " done.")

      :cancel
      (str email
           " cancelled request #"
           (:request/number request)
           ".")

      (str email
           " updated request #"
           (:request/number request)
           "."))))

(defn transition-request-state
  [state {:keys [request-id action user]}]
  (let [request  (get-in state [:requests request-id])
        revision (next-revision state)
        result   (transition-request
                  request
                  action
                  user
                  {:now-ms (now-ms)
                   :revision revision})]
    (if (= :ok (:status result))
      (let [request' (:request result)]
        [(-> state
             bump-revision
             (assoc-request request')
             (add-event
              (transition-event-kind action)
              (transition-message action user request')
              {:event/request-id (:request/id request')
               :event/action action}))
         (assoc result
                :revision revision)])

      [state
       (assoc result
              :request-id request-id
              :action action)])))

(defn transition-request!
  [ctx args]
  (update-state!
   ctx
   #(transition-request-state % args)))

(defn claim-request!
  [ctx {:keys [request-id user] :as args}]
  (transition-request!
   ctx
   (assoc args
          :request-id request-id
          :user user
          :action :claim)))

(defn unclaim-request!
  [ctx {:keys [request-id user] :as args}]
  (transition-request!
   ctx
   (assoc args
          :request-id request-id
          :user user
          :action :unclaim)))

(defn take-over-request!
  [ctx {:keys [request-id user] :as args}]
  (transition-request!
   ctx
   (assoc args
          :request-id request-id
          :user user
          :action :take-over)))

(defn mark-request-done!
  [ctx {:keys [request-id user] :as args}]
  (transition-request!
   ctx
   (assoc args
          :request-id request-id
          :user user
          :action :done)))

(defn cancel-request!
  [ctx {:keys [request-id user] :as args}]
  (transition-request!
   ctx
   (assoc args
          :request-id request-id
          :user user
          :action :cancel)))

;; -----------------------------------------------------------------------------
;; View-state helpers
;; -----------------------------------------------------------------------------

(defn initial-visible-revision
  "Return the revision an initial page load should consider visible.

   Initial load starts at latest, so the board does not immediately think it is
   stale."
  [ctx]
  (latest-revision ctx))

(defn normalize-search
  [search]
  (let [search' (some-> search str str/trim)]
    (if (str/blank? search')
      ""
      search')))

(defn normalize-selected-request-id
  [selected-request-id]
  (let [selected' (some-> selected-request-id str str/trim)]
    (when-not (str/blank? selected')
      selected')))

(defn normalize-view-state
  "Fill default view-state values from current persisted Human Help data.

   If :visible-revision is nil, it is set to the latest revision. This makes
   initial page loads stable while still allowing explicit older revisions to
   represent a stale board.

   Always returns a complete shape:
     {:search ...
      :selected-request-id ...
      :visible-revision ...}"
  [ctx view-state]
  (let [view-state' (or view-state {})]
    {:search (normalize-search (:search view-state'))
     :selected-request-id (normalize-selected-request-id
                           (:selected-request-id view-state'))
     :visible-revision (if (some? (:visible-revision view-state'))
                         (:visible-revision view-state')
                         (initial-visible-revision ctx))}))

(defn board-data
  "Return the data needed to render the request board for view-state."
  [ctx view-state]
  (let [view-state'      (normalize-view-state ctx view-state)
        requests         (all-requests ctx)
        latest-revision' (latest-revision ctx)
        visible-revision (:visible-revision view-state')]
    {:store/id store-id
     :store/name store-name
     :view-state view-state'
     :latest-revision latest-revision'
     :visible-revision visible-revision
     :stale? (board-stale? visible-revision latest-revision')
     :open-count (open-request-count requests)
     :pending-open-count (pending-open-request-count
                          requests
                          visible-revision)
     :requests (visible-board-requests
                requests
                view-state')}))

(defn toolbar-data
  "Return the data needed to render the request toolbar.

   Toolbar staleness means there are newly-created open requests that are not
   part of the viewer's current visible revision. Lifecycle-only changes such
   as claim/unclaim/done/cancel should update visible cards in place, but should
   not light the manual refresh affordance."
  [ctx view-state]
  (let [view-state'         (normalize-view-state ctx view-state)
        requests            (all-requests ctx)
        latest-revision'    (latest-revision ctx)
        visible-revision    (:visible-revision view-state')
        pending-open-count' (pending-open-request-count
                             requests
                             visible-revision)]
    {:store/id store-id
     :store/name store-name
     :view-state view-state'
     :latest-revision latest-revision'
     :visible-revision visible-revision
     :stale? (pos? pending-open-count')
     :open-count (open-request-count requests)
     :pending-open-count pending-open-count'}))

;; -----------------------------------------------------------------------------
;; Reset
;; -----------------------------------------------------------------------------

(defn reset-demo-state!
  [ctx]
  (let [new-state (initial-state)]
    (persist-state! ctx new-state)
    {:status :ok
     :revision (:revision new-state)
     :state new-state}))
