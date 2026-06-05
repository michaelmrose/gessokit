(ns gessokit.humanhelp.data
  "XTDB-backed data boundary for the Human Help analogue.

   This namespace owns Human Help request/event persistence and request
   lifecycle mutations.

   It intentionally does not know about:
   - Gesso Live
   - Hiccup/UI
   - routes
   - client plumbing

   It deliberately knows about Ring/Biff ctx now, because XTDB/Biff connection
   and node handles are carried there.

   Public data functions are ctx-first:

     (all-requests ctx)
     (request-by-id ctx request-id)
     (create-request! ctx {:user ... :input ...})
     (claim-request! ctx {:request-id ... :user ...})
     (board-data ctx view-state)

   app.clj, live.clj, and tests must be updated after this hard break."
  (:require
   [clojure.string :as str]
   [com.biffweb.experimental :as biffx]
   [gessokit.humanhelp.model :as model]
   [xtdb.api :as xt]))

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
  (str "humanhelp-store/" model/store-id))

;; -----------------------------------------------------------------------------
;; IDs
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
   tests and[object Object],[object Object],[object Object],[object Object] for reasoning about the demo store. Runtime-created requests and
   lifecycle events still use model/now-ms."
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
     :request/store-id model/store-id
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
   :event/store-id model/store-id
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
       (ex-info "Human Help data requires :biff/conn, :biff/db, :biff/node, or :xtdb/node for reads."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn tx-connectable-from-ctx
  [ctx]
  (or (:biff/node ctx)
      (:xtdb/node ctx)
      (:biff/conn ctx)
      (throw
       (ex-info "Human Help data requires :biff/node, :xtdb/node, or :biff/conn for writes."
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
   :store/id model/store-id
   :store/name model/store-name
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
      :where [:= :store/id model/store-id]}))

(defn store-meta-doc
  [ctx]
  (first (store-docs ctx)))

(defn request-docs
  [ctx]
  (q ctx
     {:select request-fields
      :from request-table
      :where [:= :request/store-id model/store-id]}))

(defn event-docs
  [ctx]
  (q ctx
     {:select event-fields
      :from event-table
      :where [:= :event/store-id model/store-id]}))

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

        existing-store-docs  (store-docs ctx)
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
  "Return visible request cards for a view-state.

   view-state keys are defined in gessokit.humanhelp.model:
     :search
     :visible-revision"
  [ctx view-state]
  (model/visible-board-requests
   (all-requests ctx)
   view-state))

(defn open-request-count
  [ctx]
  (model/open-request-count (all-requests ctx)))

(defn pending-open-request-count
  [ctx visible-revision]
  (model/pending-open-request-count
   (all-requests ctx)
   visible-revision))

(defn board-stale?
  [ctx visible-revision]
  (model/board-stale?
   visible-revision
   (latest-revision ctx)))

(defn summary
  [ctx]
  (let [requests (all-requests ctx)]
    {:store/id model/store-id
     :store/name model/store-name
     :revision (latest-revision ctx)
     :total (count requests)
     :open (model/open-request-count requests)
     :pending-open (fn [visible-revision]
                     (model/pending-open-request-count
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
                 :event/store-id model/store-id
                 :event/kind kind
                 :event/message message
                 :event/at-ms (model/now-ms)
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
        now           (model/now-ms)
        customer-name (or (:customer-name input)
                          (model/user-email user)
                          (model/user-id user))
        request       {:request/id id
                       :request/number number
                       :request/store-id model/store-id
                       :request/title (:title input)
                       :request/area (:area input)
                       :request/details (:details input)
                       :request/customer-user-id (model/user-id user)
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
  (let [email (or (model/user-email user)
                  (model/user-id user))]
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
        result   (model/transition-request
                  request
                  action
                  user
                  {:now-ms (model/now-ms)
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
    {:store/id model/store-id
     :store/name model/store-name
     :view-state view-state'
     :latest-revision latest-revision'
     :visible-revision visible-revision
     :stale? (model/board-stale? visible-revision latest-revision')
     :open-count (model/open-request-count requests)
     :pending-open-count (model/pending-open-request-count
                          requests
                          visible-revision)
     :requests (model/visible-board-requests
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
        pending-open-count' (model/pending-open-request-count
                             requests
                             visible-revision)]
    {:store/id model/store-id
     :store/name model/store-name
     :view-state view-state'
     :latest-revision latest-revision'
     :visible-revision visible-revision
     :stale? (pos? pending-open-count')
     :open-count (model/open-request-count requests)
     :pending-open-count pending-open-count'}))

#_(defn toolbar-data
  "Return the data needed to render the request toolbar."
  [ctx view-state]
  (let [view-state'      (normalize-view-state ctx view-state)
        requests         (all-requests ctx)
        latest-revision' (latest-revision ctx)
        visible-revision (:visible-revision view-state')]
    {:store/id model/store-id
     :store/name model/store-name
     :view-state view-state'
     :latest-revision latest-revision'
     :visible-revision visible-revision
     :stale? (model/board-stale? visible-revision latest-revision')
     :open-count (model/open-request-count requests)
     :pending-open-count (model/pending-open-request-count
                          requests
                          visible-revision)}))

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
