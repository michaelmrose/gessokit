(ns gessokit.humanhelp.data
  "Atom-backed fake persistence for the Human Help analogue.

   This namespace owns mutable demo state and request lifecycle mutations.

   It intentionally does not know about:
   - Ring/Biff ctx
   - Gesso Live
   - Hiccup/UI
   - routes
   - client plumbing

   The point is to make this namespace replaceable later with XTDB-backed
   persistence while keeping the rest of the app shape mostly intact."
  (:require
   [clojure.string :as str]
   [gessokit.humanhelp.model :as model]))

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
   tests and for reasoning about the demo store. Runtime-created requests and
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

(defonce !state
  (atom (initial-state)))

;; -----------------------------------------------------------------------------
;; State helpers
;; -----------------------------------------------------------------------------

(defn state
  []
  @!state)

(defn latest-revision
  []
  (:revision @!state))

(defn next-revision
  [state]
  (inc (or (:revision state) 0)))

(defn all-requests
  []
  (->> (:requests @!state)
       vals
       vec))

(defn request-by-id
  [request-id]
  (get-in @!state [:requests request-id]))

(defn all-events
  []
  (->> (:events @!state)
       (sort-by :event/at-ms >)
       vec))

(defn recent-events
  ([] (recent-events 20))
  ([n]
   (take n (all-events))))

(defn board-requests
  "Return visible request cards for a view-state.

   view-state keys are defined in gessokit.humanhelp.model:
     :search
     :visible-revision"
  [view-state]
  (model/visible-board-requests
   (all-requests)
   view-state))

(defn open-request-count
  []
  (model/open-request-count (all-requests)))

(defn pending-open-request-count
  [visible-revision]
  (model/pending-open-request-count
   (all-requests)
   visible-revision))

(defn board-stale?
  [visible-revision]
  (model/board-stale?
   visible-revision
   (latest-revision)))

(defn summary
  []
  (let [requests (all-requests)]
    {:store/id model/store-id
     :store/name model/store-name
     :revision (latest-revision)
     :total (count requests)
     :open (model/open-request-count requests)
     :pending-open (fn [visible-revision]
                     (model/pending-open-request-count
                      requests
                      visible-revision))
     :by-status (frequencies (map :request/status requests))}))

;; -----------------------------------------------------------------------------
;; Atomic update helper
;; -----------------------------------------------------------------------------

(defn update-state!
  "Atomically update !state.

   f receives old-state and must return:

     [new-state result]

   result is returned after the compare-and-set succeeds."
  [f]
  (loop []
    (let [old @!state
          [new result] (f old)]
      (if (compare-and-set! !state old new)
        result
        (recur)))))

(defn add-event
  [state kind message data]
  (let [number (:next-event-number state)
        event  (merge
                {:event/id (event-id number)
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
     {:user ...
      :input ...}

   Returns:
     {:status :ok
      :request ...
      :revision ...}"
  [{:keys [user input] :as args}]
  (when-not user
    (throw
     (ex-info "create-request! requires :user."
              {:args args})))
  (when-not input
    (throw
     (ex-info "create-request! requires :input."
              {:args args})))
  (update-state!
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
  [args]
  (update-state!
   #(transition-request-state % args)))

(defn claim-request!
  [{:keys [request-id user] :as args}]
  (transition-request!
   (assoc args
          :request-id request-id
          :user user
          :action :claim)))

(defn unclaim-request!
  [{:keys [request-id user] :as args}]
  (transition-request!
   (assoc args
          :request-id request-id
          :user user
          :action :unclaim)))

(defn take-over-request!
  [{:keys [request-id user] :as args}]
  (transition-request!
   (assoc args
          :request-id request-id
          :user user
          :action :take-over)))

(defn mark-request-done!
  [{:keys [request-id user] :as args}]
  (transition-request!
   (assoc args
          :request-id request-id
          :user user
          :action :done)))

(defn cancel-request!
  [{:keys [request-id user] :as args}]
  (transition-request!
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
  []
  (latest-revision))

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
  "Fill default view-state values from current store state.

   If :visible-revision is nil, it is set to the latest revision. This makes
   initial page loads stable while still allowing explicit older revisions to
   represent a stale board.

   Always returns a complete shape:
     {:search ...
      :selected-request-id ...
      :visible-revision ...}"
  [view-state]
  (let [view-state' (or view-state {})]
    {:search (normalize-search (:search view-state'))
     :selected-request-id (normalize-selected-request-id
                           (:selected-request-id view-state'))
     :visible-revision (if (some? (:visible-revision view-state'))
                         (:visible-revision view-state')
                         (initial-visible-revision))}))

(defn board-data
  "Return the data needed to render the request board for view-state."
  [view-state]
  (let [view-state'      (normalize-view-state view-state)
        requests         (all-requests)
        latest-revision' (latest-revision)
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
  "Return the data needed to render the request toolbar."
  [view-state]
  (let [view-state'      (normalize-view-state view-state)
        requests         (all-requests)
        latest-revision' (latest-revision)
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
  []
  (let [new-state (initial-state)]
    (reset! !state new-state)
    {:status :ok
     :revision (:revision new-state)
     :state new-state}))
