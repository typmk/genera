(ns observatory.state
  "Re-frame state management for System Observatory.

   The app state is designed for progressive disclosure:
   - Top level: system overview (processes, memory, etc.)
   - Drill path: current navigation through data
   - Selection: what's being inspected
   - Timeline: trace playback position"
  (:require [re-frame.core :as rf]
            [observatory.process :as process]
            [observatory.cache :as cache]
            [observatory.trace :as trace]))

;; =============================================================================
;; Initial State
;; =============================================================================

(def initial-db
  {:view {:current-tab :processes    ; :processes :network :cache :timeline
          :sidebar-open? true
          :inspector-open? false}

   ;; Navigation state - where we are in the data hierarchy
   :drill-path []                     ; e.g., [:processes 0 :memory-regions 2]

   ;; What's currently selected for inspection
   :selection nil                     ; {:type :process :data {...}}

   ;; System data
   :system {:processes []
            :network-connections []
            :devices []}

   ;; Hardware state
   :hardware {:cpu-topology nil
              :cache-hierarchy nil
              :ram-topology nil}

   ;; Trace data for timeline
   :trace {:events []
           :metadata nil
           :playback {:position 0     ; Current time in ns
                      :playing? false
                      :speed 1.0}}

   ;; Simulation state (when not using real data)
   :simulation {:enabled? true
                :cache-state nil}})

;; =============================================================================
;; Event Handlers
;; =============================================================================

(rf/reg-event-db
 ::initialize
 (fn [_ _]
   initial-db))

(rf/reg-event-db
 ::set-tab
 (fn [db [_ tab]]
   (assoc-in db [:view :current-tab] tab)))

(rf/reg-event-db
 ::toggle-sidebar
 (fn [db _]
   (update-in db [:view :sidebar-open?] not)))

(rf/reg-event-db
 ::toggle-inspector
 (fn [db _]
   (update-in db [:view :inspector-open?] not)))

;; --- Selection & Drill-down ---

(rf/reg-event-db
 ::select
 (fn [db [_ selection]]
   (-> db
       (assoc :selection selection)
       (assoc-in [:view :inspector-open?] true))))

(rf/reg-event-db
 ::drill-into
 (fn [db [_ path-segment]]
   (update db :drill-path conj path-segment)))

(rf/reg-event-db
 ::drill-up
 (fn [db _]
   (update db :drill-path pop)))

(rf/reg-event-db
 ::drill-reset
 (fn [db _]
   (assoc db :drill-path [])))

;; --- Data Loading ---

(rf/reg-event-db
 ::set-processes
 (fn [db [_ processes]]
   (assoc-in db [:system :processes] processes)))

(rf/reg-event-db
 ::set-cache-hierarchy
 (fn [db [_ hierarchy]]
   (-> db
       (assoc-in [:hardware :cache-hierarchy] hierarchy)
       (assoc-in [:simulation :cache-state] hierarchy))))

;; --- Sample Data (for development) ---

(rf/reg-event-db
 ::load-sample-data
 (fn [db _]
   (let [;; Generate sample processes
         processes (for [i (range 25)]
                     (process/make-process
                      (+ 1000 (* i 100))
                      (rand-nth ["chrome.exe" "firefox.exe" "code.exe"
                                 "explorer.exe" "System" "svchost.exe"
                                 "dwm.exe" "node.exe" "rust-analyzer.exe"
                                 "wsl.exe" "docker.exe" "spotify.exe"])
                      :ppid (when (> i 0) (+ 1000 (* (rand-int i) 100)))
                      :memory-bytes (rand-int (* 2 1024 1024 1024))
                      :cpu-percent (* (rand) 25.0)
                      :threads (inc (rand-int 100))))

         ;; Create cache hierarchy
         cache-hier (cache/make-cache-hierarchy
                     {:l1d {:size-kb 32 :associativity 8 :type :data}
                      :l2 {:size-kb 256 :associativity 4 :type :unified}
                      :l3 {:size-kb 8192 :associativity 16 :type :unified}})

         ;; Generate some trace events
         events (for [i (range 100)]
                  {:timestamp (* i 1000)
                   :type (rand-nth [:cache-hit :cache-hit :cache-hit :cache-miss
                                    :page-fault :disk-read])
                   :pid (+ 1000 (* (rand-int 5) 100))
                   :tid 1
                   :address (+ 0x7ff600000000 (rand-int 0x10000000))
                   :level (rand-nth [:l1 :l1 :l2 :l3])})]

     (-> db
         (assoc-in [:system :processes] (vec processes))
         (assoc-in [:hardware :cache-hierarchy] cache-hier)
         (assoc-in [:simulation :cache-state] cache-hier)
         (assoc-in [:trace :events] (vec events))
         (assoc-in [:trace :metadata] {:start-time (str (js/Date.))
                                       :event-count (count events)
                                       :duration-ns 100000})))))

;; --- Timeline Playback ---

(rf/reg-event-db
 ::set-playback-position
 (fn [db [_ position]]
   (assoc-in db [:trace :playback :position] position)))

(rf/reg-event-db
 ::toggle-playback
 (fn [db _]
   (update-in db [:trace :playback :playing?] not)))

(rf/reg-event-db
 ::set-playback-speed
 (fn [db [_ speed]]
   (assoc-in db [:trace :playback :speed] speed)))

;; --- Cache Simulation ---

(rf/reg-event-db
 ::simulate-access
 (fn [db [_ address]]
   (let [cache-state (get-in db [:simulation :cache-state])
         timestamp (get-in db [:trace :playback :position])
         {:keys [cache result]} (cache/access
                                 (get cache-state :l1d)
                                 address
                                 timestamp)]
     (-> db
         (assoc-in [:simulation :cache-state :l1d] cache)
         (update-in [:trace :events] conj
                    {:timestamp timestamp
                     :type (if (:hit? result) :cache-hit :cache-miss)
                     :address address
                     :level :l1
                     :set-index (:set-index result)
                     :way-index (:way-index result)})))))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub ::view (fn [db _] (:view db)))
(rf/reg-sub ::current-tab (fn [db _] (get-in db [:view :current-tab])))
(rf/reg-sub ::sidebar-open? (fn [db _] (get-in db [:view :sidebar-open?])))
(rf/reg-sub ::inspector-open? (fn [db _] (get-in db [:view :inspector-open?])))

(rf/reg-sub ::drill-path (fn [db _] (:drill-path db)))
(rf/reg-sub ::selection (fn [db _] (:selection db)))

(rf/reg-sub ::processes (fn [db _] (get-in db [:system :processes])))
(rf/reg-sub ::network-connections (fn [db _] (get-in db [:system :network-connections])))

(rf/reg-sub ::cache-hierarchy (fn [db _] (get-in db [:hardware :cache-hierarchy])))
(rf/reg-sub ::cache-state (fn [db _] (get-in db [:simulation :cache-state])))

(rf/reg-sub ::trace-events (fn [db _] (get-in db [:trace :events])))
(rf/reg-sub ::trace-metadata (fn [db _] (get-in db [:trace :metadata])))
(rf/reg-sub ::playback (fn [db _] (get-in db [:trace :playback])))

;; Derived subscriptions

(rf/reg-sub
 ::processes-by-cpu
 :<- [::processes]
 (fn [processes _]
   (sort-by :cpu-percent > processes)))

(rf/reg-sub
 ::processes-by-memory
 :<- [::processes]
 (fn [processes _]
   (sort-by :memory-bytes > processes)))

(rf/reg-sub
 ::trace-summary
 :<- [::trace-events]
 (fn [events _]
   (when (seq events)
     {:total (count events)
      :by-type (frequencies (map :type events))
      :hit-rate (trace/cache-hit-rate events)})))

(rf/reg-sub
 ::current-data
 :<- [::processes]
 :<- [::drill-path]
 (fn [[processes drill-path] _]
   ;; Navigate to current position in data hierarchy
   (if (empty? drill-path)
     {:type :root :data {:processes processes}}
     (get-in {:processes processes} drill-path))))
