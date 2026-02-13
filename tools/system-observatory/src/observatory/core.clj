(ns observatory.core
  "System Observatory - Main entry point.

   Ties together all subsystems:
   - Process management (taskman heritage)
   - Cache hierarchy simulation
   - Trace capture and analysis
   - Device/driver observability

   The observatory provides:
   - Live system view (when backed by real data source)
   - Trace replay (for recorded sessions)
   - Simulation (for education)
   - Analysis (pure functions on data)"
  (:require [observatory.schema :as schema]
            [observatory.cache :as cache]
            [observatory.trace :as trace]
            [observatory.process :as process]
            [observatory.device :as device]))

;; =============================================================================
;; System Snapshot
;; =============================================================================

(defn make-snapshot
  "Create a system snapshot - point-in-time state"
  [& {:keys [timestamp processes devices cpu-topology cache-state]}]
  {:timestamp (or timestamp (System/nanoTime))
   :processes (or processes [])
   :devices (or devices [])
   :cpu-topology cpu-topology
   :cache-state cache-state})

(defn snapshot-summary
  "Summarize a system snapshot"
  [{:keys [processes devices cpu-topology]}]
  {:process-count (count processes)
   :total-memory-bytes (reduce + 0 (map :memory-bytes processes))
   :total-cpu-percent (reduce + 0.0 (map :cpu-percent processes))
   :device-count (count devices)
   :cpu-cores (:logical-cores cpu-topology)})

;; =============================================================================
;; Observatory Session
;; =============================================================================

(defn make-session
  "Create an observatory session.

   A session holds:
   - System topology (static)
   - Current snapshot (dynamic)
   - Trace buffer (events)
   - Cache simulation state"
  [& {:keys [cpu-topology ram-topology disks]}]
  {:topology {:cpu cpu-topology
              :ram ram-topology
              :disks disks}
   :snapshot nil
   :trace (trace/make-trace :cpu-topology cpu-topology)
   :cache-hierarchy (when cpu-topology
                      (cache/make-cache-hierarchy
                       {:l1d {:size-kb 32 :associativity 8 :type :data}
                        :l2 {:size-kb 256 :associativity 4 :type :unified}
                        :l3 {:size-kb (get-in cpu-topology [:caches 2 :size-kb] 8192)
                             :associativity 16 :type :unified}}))
   :mode :live})  ; :live, :trace, :simulation

;; =============================================================================
;; Session Operations
;; =============================================================================

(defn update-snapshot
  "Update session with new snapshot"
  [session snapshot]
  (assoc session :snapshot snapshot))

(defn add-trace-event
  "Add event to session trace"
  [session event]
  (update session :trace trace/add-event event))

(defn add-trace-events
  "Add multiple events to session trace"
  [session events]
  (update session :trace trace/add-events events))

(defn simulate-access
  "Simulate a memory access through cache hierarchy.
   Updates cache state and adds trace event."
  [session address timestamp & [{:keys [pid tid write?]
                                 :or {pid 0 tid 0 write? false}}]]
  (let [{:keys [hierarchy results total-latency final-level]}
        (cache/hierarchy-access (:cache-hierarchy session) address timestamp
                                {:write? write?})
        event {:timestamp timestamp
               :type (if (= :ram final-level) :cache-miss :cache-hit)
               :pid pid
               :tid tid
               :address address
               :level (if (= :ram final-level) :l3 final-level)
               :latency-ns total-latency}]
    (-> session
        (assoc :cache-hierarchy hierarchy)
        (add-trace-event event))))

;; =============================================================================
;; Analysis Helpers
;; =============================================================================

(defn session-cache-stats
  "Get cache statistics from session"
  [session]
  (let [hier (:cache-hierarchy session)]
    {:l1d (get-in hier [:l1d :stats])
     :l2 (get-in hier [:l2 :stats])
     :l3 (get-in hier [:l3 :stats])}))

(defn session-trace-summary
  "Summarize trace in session"
  [session]
  (let [events (get-in session [:trace :events])]
    {:event-count (count events)
     :duration-ns (- (apply max 0 (map :timestamp events))
                     (apply min 0 (map :timestamp events)))
     :by-type (frequencies (map :type events))
     :cache-hit-rate (trace/cache-hit-rate events)
     :io (trace/io-summary events)}))

(defn session-process-ranking
  "Rank processes by various metrics"
  [session]
  (let [processes (get-in session [:snapshot :processes])]
    {:by-cpu (take 10 (process/processes-by-cpu processes))
     :by-memory (take 10 (process/processes-by-memory processes))}))

;; =============================================================================
;; Scenario Simulation
;; =============================================================================

(defn simulate-sequential-access
  "Simulate sequential memory access pattern"
  [session start-address size-bytes line-size]
  (let [addresses (range start-address (+ start-address size-bytes) line-size)]
    (reduce
     (fn [s [i addr]]
       (simulate-access s addr i {:pid 1 :tid 1}))
     session
     (map-indexed vector addresses))))

(defn simulate-random-access
  "Simulate random memory access pattern within region"
  [session start-address size-bytes num-accesses line-size]
  (let [addresses (repeatedly num-accesses
                              #(+ start-address
                                  (* line-size
                                     (rand-int (quot size-bytes line-size)))))]
    (reduce
     (fn [s [i addr]]
       (simulate-access s addr i {:pid 1 :tid 1}))
     session
     (map-indexed vector addresses))))

(defn simulate-working-set
  "Simulate access to a working set (repeated access to same addresses)"
  [session addresses iterations]
  (let [all-accesses (take (* (count addresses) iterations)
                           (cycle addresses))]
    (reduce
     (fn [s [i addr]]
       (simulate-access s addr i {:pid 1 :tid 1}))
     session
     (map-indexed vector all-accesses))))

;; =============================================================================
;; Educational Scenarios
;; =============================================================================

(def scenarios
  "Pre-built educational scenarios"
  {:cache-thrashing
   {:name "Cache Thrashing"
    :description "Access pattern that causes excessive evictions"
    :setup (fn [session]
             ;; Access addresses that all map to same cache set
             (let [l1-sets 64
                   stride (* l1-sets 64)] ; Stride that hits same set
               (simulate-working-set session
                                     (take 16 (iterate #(+ % stride) 0x1000))
                                     100)))}

   :sequential-prefetch
   {:name "Sequential Prefetch Benefit"
    :description "Sequential access benefits from hardware prefetch"
    :setup (fn [session]
             (simulate-sequential-access session 0x1000 (* 64 1024) 64))}

   :random-vs-sequential
   {:name "Random vs Sequential Comparison"
    :description "Compare random and sequential access patterns"
    :setup (fn [session]
             (let [s1 (simulate-sequential-access session 0x1000 (* 64 1024) 64)
                   s2 (simulate-random-access session 0x100000 (* 64 1024) 1000 64)]
               {:sequential (session-cache-stats s1)
                :random (session-cache-stats s2)}))}

   :working-set-size
   {:name "Working Set Size Impact"
    :description "See how working set size affects hit rate"
    :setup (fn [session]
             (for [size [16 64 256 1024 4096]]
               (let [addresses (range 0x1000 (+ 0x1000 (* size 64)) 64)
                     s (simulate-working-set session addresses 100)]
                 {:working-set-kb (quot (* size 64) 1024)
                  :hit-rate (get-in (session-trace-summary s)
                                    [:cache-hit-rate :hit-rate])})))}})

(defn run-scenario
  "Run an educational scenario"
  [scenario-key & {:keys [cpu-topology]}]
  (let [scenario (get scenarios scenario-key)
        session (make-session :cpu-topology cpu-topology)]
    (when scenario
      ((:setup scenario) session))))

;; =============================================================================
;; REPL Entry Point
;; =============================================================================

(defn start-repl-session
  "Create a session for REPL exploration"
  []
  (make-session
   :cpu-topology {:vendor "GenuineIntel"
                  :brand "12th Gen Intel Core"
                  :physical-cores 6
                  :logical-cores 12
                  :caches [{:level :l1 :type :data :size-kb 32 :associativity 8}
                           {:level :l2 :type :unified :size-kb 256 :associativity 4}
                           {:level :l3 :type :unified :size-kb 8192 :associativity 16}]}))

(comment
  ;; Start a REPL session
  (def sess (start-repl-session))

  ;; Simulate some accesses
  (def sess2 (-> sess
                 (simulate-access 0x1000 0 {:pid 1234})
                 (simulate-access 0x2000 1 {:pid 1234})
                 (simulate-access 0x1000 2 {:pid 1234})))  ; Should hit!

  ;; Check stats
  (session-cache-stats sess2)
  (session-trace-summary sess2)

  ;; Run educational scenario
  (run-scenario :working-set-size)

  ;; Simulate different patterns
  (def seq-session (simulate-sequential-access sess 0x1000 (* 64 1024) 64))
  (def rand-session (simulate-random-access sess 0x1000 (* 64 1024) 1000 64))

  (session-cache-stats seq-session)
  (session-cache-stats rand-session)
  )
