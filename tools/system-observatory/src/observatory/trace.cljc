(ns observatory.trace
  "Trace processing - pure functions for analyzing system traces.

   Traces can come from:
   - ETW (Windows Event Tracing)
   - perf (Linux)
   - DTrace (macOS/Solaris)
   - Simulation
   - Mock data for testing

   This namespace is source-agnostic - just works with data."
  (:require [observatory.schema :as schema]
            [clojure.string :as str]
            #?(:cljs [cljs.reader])))

;; =============================================================================
;; Trace Construction
;; =============================================================================

(defn make-trace
  "Create a new empty trace with metadata"
  [& {:keys [target-pid cpu-topology]
      :or {target-pid nil}}]
  {:metadata {:start-time #?(:clj (str (java.time.Instant/now))
                             :cljs (str (js/Date.)))
              :duration-ns 0
              :event-count 0
              :target-pid target-pid
              :cpu-topology cpu-topology
              :ram-topology nil
              :disks []}
   :events []})

(defn add-event
  "Add an event to a trace. Returns new trace."
  [trace event]
  (-> trace
      (update :events conj event)
      (update-in [:metadata :event-count] inc)
      (update-in [:metadata :duration-ns]
                 max (:timestamp event))))

(defn add-events
  "Add multiple events to a trace"
  [trace events]
  (reduce add-event trace events))

;; =============================================================================
;; Filtering (Pure Predicates)
;; =============================================================================

(defn by-type
  "Predicate: filter by event type(s)"
  [& types]
  (let [type-set (set types)]
    (fn [event]
      (contains? type-set (:type event)))))

(defn by-pid
  "Predicate: filter by process ID"
  [pid]
  (fn [event]
    (= pid (:pid event))))

(defn by-tid
  "Predicate: filter by thread ID"
  [tid]
  (fn [event]
    (= tid (:tid event))))

(defn by-time-range
  "Predicate: filter by timestamp range [start, end)"
  [start-ns end-ns]
  (fn [event]
    (let [ts (:timestamp event)]
      (and (>= ts start-ns) (< ts end-ns)))))

(defn by-address-range
  "Predicate: filter by address range (for memory events)"
  [start-addr end-addr]
  (fn [event]
    (when-let [addr (:address event)]
      (and (>= addr start-addr) (< addr end-addr)))))

(defn by-level
  "Predicate: filter cache events by level"
  [level]
  (fn [event]
    (= level (:level event))))

;; =============================================================================
;; Queries (Composable)
;; =============================================================================

(defn query
  "Query events from trace with predicates.

   Usage:
   (query trace
          (by-type :cache-miss)
          (by-pid 1234)
          (by-time-range 0 1000000))"
  [trace & predicates]
  (let [pred (apply every-pred predicates)]
    (filter pred (:events trace))))

(defn query-first
  "Get first event matching predicates"
  [trace & predicates]
  (first (apply query trace predicates)))

(defn query-count
  "Count events matching predicates"
  [trace & predicates]
  (count (apply query trace predicates)))

;; =============================================================================
;; Aggregations
;; =============================================================================

(defn group-by-type
  "Group events by type"
  [events]
  (group-by :type events))

(defn group-by-pid
  "Group events by process ID"
  [events]
  (group-by :pid events))

(defn group-by-address
  "Group memory events by cache-line-aligned address"
  [events & {:keys [line-size] :or {line-size 64}}]
  (let [mask (bit-not (dec line-size))]
    (group-by #(bit-and (or (:address %) 0) mask) events)))

(defn histogram
  "Create histogram of event field values"
  [events field]
  (->> events
       (map field)
       (filter some?)
       frequencies
       (sort-by val >)))

(defn timeline-buckets
  "Bucket events by time intervals"
  [events bucket-ns]
  (->> events
       (group-by #(quot (:timestamp %) bucket-ns))
       (sort-by key)
       (map (fn [[bucket evts]]
              {:bucket bucket
               :start-ns (* bucket bucket-ns)
               :end-ns (* (inc bucket) bucket-ns)
               :count (count evts)
               :events evts}))))

;; =============================================================================
;; Analysis Functions
;; =============================================================================

(defn cache-hit-rate
  "Calculate cache hit rate from events"
  [events]
  (let [cache-events (filter #(#{:cache-hit :cache-miss} (:type %)) events)
        hits (count (filter #(= :cache-hit (:type %)) cache-events))
        total (count cache-events)]
    (if (zero? total)
      nil
      {:hits hits
       :misses (- total hits)
       :total total
       :hit-rate (/ (double hits) total)})))

(defn cache-hit-rate-by-level
  "Calculate hit rate per cache level"
  [events]
  (->> events
       (filter :level)
       (group-by :level)
       (map (fn [[level evts]]
              [level (cache-hit-rate evts)]))
       (into {})))

(defn hot-addresses
  "Find most frequently accessed addresses"
  [events & {:keys [top-n line-size] :or {top-n 20 line-size 64}}]
  (let [mask (bit-not (dec line-size))]
    (->> events
         (keep :address)
         (map #(bit-and % mask))
         frequencies
         (sort-by val >)
         (take top-n))))

(defn page-fault-summary
  "Summarize page fault events"
  [events]
  (let [faults (filter #(#{:page-fault :page-fault-hard} (:type %)) events)
        hard (filter #(= :page-fault-hard (:type %)) faults)
        soft (filter #(= :page-fault (:type %)) faults)]
    {:total (count faults)
     :hard-faults (count hard)
     :soft-faults (count soft)
     :hard-fault-addresses (map :address hard)
     :by-pid (histogram faults :pid)}))

(defn io-summary
  "Summarize disk I/O events"
  [events]
  (let [io-events (filter #(#{:disk-read :disk-write} (:type %)) events)
        reads (filter #(= :disk-read (:type %)) io-events)
        writes (filter #(= :disk-write (:type %)) io-events)]
    {:read-count (count reads)
     :write-count (count writes)
     :read-bytes (reduce + 0 (map :size-bytes reads))
     :write-bytes (reduce + 0 (map :size-bytes writes))
     :avg-read-latency-ns (when (seq reads)
                            (/ (reduce + 0 (map :latency-ns reads))
                               (count reads)))
     :avg-write-latency-ns (when (seq writes)
                             (/ (reduce + 0 (map :latency-ns writes))
                                (count writes)))
     :max-queue-depth (reduce max 0 (map :queue-depth io-events))}))

;; =============================================================================
;; Time Series
;; =============================================================================

(defn events-per-second
  "Calculate event rate over time"
  [events]
  (let [ns-per-sec 1000000000
        buckets (timeline-buckets events ns-per-sec)]
    (mapv (fn [{:keys [bucket count]}]
            {:second bucket :events-per-sec count})
          buckets)))

(defn rolling-hit-rate
  "Calculate rolling cache hit rate with window"
  [events window-size]
  (->> events
       (filter #(#{:cache-hit :cache-miss} (:type %)))
       (partition window-size 1)
       (map (fn [window]
              (let [hits (count (filter #(= :cache-hit (:type %)) window))
                    ts (:timestamp (last window))]
                {:timestamp ts
                 :hit-rate (/ (double hits) window-size)})))))

;; =============================================================================
;; Correlation
;; =============================================================================

(defn correlate-miss-to-fault
  "Find page faults that follow cache misses (potential causation)"
  [events & {:keys [window-ns] :or {window-ns 10000}}]
  (let [sorted (sort-by :timestamp events)]
    (for [[miss fault] (partition 2 1 sorted)
          :when (and (= :cache-miss (:type miss))
                     (= :page-fault-hard (:type fault))
                     (< (- (:timestamp fault) (:timestamp miss)) window-ns))]
      {:miss miss
       :fault fault
       :latency-ns (- (:timestamp fault) (:timestamp miss))})))

(defn correlate-io-to-fault
  "Find disk I/O following hard page faults"
  [events & {:keys [window-ns] :or {window-ns 1000000}}]
  (let [sorted (sort-by :timestamp events)]
    (for [[fault io] (partition 2 1 sorted)
          :when (and (= :page-fault-hard (:type fault))
                     (= :disk-read (:type io))
                     (< (- (:timestamp io) (:timestamp fault)) window-ns))]
      {:fault fault
       :io io
       :latency-ns (- (:timestamp io) (:timestamp fault))})))

;; =============================================================================
;; Export / Serialization
;; =============================================================================

(defn trace->edn
  "Export trace to EDN string"
  [trace]
  (pr-str trace))

(defn edn->trace
  "Import trace from EDN string"
  [edn-str]
  #?(:clj (read-string edn-str)
     :cljs (cljs.reader/read-string edn-str)))

(defn- format-hex
  "Format number as hex string"
  [n]
  #?(:clj (format "0x%x" n)
     :cljs (str "0x" (.toString n 16))))

(defn events->csv
  "Export events to CSV format"
  [events]
  (let [headers ["timestamp" "type" "pid" "tid" "address" "level" "hit" "latency"]
        rows (for [e events]
               [(str (:timestamp e))
                (name (:type e))
                (str (:pid e))
                (str (:tid e))
                (when-let [a (:address e)] (format-hex a))
                (some-> (:level e) name)
                (str (:hit? e))
                (str (:latency-ns e))])]
    (str/join "\n"
              (cons (str/join "," headers)
                    (map #(str/join "," %) rows)))))

;; =============================================================================
;; REPL Exploration
;; =============================================================================

(comment
  ;; Create a trace with some events
  (def trace
    (-> (make-trace :target-pid 1234)
        (add-events
         [{:timestamp 0 :type :cache-miss :pid 1234 :tid 1 :address 0x1000 :level :l1}
          {:timestamp 10 :type :cache-hit :pid 1234 :tid 1 :address 0x1000 :level :l1}
          {:timestamp 20 :type :cache-miss :pid 1234 :tid 1 :address 0x2000 :level :l1}
          {:timestamp 30 :type :page-fault-hard :pid 1234 :tid 1 :address 0x3000}
          {:timestamp 100 :type :disk-read :pid 1234 :tid 1 :disk-index 0
           :offset-bytes 0x3000 :size-bytes 4096 :latency-ns 5000 :queue-depth 1}])))

  ;; Query
  (query trace (by-type :cache-miss))
  (query trace (by-type :cache-hit :cache-miss) (by-time-range 0 25))

  ;; Analyze
  (cache-hit-rate (:events trace))
  (hot-addresses (:events trace))
  (page-fault-summary (:events trace))
  (io-summary (:events trace))

  ;; Export
  (println (events->csv (:events trace)))
  )
