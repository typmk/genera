(ns observatory.process
  "Process-centric view of system activity.

   This ties together:
   - Process information (Task Manager view)
   - Process memory layout (address space)
   - Process I/O activity
   - Process cache behavior

   Connects the 'taskman' origin to the hardware observatory."
  (:require [observatory.schema :as schema]
            [observatory.trace :as trace]
            [observatory.cache :as cache]))

;; =============================================================================
;; Process Schema
;; =============================================================================

(def ProcessState
  [:enum :running :sleeping :stopped :zombie])

(def MemoryRegion
  "A region of process virtual address space"
  [:map
   [:start-address :int]
   [:end-address :int]
   [:size-bytes :int]
   [:protection [:enum :read :write :execute :read-write :read-execute :all]]
   [:type [:enum :code :data :heap :stack :mapped-file :shared]]
   [:mapped-file {:optional true} :string]  ; Path if memory-mapped file
   [:resident-bytes {:optional true} :int]]) ; Actually in RAM

(def Process
  [:map
   [:pid :int]
   [:ppid {:optional true} :int]           ; Parent PID
   [:name :string]
   [:exe-path {:optional true} :string]
   [:cmdline {:optional true} :string]
   [:state ProcessState]
   [:user {:optional true} :string]
   [:cpu-percent :double]
   [:memory-bytes :int]
   [:threads :int]
   [:handles {:optional true} :int]        ; Windows handle count
   [:start-time {:optional true} :string]
   [:memory-regions {:optional true} [:vector MemoryRegion]]])

;; =============================================================================
;; Process Data Structures
;; =============================================================================

(defn make-process
  "Create a process record"
  [pid name & {:keys [ppid exe-path cmdline state user
                      cpu-percent memory-bytes threads handles]
               :or {state :running
                    cpu-percent 0.0
                    memory-bytes 0
                    threads 1}}]
  {:pid pid
   :ppid ppid
   :name name
   :exe-path exe-path
   :cmdline cmdline
   :state state
   :user user
   :cpu-percent cpu-percent
   :memory-bytes memory-bytes
   :threads threads
   :handles handles
   :memory-regions []})

(defn make-memory-region
  "Create a memory region"
  [start-address size-bytes type protection]
  {:start-address start-address
   :end-address (+ start-address size-bytes)
   :size-bytes size-bytes
   :type type
   :protection protection})

;; =============================================================================
;; Process List Operations
;; =============================================================================

(defn processes-by-cpu
  "Sort processes by CPU usage descending"
  [processes]
  (sort-by :cpu-percent > processes))

(defn processes-by-memory
  "Sort processes by memory usage descending"
  [processes]
  (sort-by :memory-bytes > processes))

(defn process-tree
  "Build process tree from flat list.
   Returns map of {pid -> {:process ... :children [...]}}"
  [processes]
  (let [by-pid (into {} (map (juxt :pid identity) processes))
        children-map (group-by :ppid processes)]
    (letfn [(build-node [proc]
              {:process proc
               :children (mapv build-node (get children-map (:pid proc) []))})]
      (->> processes
           (filter #(nil? (get by-pid (:ppid %))))  ; Root processes
           (mapv build-node)))))

(defn flatten-tree
  "Flatten process tree back to list with depth info"
  [tree]
  (letfn [(flatten-node [{:keys [process children]} depth]
            (cons (assoc process :depth depth)
                  (mapcat #(flatten-node % (inc depth)) children)))]
    (mapcat #(flatten-node % 0) tree)))

;; =============================================================================
;; Process Memory Analysis
;; =============================================================================

(defn memory-by-type
  "Summarize memory regions by type"
  [process]
  (->> (:memory-regions process)
       (group-by :type)
       (map (fn [[type regions]]
              [type {:count (count regions)
                     :total-bytes (reduce + 0 (map :size-bytes regions))}]))
       (into {})))

(defn address-to-region
  "Find which memory region contains an address"
  [process address]
  (->> (:memory-regions process)
       (filter #(and (>= address (:start-address %))
                     (< address (:end-address %))))
       first))

(defn annotate-address
  "Add region info to an address"
  [process address]
  (let [region (address-to-region process address)]
    {:address address
     :region-type (:type region)
     :region-start (:start-address region)
     :offset-in-region (when region (- address (:start-address region)))
     :mapped-file (:mapped-file region)}))

;; =============================================================================
;; Process-Trace Correlation
;; =============================================================================

(defn process-events
  "Get all trace events for a process"
  [trace pid]
  (trace/query trace (trace/by-pid pid)))

(defn process-cache-summary
  "Summarize cache behavior for a process"
  [trace pid]
  (let [events (process-events trace pid)]
    {:cache-hit-rate (trace/cache-hit-rate events)
     :cache-by-level (trace/cache-hit-rate-by-level events)
     :hot-addresses (trace/hot-addresses events)
     :page-faults (trace/page-fault-summary events)
     :io (trace/io-summary events)}))

(defn process-memory-heatmap
  "Create heatmap of memory access frequency.
   Returns map of {region-type -> [{:address :count}]}"
  [trace process]
  (let [events (process-events trace (:pid process))
        addresses (trace/hot-addresses events :top-n 100)]
    (->> addresses
         (map (fn [[addr count]]
                (let [annotation (annotate-address process addr)]
                  (assoc annotation :access-count count))))
         (group-by :region-type))))

;; =============================================================================
;; Working Set Analysis
;; =============================================================================

(defn estimate-working-set
  "Estimate process working set from memory access trace.
   Working set = unique cache lines accessed in time window."
  [trace pid window-ns line-size]
  (let [events (->> (trace/query trace
                                 (trace/by-pid pid)
                                 (trace/by-type :cache-access :cache-hit :cache-miss))
                    (sort-by :timestamp))
        line-mask (bit-not (dec line-size))]
    (->> events
         (trace/timeline-buckets window-ns)
         (map (fn [{:keys [bucket events]}]
                {:window bucket
                 :unique-lines (->> events
                                    (keep :address)
                                    (map #(bit-and % line-mask))
                                    distinct
                                    count)
                 :working-set-bytes (* line-size
                                       (->> events
                                            (keep :address)
                                            (map #(bit-and % line-mask))
                                            distinct
                                            count))})))))

;; =============================================================================
;; Cache Pressure Analysis
;; =============================================================================

(defn cache-pressure
  "Analyze cache pressure for a process.

   Returns assessment:
   - :low - working set fits in L1
   - :medium - working set fits in L2
   - :high - working set fits in L3
   - :thrashing - working set exceeds L3"
  [trace process cache-hierarchy window-ns]
  (let [working-set (estimate-working-set trace (:pid process) window-ns 64)
        avg-bytes (/ (reduce + 0 (map :working-set-bytes working-set))
                     (max 1 (count working-set)))
        l1-size (* 32 1024)    ; Typical L1
        l2-size (* 256 1024)   ; Typical L2
        l3-size (* 8 1024 1024)] ; Typical L3
    {:working-set-bytes avg-bytes
     :pressure (cond
                 (< avg-bytes l1-size) :low
                 (< avg-bytes l2-size) :medium
                 (< avg-bytes l3-size) :high
                 :else :thrashing)
     :recommendation (cond
                       (< avg-bytes l1-size)
                       "Working set fits in L1 - optimal"

                       (< avg-bytes l2-size)
                       "Working set fits in L2 - good locality"

                       (< avg-bytes l3-size)
                       "Working set fits in L3 - consider data layout optimization"

                       :else
                       "Working set exceeds L3 - memory-bound, consider reducing footprint")}))

;; =============================================================================
;; Process Comparison
;; =============================================================================

(defn compare-processes
  "Compare cache behavior of multiple processes"
  [trace pids]
  (let [summaries (map #(assoc (process-cache-summary trace %)
                               :pid %)
                       pids)]
    {:processes summaries
     :best-hit-rate (apply max-key #(get-in % [:cache-hit-rate :hit-rate] 0) summaries)
     :worst-hit-rate (apply min-key #(get-in % [:cache-hit-rate :hit-rate] 1) summaries)
     :most-io (apply max-key #(+ (get-in % [:io :read-bytes] 0)
                                 (get-in % [:io :write-bytes] 0))
                     summaries)}))

;; =============================================================================
;; REPL Exploration
;; =============================================================================

(comment
  ;; Create some processes
  (def chrome (make-process 1234 "chrome.exe"
                            :memory-bytes (* 500 1024 1024)
                            :cpu-percent 12.5
                            :threads 45))

  (def vscode (make-process 5678 "code.exe"
                            :memory-bytes (* 800 1024 1024)
                            :cpu-percent 5.2
                            :threads 32))

  ;; Add memory regions
  (def chrome-with-mem
    (assoc chrome :memory-regions
           [(make-memory-region 0x7ff600000000 (* 50 1024 1024) :code :read-execute)
            (make-memory-region 0x7ff650000000 (* 200 1024 1024) :heap :read-write)
            (make-memory-region 0x7ff700000000 (* 100 1024 1024) :mapped-file :read)]))

  ;; Analyze
  (memory-by-type chrome-with-mem)
  (annotate-address chrome-with-mem 0x7ff650001000)

  ;; With trace data
  (def test-trace
    (trace/add-events
     (trace/make-trace :target-pid 1234)
     [{:timestamp 0 :type :cache-miss :pid 1234 :tid 1 :address 0x7ff650001000 :level :l1}
      {:timestamp 10 :type :cache-hit :pid 1234 :tid 1 :address 0x7ff650001000 :level :l1}
      {:timestamp 20 :type :cache-miss :pid 1234 :tid 1 :address 0x7ff650002000 :level :l1}]))

  (process-cache-summary test-trace 1234)
  )
