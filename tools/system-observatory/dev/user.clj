(ns user
  "REPL development namespace.

   Start here:
   1. (go) - Start Portal data inspector
   2. (reset) - Reload namespaces
   3. Explore! Use tap> to send data to Portal"
  (:require [portal.api :as p]
            [observatory.core :as obs]
            [observatory.cache :as cache]
            [observatory.trace :as trace]
            [observatory.process :as process]
            [observatory.device :as device]
            [observatory.schema :as schema]
            [malli.core :as m]
            [malli.generator :as mg]))

;; =============================================================================
;; Portal Setup (Visual Data Inspector)
;; =============================================================================

(defonce portal-instance (atom nil))

(defn go
  "Start Portal data inspector"
  []
  (when @portal-instance
    (p/close @portal-instance))
  (reset! portal-instance (p/open {:launcher :vs-code}))  ; or :intellij, :emacs
  (add-tap #'p/submit)
  (println "Portal started. Use (tap> data) to inspect.")
  :ready)

(defn halt
  "Stop Portal"
  []
  (when @portal-instance
    (p/close @portal-instance)
    (reset! portal-instance nil)
    (remove-tap #'p/submit))
  :stopped)

(defn clear
  "Clear Portal history"
  []
  (p/clear))

;; =============================================================================
;; Quick Helpers
;; =============================================================================

(defn tap
  "Send to Portal with label"
  ([data] (tap> data))
  ([label data]
   (tap> {:label label :data data})))

(defn inspect
  "Inspect any data structure in Portal"
  [x]
  (tap> x)
  x)

;; =============================================================================
;; Sample Data Generators
;; =============================================================================

(defn sample-processes
  "Generate sample process list"
  [n]
  (for [i (range n)]
    (process/make-process
     (+ 1000 i)
     (rand-nth ["chrome.exe" "firefox.exe" "code.exe" "explorer.exe"
                "System" "svchost.exe" "dwm.exe" "node.exe"])
     :memory-bytes (rand-int (* 1024 1024 1024))
     :cpu-percent (rand 100.0)
     :threads (inc (rand-int 50)))))

(defn sample-trace-events
  "Generate sample trace events"
  [n & {:keys [pid] :or {pid 1234}}]
  (for [i (range n)]
    (let [event-type (rand-nth [:cache-hit :cache-miss :cache-hit :cache-hit
                                :page-fault :disk-read])]
      (merge
       {:timestamp (* i 100)
        :type event-type
        :pid pid
        :tid 1}
       (case event-type
         (:cache-hit :cache-miss)
         {:address (+ 0x7ff600000000 (rand-int 0x10000000))
          :level (rand-nth [:l1 :l1 :l2 :l3])}

         :page-fault
         {:address (+ 0x7ff600000000 (rand-int 0x10000000))
          :is-write (rand-nth [true false])
          :is-hard (rand-nth [true false false false])}

         :disk-read
         {:disk-index 0
          :offset-bytes (rand-int 1000000000)
          :size-bytes (* 4096 (inc (rand-int 16)))
          :latency-ns (+ 10000 (rand-int 100000))
          :queue-depth (inc (rand-int 32))}

         {})))))

(defn sample-devices
  "Generate sample device tree"
  []
  [(device/make-device "pci0" "PCI Root Complex" :pci :internal)
   (device/make-device "nvme0" "Samsung 970 EVO Plus 1TB" :nvme :pcie
                       :parent-id "pci0" :driver-name "stornvme")
   (device/make-device "nvme1" "WD Black SN850 2TB" :nvme :pcie
                       :parent-id "pci0" :driver-name "stornvme")
   (device/make-device "gpu0" "NVIDIA RTX 3080" :gpu :pcie
                       :parent-id "pci0" :driver-name "nvlddmkm")
   (device/make-device "eth0" "Intel I225-V" :nic :pcie
                       :parent-id "pci0" :driver-name "e2f")
   (device/make-device "usb0" "USB Root Hub" :usb-hub :pcie
                       :parent-id "pci0" :driver-name "usbhub3")
   (device/make-device "kb0" "Keyboard" :keyboard :usb
                       :parent-id "usb0" :driver-name "kbdhid")
   (device/make-device "mouse0" "Mouse" :mouse :usb
                       :parent-id "usb0" :driver-name "mouhid")])

;; =============================================================================
;; Demo Sessions
;; =============================================================================

(defn demo-cache-simulation
  "Demonstrate cache simulation"
  []
  (let [session (obs/start-repl-session)
        ;; Simulate sequential access
        seq-result (obs/simulate-sequential-access session 0x1000 (* 64 1024) 64)
        ;; Simulate random access
        rand-result (obs/simulate-random-access session 0x100000 (* 64 1024) 1000 64)]

    (tap :sequential-stats (obs/session-cache-stats seq-result))
    (tap :random-stats (obs/session-cache-stats rand-result))
    (tap :sequential-trace (obs/session-trace-summary seq-result))
    (tap :random-trace (obs/session-trace-summary rand-result))

    {:sequential (obs/session-cache-stats seq-result)
     :random (obs/session-cache-stats rand-result)}))

(defn demo-trace-analysis
  "Demonstrate trace analysis"
  []
  (let [events (sample-trace-events 1000)
        trace (trace/add-events (trace/make-trace) events)]

    (tap :trace-events (take 20 events))
    (tap :hit-rate (trace/cache-hit-rate events))
    (tap :hit-rate-by-level (trace/cache-hit-rate-by-level events))
    (tap :hot-addresses (trace/hot-addresses events))
    (tap :page-faults (trace/page-fault-summary events))
    (tap :io-summary (trace/io-summary events))

    trace))

(defn demo-process-analysis
  "Demonstrate process analysis"
  []
  (let [processes (sample-processes 50)
        tree (process/process-tree processes)]

    (tap :processes processes)
    (tap :by-cpu (take 10 (process/processes-by-cpu processes)))
    (tap :by-memory (take 10 (process/processes-by-memory processes)))
    (tap :tree tree)

    {:count (count processes)
     :top-cpu (first (process/processes-by-cpu processes))
     :top-memory (first (process/processes-by-memory processes))}))

(defn demo-device-tree
  "Demonstrate device tree"
  []
  (let [devices (sample-devices)
        tree (device/build-device-tree devices)]

    (tap :devices devices)
    (tap :device-tree tree)
    (tap :by-bus (device/devices-by-bus devices))
    (tap :by-driver (device/devices-by-driver devices))

    tree))

;; =============================================================================
;; Educational Scenarios
;; =============================================================================

(defn run-all-scenarios
  "Run all educational scenarios and compare"
  []
  (let [results (for [[k v] obs/scenarios]
                  (try
                    {:scenario k
                     :name (:name v)
                     :result (obs/run-scenario k)}
                    (catch Exception e
                      {:scenario k :error (.getMessage e)})))]
    (tap :scenarios results)
    results))

;; =============================================================================
;; Schema Exploration
;; =============================================================================

(defn generate-sample
  "Generate sample data for a schema"
  [schema-key]
  (let [schema (get schema/registry schema-key)]
    (when schema
      (mg/generate schema))))

(defn explore-schemas
  "List all available schemas"
  []
  (->> schema/registry
       keys
       (filter #(and (keyword? %)
                     (= "observatory" (namespace %))))
       sort))

;; =============================================================================
;; Startup Message
;; =============================================================================

(println "
╔══════════════════════════════════════════════════════════════════╗
║                    System Observatory REPL                        ║
╠══════════════════════════════════════════════════════════════════╣
║                                                                   ║
║  (go)                   Start Portal data inspector              ║
║  (demo-cache-simulation) Run cache simulation demo               ║
║  (demo-trace-analysis)   Run trace analysis demo                 ║
║  (demo-process-analysis) Run process analysis demo               ║
║  (demo-device-tree)      Run device tree demo                    ║
║  (run-all-scenarios)     Run educational scenarios               ║
║                                                                   ║
║  (tap> data)             Send data to Portal                     ║
║  (inspect x)             Inspect and return x                    ║
║                                                                   ║
╚══════════════════════════════════════════════════════════════════╝
")
