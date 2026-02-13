(ns observatory.schema
  "Pure data definitions for the System Observatory.

   All schemas are data - can be inspected, transformed, generated from.
   Using Malli for performance and better error messages than spec."
  (:require [malli.core :as m]
            [malli.registry :as mr]
            [malli.generator :as mg]))

;; =============================================================================
;; Primitive Types
;; =============================================================================

(def Address
  "64-bit memory address"
  [:int {:min 0 :max 0xFFFFFFFFFFFFFFFF}])

(def Byte
  [:int {:min 0 :max 255}])

(def Bytes
  "Raw byte sequence"
  [:vector Byte])

(def Timestamp
  "Nanoseconds since trace start"
  [:int {:min 0}])

(def Pid
  "Process ID"
  [:int {:min 0 :max 0xFFFFFFFF}])

(def Tid
  "Thread ID"
  [:int {:min 0 :max 0xFFFFFFFF}])

;; =============================================================================
;; CPU Topology
;; =============================================================================

(def CacheType
  [:enum :data :instruction :unified])

(def CacheLevel
  [:enum :l1 :l2 :l3])

(def CacheInfo
  "Single cache in the hierarchy"
  [:map
   [:level CacheLevel]
   [:type CacheType]
   [:size-kb :int]
   [:line-size {:default 64} :int]
   [:associativity :int]
   [:sets :int]
   [:shared-by-cores {:default 1} :int]])

(def CpuCore
  [:map
   [:index :int]
   [:physical-core :int]          ; Which physical core (for SMT)
   [:caches [:vector CacheInfo]]])  ; Caches accessible to this core

(def CpuTopology
  [:map
   [:vendor :string]
   [:brand :string]
   [:physical-cores :int]
   [:logical-cores :int]
   [:cores [:vector CpuCore]]
   [:caches [:vector CacheInfo]]])  ; Deduplicated cache list

;; =============================================================================
;; Cache State (Simulation)
;; =============================================================================

(def MesiState
  "Cache coherency state"
  [:enum :modified :exclusive :shared :invalid])

(def CacheLine
  "A single cache line (typically 64 bytes)"
  [:map
   [:tag Address]                 ; Upper bits of address
   [:data Bytes]                  ; The actual data (64 bytes typical)
   [:state MesiState]
   [:last-access Timestamp]
   [:access-count :int]])

(def CacheWay
  "One way in a set-associative cache"
  [:map
   [:index :int]
   [:line [:maybe CacheLine]]])   ; nil if empty

(def CacheSet
  "A set containing N ways"
  [:map
   [:index :int]
   [:ways [:vector CacheWay]]
   [:lru-order [:vector :int]]])  ; Way indices in LRU order (head = victim)

(def CacheState
  "Complete state of one cache level"
  [:map
   [:level CacheLevel]
   [:type CacheType]
   [:config CacheInfo]
   [:sets [:vector CacheSet]]
   [:stats [:map
            [:hits :int]
            [:misses :int]
            [:evictions :int]]]])

;; =============================================================================
;; Memory (RAM)
;; =============================================================================

(def RamBank
  [:map
   [:index :int]
   [:rows :int]
   [:columns :int]
   [:open-row [:maybe :int]]])    ; Currently activated row (row buffer)

(def RamChannel
  [:map
   [:index :int]
   [:banks [:vector RamBank]]])

(def RamTopology
  [:map
   [:total-bytes :int]
   [:channels [:vector RamChannel]]])

;; =============================================================================
;; Disk
;; =============================================================================

(def DiskType
  [:enum :hdd :ssd :nvme])

(def SsdPage
  [:map
   [:block :int]
   [:page :int]
   [:state [:enum :free :valid :invalid]]])

(def SsdDie
  [:map
   [:index :int]
   [:blocks :int]
   [:pages-per-block :int]])

(def DiskInfo
  [:map
   [:name :string]
   [:type DiskType]
   [:capacity-bytes :int]
   ;; HDD-specific
   [:platters {:optional true} :int]
   [:rpm {:optional true} :int]
   ;; SSD-specific
   [:dies {:optional true} [:vector SsdDie]]])

;; =============================================================================
;; Trace Events
;; =============================================================================

(def EventType
  [:enum
   ;; CPU events
   :instruction-sample
   :context-switch
   :branch-mispredict
   ;; Memory events
   :page-fault
   :page-fault-hard      ; Required disk I/O
   :working-set-change
   ;; Cache events (from simulation or PMC)
   :cache-access
   :cache-hit
   :cache-miss
   :cache-eviction
   ;; Disk events
   :disk-read
   :disk-write
   :disk-flush])

(def BaseEvent
  "Common fields for all events"
  [:map
   [:timestamp Timestamp]
   [:type EventType]
   [:pid Pid]
   [:tid Tid]])

(def PageFaultEvent
  [:merge BaseEvent
   [:map
    [:type [:= :page-fault]]
    [:address Address]
    [:is-write :boolean]
    [:is-hard :boolean]]])        ; Required disk I/O

(def CacheAccessEvent
  [:merge BaseEvent
   [:map
    [:type [:enum :cache-access :cache-hit :cache-miss]]
    [:address Address]
    [:level CacheLevel]
    [:set-index :int]
    [:way-index [:maybe :int]]    ; nil for miss
    [:is-write :boolean]]])

(def CacheEvictionEvent
  [:merge BaseEvent
   [:map
    [:type [:= :cache-eviction]]
    [:level CacheLevel]
    [:set-index :int]
    [:way-index :int]
    [:evicted-address Address]
    [:new-address Address]
    [:was-dirty :boolean]]])

(def DiskIoEvent
  [:merge BaseEvent
   [:map
    [:type [:enum :disk-read :disk-write :disk-flush]]
    [:disk-index :int]
    [:offset-bytes :int]
    [:size-bytes :int]
    [:latency-ns :int]
    [:queue-depth :int]]])

(def TraceEvent
  "Union of all event types"
  [:or
   PageFaultEvent
   CacheAccessEvent
   CacheEvictionEvent
   DiskIoEvent])

;; =============================================================================
;; Trace Container
;; =============================================================================

(def TraceMetadata
  [:map
   [:start-time :string]          ; ISO-8601
   [:duration-ns :int]
   [:event-count :int]
   [:target-pid [:maybe Pid]]
   [:cpu-topology CpuTopology]
   [:ram-topology [:maybe RamTopology]]
   [:disks [:vector DiskInfo]]])

(def Trace
  "A complete trace session"
  [:map
   [:metadata TraceMetadata]
   [:events [:vector TraceEvent]]])

;; =============================================================================
;; Registry - make all schemas available by keyword
;; =============================================================================

(def registry
  (merge
   (m/default-schemas)
   {:observatory/address Address
    :observatory/bytes Bytes
    :observatory/timestamp Timestamp
    :observatory/pid Pid
    :observatory/tid Tid
    :observatory/cache-type CacheType
    :observatory/cache-level CacheLevel
    :observatory/cache-info CacheInfo
    :observatory/cache-line CacheLine
    :observatory/cache-way CacheWay
    :observatory/cache-set CacheSet
    :observatory/cache-state CacheState
    :observatory/cpu-topology CpuTopology
    :observatory/ram-topology RamTopology
    :observatory/disk-info DiskInfo
    :observatory/event-type EventType
    :observatory/trace-event TraceEvent
    :observatory/trace Trace}))

(mr/set-default-registry! registry)

;; =============================================================================
;; Generators (for testing and simulation)
;; =============================================================================

(defn generate
  "Generate sample data for a schema"
  ([schema] (mg/generate schema))
  ([schema opts] (mg/generate schema opts)))

(defn sample
  "Generate multiple samples"
  ([schema] (mg/sample schema))
  ([schema n] (mg/sample schema {:size n})))

(comment
  ;; REPL exploration
  (generate CacheInfo)
  (generate CacheAccessEvent)
  (sample TraceEvent 5)

  ;; Validate data
  (m/validate CacheInfo {:level :l1
                         :type :data
                         :size-kb 32
                         :line-size 64
                         :associativity 8
                         :sets 64
                         :shared-by-cores 1})
  )
