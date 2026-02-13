(ns observatory.device
  "Device and driver observability.

   Models:
   - Device tree (hardware topology)
   - Driver stacks
   - IRPs (I/O Request Packets)
   - DMA transfers
   - Interrupts
   - MMIO regions

   Useful for:
   - Driver development
   - Kernel debugging
   - Hardware bring-up
   - Performance analysis"
  (:require [observatory.schema :as schema]))

;; =============================================================================
;; Device Schemas
;; =============================================================================

(def DeviceType
  [:enum
   ;; Storage
   :disk :nvme :sata :scsi :usb-storage
   ;; Network
   :nic :wifi :bluetooth
   ;; Display
   :gpu :display
   ;; Input
   :keyboard :mouse :hid
   ;; Bus
   :pci :usb-hub :thunderbolt
   ;; System
   :cpu :memory-controller :pch
   ;; Other
   :audio :serial :other])

(def BusType
  [:enum :pci :pcie :usb :sata :nvme :i2c :spi :internal])

(def DeviceState
  [:enum :started :stopped :disabled :error :not-present])

(def Device
  "A hardware device in the system"
  [:map
   [:id :string]                          ; Unique identifier
   [:name :string]                        ; Human-readable name
   [:type DeviceType]
   [:bus BusType]
   [:state DeviceState]
   ;; Location
   [:bus-address {:optional true} :string] ; e.g., "0000:03:00.0" for PCI
   [:parent-id {:optional true} :string]   ; Parent device ID
   ;; Resources
   [:mmio-regions {:optional true} [:vector [:map
                                             [:start :int]
                                             [:size :int]
                                             [:name {:optional true} :string]]]]
   [:io-ports {:optional true} [:vector [:map
                                         [:start :int]
                                         [:size :int]]]]
   [:irq {:optional true} :int]
   [:dma-channels {:optional true} [:vector :int]]
   ;; Driver
   [:driver-name {:optional true} :string]
   [:driver-version {:optional true} :string]])

;; =============================================================================
;; Driver Stack Schemas
;; =============================================================================

(def DriverType
  [:enum
   :function    ; Function driver (main driver)
   :filter      ; Filter driver (intercepts IRPs)
   :bus         ; Bus driver (enumerates children)
   :miniport    ; Miniport (hardware abstraction)
   :class       ; Class driver (device class logic)])

(def Driver
  [:map
   [:name :string]
   [:type DriverType]
   [:path {:optional true} :string]        ; Driver file path
   [:version {:optional true} :string]
   [:vendor {:optional true} :string]
   [:loaded :boolean]
   [:device-count :int]])                  ; Devices using this driver

(def DriverStack
  "Stack of drivers for a device (top to bottom)"
  [:map
   [:device-id :string]
   [:drivers [:vector Driver]]])           ; Top of stack first

;; =============================================================================
;; IRP (I/O Request Packet) Schemas
;; =============================================================================

(def IrpMajorFunction
  [:enum
   :create :close :read :write
   :device-control :internal-device-control
   :flush :query-information :set-information
   :pnp :power :system-control
   :cleanup :shutdown])

(def IrpState
  [:enum :pending :processing :completed :cancelled])

(def Irp
  "An I/O Request Packet"
  [:map
   [:id :string]
   [:timestamp :int]                       ; When created
   [:major-function IrpMajorFunction]
   [:minor-function {:optional true} :int]
   [:state IrpState]
   ;; Context
   [:device-id :string]
   [:driver-name :string]
   [:pid :int]                             ; Requesting process
   [:tid :int]
   ;; Transfer info
   [:buffer-address {:optional true} :int]
   [:buffer-length {:optional true} :int]
   [:offset {:optional true} :int]         ; File/device offset
   ;; Completion
   [:completion-time {:optional true} :int]
   [:status {:optional true} :int]         ; NTSTATUS
   [:information {:optional true} :int]])  ; Bytes transferred

;; =============================================================================
;; DMA Schemas
;; =============================================================================

(def DmaDirection
  [:enum :to-device :from-device :bidirectional])

(def DmaTransfer
  "A DMA transfer operation"
  [:map
   [:id :string]
   [:timestamp :int]
   [:device-id :string]
   [:direction DmaDirection]
   [:physical-address :int]                ; Physical memory address
   [:virtual-address {:optional true} :int]
   [:length :int]
   [:completion-time {:optional true} :int]])

;; =============================================================================
;; Interrupt Schemas
;; =============================================================================

(def InterruptType
  [:enum :line :msi :msi-x])

(def Interrupt
  [:map
   [:timestamp :int]
   [:device-id :string]
   [:irq :int]
   [:type InterruptType]
   [:vector {:optional true} :int]         ; For MSI/MSI-X
   [:cpu :int]                             ; Which CPU handled it
   [:isr-duration-ns :int]                 ; Time in ISR
   [:dpc-duration-ns {:optional true} :int]]) ; Time in DPC

;; =============================================================================
;; Device Tree Operations
;; =============================================================================

(defn make-device
  "Create a device record"
  [id name type bus & {:keys [state parent-id driver-name]
                       :or {state :started}}]
  {:id id
   :name name
   :type type
   :bus bus
   :state state
   :parent-id parent-id
   :driver-name driver-name
   :mmio-regions []
   :io-ports []})

(defn build-device-tree
  "Build device tree from flat list"
  [devices]
  (let [by-id (into {} (map (juxt :id identity) devices))
        children-map (group-by :parent-id devices)]
    (letfn [(build-node [device]
              {:device device
               :children (mapv build-node (get children-map (:id device) []))})]
      (->> devices
           (filter #(nil? (:parent-id %)))
           (mapv build-node)))))

(defn devices-by-bus
  "Group devices by bus type"
  [devices]
  (group-by :bus devices))

(defn devices-by-driver
  "Group devices by driver"
  [devices]
  (group-by :driver-name devices))

(defn find-device-path
  "Find path from root to device"
  [devices device-id]
  (let [by-id (into {} (map (juxt :id identity) devices))]
    (loop [path []
           current (get by-id device-id)]
      (if (nil? current)
        (reverse path)
        (recur (conj path current)
               (get by-id (:parent-id current)))))))

;; =============================================================================
;; IRP Analysis
;; =============================================================================

(defn irp-latency
  "Calculate IRP latency (completion - creation)"
  [irp]
  (when (and (:completion-time irp) (:timestamp irp))
    (- (:completion-time irp) (:timestamp irp))))

(defn irps-by-device
  "Group IRPs by device"
  [irps]
  (group-by :device-id irps))

(defn irps-by-function
  "Group IRPs by major function"
  [irps]
  (group-by :major-function irps))

(defn pending-irps
  "Find IRPs that are still pending"
  [irps]
  (filter #(= :pending (:state %)) irps))

(defn irp-throughput
  "Calculate IRP throughput by device"
  [irps window-ns]
  (let [completed (filter #(= :completed (:state %)) irps)
        by-device (group-by :device-id completed)]
    (map (fn [[device-id device-irps]]
           (let [total-bytes (reduce + 0 (keep :information device-irps))
                 duration-ns (- (apply max (map :completion-time device-irps))
                                (apply min (map :timestamp device-irps)))]
             {:device-id device-id
              :irp-count (count device-irps)
              :total-bytes total-bytes
              :duration-ns duration-ns
              :bytes-per-sec (when (pos? duration-ns)
                               (/ (* total-bytes 1e9) duration-ns))}))
         by-device)))

(defn slow-irps
  "Find IRPs with latency above threshold"
  [irps threshold-ns]
  (->> irps
       (filter #(and (irp-latency %)
                     (> (irp-latency %) threshold-ns)))
       (sort-by irp-latency >)))

;; =============================================================================
;; DMA Analysis
;; =============================================================================

(defn dma-by-device
  "Group DMA transfers by device"
  [transfers]
  (group-by :device-id transfers))

(defn dma-bandwidth
  "Calculate DMA bandwidth per device"
  [transfers]
  (let [by-device (dma-by-device transfers)]
    (map (fn [[device-id device-transfers]]
           (let [total-bytes (reduce + 0 (map :length device-transfers))
                 duration-ns (- (apply max (map #(or (:completion-time %) (:timestamp %))
                                                device-transfers))
                                (apply min (map :timestamp device-transfers)))]
             {:device-id device-id
              :transfer-count (count device-transfers)
              :total-bytes total-bytes
              :bandwidth-mbps (when (pos? duration-ns)
                                (/ (* total-bytes 1000) duration-ns))}))
         by-device)))

(defn dma-address-ranges
  "Find physical address ranges used by DMA"
  [transfers]
  (->> transfers
       (map (fn [t] {:start (:physical-address t)
                     :end (+ (:physical-address t) (:length t))}))
       (sort-by :start)))

;; =============================================================================
;; Interrupt Analysis
;; =============================================================================

(defn interrupts-per-second
  "Calculate interrupt rate per device"
  [interrupts window-ns]
  (let [by-device (group-by :device-id interrupts)]
    (map (fn [[device-id device-ints]]
           (let [count (count device-ints)
                 duration-ns (- (apply max (map :timestamp device-ints))
                                (apply min (map :timestamp device-ints)))]
             {:device-id device-id
              :interrupt-count count
              :duration-ns duration-ns
              :rate-per-sec (when (pos? duration-ns)
                              (/ (* count 1e9) duration-ns))}))
         by-device)))

(defn interrupt-cpu-affinity
  "Analyze which CPUs handle interrupts for each device"
  [interrupts]
  (let [by-device (group-by :device-id interrupts)]
    (map (fn [[device-id device-ints]]
           {:device-id device-id
            :by-cpu (frequencies (map :cpu device-ints))
            :total-isr-ns (reduce + 0 (map :isr-duration-ns device-ints))
            :total-dpc-ns (reduce + 0 (keep :dpc-duration-ns device-ints))})
         by-device)))

(defn interrupt-storms
  "Detect interrupt storms (abnormally high rate)"
  [interrupts window-ns threshold-per-sec]
  (let [windows (partition-by #(quot (:timestamp %) window-ns) interrupts)]
    (->> windows
         (filter #(> (/ (count %) (/ window-ns 1e9)) threshold-per-sec))
         (map (fn [window]
                {:start-time (:timestamp (first window))
                 :count (count window)
                 :rate (/ (count window) (/ window-ns 1e9))
                 :devices (frequencies (map :device-id window))})))))

;; =============================================================================
;; Driver Stack Analysis
;; =============================================================================

(defn irp-path-through-stack
  "Trace IRP flow through driver stack"
  [irps driver-stack]
  ;; Group IRPs by their journey through drivers
  (let [driver-order (into {} (map-indexed (fn [i d] [(:name d) i])
                                           (:drivers driver-stack)))]
    (->> irps
         (filter #(= (:device-id %) (:device-id driver-stack)))
         (sort-by :timestamp)
         (partition-by :id)
         (map (fn [irp-instances]
                {:irp-id (:id (first irp-instances))
                 :path (mapv :driver-name irp-instances)
                 :timestamps (mapv :timestamp irp-instances)})))))

;; =============================================================================
;; REPL Exploration
;; =============================================================================

(comment
  ;; Create device hierarchy
  (def pci-root (make-device "pci0" "PCI Root" :pci :internal))
  (def nvme (make-device "nvme0" "Samsung 970 EVO" :nvme :pcie
                         :parent-id "pci0"
                         :driver-name "stornvme"))
  (def nic (make-device "eth0" "Intel I225" :nic :pcie
                        :parent-id "pci0"
                        :driver-name "e1g"))

  (def devices [pci-root nvme nic])

  ;; Build tree
  (build-device-tree devices)

  ;; Find device path
  (find-device-path devices "nvme0")

  ;; Create some IRPs
  (def irps
    [{:id "irp1" :timestamp 0 :major-function :read
      :device-id "nvme0" :driver-name "stornvme" :pid 1234 :tid 1
      :state :completed :completion-time 5000 :information 4096}
     {:id "irp2" :timestamp 1000 :major-function :write
      :device-id "nvme0" :driver-name "stornvme" :pid 1234 :tid 1
      :state :completed :completion-time 8000 :information 8192}])

  ;; Analyze
  (irp-throughput irps 10000)
  (slow-irps irps 3000)
  )
