(ns observatory.cache
  "Pure functional cache simulator.

   Models set-associative caches with LRU replacement.
   All functions are pure - take state, return new state.

   Design:
   - Cache state is a plain map
   - Access returns [new-state result]
   - Can step through access-by-access
   - Can replay any sequence of accesses"
  (:require [observatory.schema :as schema]))

;; =============================================================================
;; Address Decomposition
;; =============================================================================

(defn address-parts
  "Decompose address into tag, set index, and offset.

   For a cache with S sets and B byte lines:
   - Offset bits: log2(B) - identifies byte within line
   - Index bits: log2(S) - identifies which set
   - Tag bits: remaining high bits - identifies which line

   Example for 32KB, 8-way, 64B lines:
   - 32KB / 8 ways = 4KB per way
   - 4KB / 64B = 64 sets
   - Offset: 6 bits (64 bytes)
   - Index: 6 bits (64 sets)
   - Tag: 52 bits"
  [address {:keys [sets line-size]}]
  (let [offset-bits (int (Math/ceil (/ (Math/log line-size) (Math/log 2))))
        index-bits (int (Math/ceil (/ (Math/log sets) (Math/log 2))))
        offset-mask (dec (bit-shift-left 1 offset-bits))
        index-mask (dec (bit-shift-left 1 index-bits))]
    {:offset (bit-and address offset-mask)
     :set-index (bit-and (bit-shift-right address offset-bits) index-mask)
     :tag (bit-shift-right address (+ offset-bits index-bits))}))

(defn line-address
  "Get the cache-line-aligned address (clear offset bits)"
  [address line-size]
  (let [offset-bits (int (Math/ceil (/ (Math/log line-size) (Math/log 2))))]
    (bit-and address (bit-not (dec (bit-shift-left 1 offset-bits))))))

;; =============================================================================
;; Cache Construction
;; =============================================================================

(defn make-empty-way
  "Create an empty cache way"
  [index]
  {:index index
   :line nil})

(defn make-empty-set
  "Create an empty cache set with N ways"
  [index associativity]
  {:index index
   :ways (mapv make-empty-way (range associativity))
   :lru-order (vec (range associativity))})  ; All ways in LRU order

(defn make-cache
  "Create a new empty cache from config.

   Config should have:
   - :level (:l1, :l2, :l3)
   - :type (:data, :instruction, :unified)
   - :size-kb
   - :line-size (default 64)
   - :associativity"
  [{:keys [level type size-kb line-size associativity] :as config
    :or {line-size 64}}]
  (let [size-bytes (* size-kb 1024)
        lines (quot size-bytes line-size)
        sets (quot lines associativity)]
    {:level level
     :type type
     :config (assoc config :sets sets :line-size line-size)
     :sets (mapv #(make-empty-set % associativity) (range sets))
     :stats {:hits 0
             :misses 0
             :evictions 0
             :reads 0
             :writes 0}}))

;; =============================================================================
;; LRU Management (Pure Functions)
;; =============================================================================

(defn touch-way
  "Move a way to MRU position (end of LRU order).
   Returns updated lru-order vector."
  [lru-order way-index]
  (let [filtered (filterv #(not= % way-index) lru-order)]
    (conj filtered way-index)))

(defn lru-victim
  "Get the LRU way index (first in order)"
  [lru-order]
  (first lru-order))

;; =============================================================================
;; Cache Operations (Pure Functions)
;; =============================================================================

(defn find-line-in-set
  "Find a cache line by tag in a set.
   Returns [way-index line] or nil."
  [cache-set tag]
  (->> (:ways cache-set)
       (map-indexed (fn [i way]
                      (when (and (:line way)
                                 (= (:tag (:line way)) tag))
                        [i (:line way)])))
       (filter some?)
       first))

(defn find-empty-way
  "Find an empty way in a set. Returns way-index or nil."
  [cache-set]
  (->> (:ways cache-set)
       (map-indexed (fn [i way] (when (nil? (:line way)) i)))
       (filter some?)
       first))

(defn update-way
  "Update a specific way in a set with a new line"
  [cache-set way-index line timestamp]
  (-> cache-set
      (assoc-in [:ways way-index :line]
                (assoc line
                       :last-access timestamp
                       :access-count (inc (get line :access-count 0))))
      (update :lru-order touch-way way-index)))

(defn evict-and-insert
  "Evict LRU line and insert new one.
   Returns [new-set evicted-line]"
  [cache-set new-line timestamp]
  (let [victim-way (lru-victim (:lru-order cache-set))
        evicted (get-in cache-set [:ways victim-way :line])
        new-set (update-way cache-set victim-way new-line timestamp)]
    [new-set evicted]))

;; =============================================================================
;; Main Access Function
;; =============================================================================

(defn access
  "Perform a cache access. Pure function.

   Arguments:
   - cache: current cache state
   - address: memory address being accessed
   - timestamp: current timestamp (for LRU tracking)
   - opts: {:write? false, :data nil}

   Returns:
   {:cache new-cache-state
    :result {:hit? bool
             :level :l1/:l2/:l3
             :set-index int
             :way-index int (or nil for miss)
             :evicted nil or {:address :data :dirty?}
             :latency-cycles int}}"
  [cache address timestamp & [{:keys [write? data] :or {write? false}}]]
  (let [{:keys [config sets]} cache
        {:keys [set-index tag]} (address-parts address config)
        cache-set (get sets set-index)
        [way-index existing-line] (find-line-in-set cache-set tag)]

    (if existing-line
      ;; === HIT ===
      (let [updated-line (cond-> existing-line
                           write? (assoc :state :modified)
                           data (assoc :data data))
            new-set (update-way cache-set way-index updated-line timestamp)
            new-cache (-> cache
                          (assoc-in [:sets set-index] new-set)
                          (update-in [:stats :hits] inc)
                          (update-in [:stats (if write? :writes :reads)] inc))]
        {:cache new-cache
         :result {:hit? true
                  :level (:level cache)
                  :set-index set-index
                  :way-index way-index
                  :evicted nil
                  :latency-cycles (case (:level cache)
                                    :l1 4
                                    :l2 12
                                    :l3 36)}})

      ;; === MISS ===
      (let [new-line {:tag tag
                      :data (or data (vec (repeat (:line-size config) 0)))
                      :state (if write? :modified :exclusive)
                      :last-access timestamp
                      :access-count 1}
            empty-way (find-empty-way cache-set)
            [new-set evicted] (if empty-way
                                [(update-way cache-set empty-way new-line timestamp) nil]
                                (evict-and-insert cache-set new-line timestamp))
            new-cache (-> cache
                          (assoc-in [:sets set-index] new-set)
                          (update-in [:stats :misses] inc)
                          (update-in [:stats (if write? :writes :reads)] inc)
                          (cond-> evicted (update-in [:stats :evictions] inc)))]
        {:cache new-cache
         :result {:hit? false
                  :level (:level cache)
                  :set-index set-index
                  :way-index (or empty-way (lru-victim (:lru-order cache-set)))
                  :evicted (when evicted
                             {:address (bit-shift-left (:tag evicted)
                                                       (+ (int (Math/ceil (/ (Math/log (:sets config)) (Math/log 2))))
                                                          (int (Math/ceil (/ (Math/log (:line-size config)) (Math/log 2))))))
                              :data (:data evicted)
                              :dirty? (= :modified (:state evicted))})
                  :latency-cycles (case (:level cache)
                                    :l1 4
                                    :l2 12
                                    :l3 36)}}))))

;; =============================================================================
;; Cache Hierarchy
;; =============================================================================

(defn make-cache-hierarchy
  "Create a multi-level cache hierarchy.

   Example config:
   {:l1d {:size-kb 32 :associativity 8 :type :data}
    :l1i {:size-kb 32 :associativity 8 :type :instruction}
    :l2  {:size-kb 256 :associativity 4 :type :unified}
    :l3  {:size-kb 8192 :associativity 16 :type :unified}}"
  [config]
  {:l1d (when (:l1d config)
          (make-cache (assoc (:l1d config) :level :l1)))
   :l1i (when (:l1i config)
          (make-cache (assoc (:l1i config) :level :l1)))
   :l2 (when (:l2 config)
         (make-cache (assoc (:l2 config) :level :l2)))
   :l3 (when (:l3 config)
         (make-cache (assoc (:l3 config) :level :l3)))})

(defn hierarchy-access
  "Access through cache hierarchy.

   Checks L1 -> L2 -> L3 -> RAM.
   On miss, fills all levels.

   Returns:
   {:hierarchy new-hierarchy
    :results [{:level :l1 :hit? true/false ...} ...]
    :total-latency int
    :final-level :l1/:l2/:l3/:ram}"
  [hierarchy address timestamp & [{:keys [write? instruction?] :as opts}]]
  (let [l1-cache (if instruction? (:l1i hierarchy) (:l1d hierarchy))
        levels (cond-> []
                 l1-cache (conj [:l1 l1-cache (if instruction? :l1i :l1d)])
                 (:l2 hierarchy) (conj [:l2 (:l2 hierarchy) :l2])
                 (:l3 hierarchy) (conj [:l3 (:l3 hierarchy) :l3]))]

    (loop [remaining levels
           current-hierarchy hierarchy
           results []
           found-at nil]
      (if (or (empty? remaining) found-at)
        ;; Done - either found or checked all levels
        (let [total-latency (+ (reduce + 0 (map :latency-cycles results))
                               (if found-at 0 100))] ; 100 cycles for RAM
          {:hierarchy current-hierarchy
           :results results
           :total-latency total-latency
           :final-level (or found-at :ram)})

        ;; Check next level
        (let [[level cache key] (first remaining)
              {:keys [cache result]} (access cache address timestamp opts)
              new-hierarchy (assoc current-hierarchy key cache)
              new-results (conj results result)]
          (if (:hit? result)
            ;; Hit - we're done
            (recur [] new-hierarchy new-results level)
            ;; Miss - continue to next level
            (recur (rest remaining) new-hierarchy new-results nil)))))))

;; =============================================================================
;; Trace Replay
;; =============================================================================

(defn replay-accesses
  "Replay a sequence of memory accesses through cache hierarchy.

   accesses: [{:address 0x... :timestamp 0 :write? false} ...]

   Returns sequence of access results with running state."
  [hierarchy accesses]
  (reductions
   (fn [{:keys [hierarchy]} {:keys [address timestamp] :as access-info}]
     (let [result (hierarchy-access hierarchy address timestamp access-info)]
       (assoc result :access access-info)))
   {:hierarchy hierarchy :results [] :total-latency 0 :final-level nil}
   accesses))

(defn summarize-replay
  "Summarize results of replay-accesses"
  [replay-results]
  (let [results (rest replay-results)] ; Skip initial state
    {:total-accesses (count results)
     :l1-hits (count (filter #(= :l1 (:final-level %)) results))
     :l2-hits (count (filter #(= :l2 (:final-level %)) results))
     :l3-hits (count (filter #(= :l3 (:final-level %)) results))
     :ram-accesses (count (filter #(= :ram (:final-level %)) results))
     :total-cycles (reduce + 0 (map :total-latency results))
     :avg-latency (/ (reduce + 0.0 (map :total-latency results))
                     (max 1 (count results)))}))

;; =============================================================================
;; REPL Exploration
;; =============================================================================

(comment
  ;; Create a simple L1 cache
  (def l1 (make-cache {:level :l1
                       :type :data
                       :size-kb 32
                       :associativity 8}))

  ;; Check structure
  (:config l1)
  (count (:sets l1))  ; Should be 64 sets for 32KB/8-way

  ;; Do an access
  (def result1 (access l1 0x7ff612340000 0))
  (:result result1)  ; Should be miss

  ;; Same address again
  (def result2 (access (:cache result1) 0x7ff612340000 1))
  (:result result2)  ; Should be hit!

  ;; Different address, same set (test LRU)
  (def addr-parts (address-parts 0x7ff612340000 (:config l1)))

  ;; Create full hierarchy
  (def hier (make-cache-hierarchy
             {:l1d {:size-kb 32 :associativity 8 :type :data}
              :l2 {:size-kb 256 :associativity 4 :type :unified}
              :l3 {:size-kb 8192 :associativity 16 :type :unified}}))

  ;; Access through hierarchy
  (def h-result (hierarchy-access hier 0x7ff612340000 0))
  (:final-level h-result)  ; :ram (first access)
  (:total-latency h-result)

  ;; Replay a sequence
  (def accesses [{:address 0x1000 :timestamp 0}
                 {:address 0x2000 :timestamp 1}
                 {:address 0x1000 :timestamp 2}  ; Should hit L1
                 {:address 0x1040 :timestamp 3}  ; Same line as 0x1000!
                 {:address 0x3000 :timestamp 4}])

  (def replay (replay-accesses hier accesses))
  (summarize-replay replay)
  )
