(ns clojure.bench
  (:require [clojure.math :as math]
            [clojure.system :as sys])
  "Benchmarking utilities for performance measurement across platforms.")
  (:require [clojure.math :as math]
            [clojure.system :as sys])

;; ============================================================
;; Dynamic vars
;; ============================================================

(def ^:dynamic *benchmarks*
  "Atom containing registered benchmarks."
  (atom []))

(def ^:dynamic *bench-results*
  "Atom containing most recent benchmark results."
  (atom []))

(def ^:dynamic *warmup-iterations*
  "Number of warmup iterations before timing."
  10)

;; ============================================================
;; Time measurement
;; ============================================================

(defn nano-time
  "Returns current time in nanoseconds. For measuring elapsed time only."
  []
  (System/nanoTime))

(defn current-time-millis
  "Returns current time in milliseconds since epoch."
  []
  (System/currentTimeMillis))

;; ============================================================
;; Benchmark definition
;; ============================================================

(defmacro defbench
  "Define a named benchmark with the given number of iterations.

   Example:
   (defbench vector-conj 10000
     (reduce conj [] (range 100)))"
  [name iterations & body]
  `(swap! *benchmarks* conj
          {:name ~(str name)
           :iterations ~iterations
           :fn (fn [] ~@body)}))

(defn clear-benchmarks!
  "Clears all registered benchmarks."
  []
  (reset! *benchmarks* []))

;; ============================================================
;; Running benchmarks
;; ============================================================

(defn run-bench-single
  "Run a single benchmark and return results map."
  [bench-map]
  (let [name (:name bench-map)
        iterations (:iterations bench-map)
        f (:fn bench-map)
        ;; Warmup
        _ (dotimes [_ (min *warmup-iterations* iterations)] (f))
        ;; Timed run
        start (nano-time)
        _ (dotimes [_ iterations] (f))
        end (nano-time)
        elapsed-ns (- end start)
        ns-per-op (/ elapsed-ns iterations)
        us-per-op (/ ns-per-op 1000)
        ms-per-op (/ ns-per-op 1000000)
        ops-per-sec (if (> elapsed-ns 0)
                      (/ (* iterations 1000000000) elapsed-ns)
                      0)]
    {:name name
     :iterations iterations
     :total-ns elapsed-ns
     :total-ms (/ elapsed-ns 1000000.0)
     :ns-per-op ns-per-op
     :us-per-op us-per-op
     :ms-per-op ms-per-op
     :ops-per-sec ops-per-sec}))

(defn run-benchmarks
  "Run all registered benchmarks and print results."
  []
  (let [benchmarks @*benchmarks*]
    (println "\n========================================")
    (println "         BENCHMARK RESULTS")
    (println "========================================")
    (println)
    (printf "%-35s %12s %12s %10s\n" "Name" "ns/op" "ops/sec" "iters")
    (println (apply str (repeat 75 "-")))
    (reset! *bench-results* [])
    (doseq [b benchmarks]
      (let [result (run-bench-single b)]
        (swap! *bench-results* conj result)
        (printf "%-35s %12.0f %12.0f %10d\n"
                (:name result)
                (double (:ns-per-op result))
                (double (:ops-per-sec result))
                (:iterations result))))
    (println "========================================")
    @*bench-results*))

;; ============================================================
;; Quick benchmarking
;; ============================================================

(defmacro bench
  "Quick benchmark an expression. Runs it multiple times and prints timing.

   Example:
   (bench (reduce + (range 1000)))"
  [expr]
  `(let [f# (fn [] ~expr)
         ;; Quick calibration
         start# (nano-time)
         _# (dotimes [_# 100] (f#))
         elapsed# (- (nano-time) start#)
         ;; Estimate iterations for ~1 second
         ns-per-op# (/ elapsed# 100)
         iterations# (max 1 (min 10000000 (int (/ 1000000000 ns-per-op#))))
         ;; Actual timed run
         start2# (nano-time)
         _# (dotimes [_# iterations#] (f#))
         elapsed2# (- (nano-time) start2#)
         ns-per-op2# (/ elapsed2# iterations#)
         us-per-op# (/ ns-per-op2# 1000)
         ops-per-sec# (if (> elapsed2# 0) (/ (* iterations# 1000000000) elapsed2#) 0)]
     (println "Evaluation count:" iterations# "in" (/ elapsed2# 1000000.0) "ms")
     (println "           ns/op:" (long ns-per-op2#))
     (println "           us/op:" us-per-op#)
     (println "         ops/sec:" (long ops-per-sec#))
     {:iterations iterations#
      :ns-per-op ns-per-op2#
      :us-per-op us-per-op#
      :ops-per-sec ops-per-sec#}))

(defmacro quick-bench
  "Like bench but with fewer iterations for faster feedback."
  [expr]
  `(let [f# (fn [] ~expr)
         iterations# 1000
         ;; Warmup
         _# (dotimes [_# 10] (f#))
         ;; Timed run
         start# (nano-time)
         _# (dotimes [_# iterations#] (f#))
         elapsed# (- (nano-time) start#)
         ns-per-op# (/ elapsed# iterations#)]
     (println "Quick bench:" (/ ns-per-op# 1000) "us/op")
     {:ns-per-op ns-per-op#
      :us-per-op (/ ns-per-op# 1000)}))

;; ============================================================
;; Time macro (like clojure.core/time)
;; ============================================================

(defmacro time-it
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (nano-time)
         ret# ~expr
         elapsed# (- (nano-time) start#)]
     (println "Elapsed time:" (/ elapsed# 1000000.0) "ms")
     ret#))

;; ============================================================
;; Comparison utilities
;; ============================================================

(defn compare-benchmarks
  "Compare two benchmark result sets. Returns comparison data."
  [results1 results2]
  (let [by-name1 (into {} (map (juxt :name identity) results1))
        by-name2 (into {} (map (juxt :name identity) results2))]
    (for [name (keys by-name1)
          :when (contains? by-name2 name)]
      (let [r1 (get by-name1 name)
            r2 (get by-name2 name)
            ratio (/ (:ns-per-op r2) (:ns-per-op r1))]
        {:name name
         :ns-per-op-1 (:ns-per-op r1)
         :ns-per-op-2 (:ns-per-op r2)
         :ratio ratio
         :speedup (if (< ratio 1) (str (format "%.2fx" (/ 1 ratio)) " faster")
                                  (str (format "%.2fx" ratio) " slower"))}))))

(defn print-comparison
  "Print benchmark comparison in a table."
  [comparison]
  (println "\n========================================")
  (println "       BENCHMARK COMPARISON")
  (println "========================================")
  (println)
  (printf "%-30s %12s %12s %15s\n" "Name" "Before" "After" "Change")
  (println (apply str (repeat 75 "-")))
  (doseq [c comparison]
    (printf "%-30s %12.0f %12.0f %15s\n"
            (:name c)
            (double (:ns-per-op-1 c))
            (double (:ns-per-op-2 c))
            (:speedup c)))
  (println "========================================"))

;; ============================================================
;; Statistical Analysis (Criterium-inspired)
;; ============================================================

(def ^:dynamic *sample-count*
  "Number of samples to collect for statistical analysis."
  60)

(def ^:dynamic *target-execution-time-ns*
  "Target execution time per sample in nanoseconds (default 100ms)."
  100000000)

(def ^:dynamic *warmup-period-ns*
  "Warmup period in nanoseconds before measuring (default 3 seconds)."
  3000000000)

(def ^:dynamic *confidence-level*
  "Confidence level for intervals (default 0.95 = 95%)."
  0.95)

(defn mean
  "Calculate arithmetic mean of a sequence of numbers."
  [xs]
  (let [xs (vec xs)
        n (count xs)]
    (if (zero? n)
      0
      (/ (reduce + 0.0 xs) n))))

(defn variance
  "Calculate sample variance of a sequence of numbers."
  [xs]
  (let [xs (vec xs)
        n (count xs)]
    (if (< n 2)
      0
      (let [m (mean xs)
            sum-sq (reduce + 0.0 (map #(let [d (- % m)] (* d d)) xs))]
        (/ sum-sq (dec n))))))

(defn std-dev
  "Calculate sample standard deviation."
  [xs]
  (Math/sqrt (variance xs)))

(defn percentile
  "Calculate the p-th percentile of a sorted sequence (p in 0-100)."
  [sorted-xs p]
  (let [xs (vec sorted-xs)
        n (count xs)]
    (if (zero? n)
      0
      (let [k (* (/ p 100.0) (dec n))
            f (Math/floor k)
            c (Math/ceil k)
            fi (int f)
            ci (int c)]
        (if (= fi ci)
          (nth xs fi)
          (+ (* (- c k) (nth xs fi))
             (* (- k f) (nth xs ci))))))))

(defn quartiles
  "Calculate Q1, median (Q2), and Q3 of a sequence."
  [xs]
  (let [sorted (vec (sort xs))]
    {:q1 (percentile sorted 25)
     :median (percentile sorted 50)
     :q3 (percentile sorted 75)}))

(defn iqr
  "Calculate interquartile range (Q3 - Q1)."
  [xs]
  (let [{:keys [q1 q3]} (quartiles xs)]
    (- q3 q1)))

;; ============================================================
;; Outlier Detection (Tukey's Boxplot Method)
;; ============================================================

(defn detect-outliers
  "Detect outliers using Tukey's boxplot method.
   Returns a map with :mild and :severe outlier counts and the data."
  [xs]
  (let [sorted (vec (sort xs))
        {:keys [q1 q3]} (quartiles sorted)
        iqr-val (- q3 q1)
        lower-mild (- q1 (* 1.5 iqr-val))
        upper-mild (+ q3 (* 1.5 iqr-val))
        lower-severe (- q1 (* 3.0 iqr-val))
        upper-severe (+ q3 (* 3.0 iqr-val))
        classify (fn [x]
                   (cond
                     (or (< x lower-severe) (> x upper-severe)) :severe
                     (or (< x lower-mild) (> x upper-mild)) :mild
                     :else :normal))
        classified (group-by classify xs)]
    {:total (count xs)
     :mild-low (count (filter #(and (< % lower-mild) (>= % lower-severe)) xs))
     :mild-high (count (filter #(and (> % upper-mild) (<= % upper-severe)) xs))
     :severe-low (count (filter #(< % lower-severe) xs))
     :severe-high (count (filter #(> % upper-severe) xs))
     :lower-mild lower-mild
     :upper-mild upper-mild
     :lower-severe lower-severe
     :upper-severe upper-severe}))

(defn outlier-effect
  "Estimate the effect of outliers on variance.
   Returns :unaffected, :slight, :moderate, or :severe."
  [xs]
  (let [{:keys [total mild-low mild-high severe-low severe-high]} (detect-outliers xs)
        outlier-count (+ mild-low mild-high severe-low severe-high)
        outlier-ratio (if (zero? total) 0 (/ outlier-count total))]
    (cond
      (< outlier-ratio 0.01) :unaffected
      (< outlier-ratio 0.05) :slight
      (< outlier-ratio 0.10) :moderate
      :else :severe)))

;; ============================================================
;; Bootstrap Resampling
;; ============================================================

(defn bootstrap-sample
  "Generate a bootstrap sample (sample with replacement)."
  [xs]
  (let [xs (vec xs)
        n (count xs)]
    (repeatedly n #(nth xs (rand-int n)))))

(defn bootstrap
  "Perform bootstrap resampling to estimate confidence interval.
   Returns {:mean :lower :upper :std-dev} for the statistic."
  ([xs] (bootstrap xs mean 1000))
  ([xs statistic-fn] (bootstrap xs statistic-fn 1000))
  ([xs statistic-fn n-resamples]
   (let [original-stat (statistic-fn xs)
         bootstrap-stats (sort (repeatedly n-resamples
                                           #(statistic-fn (bootstrap-sample xs))))
         alpha (/ (- 1 *confidence-level*) 2)
         lower-idx (int (* alpha n-resamples))
         upper-idx (int (* (- 1 alpha) n-resamples))]
     {:estimate original-stat
      :lower (nth bootstrap-stats lower-idx)
      :upper (nth bootstrap-stats (dec upper-idx))
      :std-dev (std-dev bootstrap-stats)})))

;; ============================================================
;; Execution Count Estimation
;; ============================================================

(defn estimate-execution-count
  "Estimate how many iterations are needed to reach target execution time."
  [f target-ns]
  (let [;; Quick probe: run 10 times
        start (nano-time)
        _ (dotimes [_ 10] (f))
        elapsed (- (nano-time) start)
        ns-per-op (/ elapsed 10.0)]
    (if (<= ns-per-op 0)
      1000000  ;; fallback for very fast operations
      (max 1 (long (/ target-ns ns-per-op))))))

;; ============================================================
;; Overhead Estimation
;; ============================================================

(def ^:private overhead-cache (atom nil))

(defn estimate-overhead
  "Estimate the overhead of the timing loop itself."
  []
  (if-let [cached @overhead-cache]
    cached
    (let [noop (fn [])
          samples (for [_ (range 10)]
                    (let [n 100000
                          start (nano-time)
                          _ (dotimes [_ n] (noop))
                          elapsed (- (nano-time) start)]
                      (/ elapsed n)))
          overhead (mean samples)]
      (reset! overhead-cache overhead)
      overhead)))

(defn reset-overhead!
  "Reset cached overhead estimate."
  []
  (reset! overhead-cache nil))

;; ============================================================
;; Sample Collection
;; ============================================================

(defn collect-samples
  "Collect timing samples for a function.
   Returns a vector of ns-per-op measurements."
  [f n-samples execution-count]
  (let [overhead (estimate-overhead)]
    (vec
     (for [_ (range n-samples)]
       (let [start (nano-time)
             _ (dotimes [_ execution-count] (f))
             elapsed (- (nano-time) start)
             raw-ns-per-op (/ elapsed execution-count)
             adjusted (- raw-ns-per-op overhead)]
         (max 0 adjusted))))))  ;; prevent negative times

;; ============================================================
;; Warmup
;; ============================================================

(defn warmup
  "Run warmup iterations until warmup period elapsed."
  [f warmup-ns]
  (let [start (nano-time)]
    (loop [iterations 0]
      (f)
      (if (< (- (nano-time) start) warmup-ns)
        (recur (inc iterations))
        iterations))))

;; ============================================================
;; Full Benchmark with Statistics
;; ============================================================

(defn benchmark*
  "Run a comprehensive benchmark on function f.
   Options:
     :samples - number of samples (default *sample-count*)
     :target-time - target execution time per sample in ns
     :warmup - warmup period in ns
     :verbose - print progress (default false)

   Returns a detailed results map."
  ([f] (benchmark* f {}))
  ([f opts]
   (let [samples (get opts :samples *sample-count*)
         target-time (get opts :target-time *target-execution-time-ns*)
         warmup-time (get opts :warmup *warmup-period-ns*)
         verbose (get opts :verbose false)

         ;; Warmup phase
         _ (when verbose (println "Warming up..."))
         warmup-iters (warmup f warmup-time)
         _ (when verbose (println "Warmup:" warmup-iters "iterations"))

         ;; Estimate execution count
         exec-count (estimate-execution-count f target-time)
         _ (when verbose (println "Execution count:" exec-count))

         ;; Collect samples
         _ (when verbose (println "Collecting" samples "samples..."))
         raw-samples (collect-samples f samples exec-count)

         ;; Statistical analysis
         sorted-samples (vec (sort raw-samples))
         m (mean raw-samples)
         sd (std-dev raw-samples)
         {:keys [q1 median q3]} (quartiles raw-samples)
         outliers (detect-outliers raw-samples)
         effect (outlier-effect raw-samples)

         ;; Bootstrap confidence interval for mean
         ci (bootstrap raw-samples mean 1000)]

     {:samples raw-samples
      :sample-count samples
      :execution-count exec-count

      ;; Point estimates
      :mean m
      :std-dev sd
      :variance (variance raw-samples)

      ;; Percentiles
      :min (first sorted-samples)
      :max (last sorted-samples)
      :median median
      :q1 q1
      :q3 q3
      :percentile-2.5 (percentile sorted-samples 2.5)
      :percentile-97.5 (percentile sorted-samples 97.5)

      ;; Confidence interval
      :confidence-interval {:lower (:lower ci)
                            :upper (:upper ci)
                            :level *confidence-level*}

      ;; Outliers
      :outliers outliers
      :outlier-effect effect

      ;; Derived metrics
      :ns-per-op m
      :us-per-op (/ m 1000)
      :ms-per-op (/ m 1000000)
      :ops-per-sec (if (> m 0) (/ 1000000000.0 m) 0)})))

;; ============================================================
;; Report Formatting
;; ============================================================

(defn format-time
  "Format nanoseconds in appropriate units."
  [ns]
  (cond
    (< ns 1000) (str (format "%.2f" (double ns)) " ns")
    (< ns 1000000) (str (format "%.2f" (/ ns 1000.0)) " µs")
    (< ns 1000000000) (str (format "%.2f" (/ ns 1000000.0)) " ms")
    :else (str (format "%.2f" (/ ns 1000000000.0)) " s")))

(defn report-result
  "Print a formatted benchmark report."
  [result]
  (println)
  (println "========================================")
  (println "         BENCHMARK REPORT")
  (println "========================================")
  (println)
  (println "Execution count:" (:execution-count result))
  (println "Sample count:   " (:sample-count result))
  (println)
  (println "Statistics (ns/op):")
  (println "  Mean:         " (format-time (:mean result)))
  (println "  Std Dev:      " (format-time (:std-dev result)))
  (println)
  (println "Percentiles:")
  (println "  Min:          " (format-time (:min result)))
  (println "  25%:          " (format-time (:q1 result)))
  (println "  50% (median): " (format-time (:median result)))
  (println "  75%:          " (format-time (:q3 result)))
  (println "  Max:          " (format-time (:max result)))
  (println)
  (let [ci (:confidence-interval result)]
    (println (str (int (* 100 (:level ci))) "% Confidence Interval:"))
    (println "  Lower:        " (format-time (:lower ci)))
    (println "  Upper:        " (format-time (:upper ci))))
  (println)
  (let [outliers (:outliers result)]
    (println "Outlier Analysis:")
    (println "  Mild low:     " (:mild-low outliers))
    (println "  Mild high:    " (:mild-high outliers))
    (println "  Severe low:   " (:severe-low outliers))
    (println "  Severe high:  " (:severe-high outliers))
    (println "  Effect:       " (name (:outlier-effect result))))
  (println)
  (println "Throughput:     " (format "%.0f" (:ops-per-sec result)) "ops/sec")
  (println "========================================")
  result)

;; ============================================================
;; Enhanced Bench Macros
;; ============================================================

(defmacro bench-stats
  "Run a benchmark with full statistical analysis.

   Example:
   (bench-stats (reduce + (range 1000)))"
  [expr]
  `(report-result (benchmark* (fn [] ~expr))))

(defmacro quick-bench-stats
  "Quick benchmark with statistical analysis using reduced samples.

   Example:
   (quick-bench-stats (reduce + (range 1000)))"
  [expr]
  `(report-result (benchmark* (fn [] ~expr)
                              {:samples 10
                               :warmup 500000000})))
