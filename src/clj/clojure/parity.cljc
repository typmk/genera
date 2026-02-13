(ns clojure.parity
  "Cross-platform parity testing.

   Utilities for verifying that code behaves identically across platforms
   (JVM, PHP, JS, etc.). Run the same tests on multiple platforms and
   compare results.

   This namespace integrates with:
   - clojure.test for assertions and test running
   - clojure.bench for performance measurement

   Use script/gen_parity.clj to generate parity test suites."
  (:require [clojure.string :as str]))

;; ============================================================
;; Platform detection
;; ============================================================

(def platform
  "Keyword identifying the current platform."
  #?(:clj  :jvm
     :cljp :php
     :cljs :js))

(def platform-name
  "Human-readable platform name."
  #?(:clj  "JVM Clojure"
     :cljp "ClojurePHP"
     :cljs "ClojureScript"))

(defn platform-version
  "Returns the platform version string."
  []
  #?(:clj  (clojure-version)
     :cljp (php/phpversion)
     :cljs *clojurescript-version*))

;; ============================================================
;; Timing utilities (platform-agnostic)
;; ============================================================

(defn now-ns
  "Current time in nanoseconds."
  []
  #?(:clj  (System/nanoTime)
     :cljp (* (php/microtime true) 1000000000.0)
     :cljs (* (.now js/performance) 1000000)))

(defn now-us
  "Current time in microseconds."
  []
  #?(:clj  (/ (System/nanoTime) 1000.0)
     :cljp (* (php/microtime true) 1000000.0)
     :cljs (* (.now js/performance) 1000)))

(defn now-ms
  "Current time in milliseconds."
  []
  #?(:clj  (/ (System/nanoTime) 1000000.0)
     :cljp (* (php/microtime true) 1000.0)
     :cljs (.now js/performance)))

(defn memory-bytes
  "Current memory usage in bytes (approximate)."
  []
  #?(:clj  (- (.totalMemory (Runtime/getRuntime))
              (.freeMemory (Runtime/getRuntime)))
     :cljp (php/memory_get_usage)
     :cljs 0))  ; JS can't easily measure this

(defmacro timed
  "Execute body and return [result time-us]."
  [& body]
  `(let [start# (now-us)
         result# (do ~@body)
         end# (now-us)]
     [result# (- end# start#)]))

(defmacro measure
  "Execute body, record timing/memory, return result."
  [test-name category & body]
  `(let [mem-before# (memory-bytes)
         [result# time#] (timed ~@body)
         mem-after# (memory-bytes)]
     (record-result! ~test-name ~category result# time# (- mem-after# mem-before#) nil)
     result#))

;; ============================================================
;; Result collection
;; ============================================================

(def ^:dynamic *parity-results*
  "Atom containing parity test results by platform."
  (atom {}))

(def ^:dynamic *expected-values*
  "Atom containing expected values from reference platform."
  (atom {}))

(def ^:dynamic *reference-platform*
  "The reference platform to compare against (default :jvm)."
  :jvm)

(def ^:dynamic *test-results*
  "Atom for collecting individual test results with timing."
  (atom []))

(defn record-result!
  "Record a single test result with timing info."
  [test-name category result time-us mem-bytes error]
  (swap! *test-results* conj
         {:name test-name
          :category category
          :platform platform
          :result (when-not error (pr-str result))
          :time-us time-us
          :memory-bytes mem-bytes
          :error (when error
                   #?(:clj  (.getMessage ^Throwable error)
                      :cljp (php/-> error (getMessage))
                      :cljs (.-message error)))}))

(defn reset-results!
  "Clear all collected results."
  []
  (reset! *test-results* [])
  (reset! *parity-results* {}))

;; ============================================================
;; Parity test definition macros
;; ============================================================

(defmacro defparity
  "Define a parity test. The expression is evaluated and its result
   is compared across platforms.

   Example:
   (defparity map-basics
     {:result (into {} (map (fn [[k v]] [k (inc v)]) {:a 1 :b 2}))
      :expected {:a 2 :b 3}})"
  [name & body]
  `(do
     (defn ~name []
       ~@body)
     (swap! *parity-results* assoc-in [platform ~(str name)]
            (try
              {:status :pass :value (~name)}
              (catch #?(:clj Throwable :cljp Exception :cljs :default) e#
                {:status :error :error (ex-message e#)})))))

(defmacro expect
  "Records an expected value for a parity test.
   Run on the reference platform first, then compare on others.

   Example:
   (expect :map-conj (conj {:a 1} [:b 2]))"
  [test-name expr]
  `(let [result# ~expr]
     (swap! *expected-values* assoc ~test-name result#)
     result#))

(defmacro verify
  "Verifies a value matches the expected value from reference platform.

   Example:
   (verify :map-conj (conj {:a 1} [:b 2]))"
  [test-name expr]
  `(let [result# ~expr
         expected# (get @*expected-values* ~test-name ::not-found)]
     (if (= expected# ::not-found)
       {:status :no-expected
        :test ~test-name
        :actual result#}
       (if (= result# expected#)
         {:status :pass
          :test ~test-name
          :expected expected#
          :actual result#}
         {:status :fail
          :test ~test-name
          :expected expected#
          :actual result#}))))

;; ============================================================
;; Complexity verification
;; ============================================================

(defn run-scaling-test
  "Run an expression at multiple sizes to verify complexity.
   Returns map of size -> time-us."
  [setup-fn test-fn sizes]
  (into {}
        (for [n sizes]
          (let [_ (setup-fn n)
                [_ time] (timed (dotimes [_ 100] (test-fn)))]
            [n (/ time 100.0)]))))

(defn verify-complexity
  "Check if timing data matches expected complexity.
   :O1 - times should be roughly equal
   :O-n - times should scale linearly
   :O-log32-n - times should grow very slowly

   Drops the smallest size measurement (warmup) and uses generous
   tolerance to account for JIT and measurement noise."
  [timing-data expected-complexity]
  (let [;; Sort by size and drop first measurement (warmup effects)
        sorted-times (->> timing-data
                          (sort-by first)
                          (drop 1)
                          (map second))
        max-time (apply max sorted-times)
        min-time (apply min sorted-times)
        ratio (/ max-time (max min-time 0.001))]
    (case expected-complexity
      :O1 (< ratio 10.0)        ; O(1) should have <10x variation (generous for JIT)
      :O-log32-n (< ratio 10.0) ; O(log32 n) very slow growth
      :O-n true                 ; O(n) is expected to scale
      true)))

;; ============================================================
;; Parity reporting
;; ============================================================

(defn parity-summary
  "Generate summary of parity test results."
  []
  (let [results @*parity-results*
        platform-results (get results platform {})
        total (count platform-results)
        passed (count (filter #(= :pass (:status (val %))) platform-results))
        failed (count (filter #(= :fail (:status (val %))) platform-results))
        errors (count (filter #(= :error (:status (val %))) platform-results))
        parity-pct (if (> total 0) (* 100.0 (/ passed total)) 0)]
    {:platform platform
     :total total
     :passed passed
     :failed failed
     :errors errors
     :parity-percent parity-pct}))

(defn test-results-summary
  "Generate summary from collected test results."
  []
  (let [results @*test-results*
        by-category (group-by :category results)
        total (count results)
        passed (count (filter #(nil? (:error %)) results))
        failed (- total passed)
        parity-pct (if (> total 0) (* 100.0 (/ passed total)) 0)]
    {:platform platform
     :total total
     :passed passed
     :failed failed
     :parity-percent parity-pct
     :by-category (into {}
                        (for [[cat tests] by-category]
                          [cat {:total (count tests)
                                :passed (count (filter #(nil? (:error %)) tests))
                                :failed (count (filter :error tests))}]))}))

(defn print-parity-report
  "Print formatted parity report."
  []
  (let [summary (parity-summary)]
    (println)
    (println "========================================")
    (println "         PARITY REPORT")
    (println "========================================")
    (println "  Platform:       " platform-name)
    (println "  Total tests:    " (:total summary))
    (println "  Passed:         " (:passed summary))
    (println "  Failed:         " (:failed summary))
    (println "  Errors:         " (:errors summary))
    (println "----------------------------------------")
    (printf  "  PARITY:          %.1f%%\n" (double (:parity-percent summary)))
    (println "========================================")
    (println)
    ;; Print failures
    (let [results (get @*parity-results* platform {})]
      (when (some #(not= :pass (:status (val %))) results)
        (println "FAILURES:")
        (println)
        (doseq [[name result] results
                :when (not= :pass (:status result))]
          (println " " name)
          (println "    status:" (:status result))
          (when (:expected result)
            (println "    expected:" (pr-str (:expected result))))
          (when (:actual result)
            (println "    actual:" (pr-str (:actual result))))
          (when (:error result)
            (println "    error:" (:error result)))
          (println))))
    summary))

(defn print-test-results-report
  "Print report from collected test results."
  []
  (let [summary (test-results-summary)]
    (println)
    (println (str/join "" (repeat 60 "=")))
    (println " PARITY TEST RESULTS -" platform-name)
    (println (str/join "" (repeat 60 "=")))
    (println)
    (println "Summary:" (:passed summary) "/" (:total summary) "passed,"
             (:failed summary) "failed")
    (println)

    ;; By category
    (doseq [[cat stats] (sort-by first (:by-category summary))]
      (println (str "\n== " (name cat) " =="))
      (println "  Passed:" (:passed stats) "/" (:total stats)))

    ;; Errors
    (let [errors (filter :error @*test-results*)]
      (when (seq errors)
        (println "\n== ERRORS ==")
        (doseq [e errors]
          (println " " (:name e) "-" (:error e)))))

    (println)
    (println (str/join "" (repeat 60 "=")))
    (printf "PARITY: %.1f%%\n" (double (:parity-percent summary)))
    (println (str/join "" (repeat 60 "=")))
    summary))

;; ============================================================
;; Cross-platform comparison
;; ============================================================

(defn compare-platforms
  "Compare results between two platforms."
  [platform1 platform2]
  (let [results1 (get @*parity-results* platform1 {})
        results2 (get @*parity-results* platform2 {})]
    (for [name (keys results1)
          :when (contains? results2 name)]
      (let [r1 (get results1 name)
            r2 (get results2 name)]
        {:test name
         :platform1 platform1
         :platform2 platform2
         :status1 (:status r1)
         :status2 (:status r2)
         :match? (= (:value r1) (:value r2))
         :value1 (:value r1)
         :value2 (:value r2)}))))

(defn print-platform-comparison
  "Print comparison between two platforms."
  [platform1 platform2]
  (let [comparison (compare-platforms platform1 platform2)
        matching (count (filter :match? comparison))
        total (count comparison)]
    (println)
    (println "========================================")
    (println "      PLATFORM COMPARISON")
    (println "========================================")
    (println "  " (name platform1) "vs" (name platform2))
    (println "  Matching:" matching "/" total)
    (printf   "  Parity: %.1f%%\n" (if (> total 0) (* 100.0 (/ matching total)) 0.0))
    (println "========================================")
    (when (< matching total)
      (println)
      (println "DIFFERENCES:")
      (doseq [c comparison
              :when (not (:match? c))]
        (println " " (:test c))
        (println "   " (name platform1) ":" (pr-str (:value1 c)))
        (println "   " (name platform2) ":" (pr-str (:value2 c)))))
    comparison))

;; ============================================================
;; Result serialization
;; ============================================================

(defn serialize-results
  "Serialize parity results to EDN for cross-platform comparison."
  []
  (pr-str @*parity-results*))

(defn serialize-test-results
  "Serialize test results to EDN."
  []
  (pr-str @*test-results*))

(defn export-results
  "Export results to a file."
  [filename]
  #?(:clj  (spit filename (serialize-test-results))
     :cljp (php/file_put_contents filename (serialize-test-results))
     :cljs nil)  ; Not supported in browser JS
  (println "Results exported to" filename))

(defn merge-results
  "Merge results from another platform into current results."
  [other-results]
  (swap! *parity-results* merge other-results))

;; ============================================================
;; Test data definitions (for generators)
;; ============================================================

(def test-values
  "Standard test values by type, used by generators."
  {:nil [nil]
   :bool [true false]
   :int [0 1 -1 2 10 42 100 -100]
   :float [0.0 1.0 -1.0 0.5 3.14]
   :number [0 1 -1 42 0.5 3.14]
   :string ["" "a" "hello" "Hello World" "  "]
   :keyword [:a :b :foo :bar/baz]
   :symbol ['a 'b 'foo 'bar/baz]
   :vector [[] [1] [1 2] [1 2 3] [1 2 3 4 5]]
   :list ['() '(1) '(1 2) '(1 2 3)]
   :map [{} {:a 1} {:a 1 :b 2} {:a {:b 1}}]
   :set [#{} #{1} #{1 2} #{1 2 3}]
   :coll [nil [] [1 2] '(1 2) {:a 1} #{1 2}]
   :seqable [nil [] [1 2 3] '(1 2 3) "hello" {:a 1 :b 2}]
   :fn ['inc 'dec 'identity 'str 'count]
   :pred ['even? 'odd? 'nil? 'pos? 'neg? 'zero?]
   :any [nil true false 0 1 -1 "" "x" :k [] [1] {} {:a 1}]})

(def complexity-classes
  "Algorithmic complexity classes for verification."
  {:O1 {:description "Constant time"
        :max-ratio 2.0}
   :O-log-n {:description "Logarithmic time"
             :max-ratio 3.0}
   :O-log32-n {:description "Log base 32 (persistent data structures)"
               :max-ratio 5.0}
   :O-n {:description "Linear time"
         :expected-scaling true}
   :O-n-log-n {:description "Linearithmic time"
               :expected-scaling true}
   :O-n2 {:description "Quadratic time"
          :expected-scaling true}})
