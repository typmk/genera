;; ============================================================
;; COMPREHENSIVE PARITY TEST SUITE
;; Auto-generated from Clojure source libraries
;; ============================================================
;;
;; Libraries covered:
;; - clojure.core
;; - clojure.string
;; - clojure.set
;; - clojure.walk
;; - clojure.edn
;; - clojure.test
;; - clojure.pprint
;; - clojure.data
;; - clojure.zip
;;
;; Run on JVM Clojure first to establish reference results.
;; Then compile to target platform and compare.
;;

(ns clojure.test.parity-suite
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.edn :as edn]))

;; ============================================================
;; Platform-Agnostic Benchmark Framework
;; ============================================================

(def ^:dynamic *benchmarks* (atom []))
(def ^:dynamic *bench-results* (atom []))

(defn now-us []
  "Current time in microseconds (platform-agnostic)."
  #?(:clj  (quot (System/nanoTime) 1000)
     :cljp (php/call_user_func "\\Clojure\\Php\\microtime_us")
     :cljs (* (.now js/performance) 1000)))

(defmacro defbench [name iterations & body]
  "Define a benchmark. iterations is how many times to run."
  `(swap! *benchmarks* conj
          {:name ~(str name)
           :iterations ~iterations
           :fn (fn [] ~@body)}))

(defn run-bench-single [bench-map]
  (let [name (:name bench-map)
        iterations (:iterations bench-map)
        f (:fn bench-map)
        _ (dotimes [_ (min 10 iterations)] (f))
        start (now-us)
        _ (dotimes [_ iterations] (f))
        end (now-us)
        elapsed-us (- end start)
        us-per-op (/ elapsed-us iterations)
        ops-per-sec (if (> elapsed-us 0) (/ (* iterations 1000000) elapsed-us) 0)]
    {:name name :iterations iterations :total-us elapsed-us
     :us-per-op us-per-op :ops-per-sec ops-per-sec}))

(defn run-benchmarks []
  (println "\n======== BENCHMARK RESULTS ========")
  (reset! *bench-results* [])
  (doseq [b @*benchmarks*]
    (let [result (run-bench-single b)]
      (swap! *bench-results* conj result)
      (println (:name result) "->" (int (:us-per-op result)) "?s/op")))
  (println "====================================")
  @*bench-results*)

(defn platform-name []
  #?(:clj "JVM" :cljp "PHP" :cljs "JS"))

;; ============================================================
;; Result collection for parity comparison
;; ============================================================

(def ^:dynamic *results* (atom []))

(defn record-result! [fn-name expr result error]
  "Record a test result for later comparison."
  (swap! *results* conj
         {:fn fn-name
          :expr expr
          :result (when-not error (pr-str result))
          :error (when error #?(:clj (.getMessage error)
                                :cljp (.getMessage error)
                                :cljs (.-message error)))
          :platform (platform-name)}))

(defn export-results []
  "Export results as EDN for comparison."
  (pr-str @*results*))


;; ============================================================
;; clojure.core - 587 functions
;; ============================================================

(deftest test-clojure-core--star
  (testing "clojure.core/*"
    ;; Arities: [[] [x] [x y] [x y & more]]
    (try
      (let [result (*)]
        (record-result! "clojure.core/*" "(*)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(*)" nil e)))
    (try
      (let [result (* nil)]
        (record-result! "clojure.core/*" "(* nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* nil)" nil e)))
    (try
      (let [result (* true)]
        (record-result! "clojure.core/*" "(* true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* true)" nil e)))
    (try
      (let [result (* nil nil)]
        (record-result! "clojure.core/*" "(* nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* nil nil)" nil e)))
    (try
      (let [result (* nil true)]
        (record-result! "clojure.core/*" "(* nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* nil true)" nil e)))
    (try
      (let [result (* true nil)]
        (record-result! "clojure.core/*" "(* true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* true nil)" nil e)))
    (try
      (let [result (* nil nil)]
        (record-result! "clojure.core/*" "(* nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* nil nil)" nil e)))
    (try
      (let [result (* nil nil nil)]
        (record-result! "clojure.core/*" "(* nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*" "(* nil nil nil)" nil e)))))


(deftest test-clojure-core--star'
  (testing "clojure.core/*'"
    ;; Arities: [[] [x] [x y] [x y & more]]
    (try
      (let [result (*')]
        (record-result! "clojure.core/*'" "(*')" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*')" nil e)))
    (try
      (let [result (*' nil)]
        (record-result! "clojure.core/*'" "(*' nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' nil)" nil e)))
    (try
      (let [result (*' true)]
        (record-result! "clojure.core/*'" "(*' true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' true)" nil e)))
    (try
      (let [result (*' nil nil)]
        (record-result! "clojure.core/*'" "(*' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' nil nil)" nil e)))
    (try
      (let [result (*' nil true)]
        (record-result! "clojure.core/*'" "(*' nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' nil true)" nil e)))
    (try
      (let [result (*' true nil)]
        (record-result! "clojure.core/*'" "(*' true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' true nil)" nil e)))
    (try
      (let [result (*' nil nil)]
        (record-result! "clojure.core/*'" "(*' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' nil nil)" nil e)))
    (try
      (let [result (*' nil nil nil)]
        (record-result! "clojure.core/*'" "(*' nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/*'" "(*' nil nil nil)" nil e)))))


(deftest test-clojure-core--plus
  (testing "clojure.core/+"
    ;; Arities: [[] [x] [x y] [x y & more]]
    (try
      (let [result (+)]
        (record-result! "clojure.core/+" "(+)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+)" nil e)))
    (try
      (let [result (+ nil)]
        (record-result! "clojure.core/+" "(+ nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ nil)" nil e)))
    (try
      (let [result (+ true)]
        (record-result! "clojure.core/+" "(+ true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ true)" nil e)))
    (try
      (let [result (+ nil nil)]
        (record-result! "clojure.core/+" "(+ nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ nil nil)" nil e)))
    (try
      (let [result (+ nil true)]
        (record-result! "clojure.core/+" "(+ nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ nil true)" nil e)))
    (try
      (let [result (+ true nil)]
        (record-result! "clojure.core/+" "(+ true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ true nil)" nil e)))
    (try
      (let [result (+ nil nil)]
        (record-result! "clojure.core/+" "(+ nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ nil nil)" nil e)))
    (try
      (let [result (+ nil nil nil)]
        (record-result! "clojure.core/+" "(+ nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+" "(+ nil nil nil)" nil e)))))


(deftest test-clojure-core--plus'
  (testing "clojure.core/+'"
    ;; Arities: [[] [x] [x y] [x y & more]]
    (try
      (let [result (+')]
        (record-result! "clojure.core/+'" "(+')" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+')" nil e)))
    (try
      (let [result (+' nil)]
        (record-result! "clojure.core/+'" "(+' nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' nil)" nil e)))
    (try
      (let [result (+' true)]
        (record-result! "clojure.core/+'" "(+' true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' true)" nil e)))
    (try
      (let [result (+' nil nil)]
        (record-result! "clojure.core/+'" "(+' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' nil nil)" nil e)))
    (try
      (let [result (+' nil true)]
        (record-result! "clojure.core/+'" "(+' nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' nil true)" nil e)))
    (try
      (let [result (+' true nil)]
        (record-result! "clojure.core/+'" "(+' true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' true nil)" nil e)))
    (try
      (let [result (+' nil nil)]
        (record-result! "clojure.core/+'" "(+' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' nil nil)" nil e)))
    (try
      (let [result (+' nil nil nil)]
        (record-result! "clojure.core/+'" "(+' nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/+'" "(+' nil nil nil)" nil e)))))


(deftest test-clojure-core--
  (testing "clojure.core/-"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (- nil)]
        (record-result! "clojure.core/-" "(- nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil)" nil e)))
    (try
      (let [result (- true)]
        (record-result! "clojure.core/-" "(- true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- true)" nil e)))
    (try
      (let [result (- nil nil)]
        (record-result! "clojure.core/-" "(- nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil nil)" nil e)))
    (try
      (let [result (- nil true)]
        (record-result! "clojure.core/-" "(- nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil true)" nil e)))
    (try
      (let [result (- true nil)]
        (record-result! "clojure.core/-" "(- true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- true nil)" nil e)))
    (try
      (let [result (- nil nil)]
        (record-result! "clojure.core/-" "(- nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil nil)" nil e)))
    (try
      (let [result (- nil nil nil)]
        (record-result! "clojure.core/-" "(- nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil nil nil)" nil e)))
    (try
      (let [result (- nil nil nil nil)]
        (record-result! "clojure.core/-" "(- nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-" "(- nil nil nil nil)" nil e)))))


(deftest test-clojure-core--'
  (testing "clojure.core/-'"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (-' nil)]
        (record-result! "clojure.core/-'" "(-' nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil)" nil e)))
    (try
      (let [result (-' true)]
        (record-result! "clojure.core/-'" "(-' true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' true)" nil e)))
    (try
      (let [result (-' nil nil)]
        (record-result! "clojure.core/-'" "(-' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil nil)" nil e)))
    (try
      (let [result (-' nil true)]
        (record-result! "clojure.core/-'" "(-' nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil true)" nil e)))
    (try
      (let [result (-' true nil)]
        (record-result! "clojure.core/-'" "(-' true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' true nil)" nil e)))
    (try
      (let [result (-' nil nil)]
        (record-result! "clojure.core/-'" "(-' nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil nil)" nil e)))
    (try
      (let [result (-' nil nil nil)]
        (record-result! "clojure.core/-'" "(-' nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil nil nil)" nil e)))
    (try
      (let [result (-' nil nil nil nil)]
        (record-result! "clojure.core/-'" "(-' nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/-'" "(-' nil nil nil nil)" nil e)))))


(deftest test-clojure-core---gt
  (testing "clojure.core/->"
    ;; Arities: [[x & forms]]
    (try
      (let [result (-> nil)]
        (record-result! "clojure.core/->" "(-> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->" "(-> nil)" nil e)))
    (try
      (let [result (-> nil nil)]
        (record-result! "clojure.core/->" "(-> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->" "(-> nil nil)" nil e)))
    (try
      (let [result (-> nil nil nil)]
        (record-result! "clojure.core/->" "(-> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->" "(-> nil nil nil)" nil e)))))


(deftest test-clojure-core---gt-gt
  (testing "clojure.core/->>"
    ;; Arities: [[x & forms]]
    (try
      (let [result (->> nil)]
        (record-result! "clojure.core/->>" "(->> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->>" "(->> nil)" nil e)))
    (try
      (let [result (->> nil nil)]
        (record-result! "clojure.core/->>" "(->> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->>" "(->> nil nil)" nil e)))
    (try
      (let [result (->> nil nil nil)]
        (record-result! "clojure.core/->>" "(->> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/->>" "(->> nil nil nil)" nil e)))))


(deftest test-clojure-core---
  (testing "clojure.core/.."
    ;; Arities: [[x form] [x form & more]]
    (try
      (let [result (.. nil nil)]
        (record-result! "clojure.core/.." "(.. nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. nil nil)" nil e)))
    (try
      (let [result (.. nil true)]
        (record-result! "clojure.core/.." "(.. nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. nil true)" nil e)))
    (try
      (let [result (.. true nil)]
        (record-result! "clojure.core/.." "(.. true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. true nil)" nil e)))
    (try
      (let [result (.. nil nil)]
        (record-result! "clojure.core/.." "(.. nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. nil nil)" nil e)))
    (try
      (let [result (.. nil nil nil)]
        (record-result! "clojure.core/.." "(.. nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. nil nil nil)" nil e)))
    (try
      (let [result (.. nil nil nil nil)]
        (record-result! "clojure.core/.." "(.. nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/.." "(.. nil nil nil nil)" nil e)))))


(deftest test-clojure-core--
  (testing "clojure.core//"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (/ nil)]
        (record-result! "clojure.core//" "(/ nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil)" nil e)))
    (try
      (let [result (/ true)]
        (record-result! "clojure.core//" "(/ true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ true)" nil e)))
    (try
      (let [result (/ nil nil)]
        (record-result! "clojure.core//" "(/ nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil nil)" nil e)))
    (try
      (let [result (/ nil true)]
        (record-result! "clojure.core//" "(/ nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil true)" nil e)))
    (try
      (let [result (/ true nil)]
        (record-result! "clojure.core//" "(/ true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ true nil)" nil e)))
    (try
      (let [result (/ nil nil)]
        (record-result! "clojure.core//" "(/ nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil nil)" nil e)))
    (try
      (let [result (/ nil nil nil)]
        (record-result! "clojure.core//" "(/ nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil nil nil)" nil e)))
    (try
      (let [result (/ nil nil nil nil)]
        (record-result! "clojure.core//" "(/ nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core//" "(/ nil nil nil nil)" nil e)))))


(deftest test-clojure-core--lt
  (testing "clojure.core/<"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (< nil)]
        (record-result! "clojure.core/<" "(< nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil)" nil e)))
    (try
      (let [result (< true)]
        (record-result! "clojure.core/<" "(< true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< true)" nil e)))
    (try
      (let [result (< nil nil)]
        (record-result! "clojure.core/<" "(< nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil nil)" nil e)))
    (try
      (let [result (< nil true)]
        (record-result! "clojure.core/<" "(< nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil true)" nil e)))
    (try
      (let [result (< true nil)]
        (record-result! "clojure.core/<" "(< true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< true nil)" nil e)))
    (try
      (let [result (< nil nil)]
        (record-result! "clojure.core/<" "(< nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil nil)" nil e)))
    (try
      (let [result (< nil nil nil)]
        (record-result! "clojure.core/<" "(< nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil nil nil)" nil e)))
    (try
      (let [result (< nil nil nil nil)]
        (record-result! "clojure.core/<" "(< nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<" "(< nil nil nil nil)" nil e)))))


(deftest test-clojure-core--lt-eq
  (testing "clojure.core/<="
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (<= nil)]
        (record-result! "clojure.core/<=" "(<= nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil)" nil e)))
    (try
      (let [result (<= true)]
        (record-result! "clojure.core/<=" "(<= true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= true)" nil e)))
    (try
      (let [result (<= nil nil)]
        (record-result! "clojure.core/<=" "(<= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil nil)" nil e)))
    (try
      (let [result (<= nil true)]
        (record-result! "clojure.core/<=" "(<= nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil true)" nil e)))
    (try
      (let [result (<= true nil)]
        (record-result! "clojure.core/<=" "(<= true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= true nil)" nil e)))
    (try
      (let [result (<= nil nil)]
        (record-result! "clojure.core/<=" "(<= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil nil)" nil e)))
    (try
      (let [result (<= nil nil nil)]
        (record-result! "clojure.core/<=" "(<= nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil nil nil)" nil e)))
    (try
      (let [result (<= nil nil nil nil)]
        (record-result! "clojure.core/<=" "(<= nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/<=" "(<= nil nil nil nil)" nil e)))))


(deftest test-clojure-core--eq
  (testing "clojure.core/="
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (= nil)]
        (record-result! "clojure.core/=" "(= nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil)" nil e)))
    (try
      (let [result (= true)]
        (record-result! "clojure.core/=" "(= true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= true)" nil e)))
    (try
      (let [result (= nil nil)]
        (record-result! "clojure.core/=" "(= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil nil)" nil e)))
    (try
      (let [result (= nil true)]
        (record-result! "clojure.core/=" "(= nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil true)" nil e)))
    (try
      (let [result (= true nil)]
        (record-result! "clojure.core/=" "(= true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= true nil)" nil e)))
    (try
      (let [result (= nil nil)]
        (record-result! "clojure.core/=" "(= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil nil)" nil e)))
    (try
      (let [result (= nil nil nil)]
        (record-result! "clojure.core/=" "(= nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil nil nil)" nil e)))
    (try
      (let [result (= nil nil nil nil)]
        (record-result! "clojure.core/=" "(= nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/=" "(= nil nil nil nil)" nil e)))))


(deftest test-clojure-core--eq-eq
  (testing "clojure.core/=="
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (== nil)]
        (record-result! "clojure.core/==" "(== nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil)" nil e)))
    (try
      (let [result (== true)]
        (record-result! "clojure.core/==" "(== true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== true)" nil e)))
    (try
      (let [result (== nil nil)]
        (record-result! "clojure.core/==" "(== nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil nil)" nil e)))
    (try
      (let [result (== nil true)]
        (record-result! "clojure.core/==" "(== nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil true)" nil e)))
    (try
      (let [result (== true nil)]
        (record-result! "clojure.core/==" "(== true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== true nil)" nil e)))
    (try
      (let [result (== nil nil)]
        (record-result! "clojure.core/==" "(== nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil nil)" nil e)))
    (try
      (let [result (== nil nil nil)]
        (record-result! "clojure.core/==" "(== nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil nil nil)" nil e)))
    (try
      (let [result (== nil nil nil nil)]
        (record-result! "clojure.core/==" "(== nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/==" "(== nil nil nil nil)" nil e)))))


(deftest test-clojure-core--gt
  (testing "clojure.core/>"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (> nil)]
        (record-result! "clojure.core/>" "(> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil)" nil e)))
    (try
      (let [result (> true)]
        (record-result! "clojure.core/>" "(> true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> true)" nil e)))
    (try
      (let [result (> nil nil)]
        (record-result! "clojure.core/>" "(> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil nil)" nil e)))
    (try
      (let [result (> nil true)]
        (record-result! "clojure.core/>" "(> nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil true)" nil e)))
    (try
      (let [result (> true nil)]
        (record-result! "clojure.core/>" "(> true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> true nil)" nil e)))
    (try
      (let [result (> nil nil)]
        (record-result! "clojure.core/>" "(> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil nil)" nil e)))
    (try
      (let [result (> nil nil nil)]
        (record-result! "clojure.core/>" "(> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil nil nil)" nil e)))
    (try
      (let [result (> nil nil nil nil)]
        (record-result! "clojure.core/>" "(> nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>" "(> nil nil nil nil)" nil e)))))


(deftest test-clojure-core--gt-eq
  (testing "clojure.core/>="
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (>= nil)]
        (record-result! "clojure.core/>=" "(>= nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil)" nil e)))
    (try
      (let [result (>= true)]
        (record-result! "clojure.core/>=" "(>= true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= true)" nil e)))
    (try
      (let [result (>= nil nil)]
        (record-result! "clojure.core/>=" "(>= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil nil)" nil e)))
    (try
      (let [result (>= nil true)]
        (record-result! "clojure.core/>=" "(>= nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil true)" nil e)))
    (try
      (let [result (>= true nil)]
        (record-result! "clojure.core/>=" "(>= true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= true nil)" nil e)))
    (try
      (let [result (>= nil nil)]
        (record-result! "clojure.core/>=" "(>= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil nil)" nil e)))
    (try
      (let [result (>= nil nil nil)]
        (record-result! "clojure.core/>=" "(>= nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil nil nil)" nil e)))
    (try
      (let [result (>= nil nil nil nil)]
        (record-result! "clojure.core/>=" "(>= nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/>=" "(>= nil nil nil nil)" nil e)))))


(deftest test-clojure-core-NaN-p
  (testing "clojure.core/NaN?"
    ;; Arities: [[num]]
    (try
      (let [result (NaN? 0)]
        (record-result! "clojure.core/NaN?" "(NaN? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/NaN?" "(NaN? 0)" nil e)))
    (try
      (let [result (NaN? 1)]
        (record-result! "clojure.core/NaN?" "(NaN? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/NaN?" "(NaN? 1)" nil e)))))


(deftest test-clojure-core-abs
  (testing "clojure.core/abs"
    ;; Arities: [[a]]
    (try
      (let [result (abs nil)]
        (record-result! "clojure.core/abs" "(abs nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/abs" "(abs nil)" nil e)))
    (try
      (let [result (abs true)]
        (record-result! "clojure.core/abs" "(abs true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/abs" "(abs true)" nil e)))))


(deftest test-clojure-core-accessor
  (testing "clojure.core/accessor"
    ;; Arities: [[s key]]
    (try
      (let [result (accessor "" :a)]
        (record-result! "clojure.core/accessor" "(accessor \"\" :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/accessor" "(accessor \"\" :a)" nil e)))
    (try
      (let [result (accessor "" :b)]
        (record-result! "clojure.core/accessor" "(accessor \"\" :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/accessor" "(accessor \"\" :b)" nil e)))
    (try
      (let [result (accessor "a" :a)]
        (record-result! "clojure.core/accessor" "(accessor \"a\" :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/accessor" "(accessor \"a\" :a)" nil e)))))


(deftest test-clojure-core-aclone
  (testing "clojure.core/aclone"
    ;; Arities: [[array]]
    (try
      (let [result (aclone nil)]
        (record-result! "clojure.core/aclone" "(aclone nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aclone" "(aclone nil)" nil e)))
    (try
      (let [result (aclone true)]
        (record-result! "clojure.core/aclone" "(aclone true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aclone" "(aclone true)" nil e)))))


(deftest test-clojure-core-add-classpath
  (testing "clojure.core/add-classpath"
    ;; Arities: [[url]]
    (try
      (let [result (add-classpath nil)]
        (record-result! "clojure.core/add-classpath" "(add-classpath nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-classpath" "(add-classpath nil)" nil e)))
    (try
      (let [result (add-classpath true)]
        (record-result! "clojure.core/add-classpath" "(add-classpath true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-classpath" "(add-classpath true)" nil e)))))


(deftest test-clojure-core-add-tap
  (testing "clojure.core/add-tap"
    ;; Arities: [[f]]
    (try
      (let [result (add-tap 'inc)]
        (record-result! "clojure.core/add-tap" "(add-tap 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-tap" "(add-tap 'inc)" nil e)))
    (try
      (let [result (add-tap 'dec)]
        (record-result! "clojure.core/add-tap" "(add-tap 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-tap" "(add-tap 'dec)" nil e)))))


(deftest test-clojure-core-add-watch
  (testing "clojure.core/add-watch"
    ;; Arities: [[reference key fn]]
    (try
      (let [result (add-watch nil :a 'inc)]
        (record-result! "clojure.core/add-watch" "(add-watch nil :a 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-watch" "(add-watch nil :a 'inc)" nil e)))
    (try
      (let [result (add-watch nil :a 'dec)]
        (record-result! "clojure.core/add-watch" "(add-watch nil :a 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-watch" "(add-watch nil :a 'dec)" nil e)))
    (try
      (let [result (add-watch nil :b 'inc)]
        (record-result! "clojure.core/add-watch" "(add-watch nil :b 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/add-watch" "(add-watch nil :b 'inc)" nil e)))))


(deftest test-clojure-core-agent
  (testing "clojure.core/agent"
    ;; Arities: [[state & options]]
    (try
      (let [result (agent nil)]
        (record-result! "clojure.core/agent" "(agent nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent" "(agent nil)" nil e)))
    (try
      (let [result (agent nil nil)]
        (record-result! "clojure.core/agent" "(agent nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent" "(agent nil nil)" nil e)))
    (try
      (let [result (agent nil nil nil)]
        (record-result! "clojure.core/agent" "(agent nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent" "(agent nil nil nil)" nil e)))))


(deftest test-clojure-core-agent-error
  (testing "clojure.core/agent-error"
    ;; Arities: [[a]]
    (try
      (let [result (agent-error nil)]
        (record-result! "clojure.core/agent-error" "(agent-error nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent-error" "(agent-error nil)" nil e)))
    (try
      (let [result (agent-error true)]
        (record-result! "clojure.core/agent-error" "(agent-error true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent-error" "(agent-error true)" nil e)))))


(deftest test-clojure-core-agent-errors
  (testing "clojure.core/agent-errors"
    ;; Arities: [[a]]
    (try
      (let [result (agent-errors nil)]
        (record-result! "clojure.core/agent-errors" "(agent-errors nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent-errors" "(agent-errors nil)" nil e)))
    (try
      (let [result (agent-errors true)]
        (record-result! "clojure.core/agent-errors" "(agent-errors true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/agent-errors" "(agent-errors true)" nil e)))))


(deftest test-clojure-core-aget
  (testing "clojure.core/aget"
    ;; Arities: [[array idx] [array idx & idxs]]
    (try
      (let [result (aget nil 0)]
        (record-result! "clojure.core/aget" "(aget nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget nil 0)" nil e)))
    (try
      (let [result (aget nil 1)]
        (record-result! "clojure.core/aget" "(aget nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget nil 1)" nil e)))
    (try
      (let [result (aget true 0)]
        (record-result! "clojure.core/aget" "(aget true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget true 0)" nil e)))
    (try
      (let [result (aget nil 0)]
        (record-result! "clojure.core/aget" "(aget nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget nil 0)" nil e)))
    (try
      (let [result (aget nil 0 nil)]
        (record-result! "clojure.core/aget" "(aget nil 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget nil 0 nil)" nil e)))
    (try
      (let [result (aget nil 0 nil nil)]
        (record-result! "clojure.core/aget" "(aget nil 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aget" "(aget nil 0 nil nil)" nil e)))))


(deftest test-clojure-core-alength
  (testing "clojure.core/alength"
    ;; Arities: [[array]]
    (try
      (let [result (alength nil)]
        (record-result! "clojure.core/alength" "(alength nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alength" "(alength nil)" nil e)))
    (try
      (let [result (alength true)]
        (record-result! "clojure.core/alength" "(alength true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alength" "(alength true)" nil e)))))


(deftest test-clojure-core-alias
  (testing "clojure.core/alias"
    ;; Arities: [[alias namespace-sym]]
    (try
      (let [result (alias nil nil)]
        (record-result! "clojure.core/alias" "(alias nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alias" "(alias nil nil)" nil e)))
    (try
      (let [result (alias nil true)]
        (record-result! "clojure.core/alias" "(alias nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alias" "(alias nil true)" nil e)))
    (try
      (let [result (alias true nil)]
        (record-result! "clojure.core/alias" "(alias true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alias" "(alias true nil)" nil e)))))


(deftest test-clojure-core-all-ns
  (testing "clojure.core/all-ns"
    ;; Arities: [[]]
    (try
      (let [result (all-ns)]
        (record-result! "clojure.core/all-ns" "(all-ns)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/all-ns" "(all-ns)" nil e)))))


(deftest test-clojure-core-alter
  (testing "clojure.core/alter"
    ;; Arities: [[ref fun & args]]
    (try
      (let [result (alter nil nil)]
        (record-result! "clojure.core/alter" "(alter nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter" "(alter nil nil)" nil e)))
    (try
      (let [result (alter nil nil nil)]
        (record-result! "clojure.core/alter" "(alter nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter" "(alter nil nil nil)" nil e)))
    (try
      (let [result (alter nil nil nil nil)]
        (record-result! "clojure.core/alter" "(alter nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter" "(alter nil nil nil nil)" nil e)))))


(deftest test-clojure-core-alter-meta-bang
  (testing "clojure.core/alter-meta!"
    ;; Arities: [[iref f & args]]
    (try
      (let [result (alter-meta! nil 'inc)]
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc)" nil e)))
    (try
      (let [result (alter-meta! nil 'inc nil)]
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc nil)" nil e)))
    (try
      (let [result (alter-meta! nil 'inc nil nil)]
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-meta!" "(alter-meta! nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-alter-var-root
  (testing "clojure.core/alter-var-root"
    ;; Arities: [[v f & args]]
    (try
      (let [result (alter-var-root nil 'inc)]
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc)" nil e)))
    (try
      (let [result (alter-var-root nil 'inc nil)]
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc nil)" nil e)))
    (try
      (let [result (alter-var-root nil 'inc nil nil)]
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/alter-var-root" "(alter-var-root nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-amap
  (testing "clojure.core/amap"
    ;; Arities: [[a idx ret expr]]
    (try
      (let [result (amap nil 0 nil nil)]
        (record-result! "clojure.core/amap" "(amap nil 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/amap" "(amap nil 0 nil nil)" nil e)))
    (try
      (let [result (amap nil 0 nil true)]
        (record-result! "clojure.core/amap" "(amap nil 0 nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/amap" "(amap nil 0 nil true)" nil e)))
    (try
      (let [result (amap nil 0 true nil)]
        (record-result! "clojure.core/amap" "(amap nil 0 true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/amap" "(amap nil 0 true nil)" nil e)))))


(deftest test-clojure-core-ancestors
  (testing "clojure.core/ancestors"
    ;; Arities: [[tag] [h tag]]
    (try
      (let [result (ancestors nil)]
        (record-result! "clojure.core/ancestors" "(ancestors nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ancestors" "(ancestors nil)" nil e)))
    (try
      (let [result (ancestors true)]
        (record-result! "clojure.core/ancestors" "(ancestors true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ancestors" "(ancestors true)" nil e)))
    (try
      (let [result (ancestors nil nil)]
        (record-result! "clojure.core/ancestors" "(ancestors nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ancestors" "(ancestors nil nil)" nil e)))
    (try
      (let [result (ancestors nil true)]
        (record-result! "clojure.core/ancestors" "(ancestors nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ancestors" "(ancestors nil true)" nil e)))
    (try
      (let [result (ancestors true nil)]
        (record-result! "clojure.core/ancestors" "(ancestors true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ancestors" "(ancestors true nil)" nil e)))))


(deftest test-clojure-core-and
  (testing "clojure.core/and"
    ;; Arities: [[] [x] [x & next]]
    (try
      (let [result (and)]
        (record-result! "clojure.core/and" "(and)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and)" nil e)))
    (try
      (let [result (and nil)]
        (record-result! "clojure.core/and" "(and nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and nil)" nil e)))
    (try
      (let [result (and true)]
        (record-result! "clojure.core/and" "(and true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and true)" nil e)))
    (try
      (let [result (and nil)]
        (record-result! "clojure.core/and" "(and nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and nil)" nil e)))
    (try
      (let [result (and nil nil)]
        (record-result! "clojure.core/and" "(and nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and nil nil)" nil e)))
    (try
      (let [result (and nil nil nil)]
        (record-result! "clojure.core/and" "(and nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/and" "(and nil nil nil)" nil e)))))


(deftest test-clojure-core-any-p
  (testing "clojure.core/any?"
    ;; Arities: [[x]]
    (try
      (let [result (any? nil)]
        (record-result! "clojure.core/any?" "(any? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/any?" "(any? nil)" nil e)))
    (try
      (let [result (any? true)]
        (record-result! "clojure.core/any?" "(any? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/any?" "(any? true)" nil e)))))


(deftest test-clojure-core-apply
  (testing "clojure.core/apply"
    ;; Arities: [[f args] [f x args] [f x y args] [f x y z args] [f a b c d & args]]
    (try
      (let [result (apply 'inc nil)]
        (record-result! "clojure.core/apply" "(apply 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc nil)" nil e)))
    (try
      (let [result (apply 'inc true)]
        (record-result! "clojure.core/apply" "(apply 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc true)" nil e)))
    (try
      (let [result (apply 'dec nil)]
        (record-result! "clojure.core/apply" "(apply 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'dec nil)" nil e)))
    (try
      (let [result (apply 'inc nil nil)]
        (record-result! "clojure.core/apply" "(apply 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc nil nil)" nil e)))
    (try
      (let [result (apply 'inc nil true)]
        (record-result! "clojure.core/apply" "(apply 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc nil true)" nil e)))
    (try
      (let [result (apply 'inc true nil)]
        (record-result! "clojure.core/apply" "(apply 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc true nil)" nil e)))
    (try
      (let [result (apply 'inc nil nil nil)]
        (record-result! "clojure.core/apply" "(apply 'inc nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc nil nil nil)" nil e)))
    (try
      (let [result (apply 'inc nil nil true)]
        (record-result! "clojure.core/apply" "(apply 'inc nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/apply" "(apply 'inc nil nil true)" nil e)))))


(deftest test-clojure-core-areduce
  (testing "clojure.core/areduce"
    ;; Arities: [[a idx ret init expr]]
    (try
      (let [result (areduce nil 0 nil nil nil)]
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil nil nil)" nil e)))
    (try
      (let [result (areduce nil 0 nil nil true)]
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil nil true)" nil e)))
    (try
      (let [result (areduce nil 0 nil true nil)]
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/areduce" "(areduce nil 0 nil true nil)" nil e)))))


(deftest test-clojure-core-array-map
  (testing "clojure.core/array-map"
    ;; Arities: [[] [& keyvals]]
    (try
      (let [result (array-map)]
        (record-result! "clojure.core/array-map" "(array-map)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/array-map" "(array-map)" nil e)))
    (try
      (let [result (array-map)]
        (record-result! "clojure.core/array-map" "(array-map)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/array-map" "(array-map)" nil e)))))


(deftest test-clojure-core-as--gt
  (testing "clojure.core/as->"
    ;; Arities: [[expr name & forms]]
    (try
      (let [result (as-> nil nil)]
        (record-result! "clojure.core/as->" "(as-> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/as->" "(as-> nil nil)" nil e)))
    (try
      (let [result (as-> nil nil nil)]
        (record-result! "clojure.core/as->" "(as-> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/as->" "(as-> nil nil nil)" nil e)))
    (try
      (let [result (as-> nil nil nil nil)]
        (record-result! "clojure.core/as->" "(as-> nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/as->" "(as-> nil nil nil nil)" nil e)))))


(deftest test-clojure-core-aset
  (testing "clojure.core/aset"
    ;; Arities: [[array idx val] [array idx idx2 & idxv]]
    (try
      (let [result (aset nil 0 nil)]
        (record-result! "clojure.core/aset" "(aset nil 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 0 nil)" nil e)))
    (try
      (let [result (aset nil 0 true)]
        (record-result! "clojure.core/aset" "(aset nil 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 0 true)" nil e)))
    (try
      (let [result (aset nil 1 nil)]
        (record-result! "clojure.core/aset" "(aset nil 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 1 nil)" nil e)))
    (try
      (let [result (aset nil 0 nil)]
        (record-result! "clojure.core/aset" "(aset nil 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 0 nil)" nil e)))
    (try
      (let [result (aset nil 0 nil nil)]
        (record-result! "clojure.core/aset" "(aset nil 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 0 nil nil)" nil e)))
    (try
      (let [result (aset nil 0 nil nil nil)]
        (record-result! "clojure.core/aset" "(aset nil 0 nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/aset" "(aset nil 0 nil nil nil)" nil e)))))


(deftest test-clojure-core-assert
  (testing "clojure.core/assert"
    ;; Arities: [[x] [x message]]
    (try
      (let [result (assert nil)]
        (record-result! "clojure.core/assert" "(assert nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assert" "(assert nil)" nil e)))
    (try
      (let [result (assert true)]
        (record-result! "clojure.core/assert" "(assert true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assert" "(assert true)" nil e)))
    (try
      (let [result (assert nil nil)]
        (record-result! "clojure.core/assert" "(assert nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assert" "(assert nil nil)" nil e)))
    (try
      (let [result (assert nil true)]
        (record-result! "clojure.core/assert" "(assert nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assert" "(assert nil true)" nil e)))
    (try
      (let [result (assert true nil)]
        (record-result! "clojure.core/assert" "(assert true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assert" "(assert true nil)" nil e)))))


(deftest test-clojure-core-assoc
  (testing "clojure.core/assoc"
    ;; Arities: [quote ([map key val] [map key val & kvs])]
    (try
      (let [result (assoc)]
        (record-result! "clojure.core/assoc" "(assoc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc" "(assoc)" nil e)))
    (try
      (let [result (assoc)]
        (record-result! "clojure.core/assoc" "(assoc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc" "(assoc)" nil e)))))


(deftest test-clojure-core-assoc-bang
  (testing "clojure.core/assoc!"
    ;; Arities: [[coll key val] [coll key val & kvs]]
    (try
      (let [result (assoc! nil :a nil)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil)" nil e)))
    (try
      (let [result (assoc! nil :a true)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :a true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :a true)" nil e)))
    (try
      (let [result (assoc! nil :b nil)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :b nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :b nil)" nil e)))
    (try
      (let [result (assoc! nil :a nil)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil)" nil e)))
    (try
      (let [result (assoc! nil :a nil nil)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil nil)" nil e)))
    (try
      (let [result (assoc! nil :a nil nil nil)]
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc!" "(assoc! nil :a nil nil nil)" nil e)))))


(deftest test-clojure-core-assoc-in
  (testing "clojure.core/assoc-in"
    ;; Arities: [[m [k & ks] v]]
    (try
      (let [result (assoc-in {} nil nil)]
        (record-result! "clojure.core/assoc-in" "(assoc-in {} nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc-in" "(assoc-in {} nil nil)" nil e)))
    (try
      (let [result (assoc-in {} nil true)]
        (record-result! "clojure.core/assoc-in" "(assoc-in {} nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc-in" "(assoc-in {} nil true)" nil e)))
    (try
      (let [result (assoc-in {} true nil)]
        (record-result! "clojure.core/assoc-in" "(assoc-in {} true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/assoc-in" "(assoc-in {} true nil)" nil e)))))


(deftest test-clojure-core-associative-p
  (testing "clojure.core/associative?"
    ;; Arities: [[coll]]
    (try
      (let [result (associative? nil)]
        (record-result! "clojure.core/associative?" "(associative? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/associative?" "(associative? nil)" nil e)))
    (try
      (let [result (associative? [])]
        (record-result! "clojure.core/associative?" "(associative? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/associative?" "(associative? [])" nil e)))))


(deftest test-clojure-core-atom
  (testing "clojure.core/atom"
    ;; Arities: [[x] [x & options]]
    (try
      (let [result (atom nil)]
        (record-result! "clojure.core/atom" "(atom nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/atom" "(atom nil)" nil e)))
    (try
      (let [result (atom true)]
        (record-result! "clojure.core/atom" "(atom true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/atom" "(atom true)" nil e)))
    (try
      (let [result (atom nil)]
        (record-result! "clojure.core/atom" "(atom nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/atom" "(atom nil)" nil e)))
    (try
      (let [result (atom nil nil)]
        (record-result! "clojure.core/atom" "(atom nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/atom" "(atom nil nil)" nil e)))
    (try
      (let [result (atom nil nil nil)]
        (record-result! "clojure.core/atom" "(atom nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/atom" "(atom nil nil nil)" nil e)))))


(deftest test-clojure-core-await
  (testing "clojure.core/await"
    ;; Arities: [[& agents]]
    (try
      (let [result (await)]
        (record-result! "clojure.core/await" "(await)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await" "(await)" nil e)))))


(deftest test-clojure-core-await-for
  (testing "clojure.core/await-for"
    ;; Arities: [[timeout-ms & agents]]
    (try
      (let [result (await-for nil)]
        (record-result! "clojure.core/await-for" "(await-for nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await-for" "(await-for nil)" nil e)))
    (try
      (let [result (await-for nil nil)]
        (record-result! "clojure.core/await-for" "(await-for nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await-for" "(await-for nil nil)" nil e)))
    (try
      (let [result (await-for nil nil nil)]
        (record-result! "clojure.core/await-for" "(await-for nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await-for" "(await-for nil nil nil)" nil e)))))


(deftest test-clojure-core-await1
  (testing "clojure.core/await1"
    ;; Arities: [[a]]
    (try
      (let [result (await1 nil)]
        (record-result! "clojure.core/await1" "(await1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await1" "(await1 nil)" nil e)))
    (try
      (let [result (await1 true)]
        (record-result! "clojure.core/await1" "(await1 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/await1" "(await1 true)" nil e)))))


(deftest test-clojure-core-bases
  (testing "clojure.core/bases"
    ;; Arities: [[c]]
    (try
      (let [result (bases nil)]
        (record-result! "clojure.core/bases" "(bases nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bases" "(bases nil)" nil e)))
    (try
      (let [result (bases [])]
        (record-result! "clojure.core/bases" "(bases [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bases" "(bases [])" nil e)))))


(deftest test-clojure-core-bigdec
  (testing "clojure.core/bigdec"
    ;; Arities: [[x]]
    (try
      (let [result (bigdec nil)]
        (record-result! "clojure.core/bigdec" "(bigdec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bigdec" "(bigdec nil)" nil e)))
    (try
      (let [result (bigdec true)]
        (record-result! "clojure.core/bigdec" "(bigdec true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bigdec" "(bigdec true)" nil e)))))


(deftest test-clojure-core-bigint
  (testing "clojure.core/bigint"
    ;; Arities: [[x]]
    (try
      (let [result (bigint nil)]
        (record-result! "clojure.core/bigint" "(bigint nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bigint" "(bigint nil)" nil e)))
    (try
      (let [result (bigint true)]
        (record-result! "clojure.core/bigint" "(bigint true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bigint" "(bigint true)" nil e)))))


(deftest test-clojure-core-biginteger
  (testing "clojure.core/biginteger"
    ;; Arities: [[x]]
    (try
      (let [result (biginteger nil)]
        (record-result! "clojure.core/biginteger" "(biginteger nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/biginteger" "(biginteger nil)" nil e)))
    (try
      (let [result (biginteger true)]
        (record-result! "clojure.core/biginteger" "(biginteger true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/biginteger" "(biginteger true)" nil e)))))


(deftest test-clojure-core-binding
  (testing "clojure.core/binding"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (binding nil)]
        (record-result! "clojure.core/binding" "(binding nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/binding" "(binding nil)" nil e)))
    (try
      (let [result (binding nil nil)]
        (record-result! "clojure.core/binding" "(binding nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/binding" "(binding nil nil)" nil e)))
    (try
      (let [result (binding nil nil nil)]
        (record-result! "clojure.core/binding" "(binding nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/binding" "(binding nil nil nil)" nil e)))))


(deftest test-clojure-core-bit-and
  (testing "clojure.core/bit-and"
    ;; Arities: [[x y] [x y & more]]
    (try
      (let [result (bit-and nil nil)]
        (record-result! "clojure.core/bit-and" "(bit-and nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and nil nil)" nil e)))
    (try
      (let [result (bit-and nil true)]
        (record-result! "clojure.core/bit-and" "(bit-and nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and nil true)" nil e)))
    (try
      (let [result (bit-and true nil)]
        (record-result! "clojure.core/bit-and" "(bit-and true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and true nil)" nil e)))
    (try
      (let [result (bit-and nil nil)]
        (record-result! "clojure.core/bit-and" "(bit-and nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and nil nil)" nil e)))
    (try
      (let [result (bit-and nil nil nil)]
        (record-result! "clojure.core/bit-and" "(bit-and nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and nil nil nil)" nil e)))
    (try
      (let [result (bit-and nil nil nil nil)]
        (record-result! "clojure.core/bit-and" "(bit-and nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and" "(bit-and nil nil nil nil)" nil e)))))


(deftest test-clojure-core-bit-and-not
  (testing "clojure.core/bit-and-not"
    ;; Arities: [[x y] [x y & more]]
    (try
      (let [result (bit-and-not nil nil)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil)" nil e)))
    (try
      (let [result (bit-and-not nil true)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil true)" nil e)))
    (try
      (let [result (bit-and-not true nil)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not true nil)" nil e)))
    (try
      (let [result (bit-and-not nil nil)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil)" nil e)))
    (try
      (let [result (bit-and-not nil nil nil)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil nil)" nil e)))
    (try
      (let [result (bit-and-not nil nil nil nil)]
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-and-not" "(bit-and-not nil nil nil nil)" nil e)))))


(deftest test-clojure-core-bit-clear
  (testing "clojure.core/bit-clear"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-clear nil 0)]
        (record-result! "clojure.core/bit-clear" "(bit-clear nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-clear" "(bit-clear nil 0)" nil e)))
    (try
      (let [result (bit-clear nil 1)]
        (record-result! "clojure.core/bit-clear" "(bit-clear nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-clear" "(bit-clear nil 1)" nil e)))
    (try
      (let [result (bit-clear true 0)]
        (record-result! "clojure.core/bit-clear" "(bit-clear true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-clear" "(bit-clear true 0)" nil e)))))


(deftest test-clojure-core-bit-flip
  (testing "clojure.core/bit-flip"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-flip nil 0)]
        (record-result! "clojure.core/bit-flip" "(bit-flip nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-flip" "(bit-flip nil 0)" nil e)))
    (try
      (let [result (bit-flip nil 1)]
        (record-result! "clojure.core/bit-flip" "(bit-flip nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-flip" "(bit-flip nil 1)" nil e)))
    (try
      (let [result (bit-flip true 0)]
        (record-result! "clojure.core/bit-flip" "(bit-flip true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-flip" "(bit-flip true 0)" nil e)))))


(deftest test-clojure-core-bit-not
  (testing "clojure.core/bit-not"
    ;; Arities: [[x]]
    (try
      (let [result (bit-not nil)]
        (record-result! "clojure.core/bit-not" "(bit-not nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-not" "(bit-not nil)" nil e)))
    (try
      (let [result (bit-not true)]
        (record-result! "clojure.core/bit-not" "(bit-not true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-not" "(bit-not true)" nil e)))))


(deftest test-clojure-core-bit-or
  (testing "clojure.core/bit-or"
    ;; Arities: [[x y] [x y & more]]
    (try
      (let [result (bit-or nil nil)]
        (record-result! "clojure.core/bit-or" "(bit-or nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or nil nil)" nil e)))
    (try
      (let [result (bit-or nil true)]
        (record-result! "clojure.core/bit-or" "(bit-or nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or nil true)" nil e)))
    (try
      (let [result (bit-or true nil)]
        (record-result! "clojure.core/bit-or" "(bit-or true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or true nil)" nil e)))
    (try
      (let [result (bit-or nil nil)]
        (record-result! "clojure.core/bit-or" "(bit-or nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or nil nil)" nil e)))
    (try
      (let [result (bit-or nil nil nil)]
        (record-result! "clojure.core/bit-or" "(bit-or nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or nil nil nil)" nil e)))
    (try
      (let [result (bit-or nil nil nil nil)]
        (record-result! "clojure.core/bit-or" "(bit-or nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-or" "(bit-or nil nil nil nil)" nil e)))))


(deftest test-clojure-core-bit-set
  (testing "clojure.core/bit-set"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-set nil 0)]
        (record-result! "clojure.core/bit-set" "(bit-set nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-set" "(bit-set nil 0)" nil e)))
    (try
      (let [result (bit-set nil 1)]
        (record-result! "clojure.core/bit-set" "(bit-set nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-set" "(bit-set nil 1)" nil e)))
    (try
      (let [result (bit-set true 0)]
        (record-result! "clojure.core/bit-set" "(bit-set true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-set" "(bit-set true 0)" nil e)))))


(deftest test-clojure-core-bit-shift-left
  (testing "clojure.core/bit-shift-left"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-shift-left nil 0)]
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left nil 0)" nil e)))
    (try
      (let [result (bit-shift-left nil 1)]
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left nil 1)" nil e)))
    (try
      (let [result (bit-shift-left true 0)]
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-left" "(bit-shift-left true 0)" nil e)))))


(deftest test-clojure-core-bit-shift-right
  (testing "clojure.core/bit-shift-right"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-shift-right nil 0)]
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right nil 0)" nil e)))
    (try
      (let [result (bit-shift-right nil 1)]
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right nil 1)" nil e)))
    (try
      (let [result (bit-shift-right true 0)]
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-shift-right" "(bit-shift-right true 0)" nil e)))))


(deftest test-clojure-core-bit-test
  (testing "clojure.core/bit-test"
    ;; Arities: [[x n]]
    (try
      (let [result (bit-test nil 0)]
        (record-result! "clojure.core/bit-test" "(bit-test nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-test" "(bit-test nil 0)" nil e)))
    (try
      (let [result (bit-test nil 1)]
        (record-result! "clojure.core/bit-test" "(bit-test nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-test" "(bit-test nil 1)" nil e)))
    (try
      (let [result (bit-test true 0)]
        (record-result! "clojure.core/bit-test" "(bit-test true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-test" "(bit-test true 0)" nil e)))))


(deftest test-clojure-core-bit-xor
  (testing "clojure.core/bit-xor"
    ;; Arities: [[x y] [x y & more]]
    (try
      (let [result (bit-xor nil nil)]
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil)" nil e)))
    (try
      (let [result (bit-xor nil true)]
        (record-result! "clojure.core/bit-xor" "(bit-xor nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor nil true)" nil e)))
    (try
      (let [result (bit-xor true nil)]
        (record-result! "clojure.core/bit-xor" "(bit-xor true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor true nil)" nil e)))
    (try
      (let [result (bit-xor nil nil)]
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil)" nil e)))
    (try
      (let [result (bit-xor nil nil nil)]
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil nil)" nil e)))
    (try
      (let [result (bit-xor nil nil nil nil)]
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bit-xor" "(bit-xor nil nil nil nil)" nil e)))))


(deftest test-clojure-core-boolean
  (testing "clojure.core/boolean"
    ;; Arities: [[x]]
    (try
      (let [result (boolean nil)]
        (record-result! "clojure.core/boolean" "(boolean nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean" "(boolean nil)" nil e)))
    (try
      (let [result (boolean true)]
        (record-result! "clojure.core/boolean" "(boolean true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean" "(boolean true)" nil e)))))


(deftest test-clojure-core-boolean-array
  (testing "clojure.core/boolean-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (boolean-array nil)]
        (record-result! "clojure.core/boolean-array" "(boolean-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean-array" "(boolean-array nil)" nil e)))
    (try
      (let [result (boolean-array true)]
        (record-result! "clojure.core/boolean-array" "(boolean-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean-array" "(boolean-array true)" nil e)))
    (try
      (let [result (boolean-array nil nil)]
        (record-result! "clojure.core/boolean-array" "(boolean-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean-array" "(boolean-array nil nil)" nil e)))
    (try
      (let [result (boolean-array nil true)]
        (record-result! "clojure.core/boolean-array" "(boolean-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean-array" "(boolean-array nil true)" nil e)))
    (try
      (let [result (boolean-array true nil)]
        (record-result! "clojure.core/boolean-array" "(boolean-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean-array" "(boolean-array true nil)" nil e)))))


(deftest test-clojure-core-boolean-p
  (testing "clojure.core/boolean?"
    ;; Arities: [[x]]
    (try
      (let [result (boolean? nil)]
        (record-result! "clojure.core/boolean?" "(boolean? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean?" "(boolean? nil)" nil e)))
    (try
      (let [result (boolean? true)]
        (record-result! "clojure.core/boolean?" "(boolean? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/boolean?" "(boolean? true)" nil e)))))


(deftest test-clojure-core-bound-fn
  (testing "clojure.core/bound-fn"
    ;; Arities: [[& fntail]]
    (try
      (let [result (bound-fn)]
        (record-result! "clojure.core/bound-fn" "(bound-fn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bound-fn" "(bound-fn)" nil e)))))


(deftest test-clojure-core-bound-fn-star
  (testing "clojure.core/bound-fn*"
    ;; Arities: [[f]]
    (try
      (let [result (bound-fn* 'inc)]
        (record-result! "clojure.core/bound-fn*" "(bound-fn* 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bound-fn*" "(bound-fn* 'inc)" nil e)))
    (try
      (let [result (bound-fn* 'dec)]
        (record-result! "clojure.core/bound-fn*" "(bound-fn* 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bound-fn*" "(bound-fn* 'dec)" nil e)))))


(deftest test-clojure-core-bound-p
  (testing "clojure.core/bound?"
    ;; Arities: [[& vars]]
    (try
      (let [result (bound?)]
        (record-result! "clojure.core/bound?" "(bound?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bound?" "(bound?)" nil e)))))


(deftest test-clojure-core-bounded-count
  (testing "clojure.core/bounded-count"
    ;; Arities: [[n coll]]
    (try
      (let [result (bounded-count 0 nil)]
        (record-result! "clojure.core/bounded-count" "(bounded-count 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bounded-count" "(bounded-count 0 nil)" nil e)))
    (try
      (let [result (bounded-count 0 [])]
        (record-result! "clojure.core/bounded-count" "(bounded-count 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bounded-count" "(bounded-count 0 [])" nil e)))
    (try
      (let [result (bounded-count 1 nil)]
        (record-result! "clojure.core/bounded-count" "(bounded-count 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bounded-count" "(bounded-count 1 nil)" nil e)))))


(deftest test-clojure-core-butlast
  (testing "clojure.core/butlast"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (butlast)]
        (record-result! "clojure.core/butlast" "(butlast)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/butlast" "(butlast)" nil e)))
    (try
      (let [result (butlast)]
        (record-result! "clojure.core/butlast" "(butlast)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/butlast" "(butlast)" nil e)))))


(deftest test-clojure-core-byte
  (testing "clojure.core/byte"
    ;; Arities: [[x]]
    (try
      (let [result (byte nil)]
        (record-result! "clojure.core/byte" "(byte nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte" "(byte nil)" nil e)))
    (try
      (let [result (byte true)]
        (record-result! "clojure.core/byte" "(byte true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte" "(byte true)" nil e)))))


(deftest test-clojure-core-byte-array
  (testing "clojure.core/byte-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (byte-array nil)]
        (record-result! "clojure.core/byte-array" "(byte-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte-array" "(byte-array nil)" nil e)))
    (try
      (let [result (byte-array true)]
        (record-result! "clojure.core/byte-array" "(byte-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte-array" "(byte-array true)" nil e)))
    (try
      (let [result (byte-array nil nil)]
        (record-result! "clojure.core/byte-array" "(byte-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte-array" "(byte-array nil nil)" nil e)))
    (try
      (let [result (byte-array nil true)]
        (record-result! "clojure.core/byte-array" "(byte-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte-array" "(byte-array nil true)" nil e)))
    (try
      (let [result (byte-array true nil)]
        (record-result! "clojure.core/byte-array" "(byte-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/byte-array" "(byte-array true nil)" nil e)))))


(deftest test-clojure-core-bytes-p
  (testing "clojure.core/bytes?"
    ;; Arities: [[x]]
    (try
      (let [result (bytes? nil)]
        (record-result! "clojure.core/bytes?" "(bytes? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bytes?" "(bytes? nil)" nil e)))
    (try
      (let [result (bytes? true)]
        (record-result! "clojure.core/bytes?" "(bytes? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/bytes?" "(bytes? true)" nil e)))))


(deftest test-clojure-core-case
  (testing "clojure.core/case"
    ;; Arities: [[e & clauses]]
    (try
      (let [result (case nil)]
        (record-result! "clojure.core/case" "(case nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/case" "(case nil)" nil e)))
    (try
      (let [result (case nil nil)]
        (record-result! "clojure.core/case" "(case nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/case" "(case nil nil)" nil e)))
    (try
      (let [result (case nil nil nil)]
        (record-result! "clojure.core/case" "(case nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/case" "(case nil nil nil)" nil e)))))


(deftest test-clojure-core-cast
  (testing "clojure.core/cast"
    ;; Arities: [[c x]]
    (try
      (let [result (cast nil nil)]
        (record-result! "clojure.core/cast" "(cast nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cast" "(cast nil nil)" nil e)))
    (try
      (let [result (cast nil true)]
        (record-result! "clojure.core/cast" "(cast nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cast" "(cast nil true)" nil e)))
    (try
      (let [result (cast [] nil)]
        (record-result! "clojure.core/cast" "(cast [] nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cast" "(cast [] nil)" nil e)))))


(deftest test-clojure-core-cat
  (testing "clojure.core/cat"
    ;; Arities: [[rf]]
    (try
      (let [result (cat nil)]
        (record-result! "clojure.core/cat" "(cat nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cat" "(cat nil)" nil e)))
    (try
      (let [result (cat true)]
        (record-result! "clojure.core/cat" "(cat true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cat" "(cat true)" nil e)))))


(deftest test-clojure-core-char
  (testing "clojure.core/char"
    ;; Arities: [[x]]
    (try
      (let [result (char nil)]
        (record-result! "clojure.core/char" "(char nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char" "(char nil)" nil e)))
    (try
      (let [result (char true)]
        (record-result! "clojure.core/char" "(char true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char" "(char true)" nil e)))))


(deftest test-clojure-core-char-array
  (testing "clojure.core/char-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (char-array nil)]
        (record-result! "clojure.core/char-array" "(char-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char-array" "(char-array nil)" nil e)))
    (try
      (let [result (char-array true)]
        (record-result! "clojure.core/char-array" "(char-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char-array" "(char-array true)" nil e)))
    (try
      (let [result (char-array nil nil)]
        (record-result! "clojure.core/char-array" "(char-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char-array" "(char-array nil nil)" nil e)))
    (try
      (let [result (char-array nil true)]
        (record-result! "clojure.core/char-array" "(char-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char-array" "(char-array nil true)" nil e)))
    (try
      (let [result (char-array true nil)]
        (record-result! "clojure.core/char-array" "(char-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char-array" "(char-array true nil)" nil e)))))


(deftest test-clojure-core-char-p
  (testing "clojure.core/char?"
    ;; Arities: [quote ([x])]
    (try
      (let [result (char?)]
        (record-result! "clojure.core/char?" "(char?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char?" "(char?)" nil e)))
    (try
      (let [result (char?)]
        (record-result! "clojure.core/char?" "(char?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/char?" "(char?)" nil e)))))


(deftest test-clojure-core-chunk
  (testing "clojure.core/chunk"
    ;; Arities: [[b]]
    (try
      (let [result (chunk nil)]
        (record-result! "clojure.core/chunk" "(chunk nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk" "(chunk nil)" nil e)))
    (try
      (let [result (chunk true)]
        (record-result! "clojure.core/chunk" "(chunk true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk" "(chunk true)" nil e)))))


(deftest test-clojure-core-chunk-append
  (testing "clojure.core/chunk-append"
    ;; Arities: [[b x]]
    (try
      (let [result (chunk-append nil nil)]
        (record-result! "clojure.core/chunk-append" "(chunk-append nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-append" "(chunk-append nil nil)" nil e)))
    (try
      (let [result (chunk-append nil true)]
        (record-result! "clojure.core/chunk-append" "(chunk-append nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-append" "(chunk-append nil true)" nil e)))
    (try
      (let [result (chunk-append true nil)]
        (record-result! "clojure.core/chunk-append" "(chunk-append true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-append" "(chunk-append true nil)" nil e)))))


(deftest test-clojure-core-chunk-buffer
  (testing "clojure.core/chunk-buffer"
    ;; Arities: [[capacity]]
    (try
      (let [result (chunk-buffer nil)]
        (record-result! "clojure.core/chunk-buffer" "(chunk-buffer nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-buffer" "(chunk-buffer nil)" nil e)))
    (try
      (let [result (chunk-buffer true)]
        (record-result! "clojure.core/chunk-buffer" "(chunk-buffer true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-buffer" "(chunk-buffer true)" nil e)))))


(deftest test-clojure-core-chunk-cons
  (testing "clojure.core/chunk-cons"
    ;; Arities: [[chunk rest]]
    (try
      (let [result (chunk-cons nil nil)]
        (record-result! "clojure.core/chunk-cons" "(chunk-cons nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-cons" "(chunk-cons nil nil)" nil e)))
    (try
      (let [result (chunk-cons nil true)]
        (record-result! "clojure.core/chunk-cons" "(chunk-cons nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-cons" "(chunk-cons nil true)" nil e)))
    (try
      (let [result (chunk-cons true nil)]
        (record-result! "clojure.core/chunk-cons" "(chunk-cons true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-cons" "(chunk-cons true nil)" nil e)))))


(deftest test-clojure-core-chunk-first
  (testing "clojure.core/chunk-first"
    ;; Arities: [[s]]
    (try
      (let [result (chunk-first "")]
        (record-result! "clojure.core/chunk-first" "(chunk-first \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-first" "(chunk-first \"\")" nil e)))
    (try
      (let [result (chunk-first "a")]
        (record-result! "clojure.core/chunk-first" "(chunk-first \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-first" "(chunk-first \"a\")" nil e)))))


(deftest test-clojure-core-chunk-next
  (testing "clojure.core/chunk-next"
    ;; Arities: [[s]]
    (try
      (let [result (chunk-next "")]
        (record-result! "clojure.core/chunk-next" "(chunk-next \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-next" "(chunk-next \"\")" nil e)))
    (try
      (let [result (chunk-next "a")]
        (record-result! "clojure.core/chunk-next" "(chunk-next \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-next" "(chunk-next \"a\")" nil e)))))


(deftest test-clojure-core-chunk-rest
  (testing "clojure.core/chunk-rest"
    ;; Arities: [[s]]
    (try
      (let [result (chunk-rest "")]
        (record-result! "clojure.core/chunk-rest" "(chunk-rest \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-rest" "(chunk-rest \"\")" nil e)))
    (try
      (let [result (chunk-rest "a")]
        (record-result! "clojure.core/chunk-rest" "(chunk-rest \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunk-rest" "(chunk-rest \"a\")" nil e)))))


(deftest test-clojure-core-chunked-seq-p
  (testing "clojure.core/chunked-seq?"
    ;; Arities: [[s]]
    (try
      (let [result (chunked-seq? "")]
        (record-result! "clojure.core/chunked-seq?" "(chunked-seq? \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunked-seq?" "(chunked-seq? \"\")" nil e)))
    (try
      (let [result (chunked-seq? "a")]
        (record-result! "clojure.core/chunked-seq?" "(chunked-seq? \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/chunked-seq?" "(chunked-seq? \"a\")" nil e)))))


(deftest test-clojure-core-class
  (testing "clojure.core/class"
    ;; Arities: [[x]]
    (try
      (let [result (class nil)]
        (record-result! "clojure.core/class" "(class nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/class" "(class nil)" nil e)))
    (try
      (let [result (class true)]
        (record-result! "clojure.core/class" "(class true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/class" "(class true)" nil e)))))


(deftest test-clojure-core-class-p
  (testing "clojure.core/class?"
    ;; Arities: [[x]]
    (try
      (let [result (class? nil)]
        (record-result! "clojure.core/class?" "(class? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/class?" "(class? nil)" nil e)))
    (try
      (let [result (class? true)]
        (record-result! "clojure.core/class?" "(class? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/class?" "(class? true)" nil e)))))


(deftest test-clojure-core-clear-agent-errors
  (testing "clojure.core/clear-agent-errors"
    ;; Arities: [[a]]
    (try
      (let [result (clear-agent-errors nil)]
        (record-result! "clojure.core/clear-agent-errors" "(clear-agent-errors nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/clear-agent-errors" "(clear-agent-errors nil)" nil e)))
    (try
      (let [result (clear-agent-errors true)]
        (record-result! "clojure.core/clear-agent-errors" "(clear-agent-errors true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/clear-agent-errors" "(clear-agent-errors true)" nil e)))))


(deftest test-clojure-core-clojure-version
  (testing "clojure.core/clojure-version"
    ;; Arities: [[]]
    (try
      (let [result (clojure-version)]
        (record-result! "clojure.core/clojure-version" "(clojure-version)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/clojure-version" "(clojure-version)" nil e)))))


(deftest test-clojure-core-coll-p
  (testing "clojure.core/coll?"
    ;; Arities: [[x]]
    (try
      (let [result (coll? nil)]
        (record-result! "clojure.core/coll?" "(coll? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/coll?" "(coll? nil)" nil e)))
    (try
      (let [result (coll? true)]
        (record-result! "clojure.core/coll?" "(coll? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/coll?" "(coll? true)" nil e)))))


(deftest test-clojure-core-comment
  (testing "clojure.core/comment"
    ;; Arities: [[& body]]
    (try
      (let [result (comment)]
        (record-result! "clojure.core/comment" "(comment)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comment" "(comment)" nil e)))))


(deftest test-clojure-core-commute
  (testing "clojure.core/commute"
    ;; Arities: [[ref fun & args]]
    (try
      (let [result (commute nil nil)]
        (record-result! "clojure.core/commute" "(commute nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/commute" "(commute nil nil)" nil e)))
    (try
      (let [result (commute nil nil nil)]
        (record-result! "clojure.core/commute" "(commute nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/commute" "(commute nil nil nil)" nil e)))
    (try
      (let [result (commute nil nil nil nil)]
        (record-result! "clojure.core/commute" "(commute nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/commute" "(commute nil nil nil nil)" nil e)))))


(deftest test-clojure-core-comp
  (testing "clojure.core/comp"
    ;; Arities: [[] [f] [f g] [f g & fs]]
    (try
      (let [result (comp)]
        (record-result! "clojure.core/comp" "(comp)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp)" nil e)))
    (try
      (let [result (comp 'inc)]
        (record-result! "clojure.core/comp" "(comp 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'inc)" nil e)))
    (try
      (let [result (comp 'dec)]
        (record-result! "clojure.core/comp" "(comp 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'dec)" nil e)))
    (try
      (let [result (comp 'inc nil)]
        (record-result! "clojure.core/comp" "(comp 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'inc nil)" nil e)))
    (try
      (let [result (comp 'inc true)]
        (record-result! "clojure.core/comp" "(comp 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'inc true)" nil e)))
    (try
      (let [result (comp 'dec nil)]
        (record-result! "clojure.core/comp" "(comp 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'dec nil)" nil e)))
    (try
      (let [result (comp 'inc nil)]
        (record-result! "clojure.core/comp" "(comp 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'inc nil)" nil e)))
    (try
      (let [result (comp 'inc nil nil)]
        (record-result! "clojure.core/comp" "(comp 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comp" "(comp 'inc nil nil)" nil e)))))


(deftest test-clojure-core-comparator
  (testing "clojure.core/comparator"
    ;; Arities: [[pred]]
    (try
      (let [result (comparator 'even?)]
        (record-result! "clojure.core/comparator" "(comparator 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comparator" "(comparator 'even?)" nil e)))
    (try
      (let [result (comparator 'odd?)]
        (record-result! "clojure.core/comparator" "(comparator 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/comparator" "(comparator 'odd?)" nil e)))))


(deftest test-clojure-core-compare
  (testing "clojure.core/compare"
    ;; Arities: [[x y]]
    (try
      (let [result (compare nil nil)]
        (record-result! "clojure.core/compare" "(compare nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare" "(compare nil nil)" nil e)))
    (try
      (let [result (compare nil true)]
        (record-result! "clojure.core/compare" "(compare nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare" "(compare nil true)" nil e)))
    (try
      (let [result (compare true nil)]
        (record-result! "clojure.core/compare" "(compare true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare" "(compare true nil)" nil e)))))


(deftest test-clojure-core-compare-and-set-bang
  (testing "clojure.core/compare-and-set!"
    ;; Arities: [[atom oldval newval]]
    (try
      (let [result (compare-and-set! nil nil nil)]
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil nil nil)" nil e)))
    (try
      (let [result (compare-and-set! nil nil true)]
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil nil true)" nil e)))
    (try
      (let [result (compare-and-set! nil true nil)]
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compare-and-set!" "(compare-and-set! nil true nil)" nil e)))))


(deftest test-clojure-core-compile
  (testing "clojure.core/compile"
    ;; Arities: [[lib]]
    (try
      (let [result (compile nil)]
        (record-result! "clojure.core/compile" "(compile nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compile" "(compile nil)" nil e)))
    (try
      (let [result (compile true)]
        (record-result! "clojure.core/compile" "(compile true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/compile" "(compile true)" nil e)))))


(deftest test-clojure-core-complement
  (testing "clojure.core/complement"
    ;; Arities: [[f]]
    (try
      (let [result (complement 'inc)]
        (record-result! "clojure.core/complement" "(complement 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/complement" "(complement 'inc)" nil e)))
    (try
      (let [result (complement 'dec)]
        (record-result! "clojure.core/complement" "(complement 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/complement" "(complement 'dec)" nil e)))))


(deftest test-clojure-core-completing
  (testing "clojure.core/completing"
    ;; Arities: [[f] [f cf]]
    (try
      (let [result (completing 'inc)]
        (record-result! "clojure.core/completing" "(completing 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/completing" "(completing 'inc)" nil e)))
    (try
      (let [result (completing 'dec)]
        (record-result! "clojure.core/completing" "(completing 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/completing" "(completing 'dec)" nil e)))
    (try
      (let [result (completing 'inc nil)]
        (record-result! "clojure.core/completing" "(completing 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/completing" "(completing 'inc nil)" nil e)))
    (try
      (let [result (completing 'inc true)]
        (record-result! "clojure.core/completing" "(completing 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/completing" "(completing 'inc true)" nil e)))
    (try
      (let [result (completing 'dec nil)]
        (record-result! "clojure.core/completing" "(completing 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/completing" "(completing 'dec nil)" nil e)))))


(deftest test-clojure-core-concat
  (testing "clojure.core/concat"
    ;; Arities: [[] [x] [x y] [x y & zs]]
    (try
      (let [result (concat)]
        (record-result! "clojure.core/concat" "(concat)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat)" nil e)))
    (try
      (let [result (concat nil)]
        (record-result! "clojure.core/concat" "(concat nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat nil)" nil e)))
    (try
      (let [result (concat true)]
        (record-result! "clojure.core/concat" "(concat true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat true)" nil e)))
    (try
      (let [result (concat nil nil)]
        (record-result! "clojure.core/concat" "(concat nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat nil nil)" nil e)))
    (try
      (let [result (concat nil true)]
        (record-result! "clojure.core/concat" "(concat nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat nil true)" nil e)))
    (try
      (let [result (concat true nil)]
        (record-result! "clojure.core/concat" "(concat true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat true nil)" nil e)))
    (try
      (let [result (concat nil nil)]
        (record-result! "clojure.core/concat" "(concat nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat nil nil)" nil e)))
    (try
      (let [result (concat nil nil nil)]
        (record-result! "clojure.core/concat" "(concat nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/concat" "(concat nil nil nil)" nil e)))))


(deftest test-clojure-core-cond
  (testing "clojure.core/cond"
    ;; Arities: [[& clauses]]
    (try
      (let [result (cond)]
        (record-result! "clojure.core/cond" "(cond)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond" "(cond)" nil e)))))


(deftest test-clojure-core-cond--gt
  (testing "clojure.core/cond->"
    ;; Arities: [[expr & clauses]]
    (try
      (let [result (cond-> nil)]
        (record-result! "clojure.core/cond->" "(cond-> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->" "(cond-> nil)" nil e)))
    (try
      (let [result (cond-> nil nil)]
        (record-result! "clojure.core/cond->" "(cond-> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->" "(cond-> nil nil)" nil e)))
    (try
      (let [result (cond-> nil nil nil)]
        (record-result! "clojure.core/cond->" "(cond-> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->" "(cond-> nil nil nil)" nil e)))))


(deftest test-clojure-core-cond--gt-gt
  (testing "clojure.core/cond->>"
    ;; Arities: [[expr & clauses]]
    (try
      (let [result (cond->> nil)]
        (record-result! "clojure.core/cond->>" "(cond->> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->>" "(cond->> nil)" nil e)))
    (try
      (let [result (cond->> nil nil)]
        (record-result! "clojure.core/cond->>" "(cond->> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->>" "(cond->> nil nil)" nil e)))
    (try
      (let [result (cond->> nil nil nil)]
        (record-result! "clojure.core/cond->>" "(cond->> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cond->>" "(cond->> nil nil nil)" nil e)))))


(deftest test-clojure-core-condp
  (testing "clojure.core/condp"
    ;; Arities: [[pred expr & clauses]]
    (try
      (let [result (condp 'even? nil)]
        (record-result! "clojure.core/condp" "(condp 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/condp" "(condp 'even? nil)" nil e)))
    (try
      (let [result (condp 'even? nil nil)]
        (record-result! "clojure.core/condp" "(condp 'even? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/condp" "(condp 'even? nil nil)" nil e)))
    (try
      (let [result (condp 'even? nil nil nil)]
        (record-result! "clojure.core/condp" "(condp 'even? nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/condp" "(condp 'even? nil nil nil)" nil e)))))


(deftest test-clojure-core-conj
  (testing "clojure.core/conj"
    ;; Arities: [quote ([] [coll] [coll x] [coll x & xs])]
    (try
      (let [result (conj)]
        (record-result! "clojure.core/conj" "(conj)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj" "(conj)" nil e)))
    (try
      (let [result (conj)]
        (record-result! "clojure.core/conj" "(conj)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj" "(conj)" nil e)))))


(deftest test-clojure-core-conj-bang
  (testing "clojure.core/conj!"
    ;; Arities: [[] [coll] [coll x]]
    (try
      (let [result (conj!)]
        (record-result! "clojure.core/conj!" "(conj!)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj!)" nil e)))
    (try
      (let [result (conj! nil)]
        (record-result! "clojure.core/conj!" "(conj! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj! nil)" nil e)))
    (try
      (let [result (conj! [])]
        (record-result! "clojure.core/conj!" "(conj! [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj! [])" nil e)))
    (try
      (let [result (conj! nil nil)]
        (record-result! "clojure.core/conj!" "(conj! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj! nil nil)" nil e)))
    (try
      (let [result (conj! nil true)]
        (record-result! "clojure.core/conj!" "(conj! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj! nil true)" nil e)))
    (try
      (let [result (conj! [] nil)]
        (record-result! "clojure.core/conj!" "(conj! [] nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/conj!" "(conj! [] nil)" nil e)))))


(deftest test-clojure-core-cons
  (testing "clojure.core/cons"
    ;; Arities: [quote ([x seq])]
    (try
      (let [result (cons)]
        (record-result! "clojure.core/cons" "(cons)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cons" "(cons)" nil e)))
    (try
      (let [result (cons)]
        (record-result! "clojure.core/cons" "(cons)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cons" "(cons)" nil e)))))


(deftest test-clojure-core-constantly
  (testing "clojure.core/constantly"
    ;; Arities: [[x]]
    (try
      (let [result (constantly nil)]
        (record-result! "clojure.core/constantly" "(constantly nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/constantly" "(constantly nil)" nil e)))
    (try
      (let [result (constantly true)]
        (record-result! "clojure.core/constantly" "(constantly true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/constantly" "(constantly true)" nil e)))))


(deftest test-clojure-core-contains-p
  (testing "clojure.core/contains?"
    ;; Arities: [[coll key]]
    (try
      (let [result (contains? nil :a)]
        (record-result! "clojure.core/contains?" "(contains? nil :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/contains?" "(contains? nil :a)" nil e)))
    (try
      (let [result (contains? nil :b)]
        (record-result! "clojure.core/contains?" "(contains? nil :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/contains?" "(contains? nil :b)" nil e)))
    (try
      (let [result (contains? [] :a)]
        (record-result! "clojure.core/contains?" "(contains? [] :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/contains?" "(contains? [] :a)" nil e)))))


(deftest test-clojure-core-count
  (testing "clojure.core/count"
    ;; Arities: [[coll]]
    (try
      (let [result (count nil)]
        (record-result! "clojure.core/count" "(count nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/count" "(count nil)" nil e)))
    (try
      (let [result (count [])]
        (record-result! "clojure.core/count" "(count [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/count" "(count [])" nil e)))))


(deftest test-clojure-core-counted-p
  (testing "clojure.core/counted?"
    ;; Arities: [[coll]]
    (try
      (let [result (counted? nil)]
        (record-result! "clojure.core/counted?" "(counted? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/counted?" "(counted? nil)" nil e)))
    (try
      (let [result (counted? [])]
        (record-result! "clojure.core/counted?" "(counted? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/counted?" "(counted? [])" nil e)))))


(deftest test-clojure-core-create-ns
  (testing "clojure.core/create-ns"
    ;; Arities: [[sym]]
    (try
      (let [result (create-ns nil)]
        (record-result! "clojure.core/create-ns" "(create-ns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/create-ns" "(create-ns nil)" nil e)))
    (try
      (let [result (create-ns true)]
        (record-result! "clojure.core/create-ns" "(create-ns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/create-ns" "(create-ns true)" nil e)))))


(deftest test-clojure-core-create-struct
  (testing "clojure.core/create-struct"
    ;; Arities: [[& keys]]
    (try
      (let [result (create-struct)]
        (record-result! "clojure.core/create-struct" "(create-struct)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/create-struct" "(create-struct)" nil e)))))


(deftest test-clojure-core-cycle
  (testing "clojure.core/cycle"
    ;; Arities: [[coll]]
    (try
      (let [result (cycle nil)]
        (record-result! "clojure.core/cycle" "(cycle nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cycle" "(cycle nil)" nil e)))
    (try
      (let [result (cycle [])]
        (record-result! "clojure.core/cycle" "(cycle [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/cycle" "(cycle [])" nil e)))))


(deftest test-clojure-core-dec
  (testing "clojure.core/dec"
    ;; Arities: [[x]]
    (try
      (let [result (dec nil)]
        (record-result! "clojure.core/dec" "(dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dec" "(dec nil)" nil e)))
    (try
      (let [result (dec true)]
        (record-result! "clojure.core/dec" "(dec true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dec" "(dec true)" nil e)))))


(deftest test-clojure-core-dec'
  (testing "clojure.core/dec'"
    ;; Arities: [[x]]
    (try
      (let [result (dec' nil)]
        (record-result! "clojure.core/dec'" "(dec' nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dec'" "(dec' nil)" nil e)))
    (try
      (let [result (dec' true)]
        (record-result! "clojure.core/dec'" "(dec' true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dec'" "(dec' true)" nil e)))))


(deftest test-clojure-core-decimal-p
  (testing "clojure.core/decimal?"
    ;; Arities: [[n]]
    (try
      (let [result (decimal? 0)]
        (record-result! "clojure.core/decimal?" "(decimal? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/decimal?" "(decimal? 0)" nil e)))
    (try
      (let [result (decimal? 1)]
        (record-result! "clojure.core/decimal?" "(decimal? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/decimal?" "(decimal? 1)" nil e)))))


(deftest test-clojure-core-declare
  (testing "clojure.core/declare"
    ;; Arities: [[& names]]
    (try
      (let [result (declare)]
        (record-result! "clojure.core/declare" "(declare)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/declare" "(declare)" nil e)))))


(deftest test-clojure-core-dedupe
  (testing "clojure.core/dedupe"
    ;; Arities: [[] [coll]]
    (try
      (let [result (dedupe)]
        (record-result! "clojure.core/dedupe" "(dedupe)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dedupe" "(dedupe)" nil e)))
    (try
      (let [result (dedupe nil)]
        (record-result! "clojure.core/dedupe" "(dedupe nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dedupe" "(dedupe nil)" nil e)))
    (try
      (let [result (dedupe [])]
        (record-result! "clojure.core/dedupe" "(dedupe [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dedupe" "(dedupe [])" nil e)))))


(deftest test-clojure-core-default-data-readers
  (testing "clojure.core/default-data-readers"
    ;; Arities: [merge]
    (try
      (let [result (default-data-readers)]
        (record-result! "clojure.core/default-data-readers" "(default-data-readers)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/default-data-readers" "(default-data-readers)" nil e)))))


(deftest test-clojure-core-definline
  (testing "clojure.core/definline"
    ;; Arities: [[name & decl]]
    (try
      (let [result (definline nil)]
        (record-result! "clojure.core/definline" "(definline nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/definline" "(definline nil)" nil e)))
    (try
      (let [result (definline nil nil)]
        (record-result! "clojure.core/definline" "(definline nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/definline" "(definline nil nil)" nil e)))
    (try
      (let [result (definline nil nil nil)]
        (record-result! "clojure.core/definline" "(definline nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/definline" "(definline nil nil nil)" nil e)))))


(deftest test-clojure-core-defmacro
  (testing "clojure.core/defmacro"
    ;; Arities: [quote ([name doc-string? attr-map? [params*] body] [name doc-string? attr-map? ([params*] body) + attr-map?])]
    (try
      (let [result (defmacro)]
        (record-result! "clojure.core/defmacro" "(defmacro)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmacro" "(defmacro)" nil e)))
    (try
      (let [result (defmacro)]
        (record-result! "clojure.core/defmacro" "(defmacro)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmacro" "(defmacro)" nil e)))))


(deftest test-clojure-core-defmethod
  (testing "clojure.core/defmethod"
    ;; Arities: [[multifn dispatch-val & fn-tail]]
    (try
      (let [result (defmethod nil nil)]
        (record-result! "clojure.core/defmethod" "(defmethod nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmethod" "(defmethod nil nil)" nil e)))
    (try
      (let [result (defmethod nil nil nil)]
        (record-result! "clojure.core/defmethod" "(defmethod nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmethod" "(defmethod nil nil nil)" nil e)))
    (try
      (let [result (defmethod nil nil nil nil)]
        (record-result! "clojure.core/defmethod" "(defmethod nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmethod" "(defmethod nil nil nil nil)" nil e)))))


(deftest test-clojure-core-defmulti
  (testing "clojure.core/defmulti"
    ;; Arities: [quote ([name docstring? attr-map? dispatch-fn & options])]
    (try
      (let [result (defmulti)]
        (record-result! "clojure.core/defmulti" "(defmulti)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmulti" "(defmulti)" nil e)))
    (try
      (let [result (defmulti)]
        (record-result! "clojure.core/defmulti" "(defmulti)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defmulti" "(defmulti)" nil e)))))


(deftest test-clojure-core-defn
  (testing "clojure.core/defn"
    ;; Arities: [quote ([name doc-string? attr-map? [params*] prepost-map? body] [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])]
    (try
      (let [result (defn)]
        (record-result! "clojure.core/defn" "(defn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defn" "(defn)" nil e)))
    (try
      (let [result (defn)]
        (record-result! "clojure.core/defn" "(defn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defn" "(defn)" nil e)))))


(deftest test-clojure-core-defn-
  (testing "clojure.core/defn-"
    ;; Arities: [[name & decls]]
    (try
      (let [result (defn- nil)]
        (record-result! "clojure.core/defn-" "(defn- nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defn-" "(defn- nil)" nil e)))
    (try
      (let [result (defn- nil nil)]
        (record-result! "clojure.core/defn-" "(defn- nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defn-" "(defn- nil nil)" nil e)))
    (try
      (let [result (defn- nil nil nil)]
        (record-result! "clojure.core/defn-" "(defn- nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defn-" "(defn- nil nil nil)" nil e)))))


(deftest test-clojure-core-defonce
  (testing "clojure.core/defonce"
    ;; Arities: [[name expr]]
    (try
      (let [result (defonce nil nil)]
        (record-result! "clojure.core/defonce" "(defonce nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defonce" "(defonce nil nil)" nil e)))
    (try
      (let [result (defonce nil true)]
        (record-result! "clojure.core/defonce" "(defonce nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defonce" "(defonce nil true)" nil e)))
    (try
      (let [result (defonce true nil)]
        (record-result! "clojure.core/defonce" "(defonce true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defonce" "(defonce true nil)" nil e)))))


(deftest test-clojure-core-defstruct
  (testing "clojure.core/defstruct"
    ;; Arities: [[name & keys]]
    (try
      (let [result (defstruct nil)]
        (record-result! "clojure.core/defstruct" "(defstruct nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defstruct" "(defstruct nil)" nil e)))
    (try
      (let [result (defstruct nil nil)]
        (record-result! "clojure.core/defstruct" "(defstruct nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defstruct" "(defstruct nil nil)" nil e)))
    (try
      (let [result (defstruct nil nil nil)]
        (record-result! "clojure.core/defstruct" "(defstruct nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/defstruct" "(defstruct nil nil nil)" nil e)))))


(deftest test-clojure-core-delay
  (testing "clojure.core/delay"
    ;; Arities: [[& body]]
    (try
      (let [result (delay)]
        (record-result! "clojure.core/delay" "(delay)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/delay" "(delay)" nil e)))))


(deftest test-clojure-core-delay-p
  (testing "clojure.core/delay?"
    ;; Arities: [[x]]
    (try
      (let [result (delay? nil)]
        (record-result! "clojure.core/delay?" "(delay? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/delay?" "(delay? nil)" nil e)))
    (try
      (let [result (delay? true)]
        (record-result! "clojure.core/delay?" "(delay? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/delay?" "(delay? true)" nil e)))))


(deftest test-clojure-core-deliver
  (testing "clojure.core/deliver"
    ;; Arities: [[promise val]]
    (try
      (let [result (deliver nil nil)]
        (record-result! "clojure.core/deliver" "(deliver nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deliver" "(deliver nil nil)" nil e)))
    (try
      (let [result (deliver nil true)]
        (record-result! "clojure.core/deliver" "(deliver nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deliver" "(deliver nil true)" nil e)))
    (try
      (let [result (deliver true nil)]
        (record-result! "clojure.core/deliver" "(deliver true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deliver" "(deliver true nil)" nil e)))))


(deftest test-clojure-core-denominator
  (testing "clojure.core/denominator"
    ;; Arities: [[r]]
    (try
      (let [result (denominator nil)]
        (record-result! "clojure.core/denominator" "(denominator nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/denominator" "(denominator nil)" nil e)))
    (try
      (let [result (denominator true)]
        (record-result! "clojure.core/denominator" "(denominator true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/denominator" "(denominator true)" nil e)))))


(deftest test-clojure-core-deref
  (testing "clojure.core/deref"
    ;; Arities: [[ref] [ref timeout-ms timeout-val]]
    (try
      (let [result (deref nil)]
        (record-result! "clojure.core/deref" "(deref nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deref" "(deref nil)" nil e)))
    (try
      (let [result (deref true)]
        (record-result! "clojure.core/deref" "(deref true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deref" "(deref true)" nil e)))
    (try
      (let [result (deref nil nil nil)]
        (record-result! "clojure.core/deref" "(deref nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deref" "(deref nil nil nil)" nil e)))
    (try
      (let [result (deref nil nil true)]
        (record-result! "clojure.core/deref" "(deref nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deref" "(deref nil nil true)" nil e)))
    (try
      (let [result (deref nil true nil)]
        (record-result! "clojure.core/deref" "(deref nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/deref" "(deref nil true nil)" nil e)))))


(deftest test-clojure-core-derive
  (testing "clojure.core/derive"
    ;; Arities: [[tag parent] [h tag parent]]
    (try
      (let [result (derive nil nil)]
        (record-result! "clojure.core/derive" "(derive nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive nil nil)" nil e)))
    (try
      (let [result (derive nil true)]
        (record-result! "clojure.core/derive" "(derive nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive nil true)" nil e)))
    (try
      (let [result (derive true nil)]
        (record-result! "clojure.core/derive" "(derive true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive true nil)" nil e)))
    (try
      (let [result (derive nil nil nil)]
        (record-result! "clojure.core/derive" "(derive nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive nil nil nil)" nil e)))
    (try
      (let [result (derive nil nil true)]
        (record-result! "clojure.core/derive" "(derive nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive nil nil true)" nil e)))
    (try
      (let [result (derive nil true nil)]
        (record-result! "clojure.core/derive" "(derive nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/derive" "(derive nil true nil)" nil e)))))


(deftest test-clojure-core-descendants
  (testing "clojure.core/descendants"
    ;; Arities: [[tag] [h tag]]
    (try
      (let [result (descendants nil)]
        (record-result! "clojure.core/descendants" "(descendants nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/descendants" "(descendants nil)" nil e)))
    (try
      (let [result (descendants true)]
        (record-result! "clojure.core/descendants" "(descendants true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/descendants" "(descendants true)" nil e)))
    (try
      (let [result (descendants nil nil)]
        (record-result! "clojure.core/descendants" "(descendants nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/descendants" "(descendants nil nil)" nil e)))
    (try
      (let [result (descendants nil true)]
        (record-result! "clojure.core/descendants" "(descendants nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/descendants" "(descendants nil true)" nil e)))
    (try
      (let [result (descendants true nil)]
        (record-result! "clojure.core/descendants" "(descendants true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/descendants" "(descendants true nil)" nil e)))))


(deftest test-clojure-core-destructure
  (testing "clojure.core/destructure"
    ;; Arities: [[bindings]]
    (try
      (let [result (destructure nil)]
        (record-result! "clojure.core/destructure" "(destructure nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/destructure" "(destructure nil)" nil e)))
    (try
      (let [result (destructure true)]
        (record-result! "clojure.core/destructure" "(destructure true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/destructure" "(destructure true)" nil e)))))


(deftest test-clojure-core-disj
  (testing "clojure.core/disj"
    ;; Arities: [[set] [set key] [set key & ks]]
    (try
      (let [result (disj #{})]
        (record-result! "clojure.core/disj" "(disj #{})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{})" nil e)))
    (try
      (let [result (disj #{1})]
        (record-result! "clojure.core/disj" "(disj #{1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{1})" nil e)))
    (try
      (let [result (disj #{} :a)]
        (record-result! "clojure.core/disj" "(disj #{} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{} :a)" nil e)))
    (try
      (let [result (disj #{} :b)]
        (record-result! "clojure.core/disj" "(disj #{} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{} :b)" nil e)))
    (try
      (let [result (disj #{1} :a)]
        (record-result! "clojure.core/disj" "(disj #{1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{1} :a)" nil e)))
    (try
      (let [result (disj #{} :a)]
        (record-result! "clojure.core/disj" "(disj #{} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{} :a)" nil e)))
    (try
      (let [result (disj #{} :a nil)]
        (record-result! "clojure.core/disj" "(disj #{} :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{} :a nil)" nil e)))
    (try
      (let [result (disj #{} :a nil nil)]
        (record-result! "clojure.core/disj" "(disj #{} :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj" "(disj #{} :a nil nil)" nil e)))))


(deftest test-clojure-core-disj-bang
  (testing "clojure.core/disj!"
    ;; Arities: [[set] [set key] [set key & ks]]
    (try
      (let [result (disj! #{})]
        (record-result! "clojure.core/disj!" "(disj! #{})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{})" nil e)))
    (try
      (let [result (disj! #{1})]
        (record-result! "clojure.core/disj!" "(disj! #{1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{1})" nil e)))
    (try
      (let [result (disj! #{} :a)]
        (record-result! "clojure.core/disj!" "(disj! #{} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{} :a)" nil e)))
    (try
      (let [result (disj! #{} :b)]
        (record-result! "clojure.core/disj!" "(disj! #{} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{} :b)" nil e)))
    (try
      (let [result (disj! #{1} :a)]
        (record-result! "clojure.core/disj!" "(disj! #{1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{1} :a)" nil e)))
    (try
      (let [result (disj! #{} :a)]
        (record-result! "clojure.core/disj!" "(disj! #{} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{} :a)" nil e)))
    (try
      (let [result (disj! #{} :a nil)]
        (record-result! "clojure.core/disj!" "(disj! #{} :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{} :a nil)" nil e)))
    (try
      (let [result (disj! #{} :a nil nil)]
        (record-result! "clojure.core/disj!" "(disj! #{} :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/disj!" "(disj! #{} :a nil nil)" nil e)))))


(deftest test-clojure-core-dissoc
  (testing "clojure.core/dissoc"
    ;; Arities: [[map] [map key] [map key & ks]]
    (try
      (let [result (dissoc {})]
        (record-result! "clojure.core/dissoc" "(dissoc {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {})" nil e)))
    (try
      (let [result (dissoc {:a 1})]
        (record-result! "clojure.core/dissoc" "(dissoc {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {:a 1})" nil e)))
    (try
      (let [result (dissoc {} :a)]
        (record-result! "clojure.core/dissoc" "(dissoc {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {} :a)" nil e)))
    (try
      (let [result (dissoc {} :b)]
        (record-result! "clojure.core/dissoc" "(dissoc {} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {} :b)" nil e)))
    (try
      (let [result (dissoc {:a 1} :a)]
        (record-result! "clojure.core/dissoc" "(dissoc {:a 1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {:a 1} :a)" nil e)))
    (try
      (let [result (dissoc {} :a)]
        (record-result! "clojure.core/dissoc" "(dissoc {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {} :a)" nil e)))
    (try
      (let [result (dissoc {} :a nil)]
        (record-result! "clojure.core/dissoc" "(dissoc {} :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {} :a nil)" nil e)))
    (try
      (let [result (dissoc {} :a nil nil)]
        (record-result! "clojure.core/dissoc" "(dissoc {} :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc" "(dissoc {} :a nil nil)" nil e)))))


(deftest test-clojure-core-dissoc-bang
  (testing "clojure.core/dissoc!"
    ;; Arities: [[map key] [map key & ks]]
    (try
      (let [result (dissoc! {} :a)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a)" nil e)))
    (try
      (let [result (dissoc! {} :b)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :b)" nil e)))
    (try
      (let [result (dissoc! {:a 1} :a)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {:a 1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {:a 1} :a)" nil e)))
    (try
      (let [result (dissoc! {} :a)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a)" nil e)))
    (try
      (let [result (dissoc! {} :a nil)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a nil)" nil e)))
    (try
      (let [result (dissoc! {} :a nil nil)]
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dissoc!" "(dissoc! {} :a nil nil)" nil e)))))


(deftest test-clojure-core-distinct
  (testing "clojure.core/distinct"
    ;; Arities: [[] [coll]]
    (try
      (let [result (distinct)]
        (record-result! "clojure.core/distinct" "(distinct)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct" "(distinct)" nil e)))
    (try
      (let [result (distinct nil)]
        (record-result! "clojure.core/distinct" "(distinct nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct" "(distinct nil)" nil e)))
    (try
      (let [result (distinct [])]
        (record-result! "clojure.core/distinct" "(distinct [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct" "(distinct [])" nil e)))))


(deftest test-clojure-core-distinct-p
  (testing "clojure.core/distinct?"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (distinct? nil)]
        (record-result! "clojure.core/distinct?" "(distinct? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil)" nil e)))
    (try
      (let [result (distinct? true)]
        (record-result! "clojure.core/distinct?" "(distinct? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? true)" nil e)))
    (try
      (let [result (distinct? nil nil)]
        (record-result! "clojure.core/distinct?" "(distinct? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil nil)" nil e)))
    (try
      (let [result (distinct? nil true)]
        (record-result! "clojure.core/distinct?" "(distinct? nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil true)" nil e)))
    (try
      (let [result (distinct? true nil)]
        (record-result! "clojure.core/distinct?" "(distinct? true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? true nil)" nil e)))
    (try
      (let [result (distinct? nil nil)]
        (record-result! "clojure.core/distinct?" "(distinct? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil nil)" nil e)))
    (try
      (let [result (distinct? nil nil nil)]
        (record-result! "clojure.core/distinct?" "(distinct? nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil nil nil)" nil e)))
    (try
      (let [result (distinct? nil nil nil nil)]
        (record-result! "clojure.core/distinct?" "(distinct? nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/distinct?" "(distinct? nil nil nil nil)" nil e)))))


(deftest test-clojure-core-doall
  (testing "clojure.core/doall"
    ;; Arities: [[coll] [n coll]]
    (try
      (let [result (doall nil)]
        (record-result! "clojure.core/doall" "(doall nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doall" "(doall nil)" nil e)))
    (try
      (let [result (doall [])]
        (record-result! "clojure.core/doall" "(doall [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doall" "(doall [])" nil e)))
    (try
      (let [result (doall 0 nil)]
        (record-result! "clojure.core/doall" "(doall 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doall" "(doall 0 nil)" nil e)))
    (try
      (let [result (doall 0 [])]
        (record-result! "clojure.core/doall" "(doall 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doall" "(doall 0 [])" nil e)))
    (try
      (let [result (doall 1 nil)]
        (record-result! "clojure.core/doall" "(doall 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doall" "(doall 1 nil)" nil e)))))


(deftest test-clojure-core-dorun
  (testing "clojure.core/dorun"
    ;; Arities: [[coll] [n coll]]
    (try
      (let [result (dorun nil)]
        (record-result! "clojure.core/dorun" "(dorun nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dorun" "(dorun nil)" nil e)))
    (try
      (let [result (dorun [])]
        (record-result! "clojure.core/dorun" "(dorun [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dorun" "(dorun [])" nil e)))
    (try
      (let [result (dorun 0 nil)]
        (record-result! "clojure.core/dorun" "(dorun 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dorun" "(dorun 0 nil)" nil e)))
    (try
      (let [result (dorun 0 [])]
        (record-result! "clojure.core/dorun" "(dorun 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dorun" "(dorun 0 [])" nil e)))
    (try
      (let [result (dorun 1 nil)]
        (record-result! "clojure.core/dorun" "(dorun 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dorun" "(dorun 1 nil)" nil e)))))


(deftest test-clojure-core-doseq
  (testing "clojure.core/doseq"
    ;; Arities: [[seq-exprs & body]]
    (try
      (let [result (doseq nil)]
        (record-result! "clojure.core/doseq" "(doseq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doseq" "(doseq nil)" nil e)))
    (try
      (let [result (doseq nil nil)]
        (record-result! "clojure.core/doseq" "(doseq nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doseq" "(doseq nil nil)" nil e)))
    (try
      (let [result (doseq nil nil nil)]
        (record-result! "clojure.core/doseq" "(doseq nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doseq" "(doseq nil nil nil)" nil e)))))


(deftest test-clojure-core-dosync
  (testing "clojure.core/dosync"
    ;; Arities: [[& exprs]]
    (try
      (let [result (dosync)]
        (record-result! "clojure.core/dosync" "(dosync)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dosync" "(dosync)" nil e)))))


(deftest test-clojure-core-dotimes
  (testing "clojure.core/dotimes"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (dotimes nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil)" nil e)))
    (try
      (let [result (dotimes nil nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil nil)" nil e)))
    (try
      (let [result (dotimes nil nil nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil nil nil)" nil e)))))


(deftest test-clojure-core-dotimes
  (testing "clojure.core/dotimes"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (dotimes nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil)" nil e)))
    (try
      (let [result (dotimes nil nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil nil)" nil e)))
    (try
      (let [result (dotimes nil nil nil)]
        (record-result! "clojure.core/dotimes" "(dotimes nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/dotimes" "(dotimes nil nil nil)" nil e)))))


(deftest test-clojure-core-doto
  (testing "clojure.core/doto"
    ;; Arities: [[x & forms]]
    (try
      (let [result (doto nil)]
        (record-result! "clojure.core/doto" "(doto nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doto" "(doto nil)" nil e)))
    (try
      (let [result (doto nil nil)]
        (record-result! "clojure.core/doto" "(doto nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doto" "(doto nil nil)" nil e)))
    (try
      (let [result (doto nil nil nil)]
        (record-result! "clojure.core/doto" "(doto nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/doto" "(doto nil nil nil)" nil e)))))


(deftest test-clojure-core-double
  (testing "clojure.core/double"
    ;; Arities: [[x]]
    (try
      (let [result (double nil)]
        (record-result! "clojure.core/double" "(double nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double" "(double nil)" nil e)))
    (try
      (let [result (double true)]
        (record-result! "clojure.core/double" "(double true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double" "(double true)" nil e)))))


(deftest test-clojure-core-double-array
  (testing "clojure.core/double-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (double-array nil)]
        (record-result! "clojure.core/double-array" "(double-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double-array" "(double-array nil)" nil e)))
    (try
      (let [result (double-array true)]
        (record-result! "clojure.core/double-array" "(double-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double-array" "(double-array true)" nil e)))
    (try
      (let [result (double-array nil nil)]
        (record-result! "clojure.core/double-array" "(double-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double-array" "(double-array nil nil)" nil e)))
    (try
      (let [result (double-array nil true)]
        (record-result! "clojure.core/double-array" "(double-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double-array" "(double-array nil true)" nil e)))
    (try
      (let [result (double-array true nil)]
        (record-result! "clojure.core/double-array" "(double-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double-array" "(double-array true nil)" nil e)))))


(deftest test-clojure-core-double-p
  (testing "clojure.core/double?"
    ;; Arities: [[x]]
    (try
      (let [result (double? nil)]
        (record-result! "clojure.core/double?" "(double? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double?" "(double? nil)" nil e)))
    (try
      (let [result (double? true)]
        (record-result! "clojure.core/double?" "(double? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/double?" "(double? true)" nil e)))))


(deftest test-clojure-core-drop
  (testing "clojure.core/drop"
    ;; Arities: [[n] [n coll]]
    (try
      (let [result (drop 0)]
        (record-result! "clojure.core/drop" "(drop 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop" "(drop 0)" nil e)))
    (try
      (let [result (drop 1)]
        (record-result! "clojure.core/drop" "(drop 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop" "(drop 1)" nil e)))
    (try
      (let [result (drop 0 nil)]
        (record-result! "clojure.core/drop" "(drop 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop" "(drop 0 nil)" nil e)))
    (try
      (let [result (drop 0 [])]
        (record-result! "clojure.core/drop" "(drop 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop" "(drop 0 [])" nil e)))
    (try
      (let [result (drop 1 nil)]
        (record-result! "clojure.core/drop" "(drop 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop" "(drop 1 nil)" nil e)))))


(deftest test-clojure-core-drop-last
  (testing "clojure.core/drop-last"
    ;; Arities: [[coll] [n coll]]
    (try
      (let [result (drop-last nil)]
        (record-result! "clojure.core/drop-last" "(drop-last nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-last" "(drop-last nil)" nil e)))
    (try
      (let [result (drop-last [])]
        (record-result! "clojure.core/drop-last" "(drop-last [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-last" "(drop-last [])" nil e)))
    (try
      (let [result (drop-last 0 nil)]
        (record-result! "clojure.core/drop-last" "(drop-last 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-last" "(drop-last 0 nil)" nil e)))
    (try
      (let [result (drop-last 0 [])]
        (record-result! "clojure.core/drop-last" "(drop-last 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-last" "(drop-last 0 [])" nil e)))
    (try
      (let [result (drop-last 1 nil)]
        (record-result! "clojure.core/drop-last" "(drop-last 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-last" "(drop-last 1 nil)" nil e)))))


(deftest test-clojure-core-drop-while
  (testing "clojure.core/drop-while"
    ;; Arities: [[pred] [pred coll]]
    (try
      (let [result (drop-while 'even?)]
        (record-result! "clojure.core/drop-while" "(drop-while 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-while" "(drop-while 'even?)" nil e)))
    (try
      (let [result (drop-while 'odd?)]
        (record-result! "clojure.core/drop-while" "(drop-while 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-while" "(drop-while 'odd?)" nil e)))
    (try
      (let [result (drop-while 'even? nil)]
        (record-result! "clojure.core/drop-while" "(drop-while 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-while" "(drop-while 'even? nil)" nil e)))
    (try
      (let [result (drop-while 'even? [])]
        (record-result! "clojure.core/drop-while" "(drop-while 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-while" "(drop-while 'even? [])" nil e)))
    (try
      (let [result (drop-while 'odd? nil)]
        (record-result! "clojure.core/drop-while" "(drop-while 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/drop-while" "(drop-while 'odd? nil)" nil e)))))


(deftest test-clojure-core-eduction
  (testing "clojure.core/eduction"
    ;; Arities: [quote ([xform* coll])]
    (try
      (let [result (eduction)]
        (record-result! "clojure.core/eduction" "(eduction)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/eduction" "(eduction)" nil e)))
    (try
      (let [result (eduction)]
        (record-result! "clojure.core/eduction" "(eduction)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/eduction" "(eduction)" nil e)))))


(deftest test-clojure-core-empty
  (testing "clojure.core/empty"
    ;; Arities: [[coll]]
    (try
      (let [result (empty nil)]
        (record-result! "clojure.core/empty" "(empty nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/empty" "(empty nil)" nil e)))
    (try
      (let [result (empty [])]
        (record-result! "clojure.core/empty" "(empty [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/empty" "(empty [])" nil e)))))


(deftest test-clojure-core-empty-p
  (testing "clojure.core/empty?"
    ;; Arities: [[coll]]
    (try
      (let [result (empty? nil)]
        (record-result! "clojure.core/empty?" "(empty? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/empty?" "(empty? nil)" nil e)))
    (try
      (let [result (empty? [])]
        (record-result! "clojure.core/empty?" "(empty? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/empty?" "(empty? [])" nil e)))))


(deftest test-clojure-core-ensure
  (testing "clojure.core/ensure"
    ;; Arities: [[ref]]
    (try
      (let [result (ensure nil)]
        (record-result! "clojure.core/ensure" "(ensure nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ensure" "(ensure nil)" nil e)))
    (try
      (let [result (ensure true)]
        (record-result! "clojure.core/ensure" "(ensure true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ensure" "(ensure true)" nil e)))))


(deftest test-clojure-core-ensure-reduced
  (testing "clojure.core/ensure-reduced"
    ;; Arities: [[x]]
    (try
      (let [result (ensure-reduced nil)]
        (record-result! "clojure.core/ensure-reduced" "(ensure-reduced nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ensure-reduced" "(ensure-reduced nil)" nil e)))
    (try
      (let [result (ensure-reduced true)]
        (record-result! "clojure.core/ensure-reduced" "(ensure-reduced true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ensure-reduced" "(ensure-reduced true)" nil e)))))


(deftest test-clojure-core-enumeration-seq
  (testing "clojure.core/enumeration-seq"
    ;; Arities: [[e]]
    (try
      (let [result (enumeration-seq nil)]
        (record-result! "clojure.core/enumeration-seq" "(enumeration-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/enumeration-seq" "(enumeration-seq nil)" nil e)))
    (try
      (let [result (enumeration-seq true)]
        (record-result! "clojure.core/enumeration-seq" "(enumeration-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/enumeration-seq" "(enumeration-seq true)" nil e)))))


(deftest test-clojure-core-error-handler
  (testing "clojure.core/error-handler"
    ;; Arities: [[a]]
    (try
      (let [result (error-handler nil)]
        (record-result! "clojure.core/error-handler" "(error-handler nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/error-handler" "(error-handler nil)" nil e)))
    (try
      (let [result (error-handler true)]
        (record-result! "clojure.core/error-handler" "(error-handler true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/error-handler" "(error-handler true)" nil e)))))


(deftest test-clojure-core-error-mode
  (testing "clojure.core/error-mode"
    ;; Arities: [[a]]
    (try
      (let [result (error-mode nil)]
        (record-result! "clojure.core/error-mode" "(error-mode nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/error-mode" "(error-mode nil)" nil e)))
    (try
      (let [result (error-mode true)]
        (record-result! "clojure.core/error-mode" "(error-mode true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/error-mode" "(error-mode true)" nil e)))))


(deftest test-clojure-core-eval
  (testing "clojure.core/eval"
    ;; Arities: [[form]]
    (try
      (let [result (eval nil)]
        (record-result! "clojure.core/eval" "(eval nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/eval" "(eval nil)" nil e)))
    (try
      (let [result (eval true)]
        (record-result! "clojure.core/eval" "(eval true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/eval" "(eval true)" nil e)))))


(deftest test-clojure-core-even-p
  (testing "clojure.core/even?"
    ;; Arities: [[n]]
    (try
      (let [result (even? 0)]
        (record-result! "clojure.core/even?" "(even? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/even?" "(even? 0)" nil e)))
    (try
      (let [result (even? 1)]
        (record-result! "clojure.core/even?" "(even? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/even?" "(even? 1)" nil e)))))


(deftest test-clojure-core-every-pred
  (testing "clojure.core/every-pred"
    ;; Arities: [[p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]]
    (try
      (let [result (every-pred 'even?)]
        (record-result! "clojure.core/every-pred" "(every-pred 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred 'even?)" nil e)))
    (try
      (let [result (every-pred 'odd?)]
        (record-result! "clojure.core/every-pred" "(every-pred 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred 'odd?)" nil e)))
    (try
      (let [result (every-pred nil nil)]
        (record-result! "clojure.core/every-pred" "(every-pred nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred nil nil)" nil e)))
    (try
      (let [result (every-pred nil true)]
        (record-result! "clojure.core/every-pred" "(every-pred nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred nil true)" nil e)))
    (try
      (let [result (every-pred true nil)]
        (record-result! "clojure.core/every-pred" "(every-pred true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred true nil)" nil e)))
    (try
      (let [result (every-pred nil nil nil)]
        (record-result! "clojure.core/every-pred" "(every-pred nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred nil nil nil)" nil e)))
    (try
      (let [result (every-pred nil nil true)]
        (record-result! "clojure.core/every-pred" "(every-pred nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred nil nil true)" nil e)))
    (try
      (let [result (every-pred nil true nil)]
        (record-result! "clojure.core/every-pred" "(every-pred nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every-pred" "(every-pred nil true nil)" nil e)))))


(deftest test-clojure-core-every-p
  (testing "clojure.core/every?"
    ;; Arities: [[pred coll]]
    (try
      (let [result (every? 'even? nil)]
        (record-result! "clojure.core/every?" "(every? 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every?" "(every? 'even? nil)" nil e)))
    (try
      (let [result (every? 'even? [])]
        (record-result! "clojure.core/every?" "(every? 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every?" "(every? 'even? [])" nil e)))
    (try
      (let [result (every? 'odd? nil)]
        (record-result! "clojure.core/every?" "(every? 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/every?" "(every? 'odd? nil)" nil e)))))


(deftest test-clojure-core-ex-cause
  (testing "clojure.core/ex-cause"
    ;; Arities: [[ex]]
    (try
      (let [result (ex-cause nil)]
        (record-result! "clojure.core/ex-cause" "(ex-cause nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-cause" "(ex-cause nil)" nil e)))
    (try
      (let [result (ex-cause true)]
        (record-result! "clojure.core/ex-cause" "(ex-cause true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-cause" "(ex-cause true)" nil e)))))


(deftest test-clojure-core-ex-data
  (testing "clojure.core/ex-data"
    ;; Arities: [[ex]]
    (try
      (let [result (ex-data nil)]
        (record-result! "clojure.core/ex-data" "(ex-data nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-data" "(ex-data nil)" nil e)))
    (try
      (let [result (ex-data true)]
        (record-result! "clojure.core/ex-data" "(ex-data true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-data" "(ex-data true)" nil e)))))


(deftest test-clojure-core-ex-info
  (testing "clojure.core/ex-info"
    ;; Arities: [[msg map] [msg map cause]]
    (try
      (let [result (ex-info nil {})]
        (record-result! "clojure.core/ex-info" "(ex-info nil {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info nil {})" nil e)))
    (try
      (let [result (ex-info nil {:a 1})]
        (record-result! "clojure.core/ex-info" "(ex-info nil {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info nil {:a 1})" nil e)))
    (try
      (let [result (ex-info true {})]
        (record-result! "clojure.core/ex-info" "(ex-info true {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info true {})" nil e)))
    (try
      (let [result (ex-info nil {} nil)]
        (record-result! "clojure.core/ex-info" "(ex-info nil {} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info nil {} nil)" nil e)))
    (try
      (let [result (ex-info nil {} true)]
        (record-result! "clojure.core/ex-info" "(ex-info nil {} true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info nil {} true)" nil e)))
    (try
      (let [result (ex-info nil {:a 1} nil)]
        (record-result! "clojure.core/ex-info" "(ex-info nil {:a 1} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-info" "(ex-info nil {:a 1} nil)" nil e)))))


(deftest test-clojure-core-ex-message
  (testing "clojure.core/ex-message"
    ;; Arities: [[ex]]
    (try
      (let [result (ex-message nil)]
        (record-result! "clojure.core/ex-message" "(ex-message nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-message" "(ex-message nil)" nil e)))
    (try
      (let [result (ex-message true)]
        (record-result! "clojure.core/ex-message" "(ex-message true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ex-message" "(ex-message true)" nil e)))))


(deftest test-clojure-core-false-p
  (testing "clojure.core/false?"
    ;; Arities: [[x]]
    (try
      (let [result (false? nil)]
        (record-result! "clojure.core/false?" "(false? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/false?" "(false? nil)" nil e)))
    (try
      (let [result (false? true)]
        (record-result! "clojure.core/false?" "(false? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/false?" "(false? true)" nil e)))))


(deftest test-clojure-core-ffirst
  (testing "clojure.core/ffirst"
    ;; Arities: [quote ([x])]
    (try
      (let [result (ffirst)]
        (record-result! "clojure.core/ffirst" "(ffirst)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ffirst" "(ffirst)" nil e)))
    (try
      (let [result (ffirst)]
        (record-result! "clojure.core/ffirst" "(ffirst)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ffirst" "(ffirst)" nil e)))))


(deftest test-clojure-core-file-seq
  (testing "clojure.core/file-seq"
    ;; Arities: [[dir]]
    (try
      (let [result (file-seq nil)]
        (record-result! "clojure.core/file-seq" "(file-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/file-seq" "(file-seq nil)" nil e)))
    (try
      (let [result (file-seq true)]
        (record-result! "clojure.core/file-seq" "(file-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/file-seq" "(file-seq true)" nil e)))))


(deftest test-clojure-core-filter
  (testing "clojure.core/filter"
    ;; Arities: [[pred] [pred coll]]
    (try
      (let [result (filter 'even?)]
        (record-result! "clojure.core/filter" "(filter 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filter" "(filter 'even?)" nil e)))
    (try
      (let [result (filter 'odd?)]
        (record-result! "clojure.core/filter" "(filter 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filter" "(filter 'odd?)" nil e)))
    (try
      (let [result (filter 'even? nil)]
        (record-result! "clojure.core/filter" "(filter 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filter" "(filter 'even? nil)" nil e)))
    (try
      (let [result (filter 'even? [])]
        (record-result! "clojure.core/filter" "(filter 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filter" "(filter 'even? [])" nil e)))
    (try
      (let [result (filter 'odd? nil)]
        (record-result! "clojure.core/filter" "(filter 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filter" "(filter 'odd? nil)" nil e)))))


(deftest test-clojure-core-filterv
  (testing "clojure.core/filterv"
    ;; Arities: [[pred coll]]
    (try
      (let [result (filterv 'even? nil)]
        (record-result! "clojure.core/filterv" "(filterv 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filterv" "(filterv 'even? nil)" nil e)))
    (try
      (let [result (filterv 'even? [])]
        (record-result! "clojure.core/filterv" "(filterv 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filterv" "(filterv 'even? [])" nil e)))
    (try
      (let [result (filterv 'odd? nil)]
        (record-result! "clojure.core/filterv" "(filterv 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/filterv" "(filterv 'odd? nil)" nil e)))))


(deftest test-clojure-core-find
  (testing "clojure.core/find"
    ;; Arities: [[map key]]
    (try
      (let [result (find {} :a)]
        (record-result! "clojure.core/find" "(find {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find" "(find {} :a)" nil e)))
    (try
      (let [result (find {} :b)]
        (record-result! "clojure.core/find" "(find {} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find" "(find {} :b)" nil e)))
    (try
      (let [result (find {:a 1} :a)]
        (record-result! "clojure.core/find" "(find {:a 1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find" "(find {:a 1} :a)" nil e)))))


(deftest test-clojure-core-find-keyword
  (testing "clojure.core/find-keyword"
    ;; Arities: [[name] [ns name]]
    (try
      (let [result (find-keyword nil)]
        (record-result! "clojure.core/find-keyword" "(find-keyword nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-keyword" "(find-keyword nil)" nil e)))
    (try
      (let [result (find-keyword true)]
        (record-result! "clojure.core/find-keyword" "(find-keyword true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-keyword" "(find-keyword true)" nil e)))
    (try
      (let [result (find-keyword nil nil)]
        (record-result! "clojure.core/find-keyword" "(find-keyword nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-keyword" "(find-keyword nil nil)" nil e)))
    (try
      (let [result (find-keyword nil true)]
        (record-result! "clojure.core/find-keyword" "(find-keyword nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-keyword" "(find-keyword nil true)" nil e)))
    (try
      (let [result (find-keyword true nil)]
        (record-result! "clojure.core/find-keyword" "(find-keyword true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-keyword" "(find-keyword true nil)" nil e)))))


(deftest test-clojure-core-find-ns
  (testing "clojure.core/find-ns"
    ;; Arities: [[sym]]
    (try
      (let [result (find-ns nil)]
        (record-result! "clojure.core/find-ns" "(find-ns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-ns" "(find-ns nil)" nil e)))
    (try
      (let [result (find-ns true)]
        (record-result! "clojure.core/find-ns" "(find-ns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-ns" "(find-ns true)" nil e)))))


(deftest test-clojure-core-find-var
  (testing "clojure.core/find-var"
    ;; Arities: [[sym]]
    (try
      (let [result (find-var nil)]
        (record-result! "clojure.core/find-var" "(find-var nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-var" "(find-var nil)" nil e)))
    (try
      (let [result (find-var true)]
        (record-result! "clojure.core/find-var" "(find-var true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/find-var" "(find-var true)" nil e)))))


(deftest test-clojure-core-first
  (testing "clojure.core/first"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (first)]
        (record-result! "clojure.core/first" "(first)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/first" "(first)" nil e)))
    (try
      (let [result (first)]
        (record-result! "clojure.core/first" "(first)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/first" "(first)" nil e)))))


(deftest test-clojure-core-flatten
  (testing "clojure.core/flatten"
    ;; Arities: [[x]]
    (try
      (let [result (flatten nil)]
        (record-result! "clojure.core/flatten" "(flatten nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/flatten" "(flatten nil)" nil e)))
    (try
      (let [result (flatten true)]
        (record-result! "clojure.core/flatten" "(flatten true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/flatten" "(flatten true)" nil e)))))


(deftest test-clojure-core-float
  (testing "clojure.core/float"
    ;; Arities: [[x]]
    (try
      (let [result (float nil)]
        (record-result! "clojure.core/float" "(float nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float" "(float nil)" nil e)))
    (try
      (let [result (float true)]
        (record-result! "clojure.core/float" "(float true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float" "(float true)" nil e)))))


(deftest test-clojure-core-float-array
  (testing "clojure.core/float-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (float-array nil)]
        (record-result! "clojure.core/float-array" "(float-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float-array" "(float-array nil)" nil e)))
    (try
      (let [result (float-array true)]
        (record-result! "clojure.core/float-array" "(float-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float-array" "(float-array true)" nil e)))
    (try
      (let [result (float-array nil nil)]
        (record-result! "clojure.core/float-array" "(float-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float-array" "(float-array nil nil)" nil e)))
    (try
      (let [result (float-array nil true)]
        (record-result! "clojure.core/float-array" "(float-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float-array" "(float-array nil true)" nil e)))
    (try
      (let [result (float-array true nil)]
        (record-result! "clojure.core/float-array" "(float-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float-array" "(float-array true nil)" nil e)))))


(deftest test-clojure-core-float-p
  (testing "clojure.core/float?"
    ;; Arities: [[n]]
    (try
      (let [result (float? 0)]
        (record-result! "clojure.core/float?" "(float? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float?" "(float? 0)" nil e)))
    (try
      (let [result (float? 1)]
        (record-result! "clojure.core/float?" "(float? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/float?" "(float? 1)" nil e)))))


(deftest test-clojure-core-flush
  (testing "clojure.core/flush"
    ;; Arities: [[]]
    (try
      (let [result (flush)]
        (record-result! "clojure.core/flush" "(flush)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/flush" "(flush)" nil e)))))


(deftest test-clojure-core-fn
  (testing "clojure.core/fn"
    ;; Arities: [fn*]
    (try
      (let [result (fn)]
        (record-result! "clojure.core/fn" "(fn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fn" "(fn)" nil e)))))


(deftest test-clojure-core-fn
  (testing "clojure.core/fn"
    ;; Arities: [[& sigs]]
    (try
      (let [result (fn)]
        (record-result! "clojure.core/fn" "(fn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fn" "(fn)" nil e)))))


(deftest test-clojure-core-fn-p
  (testing "clojure.core/fn?"
    ;; Arities: [[x]]
    (try
      (let [result (fn? nil)]
        (record-result! "clojure.core/fn?" "(fn? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fn?" "(fn? nil)" nil e)))
    (try
      (let [result (fn? true)]
        (record-result! "clojure.core/fn?" "(fn? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fn?" "(fn? true)" nil e)))))


(deftest test-clojure-core-fnext
  (testing "clojure.core/fnext"
    ;; Arities: [quote ([x])]
    (try
      (let [result (fnext)]
        (record-result! "clojure.core/fnext" "(fnext)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnext" "(fnext)" nil e)))
    (try
      (let [result (fnext)]
        (record-result! "clojure.core/fnext" "(fnext)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnext" "(fnext)" nil e)))))


(deftest test-clojure-core-fnil
  (testing "clojure.core/fnil"
    ;; Arities: [[f x] [f x y] [f x y z]]
    (try
      (let [result (fnil 'inc nil)]
        (record-result! "clojure.core/fnil" "(fnil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc nil)" nil e)))
    (try
      (let [result (fnil 'inc true)]
        (record-result! "clojure.core/fnil" "(fnil 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc true)" nil e)))
    (try
      (let [result (fnil 'dec nil)]
        (record-result! "clojure.core/fnil" "(fnil 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'dec nil)" nil e)))
    (try
      (let [result (fnil 'inc nil nil)]
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil)" nil e)))
    (try
      (let [result (fnil 'inc nil true)]
        (record-result! "clojure.core/fnil" "(fnil 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc nil true)" nil e)))
    (try
      (let [result (fnil 'inc true nil)]
        (record-result! "clojure.core/fnil" "(fnil 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc true nil)" nil e)))
    (try
      (let [result (fnil 'inc nil nil nil)]
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil nil)" nil e)))
    (try
      (let [result (fnil 'inc nil nil true)]
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/fnil" "(fnil 'inc nil nil true)" nil e)))))


(deftest test-clojure-core-for
  (testing "clojure.core/for"
    ;; Arities: [[seq-exprs body-expr]]
    (try
      (let [result (for nil nil)]
        (record-result! "clojure.core/for" "(for nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/for" "(for nil nil)" nil e)))
    (try
      (let [result (for nil true)]
        (record-result! "clojure.core/for" "(for nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/for" "(for nil true)" nil e)))
    (try
      (let [result (for true nil)]
        (record-result! "clojure.core/for" "(for true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/for" "(for true nil)" nil e)))))


(deftest test-clojure-core-force
  (testing "clojure.core/force"
    ;; Arities: [[x]]
    (try
      (let [result (force nil)]
        (record-result! "clojure.core/force" "(force nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/force" "(force nil)" nil e)))
    (try
      (let [result (force true)]
        (record-result! "clojure.core/force" "(force true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/force" "(force true)" nil e)))))


(deftest test-clojure-core-format
  (testing "clojure.core/format"
    ;; Arities: [[fmt & args]]
    (try
      (let [result (format nil)]
        (record-result! "clojure.core/format" "(format nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/format" "(format nil)" nil e)))
    (try
      (let [result (format nil nil)]
        (record-result! "clojure.core/format" "(format nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/format" "(format nil nil)" nil e)))
    (try
      (let [result (format nil nil nil)]
        (record-result! "clojure.core/format" "(format nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/format" "(format nil nil nil)" nil e)))))


(deftest test-clojure-core-frequencies
  (testing "clojure.core/frequencies"
    ;; Arities: [[coll]]
    (try
      (let [result (frequencies nil)]
        (record-result! "clojure.core/frequencies" "(frequencies nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/frequencies" "(frequencies nil)" nil e)))
    (try
      (let [result (frequencies [])]
        (record-result! "clojure.core/frequencies" "(frequencies [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/frequencies" "(frequencies [])" nil e)))))


(deftest test-clojure-core-future
  (testing "clojure.core/future"
    ;; Arities: [[& body]]
    (try
      (let [result (future)]
        (record-result! "clojure.core/future" "(future)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future" "(future)" nil e)))))


(deftest test-clojure-core-future-call
  (testing "clojure.core/future-call"
    ;; Arities: [[f]]
    (try
      (let [result (future-call 'inc)]
        (record-result! "clojure.core/future-call" "(future-call 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-call" "(future-call 'inc)" nil e)))
    (try
      (let [result (future-call 'dec)]
        (record-result! "clojure.core/future-call" "(future-call 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-call" "(future-call 'dec)" nil e)))))


(deftest test-clojure-core-future-cancel
  (testing "clojure.core/future-cancel"
    ;; Arities: [[f]]
    (try
      (let [result (future-cancel 'inc)]
        (record-result! "clojure.core/future-cancel" "(future-cancel 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-cancel" "(future-cancel 'inc)" nil e)))
    (try
      (let [result (future-cancel 'dec)]
        (record-result! "clojure.core/future-cancel" "(future-cancel 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-cancel" "(future-cancel 'dec)" nil e)))))


(deftest test-clojure-core-future-cancelled-p
  (testing "clojure.core/future-cancelled?"
    ;; Arities: [[f]]
    (try
      (let [result (future-cancelled? 'inc)]
        (record-result! "clojure.core/future-cancelled?" "(future-cancelled? 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-cancelled?" "(future-cancelled? 'inc)" nil e)))
    (try
      (let [result (future-cancelled? 'dec)]
        (record-result! "clojure.core/future-cancelled?" "(future-cancelled? 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-cancelled?" "(future-cancelled? 'dec)" nil e)))))


(deftest test-clojure-core-future-done-p
  (testing "clojure.core/future-done?"
    ;; Arities: [[f]]
    (try
      (let [result (future-done? 'inc)]
        (record-result! "clojure.core/future-done?" "(future-done? 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-done?" "(future-done? 'inc)" nil e)))
    (try
      (let [result (future-done? 'dec)]
        (record-result! "clojure.core/future-done?" "(future-done? 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future-done?" "(future-done? 'dec)" nil e)))))


(deftest test-clojure-core-future-p
  (testing "clojure.core/future?"
    ;; Arities: [[x]]
    (try
      (let [result (future? nil)]
        (record-result! "clojure.core/future?" "(future? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future?" "(future? nil)" nil e)))
    (try
      (let [result (future? true)]
        (record-result! "clojure.core/future?" "(future? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/future?" "(future? true)" nil e)))))


(deftest test-clojure-core-gensym
  (testing "clojure.core/gensym"
    ;; Arities: [[] [prefix-string]]
    (try
      (let [result (gensym)]
        (record-result! "clojure.core/gensym" "(gensym)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/gensym" "(gensym)" nil e)))
    (try
      (let [result (gensym nil)]
        (record-result! "clojure.core/gensym" "(gensym nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/gensym" "(gensym nil)" nil e)))
    (try
      (let [result (gensym true)]
        (record-result! "clojure.core/gensym" "(gensym true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/gensym" "(gensym true)" nil e)))))


(deftest test-clojure-core-get
  (testing "clojure.core/get"
    ;; Arities: [[map key] [map key not-found]]
    (try
      (let [result (get {} :a)]
        (record-result! "clojure.core/get" "(get {} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {} :a)" nil e)))
    (try
      (let [result (get {} :b)]
        (record-result! "clojure.core/get" "(get {} :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {} :b)" nil e)))
    (try
      (let [result (get {:a 1} :a)]
        (record-result! "clojure.core/get" "(get {:a 1} :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {:a 1} :a)" nil e)))
    (try
      (let [result (get {} :a nil)]
        (record-result! "clojure.core/get" "(get {} :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {} :a nil)" nil e)))
    (try
      (let [result (get {} :a true)]
        (record-result! "clojure.core/get" "(get {} :a true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {} :a true)" nil e)))
    (try
      (let [result (get {} :b nil)]
        (record-result! "clojure.core/get" "(get {} :b nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get" "(get {} :b nil)" nil e)))))


(deftest test-clojure-core-get-in
  (testing "clojure.core/get-in"
    ;; Arities: [[m ks] [m ks not-found]]
    (try
      (let [result (get-in {} nil)]
        (record-result! "clojure.core/get-in" "(get-in {} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {} nil)" nil e)))
    (try
      (let [result (get-in {} true)]
        (record-result! "clojure.core/get-in" "(get-in {} true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {} true)" nil e)))
    (try
      (let [result (get-in {:a 1} nil)]
        (record-result! "clojure.core/get-in" "(get-in {:a 1} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {:a 1} nil)" nil e)))
    (try
      (let [result (get-in {} nil nil)]
        (record-result! "clojure.core/get-in" "(get-in {} nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {} nil nil)" nil e)))
    (try
      (let [result (get-in {} nil true)]
        (record-result! "clojure.core/get-in" "(get-in {} nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {} nil true)" nil e)))
    (try
      (let [result (get-in {} true nil)]
        (record-result! "clojure.core/get-in" "(get-in {} true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-in" "(get-in {} true nil)" nil e)))))


(deftest test-clojure-core-get-method
  (testing "clojure.core/get-method"
    ;; Arities: [[multifn dispatch-val]]
    (try
      (let [result (get-method nil nil)]
        (record-result! "clojure.core/get-method" "(get-method nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-method" "(get-method nil nil)" nil e)))
    (try
      (let [result (get-method nil true)]
        (record-result! "clojure.core/get-method" "(get-method nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-method" "(get-method nil true)" nil e)))
    (try
      (let [result (get-method true nil)]
        (record-result! "clojure.core/get-method" "(get-method true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-method" "(get-method true nil)" nil e)))))


(deftest test-clojure-core-get-thread-bindings
  (testing "clojure.core/get-thread-bindings"
    ;; Arities: [[]]
    (try
      (let [result (get-thread-bindings)]
        (record-result! "clojure.core/get-thread-bindings" "(get-thread-bindings)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-thread-bindings" "(get-thread-bindings)" nil e)))))


(deftest test-clojure-core-get-validator
  (testing "clojure.core/get-validator"
    ;; Arities: [[iref]]
    (try
      (let [result (get-validator nil)]
        (record-result! "clojure.core/get-validator" "(get-validator nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-validator" "(get-validator nil)" nil e)))
    (try
      (let [result (get-validator true)]
        (record-result! "clojure.core/get-validator" "(get-validator true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/get-validator" "(get-validator true)" nil e)))))


(deftest test-clojure-core-group-by
  (testing "clojure.core/group-by"
    ;; Arities: [[f coll]]
    (try
      (let [result (group-by 'inc nil)]
        (record-result! "clojure.core/group-by" "(group-by 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/group-by" "(group-by 'inc nil)" nil e)))
    (try
      (let [result (group-by 'inc [])]
        (record-result! "clojure.core/group-by" "(group-by 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/group-by" "(group-by 'inc [])" nil e)))
    (try
      (let [result (group-by 'dec nil)]
        (record-result! "clojure.core/group-by" "(group-by 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/group-by" "(group-by 'dec nil)" nil e)))))


(deftest test-clojure-core-halt-when
  (testing "clojure.core/halt-when"
    ;; Arities: [[pred] [pred retf]]
    (try
      (let [result (halt-when 'even?)]
        (record-result! "clojure.core/halt-when" "(halt-when 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/halt-when" "(halt-when 'even?)" nil e)))
    (try
      (let [result (halt-when 'odd?)]
        (record-result! "clojure.core/halt-when" "(halt-when 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/halt-when" "(halt-when 'odd?)" nil e)))
    (try
      (let [result (halt-when 'even? nil)]
        (record-result! "clojure.core/halt-when" "(halt-when 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/halt-when" "(halt-when 'even? nil)" nil e)))
    (try
      (let [result (halt-when 'even? true)]
        (record-result! "clojure.core/halt-when" "(halt-when 'even? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/halt-when" "(halt-when 'even? true)" nil e)))
    (try
      (let [result (halt-when 'odd? nil)]
        (record-result! "clojure.core/halt-when" "(halt-when 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/halt-when" "(halt-when 'odd? nil)" nil e)))))


(deftest test-clojure-core-hash
  (testing "clojure.core/hash"
    ;; Arities: [[x]]
    (try
      (let [result (hash nil)]
        (record-result! "clojure.core/hash" "(hash nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash" "(hash nil)" nil e)))
    (try
      (let [result (hash true)]
        (record-result! "clojure.core/hash" "(hash true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash" "(hash true)" nil e)))))


(deftest test-clojure-core-hash-map
  (testing "clojure.core/hash-map"
    ;; Arities: [[] [& keyvals]]
    (try
      (let [result (hash-map)]
        (record-result! "clojure.core/hash-map" "(hash-map)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-map" "(hash-map)" nil e)))
    (try
      (let [result (hash-map)]
        (record-result! "clojure.core/hash-map" "(hash-map)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-map" "(hash-map)" nil e)))))


(deftest test-clojure-core-hash-ordered-coll
  (testing "clojure.core/hash-ordered-coll"
    ;; Arities: [[coll]]
    (try
      (let [result (hash-ordered-coll nil)]
        (record-result! "clojure.core/hash-ordered-coll" "(hash-ordered-coll nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-ordered-coll" "(hash-ordered-coll nil)" nil e)))
    (try
      (let [result (hash-ordered-coll [])]
        (record-result! "clojure.core/hash-ordered-coll" "(hash-ordered-coll [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-ordered-coll" "(hash-ordered-coll [])" nil e)))))


(deftest test-clojure-core-hash-set
  (testing "clojure.core/hash-set"
    ;; Arities: [[] [& keys]]
    (try
      (let [result (hash-set)]
        (record-result! "clojure.core/hash-set" "(hash-set)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-set" "(hash-set)" nil e)))
    (try
      (let [result (hash-set)]
        (record-result! "clojure.core/hash-set" "(hash-set)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-set" "(hash-set)" nil e)))))


(deftest test-clojure-core-hash-unordered-coll
  (testing "clojure.core/hash-unordered-coll"
    ;; Arities: [[coll]]
    (try
      (let [result (hash-unordered-coll nil)]
        (record-result! "clojure.core/hash-unordered-coll" "(hash-unordered-coll nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-unordered-coll" "(hash-unordered-coll nil)" nil e)))
    (try
      (let [result (hash-unordered-coll [])]
        (record-result! "clojure.core/hash-unordered-coll" "(hash-unordered-coll [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/hash-unordered-coll" "(hash-unordered-coll [])" nil e)))))


(deftest test-clojure-core-ident-p
  (testing "clojure.core/ident?"
    ;; Arities: [[x]]
    (try
      (let [result (ident? nil)]
        (record-result! "clojure.core/ident?" "(ident? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ident?" "(ident? nil)" nil e)))
    (try
      (let [result (ident? true)]
        (record-result! "clojure.core/ident?" "(ident? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ident?" "(ident? true)" nil e)))))


(deftest test-clojure-core-identical-p
  (testing "clojure.core/identical?"
    ;; Arities: [[x y]]
    (try
      (let [result (identical? nil nil)]
        (record-result! "clojure.core/identical?" "(identical? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/identical?" "(identical? nil nil)" nil e)))
    (try
      (let [result (identical? nil true)]
        (record-result! "clojure.core/identical?" "(identical? nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/identical?" "(identical? nil true)" nil e)))
    (try
      (let [result (identical? true nil)]
        (record-result! "clojure.core/identical?" "(identical? true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/identical?" "(identical? true nil)" nil e)))))


(deftest test-clojure-core-identity
  (testing "clojure.core/identity"
    ;; Arities: [[x]]
    (try
      (let [result (identity nil)]
        (record-result! "clojure.core/identity" "(identity nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/identity" "(identity nil)" nil e)))
    (try
      (let [result (identity true)]
        (record-result! "clojure.core/identity" "(identity true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/identity" "(identity true)" nil e)))))


(deftest test-clojure-core-if-let
  (testing "clojure.core/if-let"
    ;; Arities: [[bindings then] [bindings then else & oldform]]
    (try
      (let [result (if-let nil nil)]
        (record-result! "clojure.core/if-let" "(if-let nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let nil nil)" nil e)))
    (try
      (let [result (if-let nil true)]
        (record-result! "clojure.core/if-let" "(if-let nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let nil true)" nil e)))
    (try
      (let [result (if-let true nil)]
        (record-result! "clojure.core/if-let" "(if-let true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let true nil)" nil e)))
    (try
      (let [result (if-let nil nil nil)]
        (record-result! "clojure.core/if-let" "(if-let nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let nil nil nil)" nil e)))
    (try
      (let [result (if-let nil nil nil nil)]
        (record-result! "clojure.core/if-let" "(if-let nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let nil nil nil nil)" nil e)))
    (try
      (let [result (if-let nil nil nil nil nil)]
        (record-result! "clojure.core/if-let" "(if-let nil nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-let" "(if-let nil nil nil nil nil)" nil e)))))


(deftest test-clojure-core-if-not
  (testing "clojure.core/if-not"
    ;; Arities: [[test then] [test then else]]
    (try
      (let [result (if-not nil nil)]
        (record-result! "clojure.core/if-not" "(if-not nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not nil nil)" nil e)))
    (try
      (let [result (if-not nil true)]
        (record-result! "clojure.core/if-not" "(if-not nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not nil true)" nil e)))
    (try
      (let [result (if-not true nil)]
        (record-result! "clojure.core/if-not" "(if-not true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not true nil)" nil e)))
    (try
      (let [result (if-not nil nil nil)]
        (record-result! "clojure.core/if-not" "(if-not nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not nil nil nil)" nil e)))
    (try
      (let [result (if-not nil nil true)]
        (record-result! "clojure.core/if-not" "(if-not nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not nil nil true)" nil e)))
    (try
      (let [result (if-not nil true nil)]
        (record-result! "clojure.core/if-not" "(if-not nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-not" "(if-not nil true nil)" nil e)))))


(deftest test-clojure-core-if-some
  (testing "clojure.core/if-some"
    ;; Arities: [[bindings then] [bindings then else & oldform]]
    (try
      (let [result (if-some nil nil)]
        (record-result! "clojure.core/if-some" "(if-some nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some nil nil)" nil e)))
    (try
      (let [result (if-some nil true)]
        (record-result! "clojure.core/if-some" "(if-some nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some nil true)" nil e)))
    (try
      (let [result (if-some true nil)]
        (record-result! "clojure.core/if-some" "(if-some true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some true nil)" nil e)))
    (try
      (let [result (if-some nil nil nil)]
        (record-result! "clojure.core/if-some" "(if-some nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some nil nil nil)" nil e)))
    (try
      (let [result (if-some nil nil nil nil)]
        (record-result! "clojure.core/if-some" "(if-some nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some nil nil nil nil)" nil e)))
    (try
      (let [result (if-some nil nil nil nil nil)]
        (record-result! "clojure.core/if-some" "(if-some nil nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/if-some" "(if-some nil nil nil nil nil)" nil e)))))


(deftest test-clojure-core-ifn-p
  (testing "clojure.core/ifn?"
    ;; Arities: [[x]]
    (try
      (let [result (ifn? nil)]
        (record-result! "clojure.core/ifn?" "(ifn? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ifn?" "(ifn? nil)" nil e)))
    (try
      (let [result (ifn? true)]
        (record-result! "clojure.core/ifn?" "(ifn? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ifn?" "(ifn? true)" nil e)))))


(deftest test-clojure-core-import
  (testing "clojure.core/import"
    ;; Arities: [[& import-symbols-or-lists]]
    (try
      (let [result (import)]
        (record-result! "clojure.core/import" "(import)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/import" "(import)" nil e)))))


(deftest test-clojure-core-inc
  (testing "clojure.core/inc"
    ;; Arities: [[x]]
    (try
      (let [result (inc nil)]
        (record-result! "clojure.core/inc" "(inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inc" "(inc nil)" nil e)))
    (try
      (let [result (inc true)]
        (record-result! "clojure.core/inc" "(inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inc" "(inc true)" nil e)))))


(deftest test-clojure-core-inc'
  (testing "clojure.core/inc'"
    ;; Arities: [[x]]
    (try
      (let [result (inc' nil)]
        (record-result! "clojure.core/inc'" "(inc' nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inc'" "(inc' nil)" nil e)))
    (try
      (let [result (inc' true)]
        (record-result! "clojure.core/inc'" "(inc' true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inc'" "(inc' true)" nil e)))))


(deftest test-clojure-core-indexed-p
  (testing "clojure.core/indexed?"
    ;; Arities: [[coll]]
    (try
      (let [result (indexed? nil)]
        (record-result! "clojure.core/indexed?" "(indexed? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/indexed?" "(indexed? nil)" nil e)))
    (try
      (let [result (indexed? [])]
        (record-result! "clojure.core/indexed?" "(indexed? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/indexed?" "(indexed? [])" nil e)))))


(deftest test-clojure-core-infinite-p
  (testing "clojure.core/infinite?"
    ;; Arities: [[num]]
    (try
      (let [result (infinite? 0)]
        (record-result! "clojure.core/infinite?" "(infinite? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/infinite?" "(infinite? 0)" nil e)))
    (try
      (let [result (infinite? 1)]
        (record-result! "clojure.core/infinite?" "(infinite? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/infinite?" "(infinite? 1)" nil e)))))


(deftest test-clojure-core-inst-ms
  (testing "clojure.core/inst-ms"
    ;; Arities: [[inst]]
    (try
      (let [result (inst-ms nil)]
        (record-result! "clojure.core/inst-ms" "(inst-ms nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inst-ms" "(inst-ms nil)" nil e)))
    (try
      (let [result (inst-ms true)]
        (record-result! "clojure.core/inst-ms" "(inst-ms true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inst-ms" "(inst-ms true)" nil e)))))


(deftest test-clojure-core-inst-p
  (testing "clojure.core/inst?"
    ;; Arities: [[x]]
    (try
      (let [result (inst? nil)]
        (record-result! "clojure.core/inst?" "(inst? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inst?" "(inst? nil)" nil e)))
    (try
      (let [result (inst? true)]
        (record-result! "clojure.core/inst?" "(inst? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/inst?" "(inst? true)" nil e)))))


(deftest test-clojure-core-instance-p
  (testing "clojure.core/instance?"
    ;; Arities: [quote ([c x])]
    (try
      (let [result (instance?)]
        (record-result! "clojure.core/instance?" "(instance?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/instance?" "(instance?)" nil e)))
    (try
      (let [result (instance?)]
        (record-result! "clojure.core/instance?" "(instance?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/instance?" "(instance?)" nil e)))))


(deftest test-clojure-core-int
  (testing "clojure.core/int"
    ;; Arities: [[x]]
    (try
      (let [result (int nil)]
        (record-result! "clojure.core/int" "(int nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int" "(int nil)" nil e)))
    (try
      (let [result (int true)]
        (record-result! "clojure.core/int" "(int true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int" "(int true)" nil e)))))


(deftest test-clojure-core-int-array
  (testing "clojure.core/int-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (int-array nil)]
        (record-result! "clojure.core/int-array" "(int-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int-array" "(int-array nil)" nil e)))
    (try
      (let [result (int-array true)]
        (record-result! "clojure.core/int-array" "(int-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int-array" "(int-array true)" nil e)))
    (try
      (let [result (int-array nil nil)]
        (record-result! "clojure.core/int-array" "(int-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int-array" "(int-array nil nil)" nil e)))
    (try
      (let [result (int-array nil true)]
        (record-result! "clojure.core/int-array" "(int-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int-array" "(int-array nil true)" nil e)))
    (try
      (let [result (int-array true nil)]
        (record-result! "clojure.core/int-array" "(int-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int-array" "(int-array true nil)" nil e)))))


(deftest test-clojure-core-int-p
  (testing "clojure.core/int?"
    ;; Arities: [[x]]
    (try
      (let [result (int? nil)]
        (record-result! "clojure.core/int?" "(int? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int?" "(int? nil)" nil e)))
    (try
      (let [result (int? true)]
        (record-result! "clojure.core/int?" "(int? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/int?" "(int? true)" nil e)))))


(deftest test-clojure-core-integer-p
  (testing "clojure.core/integer?"
    ;; Arities: [[n]]
    (try
      (let [result (integer? 0)]
        (record-result! "clojure.core/integer?" "(integer? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/integer?" "(integer? 0)" nil e)))
    (try
      (let [result (integer? 1)]
        (record-result! "clojure.core/integer?" "(integer? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/integer?" "(integer? 1)" nil e)))))


(deftest test-clojure-core-interleave
  (testing "clojure.core/interleave"
    ;; Arities: [[] [c1] [c1 c2] [c1 c2 & colls]]
    (try
      (let [result (interleave)]
        (record-result! "clojure.core/interleave" "(interleave)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave)" nil e)))
    (try
      (let [result (interleave nil)]
        (record-result! "clojure.core/interleave" "(interleave nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave nil)" nil e)))
    (try
      (let [result (interleave true)]
        (record-result! "clojure.core/interleave" "(interleave true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave true)" nil e)))
    (try
      (let [result (interleave nil nil)]
        (record-result! "clojure.core/interleave" "(interleave nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave nil nil)" nil e)))
    (try
      (let [result (interleave nil true)]
        (record-result! "clojure.core/interleave" "(interleave nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave nil true)" nil e)))
    (try
      (let [result (interleave true nil)]
        (record-result! "clojure.core/interleave" "(interleave true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave true nil)" nil e)))
    (try
      (let [result (interleave nil nil)]
        (record-result! "clojure.core/interleave" "(interleave nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave nil nil)" nil e)))
    (try
      (let [result (interleave nil nil nil)]
        (record-result! "clojure.core/interleave" "(interleave nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interleave" "(interleave nil nil nil)" nil e)))))


(deftest test-clojure-core-intern
  (testing "clojure.core/intern"
    ;; Arities: [[ns name] [ns name val]]
    (try
      (let [result (intern nil nil)]
        (record-result! "clojure.core/intern" "(intern nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern nil nil)" nil e)))
    (try
      (let [result (intern nil true)]
        (record-result! "clojure.core/intern" "(intern nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern nil true)" nil e)))
    (try
      (let [result (intern true nil)]
        (record-result! "clojure.core/intern" "(intern true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern true nil)" nil e)))
    (try
      (let [result (intern nil nil nil)]
        (record-result! "clojure.core/intern" "(intern nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern nil nil nil)" nil e)))
    (try
      (let [result (intern nil nil true)]
        (record-result! "clojure.core/intern" "(intern nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern nil nil true)" nil e)))
    (try
      (let [result (intern nil true nil)]
        (record-result! "clojure.core/intern" "(intern nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/intern" "(intern nil true nil)" nil e)))))


(deftest test-clojure-core-interpose
  (testing "clojure.core/interpose"
    ;; Arities: [[sep] [sep coll]]
    (try
      (let [result (interpose nil)]
        (record-result! "clojure.core/interpose" "(interpose nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interpose" "(interpose nil)" nil e)))
    (try
      (let [result (interpose true)]
        (record-result! "clojure.core/interpose" "(interpose true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interpose" "(interpose true)" nil e)))
    (try
      (let [result (interpose nil nil)]
        (record-result! "clojure.core/interpose" "(interpose nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interpose" "(interpose nil nil)" nil e)))
    (try
      (let [result (interpose nil [])]
        (record-result! "clojure.core/interpose" "(interpose nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interpose" "(interpose nil [])" nil e)))
    (try
      (let [result (interpose true nil)]
        (record-result! "clojure.core/interpose" "(interpose true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/interpose" "(interpose true nil)" nil e)))))


(deftest test-clojure-core-into
  (testing "clojure.core/into"
    ;; Arities: [[] [to] [to from] [to xform from]]
    (try
      (let [result (into)]
        (record-result! "clojure.core/into" "(into)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into)" nil e)))
    (try
      (let [result (into nil)]
        (record-result! "clojure.core/into" "(into nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into nil)" nil e)))
    (try
      (let [result (into true)]
        (record-result! "clojure.core/into" "(into true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into true)" nil e)))
    (try
      (let [result (into nil nil)]
        (record-result! "clojure.core/into" "(into nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into nil nil)" nil e)))
    (try
      (let [result (into nil true)]
        (record-result! "clojure.core/into" "(into nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into nil true)" nil e)))
    (try
      (let [result (into true nil)]
        (record-result! "clojure.core/into" "(into true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into true nil)" nil e)))
    (try
      (let [result (into nil nil nil)]
        (record-result! "clojure.core/into" "(into nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into nil nil nil)" nil e)))
    (try
      (let [result (into nil nil true)]
        (record-result! "clojure.core/into" "(into nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into" "(into nil nil true)" nil e)))))


(deftest test-clojure-core-into-array
  (testing "clojure.core/into-array"
    ;; Arities: [[aseq] [type aseq]]
    (try
      (let [result (into-array nil)]
        (record-result! "clojure.core/into-array" "(into-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into-array" "(into-array nil)" nil e)))
    (try
      (let [result (into-array true)]
        (record-result! "clojure.core/into-array" "(into-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into-array" "(into-array true)" nil e)))
    (try
      (let [result (into-array nil nil)]
        (record-result! "clojure.core/into-array" "(into-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into-array" "(into-array nil nil)" nil e)))
    (try
      (let [result (into-array nil true)]
        (record-result! "clojure.core/into-array" "(into-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into-array" "(into-array nil true)" nil e)))
    (try
      (let [result (into-array true nil)]
        (record-result! "clojure.core/into-array" "(into-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/into-array" "(into-array true nil)" nil e)))))


(deftest test-clojure-core-io-bang
  (testing "clojure.core/io!"
    ;; Arities: [[& body]]
    (try
      (let [result (io!)]
        (record-result! "clojure.core/io!" "(io!)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/io!" "(io!)" nil e)))))


(deftest test-clojure-core-isa-p
  (testing "clojure.core/isa?"
    ;; Arities: [[child parent] [h child parent]]
    (try
      (let [result (isa? nil nil)]
        (record-result! "clojure.core/isa?" "(isa? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? nil nil)" nil e)))
    (try
      (let [result (isa? nil true)]
        (record-result! "clojure.core/isa?" "(isa? nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? nil true)" nil e)))
    (try
      (let [result (isa? true nil)]
        (record-result! "clojure.core/isa?" "(isa? true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? true nil)" nil e)))
    (try
      (let [result (isa? nil nil nil)]
        (record-result! "clojure.core/isa?" "(isa? nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? nil nil nil)" nil e)))
    (try
      (let [result (isa? nil nil true)]
        (record-result! "clojure.core/isa?" "(isa? nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? nil nil true)" nil e)))
    (try
      (let [result (isa? nil true nil)]
        (record-result! "clojure.core/isa?" "(isa? nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/isa?" "(isa? nil true nil)" nil e)))))


(deftest test-clojure-core-iterate
  (testing "clojure.core/iterate"
    ;; Arities: [[f x]]
    (try
      (let [result (iterate 'inc nil)]
        (record-result! "clojure.core/iterate" "(iterate 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iterate" "(iterate 'inc nil)" nil e)))
    (try
      (let [result (iterate 'inc true)]
        (record-result! "clojure.core/iterate" "(iterate 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iterate" "(iterate 'inc true)" nil e)))
    (try
      (let [result (iterate 'dec nil)]
        (record-result! "clojure.core/iterate" "(iterate 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iterate" "(iterate 'dec nil)" nil e)))))


(deftest test-clojure-core-iteration
  (testing "clojure.core/iteration"
    ;; Arities: [[step & {:keys [somef vf kf initk], :or {vf identity, kf identity, somef some?, initk nil}}]]
    (try
      (let [result (iteration nil)]
        (record-result! "clojure.core/iteration" "(iteration nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iteration" "(iteration nil)" nil e)))
    (try
      (let [result (iteration nil nil)]
        (record-result! "clojure.core/iteration" "(iteration nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iteration" "(iteration nil nil)" nil e)))
    (try
      (let [result (iteration nil nil nil)]
        (record-result! "clojure.core/iteration" "(iteration nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iteration" "(iteration nil nil nil)" nil e)))))


(deftest test-clojure-core-iterator-seq
  (testing "clojure.core/iterator-seq"
    ;; Arities: [[iter]]
    (try
      (let [result (iterator-seq nil)]
        (record-result! "clojure.core/iterator-seq" "(iterator-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iterator-seq" "(iterator-seq nil)" nil e)))
    (try
      (let [result (iterator-seq true)]
        (record-result! "clojure.core/iterator-seq" "(iterator-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/iterator-seq" "(iterator-seq true)" nil e)))))


(deftest test-clojure-core-juxt
  (testing "clojure.core/juxt"
    ;; Arities: [[f] [f g] [f g h] [f g h & fs]]
    (try
      (let [result (juxt 'inc)]
        (record-result! "clojure.core/juxt" "(juxt 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc)" nil e)))
    (try
      (let [result (juxt 'dec)]
        (record-result! "clojure.core/juxt" "(juxt 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'dec)" nil e)))
    (try
      (let [result (juxt 'inc nil)]
        (record-result! "clojure.core/juxt" "(juxt 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc nil)" nil e)))
    (try
      (let [result (juxt 'inc true)]
        (record-result! "clojure.core/juxt" "(juxt 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc true)" nil e)))
    (try
      (let [result (juxt 'dec nil)]
        (record-result! "clojure.core/juxt" "(juxt 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'dec nil)" nil e)))
    (try
      (let [result (juxt 'inc nil nil)]
        (record-result! "clojure.core/juxt" "(juxt 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc nil nil)" nil e)))
    (try
      (let [result (juxt 'inc nil true)]
        (record-result! "clojure.core/juxt" "(juxt 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc nil true)" nil e)))
    (try
      (let [result (juxt 'inc true nil)]
        (record-result! "clojure.core/juxt" "(juxt 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/juxt" "(juxt 'inc true nil)" nil e)))))


(deftest test-clojure-core-keep
  (testing "clojure.core/keep"
    ;; Arities: [[f] [f coll]]
    (try
      (let [result (keep 'inc)]
        (record-result! "clojure.core/keep" "(keep 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep" "(keep 'inc)" nil e)))
    (try
      (let [result (keep 'dec)]
        (record-result! "clojure.core/keep" "(keep 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep" "(keep 'dec)" nil e)))
    (try
      (let [result (keep 'inc nil)]
        (record-result! "clojure.core/keep" "(keep 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep" "(keep 'inc nil)" nil e)))
    (try
      (let [result (keep 'inc [])]
        (record-result! "clojure.core/keep" "(keep 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep" "(keep 'inc [])" nil e)))
    (try
      (let [result (keep 'dec nil)]
        (record-result! "clojure.core/keep" "(keep 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep" "(keep 'dec nil)" nil e)))))


(deftest test-clojure-core-keep-indexed
  (testing "clojure.core/keep-indexed"
    ;; Arities: [[f] [f coll]]
    (try
      (let [result (keep-indexed 'inc)]
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc)" nil e)))
    (try
      (let [result (keep-indexed 'dec)]
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'dec)" nil e)))
    (try
      (let [result (keep-indexed 'inc nil)]
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc nil)" nil e)))
    (try
      (let [result (keep-indexed 'inc [])]
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'inc [])" nil e)))
    (try
      (let [result (keep-indexed 'dec nil)]
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keep-indexed" "(keep-indexed 'dec nil)" nil e)))))


(deftest test-clojure-core-key
  (testing "clojure.core/key"
    ;; Arities: [[e]]
    (try
      (let [result (key nil)]
        (record-result! "clojure.core/key" "(key nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/key" "(key nil)" nil e)))
    (try
      (let [result (key true)]
        (record-result! "clojure.core/key" "(key true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/key" "(key true)" nil e)))))


(deftest test-clojure-core-keys
  (testing "clojure.core/keys"
    ;; Arities: [[map]]
    (try
      (let [result (keys {})]
        (record-result! "clojure.core/keys" "(keys {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keys" "(keys {})" nil e)))
    (try
      (let [result (keys {:a 1})]
        (record-result! "clojure.core/keys" "(keys {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keys" "(keys {:a 1})" nil e)))))


(deftest test-clojure-core-keyword
  (testing "clojure.core/keyword"
    ;; Arities: [[name] [ns name]]
    (try
      (let [result (keyword nil)]
        (record-result! "clojure.core/keyword" "(keyword nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword" "(keyword nil)" nil e)))
    (try
      (let [result (keyword true)]
        (record-result! "clojure.core/keyword" "(keyword true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword" "(keyword true)" nil e)))
    (try
      (let [result (keyword nil nil)]
        (record-result! "clojure.core/keyword" "(keyword nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword" "(keyword nil nil)" nil e)))
    (try
      (let [result (keyword nil true)]
        (record-result! "clojure.core/keyword" "(keyword nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword" "(keyword nil true)" nil e)))
    (try
      (let [result (keyword true nil)]
        (record-result! "clojure.core/keyword" "(keyword true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword" "(keyword true nil)" nil e)))))


(deftest test-clojure-core-keyword-p
  (testing "clojure.core/keyword?"
    ;; Arities: [[x]]
    (try
      (let [result (keyword? nil)]
        (record-result! "clojure.core/keyword?" "(keyword? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword?" "(keyword? nil)" nil e)))
    (try
      (let [result (keyword? true)]
        (record-result! "clojure.core/keyword?" "(keyword? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/keyword?" "(keyword? true)" nil e)))))


(deftest test-clojure-core-last
  (testing "clojure.core/last"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (last)]
        (record-result! "clojure.core/last" "(last)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/last" "(last)" nil e)))
    (try
      (let [result (last)]
        (record-result! "clojure.core/last" "(last)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/last" "(last)" nil e)))))


(deftest test-clojure-core-lazy-cat
  (testing "clojure.core/lazy-cat"
    ;; Arities: [[& colls]]
    (try
      (let [result (lazy-cat)]
        (record-result! "clojure.core/lazy-cat" "(lazy-cat)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/lazy-cat" "(lazy-cat)" nil e)))))


(deftest test-clojure-core-lazy-seq
  (testing "clojure.core/lazy-seq"
    ;; Arities: [[& body]]
    (try
      (let [result (lazy-seq)]
        (record-result! "clojure.core/lazy-seq" "(lazy-seq)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/lazy-seq" "(lazy-seq)" nil e)))))


(deftest test-clojure-core-let
  (testing "clojure.core/let"
    ;; Arities: [fn*]
    (try
      (let [result (let)]
        (record-result! "clojure.core/let" "(let)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/let" "(let)" nil e)))))


(deftest test-clojure-core-let
  (testing "clojure.core/let"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (let nil)]
        (record-result! "clojure.core/let" "(let nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/let" "(let nil)" nil e)))
    (try
      (let [result (let nil nil)]
        (record-result! "clojure.core/let" "(let nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/let" "(let nil nil)" nil e)))
    (try
      (let [result (let nil nil nil)]
        (record-result! "clojure.core/let" "(let nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/let" "(let nil nil nil)" nil e)))))


(deftest test-clojure-core-letfn
  (testing "clojure.core/letfn"
    ;; Arities: [[fnspecs & body]]
    (try
      (let [result (letfn nil)]
        (record-result! "clojure.core/letfn" "(letfn nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/letfn" "(letfn nil)" nil e)))
    (try
      (let [result (letfn nil nil)]
        (record-result! "clojure.core/letfn" "(letfn nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/letfn" "(letfn nil nil)" nil e)))
    (try
      (let [result (letfn nil nil nil)]
        (record-result! "clojure.core/letfn" "(letfn nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/letfn" "(letfn nil nil nil)" nil e)))))


(deftest test-clojure-core-line-seq
  (testing "clojure.core/line-seq"
    ;; Arities: [[rdr]]
    (try
      (let [result (line-seq nil)]
        (record-result! "clojure.core/line-seq" "(line-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/line-seq" "(line-seq nil)" nil e)))
    (try
      (let [result (line-seq true)]
        (record-result! "clojure.core/line-seq" "(line-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/line-seq" "(line-seq true)" nil e)))))


(deftest test-clojure-core-list
  (testing "clojure.core/list"
    ;; Arities: [quote ([& items])]
    (try
      (let [result (list)]
        (record-result! "clojure.core/list" "(list)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list" "(list)" nil e)))
    (try
      (let [result (list)]
        (record-result! "clojure.core/list" "(list)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list" "(list)" nil e)))))


(deftest test-clojure-core-list-star
  (testing "clojure.core/list*"
    ;; Arities: [[args] [a args] [a b args] [a b c args] [a b c d & more]]
    (try
      (let [result (list* nil)]
        (record-result! "clojure.core/list*" "(list* nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil)" nil e)))
    (try
      (let [result (list* true)]
        (record-result! "clojure.core/list*" "(list* true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* true)" nil e)))
    (try
      (let [result (list* nil nil)]
        (record-result! "clojure.core/list*" "(list* nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil nil)" nil e)))
    (try
      (let [result (list* nil true)]
        (record-result! "clojure.core/list*" "(list* nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil true)" nil e)))
    (try
      (let [result (list* true nil)]
        (record-result! "clojure.core/list*" "(list* true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* true nil)" nil e)))
    (try
      (let [result (list* nil nil nil)]
        (record-result! "clojure.core/list*" "(list* nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil nil nil)" nil e)))
    (try
      (let [result (list* nil nil true)]
        (record-result! "clojure.core/list*" "(list* nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil nil true)" nil e)))
    (try
      (let [result (list* nil true nil)]
        (record-result! "clojure.core/list*" "(list* nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list*" "(list* nil true nil)" nil e)))))


(deftest test-clojure-core-list-p
  (testing "clojure.core/list?"
    ;; Arities: [[x]]
    (try
      (let [result (list? nil)]
        (record-result! "clojure.core/list?" "(list? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list?" "(list? nil)" nil e)))
    (try
      (let [result (list? true)]
        (record-result! "clojure.core/list?" "(list? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/list?" "(list? true)" nil e)))))


(deftest test-clojure-core-load
  (testing "clojure.core/load"
    ;; Arities: [[& paths]]
    (try
      (let [result (load)]
        (record-result! "clojure.core/load" "(load)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/load" "(load)" nil e)))))


(deftest test-clojure-core-load-reader
  (testing "clojure.core/load-reader"
    ;; Arities: [[rdr]]
    (try
      (let [result (load-reader nil)]
        (record-result! "clojure.core/load-reader" "(load-reader nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/load-reader" "(load-reader nil)" nil e)))
    (try
      (let [result (load-reader true)]
        (record-result! "clojure.core/load-reader" "(load-reader true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/load-reader" "(load-reader true)" nil e)))))


(deftest test-clojure-core-load-string
  (testing "clojure.core/load-string"
    ;; Arities: [[s]]
    (try
      (let [result (load-string "")]
        (record-result! "clojure.core/load-string" "(load-string \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/load-string" "(load-string \"\")" nil e)))
    (try
      (let [result (load-string "a")]
        (record-result! "clojure.core/load-string" "(load-string \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/load-string" "(load-string \"a\")" nil e)))))


(deftest test-clojure-core-loaded-libs
  (testing "clojure.core/loaded-libs"
    ;; Arities: [[]]
    (try
      (let [result (loaded-libs)]
        (record-result! "clojure.core/loaded-libs" "(loaded-libs)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/loaded-libs" "(loaded-libs)" nil e)))))


(deftest test-clojure-core-locking
  (testing "clojure.core/locking"
    ;; Arities: [[x & body]]
    (try
      (let [result (locking nil)]
        (record-result! "clojure.core/locking" "(locking nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/locking" "(locking nil)" nil e)))
    (try
      (let [result (locking nil nil)]
        (record-result! "clojure.core/locking" "(locking nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/locking" "(locking nil nil)" nil e)))
    (try
      (let [result (locking nil nil nil)]
        (record-result! "clojure.core/locking" "(locking nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/locking" "(locking nil nil nil)" nil e)))))


(deftest test-clojure-core-long
  (testing "clojure.core/long"
    ;; Arities: [[x]]
    (try
      (let [result (long nil)]
        (record-result! "clojure.core/long" "(long nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long" "(long nil)" nil e)))
    (try
      (let [result (long true)]
        (record-result! "clojure.core/long" "(long true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long" "(long true)" nil e)))))


(deftest test-clojure-core-long-array
  (testing "clojure.core/long-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (long-array nil)]
        (record-result! "clojure.core/long-array" "(long-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long-array" "(long-array nil)" nil e)))
    (try
      (let [result (long-array true)]
        (record-result! "clojure.core/long-array" "(long-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long-array" "(long-array true)" nil e)))
    (try
      (let [result (long-array nil nil)]
        (record-result! "clojure.core/long-array" "(long-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long-array" "(long-array nil nil)" nil e)))
    (try
      (let [result (long-array nil true)]
        (record-result! "clojure.core/long-array" "(long-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long-array" "(long-array nil true)" nil e)))
    (try
      (let [result (long-array true nil)]
        (record-result! "clojure.core/long-array" "(long-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/long-array" "(long-array true nil)" nil e)))))


(deftest test-clojure-core-loop
  (testing "clojure.core/loop"
    ;; Arities: [fn*]
    (try
      (let [result (loop)]
        (record-result! "clojure.core/loop" "(loop)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/loop" "(loop)" nil e)))))


(deftest test-clojure-core-loop
  (testing "clojure.core/loop"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (loop nil)]
        (record-result! "clojure.core/loop" "(loop nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/loop" "(loop nil)" nil e)))
    (try
      (let [result (loop nil nil)]
        (record-result! "clojure.core/loop" "(loop nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/loop" "(loop nil nil)" nil e)))
    (try
      (let [result (loop nil nil nil)]
        (record-result! "clojure.core/loop" "(loop nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/loop" "(loop nil nil nil)" nil e)))))


(deftest test-clojure-core-macroexpand
  (testing "clojure.core/macroexpand"
    ;; Arities: [[form]]
    (try
      (let [result (macroexpand nil)]
        (record-result! "clojure.core/macroexpand" "(macroexpand nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/macroexpand" "(macroexpand nil)" nil e)))
    (try
      (let [result (macroexpand true)]
        (record-result! "clojure.core/macroexpand" "(macroexpand true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/macroexpand" "(macroexpand true)" nil e)))))


(deftest test-clojure-core-macroexpand-1
  (testing "clojure.core/macroexpand-1"
    ;; Arities: [[form]]
    (try
      (let [result (macroexpand-1 nil)]
        (record-result! "clojure.core/macroexpand-1" "(macroexpand-1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/macroexpand-1" "(macroexpand-1 nil)" nil e)))
    (try
      (let [result (macroexpand-1 true)]
        (record-result! "clojure.core/macroexpand-1" "(macroexpand-1 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/macroexpand-1" "(macroexpand-1 true)" nil e)))))


(deftest test-clojure-core-make-array
  (testing "clojure.core/make-array"
    ;; Arities: [[type len] [type dim & more-dims]]
    (try
      (let [result (make-array nil nil)]
        (record-result! "clojure.core/make-array" "(make-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array nil nil)" nil e)))
    (try
      (let [result (make-array nil true)]
        (record-result! "clojure.core/make-array" "(make-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array nil true)" nil e)))
    (try
      (let [result (make-array true nil)]
        (record-result! "clojure.core/make-array" "(make-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array true nil)" nil e)))
    (try
      (let [result (make-array nil nil)]
        (record-result! "clojure.core/make-array" "(make-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array nil nil)" nil e)))
    (try
      (let [result (make-array nil nil nil)]
        (record-result! "clojure.core/make-array" "(make-array nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array nil nil nil)" nil e)))
    (try
      (let [result (make-array nil nil nil nil)]
        (record-result! "clojure.core/make-array" "(make-array nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-array" "(make-array nil nil nil nil)" nil e)))))


(deftest test-clojure-core-make-hierarchy
  (testing "clojure.core/make-hierarchy"
    ;; Arities: [[]]
    (try
      (let [result (make-hierarchy)]
        (record-result! "clojure.core/make-hierarchy" "(make-hierarchy)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/make-hierarchy" "(make-hierarchy)" nil e)))))


(deftest test-clojure-core-map
  (testing "clojure.core/map"
    ;; Arities: [[f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]]
    (try
      (let [result (map 'inc)]
        (record-result! "clojure.core/map" "(map 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc)" nil e)))
    (try
      (let [result (map 'dec)]
        (record-result! "clojure.core/map" "(map 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'dec)" nil e)))
    (try
      (let [result (map 'inc nil)]
        (record-result! "clojure.core/map" "(map 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc nil)" nil e)))
    (try
      (let [result (map 'inc [])]
        (record-result! "clojure.core/map" "(map 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc [])" nil e)))
    (try
      (let [result (map 'dec nil)]
        (record-result! "clojure.core/map" "(map 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'dec nil)" nil e)))
    (try
      (let [result (map 'inc nil nil)]
        (record-result! "clojure.core/map" "(map 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc nil nil)" nil e)))
    (try
      (let [result (map 'inc nil true)]
        (record-result! "clojure.core/map" "(map 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc nil true)" nil e)))
    (try
      (let [result (map 'inc true nil)]
        (record-result! "clojure.core/map" "(map 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map" "(map 'inc true nil)" nil e)))))


(deftest test-clojure-core-map-entry-p
  (testing "clojure.core/map-entry?"
    ;; Arities: [[x]]
    (try
      (let [result (map-entry? nil)]
        (record-result! "clojure.core/map-entry?" "(map-entry? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-entry?" "(map-entry? nil)" nil e)))
    (try
      (let [result (map-entry? true)]
        (record-result! "clojure.core/map-entry?" "(map-entry? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-entry?" "(map-entry? true)" nil e)))))


(deftest test-clojure-core-map-indexed
  (testing "clojure.core/map-indexed"
    ;; Arities: [[f] [f coll]]
    (try
      (let [result (map-indexed 'inc)]
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc)" nil e)))
    (try
      (let [result (map-indexed 'dec)]
        (record-result! "clojure.core/map-indexed" "(map-indexed 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-indexed" "(map-indexed 'dec)" nil e)))
    (try
      (let [result (map-indexed 'inc nil)]
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc nil)" nil e)))
    (try
      (let [result (map-indexed 'inc [])]
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-indexed" "(map-indexed 'inc [])" nil e)))
    (try
      (let [result (map-indexed 'dec nil)]
        (record-result! "clojure.core/map-indexed" "(map-indexed 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map-indexed" "(map-indexed 'dec nil)" nil e)))))


(deftest test-clojure-core-map-p
  (testing "clojure.core/map?"
    ;; Arities: [quote ([x])]
    (try
      (let [result (map?)]
        (record-result! "clojure.core/map?" "(map?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map?" "(map?)" nil e)))
    (try
      (let [result (map?)]
        (record-result! "clojure.core/map?" "(map?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/map?" "(map?)" nil e)))))


(deftest test-clojure-core-mapcat
  (testing "clojure.core/mapcat"
    ;; Arities: [[f] [f & colls]]
    (try
      (let [result (mapcat 'inc)]
        (record-result! "clojure.core/mapcat" "(mapcat 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapcat" "(mapcat 'inc)" nil e)))
    (try
      (let [result (mapcat 'dec)]
        (record-result! "clojure.core/mapcat" "(mapcat 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapcat" "(mapcat 'dec)" nil e)))
    (try
      (let [result (mapcat 'inc)]
        (record-result! "clojure.core/mapcat" "(mapcat 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapcat" "(mapcat 'inc)" nil e)))
    (try
      (let [result (mapcat 'inc nil)]
        (record-result! "clojure.core/mapcat" "(mapcat 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapcat" "(mapcat 'inc nil)" nil e)))
    (try
      (let [result (mapcat 'inc nil nil)]
        (record-result! "clojure.core/mapcat" "(mapcat 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapcat" "(mapcat 'inc nil nil)" nil e)))))


(deftest test-clojure-core-mapv
  (testing "clojure.core/mapv"
    ;; Arities: [[f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]]
    (try
      (let [result (mapv 'inc nil)]
        (record-result! "clojure.core/mapv" "(mapv 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc nil)" nil e)))
    (try
      (let [result (mapv 'inc [])]
        (record-result! "clojure.core/mapv" "(mapv 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc [])" nil e)))
    (try
      (let [result (mapv 'dec nil)]
        (record-result! "clojure.core/mapv" "(mapv 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'dec nil)" nil e)))
    (try
      (let [result (mapv 'inc nil nil)]
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil)" nil e)))
    (try
      (let [result (mapv 'inc nil true)]
        (record-result! "clojure.core/mapv" "(mapv 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc nil true)" nil e)))
    (try
      (let [result (mapv 'inc true nil)]
        (record-result! "clojure.core/mapv" "(mapv 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc true nil)" nil e)))
    (try
      (let [result (mapv 'inc nil nil nil)]
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil nil)" nil e)))
    (try
      (let [result (mapv 'inc nil nil true)]
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mapv" "(mapv 'inc nil nil true)" nil e)))))


(deftest test-clojure-core-max
  (testing "clojure.core/max"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (max nil)]
        (record-result! "clojure.core/max" "(max nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil)" nil e)))
    (try
      (let [result (max true)]
        (record-result! "clojure.core/max" "(max true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max true)" nil e)))
    (try
      (let [result (max nil nil)]
        (record-result! "clojure.core/max" "(max nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil nil)" nil e)))
    (try
      (let [result (max nil true)]
        (record-result! "clojure.core/max" "(max nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil true)" nil e)))
    (try
      (let [result (max true nil)]
        (record-result! "clojure.core/max" "(max true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max true nil)" nil e)))
    (try
      (let [result (max nil nil)]
        (record-result! "clojure.core/max" "(max nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil nil)" nil e)))
    (try
      (let [result (max nil nil nil)]
        (record-result! "clojure.core/max" "(max nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil nil nil)" nil e)))
    (try
      (let [result (max nil nil nil nil)]
        (record-result! "clojure.core/max" "(max nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max" "(max nil nil nil nil)" nil e)))))


(deftest test-clojure-core-max-key
  (testing "clojure.core/max-key"
    ;; Arities: [[k x] [k x y] [k x y & more]]
    (try
      (let [result (max-key :a nil)]
        (record-result! "clojure.core/max-key" "(max-key :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a nil)" nil e)))
    (try
      (let [result (max-key :a true)]
        (record-result! "clojure.core/max-key" "(max-key :a true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a true)" nil e)))
    (try
      (let [result (max-key :b nil)]
        (record-result! "clojure.core/max-key" "(max-key :b nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :b nil)" nil e)))
    (try
      (let [result (max-key :a nil nil)]
        (record-result! "clojure.core/max-key" "(max-key :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a nil nil)" nil e)))
    (try
      (let [result (max-key :a nil true)]
        (record-result! "clojure.core/max-key" "(max-key :a nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a nil true)" nil e)))
    (try
      (let [result (max-key :a true nil)]
        (record-result! "clojure.core/max-key" "(max-key :a true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a true nil)" nil e)))
    (try
      (let [result (max-key :a nil nil)]
        (record-result! "clojure.core/max-key" "(max-key :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a nil nil)" nil e)))
    (try
      (let [result (max-key :a nil nil nil)]
        (record-result! "clojure.core/max-key" "(max-key :a nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/max-key" "(max-key :a nil nil nil)" nil e)))))


(deftest test-clojure-core-memfn
  (testing "clojure.core/memfn"
    ;; Arities: [[name & args]]
    (try
      (let [result (memfn nil)]
        (record-result! "clojure.core/memfn" "(memfn nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/memfn" "(memfn nil)" nil e)))
    (try
      (let [result (memfn nil nil)]
        (record-result! "clojure.core/memfn" "(memfn nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/memfn" "(memfn nil nil)" nil e)))
    (try
      (let [result (memfn nil nil nil)]
        (record-result! "clojure.core/memfn" "(memfn nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/memfn" "(memfn nil nil nil)" nil e)))))


(deftest test-clojure-core-memoize
  (testing "clojure.core/memoize"
    ;; Arities: [[f]]
    (try
      (let [result (memoize 'inc)]
        (record-result! "clojure.core/memoize" "(memoize 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/memoize" "(memoize 'inc)" nil e)))
    (try
      (let [result (memoize 'dec)]
        (record-result! "clojure.core/memoize" "(memoize 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/memoize" "(memoize 'dec)" nil e)))))


(deftest test-clojure-core-merge
  (testing "clojure.core/merge"
    ;; Arities: [[& maps]]
    (try
      (let [result (merge)]
        (record-result! "clojure.core/merge" "(merge)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/merge" "(merge)" nil e)))))


(deftest test-clojure-core-merge-with
  (testing "clojure.core/merge-with"
    ;; Arities: [[f & maps]]
    (try
      (let [result (merge-with 'inc)]
        (record-result! "clojure.core/merge-with" "(merge-with 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/merge-with" "(merge-with 'inc)" nil e)))
    (try
      (let [result (merge-with 'inc nil)]
        (record-result! "clojure.core/merge-with" "(merge-with 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/merge-with" "(merge-with 'inc nil)" nil e)))
    (try
      (let [result (merge-with 'inc nil nil)]
        (record-result! "clojure.core/merge-with" "(merge-with 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/merge-with" "(merge-with 'inc nil nil)" nil e)))))


(deftest test-clojure-core-meta
  (testing "clojure.core/meta"
    ;; Arities: [quote ([obj])]
    (try
      (let [result (meta)]
        (record-result! "clojure.core/meta" "(meta)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/meta" "(meta)" nil e)))
    (try
      (let [result (meta)]
        (record-result! "clojure.core/meta" "(meta)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/meta" "(meta)" nil e)))))


(deftest test-clojure-core-methods
  (testing "clojure.core/methods"
    ;; Arities: [[multifn]]
    (try
      (let [result (methods nil)]
        (record-result! "clojure.core/methods" "(methods nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/methods" "(methods nil)" nil e)))
    (try
      (let [result (methods true)]
        (record-result! "clojure.core/methods" "(methods true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/methods" "(methods true)" nil e)))))


(deftest test-clojure-core-min
  (testing "clojure.core/min"
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (min nil)]
        (record-result! "clojure.core/min" "(min nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil)" nil e)))
    (try
      (let [result (min true)]
        (record-result! "clojure.core/min" "(min true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min true)" nil e)))
    (try
      (let [result (min nil nil)]
        (record-result! "clojure.core/min" "(min nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil nil)" nil e)))
    (try
      (let [result (min nil true)]
        (record-result! "clojure.core/min" "(min nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil true)" nil e)))
    (try
      (let [result (min true nil)]
        (record-result! "clojure.core/min" "(min true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min true nil)" nil e)))
    (try
      (let [result (min nil nil)]
        (record-result! "clojure.core/min" "(min nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil nil)" nil e)))
    (try
      (let [result (min nil nil nil)]
        (record-result! "clojure.core/min" "(min nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil nil nil)" nil e)))
    (try
      (let [result (min nil nil nil nil)]
        (record-result! "clojure.core/min" "(min nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min" "(min nil nil nil nil)" nil e)))))


(deftest test-clojure-core-min-key
  (testing "clojure.core/min-key"
    ;; Arities: [[k x] [k x y] [k x y & more]]
    (try
      (let [result (min-key :a nil)]
        (record-result! "clojure.core/min-key" "(min-key :a nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a nil)" nil e)))
    (try
      (let [result (min-key :a true)]
        (record-result! "clojure.core/min-key" "(min-key :a true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a true)" nil e)))
    (try
      (let [result (min-key :b nil)]
        (record-result! "clojure.core/min-key" "(min-key :b nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :b nil)" nil e)))
    (try
      (let [result (min-key :a nil nil)]
        (record-result! "clojure.core/min-key" "(min-key :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a nil nil)" nil e)))
    (try
      (let [result (min-key :a nil true)]
        (record-result! "clojure.core/min-key" "(min-key :a nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a nil true)" nil e)))
    (try
      (let [result (min-key :a true nil)]
        (record-result! "clojure.core/min-key" "(min-key :a true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a true nil)" nil e)))
    (try
      (let [result (min-key :a nil nil)]
        (record-result! "clojure.core/min-key" "(min-key :a nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a nil nil)" nil e)))
    (try
      (let [result (min-key :a nil nil nil)]
        (record-result! "clojure.core/min-key" "(min-key :a nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/min-key" "(min-key :a nil nil nil)" nil e)))))


(deftest test-clojure-core-mix-collection-hash
  (testing "clojure.core/mix-collection-hash"
    ;; Arities: [[hash-basis count]]
    (try
      (let [result (mix-collection-hash nil 0)]
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash nil 0)" nil e)))
    (try
      (let [result (mix-collection-hash nil 1)]
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash nil 1)" nil e)))
    (try
      (let [result (mix-collection-hash true 0)]
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mix-collection-hash" "(mix-collection-hash true 0)" nil e)))))


(deftest test-clojure-core-mod
  (testing "clojure.core/mod"
    ;; Arities: [[num div]]
    (try
      (let [result (mod 0 nil)]
        (record-result! "clojure.core/mod" "(mod 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mod" "(mod 0 nil)" nil e)))
    (try
      (let [result (mod 0 true)]
        (record-result! "clojure.core/mod" "(mod 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mod" "(mod 0 true)" nil e)))
    (try
      (let [result (mod 1 nil)]
        (record-result! "clojure.core/mod" "(mod 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/mod" "(mod 1 nil)" nil e)))))


(deftest test-clojure-core-name
  (testing "clojure.core/name"
    ;; Arities: [[x]]
    (try
      (let [result (name nil)]
        (record-result! "clojure.core/name" "(name nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/name" "(name nil)" nil e)))
    (try
      (let [result (name true)]
        (record-result! "clojure.core/name" "(name true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/name" "(name true)" nil e)))))


(deftest test-clojure-core-namespace
  (testing "clojure.core/namespace"
    ;; Arities: [[x]]
    (try
      (let [result (namespace nil)]
        (record-result! "clojure.core/namespace" "(namespace nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/namespace" "(namespace nil)" nil e)))
    (try
      (let [result (namespace true)]
        (record-result! "clojure.core/namespace" "(namespace true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/namespace" "(namespace true)" nil e)))))


(deftest test-clojure-core-nat-int-p
  (testing "clojure.core/nat-int?"
    ;; Arities: [[x]]
    (try
      (let [result (nat-int? nil)]
        (record-result! "clojure.core/nat-int?" "(nat-int? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nat-int?" "(nat-int? nil)" nil e)))
    (try
      (let [result (nat-int? true)]
        (record-result! "clojure.core/nat-int?" "(nat-int? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nat-int?" "(nat-int? true)" nil e)))))


(deftest test-clojure-core-neg-int-p
  (testing "clojure.core/neg-int?"
    ;; Arities: [[x]]
    (try
      (let [result (neg-int? nil)]
        (record-result! "clojure.core/neg-int?" "(neg-int? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/neg-int?" "(neg-int? nil)" nil e)))
    (try
      (let [result (neg-int? true)]
        (record-result! "clojure.core/neg-int?" "(neg-int? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/neg-int?" "(neg-int? true)" nil e)))))


(deftest test-clojure-core-neg-p
  (testing "clojure.core/neg?"
    ;; Arities: [[num]]
    (try
      (let [result (neg? 0)]
        (record-result! "clojure.core/neg?" "(neg? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/neg?" "(neg? 0)" nil e)))
    (try
      (let [result (neg? 1)]
        (record-result! "clojure.core/neg?" "(neg? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/neg?" "(neg? 1)" nil e)))))


(deftest test-clojure-core-newline
  (testing "clojure.core/newline"
    ;; Arities: [[]]
    (try
      (let [result (newline)]
        (record-result! "clojure.core/newline" "(newline)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/newline" "(newline)" nil e)))))


(deftest test-clojure-core-next
  (testing "clojure.core/next"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (next)]
        (record-result! "clojure.core/next" "(next)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/next" "(next)" nil e)))
    (try
      (let [result (next)]
        (record-result! "clojure.core/next" "(next)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/next" "(next)" nil e)))))


(deftest test-clojure-core-nfirst
  (testing "clojure.core/nfirst"
    ;; Arities: [quote ([x])]
    (try
      (let [result (nfirst)]
        (record-result! "clojure.core/nfirst" "(nfirst)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nfirst" "(nfirst)" nil e)))
    (try
      (let [result (nfirst)]
        (record-result! "clojure.core/nfirst" "(nfirst)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nfirst" "(nfirst)" nil e)))))


(deftest test-clojure-core-nil-p
  (testing "clojure.core/nil?"
    ;; Arities: [[x]]
    (try
      (let [result (nil? nil)]
        (record-result! "clojure.core/nil?" "(nil? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nil?" "(nil? nil)" nil e)))
    (try
      (let [result (nil? true)]
        (record-result! "clojure.core/nil?" "(nil? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nil?" "(nil? true)" nil e)))))


(deftest test-clojure-core-nnext
  (testing "clojure.core/nnext"
    ;; Arities: [quote ([x])]
    (try
      (let [result (nnext)]
        (record-result! "clojure.core/nnext" "(nnext)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nnext" "(nnext)" nil e)))
    (try
      (let [result (nnext)]
        (record-result! "clojure.core/nnext" "(nnext)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nnext" "(nnext)" nil e)))))


(deftest test-clojure-core-not
  (testing "clojure.core/not"
    ;; Arities: [[x]]
    (try
      (let [result (not nil)]
        (record-result! "clojure.core/not" "(not nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not" "(not nil)" nil e)))
    (try
      (let [result (not true)]
        (record-result! "clojure.core/not" "(not true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not" "(not true)" nil e)))))


(deftest test-clojure-core-not-any-p
  (testing "clojure.core/not-any?"
    ;; Arities: [quote ([pred coll])]
    (try
      (let [result (not-any?)]
        (record-result! "clojure.core/not-any?" "(not-any?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-any?" "(not-any?)" nil e)))
    (try
      (let [result (not-any?)]
        (record-result! "clojure.core/not-any?" "(not-any?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-any?" "(not-any?)" nil e)))))


(deftest test-clojure-core-not-empty
  (testing "clojure.core/not-empty"
    ;; Arities: [[coll]]
    (try
      (let [result (not-empty nil)]
        (record-result! "clojure.core/not-empty" "(not-empty nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-empty" "(not-empty nil)" nil e)))
    (try
      (let [result (not-empty [])]
        (record-result! "clojure.core/not-empty" "(not-empty [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-empty" "(not-empty [])" nil e)))))


(deftest test-clojure-core-not-every-p
  (testing "clojure.core/not-every?"
    ;; Arities: [quote ([pred coll])]
    (try
      (let [result (not-every?)]
        (record-result! "clojure.core/not-every?" "(not-every?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-every?" "(not-every?)" nil e)))
    (try
      (let [result (not-every?)]
        (record-result! "clojure.core/not-every?" "(not-every?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not-every?" "(not-every?)" nil e)))))


(deftest test-clojure-core-not-eq
  (testing "clojure.core/not="
    ;; Arities: [[x] [x y] [x y & more]]
    (try
      (let [result (not= nil)]
        (record-result! "clojure.core/not=" "(not= nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil)" nil e)))
    (try
      (let [result (not= true)]
        (record-result! "clojure.core/not=" "(not= true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= true)" nil e)))
    (try
      (let [result (not= nil nil)]
        (record-result! "clojure.core/not=" "(not= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil nil)" nil e)))
    (try
      (let [result (not= nil true)]
        (record-result! "clojure.core/not=" "(not= nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil true)" nil e)))
    (try
      (let [result (not= true nil)]
        (record-result! "clojure.core/not=" "(not= true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= true nil)" nil e)))
    (try
      (let [result (not= nil nil)]
        (record-result! "clojure.core/not=" "(not= nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil nil)" nil e)))
    (try
      (let [result (not= nil nil nil)]
        (record-result! "clojure.core/not=" "(not= nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil nil nil)" nil e)))
    (try
      (let [result (not= nil nil nil nil)]
        (record-result! "clojure.core/not=" "(not= nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/not=" "(not= nil nil nil nil)" nil e)))))


(deftest test-clojure-core-ns
  (testing "clojure.core/ns"
    ;; Arities: [quote ([name docstring? attr-map? references*])]
    (try
      (let [result (ns)]
        (record-result! "clojure.core/ns" "(ns)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns" "(ns)" nil e)))
    (try
      (let [result (ns)]
        (record-result! "clojure.core/ns" "(ns)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns" "(ns)" nil e)))))


(deftest test-clojure-core-ns-aliases
  (testing "clojure.core/ns-aliases"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-aliases nil)]
        (record-result! "clojure.core/ns-aliases" "(ns-aliases nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-aliases" "(ns-aliases nil)" nil e)))
    (try
      (let [result (ns-aliases true)]
        (record-result! "clojure.core/ns-aliases" "(ns-aliases true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-aliases" "(ns-aliases true)" nil e)))))


(deftest test-clojure-core-ns-imports
  (testing "clojure.core/ns-imports"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-imports nil)]
        (record-result! "clojure.core/ns-imports" "(ns-imports nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-imports" "(ns-imports nil)" nil e)))
    (try
      (let [result (ns-imports true)]
        (record-result! "clojure.core/ns-imports" "(ns-imports true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-imports" "(ns-imports true)" nil e)))))


(deftest test-clojure-core-ns-interns
  (testing "clojure.core/ns-interns"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-interns nil)]
        (record-result! "clojure.core/ns-interns" "(ns-interns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-interns" "(ns-interns nil)" nil e)))
    (try
      (let [result (ns-interns true)]
        (record-result! "clojure.core/ns-interns" "(ns-interns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-interns" "(ns-interns true)" nil e)))))


(deftest test-clojure-core-ns-map
  (testing "clojure.core/ns-map"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-map nil)]
        (record-result! "clojure.core/ns-map" "(ns-map nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-map" "(ns-map nil)" nil e)))
    (try
      (let [result (ns-map true)]
        (record-result! "clojure.core/ns-map" "(ns-map true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-map" "(ns-map true)" nil e)))))


(deftest test-clojure-core-ns-name
  (testing "clojure.core/ns-name"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-name nil)]
        (record-result! "clojure.core/ns-name" "(ns-name nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-name" "(ns-name nil)" nil e)))
    (try
      (let [result (ns-name true)]
        (record-result! "clojure.core/ns-name" "(ns-name true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-name" "(ns-name true)" nil e)))))


(deftest test-clojure-core-ns-publics
  (testing "clojure.core/ns-publics"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-publics nil)]
        (record-result! "clojure.core/ns-publics" "(ns-publics nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-publics" "(ns-publics nil)" nil e)))
    (try
      (let [result (ns-publics true)]
        (record-result! "clojure.core/ns-publics" "(ns-publics true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-publics" "(ns-publics true)" nil e)))))


(deftest test-clojure-core-ns-refers
  (testing "clojure.core/ns-refers"
    ;; Arities: [[ns]]
    (try
      (let [result (ns-refers nil)]
        (record-result! "clojure.core/ns-refers" "(ns-refers nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-refers" "(ns-refers nil)" nil e)))
    (try
      (let [result (ns-refers true)]
        (record-result! "clojure.core/ns-refers" "(ns-refers true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-refers" "(ns-refers true)" nil e)))))


(deftest test-clojure-core-ns-resolve
  (testing "clojure.core/ns-resolve"
    ;; Arities: [[ns sym] [ns env sym]]
    (try
      (let [result (ns-resolve nil nil)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil)" nil e)))
    (try
      (let [result (ns-resolve nil true)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil true)" nil e)))
    (try
      (let [result (ns-resolve true nil)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve true nil)" nil e)))
    (try
      (let [result (ns-resolve nil nil nil)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil nil)" nil e)))
    (try
      (let [result (ns-resolve nil nil true)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil nil true)" nil e)))
    (try
      (let [result (ns-resolve nil true nil)]
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-resolve" "(ns-resolve nil true nil)" nil e)))))


(deftest test-clojure-core-ns-unalias
  (testing "clojure.core/ns-unalias"
    ;; Arities: [[ns sym]]
    (try
      (let [result (ns-unalias nil nil)]
        (record-result! "clojure.core/ns-unalias" "(ns-unalias nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unalias" "(ns-unalias nil nil)" nil e)))
    (try
      (let [result (ns-unalias nil true)]
        (record-result! "clojure.core/ns-unalias" "(ns-unalias nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unalias" "(ns-unalias nil true)" nil e)))
    (try
      (let [result (ns-unalias true nil)]
        (record-result! "clojure.core/ns-unalias" "(ns-unalias true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unalias" "(ns-unalias true nil)" nil e)))))


(deftest test-clojure-core-ns-unmap
  (testing "clojure.core/ns-unmap"
    ;; Arities: [[ns sym]]
    (try
      (let [result (ns-unmap nil nil)]
        (record-result! "clojure.core/ns-unmap" "(ns-unmap nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unmap" "(ns-unmap nil nil)" nil e)))
    (try
      (let [result (ns-unmap nil true)]
        (record-result! "clojure.core/ns-unmap" "(ns-unmap nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unmap" "(ns-unmap nil true)" nil e)))
    (try
      (let [result (ns-unmap true nil)]
        (record-result! "clojure.core/ns-unmap" "(ns-unmap true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ns-unmap" "(ns-unmap true nil)" nil e)))))


(deftest test-clojure-core-nth
  (testing "clojure.core/nth"
    ;; Arities: [[coll index] [coll index not-found]]
    (try
      (let [result (nth nil 0)]
        (record-result! "clojure.core/nth" "(nth nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth nil 0)" nil e)))
    (try
      (let [result (nth nil 1)]
        (record-result! "clojure.core/nth" "(nth nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth nil 1)" nil e)))
    (try
      (let [result (nth [] 0)]
        (record-result! "clojure.core/nth" "(nth [] 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth [] 0)" nil e)))
    (try
      (let [result (nth nil 0 nil)]
        (record-result! "clojure.core/nth" "(nth nil 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth nil 0 nil)" nil e)))
    (try
      (let [result (nth nil 0 true)]
        (record-result! "clojure.core/nth" "(nth nil 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth nil 0 true)" nil e)))
    (try
      (let [result (nth nil 1 nil)]
        (record-result! "clojure.core/nth" "(nth nil 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nth" "(nth nil 1 nil)" nil e)))))


(deftest test-clojure-core-nthnext
  (testing "clojure.core/nthnext"
    ;; Arities: [[coll n]]
    (try
      (let [result (nthnext nil 0)]
        (record-result! "clojure.core/nthnext" "(nthnext nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthnext" "(nthnext nil 0)" nil e)))
    (try
      (let [result (nthnext nil 1)]
        (record-result! "clojure.core/nthnext" "(nthnext nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthnext" "(nthnext nil 1)" nil e)))
    (try
      (let [result (nthnext [] 0)]
        (record-result! "clojure.core/nthnext" "(nthnext [] 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthnext" "(nthnext [] 0)" nil e)))))


(deftest test-clojure-core-nthrest
  (testing "clojure.core/nthrest"
    ;; Arities: [[coll n]]
    (try
      (let [result (nthrest nil 0)]
        (record-result! "clojure.core/nthrest" "(nthrest nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthrest" "(nthrest nil 0)" nil e)))
    (try
      (let [result (nthrest nil 1)]
        (record-result! "clojure.core/nthrest" "(nthrest nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthrest" "(nthrest nil 1)" nil e)))
    (try
      (let [result (nthrest [] 0)]
        (record-result! "clojure.core/nthrest" "(nthrest [] 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/nthrest" "(nthrest [] 0)" nil e)))))


(deftest test-clojure-core-num
  (testing "clojure.core/num"
    ;; Arities: [[x]]
    (try
      (let [result (num nil)]
        (record-result! "clojure.core/num" "(num nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/num" "(num nil)" nil e)))
    (try
      (let [result (num true)]
        (record-result! "clojure.core/num" "(num true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/num" "(num true)" nil e)))))


(deftest test-clojure-core-number-p
  (testing "clojure.core/number?"
    ;; Arities: [[x]]
    (try
      (let [result (number? nil)]
        (record-result! "clojure.core/number?" "(number? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/number?" "(number? nil)" nil e)))
    (try
      (let [result (number? true)]
        (record-result! "clojure.core/number?" "(number? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/number?" "(number? true)" nil e)))))


(deftest test-clojure-core-numerator
  (testing "clojure.core/numerator"
    ;; Arities: [[r]]
    (try
      (let [result (numerator nil)]
        (record-result! "clojure.core/numerator" "(numerator nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/numerator" "(numerator nil)" nil e)))
    (try
      (let [result (numerator true)]
        (record-result! "clojure.core/numerator" "(numerator true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/numerator" "(numerator true)" nil e)))))


(deftest test-clojure-core-object-array
  (testing "clojure.core/object-array"
    ;; Arities: [[size-or-seq]]
    (try
      (let [result (object-array nil)]
        (record-result! "clojure.core/object-array" "(object-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/object-array" "(object-array nil)" nil e)))
    (try
      (let [result (object-array true)]
        (record-result! "clojure.core/object-array" "(object-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/object-array" "(object-array true)" nil e)))))


(deftest test-clojure-core-odd-p
  (testing "clojure.core/odd?"
    ;; Arities: [[n]]
    (try
      (let [result (odd? 0)]
        (record-result! "clojure.core/odd?" "(odd? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/odd?" "(odd? 0)" nil e)))
    (try
      (let [result (odd? 1)]
        (record-result! "clojure.core/odd?" "(odd? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/odd?" "(odd? 1)" nil e)))))


(deftest test-clojure-core-or
  (testing "clojure.core/or"
    ;; Arities: [[] [x] [x & next]]
    (try
      (let [result (or)]
        (record-result! "clojure.core/or" "(or)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or)" nil e)))
    (try
      (let [result (or nil)]
        (record-result! "clojure.core/or" "(or nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or nil)" nil e)))
    (try
      (let [result (or true)]
        (record-result! "clojure.core/or" "(or true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or true)" nil e)))
    (try
      (let [result (or nil)]
        (record-result! "clojure.core/or" "(or nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or nil)" nil e)))
    (try
      (let [result (or nil nil)]
        (record-result! "clojure.core/or" "(or nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or nil nil)" nil e)))
    (try
      (let [result (or nil nil nil)]
        (record-result! "clojure.core/or" "(or nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/or" "(or nil nil nil)" nil e)))))


(deftest test-clojure-core-parents
  (testing "clojure.core/parents"
    ;; Arities: [[tag] [h tag]]
    (try
      (let [result (parents nil)]
        (record-result! "clojure.core/parents" "(parents nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parents" "(parents nil)" nil e)))
    (try
      (let [result (parents true)]
        (record-result! "clojure.core/parents" "(parents true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parents" "(parents true)" nil e)))
    (try
      (let [result (parents nil nil)]
        (record-result! "clojure.core/parents" "(parents nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parents" "(parents nil nil)" nil e)))
    (try
      (let [result (parents nil true)]
        (record-result! "clojure.core/parents" "(parents nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parents" "(parents nil true)" nil e)))
    (try
      (let [result (parents true nil)]
        (record-result! "clojure.core/parents" "(parents true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parents" "(parents true nil)" nil e)))))


(deftest test-clojure-core-parse-boolean
  (testing "clojure.core/parse-boolean"
    ;; Arities: [[s]]
    (try
      (let [result (parse-boolean "")]
        (record-result! "clojure.core/parse-boolean" "(parse-boolean \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-boolean" "(parse-boolean \"\")" nil e)))
    (try
      (let [result (parse-boolean "a")]
        (record-result! "clojure.core/parse-boolean" "(parse-boolean \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-boolean" "(parse-boolean \"a\")" nil e)))))


(deftest test-clojure-core-parse-double
  (testing "clojure.core/parse-double"
    ;; Arities: [[s]]
    (try
      (let [result (parse-double "")]
        (record-result! "clojure.core/parse-double" "(parse-double \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-double" "(parse-double \"\")" nil e)))
    (try
      (let [result (parse-double "a")]
        (record-result! "clojure.core/parse-double" "(parse-double \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-double" "(parse-double \"a\")" nil e)))))


(deftest test-clojure-core-parse-long
  (testing "clojure.core/parse-long"
    ;; Arities: [[s]]
    (try
      (let [result (parse-long "")]
        (record-result! "clojure.core/parse-long" "(parse-long \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-long" "(parse-long \"\")" nil e)))
    (try
      (let [result (parse-long "a")]
        (record-result! "clojure.core/parse-long" "(parse-long \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-long" "(parse-long \"a\")" nil e)))))


(deftest test-clojure-core-parse-uuid
  (testing "clojure.core/parse-uuid"
    ;; Arities: [[s]]
    (try
      (let [result (parse-uuid "")]
        (record-result! "clojure.core/parse-uuid" "(parse-uuid \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-uuid" "(parse-uuid \"\")" nil e)))
    (try
      (let [result (parse-uuid "a")]
        (record-result! "clojure.core/parse-uuid" "(parse-uuid \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/parse-uuid" "(parse-uuid \"a\")" nil e)))))


(deftest test-clojure-core-partial
  (testing "clojure.core/partial"
    ;; Arities: [[f] [f arg1] [f arg1 arg2] [f arg1 arg2 arg3] [f arg1 arg2 arg3 & more]]
    (try
      (let [result (partial 'inc)]
        (record-result! "clojure.core/partial" "(partial 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc)" nil e)))
    (try
      (let [result (partial 'dec)]
        (record-result! "clojure.core/partial" "(partial 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'dec)" nil e)))
    (try
      (let [result (partial 'inc nil)]
        (record-result! "clojure.core/partial" "(partial 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc nil)" nil e)))
    (try
      (let [result (partial 'inc true)]
        (record-result! "clojure.core/partial" "(partial 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc true)" nil e)))
    (try
      (let [result (partial 'dec nil)]
        (record-result! "clojure.core/partial" "(partial 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'dec nil)" nil e)))
    (try
      (let [result (partial 'inc nil nil)]
        (record-result! "clojure.core/partial" "(partial 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc nil nil)" nil e)))
    (try
      (let [result (partial 'inc nil true)]
        (record-result! "clojure.core/partial" "(partial 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc nil true)" nil e)))
    (try
      (let [result (partial 'inc true nil)]
        (record-result! "clojure.core/partial" "(partial 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partial" "(partial 'inc true nil)" nil e)))))


(deftest test-clojure-core-partition
  (testing "clojure.core/partition"
    ;; Arities: [[n coll] [n step coll] [n step pad coll]]
    (try
      (let [result (partition 0 nil)]
        (record-result! "clojure.core/partition" "(partition 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 nil)" nil e)))
    (try
      (let [result (partition 0 [])]
        (record-result! "clojure.core/partition" "(partition 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 [])" nil e)))
    (try
      (let [result (partition 1 nil)]
        (record-result! "clojure.core/partition" "(partition 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 1 nil)" nil e)))
    (try
      (let [result (partition 0 nil nil)]
        (record-result! "clojure.core/partition" "(partition 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 nil nil)" nil e)))
    (try
      (let [result (partition 0 nil [])]
        (record-result! "clojure.core/partition" "(partition 0 nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 nil [])" nil e)))
    (try
      (let [result (partition 0 true nil)]
        (record-result! "clojure.core/partition" "(partition 0 true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 true nil)" nil e)))
    (try
      (let [result (partition 0 nil nil nil)]
        (record-result! "clojure.core/partition" "(partition 0 nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 nil nil nil)" nil e)))
    (try
      (let [result (partition 0 nil nil [])]
        (record-result! "clojure.core/partition" "(partition 0 nil nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition" "(partition 0 nil nil [])" nil e)))))


(deftest test-clojure-core-partition-all
  (testing "clojure.core/partition-all"
    ;; Arities: [[n] [n coll] [n step coll]]
    (try
      (let [result (partition-all 0)]
        (record-result! "clojure.core/partition-all" "(partition-all 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0)" nil e)))
    (try
      (let [result (partition-all 1)]
        (record-result! "clojure.core/partition-all" "(partition-all 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 1)" nil e)))
    (try
      (let [result (partition-all 0 nil)]
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil)" nil e)))
    (try
      (let [result (partition-all 0 [])]
        (record-result! "clojure.core/partition-all" "(partition-all 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0 [])" nil e)))
    (try
      (let [result (partition-all 1 nil)]
        (record-result! "clojure.core/partition-all" "(partition-all 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 1 nil)" nil e)))
    (try
      (let [result (partition-all 0 nil nil)]
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil nil)" nil e)))
    (try
      (let [result (partition-all 0 nil [])]
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0 nil [])" nil e)))
    (try
      (let [result (partition-all 0 true nil)]
        (record-result! "clojure.core/partition-all" "(partition-all 0 true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-all" "(partition-all 0 true nil)" nil e)))))


(deftest test-clojure-core-partition-by
  (testing "clojure.core/partition-by"
    ;; Arities: [[f] [f coll]]
    (try
      (let [result (partition-by 'inc)]
        (record-result! "clojure.core/partition-by" "(partition-by 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-by" "(partition-by 'inc)" nil e)))
    (try
      (let [result (partition-by 'dec)]
        (record-result! "clojure.core/partition-by" "(partition-by 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-by" "(partition-by 'dec)" nil e)))
    (try
      (let [result (partition-by 'inc nil)]
        (record-result! "clojure.core/partition-by" "(partition-by 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-by" "(partition-by 'inc nil)" nil e)))
    (try
      (let [result (partition-by 'inc [])]
        (record-result! "clojure.core/partition-by" "(partition-by 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-by" "(partition-by 'inc [])" nil e)))
    (try
      (let [result (partition-by 'dec nil)]
        (record-result! "clojure.core/partition-by" "(partition-by 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partition-by" "(partition-by 'dec nil)" nil e)))))


(deftest test-clojure-core-partitionv
  (testing "clojure.core/partitionv"
    ;; Arities: [[n coll] [n step coll] [n step pad coll]]
    (try
      (let [result (partitionv 0 nil)]
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil)" nil e)))
    (try
      (let [result (partitionv 0 [])]
        (record-result! "clojure.core/partitionv" "(partitionv 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 [])" nil e)))
    (try
      (let [result (partitionv 1 nil)]
        (record-result! "clojure.core/partitionv" "(partitionv 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 1 nil)" nil e)))
    (try
      (let [result (partitionv 0 nil nil)]
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil)" nil e)))
    (try
      (let [result (partitionv 0 nil [])]
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil [])" nil e)))
    (try
      (let [result (partitionv 0 true nil)]
        (record-result! "clojure.core/partitionv" "(partitionv 0 true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 true nil)" nil e)))
    (try
      (let [result (partitionv 0 nil nil nil)]
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil nil)" nil e)))
    (try
      (let [result (partitionv 0 nil nil [])]
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv" "(partitionv 0 nil nil [])" nil e)))))


(deftest test-clojure-core-partitionv-all
  (testing "clojure.core/partitionv-all"
    ;; Arities: [[n] [n coll] [n step coll]]
    (try
      (let [result (partitionv-all 0)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0)" nil e)))
    (try
      (let [result (partitionv-all 1)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 1)" nil e)))
    (try
      (let [result (partitionv-all 0 nil)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil)" nil e)))
    (try
      (let [result (partitionv-all 0 [])]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 [])" nil e)))
    (try
      (let [result (partitionv-all 1 nil)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 1 nil)" nil e)))
    (try
      (let [result (partitionv-all 0 nil nil)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil nil)" nil e)))
    (try
      (let [result (partitionv-all 0 nil [])]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 nil [])" nil e)))
    (try
      (let [result (partitionv-all 0 true nil)]
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/partitionv-all" "(partitionv-all 0 true nil)" nil e)))))


(deftest test-clojure-core-pcalls
  (testing "clojure.core/pcalls"
    ;; Arities: [[& fns]]
    (try
      (let [result (pcalls)]
        (record-result! "clojure.core/pcalls" "(pcalls)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pcalls" "(pcalls)" nil e)))))


(deftest test-clojure-core-peek
  (testing "clojure.core/peek"
    ;; Arities: [[coll]]
    (try
      (let [result (peek nil)]
        (record-result! "clojure.core/peek" "(peek nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/peek" "(peek nil)" nil e)))
    (try
      (let [result (peek [])]
        (record-result! "clojure.core/peek" "(peek [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/peek" "(peek [])" nil e)))))


(deftest test-clojure-core-persistent-bang
  (testing "clojure.core/persistent!"
    ;; Arities: [[coll]]
    (try
      (let [result (persistent! nil)]
        (record-result! "clojure.core/persistent!" "(persistent! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/persistent!" "(persistent! nil)" nil e)))
    (try
      (let [result (persistent! [])]
        (record-result! "clojure.core/persistent!" "(persistent! [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/persistent!" "(persistent! [])" nil e)))))


(deftest test-clojure-core-pmap
  (testing "clojure.core/pmap"
    ;; Arities: [[f coll] [f coll & colls]]
    (try
      (let [result (pmap 'inc nil)]
        (record-result! "clojure.core/pmap" "(pmap 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'inc nil)" nil e)))
    (try
      (let [result (pmap 'inc [])]
        (record-result! "clojure.core/pmap" "(pmap 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'inc [])" nil e)))
    (try
      (let [result (pmap 'dec nil)]
        (record-result! "clojure.core/pmap" "(pmap 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'dec nil)" nil e)))
    (try
      (let [result (pmap 'inc nil)]
        (record-result! "clojure.core/pmap" "(pmap 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'inc nil)" nil e)))
    (try
      (let [result (pmap 'inc nil nil)]
        (record-result! "clojure.core/pmap" "(pmap 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'inc nil nil)" nil e)))
    (try
      (let [result (pmap 'inc nil nil nil)]
        (record-result! "clojure.core/pmap" "(pmap 'inc nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pmap" "(pmap 'inc nil nil nil)" nil e)))))


(deftest test-clojure-core-pop
  (testing "clojure.core/pop"
    ;; Arities: [[coll]]
    (try
      (let [result (pop nil)]
        (record-result! "clojure.core/pop" "(pop nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pop" "(pop nil)" nil e)))
    (try
      (let [result (pop [])]
        (record-result! "clojure.core/pop" "(pop [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pop" "(pop [])" nil e)))))


(deftest test-clojure-core-pop-bang
  (testing "clojure.core/pop!"
    ;; Arities: [[coll]]
    (try
      (let [result (pop! nil)]
        (record-result! "clojure.core/pop!" "(pop! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pop!" "(pop! nil)" nil e)))
    (try
      (let [result (pop! [])]
        (record-result! "clojure.core/pop!" "(pop! [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pop!" "(pop! [])" nil e)))))


(deftest test-clojure-core-pop-thread-bindings
  (testing "clojure.core/pop-thread-bindings"
    ;; Arities: [[]]
    (try
      (let [result (pop-thread-bindings)]
        (record-result! "clojure.core/pop-thread-bindings" "(pop-thread-bindings)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pop-thread-bindings" "(pop-thread-bindings)" nil e)))))


(deftest test-clojure-core-pos-int-p
  (testing "clojure.core/pos-int?"
    ;; Arities: [[x]]
    (try
      (let [result (pos-int? nil)]
        (record-result! "clojure.core/pos-int?" "(pos-int? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pos-int?" "(pos-int? nil)" nil e)))
    (try
      (let [result (pos-int? true)]
        (record-result! "clojure.core/pos-int?" "(pos-int? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pos-int?" "(pos-int? true)" nil e)))))


(deftest test-clojure-core-pos-p
  (testing "clojure.core/pos?"
    ;; Arities: [[num]]
    (try
      (let [result (pos? 0)]
        (record-result! "clojure.core/pos?" "(pos? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pos?" "(pos? 0)" nil e)))
    (try
      (let [result (pos? 1)]
        (record-result! "clojure.core/pos?" "(pos? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pos?" "(pos? 1)" nil e)))))


(deftest test-clojure-core-pr
  (testing "clojure.core/pr"
    ;; Arities: [[] [x] [x & more]]
    (try
      (let [result (pr)]
        (record-result! "clojure.core/pr" "(pr)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr)" nil e)))
    (try
      (let [result (pr nil)]
        (record-result! "clojure.core/pr" "(pr nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr nil)" nil e)))
    (try
      (let [result (pr true)]
        (record-result! "clojure.core/pr" "(pr true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr true)" nil e)))
    (try
      (let [result (pr nil)]
        (record-result! "clojure.core/pr" "(pr nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr nil)" nil e)))
    (try
      (let [result (pr nil nil)]
        (record-result! "clojure.core/pr" "(pr nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr nil nil)" nil e)))
    (try
      (let [result (pr nil nil nil)]
        (record-result! "clojure.core/pr" "(pr nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr" "(pr nil nil nil)" nil e)))))


(deftest test-clojure-core-pr-str
  (testing "clojure.core/pr-str"
    ;; Arities: [[& xs]]
    (try
      (let [result (pr-str)]
        (record-result! "clojure.core/pr-str" "(pr-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pr-str" "(pr-str)" nil e)))))


(deftest test-clojure-core-prefer-method
  (testing "clojure.core/prefer-method"
    ;; Arities: [[multifn dispatch-val-x dispatch-val-y]]
    (try
      (let [result (prefer-method nil nil nil)]
        (record-result! "clojure.core/prefer-method" "(prefer-method nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prefer-method" "(prefer-method nil nil nil)" nil e)))
    (try
      (let [result (prefer-method nil nil true)]
        (record-result! "clojure.core/prefer-method" "(prefer-method nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prefer-method" "(prefer-method nil nil true)" nil e)))
    (try
      (let [result (prefer-method nil true nil)]
        (record-result! "clojure.core/prefer-method" "(prefer-method nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prefer-method" "(prefer-method nil true nil)" nil e)))))


(deftest test-clojure-core-prefers
  (testing "clojure.core/prefers"
    ;; Arities: [[multifn]]
    (try
      (let [result (prefers nil)]
        (record-result! "clojure.core/prefers" "(prefers nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prefers" "(prefers nil)" nil e)))
    (try
      (let [result (prefers true)]
        (record-result! "clojure.core/prefers" "(prefers true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prefers" "(prefers true)" nil e)))))


(deftest test-clojure-core-print
  (testing "clojure.core/print"
    ;; Arities: [[& more]]
    (try
      (let [result (print)]
        (record-result! "clojure.core/print" "(print)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/print" "(print)" nil e)))))


(deftest test-clojure-core-print-str
  (testing "clojure.core/print-str"
    ;; Arities: [[& xs]]
    (try
      (let [result (print-str)]
        (record-result! "clojure.core/print-str" "(print-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/print-str" "(print-str)" nil e)))))


(deftest test-clojure-core-printf
  (testing "clojure.core/printf"
    ;; Arities: [[fmt & args]]
    (try
      (let [result (printf nil)]
        (record-result! "clojure.core/printf" "(printf nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/printf" "(printf nil)" nil e)))
    (try
      (let [result (printf nil nil)]
        (record-result! "clojure.core/printf" "(printf nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/printf" "(printf nil nil)" nil e)))
    (try
      (let [result (printf nil nil nil)]
        (record-result! "clojure.core/printf" "(printf nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/printf" "(printf nil nil nil)" nil e)))))


(deftest test-clojure-core-println
  (testing "clojure.core/println"
    ;; Arities: [[& more]]
    (try
      (let [result (println)]
        (record-result! "clojure.core/println" "(println)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/println" "(println)" nil e)))))


(deftest test-clojure-core-println-str
  (testing "clojure.core/println-str"
    ;; Arities: [[& xs]]
    (try
      (let [result (println-str)]
        (record-result! "clojure.core/println-str" "(println-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/println-str" "(println-str)" nil e)))))


(deftest test-clojure-core-prn
  (testing "clojure.core/prn"
    ;; Arities: [[& more]]
    (try
      (let [result (prn)]
        (record-result! "clojure.core/prn" "(prn)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prn" "(prn)" nil e)))))


(deftest test-clojure-core-prn-str
  (testing "clojure.core/prn-str"
    ;; Arities: [[& xs]]
    (try
      (let [result (prn-str)]
        (record-result! "clojure.core/prn-str" "(prn-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/prn-str" "(prn-str)" nil e)))))


(deftest test-clojure-core-promise
  (testing "clojure.core/promise"
    ;; Arities: [[]]
    (try
      (let [result (promise)]
        (record-result! "clojure.core/promise" "(promise)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/promise" "(promise)" nil e)))))


(deftest test-clojure-core-push-thread-bindings
  (testing "clojure.core/push-thread-bindings"
    ;; Arities: [[bindings]]
    (try
      (let [result (push-thread-bindings nil)]
        (record-result! "clojure.core/push-thread-bindings" "(push-thread-bindings nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/push-thread-bindings" "(push-thread-bindings nil)" nil e)))
    (try
      (let [result (push-thread-bindings true)]
        (record-result! "clojure.core/push-thread-bindings" "(push-thread-bindings true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/push-thread-bindings" "(push-thread-bindings true)" nil e)))))


(deftest test-clojure-core-pvalues
  (testing "clojure.core/pvalues"
    ;; Arities: [[& exprs]]
    (try
      (let [result (pvalues)]
        (record-result! "clojure.core/pvalues" "(pvalues)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/pvalues" "(pvalues)" nil e)))))


(deftest test-clojure-core-qualified-ident-p
  (testing "clojure.core/qualified-ident?"
    ;; Arities: [[x]]
    (try
      (let [result (qualified-ident? nil)]
        (record-result! "clojure.core/qualified-ident?" "(qualified-ident? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-ident?" "(qualified-ident? nil)" nil e)))
    (try
      (let [result (qualified-ident? true)]
        (record-result! "clojure.core/qualified-ident?" "(qualified-ident? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-ident?" "(qualified-ident? true)" nil e)))))


(deftest test-clojure-core-qualified-keyword-p
  (testing "clojure.core/qualified-keyword?"
    ;; Arities: [[x]]
    (try
      (let [result (qualified-keyword? nil)]
        (record-result! "clojure.core/qualified-keyword?" "(qualified-keyword? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-keyword?" "(qualified-keyword? nil)" nil e)))
    (try
      (let [result (qualified-keyword? true)]
        (record-result! "clojure.core/qualified-keyword?" "(qualified-keyword? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-keyword?" "(qualified-keyword? true)" nil e)))))


(deftest test-clojure-core-qualified-symbol-p
  (testing "clojure.core/qualified-symbol?"
    ;; Arities: [[x]]
    (try
      (let [result (qualified-symbol? nil)]
        (record-result! "clojure.core/qualified-symbol?" "(qualified-symbol? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-symbol?" "(qualified-symbol? nil)" nil e)))
    (try
      (let [result (qualified-symbol? true)]
        (record-result! "clojure.core/qualified-symbol?" "(qualified-symbol? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/qualified-symbol?" "(qualified-symbol? true)" nil e)))))


(deftest test-clojure-core-quot
  (testing "clojure.core/quot"
    ;; Arities: [[num div]]
    (try
      (let [result (quot 0 nil)]
        (record-result! "clojure.core/quot" "(quot 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/quot" "(quot 0 nil)" nil e)))
    (try
      (let [result (quot 0 true)]
        (record-result! "clojure.core/quot" "(quot 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/quot" "(quot 0 true)" nil e)))
    (try
      (let [result (quot 1 nil)]
        (record-result! "clojure.core/quot" "(quot 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/quot" "(quot 1 nil)" nil e)))))


(deftest test-clojure-core-rand
  (testing "clojure.core/rand"
    ;; Arities: [[] [n]]
    (try
      (let [result (rand)]
        (record-result! "clojure.core/rand" "(rand)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand" "(rand)" nil e)))
    (try
      (let [result (rand 0)]
        (record-result! "clojure.core/rand" "(rand 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand" "(rand 0)" nil e)))
    (try
      (let [result (rand 1)]
        (record-result! "clojure.core/rand" "(rand 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand" "(rand 1)" nil e)))))


(deftest test-clojure-core-rand-int
  (testing "clojure.core/rand-int"
    ;; Arities: [[n]]
    (try
      (let [result (rand-int 0)]
        (record-result! "clojure.core/rand-int" "(rand-int 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand-int" "(rand-int 0)" nil e)))
    (try
      (let [result (rand-int 1)]
        (record-result! "clojure.core/rand-int" "(rand-int 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand-int" "(rand-int 1)" nil e)))))


(deftest test-clojure-core-rand-nth
  (testing "clojure.core/rand-nth"
    ;; Arities: [[coll]]
    (try
      (let [result (rand-nth nil)]
        (record-result! "clojure.core/rand-nth" "(rand-nth nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand-nth" "(rand-nth nil)" nil e)))
    (try
      (let [result (rand-nth [])]
        (record-result! "clojure.core/rand-nth" "(rand-nth [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rand-nth" "(rand-nth [])" nil e)))))


(deftest test-clojure-core-random-sample
  (testing "clojure.core/random-sample"
    ;; Arities: [[prob] [prob coll]]
    (try
      (let [result (random-sample nil)]
        (record-result! "clojure.core/random-sample" "(random-sample nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-sample" "(random-sample nil)" nil e)))
    (try
      (let [result (random-sample true)]
        (record-result! "clojure.core/random-sample" "(random-sample true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-sample" "(random-sample true)" nil e)))
    (try
      (let [result (random-sample nil nil)]
        (record-result! "clojure.core/random-sample" "(random-sample nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-sample" "(random-sample nil nil)" nil e)))
    (try
      (let [result (random-sample nil [])]
        (record-result! "clojure.core/random-sample" "(random-sample nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-sample" "(random-sample nil [])" nil e)))
    (try
      (let [result (random-sample true nil)]
        (record-result! "clojure.core/random-sample" "(random-sample true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-sample" "(random-sample true nil)" nil e)))))


(deftest test-clojure-core-random-uuid
  (testing "clojure.core/random-uuid"
    ;; Arities: [[]]
    (try
      (let [result (random-uuid)]
        (record-result! "clojure.core/random-uuid" "(random-uuid)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/random-uuid" "(random-uuid)" nil e)))))


(deftest test-clojure-core-range
  (testing "clojure.core/range"
    ;; Arities: [[] [end] [start end] [start end step]]
    (try
      (let [result (range)]
        (record-result! "clojure.core/range" "(range)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range)" nil e)))
    (try
      (let [result (range 0)]
        (record-result! "clojure.core/range" "(range 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 0)" nil e)))
    (try
      (let [result (range 1)]
        (record-result! "clojure.core/range" "(range 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 1)" nil e)))
    (try
      (let [result (range 0 0)]
        (record-result! "clojure.core/range" "(range 0 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 0 0)" nil e)))
    (try
      (let [result (range 0 1)]
        (record-result! "clojure.core/range" "(range 0 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 0 1)" nil e)))
    (try
      (let [result (range 1 0)]
        (record-result! "clojure.core/range" "(range 1 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 1 0)" nil e)))
    (try
      (let [result (range 0 0 nil)]
        (record-result! "clojure.core/range" "(range 0 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 0 0 nil)" nil e)))
    (try
      (let [result (range 0 0 true)]
        (record-result! "clojure.core/range" "(range 0 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/range" "(range 0 0 true)" nil e)))))


(deftest test-clojure-core-ratio-p
  (testing "clojure.core/ratio?"
    ;; Arities: [[n]]
    (try
      (let [result (ratio? 0)]
        (record-result! "clojure.core/ratio?" "(ratio? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ratio?" "(ratio? 0)" nil e)))
    (try
      (let [result (ratio? 1)]
        (record-result! "clojure.core/ratio?" "(ratio? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ratio?" "(ratio? 1)" nil e)))))


(deftest test-clojure-core-rational-p
  (testing "clojure.core/rational?"
    ;; Arities: [[n]]
    (try
      (let [result (rational? 0)]
        (record-result! "clojure.core/rational?" "(rational? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rational?" "(rational? 0)" nil e)))
    (try
      (let [result (rational? 1)]
        (record-result! "clojure.core/rational?" "(rational? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rational?" "(rational? 1)" nil e)))))


(deftest test-clojure-core-rationalize
  (testing "clojure.core/rationalize"
    ;; Arities: [[num]]
    (try
      (let [result (rationalize 0)]
        (record-result! "clojure.core/rationalize" "(rationalize 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rationalize" "(rationalize 0)" nil e)))
    (try
      (let [result (rationalize 1)]
        (record-result! "clojure.core/rationalize" "(rationalize 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rationalize" "(rationalize 1)" nil e)))))


(deftest test-clojure-core-re-find
  (testing "clojure.core/re-find"
    ;; Arities: [[m] [re s]]
    (try
      (let [result (re-find {})]
        (record-result! "clojure.core/re-find" "(re-find {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-find" "(re-find {})" nil e)))
    (try
      (let [result (re-find {:a 1})]
        (record-result! "clojure.core/re-find" "(re-find {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-find" "(re-find {:a 1})" nil e)))
    (try
      (let [result (re-find #"foo" "")]
        (record-result! "clojure.core/re-find" "(re-find #\"foo\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-find" "(re-find #\"foo\" \"\")" nil e)))
    (try
      (let [result (re-find #"foo" "a")]
        (record-result! "clojure.core/re-find" "(re-find #\"foo\" \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-find" "(re-find #\"foo\" \"a\")" nil e)))
    (try
      (let [result (re-find #"bar.*" "")]
        (record-result! "clojure.core/re-find" "(re-find #\"bar.*\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-find" "(re-find #\"bar.*\" \"\")" nil e)))))


(deftest test-clojure-core-re-groups
  (testing "clojure.core/re-groups"
    ;; Arities: [[m]]
    (try
      (let [result (re-groups {})]
        (record-result! "clojure.core/re-groups" "(re-groups {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-groups" "(re-groups {})" nil e)))
    (try
      (let [result (re-groups {:a 1})]
        (record-result! "clojure.core/re-groups" "(re-groups {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-groups" "(re-groups {:a 1})" nil e)))))


(deftest test-clojure-core-re-matcher
  (testing "clojure.core/re-matcher"
    ;; Arities: [[re s]]
    (try
      (let [result (re-matcher #"foo" "")]
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"foo\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"foo\" \"\")" nil e)))
    (try
      (let [result (re-matcher #"foo" "a")]
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"foo\" \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"foo\" \"a\")" nil e)))
    (try
      (let [result (re-matcher #"bar.*" "")]
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"bar.*\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matcher" "(re-matcher #\"bar.*\" \"\")" nil e)))))


(deftest test-clojure-core-re-matches
  (testing "clojure.core/re-matches"
    ;; Arities: [[re s]]
    (try
      (let [result (re-matches #"foo" "")]
        (record-result! "clojure.core/re-matches" "(re-matches #\"foo\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matches" "(re-matches #\"foo\" \"\")" nil e)))
    (try
      (let [result (re-matches #"foo" "a")]
        (record-result! "clojure.core/re-matches" "(re-matches #\"foo\" \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matches" "(re-matches #\"foo\" \"a\")" nil e)))
    (try
      (let [result (re-matches #"bar.*" "")]
        (record-result! "clojure.core/re-matches" "(re-matches #\"bar.*\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-matches" "(re-matches #\"bar.*\" \"\")" nil e)))))


(deftest test-clojure-core-re-pattern
  (testing "clojure.core/re-pattern"
    ;; Arities: [[s]]
    (try
      (let [result (re-pattern "")]
        (record-result! "clojure.core/re-pattern" "(re-pattern \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-pattern" "(re-pattern \"\")" nil e)))
    (try
      (let [result (re-pattern "a")]
        (record-result! "clojure.core/re-pattern" "(re-pattern \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-pattern" "(re-pattern \"a\")" nil e)))))


(deftest test-clojure-core-re-seq
  (testing "clojure.core/re-seq"
    ;; Arities: [[re s]]
    (try
      (let [result (re-seq #"foo" "")]
        (record-result! "clojure.core/re-seq" "(re-seq #\"foo\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-seq" "(re-seq #\"foo\" \"\")" nil e)))
    (try
      (let [result (re-seq #"foo" "a")]
        (record-result! "clojure.core/re-seq" "(re-seq #\"foo\" \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-seq" "(re-seq #\"foo\" \"a\")" nil e)))
    (try
      (let [result (re-seq #"bar.*" "")]
        (record-result! "clojure.core/re-seq" "(re-seq #\"bar.*\" \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/re-seq" "(re-seq #\"bar.*\" \"\")" nil e)))))


(deftest test-clojure-core-read
  (testing "clojure.core/read"
    ;; Arities: [[] [stream] [stream eof-error? eof-value] [stream eof-error? eof-value recursive?] [opts stream]]
    (try
      (let [result (read)]
        (record-result! "clojure.core/read" "(read)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read)" nil e)))
    (try
      (let [result (read nil)]
        (record-result! "clojure.core/read" "(read nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil)" nil e)))
    (try
      (let [result (read true)]
        (record-result! "clojure.core/read" "(read true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read true)" nil e)))
    (try
      (let [result (read nil nil nil)]
        (record-result! "clojure.core/read" "(read nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil nil nil)" nil e)))
    (try
      (let [result (read nil nil true)]
        (record-result! "clojure.core/read" "(read nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil nil true)" nil e)))
    (try
      (let [result (read nil true nil)]
        (record-result! "clojure.core/read" "(read nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil true nil)" nil e)))
    (try
      (let [result (read nil nil nil nil)]
        (record-result! "clojure.core/read" "(read nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil nil nil nil)" nil e)))
    (try
      (let [result (read nil nil nil true)]
        (record-result! "clojure.core/read" "(read nil nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read" "(read nil nil nil true)" nil e)))))


(deftest test-clojure-core-read-plusstring
  (testing "clojure.core/read+string"
    ;; Arities: [[] [stream] [stream eof-error? eof-value] [stream eof-error? eof-value recursive?] [opts stream]]
    (try
      (let [result (read+string)]
        (record-result! "clojure.core/read+string" "(read+string)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string)" nil e)))
    (try
      (let [result (read+string nil)]
        (record-result! "clojure.core/read+string" "(read+string nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil)" nil e)))
    (try
      (let [result (read+string true)]
        (record-result! "clojure.core/read+string" "(read+string true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string true)" nil e)))
    (try
      (let [result (read+string nil nil nil)]
        (record-result! "clojure.core/read+string" "(read+string nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil nil nil)" nil e)))
    (try
      (let [result (read+string nil nil true)]
        (record-result! "clojure.core/read+string" "(read+string nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil nil true)" nil e)))
    (try
      (let [result (read+string nil true nil)]
        (record-result! "clojure.core/read+string" "(read+string nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil true nil)" nil e)))
    (try
      (let [result (read+string nil nil nil nil)]
        (record-result! "clojure.core/read+string" "(read+string nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil nil nil nil)" nil e)))
    (try
      (let [result (read+string nil nil nil true)]
        (record-result! "clojure.core/read+string" "(read+string nil nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read+string" "(read+string nil nil nil true)" nil e)))))


(deftest test-clojure-core-read-line
  (testing "clojure.core/read-line"
    ;; Arities: [[]]
    (try
      (let [result (read-line)]
        (record-result! "clojure.core/read-line" "(read-line)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-line" "(read-line)" nil e)))))


(deftest test-clojure-core-read-string
  (testing "clojure.core/read-string"
    ;; Arities: [[s] [opts s]]
    (try
      (let [result (read-string "")]
        (record-result! "clojure.core/read-string" "(read-string \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-string" "(read-string \"\")" nil e)))
    (try
      (let [result (read-string "a")]
        (record-result! "clojure.core/read-string" "(read-string \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-string" "(read-string \"a\")" nil e)))
    (try
      (let [result (read-string nil "")]
        (record-result! "clojure.core/read-string" "(read-string nil \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-string" "(read-string nil \"\")" nil e)))
    (try
      (let [result (read-string nil "a")]
        (record-result! "clojure.core/read-string" "(read-string nil \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-string" "(read-string nil \"a\")" nil e)))
    (try
      (let [result (read-string true "")]
        (record-result! "clojure.core/read-string" "(read-string true \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/read-string" "(read-string true \"\")" nil e)))))


(deftest test-clojure-core-reader-conditional
  (testing "clojure.core/reader-conditional"
    ;; Arities: [[form splicing?]]
    (try
      (let [result (reader-conditional nil nil)]
        (record-result! "clojure.core/reader-conditional" "(reader-conditional nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reader-conditional" "(reader-conditional nil nil)" nil e)))
    (try
      (let [result (reader-conditional nil true)]
        (record-result! "clojure.core/reader-conditional" "(reader-conditional nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reader-conditional" "(reader-conditional nil true)" nil e)))
    (try
      (let [result (reader-conditional true nil)]
        (record-result! "clojure.core/reader-conditional" "(reader-conditional true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reader-conditional" "(reader-conditional true nil)" nil e)))))


(deftest test-clojure-core-reader-conditional-p
  (testing "clojure.core/reader-conditional?"
    ;; Arities: [[value]]
    (try
      (let [result (reader-conditional? nil)]
        (record-result! "clojure.core/reader-conditional?" "(reader-conditional? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reader-conditional?" "(reader-conditional? nil)" nil e)))
    (try
      (let [result (reader-conditional? true)]
        (record-result! "clojure.core/reader-conditional?" "(reader-conditional? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reader-conditional?" "(reader-conditional? true)" nil e)))))


(deftest test-clojure-core-realized-p
  (testing "clojure.core/realized?"
    ;; Arities: [[x]]
    (try
      (let [result (realized? nil)]
        (record-result! "clojure.core/realized?" "(realized? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/realized?" "(realized? nil)" nil e)))
    (try
      (let [result (realized? true)]
        (record-result! "clojure.core/realized?" "(realized? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/realized?" "(realized? true)" nil e)))))


(deftest test-clojure-core-reduce
  (testing "clojure.core/reduce"
    ;; Arities: [[f coll] [f val coll]]
    (try
      (let [result (reduce 'inc nil)]
        (record-result! "clojure.core/reduce" "(reduce 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'inc nil)" nil e)))
    (try
      (let [result (reduce 'inc [])]
        (record-result! "clojure.core/reduce" "(reduce 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'inc [])" nil e)))
    (try
      (let [result (reduce 'dec nil)]
        (record-result! "clojure.core/reduce" "(reduce 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'dec nil)" nil e)))
    (try
      (let [result (reduce 'inc nil nil)]
        (record-result! "clojure.core/reduce" "(reduce 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'inc nil nil)" nil e)))
    (try
      (let [result (reduce 'inc nil [])]
        (record-result! "clojure.core/reduce" "(reduce 'inc nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'inc nil [])" nil e)))
    (try
      (let [result (reduce 'inc true nil)]
        (record-result! "clojure.core/reduce" "(reduce 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce" "(reduce 'inc true nil)" nil e)))))


(deftest test-clojure-core-reduce-kv
  (testing "clojure.core/reduce-kv"
    ;; Arities: [[f init coll]]
    (try
      (let [result (reduce-kv 'inc nil nil)]
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc nil nil)" nil e)))
    (try
      (let [result (reduce-kv 'inc nil [])]
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc nil [])" nil e)))
    (try
      (let [result (reduce-kv 'inc true nil)]
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduce-kv" "(reduce-kv 'inc true nil)" nil e)))))


(deftest test-clojure-core-reduced
  (testing "clojure.core/reduced"
    ;; Arities: [[x]]
    (try
      (let [result (reduced nil)]
        (record-result! "clojure.core/reduced" "(reduced nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduced" "(reduced nil)" nil e)))
    (try
      (let [result (reduced true)]
        (record-result! "clojure.core/reduced" "(reduced true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduced" "(reduced true)" nil e)))))


(deftest test-clojure-core-reduced-p
  (testing "clojure.core/reduced?"
    ;; Arities: [[x]]
    (try
      (let [result (reduced? nil)]
        (record-result! "clojure.core/reduced?" "(reduced? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduced?" "(reduced? nil)" nil e)))
    (try
      (let [result (reduced? true)]
        (record-result! "clojure.core/reduced?" "(reduced? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reduced?" "(reduced? true)" nil e)))))


(deftest test-clojure-core-reductions
  (testing "clojure.core/reductions"
    ;; Arities: [[f coll] [f init coll]]
    (try
      (let [result (reductions 'inc nil)]
        (record-result! "clojure.core/reductions" "(reductions 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'inc nil)" nil e)))
    (try
      (let [result (reductions 'inc [])]
        (record-result! "clojure.core/reductions" "(reductions 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'inc [])" nil e)))
    (try
      (let [result (reductions 'dec nil)]
        (record-result! "clojure.core/reductions" "(reductions 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'dec nil)" nil e)))
    (try
      (let [result (reductions 'inc nil nil)]
        (record-result! "clojure.core/reductions" "(reductions 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'inc nil nil)" nil e)))
    (try
      (let [result (reductions 'inc nil [])]
        (record-result! "clojure.core/reductions" "(reductions 'inc nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'inc nil [])" nil e)))
    (try
      (let [result (reductions 'inc true nil)]
        (record-result! "clojure.core/reductions" "(reductions 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reductions" "(reductions 'inc true nil)" nil e)))))


(deftest test-clojure-core-ref
  (testing "clojure.core/ref"
    ;; Arities: [[x] [x & options]]
    (try
      (let [result (ref nil)]
        (record-result! "clojure.core/ref" "(ref nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref" "(ref nil)" nil e)))
    (try
      (let [result (ref true)]
        (record-result! "clojure.core/ref" "(ref true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref" "(ref true)" nil e)))
    (try
      (let [result (ref nil)]
        (record-result! "clojure.core/ref" "(ref nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref" "(ref nil)" nil e)))
    (try
      (let [result (ref nil nil)]
        (record-result! "clojure.core/ref" "(ref nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref" "(ref nil nil)" nil e)))
    (try
      (let [result (ref nil nil nil)]
        (record-result! "clojure.core/ref" "(ref nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref" "(ref nil nil nil)" nil e)))))


(deftest test-clojure-core-ref-history-count
  (testing "clojure.core/ref-history-count"
    ;; Arities: [[ref]]
    (try
      (let [result (ref-history-count nil)]
        (record-result! "clojure.core/ref-history-count" "(ref-history-count nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-history-count" "(ref-history-count nil)" nil e)))
    (try
      (let [result (ref-history-count true)]
        (record-result! "clojure.core/ref-history-count" "(ref-history-count true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-history-count" "(ref-history-count true)" nil e)))))


(deftest test-clojure-core-ref-max-history
  (testing "clojure.core/ref-max-history"
    ;; Arities: [[ref] [ref n]]
    (try
      (let [result (ref-max-history nil)]
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil)" nil e)))
    (try
      (let [result (ref-max-history true)]
        (record-result! "clojure.core/ref-max-history" "(ref-max-history true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-max-history" "(ref-max-history true)" nil e)))
    (try
      (let [result (ref-max-history nil 0)]
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil 0)" nil e)))
    (try
      (let [result (ref-max-history nil 1)]
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-max-history" "(ref-max-history nil 1)" nil e)))
    (try
      (let [result (ref-max-history true 0)]
        (record-result! "clojure.core/ref-max-history" "(ref-max-history true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-max-history" "(ref-max-history true 0)" nil e)))))


(deftest test-clojure-core-ref-min-history
  (testing "clojure.core/ref-min-history"
    ;; Arities: [[ref] [ref n]]
    (try
      (let [result (ref-min-history nil)]
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil)" nil e)))
    (try
      (let [result (ref-min-history true)]
        (record-result! "clojure.core/ref-min-history" "(ref-min-history true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-min-history" "(ref-min-history true)" nil e)))
    (try
      (let [result (ref-min-history nil 0)]
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil 0)" nil e)))
    (try
      (let [result (ref-min-history nil 1)]
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-min-history" "(ref-min-history nil 1)" nil e)))
    (try
      (let [result (ref-min-history true 0)]
        (record-result! "clojure.core/ref-min-history" "(ref-min-history true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-min-history" "(ref-min-history true 0)" nil e)))))


(deftest test-clojure-core-ref-set
  (testing "clojure.core/ref-set"
    ;; Arities: [[ref val]]
    (try
      (let [result (ref-set nil nil)]
        (record-result! "clojure.core/ref-set" "(ref-set nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-set" "(ref-set nil nil)" nil e)))
    (try
      (let [result (ref-set nil true)]
        (record-result! "clojure.core/ref-set" "(ref-set nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-set" "(ref-set nil true)" nil e)))
    (try
      (let [result (ref-set true nil)]
        (record-result! "clojure.core/ref-set" "(ref-set true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/ref-set" "(ref-set true nil)" nil e)))))


(deftest test-clojure-core-refer
  (testing "clojure.core/refer"
    ;; Arities: [[ns-sym & filters]]
    (try
      (let [result (refer nil)]
        (record-result! "clojure.core/refer" "(refer nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/refer" "(refer nil)" nil e)))
    (try
      (let [result (refer nil nil)]
        (record-result! "clojure.core/refer" "(refer nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/refer" "(refer nil nil)" nil e)))
    (try
      (let [result (refer nil nil nil)]
        (record-result! "clojure.core/refer" "(refer nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/refer" "(refer nil nil nil)" nil e)))))


(deftest test-clojure-core-refer-clojure
  (testing "clojure.core/refer-clojure"
    ;; Arities: [[& filters]]
    (try
      (let [result (refer-clojure)]
        (record-result! "clojure.core/refer-clojure" "(refer-clojure)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/refer-clojure" "(refer-clojure)" nil e)))))


(deftest test-clojure-core-release-pending-sends
  (testing "clojure.core/release-pending-sends"
    ;; Arities: [[]]
    (try
      (let [result (release-pending-sends)]
        (record-result! "clojure.core/release-pending-sends" "(release-pending-sends)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/release-pending-sends" "(release-pending-sends)" nil e)))))


(deftest test-clojure-core-rem
  (testing "clojure.core/rem"
    ;; Arities: [[num div]]
    (try
      (let [result (rem 0 nil)]
        (record-result! "clojure.core/rem" "(rem 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rem" "(rem 0 nil)" nil e)))
    (try
      (let [result (rem 0 true)]
        (record-result! "clojure.core/rem" "(rem 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rem" "(rem 0 true)" nil e)))
    (try
      (let [result (rem 1 nil)]
        (record-result! "clojure.core/rem" "(rem 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rem" "(rem 1 nil)" nil e)))))


(deftest test-clojure-core-remove
  (testing "clojure.core/remove"
    ;; Arities: [[pred] [pred coll]]
    (try
      (let [result (remove 'even?)]
        (record-result! "clojure.core/remove" "(remove 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove" "(remove 'even?)" nil e)))
    (try
      (let [result (remove 'odd?)]
        (record-result! "clojure.core/remove" "(remove 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove" "(remove 'odd?)" nil e)))
    (try
      (let [result (remove 'even? nil)]
        (record-result! "clojure.core/remove" "(remove 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove" "(remove 'even? nil)" nil e)))
    (try
      (let [result (remove 'even? [])]
        (record-result! "clojure.core/remove" "(remove 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove" "(remove 'even? [])" nil e)))
    (try
      (let [result (remove 'odd? nil)]
        (record-result! "clojure.core/remove" "(remove 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove" "(remove 'odd? nil)" nil e)))))


(deftest test-clojure-core-remove-all-methods
  (testing "clojure.core/remove-all-methods"
    ;; Arities: [[multifn]]
    (try
      (let [result (remove-all-methods nil)]
        (record-result! "clojure.core/remove-all-methods" "(remove-all-methods nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-all-methods" "(remove-all-methods nil)" nil e)))
    (try
      (let [result (remove-all-methods true)]
        (record-result! "clojure.core/remove-all-methods" "(remove-all-methods true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-all-methods" "(remove-all-methods true)" nil e)))))


(deftest test-clojure-core-remove-method
  (testing "clojure.core/remove-method"
    ;; Arities: [[multifn dispatch-val]]
    (try
      (let [result (remove-method nil nil)]
        (record-result! "clojure.core/remove-method" "(remove-method nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-method" "(remove-method nil nil)" nil e)))
    (try
      (let [result (remove-method nil true)]
        (record-result! "clojure.core/remove-method" "(remove-method nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-method" "(remove-method nil true)" nil e)))
    (try
      (let [result (remove-method true nil)]
        (record-result! "clojure.core/remove-method" "(remove-method true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-method" "(remove-method true nil)" nil e)))))


(deftest test-clojure-core-remove-ns
  (testing "clojure.core/remove-ns"
    ;; Arities: [[sym]]
    (try
      (let [result (remove-ns nil)]
        (record-result! "clojure.core/remove-ns" "(remove-ns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-ns" "(remove-ns nil)" nil e)))
    (try
      (let [result (remove-ns true)]
        (record-result! "clojure.core/remove-ns" "(remove-ns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-ns" "(remove-ns true)" nil e)))))


(deftest test-clojure-core-remove-tap
  (testing "clojure.core/remove-tap"
    ;; Arities: [[f]]
    (try
      (let [result (remove-tap 'inc)]
        (record-result! "clojure.core/remove-tap" "(remove-tap 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-tap" "(remove-tap 'inc)" nil e)))
    (try
      (let [result (remove-tap 'dec)]
        (record-result! "clojure.core/remove-tap" "(remove-tap 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-tap" "(remove-tap 'dec)" nil e)))))


(deftest test-clojure-core-remove-watch
  (testing "clojure.core/remove-watch"
    ;; Arities: [[reference key]]
    (try
      (let [result (remove-watch nil :a)]
        (record-result! "clojure.core/remove-watch" "(remove-watch nil :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-watch" "(remove-watch nil :a)" nil e)))
    (try
      (let [result (remove-watch nil :b)]
        (record-result! "clojure.core/remove-watch" "(remove-watch nil :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-watch" "(remove-watch nil :b)" nil e)))
    (try
      (let [result (remove-watch true :a)]
        (record-result! "clojure.core/remove-watch" "(remove-watch true :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/remove-watch" "(remove-watch true :a)" nil e)))))


(deftest test-clojure-core-repeat
  (testing "clojure.core/repeat"
    ;; Arities: [[x] [n x]]
    (try
      (let [result (repeat nil)]
        (record-result! "clojure.core/repeat" "(repeat nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeat" "(repeat nil)" nil e)))
    (try
      (let [result (repeat true)]
        (record-result! "clojure.core/repeat" "(repeat true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeat" "(repeat true)" nil e)))
    (try
      (let [result (repeat 0 nil)]
        (record-result! "clojure.core/repeat" "(repeat 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeat" "(repeat 0 nil)" nil e)))
    (try
      (let [result (repeat 0 true)]
        (record-result! "clojure.core/repeat" "(repeat 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeat" "(repeat 0 true)" nil e)))
    (try
      (let [result (repeat 1 nil)]
        (record-result! "clojure.core/repeat" "(repeat 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeat" "(repeat 1 nil)" nil e)))))


(deftest test-clojure-core-repeatedly
  (testing "clojure.core/repeatedly"
    ;; Arities: [[f] [n f]]
    (try
      (let [result (repeatedly 'inc)]
        (record-result! "clojure.core/repeatedly" "(repeatedly 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeatedly" "(repeatedly 'inc)" nil e)))
    (try
      (let [result (repeatedly 'dec)]
        (record-result! "clojure.core/repeatedly" "(repeatedly 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeatedly" "(repeatedly 'dec)" nil e)))
    (try
      (let [result (repeatedly 0 'inc)]
        (record-result! "clojure.core/repeatedly" "(repeatedly 0 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeatedly" "(repeatedly 0 'inc)" nil e)))
    (try
      (let [result (repeatedly 0 'dec)]
        (record-result! "clojure.core/repeatedly" "(repeatedly 0 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeatedly" "(repeatedly 0 'dec)" nil e)))
    (try
      (let [result (repeatedly 1 'inc)]
        (record-result! "clojure.core/repeatedly" "(repeatedly 1 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/repeatedly" "(repeatedly 1 'inc)" nil e)))))


(deftest test-clojure-core-replace
  (testing "clojure.core/replace"
    ;; Arities: [[smap] [smap coll]]
    (try
      (let [result (replace nil)]
        (record-result! "clojure.core/replace" "(replace nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replace" "(replace nil)" nil e)))
    (try
      (let [result (replace true)]
        (record-result! "clojure.core/replace" "(replace true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replace" "(replace true)" nil e)))
    (try
      (let [result (replace nil nil)]
        (record-result! "clojure.core/replace" "(replace nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replace" "(replace nil nil)" nil e)))
    (try
      (let [result (replace nil [])]
        (record-result! "clojure.core/replace" "(replace nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replace" "(replace nil [])" nil e)))
    (try
      (let [result (replace true nil)]
        (record-result! "clojure.core/replace" "(replace true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replace" "(replace true nil)" nil e)))))


(deftest test-clojure-core-replicate
  (testing "clojure.core/replicate"
    ;; Arities: [[n x]]
    (try
      (let [result (replicate 0 nil)]
        (record-result! "clojure.core/replicate" "(replicate 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replicate" "(replicate 0 nil)" nil e)))
    (try
      (let [result (replicate 0 true)]
        (record-result! "clojure.core/replicate" "(replicate 0 true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replicate" "(replicate 0 true)" nil e)))
    (try
      (let [result (replicate 1 nil)]
        (record-result! "clojure.core/replicate" "(replicate 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/replicate" "(replicate 1 nil)" nil e)))))


(deftest test-clojure-core-require
  (testing "clojure.core/require"
    ;; Arities: [[& args]]
    (try
      (let [result (require)]
        (record-result! "clojure.core/require" "(require)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/require" "(require)" nil e)))))


(deftest test-clojure-core-requiring-resolve
  (testing "clojure.core/requiring-resolve"
    ;; Arities: [[sym]]
    (try
      (let [result (requiring-resolve nil)]
        (record-result! "clojure.core/requiring-resolve" "(requiring-resolve nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/requiring-resolve" "(requiring-resolve nil)" nil e)))
    (try
      (let [result (requiring-resolve true)]
        (record-result! "clojure.core/requiring-resolve" "(requiring-resolve true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/requiring-resolve" "(requiring-resolve true)" nil e)))))


(deftest test-clojure-core-reset-bang
  (testing "clojure.core/reset!"
    ;; Arities: [[atom newval]]
    (try
      (let [result (reset! nil nil)]
        (record-result! "clojure.core/reset!" "(reset! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset!" "(reset! nil nil)" nil e)))
    (try
      (let [result (reset! nil true)]
        (record-result! "clojure.core/reset!" "(reset! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset!" "(reset! nil true)" nil e)))
    (try
      (let [result (reset! true nil)]
        (record-result! "clojure.core/reset!" "(reset! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset!" "(reset! true nil)" nil e)))))


(deftest test-clojure-core-reset-meta-bang
  (testing "clojure.core/reset-meta!"
    ;; Arities: [[iref metadata-map]]
    (try
      (let [result (reset-meta! nil nil)]
        (record-result! "clojure.core/reset-meta!" "(reset-meta! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-meta!" "(reset-meta! nil nil)" nil e)))
    (try
      (let [result (reset-meta! nil true)]
        (record-result! "clojure.core/reset-meta!" "(reset-meta! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-meta!" "(reset-meta! nil true)" nil e)))
    (try
      (let [result (reset-meta! true nil)]
        (record-result! "clojure.core/reset-meta!" "(reset-meta! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-meta!" "(reset-meta! true nil)" nil e)))))


(deftest test-clojure-core-reset-vals-bang
  (testing "clojure.core/reset-vals!"
    ;; Arities: [[atom newval]]
    (try
      (let [result (reset-vals! nil nil)]
        (record-result! "clojure.core/reset-vals!" "(reset-vals! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-vals!" "(reset-vals! nil nil)" nil e)))
    (try
      (let [result (reset-vals! nil true)]
        (record-result! "clojure.core/reset-vals!" "(reset-vals! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-vals!" "(reset-vals! nil true)" nil e)))
    (try
      (let [result (reset-vals! true nil)]
        (record-result! "clojure.core/reset-vals!" "(reset-vals! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reset-vals!" "(reset-vals! true nil)" nil e)))))


(deftest test-clojure-core-resolve
  (testing "clojure.core/resolve"
    ;; Arities: [[sym] [env sym]]
    (try
      (let [result (resolve nil)]
        (record-result! "clojure.core/resolve" "(resolve nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resolve" "(resolve nil)" nil e)))
    (try
      (let [result (resolve true)]
        (record-result! "clojure.core/resolve" "(resolve true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resolve" "(resolve true)" nil e)))
    (try
      (let [result (resolve nil nil)]
        (record-result! "clojure.core/resolve" "(resolve nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resolve" "(resolve nil nil)" nil e)))
    (try
      (let [result (resolve nil true)]
        (record-result! "clojure.core/resolve" "(resolve nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resolve" "(resolve nil true)" nil e)))
    (try
      (let [result (resolve true nil)]
        (record-result! "clojure.core/resolve" "(resolve true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resolve" "(resolve true nil)" nil e)))))


(deftest test-clojure-core-rest
  (testing "clojure.core/rest"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (rest)]
        (record-result! "clojure.core/rest" "(rest)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rest" "(rest)" nil e)))
    (try
      (let [result (rest)]
        (record-result! "clojure.core/rest" "(rest)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rest" "(rest)" nil e)))))


(deftest test-clojure-core-restart-agent
  (testing "clojure.core/restart-agent"
    ;; Arities: [[a new-state & options]]
    (try
      (let [result (restart-agent nil nil)]
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil)" nil e)))
    (try
      (let [result (restart-agent nil nil nil)]
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil nil)" nil e)))
    (try
      (let [result (restart-agent nil nil nil nil)]
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/restart-agent" "(restart-agent nil nil nil nil)" nil e)))))


(deftest test-clojure-core-resultset-seq
  (testing "clojure.core/resultset-seq"
    ;; Arities: [[rs]]
    (try
      (let [result (resultset-seq nil)]
        (record-result! "clojure.core/resultset-seq" "(resultset-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resultset-seq" "(resultset-seq nil)" nil e)))
    (try
      (let [result (resultset-seq true)]
        (record-result! "clojure.core/resultset-seq" "(resultset-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/resultset-seq" "(resultset-seq true)" nil e)))))


(deftest test-clojure-core-reverse
  (testing "clojure.core/reverse"
    ;; Arities: [[coll]]
    (try
      (let [result (reverse nil)]
        (record-result! "clojure.core/reverse" "(reverse nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reverse" "(reverse nil)" nil e)))
    (try
      (let [result (reverse [])]
        (record-result! "clojure.core/reverse" "(reverse [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reverse" "(reverse [])" nil e)))))


(deftest test-clojure-core-reversible-p
  (testing "clojure.core/reversible?"
    ;; Arities: [[coll]]
    (try
      (let [result (reversible? nil)]
        (record-result! "clojure.core/reversible?" "(reversible? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reversible?" "(reversible? nil)" nil e)))
    (try
      (let [result (reversible? [])]
        (record-result! "clojure.core/reversible?" "(reversible? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/reversible?" "(reversible? [])" nil e)))))


(deftest test-clojure-core-rseq
  (testing "clojure.core/rseq"
    ;; Arities: [[rev]]
    (try
      (let [result (rseq nil)]
        (record-result! "clojure.core/rseq" "(rseq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rseq" "(rseq nil)" nil e)))
    (try
      (let [result (rseq true)]
        (record-result! "clojure.core/rseq" "(rseq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rseq" "(rseq true)" nil e)))))


(deftest test-clojure-core-rsubseq
  (testing "clojure.core/rsubseq"
    ;; Arities: [[sc test key] [sc start-test start-key end-test end-key]]
    (try
      (let [result (rsubseq nil nil :a)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil :a)" nil e)))
    (try
      (let [result (rsubseq nil nil :b)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil :b)" nil e)))
    (try
      (let [result (rsubseq nil true :a)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil true :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil true :a)" nil e)))
    (try
      (let [result (rsubseq nil nil nil nil nil)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil nil nil)" nil e)))
    (try
      (let [result (rsubseq nil nil nil nil true)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil nil true)" nil e)))
    (try
      (let [result (rsubseq nil nil nil true nil)]
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/rsubseq" "(rsubseq nil nil nil true nil)" nil e)))))


(deftest test-clojure-core-run-bang
  (testing "clojure.core/run!"
    ;; Arities: [[proc coll]]
    (try
      (let [result (run! nil nil)]
        (record-result! "clojure.core/run!" "(run! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/run!" "(run! nil nil)" nil e)))
    (try
      (let [result (run! nil [])]
        (record-result! "clojure.core/run!" "(run! nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/run!" "(run! nil [])" nil e)))
    (try
      (let [result (run! true nil)]
        (record-result! "clojure.core/run!" "(run! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/run!" "(run! true nil)" nil e)))))


(deftest test-clojure-core-second
  (testing "clojure.core/second"
    ;; Arities: [quote ([x])]
    (try
      (let [result (second)]
        (record-result! "clojure.core/second" "(second)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/second" "(second)" nil e)))
    (try
      (let [result (second)]
        (record-result! "clojure.core/second" "(second)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/second" "(second)" nil e)))))


(deftest test-clojure-core-select-keys
  (testing "clojure.core/select-keys"
    ;; Arities: [[map keyseq]]
    (try
      (let [result (select-keys {} nil)]
        (record-result! "clojure.core/select-keys" "(select-keys {} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/select-keys" "(select-keys {} nil)" nil e)))
    (try
      (let [result (select-keys {} true)]
        (record-result! "clojure.core/select-keys" "(select-keys {} true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/select-keys" "(select-keys {} true)" nil e)))
    (try
      (let [result (select-keys {:a 1} nil)]
        (record-result! "clojure.core/select-keys" "(select-keys {:a 1} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/select-keys" "(select-keys {:a 1} nil)" nil e)))))


(deftest test-clojure-core-send
  (testing "clojure.core/send"
    ;; Arities: [[a f & args]]
    (try
      (let [result (send nil 'inc)]
        (record-result! "clojure.core/send" "(send nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send" "(send nil 'inc)" nil e)))
    (try
      (let [result (send nil 'inc nil)]
        (record-result! "clojure.core/send" "(send nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send" "(send nil 'inc nil)" nil e)))
    (try
      (let [result (send nil 'inc nil nil)]
        (record-result! "clojure.core/send" "(send nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send" "(send nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-send-off
  (testing "clojure.core/send-off"
    ;; Arities: [[a f & args]]
    (try
      (let [result (send-off nil 'inc)]
        (record-result! "clojure.core/send-off" "(send-off nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-off" "(send-off nil 'inc)" nil e)))
    (try
      (let [result (send-off nil 'inc nil)]
        (record-result! "clojure.core/send-off" "(send-off nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-off" "(send-off nil 'inc nil)" nil e)))
    (try
      (let [result (send-off nil 'inc nil nil)]
        (record-result! "clojure.core/send-off" "(send-off nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-off" "(send-off nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-send-via
  (testing "clojure.core/send-via"
    ;; Arities: [[executor a f & args]]
    (try
      (let [result (send-via nil nil 'inc)]
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc)" nil e)))
    (try
      (let [result (send-via nil nil 'inc nil)]
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc nil)" nil e)))
    (try
      (let [result (send-via nil nil 'inc nil nil)]
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/send-via" "(send-via nil nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-seq
  (testing "clojure.core/seq"
    ;; Arities: [quote ([coll])]
    (try
      (let [result (seq)]
        (record-result! "clojure.core/seq" "(seq)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq" "(seq)" nil e)))
    (try
      (let [result (seq)]
        (record-result! "clojure.core/seq" "(seq)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq" "(seq)" nil e)))))


(deftest test-clojure-core-seq-to-map-for-destructuring
  (testing "clojure.core/seq-to-map-for-destructuring"
    ;; Arities: [[s]]
    (try
      (let [result (seq-to-map-for-destructuring "")]
        (record-result! "clojure.core/seq-to-map-for-destructuring" "(seq-to-map-for-destructuring \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq-to-map-for-destructuring" "(seq-to-map-for-destructuring \"\")" nil e)))
    (try
      (let [result (seq-to-map-for-destructuring "a")]
        (record-result! "clojure.core/seq-to-map-for-destructuring" "(seq-to-map-for-destructuring \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq-to-map-for-destructuring" "(seq-to-map-for-destructuring \"a\")" nil e)))))


(deftest test-clojure-core-seq-p
  (testing "clojure.core/seq?"
    ;; Arities: [quote ([x])]
    (try
      (let [result (seq?)]
        (record-result! "clojure.core/seq?" "(seq?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq?" "(seq?)" nil e)))
    (try
      (let [result (seq?)]
        (record-result! "clojure.core/seq?" "(seq?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seq?" "(seq?)" nil e)))))


(deftest test-clojure-core-seqable-p
  (testing "clojure.core/seqable?"
    ;; Arities: [[x]]
    (try
      (let [result (seqable? nil)]
        (record-result! "clojure.core/seqable?" "(seqable? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seqable?" "(seqable? nil)" nil e)))
    (try
      (let [result (seqable? true)]
        (record-result! "clojure.core/seqable?" "(seqable? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seqable?" "(seqable? true)" nil e)))))


(deftest test-clojure-core-seque
  (testing "clojure.core/seque"
    ;; Arities: [[s] [n-or-q s]]
    (try
      (let [result (seque "")]
        (record-result! "clojure.core/seque" "(seque \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seque" "(seque \"\")" nil e)))
    (try
      (let [result (seque "a")]
        (record-result! "clojure.core/seque" "(seque \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seque" "(seque \"a\")" nil e)))
    (try
      (let [result (seque nil "")]
        (record-result! "clojure.core/seque" "(seque nil \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seque" "(seque nil \"\")" nil e)))
    (try
      (let [result (seque nil "a")]
        (record-result! "clojure.core/seque" "(seque nil \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seque" "(seque nil \"a\")" nil e)))
    (try
      (let [result (seque true "")]
        (record-result! "clojure.core/seque" "(seque true \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/seque" "(seque true \"\")" nil e)))))


(deftest test-clojure-core-sequence
  (testing "clojure.core/sequence"
    ;; Arities: [[coll] [xform coll] [xform coll & colls]]
    (try
      (let [result (sequence nil)]
        (record-result! "clojure.core/sequence" "(sequence nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil)" nil e)))
    (try
      (let [result (sequence [])]
        (record-result! "clojure.core/sequence" "(sequence [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence [])" nil e)))
    (try
      (let [result (sequence nil nil)]
        (record-result! "clojure.core/sequence" "(sequence nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil nil)" nil e)))
    (try
      (let [result (sequence nil [])]
        (record-result! "clojure.core/sequence" "(sequence nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil [])" nil e)))
    (try
      (let [result (sequence true nil)]
        (record-result! "clojure.core/sequence" "(sequence true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence true nil)" nil e)))
    (try
      (let [result (sequence nil nil)]
        (record-result! "clojure.core/sequence" "(sequence nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil nil)" nil e)))
    (try
      (let [result (sequence nil nil nil)]
        (record-result! "clojure.core/sequence" "(sequence nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil nil nil)" nil e)))
    (try
      (let [result (sequence nil nil nil nil)]
        (record-result! "clojure.core/sequence" "(sequence nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequence" "(sequence nil nil nil nil)" nil e)))))


(deftest test-clojure-core-sequential-p
  (testing "clojure.core/sequential?"
    ;; Arities: [[coll]]
    (try
      (let [result (sequential? nil)]
        (record-result! "clojure.core/sequential?" "(sequential? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequential?" "(sequential? nil)" nil e)))
    (try
      (let [result (sequential? [])]
        (record-result! "clojure.core/sequential?" "(sequential? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sequential?" "(sequential? [])" nil e)))))


(deftest test-clojure-core-set
  (testing "clojure.core/set"
    ;; Arities: [[coll]]
    (try
      (let [result (set nil)]
        (record-result! "clojure.core/set" "(set nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set" "(set nil)" nil e)))
    (try
      (let [result (set [])]
        (record-result! "clojure.core/set" "(set [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set" "(set [])" nil e)))))


(deftest test-clojure-core-set-agent-send-executor-bang
  (testing "clojure.core/set-agent-send-executor!"
    ;; Arities: [[executor]]
    (try
      (let [result (set-agent-send-executor! nil)]
        (record-result! "clojure.core/set-agent-send-executor!" "(set-agent-send-executor! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-agent-send-executor!" "(set-agent-send-executor! nil)" nil e)))
    (try
      (let [result (set-agent-send-executor! true)]
        (record-result! "clojure.core/set-agent-send-executor!" "(set-agent-send-executor! true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-agent-send-executor!" "(set-agent-send-executor! true)" nil e)))))


(deftest test-clojure-core-set-agent-send-off-executor-bang
  (testing "clojure.core/set-agent-send-off-executor!"
    ;; Arities: [[executor]]
    (try
      (let [result (set-agent-send-off-executor! nil)]
        (record-result! "clojure.core/set-agent-send-off-executor!" "(set-agent-send-off-executor! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-agent-send-off-executor!" "(set-agent-send-off-executor! nil)" nil e)))
    (try
      (let [result (set-agent-send-off-executor! true)]
        (record-result! "clojure.core/set-agent-send-off-executor!" "(set-agent-send-off-executor! true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-agent-send-off-executor!" "(set-agent-send-off-executor! true)" nil e)))))


(deftest test-clojure-core-set-error-handler-bang
  (testing "clojure.core/set-error-handler!"
    ;; Arities: [[a handler-fn]]
    (try
      (let [result (set-error-handler! nil nil)]
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! nil nil)" nil e)))
    (try
      (let [result (set-error-handler! nil true)]
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! nil true)" nil e)))
    (try
      (let [result (set-error-handler! true nil)]
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-handler!" "(set-error-handler! true nil)" nil e)))))


(deftest test-clojure-core-set-error-mode-bang
  (testing "clojure.core/set-error-mode!"
    ;; Arities: [[a mode-keyword]]
    (try
      (let [result (set-error-mode! nil nil)]
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! nil nil)" nil e)))
    (try
      (let [result (set-error-mode! nil true)]
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! nil true)" nil e)))
    (try
      (let [result (set-error-mode! true nil)]
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-error-mode!" "(set-error-mode! true nil)" nil e)))))


(deftest test-clojure-core-set-validator-bang
  (testing "clojure.core/set-validator!"
    ;; Arities: [[iref validator-fn]]
    (try
      (let [result (set-validator! nil nil)]
        (record-result! "clojure.core/set-validator!" "(set-validator! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-validator!" "(set-validator! nil nil)" nil e)))
    (try
      (let [result (set-validator! nil true)]
        (record-result! "clojure.core/set-validator!" "(set-validator! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-validator!" "(set-validator! nil true)" nil e)))
    (try
      (let [result (set-validator! true nil)]
        (record-result! "clojure.core/set-validator!" "(set-validator! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set-validator!" "(set-validator! true nil)" nil e)))))


(deftest test-clojure-core-set-p
  (testing "clojure.core/set?"
    ;; Arities: [[x]]
    (try
      (let [result (set? nil)]
        (record-result! "clojure.core/set?" "(set? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set?" "(set? nil)" nil e)))
    (try
      (let [result (set? true)]
        (record-result! "clojure.core/set?" "(set? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/set?" "(set? true)" nil e)))))


(deftest test-clojure-core-short
  (testing "clojure.core/short"
    ;; Arities: [[x]]
    (try
      (let [result (short nil)]
        (record-result! "clojure.core/short" "(short nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short" "(short nil)" nil e)))
    (try
      (let [result (short true)]
        (record-result! "clojure.core/short" "(short true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short" "(short true)" nil e)))))


(deftest test-clojure-core-short-array
  (testing "clojure.core/short-array"
    ;; Arities: [[size-or-seq] [size init-val-or-seq]]
    (try
      (let [result (short-array nil)]
        (record-result! "clojure.core/short-array" "(short-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short-array" "(short-array nil)" nil e)))
    (try
      (let [result (short-array true)]
        (record-result! "clojure.core/short-array" "(short-array true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short-array" "(short-array true)" nil e)))
    (try
      (let [result (short-array nil nil)]
        (record-result! "clojure.core/short-array" "(short-array nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short-array" "(short-array nil nil)" nil e)))
    (try
      (let [result (short-array nil true)]
        (record-result! "clojure.core/short-array" "(short-array nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short-array" "(short-array nil true)" nil e)))
    (try
      (let [result (short-array true nil)]
        (record-result! "clojure.core/short-array" "(short-array true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/short-array" "(short-array true nil)" nil e)))))


(deftest test-clojure-core-shuffle
  (testing "clojure.core/shuffle"
    ;; Arities: [[coll]]
    (try
      (let [result (shuffle nil)]
        (record-result! "clojure.core/shuffle" "(shuffle nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/shuffle" "(shuffle nil)" nil e)))
    (try
      (let [result (shuffle [])]
        (record-result! "clojure.core/shuffle" "(shuffle [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/shuffle" "(shuffle [])" nil e)))))


(deftest test-clojure-core-shutdown-agents
  (testing "clojure.core/shutdown-agents"
    ;; Arities: [[]]
    (try
      (let [result (shutdown-agents)]
        (record-result! "clojure.core/shutdown-agents" "(shutdown-agents)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/shutdown-agents" "(shutdown-agents)" nil e)))))


(deftest test-clojure-core-simple-ident-p
  (testing "clojure.core/simple-ident?"
    ;; Arities: [[x]]
    (try
      (let [result (simple-ident? nil)]
        (record-result! "clojure.core/simple-ident?" "(simple-ident? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-ident?" "(simple-ident? nil)" nil e)))
    (try
      (let [result (simple-ident? true)]
        (record-result! "clojure.core/simple-ident?" "(simple-ident? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-ident?" "(simple-ident? true)" nil e)))))


(deftest test-clojure-core-simple-keyword-p
  (testing "clojure.core/simple-keyword?"
    ;; Arities: [[x]]
    (try
      (let [result (simple-keyword? nil)]
        (record-result! "clojure.core/simple-keyword?" "(simple-keyword? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-keyword?" "(simple-keyword? nil)" nil e)))
    (try
      (let [result (simple-keyword? true)]
        (record-result! "clojure.core/simple-keyword?" "(simple-keyword? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-keyword?" "(simple-keyword? true)" nil e)))))


(deftest test-clojure-core-simple-symbol-p
  (testing "clojure.core/simple-symbol?"
    ;; Arities: [[x]]
    (try
      (let [result (simple-symbol? nil)]
        (record-result! "clojure.core/simple-symbol?" "(simple-symbol? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-symbol?" "(simple-symbol? nil)" nil e)))
    (try
      (let [result (simple-symbol? true)]
        (record-result! "clojure.core/simple-symbol?" "(simple-symbol? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/simple-symbol?" "(simple-symbol? true)" nil e)))))


(deftest test-clojure-core-slurp
  (testing "clojure.core/slurp"
    ;; Arities: [[f & opts]]
    (try
      (let [result (slurp 'inc)]
        (record-result! "clojure.core/slurp" "(slurp 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/slurp" "(slurp 'inc)" nil e)))
    (try
      (let [result (slurp 'inc nil)]
        (record-result! "clojure.core/slurp" "(slurp 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/slurp" "(slurp 'inc nil)" nil e)))
    (try
      (let [result (slurp 'inc nil nil)]
        (record-result! "clojure.core/slurp" "(slurp 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/slurp" "(slurp 'inc nil nil)" nil e)))))


(deftest test-clojure-core-some
  (testing "clojure.core/some"
    ;; Arities: [[pred coll]]
    (try
      (let [result (some 'even? nil)]
        (record-result! "clojure.core/some" "(some 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some" "(some 'even? nil)" nil e)))
    (try
      (let [result (some 'even? [])]
        (record-result! "clojure.core/some" "(some 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some" "(some 'even? [])" nil e)))
    (try
      (let [result (some 'odd? nil)]
        (record-result! "clojure.core/some" "(some 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some" "(some 'odd? nil)" nil e)))))


(deftest test-clojure-core-some--gt
  (testing "clojure.core/some->"
    ;; Arities: [[expr & forms]]
    (try
      (let [result (some-> nil)]
        (record-result! "clojure.core/some->" "(some-> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->" "(some-> nil)" nil e)))
    (try
      (let [result (some-> nil nil)]
        (record-result! "clojure.core/some->" "(some-> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->" "(some-> nil nil)" nil e)))
    (try
      (let [result (some-> nil nil nil)]
        (record-result! "clojure.core/some->" "(some-> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->" "(some-> nil nil nil)" nil e)))))


(deftest test-clojure-core-some--gt-gt
  (testing "clojure.core/some->>"
    ;; Arities: [[expr & forms]]
    (try
      (let [result (some->> nil)]
        (record-result! "clojure.core/some->>" "(some->> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->>" "(some->> nil)" nil e)))
    (try
      (let [result (some->> nil nil)]
        (record-result! "clojure.core/some->>" "(some->> nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->>" "(some->> nil nil)" nil e)))
    (try
      (let [result (some->> nil nil nil)]
        (record-result! "clojure.core/some->>" "(some->> nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some->>" "(some->> nil nil nil)" nil e)))))


(deftest test-clojure-core-some-fn
  (testing "clojure.core/some-fn"
    ;; Arities: [[p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]]
    (try
      (let [result (some-fn 'even?)]
        (record-result! "clojure.core/some-fn" "(some-fn 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn 'even?)" nil e)))
    (try
      (let [result (some-fn 'odd?)]
        (record-result! "clojure.core/some-fn" "(some-fn 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn 'odd?)" nil e)))
    (try
      (let [result (some-fn nil nil)]
        (record-result! "clojure.core/some-fn" "(some-fn nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn nil nil)" nil e)))
    (try
      (let [result (some-fn nil true)]
        (record-result! "clojure.core/some-fn" "(some-fn nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn nil true)" nil e)))
    (try
      (let [result (some-fn true nil)]
        (record-result! "clojure.core/some-fn" "(some-fn true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn true nil)" nil e)))
    (try
      (let [result (some-fn nil nil nil)]
        (record-result! "clojure.core/some-fn" "(some-fn nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn nil nil nil)" nil e)))
    (try
      (let [result (some-fn nil nil true)]
        (record-result! "clojure.core/some-fn" "(some-fn nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn nil nil true)" nil e)))
    (try
      (let [result (some-fn nil true nil)]
        (record-result! "clojure.core/some-fn" "(some-fn nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some-fn" "(some-fn nil true nil)" nil e)))))


(deftest test-clojure-core-some-p
  (testing "clojure.core/some?"
    ;; Arities: [[x]]
    (try
      (let [result (some? nil)]
        (record-result! "clojure.core/some?" "(some? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some?" "(some? nil)" nil e)))
    (try
      (let [result (some? true)]
        (record-result! "clojure.core/some?" "(some? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/some?" "(some? true)" nil e)))))


(deftest test-clojure-core-sort
  (testing "clojure.core/sort"
    ;; Arities: [[coll] [comp coll]]
    (try
      (let [result (sort nil)]
        (record-result! "clojure.core/sort" "(sort nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort" "(sort nil)" nil e)))
    (try
      (let [result (sort [])]
        (record-result! "clojure.core/sort" "(sort [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort" "(sort [])" nil e)))
    (try
      (let [result (sort nil nil)]
        (record-result! "clojure.core/sort" "(sort nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort" "(sort nil nil)" nil e)))
    (try
      (let [result (sort nil [])]
        (record-result! "clojure.core/sort" "(sort nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort" "(sort nil [])" nil e)))
    (try
      (let [result (sort true nil)]
        (record-result! "clojure.core/sort" "(sort true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort" "(sort true nil)" nil e)))))


(deftest test-clojure-core-sort-by
  (testing "clojure.core/sort-by"
    ;; Arities: [[keyfn coll] [keyfn comp coll]]
    (try
      (let [result (sort-by nil nil)]
        (record-result! "clojure.core/sort-by" "(sort-by nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by nil nil)" nil e)))
    (try
      (let [result (sort-by nil [])]
        (record-result! "clojure.core/sort-by" "(sort-by nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by nil [])" nil e)))
    (try
      (let [result (sort-by true nil)]
        (record-result! "clojure.core/sort-by" "(sort-by true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by true nil)" nil e)))
    (try
      (let [result (sort-by nil nil nil)]
        (record-result! "clojure.core/sort-by" "(sort-by nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by nil nil nil)" nil e)))
    (try
      (let [result (sort-by nil nil [])]
        (record-result! "clojure.core/sort-by" "(sort-by nil nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by nil nil [])" nil e)))
    (try
      (let [result (sort-by nil true nil)]
        (record-result! "clojure.core/sort-by" "(sort-by nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sort-by" "(sort-by nil true nil)" nil e)))))


(deftest test-clojure-core-sorted-map
  (testing "clojure.core/sorted-map"
    ;; Arities: [[& keyvals]]
    (try
      (let [result (sorted-map)]
        (record-result! "clojure.core/sorted-map" "(sorted-map)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-map" "(sorted-map)" nil e)))))


(deftest test-clojure-core-sorted-map-by
  (testing "clojure.core/sorted-map-by"
    ;; Arities: [[comparator & keyvals]]
    (try
      (let [result (sorted-map-by nil)]
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil)" nil e)))
    (try
      (let [result (sorted-map-by nil nil)]
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil nil)" nil e)))
    (try
      (let [result (sorted-map-by nil nil nil)]
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-map-by" "(sorted-map-by nil nil nil)" nil e)))))


(deftest test-clojure-core-sorted-set
  (testing "clojure.core/sorted-set"
    ;; Arities: [[& keys]]
    (try
      (let [result (sorted-set)]
        (record-result! "clojure.core/sorted-set" "(sorted-set)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-set" "(sorted-set)" nil e)))))


(deftest test-clojure-core-sorted-set-by
  (testing "clojure.core/sorted-set-by"
    ;; Arities: [[comparator & keys]]
    (try
      (let [result (sorted-set-by nil)]
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil)" nil e)))
    (try
      (let [result (sorted-set-by nil nil)]
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil nil)" nil e)))
    (try
      (let [result (sorted-set-by nil nil nil)]
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted-set-by" "(sorted-set-by nil nil nil)" nil e)))))


(deftest test-clojure-core-sorted-p
  (testing "clojure.core/sorted?"
    ;; Arities: [[coll]]
    (try
      (let [result (sorted? nil)]
        (record-result! "clojure.core/sorted?" "(sorted? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted?" "(sorted? nil)" nil e)))
    (try
      (let [result (sorted? [])]
        (record-result! "clojure.core/sorted?" "(sorted? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sorted?" "(sorted? [])" nil e)))))


(deftest test-clojure-core-special-symbol-p
  (testing "clojure.core/special-symbol?"
    ;; Arities: [[s]]
    (try
      (let [result (special-symbol? "")]
        (record-result! "clojure.core/special-symbol?" "(special-symbol? \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/special-symbol?" "(special-symbol? \"\")" nil e)))
    (try
      (let [result (special-symbol? "a")]
        (record-result! "clojure.core/special-symbol?" "(special-symbol? \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/special-symbol?" "(special-symbol? \"a\")" nil e)))))


(deftest test-clojure-core-spit
  (testing "clojure.core/spit"
    ;; Arities: [[f content & options]]
    (try
      (let [result (spit 'inc nil)]
        (record-result! "clojure.core/spit" "(spit 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/spit" "(spit 'inc nil)" nil e)))
    (try
      (let [result (spit 'inc nil nil)]
        (record-result! "clojure.core/spit" "(spit 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/spit" "(spit 'inc nil nil)" nil e)))
    (try
      (let [result (spit 'inc nil nil nil)]
        (record-result! "clojure.core/spit" "(spit 'inc nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/spit" "(spit 'inc nil nil nil)" nil e)))))


(deftest test-clojure-core-split-at
  (testing "clojure.core/split-at"
    ;; Arities: [[n coll]]
    (try
      (let [result (split-at 0 nil)]
        (record-result! "clojure.core/split-at" "(split-at 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-at" "(split-at 0 nil)" nil e)))
    (try
      (let [result (split-at 0 [])]
        (record-result! "clojure.core/split-at" "(split-at 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-at" "(split-at 0 [])" nil e)))
    (try
      (let [result (split-at 1 nil)]
        (record-result! "clojure.core/split-at" "(split-at 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-at" "(split-at 1 nil)" nil e)))))


(deftest test-clojure-core-split-with
  (testing "clojure.core/split-with"
    ;; Arities: [[pred coll]]
    (try
      (let [result (split-with 'even? nil)]
        (record-result! "clojure.core/split-with" "(split-with 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-with" "(split-with 'even? nil)" nil e)))
    (try
      (let [result (split-with 'even? [])]
        (record-result! "clojure.core/split-with" "(split-with 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-with" "(split-with 'even? [])" nil e)))
    (try
      (let [result (split-with 'odd? nil)]
        (record-result! "clojure.core/split-with" "(split-with 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/split-with" "(split-with 'odd? nil)" nil e)))))


(deftest test-clojure-core-splitv-at
  (testing "clojure.core/splitv-at"
    ;; Arities: [[n coll]]
    (try
      (let [result (splitv-at 0 nil)]
        (record-result! "clojure.core/splitv-at" "(splitv-at 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/splitv-at" "(splitv-at 0 nil)" nil e)))
    (try
      (let [result (splitv-at 0 [])]
        (record-result! "clojure.core/splitv-at" "(splitv-at 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/splitv-at" "(splitv-at 0 [])" nil e)))
    (try
      (let [result (splitv-at 1 nil)]
        (record-result! "clojure.core/splitv-at" "(splitv-at 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/splitv-at" "(splitv-at 1 nil)" nil e)))))


(deftest test-clojure-core-str
  (testing "clojure.core/str"
    ;; Arities: [[] [x] [x & ys]]
    (try
      (let [result (str)]
        (record-result! "clojure.core/str" "(str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str)" nil e)))
    (try
      (let [result (str nil)]
        (record-result! "clojure.core/str" "(str nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str nil)" nil e)))
    (try
      (let [result (str true)]
        (record-result! "clojure.core/str" "(str true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str true)" nil e)))
    (try
      (let [result (str nil)]
        (record-result! "clojure.core/str" "(str nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str nil)" nil e)))
    (try
      (let [result (str nil nil)]
        (record-result! "clojure.core/str" "(str nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str nil nil)" nil e)))
    (try
      (let [result (str nil nil nil)]
        (record-result! "clojure.core/str" "(str nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/str" "(str nil nil nil)" nil e)))))


(deftest test-clojure-core-stream-into-bang
  (testing "clojure.core/stream-into!"
    ;; Arities: [[to stream] [to xform stream]]
    (try
      (let [result (stream-into! nil nil)]
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil)" nil e)))
    (try
      (let [result (stream-into! nil true)]
        (record-result! "clojure.core/stream-into!" "(stream-into! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! nil true)" nil e)))
    (try
      (let [result (stream-into! true nil)]
        (record-result! "clojure.core/stream-into!" "(stream-into! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! true nil)" nil e)))
    (try
      (let [result (stream-into! nil nil nil)]
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil nil)" nil e)))
    (try
      (let [result (stream-into! nil nil true)]
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! nil nil true)" nil e)))
    (try
      (let [result (stream-into! nil true nil)]
        (record-result! "clojure.core/stream-into!" "(stream-into! nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-into!" "(stream-into! nil true nil)" nil e)))))


(deftest test-clojure-core-stream-reduce-bang
  (testing "clojure.core/stream-reduce!"
    ;; Arities: [[f s] [f init s]]
    (try
      (let [result (stream-reduce! 'inc "")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc \"\")" nil e)))
    (try
      (let [result (stream-reduce! 'inc "a")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc \"a\")" nil e)))
    (try
      (let [result (stream-reduce! 'dec "")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'dec \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'dec \"\")" nil e)))
    (try
      (let [result (stream-reduce! 'inc nil "")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc nil \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc nil \"\")" nil e)))
    (try
      (let [result (stream-reduce! 'inc nil "a")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc nil \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc nil \"a\")" nil e)))
    (try
      (let [result (stream-reduce! 'inc true "")]
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc true \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-reduce!" "(stream-reduce! 'inc true \"\")" nil e)))))


(deftest test-clojure-core-stream-seq-bang
  (testing "clojure.core/stream-seq!"
    ;; Arities: [[stream]]
    (try
      (let [result (stream-seq! nil)]
        (record-result! "clojure.core/stream-seq!" "(stream-seq! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-seq!" "(stream-seq! nil)" nil e)))
    (try
      (let [result (stream-seq! true)]
        (record-result! "clojure.core/stream-seq!" "(stream-seq! true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-seq!" "(stream-seq! true)" nil e)))))


(deftest test-clojure-core-stream-transduce-bang
  (testing "clojure.core/stream-transduce!"
    ;; Arities: [[xform f stream] [xform f init stream]]
    (try
      (let [result (stream-transduce! nil 'inc nil)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil)" nil e)))
    (try
      (let [result (stream-transduce! nil 'inc true)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc true)" nil e)))
    (try
      (let [result (stream-transduce! nil 'dec nil)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'dec nil)" nil e)))
    (try
      (let [result (stream-transduce! nil 'inc nil nil)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil nil)" nil e)))
    (try
      (let [result (stream-transduce! nil 'inc nil true)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc nil true)" nil e)))
    (try
      (let [result (stream-transduce! nil 'inc true nil)]
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/stream-transduce!" "(stream-transduce! nil 'inc true nil)" nil e)))))


(deftest test-clojure-core-string-p
  (testing "clojure.core/string?"
    ;; Arities: [quote ([x])]
    (try
      (let [result (string?)]
        (record-result! "clojure.core/string?" "(string?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/string?" "(string?)" nil e)))
    (try
      (let [result (string?)]
        (record-result! "clojure.core/string?" "(string?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/string?" "(string?)" nil e)))))


(deftest test-clojure-core-struct
  (testing "clojure.core/struct"
    ;; Arities: [[s & vals]]
    (try
      (let [result (struct "")]
        (record-result! "clojure.core/struct" "(struct \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct" "(struct \"\")" nil e)))
    (try
      (let [result (struct "" nil)]
        (record-result! "clojure.core/struct" "(struct \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct" "(struct \"\" nil)" nil e)))
    (try
      (let [result (struct "" nil nil)]
        (record-result! "clojure.core/struct" "(struct \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct" "(struct \"\" nil nil)" nil e)))))


(deftest test-clojure-core-struct-map
  (testing "clojure.core/struct-map"
    ;; Arities: [[s & inits]]
    (try
      (let [result (struct-map "")]
        (record-result! "clojure.core/struct-map" "(struct-map \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct-map" "(struct-map \"\")" nil e)))
    (try
      (let [result (struct-map "" nil)]
        (record-result! "clojure.core/struct-map" "(struct-map \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct-map" "(struct-map \"\" nil)" nil e)))
    (try
      (let [result (struct-map "" nil nil)]
        (record-result! "clojure.core/struct-map" "(struct-map \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/struct-map" "(struct-map \"\" nil nil)" nil e)))))


(deftest test-clojure-core-subs
  (testing "clojure.core/subs"
    ;; Arities: [[s start] [s start end]]
    (try
      (let [result (subs "" 0)]
        (record-result! "clojure.core/subs" "(subs \"\" 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"\" 0)" nil e)))
    (try
      (let [result (subs "" 1)]
        (record-result! "clojure.core/subs" "(subs \"\" 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"\" 1)" nil e)))
    (try
      (let [result (subs "a" 0)]
        (record-result! "clojure.core/subs" "(subs \"a\" 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"a\" 0)" nil e)))
    (try
      (let [result (subs "" 0 0)]
        (record-result! "clojure.core/subs" "(subs \"\" 0 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"\" 0 0)" nil e)))
    (try
      (let [result (subs "" 0 1)]
        (record-result! "clojure.core/subs" "(subs \"\" 0 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"\" 0 1)" nil e)))
    (try
      (let [result (subs "" 1 0)]
        (record-result! "clojure.core/subs" "(subs \"\" 1 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subs" "(subs \"\" 1 0)" nil e)))))


(deftest test-clojure-core-subseq
  (testing "clojure.core/subseq"
    ;; Arities: [[sc test key] [sc start-test start-key end-test end-key]]
    (try
      (let [result (subseq nil nil :a)]
        (record-result! "clojure.core/subseq" "(subseq nil nil :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil nil :a)" nil e)))
    (try
      (let [result (subseq nil nil :b)]
        (record-result! "clojure.core/subseq" "(subseq nil nil :b)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil nil :b)" nil e)))
    (try
      (let [result (subseq nil true :a)]
        (record-result! "clojure.core/subseq" "(subseq nil true :a)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil true :a)" nil e)))
    (try
      (let [result (subseq nil nil nil nil nil)]
        (record-result! "clojure.core/subseq" "(subseq nil nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil nil nil nil nil)" nil e)))
    (try
      (let [result (subseq nil nil nil nil true)]
        (record-result! "clojure.core/subseq" "(subseq nil nil nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil nil nil nil true)" nil e)))
    (try
      (let [result (subseq nil nil nil true nil)]
        (record-result! "clojure.core/subseq" "(subseq nil nil nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subseq" "(subseq nil nil nil true nil)" nil e)))))


(deftest test-clojure-core-subvec
  (testing "clojure.core/subvec"
    ;; Arities: [[v start] [v start end]]
    (try
      (let [result (subvec nil 0)]
        (record-result! "clojure.core/subvec" "(subvec nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec nil 0)" nil e)))
    (try
      (let [result (subvec nil 1)]
        (record-result! "clojure.core/subvec" "(subvec nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec nil 1)" nil e)))
    (try
      (let [result (subvec true 0)]
        (record-result! "clojure.core/subvec" "(subvec true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec true 0)" nil e)))
    (try
      (let [result (subvec nil 0 0)]
        (record-result! "clojure.core/subvec" "(subvec nil 0 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec nil 0 0)" nil e)))
    (try
      (let [result (subvec nil 0 1)]
        (record-result! "clojure.core/subvec" "(subvec nil 0 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec nil 0 1)" nil e)))
    (try
      (let [result (subvec nil 1 0)]
        (record-result! "clojure.core/subvec" "(subvec nil 1 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/subvec" "(subvec nil 1 0)" nil e)))))


(deftest test-clojure-core-supers
  (testing "clojure.core/supers"
    ;; Arities: [[class]]
    (try
      (let [result (supers nil)]
        (record-result! "clojure.core/supers" "(supers nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/supers" "(supers nil)" nil e)))
    (try
      (let [result (supers true)]
        (record-result! "clojure.core/supers" "(supers true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/supers" "(supers true)" nil e)))))


(deftest test-clojure-core-swap-bang
  (testing "clojure.core/swap!"
    ;; Arities: [[atom f] [atom f x] [atom f x y] [atom f x y & args]]
    (try
      (let [result (swap! nil 'inc)]
        (record-result! "clojure.core/swap!" "(swap! nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'inc)" nil e)))
    (try
      (let [result (swap! nil 'dec)]
        (record-result! "clojure.core/swap!" "(swap! nil 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'dec)" nil e)))
    (try
      (let [result (swap! true 'inc)]
        (record-result! "clojure.core/swap!" "(swap! true 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! true 'inc)" nil e)))
    (try
      (let [result (swap! nil 'inc nil)]
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil)" nil e)))
    (try
      (let [result (swap! nil 'inc true)]
        (record-result! "clojure.core/swap!" "(swap! nil 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'inc true)" nil e)))
    (try
      (let [result (swap! nil 'dec nil)]
        (record-result! "clojure.core/swap!" "(swap! nil 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'dec nil)" nil e)))
    (try
      (let [result (swap! nil 'inc nil nil)]
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil nil)" nil e)))
    (try
      (let [result (swap! nil 'inc nil true)]
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap!" "(swap! nil 'inc nil true)" nil e)))))


(deftest test-clojure-core-swap-vals-bang
  (testing "clojure.core/swap-vals!"
    ;; Arities: [[atom f] [atom f x] [atom f x y] [atom f x y & args]]
    (try
      (let [result (swap-vals! nil 'inc)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc)" nil e)))
    (try
      (let [result (swap-vals! nil 'dec)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'dec)" nil e)))
    (try
      (let [result (swap-vals! true 'inc)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! true 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! true 'inc)" nil e)))
    (try
      (let [result (swap-vals! nil 'inc nil)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil)" nil e)))
    (try
      (let [result (swap-vals! nil 'inc true)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc true)" nil e)))
    (try
      (let [result (swap-vals! nil 'dec nil)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'dec nil)" nil e)))
    (try
      (let [result (swap-vals! nil 'inc nil nil)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil nil)" nil e)))
    (try
      (let [result (swap-vals! nil 'inc nil true)]
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/swap-vals!" "(swap-vals! nil 'inc nil true)" nil e)))))


(deftest test-clojure-core-symbol
  (testing "clojure.core/symbol"
    ;; Arities: [[name] [ns name]]
    (try
      (let [result (symbol nil)]
        (record-result! "clojure.core/symbol" "(symbol nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol" "(symbol nil)" nil e)))
    (try
      (let [result (symbol true)]
        (record-result! "clojure.core/symbol" "(symbol true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol" "(symbol true)" nil e)))
    (try
      (let [result (symbol nil nil)]
        (record-result! "clojure.core/symbol" "(symbol nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol" "(symbol nil nil)" nil e)))
    (try
      (let [result (symbol nil true)]
        (record-result! "clojure.core/symbol" "(symbol nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol" "(symbol nil true)" nil e)))
    (try
      (let [result (symbol true nil)]
        (record-result! "clojure.core/symbol" "(symbol true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol" "(symbol true nil)" nil e)))))


(deftest test-clojure-core-symbol-p
  (testing "clojure.core/symbol?"
    ;; Arities: [[x]]
    (try
      (let [result (symbol? nil)]
        (record-result! "clojure.core/symbol?" "(symbol? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol?" "(symbol? nil)" nil e)))
    (try
      (let [result (symbol? true)]
        (record-result! "clojure.core/symbol?" "(symbol? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/symbol?" "(symbol? true)" nil e)))))


(deftest test-clojure-core-sync
  (testing "clojure.core/sync"
    ;; Arities: [[flags-ignored-for-now & body]]
    (try
      (let [result (sync nil)]
        (record-result! "clojure.core/sync" "(sync nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sync" "(sync nil)" nil e)))
    (try
      (let [result (sync nil nil)]
        (record-result! "clojure.core/sync" "(sync nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sync" "(sync nil nil)" nil e)))
    (try
      (let [result (sync nil nil nil)]
        (record-result! "clojure.core/sync" "(sync nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/sync" "(sync nil nil nil)" nil e)))))


(deftest test-clojure-core-tagged-literal
  (testing "clojure.core/tagged-literal"
    ;; Arities: [[tag form]]
    (try
      (let [result (tagged-literal nil nil)]
        (record-result! "clojure.core/tagged-literal" "(tagged-literal nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tagged-literal" "(tagged-literal nil nil)" nil e)))
    (try
      (let [result (tagged-literal nil true)]
        (record-result! "clojure.core/tagged-literal" "(tagged-literal nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tagged-literal" "(tagged-literal nil true)" nil e)))
    (try
      (let [result (tagged-literal true nil)]
        (record-result! "clojure.core/tagged-literal" "(tagged-literal true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tagged-literal" "(tagged-literal true nil)" nil e)))))


(deftest test-clojure-core-tagged-literal-p
  (testing "clojure.core/tagged-literal?"
    ;; Arities: [[value]]
    (try
      (let [result (tagged-literal? nil)]
        (record-result! "clojure.core/tagged-literal?" "(tagged-literal? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tagged-literal?" "(tagged-literal? nil)" nil e)))
    (try
      (let [result (tagged-literal? true)]
        (record-result! "clojure.core/tagged-literal?" "(tagged-literal? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tagged-literal?" "(tagged-literal? true)" nil e)))))


(deftest test-clojure-core-take
  (testing "clojure.core/take"
    ;; Arities: [[n] [n coll]]
    (try
      (let [result (take 0)]
        (record-result! "clojure.core/take" "(take 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take" "(take 0)" nil e)))
    (try
      (let [result (take 1)]
        (record-result! "clojure.core/take" "(take 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take" "(take 1)" nil e)))
    (try
      (let [result (take 0 nil)]
        (record-result! "clojure.core/take" "(take 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take" "(take 0 nil)" nil e)))
    (try
      (let [result (take 0 [])]
        (record-result! "clojure.core/take" "(take 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take" "(take 0 [])" nil e)))
    (try
      (let [result (take 1 nil)]
        (record-result! "clojure.core/take" "(take 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take" "(take 1 nil)" nil e)))))


(deftest test-clojure-core-take-last
  (testing "clojure.core/take-last"
    ;; Arities: [[n coll]]
    (try
      (let [result (take-last 0 nil)]
        (record-result! "clojure.core/take-last" "(take-last 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-last" "(take-last 0 nil)" nil e)))
    (try
      (let [result (take-last 0 [])]
        (record-result! "clojure.core/take-last" "(take-last 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-last" "(take-last 0 [])" nil e)))
    (try
      (let [result (take-last 1 nil)]
        (record-result! "clojure.core/take-last" "(take-last 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-last" "(take-last 1 nil)" nil e)))))


(deftest test-clojure-core-take-nth
  (testing "clojure.core/take-nth"
    ;; Arities: [[n] [n coll]]
    (try
      (let [result (take-nth 0)]
        (record-result! "clojure.core/take-nth" "(take-nth 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-nth" "(take-nth 0)" nil e)))
    (try
      (let [result (take-nth 1)]
        (record-result! "clojure.core/take-nth" "(take-nth 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-nth" "(take-nth 1)" nil e)))
    (try
      (let [result (take-nth 0 nil)]
        (record-result! "clojure.core/take-nth" "(take-nth 0 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-nth" "(take-nth 0 nil)" nil e)))
    (try
      (let [result (take-nth 0 [])]
        (record-result! "clojure.core/take-nth" "(take-nth 0 [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-nth" "(take-nth 0 [])" nil e)))
    (try
      (let [result (take-nth 1 nil)]
        (record-result! "clojure.core/take-nth" "(take-nth 1 nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-nth" "(take-nth 1 nil)" nil e)))))


(deftest test-clojure-core-take-while
  (testing "clojure.core/take-while"
    ;; Arities: [[pred] [pred coll]]
    (try
      (let [result (take-while 'even?)]
        (record-result! "clojure.core/take-while" "(take-while 'even?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-while" "(take-while 'even?)" nil e)))
    (try
      (let [result (take-while 'odd?)]
        (record-result! "clojure.core/take-while" "(take-while 'odd?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-while" "(take-while 'odd?)" nil e)))
    (try
      (let [result (take-while 'even? nil)]
        (record-result! "clojure.core/take-while" "(take-while 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-while" "(take-while 'even? nil)" nil e)))
    (try
      (let [result (take-while 'even? [])]
        (record-result! "clojure.core/take-while" "(take-while 'even? [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-while" "(take-while 'even? [])" nil e)))
    (try
      (let [result (take-while 'odd? nil)]
        (record-result! "clojure.core/take-while" "(take-while 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/take-while" "(take-while 'odd? nil)" nil e)))))


(deftest test-clojure-core-tap-gt
  (testing "clojure.core/tap>"
    ;; Arities: [[x]]
    (try
      (let [result (tap> nil)]
        (record-result! "clojure.core/tap>" "(tap> nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tap>" "(tap> nil)" nil e)))
    (try
      (let [result (tap> true)]
        (record-result! "clojure.core/tap>" "(tap> true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tap>" "(tap> true)" nil e)))))


(deftest test-clojure-core-test
  (testing "clojure.core/test"
    ;; Arities: [[v]]
    (try
      (let [result (test nil)]
        (record-result! "clojure.core/test" "(test nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/test" "(test nil)" nil e)))
    (try
      (let [result (test true)]
        (record-result! "clojure.core/test" "(test true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/test" "(test true)" nil e)))))


(deftest test-clojure-core-the-ns
  (testing "clojure.core/the-ns"
    ;; Arities: [[x]]
    (try
      (let [result (the-ns nil)]
        (record-result! "clojure.core/the-ns" "(the-ns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/the-ns" "(the-ns nil)" nil e)))
    (try
      (let [result (the-ns true)]
        (record-result! "clojure.core/the-ns" "(the-ns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/the-ns" "(the-ns true)" nil e)))))


(deftest test-clojure-core-thread-bound-p
  (testing "clojure.core/thread-bound?"
    ;; Arities: [[& vars]]
    (try
      (let [result (thread-bound?)]
        (record-result! "clojure.core/thread-bound?" "(thread-bound?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/thread-bound?" "(thread-bound?)" nil e)))))


(deftest test-clojure-core-time
  (testing "clojure.core/time"
    ;; Arities: [[expr]]
    (try
      (let [result (time nil)]
        (record-result! "clojure.core/time" "(time nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/time" "(time nil)" nil e)))
    (try
      (let [result (time true)]
        (record-result! "clojure.core/time" "(time true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/time" "(time true)" nil e)))))


(deftest test-clojure-core-to-array
  (testing "clojure.core/to-array"
    ;; Arities: [[coll]]
    (try
      (let [result (to-array nil)]
        (record-result! "clojure.core/to-array" "(to-array nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/to-array" "(to-array nil)" nil e)))
    (try
      (let [result (to-array [])]
        (record-result! "clojure.core/to-array" "(to-array [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/to-array" "(to-array [])" nil e)))))


(deftest test-clojure-core-to-array-2d
  (testing "clojure.core/to-array-2d"
    ;; Arities: [[coll]]
    (try
      (let [result (to-array-2d nil)]
        (record-result! "clojure.core/to-array-2d" "(to-array-2d nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/to-array-2d" "(to-array-2d nil)" nil e)))
    (try
      (let [result (to-array-2d [])]
        (record-result! "clojure.core/to-array-2d" "(to-array-2d [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/to-array-2d" "(to-array-2d [])" nil e)))))


(deftest test-clojure-core-trampoline
  (testing "clojure.core/trampoline"
    ;; Arities: [[f] [f & args]]
    (try
      (let [result (trampoline 'inc)]
        (record-result! "clojure.core/trampoline" "(trampoline 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/trampoline" "(trampoline 'inc)" nil e)))
    (try
      (let [result (trampoline 'dec)]
        (record-result! "clojure.core/trampoline" "(trampoline 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/trampoline" "(trampoline 'dec)" nil e)))
    (try
      (let [result (trampoline 'inc)]
        (record-result! "clojure.core/trampoline" "(trampoline 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/trampoline" "(trampoline 'inc)" nil e)))
    (try
      (let [result (trampoline 'inc nil)]
        (record-result! "clojure.core/trampoline" "(trampoline 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/trampoline" "(trampoline 'inc nil)" nil e)))
    (try
      (let [result (trampoline 'inc nil nil)]
        (record-result! "clojure.core/trampoline" "(trampoline 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/trampoline" "(trampoline 'inc nil nil)" nil e)))))


(deftest test-clojure-core-transduce
  (testing "clojure.core/transduce"
    ;; Arities: [[xform f coll] [xform f init coll]]
    (try
      (let [result (transduce nil 'inc nil)]
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil)" nil e)))
    (try
      (let [result (transduce nil 'inc [])]
        (record-result! "clojure.core/transduce" "(transduce nil 'inc [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'inc [])" nil e)))
    (try
      (let [result (transduce nil 'dec nil)]
        (record-result! "clojure.core/transduce" "(transduce nil 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'dec nil)" nil e)))
    (try
      (let [result (transduce nil 'inc nil nil)]
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil nil)" nil e)))
    (try
      (let [result (transduce nil 'inc nil [])]
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'inc nil [])" nil e)))
    (try
      (let [result (transduce nil 'inc true nil)]
        (record-result! "clojure.core/transduce" "(transduce nil 'inc true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transduce" "(transduce nil 'inc true nil)" nil e)))))


(deftest test-clojure-core-transient
  (testing "clojure.core/transient"
    ;; Arities: [[coll]]
    (try
      (let [result (transient nil)]
        (record-result! "clojure.core/transient" "(transient nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transient" "(transient nil)" nil e)))
    (try
      (let [result (transient [])]
        (record-result! "clojure.core/transient" "(transient [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/transient" "(transient [])" nil e)))))


(deftest test-clojure-core-tree-seq
  (testing "clojure.core/tree-seq"
    ;; Arities: [[branch? children root]]
    (try
      (let [result (tree-seq nil nil nil)]
        (record-result! "clojure.core/tree-seq" "(tree-seq nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tree-seq" "(tree-seq nil nil nil)" nil e)))
    (try
      (let [result (tree-seq nil nil true)]
        (record-result! "clojure.core/tree-seq" "(tree-seq nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tree-seq" "(tree-seq nil nil true)" nil e)))
    (try
      (let [result (tree-seq nil true nil)]
        (record-result! "clojure.core/tree-seq" "(tree-seq nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/tree-seq" "(tree-seq nil true nil)" nil e)))))


(deftest test-clojure-core-true-p
  (testing "clojure.core/true?"
    ;; Arities: [[x]]
    (try
      (let [result (true? nil)]
        (record-result! "clojure.core/true?" "(true? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/true?" "(true? nil)" nil e)))
    (try
      (let [result (true? true)]
        (record-result! "clojure.core/true?" "(true? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/true?" "(true? true)" nil e)))))


(deftest test-clojure-core-type
  (testing "clojure.core/type"
    ;; Arities: [[x]]
    (try
      (let [result (type nil)]
        (record-result! "clojure.core/type" "(type nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/type" "(type nil)" nil e)))
    (try
      (let [result (type true)]
        (record-result! "clojure.core/type" "(type true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/type" "(type true)" nil e)))))


(deftest test-clojure-core-unchecked-add
  (testing "clojure.core/unchecked-add"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-add nil nil)]
        (record-result! "clojure.core/unchecked-add" "(unchecked-add nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add" "(unchecked-add nil nil)" nil e)))
    (try
      (let [result (unchecked-add nil true)]
        (record-result! "clojure.core/unchecked-add" "(unchecked-add nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add" "(unchecked-add nil true)" nil e)))
    (try
      (let [result (unchecked-add true nil)]
        (record-result! "clojure.core/unchecked-add" "(unchecked-add true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add" "(unchecked-add true nil)" nil e)))))


(deftest test-clojure-core-unchecked-add-int
  (testing "clojure.core/unchecked-add-int"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-add-int nil nil)]
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int nil nil)" nil e)))
    (try
      (let [result (unchecked-add-int nil true)]
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int nil true)" nil e)))
    (try
      (let [result (unchecked-add-int true nil)]
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-add-int" "(unchecked-add-int true nil)" nil e)))))


(deftest test-clojure-core-unchecked-byte
  (testing "clojure.core/unchecked-byte"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-byte nil)]
        (record-result! "clojure.core/unchecked-byte" "(unchecked-byte nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-byte" "(unchecked-byte nil)" nil e)))
    (try
      (let [result (unchecked-byte true)]
        (record-result! "clojure.core/unchecked-byte" "(unchecked-byte true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-byte" "(unchecked-byte true)" nil e)))))


(deftest test-clojure-core-unchecked-char
  (testing "clojure.core/unchecked-char"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-char nil)]
        (record-result! "clojure.core/unchecked-char" "(unchecked-char nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-char" "(unchecked-char nil)" nil e)))
    (try
      (let [result (unchecked-char true)]
        (record-result! "clojure.core/unchecked-char" "(unchecked-char true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-char" "(unchecked-char true)" nil e)))))


(deftest test-clojure-core-unchecked-dec
  (testing "clojure.core/unchecked-dec"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-dec nil)]
        (record-result! "clojure.core/unchecked-dec" "(unchecked-dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-dec" "(unchecked-dec nil)" nil e)))
    (try
      (let [result (unchecked-dec true)]
        (record-result! "clojure.core/unchecked-dec" "(unchecked-dec true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-dec" "(unchecked-dec true)" nil e)))))


(deftest test-clojure-core-unchecked-dec-int
  (testing "clojure.core/unchecked-dec-int"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-dec-int nil)]
        (record-result! "clojure.core/unchecked-dec-int" "(unchecked-dec-int nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-dec-int" "(unchecked-dec-int nil)" nil e)))
    (try
      (let [result (unchecked-dec-int true)]
        (record-result! "clojure.core/unchecked-dec-int" "(unchecked-dec-int true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-dec-int" "(unchecked-dec-int true)" nil e)))))


(deftest test-clojure-core-unchecked-divide-int
  (testing "clojure.core/unchecked-divide-int"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-divide-int nil nil)]
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int nil nil)" nil e)))
    (try
      (let [result (unchecked-divide-int nil true)]
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int nil true)" nil e)))
    (try
      (let [result (unchecked-divide-int true nil)]
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-divide-int" "(unchecked-divide-int true nil)" nil e)))))


(deftest test-clojure-core-unchecked-double
  (testing "clojure.core/unchecked-double"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-double nil)]
        (record-result! "clojure.core/unchecked-double" "(unchecked-double nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-double" "(unchecked-double nil)" nil e)))
    (try
      (let [result (unchecked-double true)]
        (record-result! "clojure.core/unchecked-double" "(unchecked-double true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-double" "(unchecked-double true)" nil e)))))


(deftest test-clojure-core-unchecked-float
  (testing "clojure.core/unchecked-float"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-float nil)]
        (record-result! "clojure.core/unchecked-float" "(unchecked-float nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-float" "(unchecked-float nil)" nil e)))
    (try
      (let [result (unchecked-float true)]
        (record-result! "clojure.core/unchecked-float" "(unchecked-float true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-float" "(unchecked-float true)" nil e)))))


(deftest test-clojure-core-unchecked-inc
  (testing "clojure.core/unchecked-inc"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-inc nil)]
        (record-result! "clojure.core/unchecked-inc" "(unchecked-inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-inc" "(unchecked-inc nil)" nil e)))
    (try
      (let [result (unchecked-inc true)]
        (record-result! "clojure.core/unchecked-inc" "(unchecked-inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-inc" "(unchecked-inc true)" nil e)))))


(deftest test-clojure-core-unchecked-inc-int
  (testing "clojure.core/unchecked-inc-int"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-inc-int nil)]
        (record-result! "clojure.core/unchecked-inc-int" "(unchecked-inc-int nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-inc-int" "(unchecked-inc-int nil)" nil e)))
    (try
      (let [result (unchecked-inc-int true)]
        (record-result! "clojure.core/unchecked-inc-int" "(unchecked-inc-int true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-inc-int" "(unchecked-inc-int true)" nil e)))))


(deftest test-clojure-core-unchecked-int
  (testing "clojure.core/unchecked-int"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-int nil)]
        (record-result! "clojure.core/unchecked-int" "(unchecked-int nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-int" "(unchecked-int nil)" nil e)))
    (try
      (let [result (unchecked-int true)]
        (record-result! "clojure.core/unchecked-int" "(unchecked-int true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-int" "(unchecked-int true)" nil e)))))


(deftest test-clojure-core-unchecked-long
  (testing "clojure.core/unchecked-long"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-long nil)]
        (record-result! "clojure.core/unchecked-long" "(unchecked-long nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-long" "(unchecked-long nil)" nil e)))
    (try
      (let [result (unchecked-long true)]
        (record-result! "clojure.core/unchecked-long" "(unchecked-long true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-long" "(unchecked-long true)" nil e)))))


(deftest test-clojure-core-unchecked-multiply
  (testing "clojure.core/unchecked-multiply"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-multiply nil nil)]
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply nil nil)" nil e)))
    (try
      (let [result (unchecked-multiply nil true)]
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply nil true)" nil e)))
    (try
      (let [result (unchecked-multiply true nil)]
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply" "(unchecked-multiply true nil)" nil e)))))


(deftest test-clojure-core-unchecked-multiply-int
  (testing "clojure.core/unchecked-multiply-int"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-multiply-int nil nil)]
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int nil nil)" nil e)))
    (try
      (let [result (unchecked-multiply-int nil true)]
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int nil true)" nil e)))
    (try
      (let [result (unchecked-multiply-int true nil)]
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-multiply-int" "(unchecked-multiply-int true nil)" nil e)))))


(deftest test-clojure-core-unchecked-negate
  (testing "clojure.core/unchecked-negate"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-negate nil)]
        (record-result! "clojure.core/unchecked-negate" "(unchecked-negate nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-negate" "(unchecked-negate nil)" nil e)))
    (try
      (let [result (unchecked-negate true)]
        (record-result! "clojure.core/unchecked-negate" "(unchecked-negate true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-negate" "(unchecked-negate true)" nil e)))))


(deftest test-clojure-core-unchecked-negate-int
  (testing "clojure.core/unchecked-negate-int"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-negate-int nil)]
        (record-result! "clojure.core/unchecked-negate-int" "(unchecked-negate-int nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-negate-int" "(unchecked-negate-int nil)" nil e)))
    (try
      (let [result (unchecked-negate-int true)]
        (record-result! "clojure.core/unchecked-negate-int" "(unchecked-negate-int true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-negate-int" "(unchecked-negate-int true)" nil e)))))


(deftest test-clojure-core-unchecked-remainder-int
  (testing "clojure.core/unchecked-remainder-int"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-remainder-int nil nil)]
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int nil nil)" nil e)))
    (try
      (let [result (unchecked-remainder-int nil true)]
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int nil true)" nil e)))
    (try
      (let [result (unchecked-remainder-int true nil)]
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-remainder-int" "(unchecked-remainder-int true nil)" nil e)))))


(deftest test-clojure-core-unchecked-short
  (testing "clojure.core/unchecked-short"
    ;; Arities: [[x]]
    (try
      (let [result (unchecked-short nil)]
        (record-result! "clojure.core/unchecked-short" "(unchecked-short nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-short" "(unchecked-short nil)" nil e)))
    (try
      (let [result (unchecked-short true)]
        (record-result! "clojure.core/unchecked-short" "(unchecked-short true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-short" "(unchecked-short true)" nil e)))))


(deftest test-clojure-core-unchecked-subtract
  (testing "clojure.core/unchecked-subtract"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-subtract nil nil)]
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract nil nil)" nil e)))
    (try
      (let [result (unchecked-subtract nil true)]
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract nil true)" nil e)))
    (try
      (let [result (unchecked-subtract true nil)]
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract" "(unchecked-subtract true nil)" nil e)))))


(deftest test-clojure-core-unchecked-subtract-int
  (testing "clojure.core/unchecked-subtract-int"
    ;; Arities: [[x y]]
    (try
      (let [result (unchecked-subtract-int nil nil)]
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int nil nil)" nil e)))
    (try
      (let [result (unchecked-subtract-int nil true)]
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int nil true)" nil e)))
    (try
      (let [result (unchecked-subtract-int true nil)]
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unchecked-subtract-int" "(unchecked-subtract-int true nil)" nil e)))))


(deftest test-clojure-core-underive
  (testing "clojure.core/underive"
    ;; Arities: [[tag parent] [h tag parent]]
    (try
      (let [result (underive nil nil)]
        (record-result! "clojure.core/underive" "(underive nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive nil nil)" nil e)))
    (try
      (let [result (underive nil true)]
        (record-result! "clojure.core/underive" "(underive nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive nil true)" nil e)))
    (try
      (let [result (underive true nil)]
        (record-result! "clojure.core/underive" "(underive true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive true nil)" nil e)))
    (try
      (let [result (underive nil nil nil)]
        (record-result! "clojure.core/underive" "(underive nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive nil nil nil)" nil e)))
    (try
      (let [result (underive nil nil true)]
        (record-result! "clojure.core/underive" "(underive nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive nil nil true)" nil e)))
    (try
      (let [result (underive nil true nil)]
        (record-result! "clojure.core/underive" "(underive nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/underive" "(underive nil true nil)" nil e)))))


(deftest test-clojure-core-unreduced
  (testing "clojure.core/unreduced"
    ;; Arities: [[x]]
    (try
      (let [result (unreduced nil)]
        (record-result! "clojure.core/unreduced" "(unreduced nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unreduced" "(unreduced nil)" nil e)))
    (try
      (let [result (unreduced true)]
        (record-result! "clojure.core/unreduced" "(unreduced true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unreduced" "(unreduced true)" nil e)))))


(deftest test-clojure-core-unsigned-bit-shift-right
  (testing "clojure.core/unsigned-bit-shift-right"
    ;; Arities: [[x n]]
    (try
      (let [result (unsigned-bit-shift-right nil 0)]
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right nil 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right nil 0)" nil e)))
    (try
      (let [result (unsigned-bit-shift-right nil 1)]
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right nil 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right nil 1)" nil e)))
    (try
      (let [result (unsigned-bit-shift-right true 0)]
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right true 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/unsigned-bit-shift-right" "(unsigned-bit-shift-right true 0)" nil e)))))


(deftest test-clojure-core-update
  (testing "clojure.core/update"
    ;; Arities: [[m k f] [m k f x] [m k f x y] [m k f x y z] [m k f x y z & more]]
    (try
      (let [result (update {} :a 'inc)]
        (record-result! "clojure.core/update" "(update {} :a 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'inc)" nil e)))
    (try
      (let [result (update {} :a 'dec)]
        (record-result! "clojure.core/update" "(update {} :a 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'dec)" nil e)))
    (try
      (let [result (update {} :b 'inc)]
        (record-result! "clojure.core/update" "(update {} :b 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :b 'inc)" nil e)))
    (try
      (let [result (update {} :a 'inc nil)]
        (record-result! "clojure.core/update" "(update {} :a 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'inc nil)" nil e)))
    (try
      (let [result (update {} :a 'inc true)]
        (record-result! "clojure.core/update" "(update {} :a 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'inc true)" nil e)))
    (try
      (let [result (update {} :a 'dec nil)]
        (record-result! "clojure.core/update" "(update {} :a 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'dec nil)" nil e)))
    (try
      (let [result (update {} :a 'inc nil nil)]
        (record-result! "clojure.core/update" "(update {} :a 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'inc nil nil)" nil e)))
    (try
      (let [result (update {} :a 'inc nil true)]
        (record-result! "clojure.core/update" "(update {} :a 'inc nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update" "(update {} :a 'inc nil true)" nil e)))))


(deftest test-clojure-core-update-in
  (testing "clojure.core/update-in"
    ;; Arities: [[m ks f & args]]
    (try
      (let [result (update-in {} nil 'inc)]
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc)" nil e)))
    (try
      (let [result (update-in {} nil 'inc nil)]
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc nil)" nil e)))
    (try
      (let [result (update-in {} nil 'inc nil nil)]
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-in" "(update-in {} nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-update-keys
  (testing "clojure.core/update-keys"
    ;; Arities: [[m f]]
    (try
      (let [result (update-keys {} 'inc)]
        (record-result! "clojure.core/update-keys" "(update-keys {} 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-keys" "(update-keys {} 'inc)" nil e)))
    (try
      (let [result (update-keys {} 'dec)]
        (record-result! "clojure.core/update-keys" "(update-keys {} 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-keys" "(update-keys {} 'dec)" nil e)))
    (try
      (let [result (update-keys {:a 1} 'inc)]
        (record-result! "clojure.core/update-keys" "(update-keys {:a 1} 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-keys" "(update-keys {:a 1} 'inc)" nil e)))))


(deftest test-clojure-core-update-vals
  (testing "clojure.core/update-vals"
    ;; Arities: [[m f]]
    (try
      (let [result (update-vals {} 'inc)]
        (record-result! "clojure.core/update-vals" "(update-vals {} 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-vals" "(update-vals {} 'inc)" nil e)))
    (try
      (let [result (update-vals {} 'dec)]
        (record-result! "clojure.core/update-vals" "(update-vals {} 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-vals" "(update-vals {} 'dec)" nil e)))
    (try
      (let [result (update-vals {:a 1} 'inc)]
        (record-result! "clojure.core/update-vals" "(update-vals {:a 1} 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/update-vals" "(update-vals {:a 1} 'inc)" nil e)))))


(deftest test-clojure-core-uri-p
  (testing "clojure.core/uri?"
    ;; Arities: [[x]]
    (try
      (let [result (uri? nil)]
        (record-result! "clojure.core/uri?" "(uri? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/uri?" "(uri? nil)" nil e)))
    (try
      (let [result (uri? true)]
        (record-result! "clojure.core/uri?" "(uri? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/uri?" "(uri? true)" nil e)))))


(deftest test-clojure-core-use
  (testing "clojure.core/use"
    ;; Arities: [[& args]]
    (try
      (let [result (use)]
        (record-result! "clojure.core/use" "(use)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/use" "(use)" nil e)))))


(deftest test-clojure-core-uuid-p
  (testing "clojure.core/uuid?"
    ;; Arities: [[x]]
    (try
      (let [result (uuid? nil)]
        (record-result! "clojure.core/uuid?" "(uuid? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/uuid?" "(uuid? nil)" nil e)))
    (try
      (let [result (uuid? true)]
        (record-result! "clojure.core/uuid?" "(uuid? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/uuid?" "(uuid? true)" nil e)))))


(deftest test-clojure-core-val
  (testing "clojure.core/val"
    ;; Arities: [[e]]
    (try
      (let [result (val nil)]
        (record-result! "clojure.core/val" "(val nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/val" "(val nil)" nil e)))
    (try
      (let [result (val true)]
        (record-result! "clojure.core/val" "(val true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/val" "(val true)" nil e)))))


(deftest test-clojure-core-vals
  (testing "clojure.core/vals"
    ;; Arities: [[map]]
    (try
      (let [result (vals {})]
        (record-result! "clojure.core/vals" "(vals {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vals" "(vals {})" nil e)))
    (try
      (let [result (vals {:a 1})]
        (record-result! "clojure.core/vals" "(vals {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vals" "(vals {:a 1})" nil e)))))


(deftest test-clojure-core-var-get
  (testing "clojure.core/var-get"
    ;; Arities: [[x]]
    (try
      (let [result (var-get nil)]
        (record-result! "clojure.core/var-get" "(var-get nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var-get" "(var-get nil)" nil e)))
    (try
      (let [result (var-get true)]
        (record-result! "clojure.core/var-get" "(var-get true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var-get" "(var-get true)" nil e)))))


(deftest test-clojure-core-var-set
  (testing "clojure.core/var-set"
    ;; Arities: [[x val]]
    (try
      (let [result (var-set nil nil)]
        (record-result! "clojure.core/var-set" "(var-set nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var-set" "(var-set nil nil)" nil e)))
    (try
      (let [result (var-set nil true)]
        (record-result! "clojure.core/var-set" "(var-set nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var-set" "(var-set nil true)" nil e)))
    (try
      (let [result (var-set true nil)]
        (record-result! "clojure.core/var-set" "(var-set true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var-set" "(var-set true nil)" nil e)))))


(deftest test-clojure-core-var-p
  (testing "clojure.core/var?"
    ;; Arities: [[v]]
    (try
      (let [result (var? nil)]
        (record-result! "clojure.core/var?" "(var? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var?" "(var? nil)" nil e)))
    (try
      (let [result (var? true)]
        (record-result! "clojure.core/var?" "(var? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/var?" "(var? true)" nil e)))))


(deftest test-clojure-core-vary-meta
  (testing "clojure.core/vary-meta"
    ;; Arities: [[obj f & args]]
    (try
      (let [result (vary-meta nil 'inc)]
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc)" nil e)))
    (try
      (let [result (vary-meta nil 'inc nil)]
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc nil)" nil e)))
    (try
      (let [result (vary-meta nil 'inc nil nil)]
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vary-meta" "(vary-meta nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-vec
  (testing "clojure.core/vec"
    ;; Arities: [[coll]]
    (try
      (let [result (vec nil)]
        (record-result! "clojure.core/vec" "(vec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vec" "(vec nil)" nil e)))
    (try
      (let [result (vec [])]
        (record-result! "clojure.core/vec" "(vec [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vec" "(vec [])" nil e)))))


(deftest test-clojure-core-vector
  (testing "clojure.core/vector"
    ;; Arities: [[] [a] [a b] [a b c] [a b c d] [a b c d e] [a b c d e f] [a b c d e f & args]]
    (try
      (let [result (vector)]
        (record-result! "clojure.core/vector" "(vector)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector)" nil e)))
    (try
      (let [result (vector nil)]
        (record-result! "clojure.core/vector" "(vector nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector nil)" nil e)))
    (try
      (let [result (vector true)]
        (record-result! "clojure.core/vector" "(vector true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector true)" nil e)))
    (try
      (let [result (vector nil nil)]
        (record-result! "clojure.core/vector" "(vector nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector nil nil)" nil e)))
    (try
      (let [result (vector nil true)]
        (record-result! "clojure.core/vector" "(vector nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector nil true)" nil e)))
    (try
      (let [result (vector true nil)]
        (record-result! "clojure.core/vector" "(vector true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector true nil)" nil e)))
    (try
      (let [result (vector nil nil nil)]
        (record-result! "clojure.core/vector" "(vector nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector nil nil nil)" nil e)))
    (try
      (let [result (vector nil nil [])]
        (record-result! "clojure.core/vector" "(vector nil nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector" "(vector nil nil [])" nil e)))))


(deftest test-clojure-core-vector-p
  (testing "clojure.core/vector?"
    ;; Arities: [quote ([x])]
    (try
      (let [result (vector?)]
        (record-result! "clojure.core/vector?" "(vector?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector?" "(vector?)" nil e)))
    (try
      (let [result (vector?)]
        (record-result! "clojure.core/vector?" "(vector?)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vector?" "(vector?)" nil e)))))


(deftest test-clojure-core-volatile-bang
  (testing "clojure.core/volatile!"
    ;; Arities: [[val]]
    (try
      (let [result (volatile! nil)]
        (record-result! "clojure.core/volatile!" "(volatile! nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/volatile!" "(volatile! nil)" nil e)))
    (try
      (let [result (volatile! true)]
        (record-result! "clojure.core/volatile!" "(volatile! true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/volatile!" "(volatile! true)" nil e)))))


(deftest test-clojure-core-volatile-p
  (testing "clojure.core/volatile?"
    ;; Arities: [[x]]
    (try
      (let [result (volatile? nil)]
        (record-result! "clojure.core/volatile?" "(volatile? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/volatile?" "(volatile? nil)" nil e)))
    (try
      (let [result (volatile? true)]
        (record-result! "clojure.core/volatile?" "(volatile? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/volatile?" "(volatile? true)" nil e)))))


(deftest test-clojure-core-vreset-bang
  (testing "clojure.core/vreset!"
    ;; Arities: [[vol newval]]
    (try
      (let [result (vreset! nil nil)]
        (record-result! "clojure.core/vreset!" "(vreset! nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vreset!" "(vreset! nil nil)" nil e)))
    (try
      (let [result (vreset! nil true)]
        (record-result! "clojure.core/vreset!" "(vreset! nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vreset!" "(vreset! nil true)" nil e)))
    (try
      (let [result (vreset! true nil)]
        (record-result! "clojure.core/vreset!" "(vreset! true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vreset!" "(vreset! true nil)" nil e)))))


(deftest test-clojure-core-vswap-bang
  (testing "clojure.core/vswap!"
    ;; Arities: [[vol f & args]]
    (try
      (let [result (vswap! nil 'inc)]
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc)" nil e)))
    (try
      (let [result (vswap! nil 'inc nil)]
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc nil)" nil e)))
    (try
      (let [result (vswap! nil 'inc nil nil)]
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/vswap!" "(vswap! nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-when
  (testing "clojure.core/when"
    ;; Arities: [[test & body]]
    (try
      (let [result (when nil)]
        (record-result! "clojure.core/when" "(when nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when" "(when nil)" nil e)))
    (try
      (let [result (when nil nil)]
        (record-result! "clojure.core/when" "(when nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when" "(when nil nil)" nil e)))
    (try
      (let [result (when nil nil nil)]
        (record-result! "clojure.core/when" "(when nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when" "(when nil nil nil)" nil e)))))


(deftest test-clojure-core-when-first
  (testing "clojure.core/when-first"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (when-first nil)]
        (record-result! "clojure.core/when-first" "(when-first nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-first" "(when-first nil)" nil e)))
    (try
      (let [result (when-first nil nil)]
        (record-result! "clojure.core/when-first" "(when-first nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-first" "(when-first nil nil)" nil e)))
    (try
      (let [result (when-first nil nil nil)]
        (record-result! "clojure.core/when-first" "(when-first nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-first" "(when-first nil nil nil)" nil e)))))


(deftest test-clojure-core-when-let
  (testing "clojure.core/when-let"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (when-let nil)]
        (record-result! "clojure.core/when-let" "(when-let nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-let" "(when-let nil)" nil e)))
    (try
      (let [result (when-let nil nil)]
        (record-result! "clojure.core/when-let" "(when-let nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-let" "(when-let nil nil)" nil e)))
    (try
      (let [result (when-let nil nil nil)]
        (record-result! "clojure.core/when-let" "(when-let nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-let" "(when-let nil nil nil)" nil e)))))


(deftest test-clojure-core-when-not
  (testing "clojure.core/when-not"
    ;; Arities: [[test & body]]
    (try
      (let [result (when-not nil)]
        (record-result! "clojure.core/when-not" "(when-not nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-not" "(when-not nil)" nil e)))
    (try
      (let [result (when-not nil nil)]
        (record-result! "clojure.core/when-not" "(when-not nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-not" "(when-not nil nil)" nil e)))
    (try
      (let [result (when-not nil nil nil)]
        (record-result! "clojure.core/when-not" "(when-not nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-not" "(when-not nil nil nil)" nil e)))))


(deftest test-clojure-core-when-some
  (testing "clojure.core/when-some"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (when-some nil)]
        (record-result! "clojure.core/when-some" "(when-some nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-some" "(when-some nil)" nil e)))
    (try
      (let [result (when-some nil nil)]
        (record-result! "clojure.core/when-some" "(when-some nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-some" "(when-some nil nil)" nil e)))
    (try
      (let [result (when-some nil nil nil)]
        (record-result! "clojure.core/when-some" "(when-some nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/when-some" "(when-some nil nil nil)" nil e)))))


(deftest test-clojure-core-while
  (testing "clojure.core/while"
    ;; Arities: [[test & body]]
    (try
      (let [result (while nil)]
        (record-result! "clojure.core/while" "(while nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/while" "(while nil)" nil e)))
    (try
      (let [result (while nil nil)]
        (record-result! "clojure.core/while" "(while nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/while" "(while nil nil)" nil e)))
    (try
      (let [result (while nil nil nil)]
        (record-result! "clojure.core/while" "(while nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/while" "(while nil nil nil)" nil e)))))


(deftest test-clojure-core-with-bindings
  (testing "clojure.core/with-bindings"
    ;; Arities: [[binding-map & body]]
    (try
      (let [result (with-bindings nil)]
        (record-result! "clojure.core/with-bindings" "(with-bindings nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings" "(with-bindings nil)" nil e)))
    (try
      (let [result (with-bindings nil nil)]
        (record-result! "clojure.core/with-bindings" "(with-bindings nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings" "(with-bindings nil nil)" nil e)))
    (try
      (let [result (with-bindings nil nil nil)]
        (record-result! "clojure.core/with-bindings" "(with-bindings nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings" "(with-bindings nil nil nil)" nil e)))))


(deftest test-clojure-core-with-bindings-star
  (testing "clojure.core/with-bindings*"
    ;; Arities: [[binding-map f & args]]
    (try
      (let [result (with-bindings* nil 'inc)]
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc)" nil e)))
    (try
      (let [result (with-bindings* nil 'inc nil)]
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc nil)" nil e)))
    (try
      (let [result (with-bindings* nil 'inc nil nil)]
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-bindings*" "(with-bindings* nil 'inc nil nil)" nil e)))))


(deftest test-clojure-core-with-in-str
  (testing "clojure.core/with-in-str"
    ;; Arities: [[s & body]]
    (try
      (let [result (with-in-str "")]
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\")" nil e)))
    (try
      (let [result (with-in-str "" nil)]
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\" nil)" nil e)))
    (try
      (let [result (with-in-str "" nil nil)]
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-in-str" "(with-in-str \"\" nil nil)" nil e)))))


(deftest test-clojure-core-with-loading-context
  (testing "clojure.core/with-loading-context"
    ;; Arities: [[& body]]
    (try
      (let [result (with-loading-context)]
        (record-result! "clojure.core/with-loading-context" "(with-loading-context)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-loading-context" "(with-loading-context)" nil e)))))


(deftest test-clojure-core-with-local-vars
  (testing "clojure.core/with-local-vars"
    ;; Arities: [[name-vals-vec & body]]
    (try
      (let [result (with-local-vars nil)]
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil)" nil e)))
    (try
      (let [result (with-local-vars nil nil)]
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil nil)" nil e)))
    (try
      (let [result (with-local-vars nil nil nil)]
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-local-vars" "(with-local-vars nil nil nil)" nil e)))))


(deftest test-clojure-core-with-meta
  (testing "clojure.core/with-meta"
    ;; Arities: [quote ([obj m])]
    (try
      (let [result (with-meta)]
        (record-result! "clojure.core/with-meta" "(with-meta)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-meta" "(with-meta)" nil e)))
    (try
      (let [result (with-meta)]
        (record-result! "clojure.core/with-meta" "(with-meta)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-meta" "(with-meta)" nil e)))))


(deftest test-clojure-core-with-open
  (testing "clojure.core/with-open"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (with-open nil)]
        (record-result! "clojure.core/with-open" "(with-open nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-open" "(with-open nil)" nil e)))
    (try
      (let [result (with-open nil nil)]
        (record-result! "clojure.core/with-open" "(with-open nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-open" "(with-open nil nil)" nil e)))
    (try
      (let [result (with-open nil nil nil)]
        (record-result! "clojure.core/with-open" "(with-open nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-open" "(with-open nil nil nil)" nil e)))))


(deftest test-clojure-core-with-out-str
  (testing "clojure.core/with-out-str"
    ;; Arities: [[& body]]
    (try
      (let [result (with-out-str)]
        (record-result! "clojure.core/with-out-str" "(with-out-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-out-str" "(with-out-str)" nil e)))))


(deftest test-clojure-core-with-precision
  (testing "clojure.core/with-precision"
    ;; Arities: [[precision & exprs]]
    (try
      (let [result (with-precision nil)]
        (record-result! "clojure.core/with-precision" "(with-precision nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-precision" "(with-precision nil)" nil e)))
    (try
      (let [result (with-precision nil nil)]
        (record-result! "clojure.core/with-precision" "(with-precision nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-precision" "(with-precision nil nil)" nil e)))
    (try
      (let [result (with-precision nil nil nil)]
        (record-result! "clojure.core/with-precision" "(with-precision nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-precision" "(with-precision nil nil nil)" nil e)))))


(deftest test-clojure-core-with-redefs
  (testing "clojure.core/with-redefs"
    ;; Arities: [[bindings & body]]
    (try
      (let [result (with-redefs nil)]
        (record-result! "clojure.core/with-redefs" "(with-redefs nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs" "(with-redefs nil)" nil e)))
    (try
      (let [result (with-redefs nil nil)]
        (record-result! "clojure.core/with-redefs" "(with-redefs nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs" "(with-redefs nil nil)" nil e)))
    (try
      (let [result (with-redefs nil nil nil)]
        (record-result! "clojure.core/with-redefs" "(with-redefs nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs" "(with-redefs nil nil nil)" nil e)))))


(deftest test-clojure-core-with-redefs-fn
  (testing "clojure.core/with-redefs-fn"
    ;; Arities: [[binding-map func]]
    (try
      (let [result (with-redefs-fn nil 'inc)]
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn nil 'inc)" nil e)))
    (try
      (let [result (with-redefs-fn nil 'dec)]
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn nil 'dec)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn nil 'dec)" nil e)))
    (try
      (let [result (with-redefs-fn true 'inc)]
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn true 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/with-redefs-fn" "(with-redefs-fn true 'inc)" nil e)))))


(deftest test-clojure-core-xml-seq
  (testing "clojure.core/xml-seq"
    ;; Arities: [[root]]
    (try
      (let [result (xml-seq nil)]
        (record-result! "clojure.core/xml-seq" "(xml-seq nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/xml-seq" "(xml-seq nil)" nil e)))
    (try
      (let [result (xml-seq true)]
        (record-result! "clojure.core/xml-seq" "(xml-seq true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/xml-seq" "(xml-seq true)" nil e)))))


(deftest test-clojure-core-zero-p
  (testing "clojure.core/zero?"
    ;; Arities: [[num]]
    (try
      (let [result (zero? 0)]
        (record-result! "clojure.core/zero?" "(zero? 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/zero?" "(zero? 0)" nil e)))
    (try
      (let [result (zero? 1)]
        (record-result! "clojure.core/zero?" "(zero? 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/zero?" "(zero? 1)" nil e)))))


(deftest test-clojure-core-zipmap
  (testing "clojure.core/zipmap"
    ;; Arities: [[keys vals]]
    (try
      (let [result (zipmap nil nil)]
        (record-result! "clojure.core/zipmap" "(zipmap nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/zipmap" "(zipmap nil nil)" nil e)))
    (try
      (let [result (zipmap nil true)]
        (record-result! "clojure.core/zipmap" "(zipmap nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/zipmap" "(zipmap nil true)" nil e)))
    (try
      (let [result (zipmap true nil)]
        (record-result! "clojure.core/zipmap" "(zipmap true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.core/zipmap" "(zipmap true nil)" nil e)))))


;; ============================================================
;; clojure.data - 1 functions
;; ============================================================

(deftest test-clojure-data-diff
  (testing "clojure.data/diff"
    ;; Arities: [[a b]]
    (try
      (let [result (clojure.data/diff nil nil)]
        (record-result! "clojure.data/diff" "(clojure.data/diff nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.data/diff" "(clojure.data/diff nil nil)" nil e)))
    (try
      (let [result (clojure.data/diff nil true)]
        (record-result! "clojure.data/diff" "(clojure.data/diff nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.data/diff" "(clojure.data/diff nil true)" nil e)))
    (try
      (let [result (clojure.data/diff true nil)]
        (record-result! "clojure.data/diff" "(clojure.data/diff true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.data/diff" "(clojure.data/diff true nil)" nil e)))))


;; ============================================================
;; clojure.edn - 2 functions
;; ============================================================

(deftest test-clojure-edn-read
  (testing "clojure.edn/read"
    ;; Arities: [[] [stream] [opts stream]]
    (try
      (let [result (clojure.edn/read)]
        (record-result! "clojure.edn/read" "(clojure.edn/read)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read)" nil e)))
    (try
      (let [result (clojure.edn/read nil)]
        (record-result! "clojure.edn/read" "(clojure.edn/read nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read nil)" nil e)))
    (try
      (let [result (clojure.edn/read true)]
        (record-result! "clojure.edn/read" "(clojure.edn/read true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read true)" nil e)))
    (try
      (let [result (clojure.edn/read nil nil)]
        (record-result! "clojure.edn/read" "(clojure.edn/read nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read nil nil)" nil e)))
    (try
      (let [result (clojure.edn/read nil true)]
        (record-result! "clojure.edn/read" "(clojure.edn/read nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read nil true)" nil e)))
    (try
      (let [result (clojure.edn/read true nil)]
        (record-result! "clojure.edn/read" "(clojure.edn/read true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read" "(clojure.edn/read true nil)" nil e)))))


(deftest test-clojure-edn-read-string
  (testing "clojure.edn/read-string"
    ;; Arities: [[s] [opts s]]
    (try
      (let [result (clojure.edn/read-string "")]
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string \"\")" nil e)))
    (try
      (let [result (clojure.edn/read-string "a")]
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string \"a\")" nil e)))
    (try
      (let [result (clojure.edn/read-string nil "")]
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string nil \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string nil \"\")" nil e)))
    (try
      (let [result (clojure.edn/read-string nil "a")]
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string nil \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string nil \"a\")" nil e)))
    (try
      (let [result (clojure.edn/read-string true "")]
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string true \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.edn/read-string" "(clojure.edn/read-string true \"\")" nil e)))))


;; ============================================================
;; clojure.set - 12 functions
;; ============================================================

(deftest test-clojure-set-difference
  (testing "clojure.set/difference"
    ;; Arities: [[s1] [s1 s2] [s1 s2 & sets]]
    (try
      (let [result (clojure.set/difference nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil)" nil e)))
    (try
      (let [result (clojure.set/difference true)]
        (record-result! "clojure.set/difference" "(clojure.set/difference true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference true)" nil e)))
    (try
      (let [result (clojure.set/difference nil nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil)" nil e)))
    (try
      (let [result (clojure.set/difference nil true)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil true)" nil e)))
    (try
      (let [result (clojure.set/difference true nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference true nil)" nil e)))
    (try
      (let [result (clojure.set/difference nil nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil)" nil e)))
    (try
      (let [result (clojure.set/difference nil nil nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil nil)" nil e)))
    (try
      (let [result (clojure.set/difference nil nil nil nil)]
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/difference" "(clojure.set/difference nil nil nil nil)" nil e)))))


(deftest test-clojure-set-index
  (testing "clojure.set/index"
    ;; Arities: [[xrel ks]]
    (try
      (let [result (clojure.set/index nil nil)]
        (record-result! "clojure.set/index" "(clojure.set/index nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/index" "(clojure.set/index nil nil)" nil e)))
    (try
      (let [result (clojure.set/index nil true)]
        (record-result! "clojure.set/index" "(clojure.set/index nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/index" "(clojure.set/index nil true)" nil e)))
    (try
      (let [result (clojure.set/index true nil)]
        (record-result! "clojure.set/index" "(clojure.set/index true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/index" "(clojure.set/index true nil)" nil e)))))


(deftest test-clojure-set-intersection
  (testing "clojure.set/intersection"
    ;; Arities: [[s1] [s1 s2] [s1 s2 & sets]]
    (try
      (let [result (clojure.set/intersection nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil)" nil e)))
    (try
      (let [result (clojure.set/intersection true)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection true)" nil e)))
    (try
      (let [result (clojure.set/intersection nil nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil)" nil e)))
    (try
      (let [result (clojure.set/intersection nil true)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil true)" nil e)))
    (try
      (let [result (clojure.set/intersection true nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection true nil)" nil e)))
    (try
      (let [result (clojure.set/intersection nil nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil)" nil e)))
    (try
      (let [result (clojure.set/intersection nil nil nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil nil)" nil e)))
    (try
      (let [result (clojure.set/intersection nil nil nil nil)]
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/intersection" "(clojure.set/intersection nil nil nil nil)" nil e)))))


(deftest test-clojure-set-join
  (testing "clojure.set/join"
    ;; Arities: [[xrel yrel] [xrel yrel km]]
    (try
      (let [result (clojure.set/join nil nil)]
        (record-result! "clojure.set/join" "(clojure.set/join nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join nil nil)" nil e)))
    (try
      (let [result (clojure.set/join nil true)]
        (record-result! "clojure.set/join" "(clojure.set/join nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join nil true)" nil e)))
    (try
      (let [result (clojure.set/join true nil)]
        (record-result! "clojure.set/join" "(clojure.set/join true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join true nil)" nil e)))
    (try
      (let [result (clojure.set/join nil nil nil)]
        (record-result! "clojure.set/join" "(clojure.set/join nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join nil nil nil)" nil e)))
    (try
      (let [result (clojure.set/join nil nil true)]
        (record-result! "clojure.set/join" "(clojure.set/join nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join nil nil true)" nil e)))
    (try
      (let [result (clojure.set/join nil true nil)]
        (record-result! "clojure.set/join" "(clojure.set/join nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/join" "(clojure.set/join nil true nil)" nil e)))))


(deftest test-clojure-set-map-invert
  (testing "clojure.set/map-invert"
    ;; Arities: [[m]]
    (try
      (let [result (clojure.set/map-invert {})]
        (record-result! "clojure.set/map-invert" "(clojure.set/map-invert {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/map-invert" "(clojure.set/map-invert {})" nil e)))
    (try
      (let [result (clojure.set/map-invert {:a 1})]
        (record-result! "clojure.set/map-invert" "(clojure.set/map-invert {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/map-invert" "(clojure.set/map-invert {:a 1})" nil e)))))


(deftest test-clojure-set-project
  (testing "clojure.set/project"
    ;; Arities: [[xrel ks]]
    (try
      (let [result (clojure.set/project nil nil)]
        (record-result! "clojure.set/project" "(clojure.set/project nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/project" "(clojure.set/project nil nil)" nil e)))
    (try
      (let [result (clojure.set/project nil true)]
        (record-result! "clojure.set/project" "(clojure.set/project nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/project" "(clojure.set/project nil true)" nil e)))
    (try
      (let [result (clojure.set/project true nil)]
        (record-result! "clojure.set/project" "(clojure.set/project true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/project" "(clojure.set/project true nil)" nil e)))))


(deftest test-clojure-set-rename
  (testing "clojure.set/rename"
    ;; Arities: [[xrel kmap]]
    (try
      (let [result (clojure.set/rename nil nil)]
        (record-result! "clojure.set/rename" "(clojure.set/rename nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename" "(clojure.set/rename nil nil)" nil e)))
    (try
      (let [result (clojure.set/rename nil true)]
        (record-result! "clojure.set/rename" "(clojure.set/rename nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename" "(clojure.set/rename nil true)" nil e)))
    (try
      (let [result (clojure.set/rename true nil)]
        (record-result! "clojure.set/rename" "(clojure.set/rename true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename" "(clojure.set/rename true nil)" nil e)))))


(deftest test-clojure-set-rename-keys
  (testing "clojure.set/rename-keys"
    ;; Arities: [[map kmap]]
    (try
      (let [result (clojure.set/rename-keys {} nil)]
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {} nil)" nil e)))
    (try
      (let [result (clojure.set/rename-keys {} true)]
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {} true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {} true)" nil e)))
    (try
      (let [result (clojure.set/rename-keys {:a 1} nil)]
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {:a 1} nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/rename-keys" "(clojure.set/rename-keys {:a 1} nil)" nil e)))))


(deftest test-clojure-set-select
  (testing "clojure.set/select"
    ;; Arities: [[pred xset]]
    (try
      (let [result (clojure.set/select 'even? nil)]
        (record-result! "clojure.set/select" "(clojure.set/select 'even? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/select" "(clojure.set/select 'even? nil)" nil e)))
    (try
      (let [result (clojure.set/select 'even? true)]
        (record-result! "clojure.set/select" "(clojure.set/select 'even? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/select" "(clojure.set/select 'even? true)" nil e)))
    (try
      (let [result (clojure.set/select 'odd? nil)]
        (record-result! "clojure.set/select" "(clojure.set/select 'odd? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/select" "(clojure.set/select 'odd? nil)" nil e)))))


(deftest test-clojure-set-subset-p
  (testing "clojure.set/subset?"
    ;; Arities: [[set1 set2]]
    (try
      (let [result (clojure.set/subset? nil nil)]
        (record-result! "clojure.set/subset?" "(clojure.set/subset? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/subset?" "(clojure.set/subset? nil nil)" nil e)))
    (try
      (let [result (clojure.set/subset? nil true)]
        (record-result! "clojure.set/subset?" "(clojure.set/subset? nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/subset?" "(clojure.set/subset? nil true)" nil e)))
    (try
      (let [result (clojure.set/subset? true nil)]
        (record-result! "clojure.set/subset?" "(clojure.set/subset? true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/subset?" "(clojure.set/subset? true nil)" nil e)))))


(deftest test-clojure-set-superset-p
  (testing "clojure.set/superset?"
    ;; Arities: [[set1 set2]]
    (try
      (let [result (clojure.set/superset? nil nil)]
        (record-result! "clojure.set/superset?" "(clojure.set/superset? nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/superset?" "(clojure.set/superset? nil nil)" nil e)))
    (try
      (let [result (clojure.set/superset? nil true)]
        (record-result! "clojure.set/superset?" "(clojure.set/superset? nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/superset?" "(clojure.set/superset? nil true)" nil e)))
    (try
      (let [result (clojure.set/superset? true nil)]
        (record-result! "clojure.set/superset?" "(clojure.set/superset? true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/superset?" "(clojure.set/superset? true nil)" nil e)))))


(deftest test-clojure-set-union
  (testing "clojure.set/union"
    ;; Arities: [[] [s1] [s1 s2] [s1 s2 & sets]]
    (try
      (let [result (clojure.set/union)]
        (record-result! "clojure.set/union" "(clojure.set/union)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union)" nil e)))
    (try
      (let [result (clojure.set/union nil)]
        (record-result! "clojure.set/union" "(clojure.set/union nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union nil)" nil e)))
    (try
      (let [result (clojure.set/union true)]
        (record-result! "clojure.set/union" "(clojure.set/union true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union true)" nil e)))
    (try
      (let [result (clojure.set/union nil nil)]
        (record-result! "clojure.set/union" "(clojure.set/union nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union nil nil)" nil e)))
    (try
      (let [result (clojure.set/union nil true)]
        (record-result! "clojure.set/union" "(clojure.set/union nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union nil true)" nil e)))
    (try
      (let [result (clojure.set/union true nil)]
        (record-result! "clojure.set/union" "(clojure.set/union true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union true nil)" nil e)))
    (try
      (let [result (clojure.set/union nil nil)]
        (record-result! "clojure.set/union" "(clojure.set/union nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union nil nil)" nil e)))
    (try
      (let [result (clojure.set/union nil nil nil)]
        (record-result! "clojure.set/union" "(clojure.set/union nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.set/union" "(clojure.set/union nil nil nil)" nil e)))))


;; ============================================================
;; clojure.string - 21 functions
;; ============================================================

(deftest test-clojure-string-blank-p
  (testing "clojure.string/blank?"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/blank? "")]
        (record-result! "clojure.string/blank?" "(clojure.string/blank? \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/blank?" "(clojure.string/blank? \"\")" nil e)))
    (try
      (let [result (clojure.string/blank? "a")]
        (record-result! "clojure.string/blank?" "(clojure.string/blank? \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/blank?" "(clojure.string/blank? \"a\")" nil e)))))


(deftest test-clojure-string-capitalize
  (testing "clojure.string/capitalize"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/capitalize "")]
        (record-result! "clojure.string/capitalize" "(clojure.string/capitalize \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/capitalize" "(clojure.string/capitalize \"\")" nil e)))
    (try
      (let [result (clojure.string/capitalize "a")]
        (record-result! "clojure.string/capitalize" "(clojure.string/capitalize \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/capitalize" "(clojure.string/capitalize \"a\")" nil e)))))


(deftest test-clojure-string-ends-with-p
  (testing "clojure.string/ends-with?"
    ;; Arities: [[s substr]]
    (try
      (let [result (clojure.string/ends-with? "" nil)]
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/ends-with? "" true)]
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"\" true)" nil e)))
    (try
      (let [result (clojure.string/ends-with? "a" nil)]
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/ends-with?" "(clojure.string/ends-with? \"a\" nil)" nil e)))))


(deftest test-clojure-string-escape
  (testing "clojure.string/escape"
    ;; Arities: [[s cmap]]
    (try
      (let [result (clojure.string/escape "" nil)]
        (record-result! "clojure.string/escape" "(clojure.string/escape \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/escape" "(clojure.string/escape \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/escape "" true)]
        (record-result! "clojure.string/escape" "(clojure.string/escape \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/escape" "(clojure.string/escape \"\" true)" nil e)))
    (try
      (let [result (clojure.string/escape "a" nil)]
        (record-result! "clojure.string/escape" "(clojure.string/escape \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/escape" "(clojure.string/escape \"a\" nil)" nil e)))))


(deftest test-clojure-string-includes-p
  (testing "clojure.string/includes?"
    ;; Arities: [[s substr]]
    (try
      (let [result (clojure.string/includes? "" nil)]
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/includes? "" true)]
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"\" true)" nil e)))
    (try
      (let [result (clojure.string/includes? "a" nil)]
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/includes?" "(clojure.string/includes? \"a\" nil)" nil e)))))


(deftest test-clojure-string-index-of
  (testing "clojure.string/index-of"
    ;; Arities: [[s value] [s value from-index]]
    (try
      (let [result (clojure.string/index-of "" nil)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/index-of "" true)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" true)" nil e)))
    (try
      (let [result (clojure.string/index-of "a" nil)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"a\" nil)" nil e)))
    (try
      (let [result (clojure.string/index-of "" nil nil)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil nil)" nil e)))
    (try
      (let [result (clojure.string/index-of "" nil true)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" nil true)" nil e)))
    (try
      (let [result (clojure.string/index-of "" true nil)]
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/index-of" "(clojure.string/index-of \"\" true nil)" nil e)))))


(deftest test-clojure-string-join
  (testing "clojure.string/join"
    ;; Arities: [[coll] [separator coll]]
    (try
      (let [result (clojure.string/join nil)]
        (record-result! "clojure.string/join" "(clojure.string/join nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/join" "(clojure.string/join nil)" nil e)))
    (try
      (let [result (clojure.string/join [])]
        (record-result! "clojure.string/join" "(clojure.string/join [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/join" "(clojure.string/join [])" nil e)))
    (try
      (let [result (clojure.string/join nil nil)]
        (record-result! "clojure.string/join" "(clojure.string/join nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/join" "(clojure.string/join nil nil)" nil e)))
    (try
      (let [result (clojure.string/join nil [])]
        (record-result! "clojure.string/join" "(clojure.string/join nil [])" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/join" "(clojure.string/join nil [])" nil e)))
    (try
      (let [result (clojure.string/join true nil)]
        (record-result! "clojure.string/join" "(clojure.string/join true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/join" "(clojure.string/join true nil)" nil e)))))


(deftest test-clojure-string-last-index-of
  (testing "clojure.string/last-index-of"
    ;; Arities: [[s value] [s value from-index]]
    (try
      (let [result (clojure.string/last-index-of "" nil)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/last-index-of "" true)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" true)" nil e)))
    (try
      (let [result (clojure.string/last-index-of "a" nil)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"a\" nil)" nil e)))
    (try
      (let [result (clojure.string/last-index-of "" nil nil)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil nil)" nil e)))
    (try
      (let [result (clojure.string/last-index-of "" nil true)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" nil true)" nil e)))
    (try
      (let [result (clojure.string/last-index-of "" true nil)]
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/last-index-of" "(clojure.string/last-index-of \"\" true nil)" nil e)))))


(deftest test-clojure-string-lower-case
  (testing "clojure.string/lower-case"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/lower-case "")]
        (record-result! "clojure.string/lower-case" "(clojure.string/lower-case \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/lower-case" "(clojure.string/lower-case \"\")" nil e)))
    (try
      (let [result (clojure.string/lower-case "a")]
        (record-result! "clojure.string/lower-case" "(clojure.string/lower-case \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/lower-case" "(clojure.string/lower-case \"a\")" nil e)))))


(deftest test-clojure-string-re-quote-replacement
  (testing "clojure.string/re-quote-replacement"
    ;; Arities: [[replacement]]
    (try
      (let [result (clojure.string/re-quote-replacement nil)]
        (record-result! "clojure.string/re-quote-replacement" "(clojure.string/re-quote-replacement nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/re-quote-replacement" "(clojure.string/re-quote-replacement nil)" nil e)))
    (try
      (let [result (clojure.string/re-quote-replacement true)]
        (record-result! "clojure.string/re-quote-replacement" "(clojure.string/re-quote-replacement true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/re-quote-replacement" "(clojure.string/re-quote-replacement true)" nil e)))))


(deftest test-clojure-string-replace
  (testing "clojure.string/replace"
    ;; Arities: [[s match replacement]]
    (try
      (let [result (clojure.string/replace "" nil nil)]
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" nil nil)" nil e)))
    (try
      (let [result (clojure.string/replace "" nil true)]
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" nil true)" nil e)))
    (try
      (let [result (clojure.string/replace "" true nil)]
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace" "(clojure.string/replace \"\" true nil)" nil e)))))


(deftest test-clojure-string-replace-first
  (testing "clojure.string/replace-first"
    ;; Arities: [[s match replacement]]
    (try
      (let [result (clojure.string/replace-first "" nil nil)]
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" nil nil)" nil e)))
    (try
      (let [result (clojure.string/replace-first "" nil true)]
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" nil true)" nil e)))
    (try
      (let [result (clojure.string/replace-first "" true nil)]
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/replace-first" "(clojure.string/replace-first \"\" true nil)" nil e)))))


(deftest test-clojure-string-reverse
  (testing "clojure.string/reverse"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/reverse "")]
        (record-result! "clojure.string/reverse" "(clojure.string/reverse \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/reverse" "(clojure.string/reverse \"\")" nil e)))
    (try
      (let [result (clojure.string/reverse "a")]
        (record-result! "clojure.string/reverse" "(clojure.string/reverse \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/reverse" "(clojure.string/reverse \"a\")" nil e)))))


(deftest test-clojure-string-split
  (testing "clojure.string/split"
    ;; Arities: [[s re] [s re limit]]
    (try
      (let [result (clojure.string/split "" #"foo")]
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\")" nil e)))
    (try
      (let [result (clojure.string/split "" #"bar.*")]
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"bar.*\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"bar.*\")" nil e)))
    (try
      (let [result (clojure.string/split "a" #"foo")]
        (record-result! "clojure.string/split" "(clojure.string/split \"a\" #\"foo\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"a\" #\"foo\")" nil e)))
    (try
      (let [result (clojure.string/split "" #"foo" nil)]
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\" nil)" nil e)))
    (try
      (let [result (clojure.string/split "" #"foo" true)]
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"foo\" true)" nil e)))
    (try
      (let [result (clojure.string/split "" #"bar.*" nil)]
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"bar.*\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split" "(clojure.string/split \"\" #\"bar.*\" nil)" nil e)))))


(deftest test-clojure-string-split-lines
  (testing "clojure.string/split-lines"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/split-lines "")]
        (record-result! "clojure.string/split-lines" "(clojure.string/split-lines \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split-lines" "(clojure.string/split-lines \"\")" nil e)))
    (try
      (let [result (clojure.string/split-lines "a")]
        (record-result! "clojure.string/split-lines" "(clojure.string/split-lines \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/split-lines" "(clojure.string/split-lines \"a\")" nil e)))))


(deftest test-clojure-string-starts-with-p
  (testing "clojure.string/starts-with?"
    ;; Arities: [[s substr]]
    (try
      (let [result (clojure.string/starts-with? "" nil)]
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"\" nil)" nil e)))
    (try
      (let [result (clojure.string/starts-with? "" true)]
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"\" true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"\" true)" nil e)))
    (try
      (let [result (clojure.string/starts-with? "a" nil)]
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"a\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/starts-with?" "(clojure.string/starts-with? \"a\" nil)" nil e)))))


(deftest test-clojure-string-trim
  (testing "clojure.string/trim"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/trim "")]
        (record-result! "clojure.string/trim" "(clojure.string/trim \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trim" "(clojure.string/trim \"\")" nil e)))
    (try
      (let [result (clojure.string/trim "a")]
        (record-result! "clojure.string/trim" "(clojure.string/trim \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trim" "(clojure.string/trim \"a\")" nil e)))))


(deftest test-clojure-string-trim-newline
  (testing "clojure.string/trim-newline"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/trim-newline "")]
        (record-result! "clojure.string/trim-newline" "(clojure.string/trim-newline \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trim-newline" "(clojure.string/trim-newline \"\")" nil e)))
    (try
      (let [result (clojure.string/trim-newline "a")]
        (record-result! "clojure.string/trim-newline" "(clojure.string/trim-newline \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trim-newline" "(clojure.string/trim-newline \"a\")" nil e)))))


(deftest test-clojure-string-triml
  (testing "clojure.string/triml"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/triml "")]
        (record-result! "clojure.string/triml" "(clojure.string/triml \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/triml" "(clojure.string/triml \"\")" nil e)))
    (try
      (let [result (clojure.string/triml "a")]
        (record-result! "clojure.string/triml" "(clojure.string/triml \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/triml" "(clojure.string/triml \"a\")" nil e)))))


(deftest test-clojure-string-trimr
  (testing "clojure.string/trimr"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/trimr "")]
        (record-result! "clojure.string/trimr" "(clojure.string/trimr \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trimr" "(clojure.string/trimr \"\")" nil e)))
    (try
      (let [result (clojure.string/trimr "a")]
        (record-result! "clojure.string/trimr" "(clojure.string/trimr \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/trimr" "(clojure.string/trimr \"a\")" nil e)))))


(deftest test-clojure-string-upper-case
  (testing "clojure.string/upper-case"
    ;; Arities: [[s]]
    (try
      (let [result (clojure.string/upper-case "")]
        (record-result! "clojure.string/upper-case" "(clojure.string/upper-case \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/upper-case" "(clojure.string/upper-case \"\")" nil e)))
    (try
      (let [result (clojure.string/upper-case "a")]
        (record-result! "clojure.string/upper-case" "(clojure.string/upper-case \"a\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.string/upper-case" "(clojure.string/upper-case \"a\")" nil e)))))


;; ============================================================
;; clojure.test - 35 functions
;; ============================================================

(deftest test-clojure-test--startesting-contexts-star
  (testing "clojure.test/*testing-contexts*"
    ;; Arities: [list]
    (try
      (let [result (clojure.test/*testing-contexts*)]
        (record-result! "clojure.test/*testing-contexts*" "(clojure.test/*testing-contexts*)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/*testing-contexts*" "(clojure.test/*testing-contexts*)" nil e)))))


(deftest test-clojure-test--startesting-vars-star
  (testing "clojure.test/*testing-vars*"
    ;; Arities: [list]
    (try
      (let [result (clojure.test/*testing-vars*)]
        (record-result! "clojure.test/*testing-vars*" "(clojure.test/*testing-vars*)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/*testing-vars*" "(clojure.test/*testing-vars*)" nil e)))))


(deftest test-clojure-test-are
  (testing "clojure.test/are"
    ;; Arities: [[argv expr & args]]
    (try
      (let [result (clojure.test/are nil nil)]
        (record-result! "clojure.test/are" "(clojure.test/are nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/are" "(clojure.test/are nil nil)" nil e)))
    (try
      (let [result (clojure.test/are nil nil nil)]
        (record-result! "clojure.test/are" "(clojure.test/are nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/are" "(clojure.test/are nil nil nil)" nil e)))
    (try
      (let [result (clojure.test/are nil nil nil nil)]
        (record-result! "clojure.test/are" "(clojure.test/are nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/are" "(clojure.test/are nil nil nil nil)" nil e)))))


(deftest test-clojure-test-assert-any
  (testing "clojure.test/assert-any"
    ;; Arities: [[msg form]]
    (try
      (let [result (clojure.test/assert-any nil nil)]
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any nil nil)" nil e)))
    (try
      (let [result (clojure.test/assert-any nil true)]
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any nil true)" nil e)))
    (try
      (let [result (clojure.test/assert-any true nil)]
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-any" "(clojure.test/assert-any true nil)" nil e)))))


(deftest test-clojure-test-assert-predicate
  (testing "clojure.test/assert-predicate"
    ;; Arities: [[msg form]]
    (try
      (let [result (clojure.test/assert-predicate nil nil)]
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate nil nil)" nil e)))
    (try
      (let [result (clojure.test/assert-predicate nil true)]
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate nil true)" nil e)))
    (try
      (let [result (clojure.test/assert-predicate true nil)]
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/assert-predicate" "(clojure.test/assert-predicate true nil)" nil e)))))


(deftest test-clojure-test-compose-fixtures
  (testing "clojure.test/compose-fixtures"
    ;; Arities: [[f1 f2]]
    (try
      (let [result (clojure.test/compose-fixtures nil nil)]
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures nil nil)" nil e)))
    (try
      (let [result (clojure.test/compose-fixtures nil true)]
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures nil true)" nil e)))
    (try
      (let [result (clojure.test/compose-fixtures true nil)]
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/compose-fixtures" "(clojure.test/compose-fixtures true nil)" nil e)))))


(deftest test-clojure-test-deftest
  (testing "clojure.test/deftest"
    ;; Arities: [[name & body]]
    (try
      (let [result (clojure.test/deftest nil)]
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil)" nil e)))
    (try
      (let [result (clojure.test/deftest nil nil)]
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil nil)" nil e)))
    (try
      (let [result (clojure.test/deftest nil nil nil)]
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest" "(clojure.test/deftest nil nil nil)" nil e)))))


(deftest test-clojure-test-deftest-
  (testing "clojure.test/deftest-"
    ;; Arities: [[name & body]]
    (try
      (let [result (clojure.test/deftest- nil)]
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil)" nil e)))
    (try
      (let [result (clojure.test/deftest- nil nil)]
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil nil)" nil e)))
    (try
      (let [result (clojure.test/deftest- nil nil nil)]
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/deftest-" "(clojure.test/deftest- nil nil nil)" nil e)))))


(deftest test-clojure-test-do-report
  (testing "clojure.test/do-report"
    ;; Arities: [[m]]
    (try
      (let [result (clojure.test/do-report {})]
        (record-result! "clojure.test/do-report" "(clojure.test/do-report {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/do-report" "(clojure.test/do-report {})" nil e)))
    (try
      (let [result (clojure.test/do-report {:a 1})]
        (record-result! "clojure.test/do-report" "(clojure.test/do-report {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/do-report" "(clojure.test/do-report {:a 1})" nil e)))))


(deftest test-clojure-test-file-position
  (testing "clojure.test/file-position"
    ;; Arities: [[n]]
    (try
      (let [result (clojure.test/file-position 0)]
        (record-result! "clojure.test/file-position" "(clojure.test/file-position 0)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/file-position" "(clojure.test/file-position 0)" nil e)))
    (try
      (let [result (clojure.test/file-position 1)]
        (record-result! "clojure.test/file-position" "(clojure.test/file-position 1)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/file-position" "(clojure.test/file-position 1)" nil e)))))


(deftest test-clojure-test-function-p
  (testing "clojure.test/function?"
    ;; Arities: [[x]]
    (try
      (let [result (clojure.test/function? nil)]
        (record-result! "clojure.test/function?" "(clojure.test/function? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/function?" "(clojure.test/function? nil)" nil e)))
    (try
      (let [result (clojure.test/function? true)]
        (record-result! "clojure.test/function?" "(clojure.test/function? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/function?" "(clojure.test/function? true)" nil e)))))


(deftest test-clojure-test-get-possibly-unbound-var
  (testing "clojure.test/get-possibly-unbound-var"
    ;; Arities: [[v]]
    (try
      (let [result (clojure.test/get-possibly-unbound-var nil)]
        (record-result! "clojure.test/get-possibly-unbound-var" "(clojure.test/get-possibly-unbound-var nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/get-possibly-unbound-var" "(clojure.test/get-possibly-unbound-var nil)" nil e)))
    (try
      (let [result (clojure.test/get-possibly-unbound-var true)]
        (record-result! "clojure.test/get-possibly-unbound-var" "(clojure.test/get-possibly-unbound-var true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/get-possibly-unbound-var" "(clojure.test/get-possibly-unbound-var true)" nil e)))))


(deftest test-clojure-test-inc-report-counter
  (testing "clojure.test/inc-report-counter"
    ;; Arities: [[name]]
    (try
      (let [result (clojure.test/inc-report-counter nil)]
        (record-result! "clojure.test/inc-report-counter" "(clojure.test/inc-report-counter nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/inc-report-counter" "(clojure.test/inc-report-counter nil)" nil e)))
    (try
      (let [result (clojure.test/inc-report-counter true)]
        (record-result! "clojure.test/inc-report-counter" "(clojure.test/inc-report-counter true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/inc-report-counter" "(clojure.test/inc-report-counter true)" nil e)))))


(deftest test-clojure-test-is
  (testing "clojure.test/is"
    ;; Arities: [[form] [form msg]]
    (try
      (let [result (clojure.test/is nil)]
        (record-result! "clojure.test/is" "(clojure.test/is nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/is" "(clojure.test/is nil)" nil e)))
    (try
      (let [result (clojure.test/is true)]
        (record-result! "clojure.test/is" "(clojure.test/is true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/is" "(clojure.test/is true)" nil e)))
    (try
      (let [result (clojure.test/is nil nil)]
        (record-result! "clojure.test/is" "(clojure.test/is nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/is" "(clojure.test/is nil nil)" nil e)))
    (try
      (let [result (clojure.test/is nil true)]
        (record-result! "clojure.test/is" "(clojure.test/is nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/is" "(clojure.test/is nil true)" nil e)))
    (try
      (let [result (clojure.test/is true nil)]
        (record-result! "clojure.test/is" "(clojure.test/is true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/is" "(clojure.test/is true nil)" nil e)))))


(deftest test-clojure-test-join-fixtures
  (testing "clojure.test/join-fixtures"
    ;; Arities: [[fixtures]]
    (try
      (let [result (clojure.test/join-fixtures nil)]
        (record-result! "clojure.test/join-fixtures" "(clojure.test/join-fixtures nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/join-fixtures" "(clojure.test/join-fixtures nil)" nil e)))
    (try
      (let [result (clojure.test/join-fixtures true)]
        (record-result! "clojure.test/join-fixtures" "(clojure.test/join-fixtures true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/join-fixtures" "(clojure.test/join-fixtures true)" nil e)))))


(deftest test-clojure-test-run-all-tests
  (testing "clojure.test/run-all-tests"
    ;; Arities: [[] [re]]
    (try
      (let [result (clojure.test/run-all-tests)]
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests)" nil e)))
    (try
      (let [result (clojure.test/run-all-tests #"foo")]
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests #\"foo\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests #\"foo\")" nil e)))
    (try
      (let [result (clojure.test/run-all-tests #"bar.*")]
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests #\"bar.*\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-all-tests" "(clojure.test/run-all-tests #\"bar.*\")" nil e)))))


(deftest test-clojure-test-run-test
  (testing "clojure.test/run-test"
    ;; Arities: [[test-symbol]]
    (try
      (let [result (clojure.test/run-test nil)]
        (record-result! "clojure.test/run-test" "(clojure.test/run-test nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-test" "(clojure.test/run-test nil)" nil e)))
    (try
      (let [result (clojure.test/run-test true)]
        (record-result! "clojure.test/run-test" "(clojure.test/run-test true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-test" "(clojure.test/run-test true)" nil e)))))


(deftest test-clojure-test-run-test-var
  (testing "clojure.test/run-test-var"
    ;; Arities: [[v]]
    (try
      (let [result (clojure.test/run-test-var nil)]
        (record-result! "clojure.test/run-test-var" "(clojure.test/run-test-var nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-test-var" "(clojure.test/run-test-var nil)" nil e)))
    (try
      (let [result (clojure.test/run-test-var true)]
        (record-result! "clojure.test/run-test-var" "(clojure.test/run-test-var true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-test-var" "(clojure.test/run-test-var true)" nil e)))))


(deftest test-clojure-test-run-tests
  (testing "clojure.test/run-tests"
    ;; Arities: [[] [& namespaces]]
    (try
      (let [result (clojure.test/run-tests)]
        (record-result! "clojure.test/run-tests" "(clojure.test/run-tests)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-tests" "(clojure.test/run-tests)" nil e)))
    (try
      (let [result (clojure.test/run-tests)]
        (record-result! "clojure.test/run-tests" "(clojure.test/run-tests)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/run-tests" "(clojure.test/run-tests)" nil e)))))


(deftest test-clojure-test-set-test
  (testing "clojure.test/set-test"
    ;; Arities: [[name & body]]
    (try
      (let [result (clojure.test/set-test nil)]
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil)" nil e)))
    (try
      (let [result (clojure.test/set-test nil nil)]
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil nil)" nil e)))
    (try
      (let [result (clojure.test/set-test nil nil nil)]
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/set-test" "(clojure.test/set-test nil nil nil)" nil e)))))


(deftest test-clojure-test-successful-p
  (testing "clojure.test/successful?"
    ;; Arities: [[summary]]
    (try
      (let [result (clojure.test/successful? nil)]
        (record-result! "clojure.test/successful?" "(clojure.test/successful? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/successful?" "(clojure.test/successful? nil)" nil e)))
    (try
      (let [result (clojure.test/successful? true)]
        (record-result! "clojure.test/successful?" "(clojure.test/successful? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/successful?" "(clojure.test/successful? true)" nil e)))))


(deftest test-clojure-test-test-all-vars
  (testing "clojure.test/test-all-vars"
    ;; Arities: [[ns]]
    (try
      (let [result (clojure.test/test-all-vars nil)]
        (record-result! "clojure.test/test-all-vars" "(clojure.test/test-all-vars nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-all-vars" "(clojure.test/test-all-vars nil)" nil e)))
    (try
      (let [result (clojure.test/test-all-vars true)]
        (record-result! "clojure.test/test-all-vars" "(clojure.test/test-all-vars true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-all-vars" "(clojure.test/test-all-vars true)" nil e)))))


(deftest test-clojure-test-test-ns
  (testing "clojure.test/test-ns"
    ;; Arities: [[ns]]
    (try
      (let [result (clojure.test/test-ns nil)]
        (record-result! "clojure.test/test-ns" "(clojure.test/test-ns nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-ns" "(clojure.test/test-ns nil)" nil e)))
    (try
      (let [result (clojure.test/test-ns true)]
        (record-result! "clojure.test/test-ns" "(clojure.test/test-ns true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-ns" "(clojure.test/test-ns true)" nil e)))))


(deftest test-clojure-test-test-var
  (testing "clojure.test/test-var"
    ;; Arities: [[v]]
    (try
      (let [result (clojure.test/test-var nil)]
        (record-result! "clojure.test/test-var" "(clojure.test/test-var nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-var" "(clojure.test/test-var nil)" nil e)))
    (try
      (let [result (clojure.test/test-var true)]
        (record-result! "clojure.test/test-var" "(clojure.test/test-var true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-var" "(clojure.test/test-var true)" nil e)))))


(deftest test-clojure-test-test-vars
  (testing "clojure.test/test-vars"
    ;; Arities: [[vars]]
    (try
      (let [result (clojure.test/test-vars nil)]
        (record-result! "clojure.test/test-vars" "(clojure.test/test-vars nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-vars" "(clojure.test/test-vars nil)" nil e)))
    (try
      (let [result (clojure.test/test-vars true)]
        (record-result! "clojure.test/test-vars" "(clojure.test/test-vars true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/test-vars" "(clojure.test/test-vars true)" nil e)))))


(deftest test-clojure-test-testing
  (testing "clojure.test/testing"
    ;; Arities: [[string & body]]
    (try
      (let [result (clojure.test/testing "")]
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\")" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\")" nil e)))
    (try
      (let [result (clojure.test/testing "" nil)]
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\" nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\" nil)" nil e)))
    (try
      (let [result (clojure.test/testing "" nil nil)]
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\" nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing" "(clojure.test/testing \"\" nil nil)" nil e)))))


(deftest test-clojure-test-testing-contexts-str
  (testing "clojure.test/testing-contexts-str"
    ;; Arities: [[]]
    (try
      (let [result (clojure.test/testing-contexts-str)]
        (record-result! "clojure.test/testing-contexts-str" "(clojure.test/testing-contexts-str)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing-contexts-str" "(clojure.test/testing-contexts-str)" nil e)))))


(deftest test-clojure-test-testing-vars-str
  (testing "clojure.test/testing-vars-str"
    ;; Arities: [[m]]
    (try
      (let [result (clojure.test/testing-vars-str {})]
        (record-result! "clojure.test/testing-vars-str" "(clojure.test/testing-vars-str {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing-vars-str" "(clojure.test/testing-vars-str {})" nil e)))
    (try
      (let [result (clojure.test/testing-vars-str {:a 1})]
        (record-result! "clojure.test/testing-vars-str" "(clojure.test/testing-vars-str {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/testing-vars-str" "(clojure.test/testing-vars-str {:a 1})" nil e)))))


(deftest test-clojure-test-try-expr
  (testing "clojure.test/try-expr"
    ;; Arities: [[msg form]]
    (try
      (let [result (clojure.test/try-expr nil nil)]
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr nil nil)" nil e)))
    (try
      (let [result (clojure.test/try-expr nil true)]
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr nil true)" nil e)))
    (try
      (let [result (clojure.test/try-expr true nil)]
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/try-expr" "(clojure.test/try-expr true nil)" nil e)))))


(deftest test-clojure-test-with-test
  (testing "clojure.test/with-test"
    ;; Arities: [[definition & body]]
    (try
      (let [result (clojure.test/with-test nil)]
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil)" nil e)))
    (try
      (let [result (clojure.test/with-test nil nil)]
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil nil)" nil e)))
    (try
      (let [result (clojure.test/with-test nil nil nil)]
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/with-test" "(clojure.test/with-test nil nil nil)" nil e)))))


(deftest test-clojure-test-with-test-out
  (testing "clojure.test/with-test-out"
    ;; Arities: [[& body]]
    (try
      (let [result (clojure.test/with-test-out)]
        (record-result! "clojure.test/with-test-out" "(clojure.test/with-test-out)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.test/with-test-out" "(clojure.test/with-test-out)" nil e)))))


;; ============================================================
;; clojure.walk - 10 functions
;; ============================================================

(deftest test-clojure-walk-keywordize-keys
  (testing "clojure.walk/keywordize-keys"
    ;; Arities: [[m]]
    (try
      (let [result (clojure.walk/keywordize-keys {})]
        (record-result! "clojure.walk/keywordize-keys" "(clojure.walk/keywordize-keys {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/keywordize-keys" "(clojure.walk/keywordize-keys {})" nil e)))
    (try
      (let [result (clojure.walk/keywordize-keys {:a 1})]
        (record-result! "clojure.walk/keywordize-keys" "(clojure.walk/keywordize-keys {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/keywordize-keys" "(clojure.walk/keywordize-keys {:a 1})" nil e)))))


(deftest test-clojure-walk-macroexpand-all
  (testing "clojure.walk/macroexpand-all"
    ;; Arities: [[form]]
    (try
      (let [result (clojure.walk/macroexpand-all nil)]
        (record-result! "clojure.walk/macroexpand-all" "(clojure.walk/macroexpand-all nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/macroexpand-all" "(clojure.walk/macroexpand-all nil)" nil e)))
    (try
      (let [result (clojure.walk/macroexpand-all true)]
        (record-result! "clojure.walk/macroexpand-all" "(clojure.walk/macroexpand-all true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/macroexpand-all" "(clojure.walk/macroexpand-all true)" nil e)))))


(deftest test-clojure-walk-postwalk
  (testing "clojure.walk/postwalk"
    ;; Arities: [[f form]]
    (try
      (let [result (clojure.walk/postwalk 'inc nil)]
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'inc nil)" nil e)))
    (try
      (let [result (clojure.walk/postwalk 'inc true)]
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'inc true)" nil e)))
    (try
      (let [result (clojure.walk/postwalk 'dec nil)]
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk" "(clojure.walk/postwalk 'dec nil)" nil e)))))


(deftest test-clojure-walk-postwalk-demo
  (testing "clojure.walk/postwalk-demo"
    ;; Arities: [[form]]
    (try
      (let [result (clojure.walk/postwalk-demo nil)]
        (record-result! "clojure.walk/postwalk-demo" "(clojure.walk/postwalk-demo nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk-demo" "(clojure.walk/postwalk-demo nil)" nil e)))
    (try
      (let [result (clojure.walk/postwalk-demo true)]
        (record-result! "clojure.walk/postwalk-demo" "(clojure.walk/postwalk-demo true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk-demo" "(clojure.walk/postwalk-demo true)" nil e)))))


(deftest test-clojure-walk-postwalk-replace
  (testing "clojure.walk/postwalk-replace"
    ;; Arities: [[smap form]]
    (try
      (let [result (clojure.walk/postwalk-replace nil nil)]
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace nil nil)" nil e)))
    (try
      (let [result (clojure.walk/postwalk-replace nil true)]
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace nil true)" nil e)))
    (try
      (let [result (clojure.walk/postwalk-replace true nil)]
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/postwalk-replace" "(clojure.walk/postwalk-replace true nil)" nil e)))))


(deftest test-clojure-walk-prewalk
  (testing "clojure.walk/prewalk"
    ;; Arities: [[f form]]
    (try
      (let [result (clojure.walk/prewalk 'inc nil)]
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'inc nil)" nil e)))
    (try
      (let [result (clojure.walk/prewalk 'inc true)]
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'inc true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'inc true)" nil e)))
    (try
      (let [result (clojure.walk/prewalk 'dec nil)]
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'dec nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk" "(clojure.walk/prewalk 'dec nil)" nil e)))))


(deftest test-clojure-walk-prewalk-demo
  (testing "clojure.walk/prewalk-demo"
    ;; Arities: [[form]]
    (try
      (let [result (clojure.walk/prewalk-demo nil)]
        (record-result! "clojure.walk/prewalk-demo" "(clojure.walk/prewalk-demo nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk-demo" "(clojure.walk/prewalk-demo nil)" nil e)))
    (try
      (let [result (clojure.walk/prewalk-demo true)]
        (record-result! "clojure.walk/prewalk-demo" "(clojure.walk/prewalk-demo true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk-demo" "(clojure.walk/prewalk-demo true)" nil e)))))


(deftest test-clojure-walk-prewalk-replace
  (testing "clojure.walk/prewalk-replace"
    ;; Arities: [[smap form]]
    (try
      (let [result (clojure.walk/prewalk-replace nil nil)]
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace nil nil)" nil e)))
    (try
      (let [result (clojure.walk/prewalk-replace nil true)]
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace nil true)" nil e)))
    (try
      (let [result (clojure.walk/prewalk-replace true nil)]
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/prewalk-replace" "(clojure.walk/prewalk-replace true nil)" nil e)))))


(deftest test-clojure-walk-stringify-keys
  (testing "clojure.walk/stringify-keys"
    ;; Arities: [[m]]
    (try
      (let [result (clojure.walk/stringify-keys {})]
        (record-result! "clojure.walk/stringify-keys" "(clojure.walk/stringify-keys {})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/stringify-keys" "(clojure.walk/stringify-keys {})" nil e)))
    (try
      (let [result (clojure.walk/stringify-keys {:a 1})]
        (record-result! "clojure.walk/stringify-keys" "(clojure.walk/stringify-keys {:a 1})" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/stringify-keys" "(clojure.walk/stringify-keys {:a 1})" nil e)))))


(deftest test-clojure-walk-walk
  (testing "clojure.walk/walk"
    ;; Arities: [[inner outer form]]
    (try
      (let [result (clojure.walk/walk nil nil nil)]
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil nil nil)" nil e)))
    (try
      (let [result (clojure.walk/walk nil nil true)]
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil nil true)" nil e)))
    (try
      (let [result (clojure.walk/walk nil true nil)]
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.walk/walk" "(clojure.walk/walk nil true nil)" nil e)))))


;; ============================================================
;; clojure.zip - 28 functions
;; ============================================================

(deftest test-clojure-zip-append-child
  (testing "clojure.zip/append-child"
    ;; Arities: [[loc item]]
    (try
      (let [result (clojure.zip/append-child nil nil)]
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child nil nil)" nil e)))
    (try
      (let [result (clojure.zip/append-child nil true)]
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child nil true)" nil e)))
    (try
      (let [result (clojure.zip/append-child true nil)]
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/append-child" "(clojure.zip/append-child true nil)" nil e)))))


(deftest test-clojure-zip-branch-p
  (testing "clojure.zip/branch?"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/branch? nil)]
        (record-result! "clojure.zip/branch?" "(clojure.zip/branch? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/branch?" "(clojure.zip/branch? nil)" nil e)))
    (try
      (let [result (clojure.zip/branch? true)]
        (record-result! "clojure.zip/branch?" "(clojure.zip/branch? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/branch?" "(clojure.zip/branch? true)" nil e)))))


(deftest test-clojure-zip-children
  (testing "clojure.zip/children"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/children nil)]
        (record-result! "clojure.zip/children" "(clojure.zip/children nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/children" "(clojure.zip/children nil)" nil e)))
    (try
      (let [result (clojure.zip/children true)]
        (record-result! "clojure.zip/children" "(clojure.zip/children true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/children" "(clojure.zip/children true)" nil e)))))


(deftest test-clojure-zip-down
  (testing "clojure.zip/down"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/down nil)]
        (record-result! "clojure.zip/down" "(clojure.zip/down nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/down" "(clojure.zip/down nil)" nil e)))
    (try
      (let [result (clojure.zip/down true)]
        (record-result! "clojure.zip/down" "(clojure.zip/down true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/down" "(clojure.zip/down true)" nil e)))))


(deftest test-clojure-zip-edit
  (testing "clojure.zip/edit"
    ;; Arities: [[loc f & args]]
    (try
      (let [result (clojure.zip/edit nil 'inc)]
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc)" nil e)))
    (try
      (let [result (clojure.zip/edit nil 'inc nil)]
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc nil)" nil e)))
    (try
      (let [result (clojure.zip/edit nil 'inc nil nil)]
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/edit" "(clojure.zip/edit nil 'inc nil nil)" nil e)))))


(deftest test-clojure-zip-end-p
  (testing "clojure.zip/end?"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/end? nil)]
        (record-result! "clojure.zip/end?" "(clojure.zip/end? nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/end?" "(clojure.zip/end? nil)" nil e)))
    (try
      (let [result (clojure.zip/end? true)]
        (record-result! "clojure.zip/end?" "(clojure.zip/end? true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/end?" "(clojure.zip/end? true)" nil e)))))


(deftest test-clojure-zip-insert-child
  (testing "clojure.zip/insert-child"
    ;; Arities: [[loc item]]
    (try
      (let [result (clojure.zip/insert-child nil nil)]
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child nil nil)" nil e)))
    (try
      (let [result (clojure.zip/insert-child nil true)]
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child nil true)" nil e)))
    (try
      (let [result (clojure.zip/insert-child true nil)]
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-child" "(clojure.zip/insert-child true nil)" nil e)))))


(deftest test-clojure-zip-insert-left
  (testing "clojure.zip/insert-left"
    ;; Arities: [[loc item]]
    (try
      (let [result (clojure.zip/insert-left nil nil)]
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left nil nil)" nil e)))
    (try
      (let [result (clojure.zip/insert-left nil true)]
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left nil true)" nil e)))
    (try
      (let [result (clojure.zip/insert-left true nil)]
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-left" "(clojure.zip/insert-left true nil)" nil e)))))


(deftest test-clojure-zip-insert-right
  (testing "clojure.zip/insert-right"
    ;; Arities: [[loc item]]
    (try
      (let [result (clojure.zip/insert-right nil nil)]
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right nil nil)" nil e)))
    (try
      (let [result (clojure.zip/insert-right nil true)]
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right nil true)" nil e)))
    (try
      (let [result (clojure.zip/insert-right true nil)]
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/insert-right" "(clojure.zip/insert-right true nil)" nil e)))))


(deftest test-clojure-zip-left
  (testing "clojure.zip/left"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/left nil)]
        (record-result! "clojure.zip/left" "(clojure.zip/left nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/left" "(clojure.zip/left nil)" nil e)))
    (try
      (let [result (clojure.zip/left true)]
        (record-result! "clojure.zip/left" "(clojure.zip/left true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/left" "(clojure.zip/left true)" nil e)))))


(deftest test-clojure-zip-leftmost
  (testing "clojure.zip/leftmost"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/leftmost nil)]
        (record-result! "clojure.zip/leftmost" "(clojure.zip/leftmost nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/leftmost" "(clojure.zip/leftmost nil)" nil e)))
    (try
      (let [result (clojure.zip/leftmost true)]
        (record-result! "clojure.zip/leftmost" "(clojure.zip/leftmost true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/leftmost" "(clojure.zip/leftmost true)" nil e)))))


(deftest test-clojure-zip-lefts
  (testing "clojure.zip/lefts"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/lefts nil)]
        (record-result! "clojure.zip/lefts" "(clojure.zip/lefts nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/lefts" "(clojure.zip/lefts nil)" nil e)))
    (try
      (let [result (clojure.zip/lefts true)]
        (record-result! "clojure.zip/lefts" "(clojure.zip/lefts true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/lefts" "(clojure.zip/lefts true)" nil e)))))


(deftest test-clojure-zip-make-node
  (testing "clojure.zip/make-node"
    ;; Arities: [[loc node children]]
    (try
      (let [result (clojure.zip/make-node nil nil nil)]
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil nil nil)" nil e)))
    (try
      (let [result (clojure.zip/make-node nil nil true)]
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil nil true)" nil e)))
    (try
      (let [result (clojure.zip/make-node nil true nil)]
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/make-node" "(clojure.zip/make-node nil true nil)" nil e)))))


(deftest test-clojure-zip-next
  (testing "clojure.zip/next"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/next nil)]
        (record-result! "clojure.zip/next" "(clojure.zip/next nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/next" "(clojure.zip/next nil)" nil e)))
    (try
      (let [result (clojure.zip/next true)]
        (record-result! "clojure.zip/next" "(clojure.zip/next true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/next" "(clojure.zip/next true)" nil e)))))


(deftest test-clojure-zip-node
  (testing "clojure.zip/node"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/node nil)]
        (record-result! "clojure.zip/node" "(clojure.zip/node nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/node" "(clojure.zip/node nil)" nil e)))
    (try
      (let [result (clojure.zip/node true)]
        (record-result! "clojure.zip/node" "(clojure.zip/node true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/node" "(clojure.zip/node true)" nil e)))))


(deftest test-clojure-zip-path
  (testing "clojure.zip/path"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/path nil)]
        (record-result! "clojure.zip/path" "(clojure.zip/path nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/path" "(clojure.zip/path nil)" nil e)))
    (try
      (let [result (clojure.zip/path true)]
        (record-result! "clojure.zip/path" "(clojure.zip/path true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/path" "(clojure.zip/path true)" nil e)))))


(deftest test-clojure-zip-prev
  (testing "clojure.zip/prev"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/prev nil)]
        (record-result! "clojure.zip/prev" "(clojure.zip/prev nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/prev" "(clojure.zip/prev nil)" nil e)))
    (try
      (let [result (clojure.zip/prev true)]
        (record-result! "clojure.zip/prev" "(clojure.zip/prev true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/prev" "(clojure.zip/prev true)" nil e)))))


(deftest test-clojure-zip-remove
  (testing "clojure.zip/remove"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/remove nil)]
        (record-result! "clojure.zip/remove" "(clojure.zip/remove nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/remove" "(clojure.zip/remove nil)" nil e)))
    (try
      (let [result (clojure.zip/remove true)]
        (record-result! "clojure.zip/remove" "(clojure.zip/remove true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/remove" "(clojure.zip/remove true)" nil e)))))


(deftest test-clojure-zip-replace
  (testing "clojure.zip/replace"
    ;; Arities: [[loc node]]
    (try
      (let [result (clojure.zip/replace nil nil)]
        (record-result! "clojure.zip/replace" "(clojure.zip/replace nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/replace" "(clojure.zip/replace nil nil)" nil e)))
    (try
      (let [result (clojure.zip/replace nil true)]
        (record-result! "clojure.zip/replace" "(clojure.zip/replace nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/replace" "(clojure.zip/replace nil true)" nil e)))
    (try
      (let [result (clojure.zip/replace true nil)]
        (record-result! "clojure.zip/replace" "(clojure.zip/replace true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/replace" "(clojure.zip/replace true nil)" nil e)))))


(deftest test-clojure-zip-right
  (testing "clojure.zip/right"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/right nil)]
        (record-result! "clojure.zip/right" "(clojure.zip/right nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/right" "(clojure.zip/right nil)" nil e)))
    (try
      (let [result (clojure.zip/right true)]
        (record-result! "clojure.zip/right" "(clojure.zip/right true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/right" "(clojure.zip/right true)" nil e)))))


(deftest test-clojure-zip-rightmost
  (testing "clojure.zip/rightmost"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/rightmost nil)]
        (record-result! "clojure.zip/rightmost" "(clojure.zip/rightmost nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/rightmost" "(clojure.zip/rightmost nil)" nil e)))
    (try
      (let [result (clojure.zip/rightmost true)]
        (record-result! "clojure.zip/rightmost" "(clojure.zip/rightmost true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/rightmost" "(clojure.zip/rightmost true)" nil e)))))


(deftest test-clojure-zip-rights
  (testing "clojure.zip/rights"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/rights nil)]
        (record-result! "clojure.zip/rights" "(clojure.zip/rights nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/rights" "(clojure.zip/rights nil)" nil e)))
    (try
      (let [result (clojure.zip/rights true)]
        (record-result! "clojure.zip/rights" "(clojure.zip/rights true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/rights" "(clojure.zip/rights true)" nil e)))))


(deftest test-clojure-zip-root
  (testing "clojure.zip/root"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/root nil)]
        (record-result! "clojure.zip/root" "(clojure.zip/root nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/root" "(clojure.zip/root nil)" nil e)))
    (try
      (let [result (clojure.zip/root true)]
        (record-result! "clojure.zip/root" "(clojure.zip/root true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/root" "(clojure.zip/root true)" nil e)))))


(deftest test-clojure-zip-seq-zip
  (testing "clojure.zip/seq-zip"
    ;; Arities: [[root]]
    (try
      (let [result (clojure.zip/seq-zip nil)]
        (record-result! "clojure.zip/seq-zip" "(clojure.zip/seq-zip nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/seq-zip" "(clojure.zip/seq-zip nil)" nil e)))
    (try
      (let [result (clojure.zip/seq-zip true)]
        (record-result! "clojure.zip/seq-zip" "(clojure.zip/seq-zip true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/seq-zip" "(clojure.zip/seq-zip true)" nil e)))))


(deftest test-clojure-zip-up
  (testing "clojure.zip/up"
    ;; Arities: [[loc]]
    (try
      (let [result (clojure.zip/up nil)]
        (record-result! "clojure.zip/up" "(clojure.zip/up nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/up" "(clojure.zip/up nil)" nil e)))
    (try
      (let [result (clojure.zip/up true)]
        (record-result! "clojure.zip/up" "(clojure.zip/up true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/up" "(clojure.zip/up true)" nil e)))))


(deftest test-clojure-zip-vector-zip
  (testing "clojure.zip/vector-zip"
    ;; Arities: [[root]]
    (try
      (let [result (clojure.zip/vector-zip nil)]
        (record-result! "clojure.zip/vector-zip" "(clojure.zip/vector-zip nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/vector-zip" "(clojure.zip/vector-zip nil)" nil e)))
    (try
      (let [result (clojure.zip/vector-zip true)]
        (record-result! "clojure.zip/vector-zip" "(clojure.zip/vector-zip true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/vector-zip" "(clojure.zip/vector-zip true)" nil e)))))


(deftest test-clojure-zip-xml-zip
  (testing "clojure.zip/xml-zip"
    ;; Arities: [[root]]
    (try
      (let [result (clojure.zip/xml-zip nil)]
        (record-result! "clojure.zip/xml-zip" "(clojure.zip/xml-zip nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/xml-zip" "(clojure.zip/xml-zip nil)" nil e)))
    (try
      (let [result (clojure.zip/xml-zip true)]
        (record-result! "clojure.zip/xml-zip" "(clojure.zip/xml-zip true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/xml-zip" "(clojure.zip/xml-zip true)" nil e)))))


(deftest test-clojure-zip-zipper
  (testing "clojure.zip/zipper"
    ;; Arities: [[branch? children make-node root]]
    (try
      (let [result (clojure.zip/zipper nil nil nil nil)]
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil nil nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil nil nil)" nil e)))
    (try
      (let [result (clojure.zip/zipper nil nil nil true)]
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil nil true)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil nil true)" nil e)))
    (try
      (let [result (clojure.zip/zipper nil nil true nil)]
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil true nil)" result nil))
      (catch #?(:clj Throwable :cljs :default) e
        (record-result! "clojure.zip/zipper" "(clojure.zip/zipper nil nil true nil)" nil e)))))



;; ============================================================
;; BENCHMARKS
;; ============================================================

(defbench bench-clojure-core-list 100
  (dotimes [_ 100] (list)))

(defbench bench-clojure-core-cons 100
  (dotimes [_ 100] (cons)))

(defbench bench-clojure-core-let 100
  (dotimes [_ 100] (let)))

(defbench bench-clojure-core-loop 100
  (dotimes [_ 100] (loop)))

(defbench bench-clojure-core-fn 100
  (dotimes [_ 100] (fn)))

(defbench bench-clojure-core-first 100
  (dotimes [_ 100] (first)))

(defbench bench-clojure-core-next 100
  (dotimes [_ 100] (next)))

(defbench bench-clojure-core-rest 100
  (dotimes [_ 100] (rest)))

(defbench bench-clojure-core-conj 100
  (dotimes [_ 100] (conj)))

(defbench bench-clojure-core-second 100
  (dotimes [_ 100] (second)))

(defbench bench-clojure-core-ffirst 100
  (dotimes [_ 100] (ffirst)))

(defbench bench-clojure-core-nfirst 100
  (dotimes [_ 100] (nfirst)))

(defbench bench-clojure-core-fnext 100
  (dotimes [_ 100] (fnext)))

(defbench bench-clojure-core-nnext 100
  (dotimes [_ 100] (nnext)))

(defbench bench-clojure-core-seq 100
  (dotimes [_ 100] (seq)))

(defbench bench-clojure-core-instance-p 100
  (dotimes [_ 100] (instance?)))

(defbench bench-clojure-core-seq-p 100
  (dotimes [_ 100] (seq?)))

(defbench bench-clojure-core-char-p 100
  (dotimes [_ 100] (char?)))

(defbench bench-clojure-core-string-p 100
  (dotimes [_ 100] (string?)))

(defbench bench-clojure-core-map-p 100
  (dotimes [_ 100] (map?)))

(defbench bench-clojure-core-vector-p 100
  (dotimes [_ 100] (vector?)))

(defbench bench-clojure-core-assoc 100
  (dotimes [_ 100] (assoc)))

(defbench bench-clojure-core-meta 100
  (dotimes [_ 100] (meta)))

(defbench bench-clojure-core-with-meta 100
  (dotimes [_ 100] (with-meta)))

(defbench bench-clojure-core-last 100
  (dotimes [_ 100] (last)))

(defbench bench-clojure-core-butlast 100
  (dotimes [_ 100] (butlast)))

(defbench bench-clojure-core-defn 100
  (dotimes [_ 100] (defn)))

(defbench bench-clojure-core-to-array 100
  (dotimes [_ 100] (to-array nil)))

(defbench bench-clojure-core-cast 100
  (dotimes [_ 100] (cast nil nil)))

(defbench bench-clojure-core-vector 100
  (dotimes [_ 100] (vector)))

(defbench bench-clojure-core-vec 100
  (dotimes [_ 100] (vec nil)))

(defbench bench-clojure-core-hash-map 100
  (dotimes [_ 100] (hash-map)))

(defbench bench-clojure-core-hash-set 100
  (dotimes [_ 100] (hash-set)))

(defbench bench-clojure-core-sorted-map 100
  (dotimes [_ 100] (sorted-map)))

(defbench bench-clojure-core-sorted-map-by 100
  (dotimes [_ 100] (sorted-map-by nil)))

(defbench bench-clojure-core-sorted-set 100
  (dotimes [_ 100] (sorted-set)))

(defbench bench-clojure-core-sorted-set-by 100
  (dotimes [_ 100] (sorted-set-by nil)))

(defbench bench-clojure-core-nil-p 100
  (dotimes [_ 100] (nil? nil)))

(defbench bench-clojure-core-defmacro 100
  (dotimes [_ 100] (defmacro)))

(defbench bench-clojure-core-when 100
  (dotimes [_ 100] (when nil)))

(defbench bench-clojure-core-when-not 100
  (dotimes [_ 100] (when-not nil)))

(defbench bench-clojure-core-false-p 100
  (dotimes [_ 100] (false? nil)))

(defbench bench-clojure-core-true-p 100
  (dotimes [_ 100] (true? nil)))

(defbench bench-clojure-core-boolean-p 100
  (dotimes [_ 100] (boolean? nil)))

(defbench bench-clojure-core-not 100
  (dotimes [_ 100] (not nil)))

(defbench bench-clojure-core-some-p 100
  (dotimes [_ 100] (some? nil)))

(defbench bench-clojure-core-any-p 100
  (dotimes [_ 100] (any? nil)))

(defbench bench-clojure-core-str 100
  (dotimes [_ 100] (str)))

(defbench bench-clojure-core-symbol-p 100
  (dotimes [_ 100] (symbol? nil)))

(defbench bench-clojure-core-keyword-p 100
  (dotimes [_ 100] (keyword? nil)))

(defbench bench-clojure-core-cond 100
  (dotimes [_ 100] (cond)))

(defbench bench-clojure-core-symbol 100
  (dotimes [_ 100] (symbol nil)))

(defbench bench-clojure-core-gensym 100
  (dotimes [_ 100] (gensym)))

(defbench bench-clojure-core-keyword 100
  (dotimes [_ 100] (keyword nil)))

(defbench bench-clojure-core-find-keyword 100
  (dotimes [_ 100] (find-keyword nil)))

(defbench bench-clojure-core-list-star 100
  (dotimes [_ 100] (list* nil)))

(defbench bench-clojure-core-apply 100
  (dotimes [_ 100] (apply 'inc nil)))

(defbench bench-clojure-core-vary-meta 100
  (dotimes [_ 100] (vary-meta nil 'inc)))

(defbench bench-clojure-core-lazy-seq 100
  (dotimes [_ 100] (lazy-seq)))

(defbench bench-clojure-core-chunk-buffer 100
  (dotimes [_ 100] (chunk-buffer nil)))

(defbench bench-clojure-core-chunk-append 100
  (dotimes [_ 100] (chunk-append nil nil)))

(defbench bench-clojure-core-chunk 100
  (dotimes [_ 100] (chunk nil)))

(defbench bench-clojure-core-chunk-first 100
  (dotimes [_ 100] (chunk-first "")))

(defbench bench-clojure-core-chunk-rest 100
  (dotimes [_ 100] (chunk-rest "")))

(defbench bench-clojure-core-chunk-next 100
  (dotimes [_ 100] (chunk-next "")))

(defbench bench-clojure-core-chunk-cons 100
  (dotimes [_ 100] (chunk-cons nil nil)))

(defbench bench-clojure-core-chunked-seq-p 100
  (dotimes [_ 100] (chunked-seq? "")))

(defbench bench-clojure-core-concat 100
  (dotimes [_ 100] (concat)))

(defbench bench-clojure-core-delay 100
  (dotimes [_ 100] (delay)))

(defbench bench-clojure-core-delay-p 100
  (dotimes [_ 100] (delay? nil)))

(defbench bench-clojure-core-force 100
  (dotimes [_ 100] (force nil)))

(defbench bench-clojure-core-if-not 100
  (dotimes [_ 100] (if-not nil nil)))

(defbench bench-clojure-core-identical-p 100
  (dotimes [_ 100] (identical? nil nil)))

(defbench bench-clojure-core--eq 100
  (dotimes [_ 100] (= nil)))

(defbench bench-clojure-core-not-eq 100
  (dotimes [_ 100] (not= nil)))

(defbench bench-clojure-core-compare 100
  (dotimes [_ 100] (compare nil nil)))

(defbench bench-clojure-core-and 100
  (dotimes [_ 100] (and)))

(defbench bench-clojure-core-or 100
  (dotimes [_ 100] (or)))

(defbench bench-clojure-core-zero-p 100
  (dotimes [_ 100] (zero? 0)))

(defbench bench-clojure-core-count 100
  (dotimes [_ 100] (count nil)))

(defbench bench-clojure-core-int 100
  (dotimes [_ 100] (int nil)))

(defbench bench-clojure-core-nth 100
  (dotimes [_ 100] (nth nil 0)))

(defbench bench-clojure-core--lt 100
  (dotimes [_ 100] (< nil)))

(defbench bench-clojure-core-inc' 100
  (dotimes [_ 100] (inc' nil)))

(defbench bench-clojure-core-inc 100
  (dotimes [_ 100] (inc nil)))

(defbench bench-clojure-core-reverse 100
  (dotimes [_ 100] (reverse nil)))

(defbench bench-clojure-core--plus' 100
  (dotimes [_ 100] (+')))

(defbench bench-clojure-core--plus 100
  (dotimes [_ 100] (+)))

(defbench bench-clojure-core--star' 100
  (dotimes [_ 100] (*')))

(defbench bench-clojure-core--star 100
  (dotimes [_ 100] (*)))

(defbench bench-clojure-core-- 100
  (dotimes [_ 100] (/ nil)))

(defbench bench-clojure-core--' 100
  (dotimes [_ 100] (-' nil)))

(defbench bench-clojure-core-- 100
  (dotimes [_ 100] (- nil)))

(defbench bench-clojure-core--lt-eq 100
  (dotimes [_ 100] (<= nil)))

(defbench bench-clojure-core--gt 100
  (dotimes [_ 100] (> nil)))

(defbench bench-clojure-core--gt-eq 100
  (dotimes [_ 100] (>= nil)))

(defbench bench-clojure-core--eq-eq 100
  (dotimes [_ 100] (== nil)))

(defbench bench-clojure-core-max 100
  (dotimes [_ 100] (max nil)))


;; ============================================================
;; Run suite
;; ============================================================

(defn run-parity-suite []
  (reset! *results* [])
  (run-tests)
  (println "\n=== PARITY RESULTS ===")
  (println "Total tests:" (count @*results*))
  (println "Passed:" (count (filter #(nil? (:error %)) @*results*)))
  (println "Errors:" (count (filter :error @*results*)))
  (println)
  (println "=== ERRORS ===")
  (doseq [r (filter :error @*results*)]
    (println (:fn r) "-" (:error r)))
  (spit "parity_results.edn" (export-results))
  (println "\nResults saved to parity_results.edn"))

(run-parity-suite)

