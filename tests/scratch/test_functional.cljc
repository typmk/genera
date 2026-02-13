(ns test-functional
  (:require [clojure.test :refer [deftest is run-tests]]
            [clj-php.core :refer [cond -> ->>]]))

;; 1. Global Def
(def x 10)

(deftest test-globals
  (is (= x 10) "Global def should work"))

;; 2. Control Flow
(deftest test-control-flow
  (is (= 1 (if true 1 2)) "If true")
  (is (= 2 (if false 1 2)) "If false")
  (is (= :a (cond true :a :else :b)) "Cond")
  (is (= 6 (do 1 2 3 (if true 6 7))) "Do block"))

;; 3. Functional
(deftest test-functional
  (is (= 2 (inc 1)) "inc")
  (is (= 0 (dec 1)) "dec")
  (is (= 1 (identity 1)) "identity")
  (is (= 6 (apply + [1 2 3])) "apply +")
  (is (= 3 ((comp inc inc) 1)) "comp")
  (is (= 6 ((partial + 5) 1)) "partial")
  (is (nil? nil) "nil? nil")
  (is (not (nil? 1)) "nil? 1"))

;; 4. Sequences
(deftest test-sequences
  (is (= 1 (first [1 2])) "first")
  (is (= 2 (first (rest [1 2]))) "rest")
  (is (= 3 (count [1 2 3])) "count")
  (is (seq [1]) "seq true")
  (is (nil? (seq [])) "seq false")
  (is (= 6 (reduce + 0 [1 2 3])) "reduce"))

;; 5. Arithmetic
(deftest test-arithmetic
  (is (= 3 (+ 1 2)) "+ infix")
  (is (= 10 (+ 1 2 3 4)) "+ varargs")
  (is (= -10 (- 10)) "- unary")
  (is (= 8 (- 10 2)) "- binary")
  (is (= 24 (* 2 3 4)) "* varargs"))

(run-tests)
