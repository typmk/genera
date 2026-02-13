(ns tests.specs
  "Canonical emit specifications.

   These serve dual purpose:
   1. Unit tests for compiler output
   2. Documentation for LLM agents

   Each spec is {:in form, :ctx context, :out expected-php}

   See AGENTS.md for context flag documentation."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-php.emit.core :as emit]))

;; -----------------------------------------------------------------------------
;; Test Helper
;; -----------------------------------------------------------------------------

(defn compile-str
  "Compiles form with given context, returns PHP string."
  ([form] (compile-str {} form))
  ([ctx form]
   (binding [emit/*writer* (java.io.StringWriter.)
             emit/*php-line* (atom 0)
             emit/*source-map* (atom [])]
     (emit/emit (merge {:top-level false} ctx) form)
     (str emit/*writer*))))

;; -----------------------------------------------------------------------------
;; Specs: Literals
;; -----------------------------------------------------------------------------

(def literal-specs
  "Specifications for literal emission."
  [{:name "nil"
    :in nil
    :ctx {}
    :out "null"}

   {:name "true"
    :in true
    :ctx {}
    :out "true"}

   {:name "false"
    :in false
    :ctx {}
    :out "false"}

   {:name "integer"
    :in 42
    :ctx {}
    :out "42"}

   {:name "negative integer"
    :in -1
    :ctx {}
    :out "-1"}

   {:name "string"
    :in "hello"
    :ctx {}
    :out "'hello'"}

   {:name "string with quote"
    :in "it's"
    :ctx {}
    :out "'it\\'s'"}

   {:name "keyword"
    :in :foo
    :ctx {}
    :out "\\CljPhp\\Runtime::keyword('foo')"}

   {:name "symbol"
    :in 'x
    :ctx {}
    :out "$x"}

   {:name "symbol with dash"
    :in 'my-var
    :ctx {}
    :out "$my_var"}

   {:name "symbol with question mark"
    :in 'nil?
    :ctx {}
    :out "$nil_QMARK_"}

   {:name "empty vector"
    :in []
    :ctx {}
    :out "\\CljPhp\\Runtime::vector([])"}

   {:name "vector"
    :in [1 2 3]
    :ctx {}
    :out "\\CljPhp\\Runtime::vector([1, 2, 3])"}])

(deftest test-literals
  (doseq [{:keys [name in ctx out]} literal-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: If
;; -----------------------------------------------------------------------------

(def if-specs
  "Specifications for if emission."
  [{:name "if expression → ternary"
    :in '(if true 1 0)
    :ctx {}
    :out "(true ? 1 : 0)"}

   {:name "if statement → if block"
    :in '(if true 1 0)
    :ctx {:statement true}
    :out "if (true) { 1; } else { 0; }"}

   {:name "if return → if block with returns"
    :in '(if true 1 0)
    :ctx {:return true}
    :out "if (true) { return 1; } else { return 0; }"}

   {:name "if with expressions"
    :in '(if (< x 10) (+ x 1) x)
    :ctx {}
    :out "(($x < 10) ? ($x + 1) : $x)"}])

(deftest test-if
  (doseq [{:keys [name in ctx out]} if-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Let
;; -----------------------------------------------------------------------------

(def let-specs
  "Specifications for let emission."
  [{:name "let expression → IIFE"
    :in '(let [x 1] x)
    :ctx {}
    :out "(call_user_func(function() { $x = 1; return $x;}))"}

   {:name "let statement → inline"
    :in '(let [x 1] x)
    :ctx {:statement true}
    :out "$x = 1; $x;"}

   {:name "let multiple bindings"
    :in '(let [x 1 y 2] (+ x y))
    :ctx {:statement true}
    :out "$x = 1; $y = 2; ($x + $y);"}])

(deftest test-let
  (doseq [{:keys [name in ctx out]} let-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Do
;; -----------------------------------------------------------------------------

(def do-specs
  "Specifications for do emission."
  [{:name "do expression → IIFE"
    :in '(do (f) (g))
    :ctx {}
    :out "(call_user_func(function() { call_user_func($f); return call_user_func($g);}))"}

   {:name "do statement → inline"
    :in '(do (f) (g))
    :ctx {:statement true}
    :out "call_user_func($f); call_user_func($g);"}])

(deftest test-do
  (doseq [{:keys [name in ctx out]} do-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Fn
;; -----------------------------------------------------------------------------

(def fn-specs
  "Specifications for fn emission."
  [{:name "simple fn"
    :in '(fn [x] x)
    :ctx {}
    :out "(function($x) { return $x;})"}

   {:name "fn multiple args"
    :in '(fn [x y] (+ x y))
    :ctx {}
    :out "(function($x, $y) { return ($x + $y);})"}

   {:name "fn variadic"
    :in '(fn [x & more] x)
    :ctx {}
    :out "(function($x, ...$more) { return $x;})"}

   {:name "fn with type hints"
    :in '(fn [^int x ^int y] (+ x y))
    :ctx {}
    :out "(function(int $x, int $y) { return ($x + $y);})"}])

(deftest test-fn
  (doseq [{:keys [name in ctx out]} fn-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Interop
;; -----------------------------------------------------------------------------

(def interop-specs
  "Specifications for PHP interop emission."
  [{:name "php function call"
    :in '(php/strlen "hello")
    :ctx {}
    :out "strlen('hello')"}

   {:name "method call"
    :in '(.format date "Y-m-d")
    :ctx {}
    :out "$date->format('Y-m-d')"}

   {:name "constructor"
    :in '(new DateTime)
    :ctx {}
    :out "(new DateTime())"}

   {:name "constructor with args"
    :in '(new DateTime "2024-01-01")
    :ctx {}
    :out "(new DateTime('2024-01-01'))"}])

(deftest test-interop
  (doseq [{:keys [name in ctx out]} interop-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Arithmetic
;; -----------------------------------------------------------------------------

(def arithmetic-specs
  "Specifications for arithmetic emission."
  [{:name "addition"
    :in '(+ 1 2)
    :ctx {}
    :out "(1 + 2)"}

   {:name "addition multiple"
    :in '(+ 1 2 3)
    :ctx {}
    :out "(1 + 2 + 3)"}

   {:name "subtraction"
    :in '(- 5 3)
    :ctx {}
    :out "(5 - 3)"}

   {:name "multiplication"
    :in '(* 2 3)
    :ctx {}
    :out "(2 * 3)"}

   {:name "division"
    :in '(/ 10 2)
    :ctx {}
    :out "(10 / 2)"}

   {:name "comparison lt"
    :in '(< x 10)
    :ctx {}
    :out "($x < 10)"}

   {:name "equality"
    :in '(= x y)
    :ctx {}
    :out "($x === $y)"}])

(deftest test-arithmetic
  (doseq [{:keys [name in ctx out]} arithmetic-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Specs: Loop/Recur
;; -----------------------------------------------------------------------------

(def loop-specs
  "Specifications for loop/recur emission."
  [{:name "simple loop"
    :in '(loop [i 0] i)
    :ctx {}
    :out "(call_user_func(function() { $i = 0; while(true) { $i; break; } return $i; }))"}])

(deftest test-loop
  (doseq [{:keys [name in ctx out]} loop-specs]
    (testing name
      (is (= out (compile-str ctx in))))))

;; -----------------------------------------------------------------------------
;; Run All Specs
;; -----------------------------------------------------------------------------

(def all-specs
  "All specification categories for reference."
  {:literals literal-specs
   :if if-specs
   :let let-specs
   :do do-specs
   :fn fn-specs
   :interop interop-specs
   :arithmetic arithmetic-specs
   :loop loop-specs})

(defn run-all-specs
  "Runs all specs and returns results map."
  []
  (into {}
        (for [[category specs] all-specs]
          [category
           (for [{:keys [name in ctx out]} specs]
             {:name name
              :expected out
              :actual (compile-str ctx in)
              :pass? (= out (compile-str ctx in))})])))
