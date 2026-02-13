(ns tests.varargs
  (:require [clojure.test :refer :all]
            [clj-php.compiler :as compiler]))

(defmacro with-compiler [& body]
  `(binding [compiler/*writer* (java.io.StringWriter.)]
     ~@body))

(deftest test-varargs
  (with-compiler
    ;; 1. Basic Varargs
    ;; (fn [& x] x) -> function(...$x) { return \CljPhp\Runtime::vector($x); }
    (let [form '(fn [& x] x)
          php (compiler/compile-form form)]
      (is (.contains php "...$x") "Should use PHP variadic syntax")
      (is (.contains php "return $x") "Should return the array (or vector conversion)"))
    
    ;; 2. Mixed Arguments
    ;; (fn [a & b] [a b]) -> function($a, ...$b) ...
    (let [form '(fn [a & b] [a b])
          php (compiler/compile-form form)]
      (is (.contains php "function($a, ...$b)") "Should handle mixed positional and varargs"))))

(run-tests)
