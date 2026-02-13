(ns tests.arithmetic
  (:require [clojure.test :refer :all]
            [clj-php.compiler :as compiler]))

(defmacro with-compiler [& body]
  `(binding [compiler/*writer* (java.io.StringWriter.)]
     ~@body))

(deftest test-arithmetic-compile
  (with-compiler
    ;; 1. Variadic Addition
    (let [php (compiler/compile-form '(+ 1 2 3))]
       (is (.contains php "reduce") "Variadic + should use reduce or specialized emit"))
    
    ;; 2. Unary Minus
    (let [php (compiler/compile-form '(- 10))]
       (is (.contains php "- 10") "Unary minus"))))

;; Runtime Tests (Compiled then run)
;; We will trust test_functional.cljc for runtime verification, 
;; this file is mostly for "Does it compile?" check manually.
