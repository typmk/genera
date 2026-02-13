(ns tests.macros
  (:require [clojure.test :refer :all]
            [clj-php.compiler :as compiler]))

(defmacro with-compiler [& body]
  `(binding [compiler/*writer* (java.io.StringWriter.)]
     ~@body))

(deftest test-defmacro
  (with-compiler
    ;; 1. Register a macro
    (compiler/emit-defmacro {} '(defmacro foo [x] `(list 'quote ~x)))
    
    ;; 2. Verify registration
    (is (fn? (get @compiler/*macros* 'foo)) "Macro should be registered")))

(deftest test-macroexpand
  (with-compiler
    ;; 1. Register 'when' macro manually
    (compiler/emit-defmacro {} '(defmacro when [test & body] (list 'if test (cons 'do body))))
    
    ;; 2. Verify expansion
    (let [form '(when true (println 1))
          expected '(if true (do (println 1)))]
      (is (= expected (compiler/expand-form form)) "Macro should expand to if-do"))))

(run-tests)
