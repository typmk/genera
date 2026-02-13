(ns tests.core
  (:require [clojure.test :refer :all]
            [clj-php.compiler :as compiler]))

(defmacro with-compiler [& body]
  `(binding [compiler/*writer* (java.io.StringWriter.)]
     ~@body))

(deftest test-functional
  (with-compiler
    ;; 1. Apply
    (let [form '(apply inc [1])
          php (compiler/compile-form form)]
      (is (.contains php "call_user_func_array") "apply should compile to call_user_func_array"))
    
    (let [form '(apply identity 1 [2])
          php (compiler/compile-form form)]
      (is (.contains php "array_merge") "apply with extra args should use array_merge"))

    ;; 2. Comp
    ;; Note: We test compilation, not runtime execution here.
    ;; Ideally strict string match or just structure check.
    (let [form '((comp inc inc) 1)
          php (compiler/compile-form form)]
      (is (.contains php "comp") "should emit call to comp"))

    ;; 3. Partial
    (let [form '((partial inc) 1)
          php (compiler/compile-form form)]
      (is (.contains php "partial") "should emit call to partial"))

    ;; 4. Nil?
    (let [form '(nil? nil)
          php (compiler/compile-form form)]
      (is (.contains php "is_null") "nil? should use is_null"))))

(run-tests)
