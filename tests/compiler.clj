(ns tests.compiler
  (:require [clojure.test :refer :all]
            [clj-php.compiler :as compiler]))

(defn compile-str [form]
  ;; Helper to capture compiler output
  (binding [compiler/*writer* (java.io.StringWriter.)
            compiler/*php-line* (atom 0)
            compiler/*source-map* (atom [])]
    (compiler/emit {:top-level true :statement true} form)
    (str compiler/*writer*)))

(deftest test-basics
  (is (= "1" (compile-str 1)))
  (is (= "'hello'" (compile-str "hello"))))

(deftest test-statements
  ;; Top-level If -> Statement
  (let [code (compile-str '(if true 1 0))]
    (is (= "if (true) { 1; } else { 0; }" code))))

(deftest test-expressions
  ;; Nested If -> Ternary
  (binding [compiler/*writer* (java.io.StringWriter.)]
    (compiler/emit {:top-level false} '(if true 1 0))
    (let [res (str compiler/*writer*)]
      (is (= "(true ? 1 : 0)" res)))))

(run-tests)
