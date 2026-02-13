(ns hello-clojure.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [hello-clojure.core :as core]))

(deftest greet-test
  (testing "greet function returns proper greeting"
    (is (= "Hello, Alice! Welcome to Clojure development."
           (core/greet "Alice")))
    (is (= "Hello, Bob! Welcome to Clojure development."
           (core/greet "Bob")))))

(deftest add-test
  (testing "add function adds two numbers"
    (is (= 5 (core/add 2 3)))
    (is (= 0 (core/add -1 1)))
    (is (= 100 (core/add 50 50)))))

(deftest factorial-test
  (testing "factorial function computes correctly"
    (is (= 1 (core/factorial 0)))
    (is (= 1 (core/factorial 1)))
    (is (= 120 (core/factorial 5)))
    (is (= 5040 (core/factorial 7)))))
