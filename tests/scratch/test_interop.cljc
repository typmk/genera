(ns test-interop
  (:require [clojure.test :refer [deftest is run-tests]]))

(deftest test-native-functions
  (is (= 3 (php/strlen "foo")) "strlen")
  (is (= "FOO" (php/strtoupper "foo")) "strtoupper")
  (is (= "a,b,c" (php/implode "," ["a" "b" "c"])) "implode vector")
  (is (= 3 (count (php/explode "," "a,b,c"))) "explode to array"))

(deftest test-classes
  (let [d (php/new "DateTime" "2023-01-01")]
    (is (php/is_object d) "is_object")
    (is (= "2023" (.format d "Y")) "DateTime->format")))

(deftest test-types
  (is (php/is_string "foo") "is_string")
  (is (php/is_int 123) "is_int")
  (is (php/is_bool true) "is_bool")
  (is (php/is_null nil) "is_null"))

(run-tests)
