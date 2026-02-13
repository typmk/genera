(ns clojure.compiler.emit-test
  (:require [clojure.test :refer :all]
            [clojure.compiler.emit :as emit :refer [emit]]))



(def php-syntax
  (merge emit/base-syntax
         {:var-prefix    "$"
          :null          "null"
          :map-entry     ["" " => " ""]
          :static-access ["::" ""]
          :access        ["->" ""]
          :fn            ["function" "(" ", " ") " "{ return " "; }"]
          :fn-multi      ["function" "(" ", " ") " "{\n" "\n}"]
          :use           ["use (" ", " ") " ""]
          :namespace     ["namespace " ";\n"]
          :ops           (assoc (:ops emit/base-syntax)
                                '= " === "
                                'not= " !== "
                                'div " / ")}))

(def js-syntax
  (merge emit/base-syntax
         {:null          "null"
          :undefined     "undefined"
          :map-entry     ["" ": " ""]
          :map           ["{" ", " "}"]
          :fn            ["" "(" ", " ") => " "" ""]      ; Arrow func
          :fn-multi      ["" "(" ", " ") => " "{\n" "\n}"]
          :const         ["const " " = " ""]
          :let           ["let " " = " ""]
          :ops           (assoc (:ops emit/base-syntax)
                                '= " === "
                                'not= " !== ")}))

(def go-syntax
  (merge emit/base-syntax
         {:null          "nil"
          :true          "true"
          :false         "false"
          :statement     "\n"
          :fn            ["func" "(" ", " ") " "{ return " " }"]
          :fn-multi      ["func" "(" ", " ") " "{\n" "\n}"]
          :map-entry     ["" ": " ""]
          :map           ["map[string]any{" ", " "}"]
          :array         ["[]any{" ", " "}"]
          :short-decl    [" := " ""]
          :if            ["if " " " " else "]
          :ternary       ["if " " " " else " ""]
          :ops           (assoc (:ops emit/base-syntax)
                                '= " == "
                                'not= " != "
                                'and " && "
                                'or " || ")}))

(def rust-syntax
  (merge emit/base-syntax
         {:null          "None"
          :true          "true"
          :false         "false"
          :fn            ["" "|" ", " "| " "" ""]
          :fn-multi      ["" "|" ", " "| " "{\n" "\n}"]
          :map-entry     ["" ": " ""]
          :array         ["vec![" ", " "]"]
          :map           ["HashMap::from([" ", " "])"]
          :let           ["let " " = " ""]
          :let-mut       ["let mut " " = " ""]
          :access        ["." ""]
          :if            ["if " " " " else "]
          :match         ["match " " {\n" "}\n"]}))

(deftest test-emit-const
  (is (= "null" (emit php-syntax {:op :const :val nil})))
  (is (= "true" (emit php-syntax {:op :const :val true})))
  (is (= "false" (emit php-syntax {:op :const :val false})))
  (is (= "\"foo\"" (emit php-syntax {:op :const :val "foo"})))
  (is (= "\"bar\"" (emit php-syntax {:op :const :val :bar})))
  (is (= "123" (emit php-syntax {:op :const :val 123}))))

(deftest test-emit-invoke
  (let [node {:op :invoke
              :f {:op :var :name "myFunc"}
              :args [{:op :const :val 1} {:op :const :val 2}]}]
    (is (= "$myFunc(1, 2)" (emit php-syntax node)))
    (is (= "myFunc(1, 2)" (emit js-syntax node)))))

(deftest test-emit-if
  (let [node {:op :if
              :test {:op :const :val true}
              :then {:op :const :val 1}
              :else {:op :const :val 0}}]
    ;; Expression context (ternary)
    (is (= "(true ? 1 : 0)" (emit php-syntax node)))
    (is (= "(true ? 1 : 0)" (emit js-syntax node)))

    ;; Go uses if/else even for expressions in this simplistic emitter, 
    ;; or rather Go doesn't have ternary, so checking if-stmt map
    (is (= "if true 1 else 0" (emit go-syntax node)))))

(deftest test-emit-array
  (let [node {:op :array
              :items [{:op :const :val 1} {:op :const :val 2}]}]
    (is (= "[1, 2]" (emit php-syntax node)))
    (is (= "[1, 2]" (emit js-syntax node)))
    (is (= "[]any{1, 2}" (emit go-syntax node)))
    (is (= "vec![1, 2]" (emit rust-syntax node)))))

(deftest test-emit-map
  (let [node {:op :map
              :entries [[{:op :const :val "a"} {:op :const :val 1}]
                        [{:op :const :val "b"} {:op :const :val 2}]]}]
    (is (= "[\"a\" => 1, \"b\" => 2]" (emit php-syntax node)))
    (is (= "{\"a\": 1, \"b\": 2}" (emit js-syntax node)))
    (is (= "map[string]any{\"a\": 1, \"b\": 2}" (emit go-syntax node)))))

(deftest test-emit-fn
  (let [node {:op :fn
              :params ["x" "y"]
              :body {:op :infix :operator '+ :args [{:op :local :name "x"} {:op :local :name "y"}]}}]
    ;; PHP: function($x, $y) { return ($x + $y); }
    (is (= "function($x, $y) { return ($x + $y); }" (emit php-syntax node)))
    ;; JS: (x, y) => (x + y)
    (is (= "(x, y) => (x + y)" (emit js-syntax node)))))
