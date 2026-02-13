(ns clojure.php.emit-test
  (:require [clojure.test :refer :all]
            [clojure.php.emit :refer [emit-str munge]]))

(deftest test-munge
  (is (= "foo" (munge "foo")))
  (is (= "foo_bar" (munge "foo-bar")))
  (is (= "foo_BAR" (munge "foo|BAR")))
  (is (= "foo_QMARK_" (munge "foo?")))
  (is (= "_BANG_foo" (munge "!foo"))))

(deftest test-emit-values
  (is (= "null" (emit-str {:op :const :val nil})))
  (is (= "true" (emit-str {:op :const :val true})))
  (is (= "\"hello\"" (emit-str {:op :const :val "hello"}))))

(deftest test-emit-locals
  (is (= "$foo" (emit-str {:op :local :name "foo"})))
  (is (= "$foo_bar" (emit-str {:op :local :name "foo-bar"}))))

(deftest test-emit-vars
  ;; Global var assumption for now
  (is (= "$clojure_core_map" (emit-str {:op :var :name "map" :ns "clojure.core"})))
  (is (= "$my_var" (emit-str {:op :var :name "my-var"}))))

(deftest test-emit-if
  (let [node {:op :if
              :test {:op :const :val true}
              :then {:op :const :val 1}
              :else {:op :const :val 2}}]
    ;; Default (Expression context) -> Ternary
    (is (= "(true ? 1 : 2)" (emit-str node)))

    ;; Statement context -> if/else block
    (let [stmt-node (assoc-in node [:env :context] :ctx/statement)]
      (is (= "if (true) {\n1;\n} else {\n2;\n}\n" (emit-str stmt-node))))))

(deftest test-emit-do
  (let [node {:op :do
              :statements [{:op :const :val "side-effect"}]
              :ret {:op :const :val "result"}}]
    (is (= "\"side-effect\";\n\"result\"" (emit-str node)))))
