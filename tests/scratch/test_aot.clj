(ns test-aot
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "=== Testing AOT Mode ===\n")

;; Test AOT compilation of kernel.cljc
(println "Compiling kernel.cljc in AOT mode")
(let [result (m/compile-files ["src/clj/clojure/types/kernel.cljc"]
                               {:runtime :none :aot? true})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (:errors result))
  (println "\nFirst 800 chars:")
  (println (subs (:php result) 0 (min 800 (count (:php result))))))
