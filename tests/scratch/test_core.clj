(ns test-core
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "=== Compiling clojure.core ===\n")

(let [result (m/compile-files ["src/clj/clojure/core.cljc"]
                               {:runtime :none :aot? true})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (count (:errors result)) "errors")
  (when (seq (:errors result))
    (println "\nFirst 5 errors:")
    (doseq [e (take 5 (:errors result))]
      (println "  -" (:msg e) "in" (:file e))))
  (when (empty? (:errors result))
    (spit "out/core_aot.php" (:php result))
    (println "Written to out/core_aot.php")
    (println "\nFirst 1000 chars:")
    (println (subs (:php result) 0 (min 1000 (count (:php result)))))))
