(ns test-core-new
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "=== Compiling clojure.core-new ===\n")

(let [result (m/compile-files ["test_core_new_partial.cljc"]
                               {:runtime :none :aot? true})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (count (:errors result)) "errors")
  (when (seq (:errors result))
    (println "\nFirst 10 errors:")
    (doseq [e (take 10 (:errors result))]
      (println "  - Error:" (:msg e))
      (println "    Form:" (:form e))))
  (when (empty? (:errors result))
    (spit "out/core_new_aot.php" (:php result))
    (println "Written to out/core_new_aot.php")
    (println "\nFirst 2000 chars:")
    (println (subs (:php result) 0 (min 2000 (count (:php result)))))))
