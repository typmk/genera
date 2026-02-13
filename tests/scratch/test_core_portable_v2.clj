(ns test-core-portable-v2
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "=== Compiling clojure.core-portable-v2 ===\n")

(let [result (m/compile-files ["test_partial_v2.cljc"]
                               {:runtime :none :aot? true})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (count (:errors result)) "errors")
  (when (seq (:errors result))
    (println "\nFirst 15 errors:")
    (doseq [e (take 15 (:errors result))]
      (println "  - Error:" (:msg e))
      (println "    Form:" (:form e))))
  (when (empty? (:errors result))
    (spit "out/core_portable_v2_aot.php" (:php result))
    (println "Written to out/core_portable_v2_aot.php")
    (println "\nFirst 2000 chars:")
    (println (subs (:php result) 0 (min 2000 (count (:php result)))))))
