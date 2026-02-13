(ns test-aot-full
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
  (spit "out/kernel_aot.php" (:php result))
  (println "Written to out/kernel_aot.php"))

(println "\n=== Testing REPL Mode ===\n")

;; Test REPL compilation of kernel.cljc
(println "Compiling kernel.cljc in REPL mode")
(let [result (m/compile-files ["src/clj/clojure/types/kernel.cljc"]
                               {:runtime :none :aot? false})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (:errors result))
  (spit "out/kernel_repl.php" (:php result))
  (println "Written to out/kernel_repl.php"))
