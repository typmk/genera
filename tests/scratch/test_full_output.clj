(ns test-full-output
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "Compiling kernel.cljc")
(let [result (m/compile-files ["src/clj/clojure/types/kernel.cljc"] {:runtime :none})]
  (println "Result PHP length:" (count (:php result)))
  (println "Errors:" (:errors result))
  (spit "out/kernel.php" (:php result))
  (println "Written to out/kernel.php"))
