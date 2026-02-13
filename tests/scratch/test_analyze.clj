(ns test-analyze
  (:require [clojure.php.main :as m]
            [clojure.jvm.host :as jvm-host]))

(jvm-host/init!)

(println "Compiling kernel.cljc only")
(let [result (m/compile-files ["src/clj/clojure/types/kernel.cljc"] {:runtime :none})]
  (println "Result PHP length:" (count (:php result)))
  (println "\nErrors:" (:errors result))
  (println "\nFirst 500 chars:")
  (println (subs (:php result) 0 (min 500 (count (:php result))))))
