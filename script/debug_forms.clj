(require '[clojure.java.io :as io])
(def reader (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. (slurp "src/clj_php/compiler.cljc"))))
(loop []
  (let [f (read {:eof :eof} reader)]
    (when (not= f :eof)
      (println "Form:" (class f) f)
      (recur))))
