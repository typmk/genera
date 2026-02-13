(require '[clojure.java.io :as io])
(let [r (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. "(foo bar)"))
      f (read r)]
  (println "Meta:" (meta f)))
