;; Test keyword/symbol inlines

(def k (keyword "foo"))
(def s (symbol "bar"))
(def n (name k))
(println "keyword:" k)
(println "symbol:" s)
(println "name:" n)
