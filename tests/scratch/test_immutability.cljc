(println "Testing Immutability")

(def v1 [1 2 3])
(println "v1 original:")
(println v1)

(def v2 (conj v1 4))
(println "v2 (conj v1 4):")
(println v2)

(println "v1 after conj (should be unchanged):")
(println v1)

(println "Testing Map & Keywords")
(def m {:a 100 :b 200})
(println m)

(println "Keyword Lookup (:a m):")
(println (:a m))
