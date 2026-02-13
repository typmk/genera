;; More complex test
(def xs [1 2 3 4 5])
(def doubled (map (fn [x] (* x 2)) xs))
(println "xs =" xs)
(println "doubled =" (vec doubled))

(def m {:a 1 :b 2 :c 3})
(println "map =" m)
(println "keys =" (keys m))

(def sum (reduce + 0 xs))
(println "sum =" sum)
