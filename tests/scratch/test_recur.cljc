;; Test multi-arity fn with recur

(defn test-lt
  "Test less-than with recur in 3-arity"
  ([_] true)
  ([x y] (php/< x y))
  ([x y z]
   (if (php/< x y)
     (recur y z z)
     false)))

;; Test it
(println "Testing recur:" (test-lt 1) (test-lt 1 2) (test-lt 1 2 3))
