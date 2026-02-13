;; Test multi-arity fn with recur - verbose output

(defn test-lt
  "Test less-than with recur in 3-arity"
  ([_] true)
  ([x y] (php/< x y))
  ([x y z]
   (if (php/< x y)
     (recur y z z)
     false)))

;; Test it with explicit output
(println "1-arity:" (if (test-lt 1) "true" "false"))
(println "2-arity:" (if (test-lt 1 2) "true" "false"))
(println "3-arity:" (if (test-lt 1 2 3) "true" "false"))
