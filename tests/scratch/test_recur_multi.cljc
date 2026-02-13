;; Test multi-arity recur
(defn <
  ([_] true)
  ([x y] (php/< x y))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(println (< 1))       ; true
(println (< 1 2))     ; true
(println (< 1 2 3))   ; true
(println (< 3 2 1))   ; false
