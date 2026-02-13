;; Minimal test - multi-arity recur with PHP-only primitives
(defn test-lt
  ([_] true)
  ([x y] (php/< x y))
  ([x y z]
   (if (php/< x y)
     (recur y z z)  ; recur with same arity
     false)))

(php/echo "Testing recur: ")
(php/echo (test-lt 1))
(php/echo " ")
(php/echo (test-lt 1 2))
(php/echo " ")
(php/echo (test-lt 1 2 3))
(php/echo " ")
(php/echo (test-lt 3 2 1))
(php/echo "\n")
