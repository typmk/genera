;; Test Stage 2 functions

;; Test type predicates
(php/echo "Testing string?: ")
(php/echo (if (string? "hello") "pass" "fail"))
(php/echo "\n")

(php/echo "Testing int?: ")
(php/echo (if (int? 42) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing number?: ")
(php/echo (if (number? 3.14) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing nil?: ")
(php/echo (if (nil? nil) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing vector?: ")
(php/echo (if (vector? [1 2 3]) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing empty?: ")
(php/echo (if (empty? []) "pass" "fail"))
(php/echo "\n")

;; Test keys/vals
(php/echo "Testing keys: ")
(let [m {:a 1 :b 2}]
  (php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (keys m))))
(php/echo "\n")

(php/echo "Testing vals: ")
(let [m {:a 1 :b 2}]
  (php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (vals m))))
(php/echo "\n")

;; Test take/drop
(php/echo "Testing take: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (take 3 [1 2 3 4 5])))
(php/echo "\n")

(php/echo "Testing drop: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (drop 2 [1 2 3 4 5])))
(php/echo "\n")

;; Test range
(php/echo "Testing range: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (range 5)))
(php/echo "\n")

;; Test every?/some
(php/echo "Testing every?: ")
(php/echo (if (every? (fn [x] (php/> x 0)) [1 2 3]) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing some: ")
(php/echo (if (some (fn [x] (php/> x 5)) [1 2 3 6]) "pass" "fail"))
(php/echo "\n")

;; Test concat
(php/echo "Testing concat: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (concat [1 2] [3 4])))
(php/echo "\n")

;; Test reverse
(php/echo "Testing reverse: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (reverse [1 2 3])))
(php/echo "\n")

;; Test nth
(php/echo "Testing nth: ")
(php/echo (nth [10 20 30] 1))
(php/echo "\n")

;; Test repeat
(php/echo "Testing repeat: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (repeat 3 "x")))
(php/echo "\n")

;; Test into
(php/echo "Testing into: ")
(php/echo (php/call_user_func "\\CljPhp\\Runtime::str" (into [1 2] [3 4])))
(php/echo "\n")

;; Test constantly
(php/echo "Testing constantly: ")
(let [f (constantly 42)]
  (php/echo (f 1 2 3)))
(php/echo "\n")

;; Test complement
(php/echo "Testing complement: ")
(let [not-nil? (complement nil?)]
  (php/echo (if (not-nil? 42) "pass" "fail")))
(php/echo "\n")
