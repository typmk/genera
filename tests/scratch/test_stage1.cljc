;; Test Stage 1 functions

;; Test map
(php/echo "Testing map: ")
(let [result (map inc [1 2 3])]
  (php/echo (php/call_user_func "\\CljPhp\\Runtime::str" result)))
(php/echo "\n")

;; Test filter
(php/echo "Testing filter: ")
(let [result (filter (fn [x] (php/> x 2)) [1 2 3 4 5])]
  (php/echo (php/call_user_func "\\CljPhp\\Runtime::str" result)))
(php/echo "\n")

;; Test last
(php/echo "Testing last: ")
(php/echo (last [1 2 3 4 5]))
(php/echo "\n")

;; Test and/or
(php/echo "Testing and: ")
(php/echo (if (and true true) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing and short-circuit: ")
(php/echo (if (and false (php/throw (new Exception "Should not reach"))) "fail" "pass"))
(php/echo "\n")

(php/echo "Testing or: ")
(php/echo (if (or false true) "pass" "fail"))
(php/echo "\n")

;; Test if-let
(php/echo "Testing if-let truthy: ")
(php/echo (if-let [x 42] x "nope"))
(php/echo "\n")

(php/echo "Testing if-let falsy: ")
(php/echo (if-let [x nil] x "nope"))
(php/echo "\n")

;; Test when-let
(php/echo "Testing when-let: ")
(when-let [x 42]
  (php/echo x)
  (php/echo " worked"))
(php/echo "\n")

;; Test equality
(php/echo "Testing =: ")
(php/echo (if (= 1 1) "pass" "fail"))
(php/echo "\n")

(php/echo "Testing not=: ")
(php/echo (if (not= 1 2) "pass" "fail"))
(php/echo "\n")
