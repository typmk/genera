;; test/passes.clj — Pass + index comparison tests
;;
;; C analyze → snapshot → clear → Clojure analyze → compare.
;; Verifies Clojure passes produce identical views to C passes.
;;
;; C gram_index → snapshot → clear → Clojure gram_index → compare.
;; Verifies Clojure indexer produces identical structural bitmasks.

(describe! "clj passes (clj)")

(test-compare "(+ 1 2)")
(test-compare "(defn f [x] (+ x 1))")
(test-compare "(let [a 1 b 2] (+ a b))")
(test-compare "(if true 42 99)")
(test-compare "(do (def x 10) (+ x 1))")
(test-compare "(fn [a b] (* a b))")
(test-compare "(loop [i 0] (if (>= i 10) i (recur (inc i))))")
(test-compare "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
(test-compare "(when false 42)")
(test-compare "(+ (* 2 3) (- 10 4))")
(test-compare "(cond (< 5 3) 0 (> 5 3) 1)")

(describe! "clj gram-index (clj)")

(test-index-compare "42")
(test-index-compare "(+ 1 2)")
(test-index-compare "(defn f [x] (+ x 1))")
(test-index-compare "(let [a 1 b 2] (+ a b))")
(test-index-compare "(if true 42 99)")
(test-index-compare "(do (def x 10) (+ x 1))")
(test-index-compare "(fn [a b] (* a b))")
(test-index-compare "(loop [i 0] (if (>= i 10) i (recur (inc i))))")
(test-index-compare "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
(test-index-compare "(when false 42)")
(test-index-compare "(+ (* 2 3) (- 10 4))")
(test-index-compare "(cond (< 5 3) 0 (> 5 3) 1)")
(test-index-compare "[1 2 3]")
(test-index-compare "{:a 1 :b 2}")
(test-index-compare "(def x \"hello\")")
(test-index-compare "'(1 2 3)")

(describe! "clj eval-node (clj)")

(test-eval-compare "42")
(test-eval-compare "(+ 1 2)")
(test-eval-compare "(+ 1 2 3)")
(test-eval-compare "(* 2 3)")
(test-eval-compare "(if true 42 99)")
(test-eval-compare "(if false 42 99)")
(test-eval-compare "(let [x 10] x)")
(test-eval-compare "(let [x 10 y 20] (+ x y))")
(test-eval-compare "(do 1 2 3)")
(test-eval-compare "(+ (* 2 3) (- 10 4))")
(test-eval-compare "[1 2 3]")
(test-eval-compare "{:a 1 :b 2}")
(test-eval-compare ":foo")
(test-eval-compare "'(1 2 3)")
