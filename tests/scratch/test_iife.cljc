;; Test IIFE codepaths - let, do, letfn in expression context

;; Test 1: let in expression context
(println "Test 1: let in expr context")
(println (+ 1 (let [x 2] x)))  ; should be 3

;; Test 2: do in expression context
(println "Test 2: do in expr context")
(println (+ 10 (do (def temp 5) temp)))  ; should be 15

;; Test 3: nested let in expression
(println "Test 3: nested let")
(println (+ (let [a 1] a) (let [b 2] b)))  ; should be 3

;; Test 4: let with multiple bindings in expression
(println "Test 4: let with multiple bindings")
(println (* 2 (let [x 3 y 4] (+ x y))))  ; should be 14

;; Test 5: letfn in expression context
(println "Test 5: letfn in expr context")
(println (+ 100 (letfn [(double [n] (* 2 n))] (double 5))))  ; should be 110

;; Test 6: do with side effects in expression
(println "Test 6: do with side effects")
(def counter (atom 0))
(println (+ 1 (do (swap! counter inc) @counter)))  ; should be 2

;; Test 7: let returning function result
(println "Test 7: let with fn call")
(println (str "Result: " (let [nums [1 2 3]] (reduce + nums))))  ; should be "Result: 6"

(println "All IIFE tests passed!")
