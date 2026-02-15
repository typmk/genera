;; test/emit.clj — C emit tests in Clojure
;;
;; Uses cc-emit builtin to verify C emitter produces non-empty output.

(describe! "clj c emit tests")

(defn cc-test [name src]
  (let [output (cc-emit src)]
    (it! (str "cc:" name) (> (count output) 0))))

(cc-test "add" "(println (+ 1 2))")
(cc-test "mul" "(println (* 6 7))")
(cc-test "sub" "(println (- 10 3))")
(cc-test "nested" "(println (+ (* 2 3) (- 10 4)))")
(cc-test "if-t" "(println (if 1 10 20))")
(cc-test "let" "(println (let [a 1 b 2] (+ a b)))")
(cc-test "do" "(println (do 1 2 42))")
(cc-test "def" "(def x 42) (println x)")
(cc-test "defn" "(defn double [x] (* x 2)) (println (double 21))")
(cc-test "fact-rec" "(defn fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))\n(println (fact 10))")
(cc-test "tco-fact" "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n(println (fact 10 1))")
(cc-test "loop" "(println (loop [i 0 s 0] (if (>= i 10) s (recur (+ i 1) (+ s i)))))")
(cc-test "cond" "(println (cond (< 5 3) 0 (> 5 3) 42))")
(cc-test "when-t" "(println (when 1 42))")
(cc-test "str" "(println \"hello\")")
