(println "Start")
(do
  (println "Step 1")
  (println "Step 2"))

(let [a 1
      b 2]
  (println (+ a b)))

(loop [x 0]
  (if (< x 3)
    (do (println x)
        (recur (+ x 1)))
    (println "Done")))
