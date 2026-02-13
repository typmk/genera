(println "Testing Def and Loop")
(def x 10)
(println x)

(println "Loop test:")
(println 
  (loop [i 0 acc ""]
    (if (< i 5)
      (recur (+ i 1) (str acc "*"))
      acc)))
