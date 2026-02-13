;; Test infinite sequences
(defn naturals [n]
  (lazy-seq (cons n (naturals (inc n)))))

(println "Take 5:" (take 5 (naturals 0)))
(println "Drop 10 take 5:" (take 5 (drop 10 (naturals 0))))
(println "Map inc take 5:" (take 5 (map inc (naturals 0))))
(println "Filter even take 5:" (take 5 (filter even? (naturals 0))))

;; Test composition - filter odd from naturals, then double
(println "Composed:" (take 3 (map #(* % 2) (filter odd? (naturals 0)))))

;; Test with finite data (should still work)
(println "Range:" (take 5 (range 10)))
