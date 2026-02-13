
;; Build a map with keyword keys and do lookups
(def data {:name "Alice" :age 30 :city "NYC" :role "admin"})

(defn bench []
  (loop [i 0]
    (when (< i 10000)
      (:name data)
      (:age data)
      (:city data)
      (:role data)
      (recur (inc i)))))

(let [start (php/microtime true)]
  (bench)
  (let [elapsed (php/* 1000 (php/- (php/microtime true) start))]
    (println "10000 iterations x 4 lookups:" elapsed "ms")))

