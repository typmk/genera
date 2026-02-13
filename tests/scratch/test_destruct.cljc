;; Test vector destructuring
(let [[a b] [1 2]]
  (println "Vector:" a b))

;; Test with :as
(let [[a b :as all] [1 2 3]]
  (println "With :as:" a b all))

;; Test with & rest (using 'rest' as variable name - tests shadowing fix)
(let [[a b & rest] [1 2 3 4 5]]
  (println "With rest:" a b rest))

;; Test nested destructuring
(let [[a [b c]] [1 [2 3]]]
  (println "Nested:" a b c))

;; Test map destructuring with :keys
(let [{:keys [x y]} {:x 10 :y 20}]
  (println "Map :keys:" x y))

;; Test map with :or defaults
(let [{:keys [x y] :or {y 99}} {:x 10}]
  (println "Map with :or:" x y))

;; Test map with :as
(let [{:keys [x] :as m} {:x 10 :y 20}]
  (println "Map with :as:" x m))

;; Test fn param destructuring
(defn point-str [[x y]]
  (str "(" x ", " y ")"))

(println "Fn destruct:" (point-str [3 4]))

;; Test shadowing common names
(let [first 100
      second 200
      map {:a 1}]
  (println "Shadowing:" first second map))
