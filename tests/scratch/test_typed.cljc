(ns test-typed
  "Test typed output for Psalm/PHPStan.")

;; Vector with typed elements
(def numbers [1 2 3 4 5])

;; Map with typed keys/values
(def user {:name "Alice" :age 30})

;; Set
(def tags #{"php" "clojure"})

;; Function with type hints
(defn add-numbers [^int a ^int b]
  (php/+ a b))

;; Function returning a vector
(defn get-numbers []
  numbers)

(println "numbers:" numbers)
(println "user:" user)
(println "add:" (add-numbers 1 2))
