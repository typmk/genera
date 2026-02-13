;; Test inline expansion

(ns test-inline
  "Test inline expansion with non-constant args.")

;; Test inline expansion with non-constant args
(defn check-nil [x]
  (if (nil? x)
    "is nil"
    "not nil"))

(defn test-identical [a b]
  (identical? a b))

(defn test-keyword [x]
  (keyword x))

;; Test type predicates
(defn check-type [x]
  {:nil? (nil? x)
   :some? (some? x)
   :string? (string? x)
   :number? (number? x)})

(println "check-nil nil:" (check-nil nil))
(println "check-nil 1:" (check-nil 1))
