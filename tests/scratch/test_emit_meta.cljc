(ns test-emit-meta
  "Test metadata-driven emission.")

;; Inline function - expands at call site
(defn ^:inline nil? [x]
  (php/=== x nil))

;; PHP function - emitted as proper PHP function
(defn ^:php-fn add [a b]
  (php/+ a b))

;; Test calls
(println "nil? nil:" (nil? nil))
(println "nil? 1:" (nil? 1))
(println "add 1 2:" (add 1 2))
