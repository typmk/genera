;; Test cross-namespace references
(ns test-cross-ns
  "Test cross-namespace variable references.")

;; Define a var in this namespace
(def my-val 42)

;; Reference from clojure.core (simulated)
(def core-ref clojure.core/first)

;; Reference from another namespace
(def other-ref other.ns/bar)

;; Local reference (same namespace)
(def local-ref my-val)

;; Function using cross-ns refs
(defn use-refs []
  (println my-val core-ref other-ref))
