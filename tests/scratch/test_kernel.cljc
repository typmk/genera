;; Test that kernel types work
(ns test-kernel
  (:require [clojure.lang.kernel :as k]))

;; Test Cons
(def c (k/->Cons 1 (k/->Cons 2 nil nil) nil))
(println "Cons first:" (first c))
(println "Cons rest:" (first (rest c)))

;; Test symbol
(def s (k/symbol "foo"))
(println "Symbol:" s)

;; Test keyword  
(def kw (k/keyword "bar"))
(println "Keyword:" kw)

;; Test atom
(def a (k/atom 42))
(println "Atom value:" @a)
(reset! a 100)
(println "After reset!:" @a)

(println "All kernel tests passed!")
