(ns test.square-brackets
  "Test file for square bracket [] diagnostics.
  Contains intentional errors ONLY in square brackets.
  Parentheses () and curly braces {} remain balanced."
  (:require [clojure.string :as str]))

;; ============================================================================
;; BASELINE: Correct syntax (for reference)
;; ============================================================================

(defn baseline-correct [x y]
  "This function has correct square bracket usage."
  (let [sum (+ x y)
        product (* x y)]
    [sum product]))

;; ============================================================================
;; ERROR TYPE 1: Missing CLOSING square brackets ]
;; ============================================================================

;; Error #1: Missing ] in function parameters
(defn missing-close-params [a b c
  "Missing closing ] in parameter vector"
  (+ a b c))

;; Error #2: Missing ] in let binding
(defn missing-close-let [x]
  (let [doubled (* x 2
        tripled (* x 3)]
    [doubled tripled]))

;; Error #3: Missing ] in vector literal
(def missing-close-vector
  [1 2 3 4 5)

;; Error #4: Missing ] in nested vector
(def nested-missing
  [[1 2 3]
   [4 5 6
   [7 8 9]])

;; Error #5: Missing ] in destructuring
(defn missing-close-destruct [{:keys [name age address}]
  "Missing ] after :keys vector"
  (str name " is " age " years old"))

;; ============================================================================
;; ERROR TYPE 2: Missing OPENING square brackets [
;; ============================================================================

;; Error #6: Missing [ in function parameters
(defn missing-open-params x y z]
  "Missing opening [ before parameters"
  (+ x y z))

;; Error #7: Missing [ in let binding
(defn missing-open-let []
  (let x 10
        y 20]
    (+ x y)))

;; Error #8: Missing [ in vector literal
(def missing-open-vector
  1 2 3 4 5])

;; Error #9: Missing [ in nested structure
(def nested-missing-open
  ([1 2 3]
   4 5 6]
   [7 8 9]))

;; ============================================================================
;; ERROR TYPE 3: Extra/misplaced square brackets
;; ============================================================================

;; Error #10: Extra closing ]
(defn extra-close [a b]
  (let [sum (+ a b)]
    sum]])

;; Error #11: Extra opening [
(defn extra-open [x]
  (let [[y (* x 2)]
    y))

;; ============================================================================
;; CONTROL: Correct nested square brackets (no errors)
;; ============================================================================

(defn control-correct-nested [items]
  "This function should show ZERO errors - control group"
  (let [filtered (filter even? items)
        mapped (map inc filtered)
        reduced (reduce + mapped)]
    [filtered mapped reduced]))

;; ============================================================================
;; ERROR TYPE 4: Square brackets in complex nesting
;; ============================================================================

;; Error #12: Missing ] in nested let
(defn complex-nesting [data]
  (let [first-pass (map inc data)
        second-pass (let [doubled (map #(* % 2) first-pass
                          tripled (map #(* % 3) first-pass)]
                      [doubled tripled])]
    second-pass))

;; Error #13: Missing [ in for comprehension
(defn missing-in-for [n]
  (for i (range n)
        j (range i)]
    [i j]))

;; ============================================================================
;; STATISTICS
;; ============================================================================

;; Square bracket counts in this file:
;; - Intentionally MISSING closing ]: 8 locations
;; - Intentionally MISSING opening [: 4 locations
;; - Extra brackets: 2 locations
;; - Total errors: 14 square bracket errors
;;
;; All () parentheses: BALANCED (no errors)
;; All {} curly braces: BALANCED (no errors)
;; Only [] square brackets have errors
