(ns test.type-mismatches
  "Test file for bracket TYPE MISMATCH diagnostics.
  Contains errors where brackets are OPENED with one type but CLOSED with another.
  Examples: [ closed with ), { closed with ], ( closed with }, etc.

  This is the most challenging test - requires parser-level type tracking."
  (:require [clojure.string :as str]))

;; ============================================================================
;; BASELINE: Correct mixed bracket usage (for reference)
;; ============================================================================

(defn baseline-correct [items]
  "Correct usage of all three bracket types together."
  (let [{:keys [name age]} {:name "Alice" :age 30}
        data [1 2 3 4 5]
        unique #{:a :b :c}]
    {:person {:name name :age age}
     :numbers data
     :tags unique}))

;; ============================================================================
;; ERROR TYPE 1: Square bracket [ closed with wrong type
;; ============================================================================

;; Error #1: [ closed with )
(defn bracket-closed-with-paren [x y)
  "Function parameters: opened with [ but closed with )"
  (+ x y))

;; Error #2: [ closed with } in vector
(def vector-closed-with-brace
  [1 2 3 4 5})

;; Error #3: [ closed with ) in let binding
(defn let-binding-mismatch [n]
  (let [doubled (* n 2)
    doubled))

;; Error #4: [ closed with } in nested structure
(def nested-bracket-mismatch
  {:data [1 2 3}
   :more [4 5 6]})

;; ============================================================================
;; ERROR TYPE 2: Curly brace { closed with wrong type
;; ============================================================================

;; Error #5: { closed with ]
(def map-closed-with-bracket
  {:host "localhost"
   :port 8080])

;; Error #6: { closed with )
(def map-closed-with-paren
  {:name "Bob"
   :age 25))

;; Error #7: #{ closed with )
(def set-closed-with-paren
  #{:apple :banana :cherry))

;; Error #8: { closed with ] in destructuring
(defn destruct-mismatch [data]
  (let [{:keys [x y] data]
    (+ x y)))

;; ============================================================================
;; ERROR TYPE 3: Parenthesis ( closed with wrong type
;; ============================================================================

;; Error #9: ( closed with ]
(defn paren-closed-with-bracket []
  (let [x 10]
    (+ x 20]))

;; Error #10: ( closed with }
(defn paren-closed-with-brace [a b]
  (+ a b})

;; Error #11: ( closed with ] in nested expr
(def complex-paren-mismatch
  (map inc [1 2 3]))

;; Error #12: ( closed with } in function call
(defn func-call-mismatch [items]
  (filter even? items})

;; ============================================================================
;; ERROR TYPE 4: Multiple mismatches in same expression
;; ============================================================================

;; Error #13-14: Double mismatch - [ with ) and { with ]
(defn double-mismatch [x]
  (let [config {:host "localhost") :port 8080]
    config))

;; Error #15-16: Triple mismatch - all three types wrong
(def triple-mismatch
  {:data [1 2 3)
   :tags #{:a :b :c]
   :fn (fn [x} x)})

;; ============================================================================
;; ERROR TYPE 5: Nested mismatch propagation
;; ============================================================================

;; Error #17: Inner mismatch affects outer structure
(defn nested-propagation []
  (let [inner [1 2 3}
        outer {:data inner}]
    outer))

;; Error #18: Cascading mismatches
(def cascading-mismatches
  [(map (fn [x) (* x 2)) [1 2 3})
   {:result "error"}])

;; ============================================================================
;; ERROR TYPE 6: Reader macro mismatches
;; ============================================================================

;; Error #19: #{ closed with ]
(def reader-set-mismatch
  #{1 2 3 4 5])

;; Error #20: #() closed with ]
(def reader-fn-mismatch
  (map #(* % 2] [1 2 3]))

;; ============================================================================
;; CONTROL: Complex correct nesting (no errors)
;; ============================================================================

(defn control-complex-correct [data]
  "All bracket types used correctly in complex nesting"
  (let [{:keys [items tags]} data
        processed (map (fn [item]
                        {:value (* item 2)
                         :tags (into #{} tags)
                         :meta [(:id item) (:name item)]})
                      items)]
    {:results processed
     :count (count processed)
     :summary {:total (reduce + (map :value processed))}}))

;; ============================================================================
;; ERROR TYPE 7: Subtle mismatches (easy to miss visually)
;; ============================================================================

;; Error #21: Looks correct at first glance - ) instead of ]
(defn subtle-vector-error [items]
  (let [filtered (filter pos? items)]
    filtered))

;; Error #22: Map with extra depth - } instead of ]
(def subtle-map-error
  {:config {:nested {:deep [1 2 3}}
   :other "value"}})

;; Error #23: Function parameter destructuring wrong closer
(defn subtle-destruct [{:keys [a b c)]
  "Opened { but closed with )"
  (+ a b c))

;; ============================================================================
;; STATISTICS
;; ============================================================================

;; Bracket type mismatch counts:
;; - [ closed with ): 4 locations
;; - [ closed with }: 3 locations
;; - { closed with ]: 4 locations
;; - { closed with ): 3 locations
;; - ( closed with ]: 4 locations
;; - ( closed with }: 4 locations
;; - Reader macro mismatches: 2 locations
;; - Total mismatch errors: 24 type errors
;;
;; Note: Net bracket counts may be BALANCED (e.g., 10 opens, 10 closes)
;; but TYPES are wrong. This requires parser-level detection.
;;
;; Expected tool behavior:
;; - Stack counter: May show "balanced" (wrong!)
;; - Depth maps: May show odd depth patterns
;; - clj-kondo: Should detect ALL mismatches with exact line:col
;; - Parinfer: May detect via indentation inference
;; - claude-paren-diagnostics (enhanced): Should detect with type tracking
