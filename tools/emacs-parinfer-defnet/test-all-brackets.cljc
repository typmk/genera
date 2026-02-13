(ns test.all-brackets
  "Comprehensive test file combining ALL bracket error types.

  Tests:
  1. Missing closing brackets: ), ], }
  2. Missing opening brackets: (, [, {
  3. Type mismatches: [ with ), { with ], etc.
  4. Mixed errors in same function
  5. Complex nested scenarios

  This is the ultimate stress test for bracket diagnostics."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; ============================================================================
;; SECTION 1: Missing CLOSING brackets (all types)
;; ============================================================================

;; Error #1: Missing ) in function call
(defn missing-close-paren [x]
  (inc (+ x 10)

;; Error #2: Missing ] in vector
(def missing-close-bracket
  [:a :b :c :d)

;; Error #3: Missing } in map
(def missing-close-brace
  {:name "Alice"
   :age 30)

;; ============================================================================
;; SECTION 2: Missing OPENING brackets (all types)
;; ============================================================================

;; Error #4: Missing ( before function call
(defn missing-open-paren []
  + 1 2 3))

;; Error #5: Missing [ before parameters
(defn missing-open-bracket x y z]
  (* x y z))

;; Error #6: Missing { before map
(def missing-open-brace
  :host "localhost" :port 8080})

;; ============================================================================
;; SECTION 3: Type mismatches (critical - requires type tracking)
;; ============================================================================

;; Error #7: [ closed with )
(defn bracket-paren-mismatch [a b)
  (+ a b))

;; Error #8: { closed with ]
(def brace-bracket-mismatch
  {:x 1 :y 2])

;; Error #9: ( closed with }
(defn paren-brace-mismatch []
  (let [x 10}
    x))

;; ============================================================================
;; SECTION 4: Complex mixed errors
;; ============================================================================

;; Error #10-12: Triple error - missing ), type mismatch, missing }
(defn triple-error [config]
  (let [{:keys [host port] config
        url (str "http://" host ":" port)  ; Missing )
    {:url url :status :ok])  ; Type mismatch and missing }

;; Error #13-14: Nested mismatches
(def nested-errors
  {:data [(map inc [1 2 3}}  ; ] with } AND missing )
   :tags #{:a :b :c}})

;; ============================================================================
;; SECTION 5: Reader macros with errors
;; ============================================================================

;; Error #15: #{ closed with )
(def set-error
  #{1 2 3 4 5))

;; Error #16: #() closed with ]
(def anon-fn-error
  (map #(* % 2] [1 2 3]))

;; ============================================================================
;; SECTION 6: Cascading errors (early error affects later code)
;; ============================================================================

;; Error #17: Missing ] causes all subsequent code to misalign
(def cascade-start
  [1 2 3 4 5)  ; Missing ]

(defn cascade-affected []
  "This function is syntactically correct, but previous error cascades"
  (+ 1 2 3))

;; Error #18: Type mismatch that cascades
(defn cascade-source [{:keys [x y)}  ; } instead of ]
  (+ x y))

(defn cascade-victim [a b]
  "Also correct, but affected by previous mismatch"
  (* a b))

;; ============================================================================
;; SECTION 7: Destructuring errors (common real-world mistakes)
;; ============================================================================

;; Error #19: Map destructuring with wrong closer
(defn destruct-error-1 [{:keys [name age)}]
  "Opened { but closed with )"
  (str name " is " age))

;; Error #20: Vector destructuring missing close
(defn destruct-error-2 [[first second & rest]
  "Missing ] after rest"
  first)

;; Error #21: Nested destructuring mismatch
(defn destruct-error-3 [{:keys [person {:keys [name]}]
  "Inner destructuring missing ]"
  name)

;; ============================================================================
;; SECTION 8: Edge cases
;; ============================================================================

;; Error #22: Empty brackets with mismatch
(def empty-mismatch
  [})

;; Error #23: Single element with mismatch
(def single-mismatch
  [42))

;; Error #24: Deeply nested with multiple types wrong
(def deep-nest-errors
  {:level1 {:level2 {:level3 [1 2 3)}}}  ; ) instead of ]
               ; Missing multiple }

;; ============================================================================
;; CONTROL: Correct complex nesting (should show ZERO errors)
;; ============================================================================

(defn control-all-correct [data]
  "Complex function with ALL bracket types used correctly.
  Should show ZERO errors to validate diagnostic accuracy."
  (let [{:keys [items config]} data
        {:keys [host port]} config
        processed (map (fn [item]
                        {:id (:id item)
                         :value (* (:value item) 2)
                         :tags (into #{} (:tags item))})
                      items)
        summary {:total (count processed)
                 :values (mapv :value processed)
                 :config {:host host :port port}}]
    (if (empty? processed)
      nil
      {:results processed
       :summary summary
       :status :ok})))

;; ============================================================================
;; STATISTICS & EXPECTED BEHAVIOR
;; ============================================================================

;; Total errors in this file:
;; - Missing closing: 3 (one of each type)
;; - Missing opening: 3 (one of each type)
;; - Type mismatches: 15+ (various combinations)
;; - Cascading effects: 3+ functions affected
;; - Total distinct error locations: ~24
;;
;; Expected tool behavior:
;;
;; 1. STACK COUNTER (diagnose-parens.el)
;;    - May show NET imbalance (if types balanced, shows 0 - WRONG!)
;;    - Cannot detect type mismatches
;;    - Good for confirming "something is wrong"
;;
;; 2. DEPTH MAPS (detailed-diagnosis.el)
;;    - May detect SOME mismatches via depth anomalies
;;    - Cascading effects will confuse later errors
;;    - Heuristics may catch a few type errors
;;
;; 3. clj-kondo (GOLD STANDARD)
;;    - Should detect ALL type mismatches with exact line:column
;;    - "Mismatched bracket: found [ and closing )"
;;    - May stop at first syntax error (iterative fixing needed)
;;
;; 4. PARINFER
;;    - May infer some errors from indentation
;;    - Good at finding missing opening brackets
;;    - Type mismatches harder to detect via indentation
;;
;; 5. claude-paren-diagnostics.el (ENHANCED)
;;    - After enhancement: Should detect type mismatches
;;    - Stack-based tracking of bracket types
;;    - Report "opened [, closed )" with line numbers
;;
;; 6. LLM HYBRID
;;    - Context-aware: Understands Clojure semantics
;;    - Can suggest fixes: "Line 7: Change ) to ]"
;;    - Best for explaining WHY it's wrong
