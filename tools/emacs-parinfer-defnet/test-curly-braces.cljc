(ns test.curly-braces
  "Test file for curly brace {} diagnostics.
  Contains intentional errors ONLY in curly braces.
  Parentheses () and square brackets [] remain balanced."
  (:require [clojure.string :as str]))

;; ============================================================================
;; BASELINE: Correct syntax (for reference)
;; ============================================================================

(defn baseline-correct []
  "This function has correct curly brace usage."
  (let [person {:name "Alice" :age 30 :city "NYC"}]
    (:name person)))

;; ============================================================================
;; ERROR TYPE 1: Missing CLOSING curly braces }
;; ============================================================================

;; Error #1: Missing } in map literal
(def missing-close-map
  {:host "localhost"
   :port 8080
   :timeout 5000)

;; Error #2: Missing } in nested map
(def nested-missing-close
  {:server {:host "localhost"
            :port 8080
   :client {:host "127.0.0.1"
            :port 9000}})

;; Error #3: Missing } in destructuring
(defn missing-close-destruct [x y]
  (let [{:keys [name age] {:name "Bob" :age 25}]
    name))

;; Error #4: Missing } in set literal
(def missing-close-set
  #{:apple :banana :cherry)

;; Error #5: Missing } in function body map
(defn create-config [host port]
  {:connection {:host host
                :port port
   :timeout 30})

;; ============================================================================
;; ERROR TYPE 2: Missing OPENING curly braces {
;; ============================================================================

;; Error #6: Missing { in map literal
(def missing-open-map
  :a 1 :b 2 :c 3})

;; Error #7: Missing { in nested structure
(def nested-missing-open
  {:outer :a 1 :b 2}
   :inner {:c 3 :d 4}})

;; Error #8: Missing { in destructuring
(defn missing-open-destruct [data]
  (let [:keys [x y z]} data]
    (+ x y z)))

;; Error #9: Missing #{ in set literal
(def missing-hash-set
  1 2 3 4 5})

;; ============================================================================
;; ERROR TYPE 3: Extra/misplaced curly braces
;; ============================================================================

;; Error #10: Extra closing }
(defn extra-close-brace []
  (let [m {:a 1 :b 2}]
    m}})

;; Error #11: Extra opening {
(defn extra-open-brace [x]
  (let [m {{:value x}]
    m))

;; ============================================================================
;; CONTROL: Correct nested curly braces (no errors)
;; ============================================================================

(defn control-correct-nested []
  "This function should show ZERO errors - control group"
  (let [config {:database {:host "localhost"
                           :port 5432
                           :name "mydb"}
                :cache {:enabled true
                        :ttl 3600}
                :logging {:level :info
                          :file "app.log"}}]
    (:database config)))

;; ============================================================================
;; ERROR TYPE 4: Curly braces in complex nesting
;; ============================================================================

;; Error #12: Missing } in deeply nested map
(defn complex-nesting []
  (let [app-config {:server {:http {:port 8080
                                    :host "0.0.0.0"
                            :https {:port 8443
                                    :host "0.0.0.0"}}
                    :database {:url "jdbc:..."
                               :pool {:size 10}}}]
    app-config))

;; Error #13: Missing { in map update
(defn update-config [config]
  (assoc config :new-key :a 1 :b 2}))

;; Error #14: Missing } in set operations
(def set-operations
  (clojure.set/union #{1 2 3
                     #{4 5 6}))

;; ============================================================================
;; READER MACRO ERRORS (sets with #{})
;; ============================================================================

;; Error #15: Missing } in reader set
(def reader-set-error
  #{:x :y :z)

;; Error #16: Missing # before set
(def missing-hash
  {:not-a-set 1 2 3})

;; ============================================================================
;; STATISTICS
;; ============================================================================

;; Curly brace counts in this file:
;; - Intentionally MISSING closing }: 9 locations
;; - Intentionally MISSING opening {: 4 locations
;; - Extra braces: 2 locations
;; - Reader macro errors: 2 locations
;; - Total errors: 17 curly brace errors
;;
;; All () parentheses: BALANCED (no errors)
;; All [] square brackets: BALANCED (no errors)
;; Only {} curly braces have errors
