(ns clj-php.ast-test
  "Tests for AST and analyze modules."
  (:require [clj-php.ast :as ast]
            [clj-php.analyze :as ana]))

;; ============================================================
;; Test Helpers
;; ============================================================

(defn analyze [form]
  (ana/analyze (ast/make-env) form))

(defn check [description expected actual]
  (if (= expected actual)
    (println "PASS:" description)
    (do
      (println "FAIL:" description)
      (println "  Expected:" expected)
      (println "  Actual:  " actual))))

(defn check-op [description expected-op form]
  (let [result (analyze form)]
    (check description expected-op (:op result))))

;; ============================================================
;; Classification Tests
;; ============================================================

(println "\n=== Classification Tests ===")

(check "nil is const" :const (ana/classify nil))
(check "number is const" :const (ana/classify 42))
(check "string is const" :const (ana/classify "hello"))
(check "keyword is const" :const (ana/classify :foo))
(check "symbol is var" :var (ana/classify 'x))
(check "vector is vector" :vector (ana/classify [1 2 3]))
(check "map is map" :map (ana/classify {:a 1}))
(check "set is set" :set (ana/classify #{1 2}))
(check "if is :if" :if (ana/classify '(if x y z)))
(check "do is :do" :do (ana/classify '(do x y)))
(check "let is :let" :let (ana/classify '(let [x 1] x)))
(check "fn is :fn" :fn (ana/classify '(fn [x] x)))
(check "def is :def" :def (ana/classify '(def x 1)))
(check "loop is :loop" :loop (ana/classify '(loop [x 0] x)))
(check "recur is :recur" :recur (ana/classify '(recur 1)))
(check "throw is :throw" :throw (ana/classify '(throw e)))
(check "try is :try" :try (ana/classify '(try x)))
(check "quote is :quote" :quote (ana/classify '(quote x)))
(check "new is :new" :new (ana/classify '(new Class)))
(check "php call is :php-call" :php-call (ana/classify '(php/strlen s)))
(check "method is :method" :method (ana/classify '(.foo obj)))
(check "constructor is :constructor" :constructor (ana/classify '(Class. x)))
(check "static call is :static-call" :static-call (ana/classify '(Class/method x)))
(check "invoke is :invoke" :invoke (ana/classify '(f x y)))

;; ============================================================
;; Analyze Tests
;; ============================================================

(println "\n=== Analyze Tests ===")

;; Constants
(let [result (analyze 42)]
  (check "const: op" :const (:op result))
  (check "const: val" 42 (:val result)))

(let [result (analyze "hello")]
  (check "string const: val" "hello" (:val result)))

(let [result (analyze nil)]
  (check "nil const: val" nil (:val result)))

;; Variables
(let [result (analyze 'x)]
  (check "var: op" :var (:op result))
  (check "var: name" 'x (:name result)))

;; If
(let [result (analyze '(if true 1 2))]
  (check "if: op" :if (:op result))
  (check "if: test op" :const (get-in result [:test :op]))
  (check "if: test val" true (get-in result [:test :val]))
  (check "if: then val" 1 (get-in result [:then :val]))
  (check "if: else val" 2 (get-in result [:else :val]))
  (check "if: children" [:test :then :else] (:children result)))

;; If without else
(let [result (analyze '(if true 1))]
  (check "if no-else: else" nil (:else result)))

;; Do
(let [result (analyze '(do 1 2 3))]
  (check "do: op" :do (:op result))
  (check "do: stmt count" 2 (count (:statements result)))
  (check "do: ret val" 3 (get-in result [:ret :val])))

;; Let
(let [result (analyze '(let [x 1 y 2] x))]
  (check "let: op" :let (:op result))
  (check "let: binding count" 2 (count (:bindings result)))
  (check "let: first binding name" 'x (:name (first (:bindings result))))
  (check "let: first binding init" 1 (get-in result [:bindings 0 :init :val])))

;; Fn
(let [result (analyze '(fn [x y] (+ x y)))]
  (check "fn: op" :fn (:op result))
  (check "fn: params" '[x y] (:params result))
  (check "fn: variadic?" false (:variadic? result))
  (check "fn: name" nil (:name result)))

;; Fn with name
(let [result (analyze '(fn foo [x] x))]
  (check "named fn: name" 'foo (:name result)))

;; Fn variadic
(let [result (analyze '(fn [x & more] x))]
  (check "variadic fn: variadic?" true (:variadic? result))
  (check "variadic fn: params" '[x more] (:params result)))

;; Def
(let [result (analyze '(def x 42))]
  (check "def: op" :def (:op result))
  (check "def: name" 'x (:name result))
  (check "def: init val" 42 (get-in result [:init :val])))

;; Loop
(let [result (analyze '(loop [i 0] i))]
  (check "loop: op" :loop (:op result))
  (check "loop: binding count" 1 (count (:bindings result)))
  (check "loop: binding name" 'i (:name (first (:bindings result)))))

;; Recur
(let [result (analyze '(recur 1 2))]
  (check "recur: op" :recur (:op result))
  (check "recur: arg count" 2 (count (:args result))))

;; Throw
(let [result (analyze '(throw ex))]
  (check "throw: op" :throw (:op result))
  (check "throw: exception op" :var (get-in result [:exception :op])))

;; Quote
(let [result (analyze '(quote (+ 1 2)))]
  (check "quote: op" :quote (:op result))
  (check "quote: val" '(+ 1 2) (:val result)))

;; New
(let [result (analyze '(new Exception "msg"))]
  (check "new: op" :new (:op result))
  (check "new: class" 'Exception (:class result))
  (check "new: arg count" 1 (count (:args result))))

;; PHP call
(let [result (analyze '(php/strlen "hello"))]
  (check "php-call: op" :php-call (:op result))
  (check "php-call: fn-name" "strlen" (:fn-name result))
  (check "php-call: arg count" 1 (count (:args result))))

;; Method call
(let [result (analyze '(.getMessage ex))]
  (check "method: op" :method (:op result))
  (check "method: method" 'getMessage (:method result))
  (check "method: target op" :var (get-in result [:target :op])))

;; Constructor shorthand
(let [result (analyze '(Exception. "msg"))]
  (check "constructor: op" :new (:op result))
  (check "constructor: class" 'Exception (:class result)))

;; Vector
(let [result (analyze '[1 2 3])]
  (check "vector: op" :vector (:op result))
  (check "vector: item count" 3 (count (:items result))))

;; Map
(let [result (analyze '{:a 1 :b 2})]
  (check "map: op" :map (:op result))
  (check "map: key count" 2 (count (:keys result)))
  (check "map: val count" 2 (count (:vals result))))

;; Invoke
(let [result (analyze '(f x y))]
  (check "invoke: op" :invoke (:op result))
  (check "invoke: fn op" :var (get-in result [:fn :op]))
  (check "invoke: fn name" 'f (get-in result [:fn :name]))
  (check "invoke: arg count" 2 (count (:args result))))

;; ============================================================
;; Tree Walker Tests
;; ============================================================

(println "\n=== Tree Walker Tests ===")

;; Count all nodes
(defn count-nodes [ast]
  (let [counter (atom 0)]
    (ast/postwalk (fn [node]
                    (swap! counter inc)
                    node)
                  ast)
    @counter))

(let [ast (analyze '(if true 1 2))]
  (check "count nodes in (if true 1 2)" 4 (count-nodes ast)))

(let [ast (analyze '(let [x 1] x))]
  ;; let + binding init + body var = nodes
  (check "count nodes in (let [x 1] x)" 3 (count-nodes ast)))

;; Transform all constants
(let [ast (analyze '(if true 1 2))
      doubled (ast/postwalk
               (fn [node]
                 (if (and (= :const (:op node)) (number? (:val node)))
                   (assoc node :val (* 2 (:val node)))
                   node))
               ast)]
  (check "transform: then doubled" 2 (get-in doubled [:then :val]))
  (check "transform: else doubled" 4 (get-in doubled [:else :val])))

(println "\n=== All Tests Complete ===")
