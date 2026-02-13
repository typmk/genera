(ns test-ast-pipeline
  "Test the new AST-based compilation pipeline."
  (:require [clj-php.emit.core :as emit]
            [clj-php.analyze :as ana]
            [clj-php.ast :as ast]
            [clj-php.emit :as emit-ast]
            [clj-php.infer :as infer]))

(defn test-compile [label form]
  (println "---" label "---")
  (println "Form:" (pr-str form))
  (println)
  (println "Old pipeline:")
  (println (emit/compile-form form))
  (println)
  (println "New AST pipeline:")
  (println (emit/compile-form-ast form))
  (println))

;; Test cases
(println "=== Testing AST Pipeline ===\n")

(test-compile "Simple constant"
  42)

(test-compile "Variable reference"
  'x)

(test-compile "If expression"
  '(if true 1 2))

(test-compile "Let binding"
  '(let [x 1] x))

(test-compile "Nested let"
  '(let [a 5]
     (let [b 3]
       (php/+ a b))))

(test-compile "PHP call with type info"
  '(php/strlen "hello"))

(test-compile "Method call"
  '(.getMessage (new Exception "test")))

(test-compile "Function definition"
  '(fn [x] (php/+ x 1)))

(test-compile "Def"
  '(def inc (fn [x] (php/+ x 1))))

(test-compile "Loop/recur"
  '(loop [i 0]
     (if (php/< i 5)
       (recur (php/+ i 1))
       i)))

(println "=== AST Metadata Test ===\n")

;; Show AST with type metadata
(let [env (ast/make-env)
      ast (ana/analyze env '(php/is_iterable x))]
  (println "AST for (php/is_iterable x):")
  (println "  :op" (:op ast))
  (println "  :fn-name" (:fn-name ast))
  (println "  :php/type" (:php/type ast))
  (println "  :php/pure" (:php/pure ast)))

(let [env (ast/make-env)
      ast (ana/analyze env '(php/utf8_encode "hello"))]
  (println "\nAST for (php/utf8_encode \"hello\"):")
  (println "  :op" (:op ast))
  (println "  :fn-name" (:fn-name ast))
  (println "  :php/type" (:php/type ast))
  (println "  :php/pure" (:php/pure ast)))

(println "\n=== Type Inference Test ===\n")

(defn test-infer [label form]
  (let [env (ast/make-env)
        ast (ana/analyze env form)
        typed (infer/infer-types ast)]
    (println "---" label "---")
    (println "Form:" (pr-str form))
    (println "Inferred type:" (:inferred-type typed))
    (println)))

(test-infer "Integer literal" 42)
(test-infer "String literal" "hello")
(test-infer "Boolean literal" true)
(test-infer "Null literal" nil)

(test-infer "PHP call with return type"
  '(php/is_iterable x))

(test-infer "Let binding propagation"
  '(let [x 42] x))

(test-infer "If expression (union type)"
  '(if test 1 "two"))

(test-infer "Function with typed body"
  '(fn [x] (php/is_iterable x)))

(test-infer "Arithmetic infix"
  '(php/+ 1 2))

(test-infer "New expression"
  '(new Exception "error"))

;; Test nested type flow
(println "--- Nested type inference ---")
(let [env (ast/make-env)
      form '(let [s "hello"
                  len (php/strlen s)]
              (php/> len 5))
      ast (ana/analyze env form)
      typed (infer/infer-types ast)]
  (println "Form:" (pr-str form))
  (println "Top-level type:" (:inferred-type typed))
  (println "Bindings:")
  (doseq [b (:bindings typed)]
    (println "  " (:name b) ":" (:type b))))

(println "\n=== Docblock Generation Test ===\n")

(defn test-typed-compile [label form]
  (println "---" label "---")
  (println "Form:" (pr-str form))
  (println "Output:")
  (println (emit/compile-form-typed form))
  (println))

(test-typed-compile "Function with inferred return type"
  '(fn [x] (php/is_iterable x)))

(test-typed-compile "Arithmetic function"
  '(fn [a b] (php/+ a b)))

(test-typed-compile "Boolean logic"
  '(fn [x y] (php/&& x y)))

(test-typed-compile "Object constructor"
  '(fn [msg] (new Exception msg)))

(test-typed-compile "Let with function"
  '(def inc (fn [x] (php/+ x 1))))
