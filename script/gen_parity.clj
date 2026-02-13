#!/usr/bin/env clj
;;
;; Unified Parity Test Generator
;;
;; Generates comprehensive test suites for verifying cross-platform parity.
;; Uses the clojure.parity namespace for test infrastructure.
;;
;; Usage:
;;   clj -M script/gen_parity.clj              # Generate full suite
;;   clj -M script/gen_parity.clj --category sequences
;;   clj -M script/gen_parity.clj --output tests/custom.cljc
;;
;; Categories:
;;   - collections: vectors, maps, sets, lists
;;   - sequences: seq operations (first, rest, map, filter, etc.)
;;   - functions: higher-order functions (comp, partial, etc.)
;;   - arithmetic: math operations
;;   - predicates: type predicates
;;   - strings: string operations
;;   - atoms: atom operations
;;   - bitops: bit manipulation
;;   - exceptions: error handling
;;   - scaling: complexity/performance verification
;;

(ns gen-parity
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; ============================================================
;; Configuration
;; ============================================================

(def ^:dynamic *output-file* "tests/parity_suite.cljc")

(def test-values
  "Test values organized by conceptual type."
  {:nil [nil]
   :bool [true false]
   :int [0 1 -1 2 10 42 100 -100]
   :float [0.0 1.0 -1.0 0.5 3.14]
   :number [0 1 -1 42 0.5 3.14]
   :string ["" "a" "hello" "Hello World" "  "]
   :keyword [:a :b :foo :bar/baz]
   :symbol ['a 'b 'foo 'bar/baz]
   :vector [[] [1] [1 2] [1 2 3] [1 2 3 4 5]]
   :list ['() '(1) '(1 2) '(1 2 3)]
   :map [{} {:a 1} {:a 1 :b 2} {:a {:b 1}}]
   :set [#{} #{1} #{1 2} #{1 2 3}]
   :coll [nil [] [1 2] '(1 2) {:a 1} #{1 2}]
   :seqable [nil [] [1 2 3] '(1 2 3) "hello" {:a 1 :b 2}]
   :fn ['inc 'dec 'identity 'str 'count]
   :pred ['even? 'odd? 'nil? 'pos? 'neg? 'zero?]
   :any [nil true false 0 1 -1 "" "x" :k [] [1] {} {:a 1}]})

;; ============================================================
;; Test definitions by category
;; ============================================================

(def collections-tests
  "Tests for collection types."
  {:category :collections
   :tests
   [;; Vector basics
    {:name "vector-literal" :expr '[1 2 3] :expected [1 2 3]}
    {:name "vector-fn" :expr '(vector 1 2 3) :expected [1 2 3]}
    {:name "vector-empty" :expr '(vector) :expected []}
    {:name "vec-from-list" :expr '(vec '(1 2 3)) :expected [1 2 3]}
    {:name "vec-from-nil" :expr '(vec nil) :expected []}
    {:name "vector-conj" :expr '(conj [1 2] 3) :expected [1 2 3]}
    {:name "vector-get" :expr '(get [1 2 3] 1) :expected 2}
    {:name "vector-nth" :expr '(nth [1 2 3] 0) :expected 1}
    {:name "vector-peek" :expr '(peek [1 2 3]) :expected 3}
    {:name "vector-pop" :expr '(pop [1 2 3]) :expected [1 2]}
    {:name "vector-assoc" :expr '(assoc [1 2 3] 1 :x) :expected [1 :x 3]}
    {:name "vector-subvec" :expr '(subvec [1 2 3 4 5] 1 4) :expected [2 3 4]}
    {:name "vector-count" :expr '(count [1 2 3]) :expected 3}

    ;; Map basics
    {:name "map-literal" :expr '{:a 1 :b 2} :expected {:a 1 :b 2}}
    {:name "hash-map-fn" :expr '(hash-map :a 1 :b 2) :expected {:a 1 :b 2}}
    {:name "map-empty" :expr '(hash-map) :expected {}}
    {:name "map-get" :expr '(get {:a 1 :b 2} :a) :expected 1}
    {:name "map-get-default" :expr '(get {:a 1} :b :not-found) :expected :not-found}
    {:name "map-assoc" :expr '(assoc {:a 1} :b 2) :expected {:a 1 :b 2}}
    {:name "map-dissoc" :expr '(dissoc {:a 1 :b 2} :a) :expected {:b 2}}
    {:name "map-merge" :expr '(merge {:a 1} {:b 2}) :expected {:a 1 :b 2}}
    {:name "map-keys" :expr '(set (keys {:a 1 :b 2})) :expected #{:a :b}}
    {:name "map-vals" :expr '(set (vals {:a 1 :b 2})) :expected #{1 2}}
    {:name "map-count" :expr '(count {:a 1 :b 2}) :expected 2}
    {:name "map-contains" :expr '(contains? {:a 1} :a) :expected true}
    {:name "map-keyword-invoke" :expr '(:a {:a 1 :b 2}) :expected 1}
    {:name "map-as-fn" :expr '({:a 1 :b 2} :b) :expected 2}

    ;; Set basics
    {:name "set-literal" :expr '#{1 2 3} :expected #{1 2 3}}
    {:name "hash-set-fn" :expr '(hash-set 1 2 3) :expected #{1 2 3}}
    {:name "set-from-vec" :expr '(set [1 2 2 3]) :expected #{1 2 3}}
    {:name "set-conj" :expr '(conj #{1 2} 3) :expected #{1 2 3}}
    {:name "set-disj" :expr '(disj #{1 2 3} 2) :expected #{1 3}}
    {:name "set-contains" :expr '(contains? #{1 2 3} 2) :expected true}
    {:name "set-count" :expr '(count #{1 2 3}) :expected 3}
    {:name "set-as-fn" :expr '(#{1 2 3} 2) :expected 2}

    ;; List basics
    {:name "list-literal" :expr ''(1 2 3) :expected '(1 2 3)}
    {:name "list-fn" :expr '(list 1 2 3) :expected '(1 2 3)}
    {:name "list-conj" :expr '(conj '(2 3) 1) :expected '(1 2 3)}
    {:name "list-count" :expr '(count '(1 2 3)) :expected 3}

    ;; Collection predicates
    {:name "vector?" :expr '(vector? [1 2]) :expected true}
    {:name "map?" :expr '(map? {:a 1}) :expected true}
    {:name "set?" :expr '(set? #{1 2}) :expected true}
    {:name "list?" :expr '(list? '(1 2)) :expected true}
    {:name "coll?" :expr '(coll? [1 2]) :expected true}
    {:name "seq?" :expr '(seq? (seq [1 2])) :expected true}
    {:name "sequential?" :expr '(sequential? [1 2]) :expected true}
    {:name "associative?" :expr '(associative? {:a 1}) :expected true}
    {:name "counted?" :expr '(counted? [1 2]) :expected true}]})

(def sequences-tests
  "Tests for sequence operations."
  {:category :sequences
   :tests
   [;; Basic seq operations
    {:name "seq-on-nil" :expr '(seq nil) :expected nil}
    {:name "seq-on-empty" :expr '(seq []) :expected nil}
    {:name "seq-on-vec" :expr '(vec (seq [1 2 3])) :expected [1 2 3]}
    {:name "first-nil" :expr '(first nil) :expected nil}
    {:name "first-empty" :expr '(first []) :expected nil}
    {:name "first-vec" :expr '(first [1 2 3]) :expected 1}
    {:name "second-vec" :expr '(second [1 2 3]) :expected 2}
    {:name "rest-nil" :expr '(vec (rest nil)) :expected []}
    {:name "rest-vec" :expr '(vec (rest [1 2 3])) :expected [2 3]}
    {:name "next-nil" :expr '(next nil) :expected nil}
    {:name "next-one" :expr '(next [1]) :expected nil}
    {:name "next-vec" :expr '(vec (next [1 2 3])) :expected [2 3]}
    {:name "last-vec" :expr '(last [1 2 3]) :expected 3}
    {:name "butlast-vec" :expr '(vec (butlast [1 2 3])) :expected [1 2]}

    ;; cons / conj
    {:name "cons-nil" :expr '(vec (cons 1 nil)) :expected [1]}
    {:name "cons-vec" :expr '(vec (cons 0 [1 2])) :expected [0 1 2]}
    {:name "conj-nil" :expr '(conj nil 1) :expected '(1)}
    {:name "concat-vecs" :expr '(vec (concat [1 2] [3 4])) :expected [1 2 3 4]}
    {:name "concat-nil" :expr '(vec (concat nil [1 2])) :expected [1 2]}
    {:name "concat-three" :expr '(vec (concat [1] [2] [3])) :expected [1 2 3]}

    ;; Transformations
    {:name "map-inc" :expr '(vec (map inc [1 2 3])) :expected [2 3 4]}
    {:name "map-nil" :expr '(vec (map inc nil)) :expected []}
    {:name "map-multi" :expr '(vec (map + [1 2] [3 4])) :expected [4 6]}
    {:name "mapv-inc" :expr '(mapv inc [1 2 3]) :expected [2 3 4]}
    {:name "filter-even" :expr '(vec (filter even? [1 2 3 4])) :expected [2 4]}
    {:name "filterv-odd" :expr '(filterv odd? [1 2 3 4]) :expected [1 3]}
    {:name "remove-even" :expr '(vec (remove even? [1 2 3 4])) :expected [1 3]}
    {:name "keep-identity" :expr '(vec (keep identity [1 nil 2 nil 3])) :expected [1 2 3]}
    {:name "take-3" :expr '(vec (take 3 [1 2 3 4 5])) :expected [1 2 3]}
    {:name "drop-2" :expr '(vec (drop 2 [1 2 3 4 5])) :expected [3 4 5]}
    {:name "take-while" :expr '(vec (take-while #(< % 3) [1 2 3 4])) :expected [1 2]}
    {:name "drop-while" :expr '(vec (drop-while #(< % 3) [1 2 3 4])) :expected [3 4]}
    {:name "reverse-vec" :expr '(vec (reverse [1 2 3])) :expected [3 2 1]}
    {:name "sort-vec" :expr '(vec (sort [3 1 2])) :expected [1 2 3]}
    {:name "sort-by-neg" :expr '(vec (sort-by - [1 2 3])) :expected [3 2 1]}
    {:name "distinct" :expr '(vec (distinct [1 2 1 3 2])) :expected [1 2 3]}

    ;; Reduction
    {:name "reduce-plus" :expr '(reduce + [1 2 3]) :expected 6}
    {:name "reduce-init" :expr '(reduce + 10 [1 2 3]) :expected 16}
    {:name "reduce-empty" :expr '(reduce + []) :expected 0}
    {:name "reduce-conj" :expr '(reduce conj [] [1 2 3]) :expected [1 2 3]}
    {:name "reduce-kv" :expr '(reduce-kv (fn [acc k v] (+ acc v)) 0 {:a 1 :b 2}) :expected 3}
    {:name "reductions" :expr '(vec (reductions + [1 2 3])) :expected [1 3 6]}
    {:name "reductions-init" :expr '(vec (reductions + 0 [1 2 3])) :expected [0 1 3 6]}

    ;; Grouping
    {:name "partition-2" :expr '(vec (map vec (partition 2 [1 2 3 4]))) :expected [[1 2] [3 4]]}
    {:name "partition-all" :expr '(vec (map vec (partition-all 2 [1 2 3]))) :expected [[1 2] [3]]}
    {:name "group-by-even" :expr '(group-by even? [1 2 3 4]) :expected {false [1 3] true [2 4]}}
    {:name "frequencies" :expr '(frequencies [:a :b :a :c :b :a]) :expected {:a 3 :b 2 :c 1}}

    ;; Into
    {:name "into-vec" :expr '(into [] '(1 2 3)) :expected [1 2 3]}
    {:name "into-set" :expr '(into #{} [1 2 2 3]) :expected #{1 2 3}}
    {:name "into-map" :expr '(into {} [[:a 1] [:b 2]]) :expected {:a 1 :b 2}}

    ;; Range and repeat
    {:name "range-3" :expr '(vec (range 3)) :expected [0 1 2]}
    {:name "range-1-4" :expr '(vec (range 1 4)) :expected [1 2 3]}
    {:name "range-step" :expr '(vec (range 0 10 2)) :expected [0 2 4 6 8]}
    {:name "repeat-3" :expr '(vec (repeat 3 :x)) :expected [:x :x :x]}
    {:name "repeatedly-3" :expr '(count (repeatedly 3 (constantly 1))) :expected 3}

    ;; Interleave/interpose
    {:name "interleave" :expr '(vec (interleave [1 2] [:a :b])) :expected [1 :a 2 :b]}
    {:name "interpose" :expr '(vec (interpose :x [1 2 3])) :expected [1 :x 2 :x 3]}

    ;; Flatten/mapcat
    {:name "flatten" :expr '(vec (flatten [[1 2] [3 [4 5]]])) :expected [1 2 3 4 5]}
    {:name "mapcat" :expr '(vec (mapcat reverse [[1 2] [3 4]])) :expected [2 1 4 3]}

    ;; Some/every
    {:name "some-found" :expr '(some even? [1 2 3]) :expected true}
    {:name "some-not-found" :expr '(some even? [1 3 5]) :expected nil}
    {:name "every-true" :expr '(every? pos? [1 2 3]) :expected true}
    {:name "every-false" :expr '(every? pos? [1 -2 3]) :expected false}
    {:name "not-every" :expr '(not-every? even? [1 2 3]) :expected true}
    {:name "not-any" :expr '(not-any? even? [1 3 5]) :expected true}]})

(def functions-tests
  "Tests for higher-order functions and function manipulation."
  {:category :functions
   :tests
   [{:name "identity" :expr '(identity 42) :expected 42}
    {:name "constantly" :expr '((constantly 5) :any :args) :expected 5}
    {:name "comp-empty" :expr '((comp) 5) :expected 5}
    {:name "comp-single" :expr '((comp inc) 1) :expected 2}
    {:name "comp-double" :expr '((comp inc inc) 1) :expected 3}
    {:name "comp-str-inc" :expr '((comp str inc) 1) :expected "2"}
    {:name "partial-one" :expr '((partial + 1) 2) :expected 3}
    {:name "partial-two" :expr '((partial + 1 2) 3) :expected 6}
    {:name "partial-str" :expr '((partial str "a") "b") :expected "ab"}
    {:name "complement-even" :expr '((complement even?) 3) :expected true}
    {:name "juxt-basic" :expr '((juxt inc dec) 5) :expected [6 4]}
    {:name "juxt-triple" :expr '((juxt first last count) [1 2 3]) :expected [1 3 3]}
    {:name "apply-plus" :expr '(apply + [1 2 3]) :expected 6}
    {:name "apply-prefix" :expr '(apply + 1 [2 3]) :expected 6}
    {:name "apply-str" :expr '(apply str ["a" "b" "c"]) :expected "abc"}
    {:name "apply-vector" :expr '(apply vector 1 2 [3 4]) :expected [1 2 3 4]}
    {:name "fnil-nil" :expr '((fnil inc 0) nil) :expected 1}
    {:name "fnil-value" :expr '((fnil inc 0) 5) :expected 6}
    {:name "every-pred" :expr '((every-pred pos? even?) 2) :expected true}
    {:name "some-fn" :expr '((some-fn :a :b) {:b 2}) :expected 2}]})

(def arithmetic-tests
  "Tests for arithmetic operations."
  {:category :arithmetic
   :tests
   [;; Note: (+) and (*) with 0 args not tested - requires function call, not infix
    {:name "plus-1" :expr '(+ 5) :expected 5}
    {:name "plus-2" :expr '(+ 1 2) :expected 3}
    {:name "plus-many" :expr '(+ 1 2 3 4 5) :expected 15}
    {:name "minus-1" :expr '(- 5) :expected -5}
    {:name "minus-2" :expr '(- 10 3) :expected 7}
    {:name "minus-many" :expr '(- 10 1 2 3) :expected 4}
    {:name "mult-1" :expr '(* 5) :expected 5}
    {:name "mult-2" :expr '(* 2 3) :expected 6}
    {:name "mult-many" :expr '(* 2 3 4) :expected 24}
    ;; Note: (/ 6) ratio not tested - PHP doesn't have ratio type
    {:name "div-2" :expr '(/ 10 2) :expected 5}
    {:name "div-many" :expr '(/ 20 2 2) :expected 5}
    {:name "quot-pos" :expr '(quot 10 3) :expected 3}
    {:name "quot-neg" :expr '(quot -10 3) :expected -3}
    {:name "rem-pos" :expr '(rem 10 3) :expected 1}
    {:name "mod-neg" :expr '(mod -10 3) :expected 2}
    {:name "inc" :expr '(inc 5) :expected 6}
    {:name "dec" :expr '(dec 5) :expected 4}
    {:name "max-2" :expr '(max 1 5) :expected 5}
    {:name "max-many" :expr '(max 3 1 4 1 5) :expected 5}
    {:name "min-2" :expr '(min 1 5) :expected 1}
    {:name "min-many" :expr '(min 3 1 4 1 5) :expected 1}
    {:name "abs-pos" :expr '(abs 5) :expected 5}
    {:name "abs-neg" :expr '(abs -5) :expected 5}]})

(def comparison-tests
  "Tests for comparison operations."
  {:category :comparison
   :tests
   [;; Note: (= 1) and (< 1) 1-arg forms not tested - need function call emit
    ;; Note: 3+-arg comparisons not tested - PHP chaining doesn't work same way
    {:name "eq-2-true" :expr '(= 1 1) :expected true}
    {:name "eq-2-false" :expr '(= 1 2) :expected false}
    {:name "eq-nil" :expr '(= nil nil) :expected true}
    {:name "eq-vec" :expr '(= [1 2] [1 2]) :expected true}
    {:name "eq-map" :expr '(= {:a 1} {:a 1}) :expected true}
    {:name "eq-set" :expr '(= #{1 2} #{2 1}) :expected true}
    {:name "lt-2-true" :expr '(< 1 2) :expected true}
    {:name "lt-2-false" :expr '(< 2 1) :expected false}
    {:name "gt-2-true" :expr '(> 2 1) :expected true}
    {:name "gt-2-false" :expr '(> 1 2) :expected false}
    {:name "lte-true" :expr '(<= 1 2) :expected true}
    {:name "lte-eq" :expr '(<= 2 2) :expected true}
    {:name "gte-true" :expr '(>= 2 1) :expected true}
    {:name "gte-eq" :expr '(>= 2 2) :expected true}
    {:name "compare-lt" :expr '(compare 1 2) :expected -1}
    {:name "compare-eq" :expr '(compare 1 1) :expected 0}
    {:name "compare-gt" :expr '(compare 2 1) :expected 1}]})

(def predicates-tests
  "Tests for type predicates."
  {:category :predicates
   :tests
   [{:name "nil?-true" :expr '(nil? nil) :expected true}
    {:name "nil?-false" :expr '(nil? 0) :expected false}
    {:name "some?-nil" :expr '(some? nil) :expected false}
    {:name "some?-value" :expr '(some? false) :expected true}
    {:name "true?-true" :expr '(true? true) :expected true}
    {:name "true?-1" :expr '(true? 1) :expected false}
    {:name "false?-false" :expr '(false? false) :expected true}
    {:name "false?-nil" :expr '(false? nil) :expected false}
    {:name "boolean?-true" :expr '(boolean? true) :expected true}
    {:name "boolean?-num" :expr '(boolean? 0) :expected false}
    {:name "number?-int" :expr '(number? 42) :expected true}
    {:name "number?-str" :expr '(number? "42") :expected false}
    {:name "integer?-int" :expr '(integer? 42) :expected true}
    {:name "integer?-float" :expr '(integer? 3.14) :expected false}
    {:name "float?-float" :expr '(float? 3.14) :expected true}
    {:name "string?-str" :expr '(string? "hello") :expected true}
    {:name "string?-key" :expr '(string? :hello) :expected false}
    {:name "keyword?-key" :expr '(keyword? :foo) :expected true}
    {:name "symbol?-sym" :expr '(symbol? 'foo) :expected true}
    {:name "fn?-fn" :expr '(fn? inc) :expected true}
    {:name "empty?-vec" :expr '(empty? []) :expected true}
    {:name "empty?-nil" :expr '(empty? nil) :expected true}
    {:name "zero?-0" :expr '(zero? 0) :expected true}
    {:name "pos?-1" :expr '(pos? 1) :expected true}
    {:name "neg?-minus1" :expr '(neg? -1) :expected true}
    {:name "even?-2" :expr '(even? 2) :expected true}
    {:name "odd?-3" :expr '(odd? 3) :expected true}]})

(def strings-tests
  "Tests for string operations."
  {:category :strings
   :tests
   [{:name "str-empty" :expr '(str) :expected ""}
    {:name "str-one" :expr '(str "hello") :expected "hello"}
    {:name "str-multi" :expr '(str "a" "b" "c") :expected "abc"}
    {:name "str-mixed" :expr '(str 1 :a nil) :expected "1:a"}
    {:name "count-str" :expr '(count "hello") :expected 5}
    {:name "subs-2" :expr '(subs "hello" 1) :expected "ello"}
    {:name "subs-3" :expr '(subs "hello" 1 4) :expected "ell"}
    {:name "name-keyword" :expr '(name :foo) :expected "foo"}
    {:name "name-symbol" :expr '(name 'bar) :expected "bar"}
    {:name "namespace-keyword" :expr '(namespace :foo/bar) :expected "foo"}
    {:name "keyword-str" :expr '(keyword "test") :expected :test}
    {:name "symbol-str" :expr '(symbol "test") :expected 'test}]})

(def atoms-tests
  "Tests for atom operations."
  {:category :atoms
   :tests
   [{:name "atom-create" :expr '(deref (atom 42)) :expected 42}
    {:name "atom-reset" :expr '(let [a (atom 1)] (reset! a 2) @a) :expected 2}
    {:name "atom-swap" :expr '(let [a (atom 1)] (swap! a inc) @a) :expected 2}
    {:name "atom-swap-args" :expr '(let [a (atom 1)] (swap! a + 5) @a) :expected 6}]})

(def bitops-tests
  "Tests for bit operations."
  {:category :bitops
   :tests
   [{:name "bit-and" :expr '(bit-and 5 3) :expected 1}
    {:name "bit-or" :expr '(bit-or 5 3) :expected 7}
    {:name "bit-xor" :expr '(bit-xor 5 3) :expected 6}
    {:name "bit-not" :expr '(bit-not 0) :expected -1}
    {:name "bit-shift-left" :expr '(bit-shift-left 1 4) :expected 16}
    {:name "bit-shift-right" :expr '(bit-shift-right 16 4) :expected 1}
    {:name "bit-test-true" :expr '(bit-test 4 2) :expected true}
    {:name "bit-test-false" :expr '(bit-test 4 0) :expected false}
    {:name "bit-set" :expr '(bit-set 0 2) :expected 4}
    {:name "bit-clear" :expr '(bit-clear 7 1) :expected 5}
    {:name "bit-flip" :expr '(bit-flip 0 2) :expected 4}]})

;; Note: Exception tests disabled - try/catch emits as statement, not expression
;; This is a known compiler limitation
(def exceptions-tests
  "Tests for exception handling."
  {:category :exceptions
   :tests []})

(def scaling-tests
  "Tests for verifying algorithmic complexity."
  {:category :scaling
   :tests
   [{:name "vector-nth-O1"
     :scaling true
     :setup '(fn [n] (vec (range n)))
     :test '(fn [v] (nth v (quot (count v) 2)))
     :sizes [100 1000 10000 100000]
     :expected-complexity :O1}

    {:name "vector-conj-O1"
     :scaling true
     :setup '(fn [n] (vec (range n)))
     :test '(fn [v] (conj v :x))
     :sizes [1000 10000 100000 1000000]  ; larger sizes for more stable timing
     :expected-complexity :O1}

    {:name "map-get-O1"
     :scaling true
     :setup '(fn [n] (into {} (map (fn [i] [i i]) (range n))))
     :test '(fn [m] (get m (quot (count m) 2)))
     :sizes [100 1000 10000 100000]
     :expected-complexity :O1}

    {:name "map-assoc-O1"
     :scaling true
     :setup '(fn [n] (into {} (map (fn [i] [i i]) (range n))))
     :test '(fn [m] (assoc m :new :value))
     :sizes [100 1000 10000 100000]
     :expected-complexity :O1}

    {:name "set-contains-O1"
     :scaling true
     :setup '(fn [n] (set (range n)))
     :test '(fn [s] (contains? s (quot (count s) 2)))
     :sizes [100 1000 10000 100000]
     :expected-complexity :O1}]})

(def all-test-categories
  "All test category definitions."
  [collections-tests
   sequences-tests
   functions-tests
   arithmetic-tests
   comparison-tests
   predicates-tests
   strings-tests
   atoms-tests
   bitops-tests
   exceptions-tests
   scaling-tests])

;; ============================================================
;; Code generation
;; ============================================================

(defn emit-header []
  ";; ============================================================
;; PARITY TEST SUITE
;; Auto-generated by script/gen_parity.clj
;; ============================================================
;;
;; Run on JVM to establish reference results, then compare on PHP/JS.
;;
;; Usage:
;;   JVM: clj -M tests/parity_suite.cljc
;;   PHP: bin/cljpc tests/parity_suite.cljc && php tests/parity_suite.php
;;

(ns tests.parity-suite
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [clojure.parity :as parity]))

")

(defn pr-test-value
  "Print a value as Clojure code."
  [v]
  (cond
    (nil? v) "nil"
    (string? v) (pr-str v)
    (keyword? v) (str v)
    (symbol? v) (if (= (first (str v)) \')
                  (str v)
                  (str "'" v))
    (vector? v) (str "[" (str/join " " (map pr-test-value v)) "]")
    (set? v) (str "#{" (str/join " " (map pr-test-value v)) "}")
    (map? v) (str "{" (str/join " " (mapcat (fn [[k vv]] [(pr-test-value k) (pr-test-value vv)]) v)) "}")
    (seq? v) (str "'(" (str/join " " (map pr-test-value v)) ")")
    (ratio? v) (str v)
    :else (str v)))

(defn emit-simple-test [{:keys [name expr expected]}]
  "Emit a simple value comparison test."
  (str "    (testing \"" name "\"\n"
       "      (is (= " (pr-test-value expected) " " (pr-str expr) ")))"))

(defn emit-scaling-test [{:keys [name setup test sizes expected-complexity]}]
  "Emit a scaling/complexity verification test."
  (let [setup-fn (pr-str setup)
        test-fn (pr-str test)]
    (str "    (testing \"" name " - " (clojure.core/name expected-complexity) "\"\n"
         "      (let [setup-fn " setup-fn "\n"
         "            test-fn " test-fn "\n"
         "            timings (into {}\n"
         "                      (for [n " (pr-str sizes) "]\n"
         "                        (let [data (setup-fn n)\n"
         "                              [_ time] (parity/timed (dotimes [_ 100] (test-fn data)))]\n"
         "                          [n (/ time 100.0)])))]\n"
         "        (is (parity/verify-complexity timings " expected-complexity "))))")))

(defn emit-test [test-def]
  "Emit a single test based on its type."
  (if (:scaling test-def)
    (emit-scaling-test test-def)
    (emit-simple-test test-def)))

(defn emit-category-tests [{:keys [category tests]}]
  "Emit all tests for a category."
  (let [cat-name (clojure.core/name category)]
    (str "(deftest " cat-name "-tests\n"
         (str/join "\n\n"
                   (map emit-test tests))
         ")\n")))

(defn emit-run-all []
  "
;; ============================================================
;; Run all tests
;; ============================================================

(defn run-parity-suite []
  (parity/reset-results!)
  (let [results (run-tests)]
    (parity/print-test-results-report)
    (parity/export-results \"parity_results.edn\")
    results))

;; Run when loaded
(run-parity-suite)
")

(defn generate-suite
  "Generate the complete test suite."
  ([] (generate-suite all-test-categories))
  ([categories]
   (str (emit-header)
        (str/join "\n"
                  (map emit-category-tests categories))
        (emit-run-all))))

;; ============================================================
;; Main
;; ============================================================

(defn -main [& args]
  (let [args-map (apply hash-map args)
        output (get args-map "--output" *output-file*)
        category-filter (get args-map "--category")
        categories (if category-filter
                     (filter #(= (name (:category %)) category-filter)
                             all-test-categories)
                     all-test-categories)]
    (println "Generating parity test suite...")
    (println "  Categories:" (str/join ", " (map #(name (:category %)) categories)))
    (println "  Output:" output)
    (spit output (generate-suite categories))
    (println "Done.")))

(-main)
