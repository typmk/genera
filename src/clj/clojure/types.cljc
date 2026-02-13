;; Copyright (c) Rich Hickey. All rights reserved.
;; Eclipse Public License 1.0

(ns clojure.types
  "Unified type constructors and predicates for clojure.core.

   Re-exports from clojure.types.* namespaces for convenient access.
   This is the bridge between core.cljc and type implementations."
  (:require [clojure.protocols :as p]
            [clojure.host :as h]
            [clojure.types.kernel :as kernel]
            [clojure.types.vector :as vec]
            [clojure.types.hashmap :as hmap]
            [clojure.types.hashset :as hset]
            [clojure.types.lazy :as lazy]
            [clojure.types.var :as var]))

;; =============================================================================
;; Sequence Constructors
;; =============================================================================

(def list kernel/EMPTY-LIST)

(defn cons
  "Creates a cons cell (head, tail)."
  [head tail]
  (kernel/cons head tail))

;; =============================================================================
;; Symbol & Keyword
;; =============================================================================

(defn symbol
  "Creates a symbol."
  ([name] (kernel/symbol nil name))
  ([ns name] (kernel/symbol ns name)))

(defn symbol? [x]
  (instance? kernel/Sym x))

(defn keyword
  "Returns interned keyword."
  ([name] (kernel/keyword nil name))
  ([ns name] (kernel/keyword ns name)))

(defn keyword? [x]
  (instance? kernel/Kw x))

(defn find-keyword
  "Finds an existing keyword without interning."
  ([name] (kernel/find-keyword name))
  ([ns name] (kernel/find-keyword ns name)))

(defn keyword->symbol
  "Converts keyword to symbol."
  [kw]
  (symbol (p/-namespace kw) (p/-name kw)))

(defn name
  "Returns the name string of a symbol or keyword."
  [x]
  (if (string? x)
    x
    (p/-name x)))

;; =============================================================================
;; Vector
;; =============================================================================

(def EMPTY-VECTOR vec/EMPTY-VECTOR)

(defn vec
  "Creates a vector from a collection."
  [coll]
  (if (nil? coll)
    EMPTY-VECTOR
    (vec/vec coll)))

(defn vector
  "Creates a vector from args."
  [& args]
  (vec args))

(defn subvec
  "Returns a subvector."
  ([v start] (vec/subvec v start))
  ([v start end] (vec/subvec v start end)))

;; =============================================================================
;; HashMap
;; =============================================================================

(def EMPTY-MAP hmap/EMPTY-MAP)

(defn hash-map
  "Creates a hash map from key-value pairs."
  [kvs]
  (hmap/create kvs))

(defn sorted-map
  "Creates a sorted map from key-value pairs."
  [kvs]
  (hmap/sorted-map kvs))

(defn sorted-map-by
  "Creates a sorted map with comparator."
  [comparator kvs]
  (hmap/sorted-map-by comparator kvs))

;; =============================================================================
;; HashSet
;; =============================================================================

(def EMPTY-SET hset/EMPTY-SET)

(defn hash-set
  "Creates a hash set from keys."
  [keys]
  (hset/create keys))

(defn sorted-set
  "Creates a sorted set from keys."
  [keys]
  (hset/sorted-set keys))

(defn sorted-set-by
  "Creates a sorted set with comparator."
  [comparator keys]
  (hset/sorted-set-by comparator keys))

;; =============================================================================
;; Lazy Sequences
;; =============================================================================

(defn lazy-seq*
  "Creates a lazy seq from a thunk."
  [f]
  (lazy/lazy-seq* f))

(defn delay*
  "Creates a delay from a thunk."
  [f]
  (lazy/delay* f))

(defn delay? [x]
  (instance? lazy/Delay x))

;; =============================================================================
;; Chunked Sequences (optional optimization)
;; =============================================================================

(defn chunk-buffer [capacity]
  (lazy/chunk-buffer capacity))

(defn chunk-append [b x]
  (lazy/chunk-append b x))

(defn chunk [b]
  (lazy/chunk b))

(defn chunk-first [s]
  (lazy/chunk-first s))

(defn chunk-rest [s]
  (lazy/chunk-rest s))

(defn chunk-next [s]
  (lazy/chunk-next s))

(defn chunk-cons [chunk rest]
  (lazy/chunk-cons chunk rest))

(defn chunked-seq? [s]
  (lazy/chunked-seq? s))

;; =============================================================================
;; Atom
;; =============================================================================

(defn atom
  "Creates an atom with initial value."
  ([val] (kernel/atom val))
  ([val & opts] (apply kernel/atom val opts)))

;; =============================================================================
;; Var
;; =============================================================================

(defn var? [x]
  (instance? var/Var x))

(defn var->symbol
  "Converts var to symbol."
  [v]
  (symbol (var/var-ns v) (var/var-name v)))

;; =============================================================================
;; Strings
;; =============================================================================

(defn string? [x]
  (h/-string? (h/host) x))

(defn char? [x]
  (h/-char? (h/host) x))

(defn to-string
  "Converts x to string."
  [x]
  (h/-to-string (h/host) x))

(defn string-builder
  "Creates a string builder."
  ([] (h/-string-builder (h/host)))
  ([s] (h/-string-builder (h/host) s)))

(defn sb-append
  "Appends to string builder."
  [sb s]
  (h/-sb-append (h/host) sb s))

(defn sb-to-string
  "Converts string builder to string."
  [sb]
  (h/-sb-to-string (h/host) sb))

;; =============================================================================
;; Function Application
;; =============================================================================

(defn apply-to
  "Applies function to argument seq. Platform-specific implementation."
  [f args]
  (h/-apply-to (h/host) f args))

;; =============================================================================
;; ID Generation
;; =============================================================================

(def ^:private id-counter (kernel/atom 0))

(defn next-id
  "Returns next unique ID."
  []
  (p/-swap! id-counter inc))

;; =============================================================================
;; Type Predicates
;; =============================================================================

(defn number? [x]
  (h/-number? (h/host) x))

(defn integer? [x]
  (h/-integer? (h/host) x))

(defn float? [x]
  (h/-float? (h/host) x))
