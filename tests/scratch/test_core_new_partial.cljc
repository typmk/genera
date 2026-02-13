;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; =============================================================================
;; PORTABLE CLOJURE CORE
;; =============================================================================
;; Modified from upstream clojure.core to use protocols and host abstraction
;; instead of JVM-specific clojure.lang.* calls.
;;
;; Replacement patterns:
;;   clojure.lang.RT/first      → (p/-first (p/-seq coll))
;;   clojure.lang.RT/next       → (p/-next coll)
;;   clojure.lang.RT/more       → (p/-rest coll)
;;   clojure.lang.RT/seq        → (p/-seq coll)
;;   clojure.lang.RT/cons       → (t/cons x coll)
;;   clojure.lang.RT/conj       → (p/-conj coll x)
;;   clojure.lang.RT/assoc      → (p/-assoc m k v)
;;   clojure.lang.RT/count      → (p/-count coll)
;;   clojure.lang.RT/nth        → (p/-nth coll i)
;;   clojure.lang.RT/get        → (p/-lookup m k)
;;   clojure.lang.RT/contains   → (p/-contains-key? coll k)
;;   clojure.lang.RT/peek       → (p/-peek coll)
;;   clojure.lang.RT/pop        → (p/-pop coll)
;;   clojure.lang.RT/keys       → (t/keys m)
;;   clojure.lang.RT/vals       → (t/vals m)
;;   clojure.lang.Util/identical → (h/-identical? (h/host) a b)
;;   (instance? clojure.lang.ISeq x) → (satisfies? p/ISeq x)
;; =============================================================================

(ns ^{:doc "The core Clojure language."
       :author "Rich Hickey"}
  clojure.core
  (:require [clojure.protocols :as p]
            [clojure.host :as h]
            [clojure.types :as t]))

(def unquote)
(def unquote-splicing)

;; =============================================================================
;; Bootstrap - defined before defn exists
;; =============================================================================

(def
 ^{:arglists '([& items])
   :doc "Creates a new list containing the items."
   :added "1.0"}
 list t/list)

(def
 ^{:arglists '([x seq])
   :doc "Returns a new seq where x is the first element and seq is
    the rest."
   :added "1.0"
   :static true}
 cons (fn* ^:static cons [x coll] (t/cons x coll)))

;; during bootstrap we don't have destructuring let, loop or fn, will redefine later
(def
  ^{:macro true
    :added "1.0"}
  let (fn* let [&form &env & decl] (cons 'let* decl)))

(def
 ^{:macro true
   :added "1.0"}
 loop (fn* loop [&form &env & decl] (cons 'loop* decl)))

(def
 ^{:macro true
   :added "1.0"}
 fn (fn* fn [&form &env & decl]
         (.withMeta ^clojure.lang.IObj (cons 'fn* decl)
                    (.meta ^clojure.lang.IMeta &form))))

(def
 ^{:arglists '([coll])
   :doc "Returns the first item in the collection. Calls seq on its
    argument. If coll is nil, returns nil."
   :added "1.0"
   :static true}
 first (fn ^:static first [coll]
         (when coll
           (let [s (p/-seq coll)]
             (when s (p/-first s))))))

(def
 ^{:arglists '([coll])
   :doc "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
   :added "1.0"
   :static true}
 next (fn ^:static next [coll]
        (when coll
          (let [s (p/-seq coll)]
            (when s (p/-next s))))))

(def
 ^{:arglists '([coll])
   :doc "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
   :added "1.0"
   :static true}
 rest (fn ^:static rest [coll]
        (if coll
          (let [s (p/-seq coll)]
            (if s (p/-rest s) ()))
          ())))

(def
 ^{:arglists '([] [coll] [coll x] [coll x & xs])
   :doc "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).
    (conj coll) returns coll. (conj) returns [].
    The 'addition' may happen at different 'places' depending
    on the concrete type."
   :added "1.0"
   :static true}
 conj (fn ^:static conj
        ([] [])
        ([coll] coll)
        ([coll x] (if (nil? coll)
                    (t/list x)
                    (p/-conj coll x)))
        ([coll x & xs]
         (if xs
           (recur (conj coll x) (first xs) (next xs))
           (conj coll x)))))

(def
 ^{:doc "Same as (first (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 second (fn ^:static second [x] (first (next x))))

(def
 ^{:doc "Same as (first (first x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 ffirst (fn ^:static ffirst [x] (first (first x))))

(def
 ^{:doc "Same as (next (first x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 nfirst (fn ^:static nfirst [x] (next (first x))))

(def
 ^{:doc "Same as (first (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 fnext (fn ^:static fnext [x] (first (next x))))

(def
 ^{:doc "Same as (next (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 nnext (fn ^:static nnext [x] (next (next x))))

(def
 ^{:arglists '([coll])
   :doc "Returns a seq on the collection. If the collection is
    empty, returns nil.  (seq nil) returns nil. seq also works on
    Strings, native arrays (of reference types) and any objects
    that implement Iterable."
   :added "1.0"
   :static true}
 seq (fn ^:static seq [coll]
       (when coll (p/-seq coll))))

(def
 ^{:arglists '([type x])
   :doc "Evaluates x and tests if it is an instance of the type.
    Returns true or false. Type checking is platform-specific."
   :added "1.0"}
 instance? (fn instance? [type x] (h/-instance? (h/host) type x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements ISeq"
   :added "1.0"
   :static true}
 seq? (fn ^:static seq? [x] (satisfies? p/ISeq x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a Character"
   :added "1.0"
   :static true}
 char? (fn ^:static char? [x] (t/char? x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a String"
   :added "1.0"
   :static true}
 string? (fn ^:static string? [x] (t/string? x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IPersistentMap"
   :added "1.0"
   :static true}
 map? (fn ^:static map? [x] (satisfies? p/IMap x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IPersistentVector"
   :added "1.0"
   :static true}
 vector? (fn ^:static vector? [x] (satisfies? p/IIndexed x)))

(def
 ^{:arglists '([map key val] [map key val & kvs])
   :doc "assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be <= (count vector)."
   :added "1.0"
   :static true}
 assoc
 (fn ^:static assoc
   ([map key val] (p/-assoc map key val))
   ([map key val & kvs]
    (let [ret (p/-assoc map key val)]
      (if kvs
        (if (next kvs)
          (recur ret (first kvs) (second kvs) (nnext kvs))
          (throw (ex-info "assoc expects even number of arguments after map/vector, found odd number" {})))
        ret)))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
 ^{:arglists '([obj])
   :doc "Returns the metadata of obj, returns nil if there is no metadata."
   :added "1.0"
   :static true}
 meta (fn ^:static meta [x]
        (when (satisfies? p/IMeta x)
          (p/-meta x))))
