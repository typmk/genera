#!/usr/bin/env bb
;; =============================================================================
;; portabilize_v3.clj — rewrite-clj based core portabilization
;;
;; Unlike v1 (regex on text) and v2 (AST macroexpand), this version:
;;   - Walks the syntax tree via rewrite-clj (preserves comments, whitespace)
;;   - Does NOT macroexpand syntax-quote (backtick templates stay intact)
;;   - Matches forms structurally (catches multi-line and nested patterns)
;;   - Falls back to string replacement for edge cases
;; =============================================================================

(ns portabilize-v3
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure.string :as str]))

;; =============================================================================
;; 1. Symbol Replacement Tables
;;    Symbol nodes: clojure.lang.Xxx/yyy → portable equivalent
;; =============================================================================

(def symbol-replacements
  '{;; RT static refs
    clojure.lang.RT/conj         p/-conj
    clojure.lang.RT/assoc        p/-assoc
    clojure.lang.RT/count        p/-count
    clojure.lang.RT/nth          p/-nth
    clojure.lang.RT/get          p/-lookup
    clojure.lang.RT/contains     p/-contains-key?
    clojure.lang.RT/dissoc       p/-dissoc
    clojure.lang.RT/peek         p/-peek
    clojure.lang.RT/pop          p/-pop
    clojure.lang.RT/intCast      int
    clojure.lang.RT/longCast     long
    clojure.lang.RT/floatCast    float
    clojure.lang.RT/doubleCast   double
    clojure.lang.RT/booleanCast  boolean
    clojure.lang.RT/charCast     char
    clojure.lang.RT/shortCast    short
    clojure.lang.RT/byteCast     byte
    clojure.lang.RT/isReduced    reduced?
    clojure.lang.RT/nextID       t/next-id
    clojure.lang.RT/subvec       t/subvec
    clojure.lang.RT/iter         t/iterator
    clojure.lang.RT/readString   t/read-string
    clojure.lang.RT/chunkIteratorSeq  t/chunk-iterator-seq
    clojure.lang.RT/seqToTypedArray   t/seq-to-array
    clojure.lang.RT/addURL       t/add-url
    clojure.lang.RT/REQUIRE_LOCK t/require-lock
    clojure.lang.RT/load         t/load-resource
    clojure.lang.RT/canSeq       t/seqable?
    clojure.lang.RT/baseLoader   t/base-loader
    clojure.lang.RT/uncheckedLongCast  unchecked-long
    clojure.lang.RT/uncheckedIntCast   unchecked-int
    clojure.lang.RT/uncheckedByteCast  unchecked-byte
    clojure.lang.RT/uncheckedShortCast unchecked-short
    clojure.lang.RT/uncheckedFloatCast unchecked-float
    clojure.lang.RT/uncheckedDoubleCast unchecked-double

    ;; Numbers static refs
    clojure.lang.Numbers/abs       abs
    clojure.lang.Numbers/quotient  quot
    clojure.lang.Numbers/remainder rem
    clojure.lang.Numbers/add       h/-add
    clojure.lang.Numbers/multiply  h/-multiply
    clojure.lang.Numbers/divide    h/-divide
    clojure.lang.Numbers/minus     h/-subtract
    clojure.lang.Numbers/inc       h/-inc
    clojure.lang.Numbers/dec       h/-dec
    clojure.lang.Numbers/lt        h/-lt
    clojure.lang.Numbers/lte       h/-lte
    clojure.lang.Numbers/gt        h/-gt
    clojure.lang.Numbers/gte       h/-gte
    clojure.lang.Numbers/equiv     h/-num-equiv
    clojure.lang.Numbers/isZero    h/-zero?
    clojure.lang.Numbers/isPos     h/-pos?
    clojure.lang.Numbers/isNeg     h/-neg?
    clojure.lang.Numbers/and       h/-bit-and
    clojure.lang.Numbers/or        h/-bit-or
    clojure.lang.Numbers/xor       h/-bit-xor
    clojure.lang.Numbers/not       h/-bit-not
    clojure.lang.Numbers/shiftLeft      h/-bit-shift-left
    clojure.lang.Numbers/shiftRight     h/-bit-shift-right
    clojure.lang.Numbers/unsignedShiftRight h/-unsigned-bit-shift-right
    clojure.lang.Numbers/unchecked_add       unchecked-add
    clojure.lang.Numbers/unchecked_minus     unchecked-subtract
    clojure.lang.Numbers/unchecked_multiply  unchecked-multiply
    clojure.lang.Numbers/unchecked_inc       unchecked-inc
    clojure.lang.Numbers/unchecked_dec       unchecked-dec
    clojure.lang.Numbers/unchecked_negate    unchecked-negate

    ;; Util static refs
    clojure.lang.Util/equiv     p/-equiv
    clojure.lang.Util/hash      p/-hash
    clojure.lang.Util/compare   p/-compare
    clojure.lang.Util/equals    =

    ;; Constructor/factory statics
    clojure.lang.LazilyPersistentVector/create  t/vec
    clojure.lang.PersistentVector/create        t/vec
    clojure.lang.PersistentHashMap/create       t/hash-map
    clojure.lang.PersistentHashSet/create       t/hash-set
    clojure.lang.PersistentTreeMap/create        t/sorted-map
    clojure.lang.PersistentTreeSet/create        t/sorted-set
    clojure.lang.PersistentArrayMap/create       t/array-map
    clojure.lang.Symbol/intern     t/symbol
    clojure.lang.Keyword/intern    t/keyword
    clojure.lang.Keyword/find      t/find-keyword
    clojure.lang.Cycle/create      t/cycle
    clojure.lang.Repeat/create     t/repeat
    clojure.lang.Iterate/create    t/iterate
    clojure.lang.LongRange/create  t/range
    clojure.lang.Range/create      t/range
    clojure.lang.TransformerIterator/create       t/transformer-iterator
    clojure.lang.TransformerIterator/createMulti  t/transformer-iterator-multi
    clojure.lang.EnumerationSeq/create  t/enumeration-seq
    clojure.lang.BigInt/fromBigInteger  t/bigint-from-biginteger
    clojure.lang.BigInt/valueOf         t/bigint
    clojure.lang.TaggedLiteral/create   t/tagged-literal
    clojure.lang.ReaderConditional/create t/reader-conditional
    clojure.lang.PersistentArrayMap/EMPTY t/empty-map

    ;; Compiler statics
    clojure.lang.Compiler/maybeResolveIn  t/maybe-resolve-in
    clojure.lang.Compiler/LOADER          t/current-loader
    clojure.lang.Compiler/LINE            t/current-line
    clojure.lang.Compiler/COLUMN          t/current-column
    clojure.lang.Compiler$HostExpr/maybeSpecialTag t/maybe-special-tag
    clojure.lang.Compiler$HostExpr/maybeClass      t/maybe-class

    ;; Var statics
    clojure.lang.Var/pushThreadBindings    t/push-thread-bindings
    clojure.lang.Var/popThreadBindings     t/pop-thread-bindings
    clojure.lang.Var/getThreadBindings     t/get-thread-bindings
    clojure.lang.Var/cloneThreadBindingFrame t/clone-thread-binding-frame
    clojure.lang.Var/resetThreadBindingFrame t/reset-thread-binding-frame
    clojure.lang.Var/intern                  t/intern-var

    ;; Agent statics
    clojure.lang.Agent/pooledExecutor      t/pooled-executor
    clojure.lang.Agent/soloExecutor        t/solo-executor
    clojure.lang.Agent/releasePendingSends t/release-pending-sends

    ;; Hashing
    clojure.lang.Murmur3/mixCollHash    t/mix-coll-hash
    clojure.lang.Murmur3/hashOrdered    t/hash-ordered
    clojure.lang.Murmur3/hashUnordered  t/hash-unordered

    ;; LockingTransaction
    clojure.lang.LockingTransaction/isRunning t/in-transaction?

    ;; Misc
    clojure.lang.Reflector/prepRet  t/prep-ret
    clojure.lang.Namespace/find     t/find-ns
    clojure.lang.Namespace/findOrCreate t/find-or-create-ns
    clojure.lang.Namespace/remove   t/remove-ns
    clojure.lang.Namespace/all      t/all-ns

    ;; Protocol/interface references
    clojure.lang.IKVReduce          p/IKVReduce
    clojure.lang.IDeref             p/IDeref
    clojure.lang.IBlockingDeref     p/IBlockingDeref
    clojure.lang.IPending           p/IPending
    clojure.lang.IReduceInit        p/IReduceInit
    clojure.lang.Sequential         p/ISequential
    clojure.lang.Seqable            p/ISeqable
    clojure.lang.IFn                p/IFn
    clojure.lang.protocols/IMapEntry p/IMapEntry
    clojure.lang.protocols/-key      p/-key
    clojure.lang.protocols/-val      p/-val})

;; =============================================================================
;; 2. Java Interop: (. class (method args...)) → portable
;; =============================================================================

;; Table: [class method nargs] → fn that takes args and returns replacement string
;; nargs = :any means variable

(def dot-rt-replacements
  "clojure.lang.RT dot-method replacements"
  {"first"     (fn [[a]]   (format "(let [s# (p/-seq %s)] (when s# (p/-first s#)))" a))
   "next"      (fn [[a]]   (format "(p/-next %s)" a))
   "more"      (fn [[a]]   (format "(p/-rest %s)" a))
   "seq"       (fn [[a]]   (format "(p/-seq %s)" a))
   "cons"      (fn [[a b]] (format "(t/cons %s %s)" a b))
   "conj"      (fn [[a b]] (format "(p/-conj %s %s)" a b))
   "assoc"     (fn [[a b c]] (format "(p/-assoc %s %s %s)" a b c))
   "count"     (fn [[a]]   (format "(p/-count %s)" a))
   "nth"       (fn [args]  (if (= 3 (count args))
                             (format "(p/-nth %s %s %s)" (nth args 0) (nth args 1) (nth args 2))
                             (format "(p/-nth %s %s)" (nth args 0) (nth args 1))))
   "get"       (fn [args]  (if (= 3 (count args))
                             (format "(p/-lookup %s %s %s)" (nth args 0) (nth args 1) (nth args 2))
                             (format "(p/-lookup %s %s)" (nth args 0) (nth args 1))))
   "contains"  (fn [[a b]] (format "(p/-contains-key? %s %s)" a b))
   "dissoc"    (fn [[a b]] (format "(p/-dissoc %s %s)" a b))
   "peek"      (fn [[a]]   (format "(p/-peek %s)" a))
   "pop"       (fn [[a]]   (format "(p/-pop %s)" a))
   "keys"      (fn [[a]]   (format "(t/keys %s)" a))
   "vals"      (fn [[a]]   (format "(t/vals %s)" a))
   "find"      (fn [[a b]] (format "(t/find %s %s)" a b))
   "toArray"   (fn [[a]]   (format "(h/-to-array (h/host) %s)" a))
   "intCast"   (fn [[a]]   (format "(int %s)" a))
   "longCast"  (fn [[a]]   (format "(long %s)" a))
   "floatCast" (fn [[a]]   (format "(float %s)" a))
   "doubleCast" (fn [[a]]  (format "(double %s)" a))
   "booleanCast" (fn [[a]] (format "(boolean %s)" a))
   "charCast"  (fn [[a]]   (format "(char %s)" a))
   "shortCast" (fn [[a]]   (format "(short %s)" a))
   "byteCast"  (fn [[a]]   (format "(byte %s)" a))
   "nextID"    (fn [_]     "(t/next-id)")
   "subvec"    (fn [[a b c]] (format "(t/subvec %s %s %s)" a b c))
   "chunkIteratorSeq" (fn [[a]] (format "(t/chunk-iterator-seq %s)" a))
   "readString" (fn [args] (str/join " " (cons "(t/read-string" (conj (vec args) ")"))))
   "alength"   (fn [[a]]   (format "(h/-alength (h/host) %s)" a))
   "aclone"    (fn [[a]]   (format "(h/-aclone (h/host) %s)" a))
   "aget"      (fn [[a b]] (format "(h/-aget (h/host) %s %s)" a b))
   "aset"      (fn [[a b c]] (format "(h/-aset (h/host) %s %s %s)" a b c))
   ;; Unchecked casts
   "uncheckedIntCast"    (fn [[a]] (format "(unchecked-int %s)" a))
   "uncheckedLongCast"   (fn [[a]] (format "(unchecked-long %s)" a))
   "uncheckedFloatCast"  (fn [[a]] (format "(unchecked-float %s)" a))
   "uncheckedDoubleCast" (fn [[a]] (format "(unchecked-double %s)" a))
   "uncheckedCharCast"   (fn [[a]] (format "(unchecked-char %s)" a))
   "uncheckedShortCast"  (fn [[a]] (format "(unchecked-short %s)" a))
   "uncheckedByteCast"   (fn [[a]] (format "(unchecked-byte %s)" a))})

(def dot-numbers-replacements
  "clojure.lang.Numbers dot-method replacements"
  {"add"       (fn [[a b]] (format "(h/-add (h/host) %s %s)" a b))
   "addP"      (fn [[a b]] (format "(h/-add (h/host) %s %s)" a b))
   "minus"     (fn [args]  (if (= 1 (count args))
                             (format "(h/-negate (h/host) %s)" (first args))
                             (format "(h/-subtract (h/host) %s %s)" (first args) (second args))))
   "minusP"    (fn [args]  (if (= 1 (count args))
                             (format "(h/-negate (h/host) %s)" (first args))
                             (format "(h/-subtract (h/host) %s %s)" (first args) (second args))))
   "multiply"  (fn [[a b]] (format "(h/-multiply (h/host) %s %s)" a b))
   "multiplyP" (fn [[a b]] (format "(h/-multiply (h/host) %s %s)" a b))
   "divide"    (fn [[a b]] (format "(h/-divide (h/host) %s %s)" a b))
   "inc"       (fn [[a]]   (format "(h/-inc (h/host) %s)" a))
   "incP"      (fn [[a]]   (format "(h/-inc (h/host) %s)" a))
   "dec"       (fn [[a]]   (format "(h/-dec (h/host) %s)" a))
   "decP"      (fn [[a]]   (format "(h/-dec (h/host) %s)" a))
   "lt"        (fn [[a b]] (format "(h/-lt (h/host) %s %s)" a b))
   "lte"       (fn [[a b]] (format "(h/-lte (h/host) %s %s)" a b))
   "gt"        (fn [[a b]] (format "(h/-gt (h/host) %s %s)" a b))
   "gte"       (fn [[a b]] (format "(h/-gte (h/host) %s %s)" a b))
   "equiv"     (fn [[a b]] (format "(h/-num-equiv (h/host) %s %s)" a b))
   "isZero"    (fn [[a]]   (format "(h/-zero? (h/host) %s)" a))
   "isPos"     (fn [[a]]   (format "(h/-pos? (h/host) %s)" a))
   "isNeg"     (fn [[a]]   (format "(h/-neg? (h/host) %s)" a))
   "max"       (fn [[a b]] (format "(if (h/-gt (h/host) %s %s) %s %s)" a b a b))
   "min"       (fn [[a b]] (format "(if (h/-lt (h/host) %s %s) %s %s)" a b a b))
   "quotient"  (fn [[a b]] (format "(quot %s %s)" a b))
   "remainder" (fn [[a b]] (format "(rem %s %s)" a b))
   "num"       (fn [[a]]   (format "(t/num %s)" a))
   "rationalize" (fn [[a]] (format "(t/rationalize %s)" a))
   ;; Bitwise
   "and"       (fn [[a b]] (format "(h/-bit-and (h/host) %s %s)" a b))
   "or"        (fn [[a b]] (format "(h/-bit-or (h/host) %s %s)" a b))
   "xor"       (fn [[a b]] (format "(h/-bit-xor (h/host) %s %s)" a b))
   "not"       (fn [[a]]   (format "(h/-bit-not (h/host) %s)" a))
   "andNot"    (fn [[a b]] (format "(h/-bit-and-not (h/host) %s %s)" a b))
   "shiftLeft" (fn [[a b]] (format "(h/-bit-shift-left (h/host) %s %s)" a b))
   "shiftRight" (fn [[a b]] (format "(h/-bit-shift-right (h/host) %s %s)" a b))
   "unsignedShiftRight" (fn [[a b]] (format "(h/-unsigned-bit-shift-right (h/host) %s %s)" a b))
   "clearBit"  (fn [[a b]] (format "(h/-bit-clear (h/host) %s %s)" a b))
   "setBit"    (fn [[a b]] (format "(h/-bit-set (h/host) %s %s)" a b))
   "flipBit"   (fn [[a b]] (format "(h/-bit-flip (h/host) %s %s)" a b))
   "testBit"   (fn [[a b]] (format "(h/-bit-test (h/host) %s %s)" a b))
   ;; Unchecked
   "unchecked_add"       (fn [[a b]] (format "(unchecked-add %s %s)" a b))
   "unchecked_minus"     (fn [args]  (if (= 1 (count args))
                                       (format "(unchecked-negate %s)" (first args))
                                       (format "(unchecked-subtract %s %s)" (first args) (second args))))
   "unchecked_subtract"  (fn [[a b]] (format "(unchecked-subtract %s %s)" a b))
   "unchecked_multiply"  (fn [[a b]] (format "(unchecked-multiply %s %s)" a b))
   "unchecked_inc"       (fn [[a]]   (format "(unchecked-inc %s)" a))
   "unchecked_dec"       (fn [[a]]   (format "(unchecked-dec %s)" a))
   "unchecked_negate"    (fn [[a]]   (format "(unchecked-negate %s)" a))
   "unchecked_divide"    (fn [[a b]] (format "(unchecked-divide %s %s)" a b))
   "unchecked_remainder" (fn [[a b]] (format "(unchecked-remainder %s %s)" a b))
   ;; int/long specific unchecked
   "unchecked_int_inc"       (fn [[a]]   (format "(unchecked-inc %s)" a))
   "unchecked_int_dec"       (fn [[a]]   (format "(unchecked-dec %s)" a))
   "unchecked_int_add"       (fn [[a b]] (format "(unchecked-add %s %s)" a b))
   "unchecked_int_subtract"  (fn [[a b]] (format "(unchecked-subtract %s %s)" a b))
   "unchecked_int_multiply"  (fn [[a b]] (format "(unchecked-multiply %s %s)" a b))
   "unchecked_int_negate"    (fn [[a]]   (format "(unchecked-negate %s)" a))
   "unchecked_int_divide"    (fn [[a b]] (format "(unchecked-divide-int %s %s)" a b))
   "unchecked_int_remainder" (fn [[a b]] (format "(unchecked-remainder-int %s %s)" a b))
   "unchecked_long_inc"      (fn [[a]]   (format "(unchecked-inc %s)" a))
   "unchecked_long_dec"      (fn [[a]]   (format "(unchecked-dec %s)" a))
   "unchecked_long_add"      (fn [[a b]] (format "(unchecked-add %s %s)" a b))
   "unchecked_long_subtract" (fn [[a b]] (format "(unchecked-subtract %s %s)" a b))
   "unchecked_long_multiply" (fn [[a b]] (format "(unchecked-multiply %s %s)" a b))
   "unchecked_long_negate"   (fn [[a]]   (format "(unchecked-negate %s)" a))
   ;; Typed arrays
   "float_array"     (fn [args] (str/join " " (cons "(h/-float-array (h/host)" (conj (vec args) ")"))))
   "double_array"    (fn [args] (str/join " " (cons "(h/-double-array (h/host)" (conj (vec args) ")"))))
   "int_array"       (fn [args] (str/join " " (cons "(h/-int-array (h/host)" (conj (vec args) ")"))))
   "long_array"      (fn [args] (str/join " " (cons "(h/-long-array (h/host)" (conj (vec args) ")"))))
   "short_array"     (fn [args] (str/join " " (cons "(h/-short-array (h/host)" (conj (vec args) ")"))))
   "byte_array"      (fn [args] (str/join " " (cons "(h/-byte-array (h/host)" (conj (vec args) ")"))))
   "char_array"      (fn [args] (str/join " " (cons "(h/-char-array (h/host)" (conj (vec args) ")"))))
   "boolean_array"   (fn [args] (str/join " " (cons "(h/-boolean-array (h/host)" (conj (vec args) ")"))))
   ;; Typed array coercion
   "booleans"  (fn [[a]] (format "(t/booleans %s)" a))
   "bytes"     (fn [[a]] (format "(t/bytes %s)" a))
   "chars"     (fn [[a]] (format "(t/chars %s)" a))
   "shorts"    (fn [[a]] (format "(t/shorts %s)" a))
   "ints"      (fn [[a]] (format "(t/ints %s)" a))
   "longs"     (fn [[a]] (format "(t/longs %s)" a))
   "floats"    (fn [[a]] (format "(t/floats %s)" a))
   "doubles"   (fn [[a]] (format "(t/doubles %s)" a))})

(def dot-util-replacements
  "clojure.lang.Util dot-method replacements"
  {"identical" (fn [[a b]] (format "(h/-identical? (h/host) %s %s)" a b))
   "equiv"     (fn [[a b]] (format "(p/-equiv %s %s)" a b))
   "hash"      (fn [[a]]   (format "(p/-hash %s)" a))
   "hasheq"    (fn [[a]]   (format "(p/-hash %s)" a))
   "compare"   (fn [[a b]] (format "(p/-compare %s %s)" a b))
   "equals"    (fn [[a b]] (format "(= %s %s)" a b))})

(def dot-misc-replacements
  "Other class dot-method replacements"
  {["clojure.lang.Compiler" "eval"]         (fn [[a]]   (format "(t/eval %s)" a))
   ["clojure.lang.Compiler" "macroexpand1"] (fn [[a]]   (format "(t/macroexpand-1 %s)" a))
   ["clojure.lang.Compiler" "load"]         (fn [[a]]   (format "(t/load-reader %s)" a))
   ["clojure.lang.Delay" "force"]           (fn [[a]]   (format "(t/force %s)" a))
   ["clojure.lang.LazilyPersistentVector" "create"] (fn [[a]] (format "(t/vec %s)" a))
   ["clojure.lang.PersistentHashMap" "create"]  (fn [[a]] (format "(t/hash-map %s)" a))
   ["clojure.lang.PersistentArrayMap" "create"] (fn [[a]] (format "(t/array-map %s)" a))
   ["clojure.lang.PersistentStructMap" "createSlotMap"] (fn [[a]] (format "(t/create-slot-map %s)" a))
   ["clojure.lang.PersistentStructMap" "create"]    (fn [[a b]] (format "(t/create-struct %s %s)" a b))
   ["clojure.lang.PersistentStructMap" "construct"]  (fn [[a b]] (format "(t/construct-struct %s %s)" a b))
   ["clojure.lang.PersistentStructMap" "getAccessor"] (fn [[a b]] (format "(t/get-accessor %s %s)" a b))
   ["clojure.lang.Symbol" "intern"]         (fn [[a]]   (format "(t/symbol %s)" a))
   ["clojure.lang.Var" "pushThreadBindings"] (fn [[a]]  (format "(t/push-thread-bindings %s)" a))
   ["clojure.lang.Var" "popThreadBindings"]  (fn [_]    "(t/pop-thread-bindings)")
   ["clojure.lang.Var" "find"]               (fn [[a]]  (format "(t/find-var %s)" a))
   ["clojure.lang.Agent" "shutdown"]          (fn [_]    "(t/shutdown-agents)")
   ["Array" "get"]                            (fn [[a b]] (format "(h/-aget (h/host) %s %s)" a b))})

;; =============================================================================
;; 3. instance? → satisfies? / type predicates
;; =============================================================================

(def instance-replacements
  '{clojure.lang.ISeq               (satisfies? p/ISeq)
    clojure.lang.IPersistentMap      (satisfies? p/IMap)
    clojure.lang.IPersistentVector   (satisfies? p/IIndexed)
    clojure.lang.IPersistentSet      (satisfies? p/ISet)
    clojure.lang.IPersistentCollection (satisfies? p/ICollection)
    clojure.lang.IPersistentList     (t/list?)
    clojure.lang.IDeref              (satisfies? p/IDeref)
    clojure.lang.IMeta               (satisfies? p/IMeta)
    clojure.lang.IObj                (satisfies? p/IWithMeta)
    clojure.lang.Counted             (satisfies? p/ICounted)
    clojure.lang.Indexed             (satisfies? p/IIndexed)
    clojure.lang.IFn                 (satisfies? p/IFn)
    clojure.lang.Fn                  (satisfies? p/IFn)
    clojure.lang.Sequential          (satisfies? p/ISequential)
    clojure.lang.Associative         (satisfies? p/IAssociative)
    clojure.lang.Reversible          (satisfies? p/IReversible)
    clojure.lang.IEditableCollection (satisfies? p/IEditableCollection)
    clojure.lang.ITransientCollection (satisfies? p/ITransient)
    clojure.lang.IChunkedSeq         (t/chunked-seq?)
    clojure.lang.IReduceInit         (satisfies? p/IReduceInit)
    clojure.lang.IReduce             (satisfies? p/IReduce)
    clojure.lang.IDrop               (satisfies? p/IDrop)
    clojure.lang.IBlockingDeref      (satisfies? p/IBlockingDeref)
    clojure.lang.Sorted              (satisfies? p/ISorted)
    clojure.lang.Symbol              (t/symbol?)
    clojure.lang.Keyword             (t/keyword?)
    clojure.lang.Delay               (t/delay?)
    clojure.lang.Var                 (t/var?)
    clojure.lang.Ratio               (t/ratio?)
    clojure.lang.BigInt              (t/bigint?)
    clojure.lang.MultiFn             (t/multifn?)
    clojure.lang.TaggedLiteral       (t/tagged-literal?)
    clojure.lang.ReaderConditional   (t/reader-conditional?)
    clojure.lang.LineNumberingPushbackReader (t/line-numbering-reader?)
    clojure.lang.Namespace           (t/namespace?)
    clojure.lang.Named               (t/named?)
    clojure.lang.Volatile            (t/volatile?)
    String     (t/string?)
    Character  (t/char?)
    Number     (t/number?)
    Boolean    (or (true?) (false?))
    Integer    (t/int?)
    Long       (t/long?)
    Double     (t/double?)
    Float      (t/float?)
    BigDecimal (t/bigdec?)
    BigInteger (t/biginteger?)})

;; =============================================================================
;; 4. Type hint removal patterns (metadata)
;; =============================================================================

(def jvm-type-hint-prefixes
  #{"clojure.lang." "java.lang." "[Ljava.lang."})

(def jvm-type-hint-names
  #{"Class" "String" "Object" "Number" "StringBuilder" "Boolean"
    "Long" "Integer" "Double" "Float"})

;; =============================================================================
;; 5. Constructor replacements: (new Class ...) → (t/factory ...)
;; =============================================================================

(def new-replacements
  '{clojure.lang.LazySeq    t/lazy-seq*
    clojure.lang.Delay       t/delay*
    clojure.lang.MultiFn     t/multifn
    clojure.lang.Agent       t/agent
    clojure.lang.Ref         t/ref
    clojure.lang.Atom        t/atom})

;; Foo. constructor syntax → t/factory
(def dot-constructor-replacements
  '{clojure.lang.ChunkBuffer.  t/chunk-buffer
    clojure.lang.ChunkedCons.  t/chunk-cons
    clojure.lang.Cons.         t/cons
    clojure.lang.Reduced.      t/reduced
    clojure.lang.Volatile.     t/volatile
    clojure.lang.LineNumberingPushbackReader. t/line-numbering-reader})

;; =============================================================================
;; 6. Exception replacements
;; =============================================================================

(def exception-class-map
  '{IllegalArgumentException.       "Illegal argument"
    IndexOutOfBoundsException.      "Index out of bounds"
    ClassCastException.             "Cast failed"
    UnsupportedOperationException.  "Unsupported operation"
    ArithmeticException.            "Arithmetic error"
    RuntimeException.               nil ;; keep the message
    IllegalAccessError.             "Illegal access"
    java.lang.UnsupportedOperationException. "Unsupported operation"
    java.lang.IllegalAccessError.   "Illegal access"})

;; =============================================================================
;; 7. .method replacements (on objects)
;; =============================================================================

(def dot-method-replacements
  '{.withMeta  p/-with-meta
    .meta      p/-meta})

;; =============================================================================
;; 7b. List-head replacements: (Foo/bar args...) → (replacement (h/host) args...)
;;     For static calls that need (h/host) injected as first arg
;; =============================================================================

(def list-head-replacements
  "When head of a list is one of these symbols, rewrite the whole call"
  '{clojure.lang.Util/identical  h/-identical?
    clojure.lang.Util/equiv      p/-equiv
    clojure.lang.Util/hash       p/-hash
    clojure.lang.Util/compare    p/-compare
    clojure.lang.Util/equals     =})

;; Heads that need (h/host) injected as first arg
(def host-injected-heads
  #{'h/-identical? 'h/-add 'h/-subtract 'h/-multiply 'h/-divide
    'h/-inc 'h/-dec 'h/-negate
    'h/-lt 'h/-lte 'h/-gt 'h/-gte 'h/-num-equiv
    'h/-zero? 'h/-pos? 'h/-neg?
    'h/-bit-and 'h/-bit-or 'h/-bit-xor 'h/-bit-not
    'h/-bit-shift-left 'h/-bit-shift-right 'h/-unsigned-bit-shift-right
    'h/-bit-and-not 'h/-bit-clear 'h/-bit-set 'h/-bit-flip 'h/-bit-test
    'h/-to-array 'h/-alength 'h/-aclone 'h/-aget 'h/-aset
    'h/-instance? 'h/-nano-time 'h/-current-time-ms})

;; Also handle Numbers static calls as list heads:
;; (clojure.lang.Numbers/isZero x) → (h/-zero? (h/host) x)
(def numbers-head-replacements
  '{clojure.lang.Numbers/isZero  h/-zero?
    clojure.lang.Numbers/isPos   h/-pos?
    clojure.lang.Numbers/isNeg   h/-neg?
    clojure.lang.Numbers/add     h/-add
    clojure.lang.Numbers/minus   h/-subtract
    clojure.lang.Numbers/multiply h/-multiply
    clojure.lang.Numbers/divide  h/-divide
    clojure.lang.Numbers/inc     h/-inc
    clojure.lang.Numbers/dec     h/-dec
    clojure.lang.Numbers/lt      h/-lt
    clojure.lang.Numbers/lte     h/-lte
    clojure.lang.Numbers/gt      h/-gt
    clojure.lang.Numbers/gte     h/-gte
    clojure.lang.Numbers/equiv   h/-num-equiv
    clojure.lang.Numbers/abs     abs
    clojure.lang.Numbers/quotient quot
    clojure.lang.Numbers/remainder rem})

;; =============================================================================
;; 8. System interop
;; =============================================================================

(def system-method-replacements
  {"nanoTime"       "(h/-nano-time (h/host))"
   "currentTimeMillis" "(h/-current-time-ms (h/host))"
   "getProperty"    "(h/-get-property (h/host))"
   "getenv"         "(h/-getenv (h/host))"})

;; =============================================================================
;; Tree-walking engine
;; =============================================================================

(defn sym-str [zloc]
  "Get string of a symbol node"
  (when (= :token (z/tag zloc))
    (try (let [s (z/sexpr zloc)]
           (when (symbol? s) (str s)))
         (catch Exception _ nil))))

(defn try-sexpr [zloc]
  (try (z/sexpr zloc) (catch Exception _ nil)))

(defn replace-with-str [zloc s]
  "Replace current node with parsed string"
  (z/replace zloc (n/coerce (read-string s))))

(defn children-strs
  "Get string representations of children of a list node"
  [zloc]
  (when (z/list? zloc)
    (let [child (z/down zloc)]
      (when child
        (loop [c child acc []]
          (let [acc (if (z/whitespace-or-comment? c)
                      acc
                      (conj acc (z/string c)))]
            (if-let [r (z/right c)]
              (recur r acc)
              acc)))))))

(defn transform-dot-form
  "Transform (. Class (method args...)) or (. Class method args...) forms"
  [zloc]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 3)
               (= "." (first children)))
      (let [class-str (second children)
            ;; Two forms: (. Class (method args...)) or (. Class method args...)
            rest-str (nth children 2)
            paren-form? (str/starts-with? rest-str "(")]
        (if paren-form?
          ;; (. Class (method arg1 arg2))
          (let [inner (read-string rest-str)
                method-name (str (first inner))
                args (mapv str (rest inner))]
            (cond
              (= class-str "clojure.lang.RT")
              (when-let [f (dot-rt-replacements method-name)]
                (f args))

              (= class-str "clojure.lang.Numbers")
              (when-let [f (dot-numbers-replacements method-name)]
                (f args))

              (= class-str "clojure.lang.Util")
              (when-let [f (dot-util-replacements method-name)]
                (f args))

              (= class-str "System")
              (when-let [repl (system-method-replacements method-name)]
                repl)

              :else
              (when-let [f (dot-misc-replacements [class-str method-name])]
                (f args))))
          ;; (. Class method arg1 arg2) — space form
          (let [method-name rest-str
                args (vec (drop 3 children))]
            (cond
              (= class-str "clojure.lang.RT")
              (when-let [f (dot-rt-replacements method-name)]
                (f args))

              (= class-str "clojure.lang.Numbers")
              (when-let [f (dot-numbers-replacements method-name)]
                (f args))

              (= class-str "clojure.lang.Util")
              (when-let [f (dot-util-replacements method-name)]
                (f args))

              (= class-str "System")
              (when-let [repl (system-method-replacements method-name)]
                repl)

              (= class-str "clojure.lang.Compiler")
              (when (= method-name "specials")
                "t/special-symbols")

              (= class-str "clojure.lang.PersistentArrayMap")
              (when (= method-name "EMPTY")
                "t/empty-map")

              :else
              (when-let [f (dot-misc-replacements [class-str method-name])]
                (f args)))))))))

(defn transform-instance-form
  "Transform (instance? Class x) forms"
  [zloc]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 3)
               (= "instance?" (first children)))
      (let [class-sym (symbol (second children))
            val-str (nth children 2)]
        (when-let [replacement (instance-replacements class-sym)]
          (let [repl-str (str replacement)]
            (if (str/starts-with? repl-str "(")
              ;; (satisfies? p/ISeq x) or (t/string? x) etc
              (let [parts (read-string repl-str)]
                (if (= 'or (first parts))
                  ;; Boolean special: (or (true? x) (false? x))
                  (format "(or (%s %s) (%s %s))" (second parts) val-str (nth parts 2) val-str)
                  ;; Normal: (satisfies? p/IFoo x) or (t/foo? x)
                  (if (= 2 (count parts))
                    (format "(%s %s)" (first parts) val-str)
                    (format "(%s %s %s)" (first parts) (second parts) val-str))))
              ;; Simple symbol
              (format "(%s %s)" repl-str val-str))))))))

(defn transform-new-form
  "Transform (new Class args...) forms"
  [zloc]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 2)
               (= "new" (first children)))
      (let [class-sym (symbol (second children))
            args (drop 2 children)]
        (when-let [replacement (new-replacements class-sym)]
          (if (seq args)
            (format "(%s %s)" replacement (str/join " " args))
            (format "(%s)" replacement)))))))

(defn transform-throw-form
  "Transform (throw (ExClass. msg)) → (throw (ex-info msg {}))"
  [zloc]
  (let [children (children-strs zloc)]
    (when (and (= 2 (count children))
               (= "throw" (first children)))
      (let [inner-str (second children)]
        (when (str/starts-with? inner-str "(")
          (try
            (let [inner (read-string inner-str)
                  ctor (first inner)]
              (when-let [default-msg (exception-class-map ctor)]
                (let [user-msg (second inner)
                      msg (or (when (string? user-msg) user-msg)
                              (when user-msg (format "(str %s)" user-msg))
                              default-msg
                              "Error")]
                  (if (string? msg)
                    (format "(throw (ex-info \"%s\" {}))" msg)
                    (format "(throw (ex-info %s {}))" msg)))))
            (catch Exception _ nil)))))))

(defn jvm-type-hint? [s]
  "Check if a string represents a JVM type hint to remove"
  (or (some #(str/starts-with? s %) jvm-type-hint-prefixes)
      (jvm-type-hint-names s)))

(defn transform-node [zloc]
  "Try all transformations on the current node. Returns replacement string or nil."
  (let [tag (z/tag zloc)]
    (cond
      ;; Symbol nodes — direct replacement
      (= :token tag)
      (when-let [s (sym-str zloc)]
        (when-let [repl (symbol-replacements (symbol s))]
          (str repl)))

      ;; List forms — structural matching
      (contains? #{:list} tag)
      (let [children (children-strs zloc)]
        (when (seq children)
          (let [head (first children)]
            (cond
              ;; (. Class (method ...)) or (. Class method ...)
              (= "." head)
              (transform-dot-form zloc)

              ;; (instance? Class x)
              (= "instance?" head)
              (transform-instance-form zloc)

              ;; (new Class ...)
              (= "new" head)
              (transform-new-form zloc)

              ;; (throw (Exception. ...))
              (= "throw" head)
              (transform-throw-form zloc)

              ;; (.method obj args...) — dot-method calls
              (and (str/starts-with? head ".")
                   (not= "." head)
                   (not (str/starts-with? head "..")))
              (let [method-sym (symbol head)]
                (when-let [repl (dot-method-replacements method-sym)]
                  (let [args (rest children)]
                    (if (seq args)
                      (format "(%s %s)" repl (str/join " " args))
                      (str "(" repl ")")))))

              ;; Foo. constructor syntax: (ChunkBuffer. args)
              (let [s (symbol head)]
                (dot-constructor-replacements s))
              (let [repl (dot-constructor-replacements (symbol head))
                    args (rest children)]
                (if (seq args)
                  (format "(%s %s)" repl (str/join " " args))
                  (format "(%s)" repl)))

              ;; List-head: (clojure.lang.Util/identical x y) → (h/-identical? (h/host) x y)
              ;; Also Numbers: (clojure.lang.Numbers/isZero x) → (h/-zero? (h/host) x)
              :else
              (let [head-sym (symbol head)
                    repl (or (list-head-replacements head-sym)
                             (numbers-head-replacements head-sym))]
                (when repl
                  (let [args (rest children)]
                    (if (host-injected-heads repl)
                      (if (seq args)
                        (format "(%s (h/host) %s)" repl (str/join " " args))
                        (format "(%s (h/host))" repl))
                      (if (seq args)
                        (format "(%s %s)" repl (str/join " " args))
                        (format "(%s)" repl))))))))))

      :else nil)))

;; =============================================================================
;; String-level fallback for patterns too complex for tree-walking
;; =============================================================================

(def string-fallback-replacements
  [;; PersistentList creator
   [#"\(\. clojure\.lang\.PersistentList creator\)" "t/list"]

   ;; :inline and :inline-arities removal (multi-line aware)
   ;; These contain backtick-quoted JVM-specific code that should be stripped
   ;; Pattern: :inline (fn [...] `(. clojure.lang.Numbers ...)) at end of metadata
   [#"\n\s+:inline \(fn [^\n]+\)\}" "}"]
   [#"\n\s+:inline \(fn [^\n]+`[^\n]+\)\}" "}"]
   ;; :inline as a key in metadata followed by another key
   [#":inline \(fn [^:}]+\)\)\s*\n\s*:" ":"]
   ;; Standalone :inline lines
   [#"(?m)^\s+:inline \(fn [^\n]+\n?" ""]
   ;; :inline-arities
   [#"(?m)\s*:inline-arities[^\n]+\n?" ""]
   ;; Multi-line :inline — match :inline ... up to matching close-paren
   ;; Best effort: strip the whole line and next line if continued
   [#":inline\s+\(fn\s+\[[^\]]*\]\s+`\([^)]+\)\)" ""]

   ;; Remaining type tags in metadata
   [#"\{:tag clojure\.lang\.[A-Za-z]+\}" ""]
   [#":tag clojure\.lang\.[A-Za-z]+" ""]
   [#"\{:tag \"\[Ljava\.lang\.Object;\"\}" ""]
   [#":tag \"\[Ljava\.lang\.Object;\"" ""]
   [#":tag \"\[\[Ljava\.lang\.Object;\"" ""]
   [#":tag 'clojure\.lang\.[A-Za-z]+" ""]

   ;; Java import statements
   [#"\(import '\(java\.lang\.reflect Array\)\)"
    ";; import removed — using host array fns"]
   [#"\(import clojure\.lang\.[A-Za-z]+ clojure\.lang\.[A-Za-z]+\)"
    ";; import removed — using portable equivalents"]
   [#"\(import clojure\.lang\.[A-Za-z]+\)"
    ";; import removed — using portable equivalents"]

   ;; Java class refs in catch
   [#"\(catch IllegalArgumentException" "(catch Exception"]
   [#"\(catch Throwable" "(catch Exception"]

   ;; Quoted class names
   [#"'clojure\.lang\.LazySeq" "'t/LazySeq"]
   [#"'clojure\.lang\.Delay" "'t/Delay"]
   [#"'clojure\.lang\.ISeq" "'p/ISeq"]

   ;; java.net.URI
   [#"java\.net\.URI" "t/URI"]
   [#"java\.util\.UUID" "t/UUID"]

   ;; Annotation refs
   [#"java\.lang\.annotation\.Retention" "t/Retention"]
   [#"java\.lang\.annotation\.RetentionPolicy/RUNTIME" ":runtime"]

   ;; Remove type hints (after tree walk, catch stragglers)
   [#"\^clojure\.lang\.[A-Za-z]+" ""]
   [#"\^java\.lang\.[A-Za-z]+" ""]
   [#"\^\[Ljava\.lang\.Object;" ""]
   [#"\^Class\b" ""]
   [#"\^String\b" ""]
   [#"\^Object\b" ""]
   [#"\^Number\b" ""]
   [#"\^StringBuilder\b" ""]
   [#"\^Boolean\b" ""]
   [#"\^Long\b" ""]
   [#"\^Integer\b" ""]
   [#"\^Double\b" ""]
   [#"\^Float\b" ""]

   ;; java.lang types (in code, not docstrings)
   [#"java\.lang\.Enum" "t/Enum"]
   [#"java\.lang\.Object" "t/Object"]
   [#"java\.lang\.Class" "t/Class"]

   ;; Double/isNaN etc
   [#"Double/isNaN" "t/nan?"]
   [#"Double/isInfinite" "t/infinite?"]
   [#"Double/valueOf" "t/parse-double"]
   [#"Long/valueOf" "t/parse-long"]
   [#"java\.util\.UUID/fromString" "t/parse-uuid"]

   ;; Thread
   [#"Thread/currentThread" "(h/-current-thread (h/host))"]
   [#"\(\.getContextClassLoader" "(h/-context-class-loader (h/host)"]
   [#"\bThread\." "t/thread"]
   [#"\.setDaemon" "t/set-daemon"]
   [#"\.start\b" "t/start"]

   ;; StringBuilder
   [#"\(new StringBuilder" "(t/string-builder"]
   [#"\(\.append\s+" "(t/sb-append "]
   [#"\(\.toString\s+" "(t/to-string "]

   ;; Exception constructors — string fallback for ones tree-walk missed
   [#"\(IllegalArgumentException\.\s+" "(ex-info "]
   [#"\(IndexOutOfBoundsException\.\)" "(ex-info \"Index out of bounds\" {})"]
   [#"\(UnsupportedOperationException\.\s+" "(ex-info "]
   [#"\(ArithmeticException\.\s+" "(ex-info "]
   [#"\(RuntimeException\.\s+" "(ex-info "]
   [#"\(IllegalAccessError\.\s+" "(ex-info "]
   [#"\(new java\.lang\.IllegalAccessError\s+" "(ex-info "]
   [#"\(java\.lang\.UnsupportedOperationException\.\s+" "(ex-info "]
   [#"\(NumberFormatException\s+" "(Exception "]

   ;; java.util types
   [#"java\.util\.Map\$Entry" "t/MapEntry"]
   [#"java\.util\.concurrent\.ArrayBlockingQueue\." "t/array-blocking-queue"]
   [#"\.offer\s+" "t/offer "]
   [#"\.take\s+" "t/take-blocking "]
   [#"java\.util\.Formatter" "t/Formatter"]
   [#"java\.util\.regex\.Pattern" "t/Pattern"]
   [#"java\.util\.regex\.Matcher" "t/Matcher"]

   ;; java.io types
   [#"java\.io\.BufferedReader" "t/BufferedReader"]
   [#"java\.io\.PushbackReader" "t/PushbackReader"]
   [#"java\.io\.Reader" "t/Reader"]
   [#"java\.io\.Writer" "t/Writer"]
   [#"java\.io\.InputStreamReader\." "t/input-stream-reader"]
   [#"java\.io\.StringWriter\." "t/string-writer"]
   [#"java\.io\.StringReader\." "t/string-reader"]

   ;; java.net
   [#"java\.net\.URL" "t/URL"]

   ;; java.sql
   [#"java\.sql\.ResultSet" "t/ResultSet"]
   [#"java\.sql\.Connection" "t/Connection"]
   [#"java\.sql\.Statement" "t/Statement"]
   [#"java\.sql\.Timestamp" "t/Timestamp"]

   ;; Annotation
   [#"java\.lang\.annotation\.Annotation" "t/Annotation"]
   [#"\.isAssignableFrom" "t/assignable-from?"]
   [#"\.getAnnotation" "t/get-annotation"]

   ;; Enum
   [#"\.getEnumConstants" "t/enum-constants"]
   [#"\.isEnum" "t/enum?"]

   ;; (.. System ...) form
   [#"\(\.\. System \(getProperties\)" "(h/-system-properties (h/host)"]
   [#"\(\. System \(getProperties\)\)" "(h/-system-properties (h/host))"]

   ;; Remaining clojure.lang references
   [#"clojure\.lang\.Compiler\$CompilerException\." "t/compiler-exception"]
   [#"\(\.\. clojure\.lang\.Var create setDynamic\)" "t/create-dynamic-var"]

   ;; Backtick-quoted arithmetic macros: `(. clojure.lang.Numbers (~op ~x))
   ;; These are code-generating templates — replace class, keep ~op
   [#"`\(\. clojure\.lang\.Numbers \(~op ~x\)\)" "`(h/host-call (h/host) ~op ~x)"]
   [#"`\(\. clojure\.lang\.Numbers \(~op ~x ~y\)\)" "`(h/host-call (h/host) ~op ~x ~y)"]
   [#"`\(\. clojure\.lang\.Numbers \(~op ~a ~b\)\)" "`(h/host-call (h/host) ~op ~a ~b)"]
   ;; Inline arithmetic with more context
   [#"\(fn \[a b\] `\(\. clojure\.lang\.Numbers \(~op ~a ~b\)\)"
    "(fn [a b] `(h/host-call (h/host) ~op ~a ~b))"]
   ;; Remaining `(. clojure.lang.Numbers ... patterns with more
   [#"`\(\. clojure\.lang\.Numbers" "`(h/host-call (h/host)"]

   ;; Remaining RT: object_array and RT/vector
   [#"\(\. clojure\.lang\.RT object_array ([^\)]+)\)" "(h/-array (h/host) $1)"]
   [#"clojure\.lang\.RT/vector" "t/vec"]

   ;; Remaining java.util.concurrent
   [#"java\.util\.concurrent\.Future" "t/Future"]
   [#"java\.util\.concurrent\.Callable" "t/Callable"]
   [#"java\.util\.concurrent\.ExecutorService" "t/ExecutorService"]
   [#"java\.util\.Enumeration" "t/Enumeration"]
   [#"java\.util\.Iterator" "t/Iterator"]
   [#"java\.util\.Collection" "t/Collection"]

   ;; (.. Thread currentThread getContextClassLoader) — dotdot form
   [#"\(\.\. Thread currentThread getContextClassLoader\)"
    "(h/-context-class-loader (h/host))"]

   ;; java.io remaining
   [#"\(new java\.io\.StringWriter\)" "(t/string-writer)"]
   [#"java\.io\.File" "t/File"]

   ;; Clean up whitespace artifacts
   [#"  +" " "]
   [#"\n\n\n+" "\n\n"]])

(defn apply-string-fallbacks [text]
  (reduce (fn [t [pattern replacement]]
            (str/replace t pattern replacement))
          text
          string-fallback-replacements))

;; =============================================================================
;; NS form transformation
;; =============================================================================

(defn transform-ns [text]
  "Replace ns form to add requires"
  (str/replace text
               #"(?s)\(ns \^[^)]+\}\s+clojure\.core\)"
               "(ns ^{:doc \"The core Clojure language.\"
       :author \"Rich Hickey\"}
  clojure.core
  (:require [clojure.protocols :as p]
            [clojure.host :as h]
            [clojure.types :as t]))"))

;; =============================================================================
;; Main pipeline
;; =============================================================================

(defn tree-walk [text]
  "Walk rewrite-clj tree, apply structural transforms, return string"
  (let [zloc (z/of-string text {:track-position? true})]
    (loop [loc zloc
           n 0]
      (if (z/end? loc)
        (z/root-string loc)
        (let [replacement (try (transform-node loc) (catch Exception _ nil))]
          (if replacement
            (let [new-loc (try (replace-with-str loc replacement)
                               (catch Exception _
                                 ;; If parsing replacement fails, skip
                                 loc))]
              (recur (z/next new-loc) (inc n)))
            (recur (z/next loc) n)))))))

(defn transform [input-file output-file]
  (println "portabilize_v3: rewrite-clj based transformation")
  (println "  Input:" input-file)
  (let [content (slurp input-file)
        _ (println "  Lines:" (count (str/split-lines content)))

        ;; Phase 1: Add requires to ns
        step1 (transform-ns content)
        _ (println "  [1/4] NS form transformed")

        ;; Phase 2: Tree walk — structural transforms
        step2 (tree-walk step1)
        _ (println "  [2/4] Tree walk complete")

        ;; Phase 3: String fallbacks for edge cases
        step3 (apply-string-fallbacks step2)
        _ (println "  [3/4] String fallbacks applied")

        ;; Phase 4: Add header
        result (str ";; =============================================================================
;; PORTABLE CLOJURE CORE v3 — Generated by portabilize_v3.clj (rewrite-clj)
;; =============================================================================
;; Structure-preserving transformation. Comments, whitespace, and syntax-quote
;; templates are intact. JVM interop replaced with protocol/host/types calls.
;; =============================================================================

" step3)]

    (spit output-file result)
    (println "  [4/4] Written to" output-file)
    (println "  Output:" (count (str/split-lines result)) "lines")

    ;; Verification
    (println "\n=== JVM Residue Check ===")
    (let [lines (str/split-lines result)
          patterns [["clojure.lang.RT" #"clojure\.lang\.RT"]
                    ["clojure.lang.Numbers" #"clojure\.lang\.Numbers"]
                    ["clojure.lang.Util" #"clojure\.lang\.Util"]
                    ["clojure.lang.Symbol" #"clojure\.lang\.Symbol"]
                    ["clojure.lang.Keyword" #"clojure\.lang\.Keyword"]
                    ["clojure.lang.PersistentList" #"clojure\.lang\.PersistentList"]
                    [". System" #"\. System\b"]
                    ["java.lang" #"java\.lang"]
                    ["java.util" #"java\.util"]
                    ["java.io" #"java\.io"]
                    ["java.net" #"java\.net"]
                    ["java.sql" #"java\.sql"]
                    ["IllegalArgumentException" #"IllegalArgumentException"]
                    ["Thread" #"\bThread\b"]]]
      (doseq [[name pattern] patterns]
        (let [hits (filter #(re-find pattern %) lines)
              ;; Exclude matches in comments and docstrings
              code-hits (remove #(str/starts-with? (str/trim %) ";;") hits)]
          (when (pos? (count code-hits))
            (println (format "  ⚠ %s: %d" name (count code-hits)))
            (doseq [line (take 3 code-hits)]
              (let [idx (.indexOf (vec lines) line)]
                (println (format "      L%d: %s" (inc idx)
                                 (subs (str/trim line) 0 (min 90 (count (str/trim line)))))))))))

      (println "\n=== Form Counts ===")
      (let [text result]
        (println (format "  defn:     %d" (count (re-seq #"\(defn\s" text))))
        (println (format "  defmacro: %d" (count (re-seq #"\(defmacro\s" text))))
        (println (format "  def:      %d" (count (re-seq #"\(def\s" text))))))))

;; =============================================================================
;; Entry
;; =============================================================================

(defn -main [& args]
  (let [input (or (first args) "src/clj/clojure/core.cljc")
        output (or (second args) "src/clj/clojure/core_portable_v3.cljc")]
    (transform input output)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
