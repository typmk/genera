#!/usr/bin/env bb
;; Portable Core Transformer v2 - AST-based approach
;; Instead of regex, we read Clojure forms and transform them structurally.

(ns portabilize-core-v2
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

;; =============================================================================
;; Transformation Rules (declarative)
;; =============================================================================

(def symbol-renames
  "Direct symbol renames - static method/field references"
  {;; RT static methods
   'clojure.lang.RT/conj         'p/-conj
   'clojure.lang.RT/assoc        'p/-assoc
   'clojure.lang.RT/count        'p/-count
   'clojure.lang.RT/nth          'p/-nth
   'clojure.lang.RT/get          'p/-lookup
   'clojure.lang.RT/contains     'p/-contains-key?
   'clojure.lang.RT/dissoc       'p/-dissoc
   'clojure.lang.RT/peek         'p/-peek
   'clojure.lang.RT/pop          'p/-pop
   'clojure.lang.RT/intCast      'int
   'clojure.lang.RT/longCast     'long
   'clojure.lang.RT/floatCast    'float
   'clojure.lang.RT/doubleCast   'double
   'clojure.lang.RT/booleanCast  'boolean
   'clojure.lang.RT/charCast     'char
   'clojure.lang.RT/shortCast    'short
   'clojure.lang.RT/byteCast     'byte
   'clojure.lang.RT/isReduced    'reduced?
   'clojure.lang.RT/subvec       't/subvec
   'clojure.lang.RT/iter         't/iterator
   'clojure.lang.RT/uncheckedLongCast 'unchecked-long
   'clojure.lang.RT/uncheckedIntCast  'unchecked-int
   'clojure.lang.RT/seqToTypedArray   't/seq-to-array
   'clojure.lang.RT/readString        't/read-string
   'clojure.lang.RT/vector            't/vec
   'clojure.lang.RT/chunkIteratorSeq  't/chunk-iterator-seq
   'clojure.lang.RT/load              't/load-resource
   'clojure.lang.RT/canSeq            't/seqable?
   'clojure.lang.RT/baseLoader        't/base-loader

   ;; Numbers static
   'clojure.lang.Numbers/abs           'abs
   'clojure.lang.Numbers/quotient      'quot
   'clojure.lang.Numbers/remainder     'rem
   'clojure.lang.Numbers/add           '(fn [x y] (h/-add (h/host) x y))
   'clojure.lang.Numbers/minus         '(fn [x y] (h/-subtract (h/host) x y))
   'clojure.lang.Numbers/multiply      '(fn [x y] (h/-multiply (h/host) x y))
   'clojure.lang.Numbers/divide        '(fn [x y] (h/-divide (h/host) x y))
   'clojure.lang.Numbers/inc           '(fn [x] (h/-inc (h/host) x))
   'clojure.lang.Numbers/dec           '(fn [x] (h/-dec (h/host) x))
   'clojure.lang.Numbers/lt            '(fn [x y] (h/-lt (h/host) x y))
   'clojure.lang.Numbers/lte           '(fn [x y] (h/-lte (h/host) x y))
   'clojure.lang.Numbers/gt            '(fn [x y] (h/-gt (h/host) x y))
   'clojure.lang.Numbers/gte           '(fn [x y] (h/-gte (h/host) x y))
   'clojure.lang.Numbers/equiv         '(fn [x y] (h/-num-equiv (h/host) x y))
   'clojure.lang.Numbers/isZero        '(fn [x] (h/-zero? (h/host) x))
   'clojure.lang.Numbers/isPos         '(fn [x] (h/-pos? (h/host) x))
   'clojure.lang.Numbers/isNeg         '(fn [x] (h/-neg? (h/host) x))
   'clojure.lang.Numbers/shiftLeft     '(fn [x n] (h/-bit-shift-left (h/host) x n))
   'clojure.lang.Numbers/shiftRight    '(fn [x n] (h/-bit-shift-right (h/host) x n))
   'clojure.lang.Numbers/unsignedShiftRight '(fn [x n] (h/-unsigned-bit-shift-right (h/host) x n))
   'clojure.lang.Numbers/and           '(fn [x y] (h/-bit-and (h/host) x y))
   'clojure.lang.Numbers/or            '(fn [x y] (h/-bit-or (h/host) x y))
   'clojure.lang.Numbers/xor           '(fn [x y] (h/-bit-xor (h/host) x y))
   'clojure.lang.Numbers/not           '(fn [x] (h/-bit-not (h/host) x))
   'clojure.lang.Numbers/unchecked_add '(fn [x y] (unchecked-add x y))
   'clojure.lang.Numbers/unchecked_minus '(fn [x y] (unchecked-subtract x y))
   'clojure.lang.Numbers/unchecked_multiply '(fn [x y] (unchecked-multiply x y))
   'clojure.lang.Numbers/unchecked_inc '(fn [x] (unchecked-inc x))
   'clojure.lang.Numbers/unchecked_dec '(fn [x] (unchecked-dec x))
   'clojure.lang.Numbers/unchecked_negate '(fn [x] (unchecked-negate x))

   'clojure.lang.Util/identical  '(fn [a b] (h/-identical? (h/host) a b))
   'clojure.lang.Util/equiv      'p/-equiv
   'clojure.lang.Util/hash       'p/-hash
   'clojure.lang.Util/compare    'p/-compare
   'clojure.lang.Util/equals     '=

   'clojure.lang.LazilyPersistentVector/create  't/vec
   'clojure.lang.PersistentVector/create        't/vec
   'clojure.lang.PersistentHashMap/create       't/hash-map
   'clojure.lang.PersistentHashSet/create       't/hash-set
   'clojure.lang.PersistentArrayMap/create      't/array-map
   'clojure.lang.PersistentTreeMap/create       't/sorted-map
   'clojure.lang.PersistentTreeSet/create       't/sorted-set
   'clojure.lang.Symbol/intern                  't/symbol
   'clojure.lang.Keyword/intern                 't/keyword
   'clojure.lang.Keyword/find                   't/find-keyword

   'clojure.lang.Var/pushThreadBindings   't/push-thread-bindings
   'clojure.lang.Var/popThreadBindings    't/pop-thread-bindings
   'clojure.lang.Var/getThreadBindings    't/get-thread-bindings
   'clojure.lang.Var/cloneThreadBindingFrame  't/clone-thread-binding-frame
   'clojure.lang.Var/resetThreadBindingFrame  't/reset-thread-binding-frame
   'clojure.lang.Var/intern               't/intern-var

   'clojure.lang.Compiler/LOADER          't/current-loader
   'clojure.lang.Compiler/LINE            't/current-line
   'clojure.lang.Compiler/COLUMN          't/current-column

   ;; Java annotation constants
   'java.lang.annotation.RetentionPolicy/RUNTIME 't/retention-runtime

   'clojure.lang.RT/uncheckedByteCast     'unchecked-byte
   'clojure.lang.RT/uncheckedShortCast    'unchecked-short
   'clojure.lang.RT/uncheckedCharCast     'unchecked-char
   'clojure.lang.RT/uncheckedFloatCast    'unchecked-float
   'clojure.lang.RT/uncheckedDoubleCast   'unchecked-double
   'clojure.lang.RT/addURL                't/add-url
   'clojure.lang.RT/REQUIRE_LOCK          't/require-lock

   'clojure.lang.LockingTransaction/isRunning 't/in-transaction?
   'clojure.lang.LockingTransaction/runInTransaction 't/run-in-transaction

   'clojure.lang.Agent/pooledExecutor     't/pooled-executor
   'clojure.lang.Agent/soloExecutor       't/solo-executor
   'clojure.lang.Agent/releasePendingSends 't/release-pending-sends

   'clojure.lang.Namespace/find           't/find-ns
   'clojure.lang.Namespace/findOrCreate   't/find-or-create-ns
   'clojure.lang.Namespace/remove         't/remove-ns
   'clojure.lang.Namespace/all            't/all-ns

   'clojure.lang.Cycle/create             't/cycle
   'clojure.lang.Repeat/create            't/repeat
   'clojure.lang.Iterate/create           't/iterate
   'clojure.lang.LongRange/create         't/range
   'clojure.lang.Range/create             't/range

   'clojure.lang.TransformerIterator/create      't/transformer-iterator
   'clojure.lang.TransformerIterator/createMulti 't/transformer-iterator-multi

   'clojure.lang.Murmur3/mixCollHash      't/mix-coll-hash
   'clojure.lang.Murmur3/hashOrdered      't/hash-ordered
   'clojure.lang.Murmur3/hashUnordered    't/hash-unordered

   'clojure.lang.EnumerationSeq/create    't/enumeration-seq
   'clojure.lang.BigInt/fromBigInteger    't/bigint-from-biginteger
   'clojure.lang.BigInt/valueOf           't/bigint

   'clojure.lang.PersistentArrayMap/EMPTY 't/empty-map
   'clojure.lang.PersistentArrayMap/createAsIfByAssoc 't/array-map-assoc

   ;; Struct operations
   'clojure.lang.PersistentStructMap/createSlotMap  't/create-slot-map
   'clojure.lang.PersistentStructMap/create         't/create-struct
   'clojure.lang.PersistentStructMap/construct      't/construct-struct
   'clojure.lang.PersistentStructMap/getAccessor    't/get-accessor

   ;; Tagged literals
   'clojure.lang.TaggedLiteral/create        't/tagged-literal
   'clojure.lang.ReaderConditional/create    't/reader-conditional

   ;; Constructors as functions
   'clojure.lang.Volatile.                   't/volatile
   'clojure.lang.MultiFn.                    't/multifn
   'clojure.lang.Reduced.                    't/reduced
   'clojure.lang.Cons.                       't/cons
   'clojure.lang.ChunkBuffer.                't/chunk-buffer
   'clojure.lang.ChunkedCons.                't/chunk-cons
   'clojure.lang.LineNumberingPushbackReader. 't/line-numbering-reader

   ;; Delay force
   'clojure.lang.Delay/force                 't/force

   'clojure.lang.Compiler$HostExpr/maybeSpecialTag 't/maybe-special-tag
   'clojure.lang.Compiler$HostExpr/maybeClass      't/maybe-class
   'clojure.lang.Compiler/maybeResolveIn           't/maybe-resolve-in

   'clojure.lang.Reflector/prepRet        't/prep-ret

   ;; Protocol/interface symbols
   'clojure.lang.IFn                      'p/IFn
   'clojure.lang.IKVReduce                'p/IKVReduce
   'clojure.lang.IDeref                   'p/IDeref
   'clojure.lang.IBlockingDeref           'p/IBlockingDeref
   'clojure.lang.IPending                 'p/IPending
   'clojure.lang.IReduceInit              'p/IReduceInit
   'clojure.lang.Sequential               'p/ISequential
   'clojure.lang.Seqable                  'p/ISeqable

   ;; Java types used directly in code
   'java.lang.Object                      'Object
   'java.lang.Enum                        't/Enum
   'java.lang.annotation.Annotation       't/Annotation
   'java.lang.annotation.Retention        't/Retention
   'java.lang.reflect.Array               't/Array})

(def type-hints-to-remove
  "Type hints that should be removed (JVM-specific)"
  #{'clojure.lang.ISeq
    'clojure.lang.IPersistentMap
    'clojure.lang.Symbol
    'clojure.lang.Keyword
    'clojure.lang.IFn
    'clojure.lang.IObj
    'clojure.lang.IMeta
    'clojure.lang.ChunkBuffer
    'clojure.lang.IChunk
    'clojure.lang.IChunkedSeq
    'clojure.lang.Agent
    'clojure.lang.BigInt
    'clojure.lang.Namespace
    'clojure.lang.Var
    'clojure.lang.Volatile
    'clojure.lang.MultiFn
    'clojure.lang.Delay
    'clojure.lang.LazySeq
    'clojure.lang.PersistentList
    'clojure.lang.PersistentVector
    'clojure.lang.PersistentHashMap
    'clojure.lang.PersistentArrayMap
    'clojure.lang.Compiler$CompilerException
    'Class 'String 'Object 'Number 'StringBuilder 'Boolean
    'Long 'Integer 'Double 'Float 'Short 'Byte 'Character
    'BigDecimal 'BigInteger 'StackTraceElement})

(def instance-checks
  "instance? checks to transform"
  {'clojure.lang.ISeq              '(fn [x] (satisfies? p/ISeq x))
   'clojure.lang.IPersistentMap    '(fn [x] (satisfies? p/IMap x))
   'clojure.lang.IPersistentVector '(fn [x] (satisfies? p/IIndexed x))
   'clojure.lang.IPersistentSet    '(fn [x] (satisfies? p/ISet x))
   'clojure.lang.IPersistentCollection '(fn [x] (satisfies? p/ICollection x))
   'clojure.lang.IPersistentList   '(fn [x] (t/list? x))
   'clojure.lang.IDeref            '(fn [x] (satisfies? p/IDeref x))
   'clojure.lang.IMeta             '(fn [x] (satisfies? p/IMeta x))
   'clojure.lang.IObj              '(fn [x] (satisfies? p/IWithMeta x))
   'clojure.lang.Counted           '(fn [x] (satisfies? p/ICounted x))
   'clojure.lang.Indexed           '(fn [x] (satisfies? p/IIndexed x))
   'clojure.lang.IFn               '(fn [x] (satisfies? p/IFn x))
   'clojure.lang.Fn                '(fn [x] (satisfies? p/IFn x))
   'clojure.lang.Sequential        '(fn [x] (satisfies? p/ISequential x))
   'clojure.lang.Associative       '(fn [x] (satisfies? p/IAssociative x))
   'clojure.lang.Reversible        '(fn [x] (satisfies? p/IReversible x))
   'clojure.lang.Sorted            '(fn [x] (satisfies? p/ISorted x))
   'clojure.lang.IEditableCollection '(fn [x] (satisfies? p/IEditableCollection x))
   'clojure.lang.IChunkedSeq       '(fn [x] (t/chunked-seq? x))
   'clojure.lang.Symbol            '(fn [x] (t/symbol? x))
   'clojure.lang.Keyword           '(fn [x] (t/keyword? x))
   'clojure.lang.Delay             '(fn [x] (t/delay? x))
   'clojure.lang.Var               '(fn [x] (t/var? x))
   'clojure.lang.Ratio             '(fn [x] (t/ratio? x))
   'clojure.lang.BigInt            '(fn [x] (t/bigint? x))
   'clojure.lang.MultiFn           '(fn [x] (t/multifn? x))
   'clojure.lang.Volatile          '(fn [x] (t/volatile? x))
   'clojure.lang.Named             '(fn [x] (t/named? x))
   'clojure.lang.Namespace         '(fn [x] (t/namespace? x))
   'clojure.lang.IDrop             '(fn [x] (satisfies? p/IDrop x))
   'clojure.lang.IReduce           '(fn [x] (satisfies? p/IReduce x))
   'clojure.lang.IReduceInit       '(fn [x] (satisfies? p/IReduceInit x))
   'clojure.lang.TaggedLiteral     '(fn [x] (t/tagged-literal? x))
   'clojure.lang.ReaderConditional '(fn [x] (t/reader-conditional? x))
   'clojure.lang.LineNumberingPushbackReader '(fn [x] (t/line-numbering-reader? x))
   'String                         '(fn [x] (t/string? x))
   'Character                      '(fn [x] (t/char? x))
   'Number                         '(fn [x] (t/number? x))
   'Boolean                        '(fn [x] (or (true? x) (false? x)))
   'Integer                        '(fn [x] (t/int? x))
   'Long                           '(fn [x] (t/long? x))
   'Double                         '(fn [x] (t/double? x))
   'Float                          '(fn [x] (t/float? x))
   'BigDecimal                     '(fn [x] (t/bigdec? x))
   'BigInteger                     '(fn [x] (t/biginteger? x))})

;; =============================================================================
;; Form Transformers
;; =============================================================================

(defn jvm-type-hint? [hint]
  "Is this a JVM-specific type hint symbol or string?"
  (cond
    (symbol? hint)
    (let [s (str hint)]
      (or (str/starts-with? s "clojure.lang.")
          (str/starts-with? s "java.lang.")
          (str/starts-with? s "java.util.")
          (str/starts-with? s "^clojure.lang.")
          (str/starts-with? s "^java.lang.")
          (str/starts-with? s "^java.util.")
          (contains? type-hints-to-remove hint)))

    ;; String type hints like "[Ljava.lang.Object;" or "java.lang.String"
    (string? hint)
    (or (str/includes? hint "java.lang.")
        (str/includes? hint "java.util.")
        (str/includes? hint "clojure.lang.")
        (str/starts-with? hint "[L"))

    :else false))

(defn remove-type-hints [form]
  "Remove JVM type hints from metadata"
  (if (instance? clojure.lang.IObj form)
    (let [m (meta form)]
      (if (and m (:tag m) (jvm-type-hint? (:tag m)))
        (with-meta form (dissoc m :tag))
        form))
    form))

(defn remove-jvm-metadata [form]
  "Recursively remove JVM type hints from metadata on any form"
  (walk/postwalk
   (fn [x]
     (if (instance? clojure.lang.IObj x)
       (let [m (meta x)]
         (if (and m (:tag m) (jvm-type-hint? (:tag m)))
           (with-meta x (dissoc m :tag))
           x))
       x))
   form))

(defn transform-metadata-map [form]
  "Transform JVM symbols in metadata-looking maps (maps with :tag, :arglists, etc.)"
  (when (map? form)
    (let [tag (:tag form)
          arglists (:arglists form)
          needs-tag-removal (and tag (jvm-type-hint? tag))
          needs-arglists-clean (and arglists (seq? arglists) (= 'quote (first arglists)))]
      (when (or needs-tag-removal needs-arglists-clean)
        (cond-> form
          ;; Remove :tag if it's a JVM type
          needs-tag-removal
          (dissoc :tag)

          ;; Clean :arglists of type hints (both as symbols AND as metadata on vectors)
          needs-arglists-clean
          (assoc :arglists
                 (let [arglist-data (second arglists)
                       ;; First remove metadata from forms (like ^clojure.lang.ISeq [coll])
                       cleaned (remove-jvm-metadata arglist-data)
                       ;; Then remove any JVM symbols
                       cleaned2 (walk/postwalk
                                 (fn [x]
                                   (if (and (symbol? x) (jvm-type-hint? x))
                                     nil
                                     x))
                                 cleaned)
                       ;; Filter out nils from vectors
                       cleaned3 (walk/postwalk
                                 (fn [x]
                                   (if (vector? x)
                                     (vec (remove nil? x))
                                     x))
                                 cleaned2)]
                   `(quote ~cleaned3))))))))

(defn parse-dot-call [rest]
  "Parse the method and args from a dot call.
   Handles both (. target (method args)) and (. target method args) forms."
  (let [[method-form & extra-args] rest]
    (if (seq? method-form)
      ;; Paren form: (. target (method arg1 arg2))
      (let [[method & method-args] method-form]
        [method (concat method-args extra-args)])
      ;; Space form: (. target method arg1 arg2)
      [method-form extra-args])))

(defn transform-dot-call [form]
  "Transform (. target (method args)) or (. target method args) forms"
  (when (and (seq? form) (= '. (first form)))
    (let [[_ target & rest] form
          [method all-args] (parse-dot-call rest)]
      (cond
        ;; (. clojure.lang.RT (first x)) -> (p/-first x) etc.
        (= target 'clojure.lang.RT)
        (case method
          first  (if (= 1 (count all-args))
                   `(let [s# (p/-seq ~(first all-args))]
                      (when s# (p/-first s#)))
                   form)
          next   `(p/-next ~@all-args)
          more   `(p/-rest ~@all-args)
          seq    `(p/-seq ~@all-args)
          cons   `(t/cons ~@all-args)
          conj   `(p/-conj ~@all-args)
          assoc  `(p/-assoc ~@all-args)
          count  `(p/-count ~@all-args)
          nth    `(p/-nth ~@all-args)
          get    `(p/-lookup ~@all-args)
          contains `(p/-contains-key? ~@all-args)
          dissoc `(p/-dissoc ~@all-args)
          peek   `(p/-peek ~@all-args)
          pop    `(p/-pop ~@all-args)
          keys   `(t/keys ~@all-args)
          vals   `(t/vals ~@all-args)
          find   `(t/find ~@all-args)
          toArray `(h/-to-array (h/host) ~@all-args)
          subvec  `(t/subvec ~@all-args)
          intCast `(int ~@all-args)
          longCast `(long ~@all-args)
          floatCast `(float ~@all-args)
          doubleCast `(double ~@all-args)
          booleanCast `(boolean ~@all-args)
          charCast `(char ~@all-args)
          shortCast `(short ~@all-args)
          byteCast `(byte ~@all-args)
          uncheckedIntCast `(unchecked-int ~@all-args)
          uncheckedLongCast `(unchecked-long ~@all-args)
          nextID `(t/next-id)
          chunkIteratorSeq `(t/chunk-iterator-seq ~@all-args)
          readString `(t/read-string ~@all-args)
          alength `(h/-alength (h/host) ~@all-args)
          aclone `(h/-aclone (h/host) ~@all-args)
          aget `(h/-aget (h/host) ~@all-args)
          aset `(h/-aset (h/host) ~@all-args)
          uncheckedByteCast `(unchecked-byte ~@all-args)
          uncheckedShortCast `(unchecked-short ~@all-args)
          uncheckedCharCast `(unchecked-char ~@all-args)
          uncheckedFloatCast `(unchecked-float ~@all-args)
          uncheckedDoubleCast `(unchecked-double ~@all-args)
          object_array `(t/object-array ~@all-args)
          addURL `(t/add-url ~@all-args)
          form)

        ;; (. clojure.lang.Numbers (add x y)) -> (h/-add (h/host) x y) etc.
        (= target 'clojure.lang.Numbers)
        (case method
          add      `(h/-add (h/host) ~@all-args)
          addP     `(h/-add (h/host) ~@all-args)
          minus    (if (= 1 (count all-args))
                     `(h/-negate (h/host) ~@all-args)
                     `(h/-subtract (h/host) ~@all-args))
          minusP   (if (= 1 (count all-args))
                     `(h/-negate (h/host) ~@all-args)
                     `(h/-subtract (h/host) ~@all-args))
          negate   `(h/-negate (h/host) ~@all-args)
          multiply `(h/-multiply (h/host) ~@all-args)
          multiplyP `(h/-multiply (h/host) ~@all-args)
          divide   `(h/-divide (h/host) ~@all-args)
          inc      `(h/-inc (h/host) ~@all-args)
          incP     `(h/-inc (h/host) ~@all-args)
          dec      `(h/-dec (h/host) ~@all-args)
          decP     `(h/-dec (h/host) ~@all-args)
          lt       `(h/-lt (h/host) ~@all-args)
          lte      `(h/-lte (h/host) ~@all-args)
          gt       `(h/-gt (h/host) ~@all-args)
          gte      `(h/-gte (h/host) ~@all-args)
          equiv    `(h/-num-equiv (h/host) ~@all-args)
          isZero   `(h/-zero? (h/host) ~@all-args)
          isPos    `(h/-pos? (h/host) ~@all-args)
          isNeg    `(h/-neg? (h/host) ~@all-args)
          max      `(if (h/-gt (h/host) ~(first all-args) ~(second all-args))
                      ~(first all-args) ~(second all-args))
          min      `(if (h/-lt (h/host) ~(first all-args) ~(second all-args))
                      ~(first all-args) ~(second all-args))
          and      `(h/-bit-and (h/host) ~@all-args)
          or       `(h/-bit-or (h/host) ~@all-args)
          xor      `(h/-bit-xor (h/host) ~@all-args)
          not      `(h/-bit-not (h/host) ~@all-args)
          shiftLeft `(h/-bit-shift-left (h/host) ~@all-args)
          shiftRight `(h/-bit-shift-right (h/host) ~@all-args)
          unsignedShiftRight `(h/-unsigned-bit-shift-right (h/host) ~@all-args)
          quotient `(quot ~@all-args)
          remainder `(rem ~@all-args)
          rationalize `(t/rationalize ~@all-args)
          num      `(t/num ~@all-args)
          ;; Unchecked operations
          unchecked_add `(unchecked-add ~@all-args)
          unchecked_minus `(unchecked-subtract ~@all-args)
          unchecked_multiply `(unchecked-multiply ~@all-args)
          unchecked_inc `(unchecked-inc ~@all-args)
          unchecked_dec `(unchecked-dec ~@all-args)
          unchecked_negate `(unchecked-negate ~@all-args)
          ;; Int-specific unchecked
          unchecked_int_inc `(unchecked-inc-int ~@all-args)
          unchecked_int_dec `(unchecked-dec-int ~@all-args)
          unchecked_int_negate `(unchecked-negate-int ~@all-args)
          unchecked_int_add `(unchecked-add-int ~@all-args)
          unchecked_int_subtract `(unchecked-subtract-int ~@all-args)
          unchecked_int_multiply `(unchecked-multiply-int ~@all-args)
          unchecked_int_divide `(unchecked-divide-int ~@all-args)
          unchecked_int_remainder `(unchecked-remainder-int ~@all-args)
          ;; Long-specific unchecked
          unchecked_long_inc `(unchecked-inc ~@all-args)
          unchecked_long_dec `(unchecked-dec ~@all-args)
          unchecked_long_negate `(unchecked-negate ~@all-args)
          unchecked_long_add `(unchecked-add ~@all-args)
          unchecked_long_subtract `(unchecked-subtract ~@all-args)
          unchecked_long_multiply `(unchecked-multiply ~@all-args)
          ;; Bit operations
          andNot   `(bit-and-not ~@all-args)
          clearBit `(bit-clear ~@all-args)
          setBit   `(bit-set ~@all-args)
          flipBit  `(bit-flip ~@all-args)
          testBit  `(bit-test ~@all-args)
          ;; Array creation
          float_array   `(t/float-array ~@all-args)
          boolean_array `(t/boolean-array ~@all-args)
          byte_array    `(t/byte-array ~@all-args)
          char_array    `(t/char-array ~@all-args)
          short_array   `(t/short-array ~@all-args)
          double_array  `(t/double-array ~@all-args)
          int_array     `(t/int-array ~@all-args)
          long_array    `(t/long-array ~@all-args)
          ;; Array cast
          booleans `(t/booleans ~@all-args)
          bytes    `(t/bytes ~@all-args)
          chars    `(t/chars ~@all-args)
          shorts   `(t/shorts ~@all-args)
          floats   `(t/floats ~@all-args)
          ints     `(t/ints ~@all-args)
          longs    `(t/longs ~@all-args)
          doubles  `(t/doubles ~@all-args)
          form)

        ;; (. clojure.lang.Util (identical a b)) -> (h/-identical? (h/host) a b)
        (= target 'clojure.lang.Util)
        (case method
          identical `(h/-identical? (h/host) ~@all-args)
          equiv     `(p/-equiv ~@all-args)
          hash      `(p/-hash ~@all-args)
          hasheq    `(p/-hash ~@all-args)
          compare   `(p/-compare ~@all-args)
          equals    `(= ~@all-args)
          form)

        ;; (. clojure.lang.Var (pushThreadBindings m)) -> (t/push-thread-bindings m)
        (= target 'clojure.lang.Var)
        (case method
          pushThreadBindings `(t/push-thread-bindings ~@all-args)
          popThreadBindings  `(t/pop-thread-bindings)
          find               `(t/find-var ~@all-args)
          create             `(t/create-var)
          setDynamic         `(t/set-dynamic ~@all-args)
          form)

        ;; (. clojure.lang.Compiler (eval x)) -> (t/eval x)
        (= target 'clojure.lang.Compiler)
        (case method
          eval        `(t/eval ~@all-args)
          macroexpand1 `(t/macroexpand-1 ~@all-args)
          load        `(t/load-reader ~@all-args)
          specials    `t/special-symbols
          form)

        ;; (. clojure.lang.LispReader (read ...)) -> (t/read-clojure ...)
        (= target 'clojure.lang.LispReader)
        (case method
          read `(t/read-clojure ~@all-args)
          form)

        ;; (. clojure.lang.Agent shutdown) -> (t/shutdown-agents)
        (= target 'clojure.lang.Agent)
        (case method
          shutdown `(t/shutdown-agents)
          form)

        ;; (. clojure.lang.LockingTransaction ...)
        (= target 'clojure.lang.LockingTransaction)
        (case method
          isRunning        `(t/in-transaction?)
          runInTransaction `(t/run-in-transaction ~@all-args)
          form)

        ;; (. clojure.lang.PersistentStructMap ...) methods
        (= target 'clojure.lang.PersistentStructMap)
        (case method
          createSlotMap `(t/create-slot-map ~@all-args)
          create        `(t/create-struct ~@all-args)
          construct     `(t/construct-struct ~@all-args)
          getAccessor   `(t/get-accessor ~@all-args)
          form)

        ;; (. clojure.lang.LazilyPersistentVector (create x))
        (= target 'clojure.lang.LazilyPersistentVector)
        (case method
          create `(t/vec ~@all-args)
          form)

        ;; (. clojure.lang.PersistentHashMap (create x))
        (= target 'clojure.lang.PersistentHashMap)
        (case method
          create `(t/hash-map ~@all-args)
          form)

        ;; (. clojure.lang.PersistentArrayMap ...)
        (= target 'clojure.lang.PersistentArrayMap)
        (case method
          create           `(t/array-map ~@all-args)
          createAsIfByAssoc `(t/array-map-assoc ~@all-args)
          EMPTY            `t/empty-map
          form)

        ;; (. clojure.lang.PersistentList ...)
        (= target 'clojure.lang.PersistentList)
        (case method
          creator `t/list-creator
          form)

        ;; (. clojure.lang.Delay (force d))
        (= target 'clojure.lang.Delay)
        (case method
          force `(t/force ~@all-args)
          form)

        ;; (. clojure.lang.Symbol (intern x))
        (= target 'clojure.lang.Symbol)
        (case method
          intern `(t/symbol ~@all-args)
          form)

        ;; (. Array (get arr i))
        (= target 'Array)
        (case method
          get `(h/-aget (h/host) ~@all-args)
          form)

        ;; Other dot calls - keep as is
        :else form))))

(defn transform-new [form]
  "Transform (new ClassName args) forms"
  (when (and (seq? form) (= 'new (first form)))
    (let [[_ classname & args] form]
      (case classname
        clojure.lang.LazySeq    `(t/lazy-seq* ~@args)
        clojure.lang.Delay      `(t/delay* ~@args)
        clojure.lang.MultiFn    `(t/multifn ~@args)
        clojure.lang.Agent      `(t/agent ~@args)
        clojure.lang.Ref        `(t/ref ~@args)
        clojure.lang.Atom       `(t/atom ~@args)
        clojure.lang.Compiler$CompilerException `(t/compiler-exception ~@args)
        java.lang.IllegalAccessError `(ex-info "Illegal access" {})
        java.lang.IllegalStateException `(ex-info ~(or (first args) "Illegal state") {})
        Exception               `(ex-info ~(or (first args) "Error") {})
        nil))))

(defn transform-instance [form]
  "Transform (instance? ClassName x) forms"
  (when (and (seq? form)
             (= 'instance? (first form))
             (= 3 (count form)))
    (let [[_ classname x] form]
      (when-let [check-fn (get instance-checks classname)]
        `(~@(rest check-fn) ~x)))))

(defn transform-throw [form]
  "Transform throw forms with JVM exceptions"
  (when (and (seq? form) (= 'throw (first form)))
    (let [[_ exception-form] form]
      (cond
        ;; (throw (IllegalArgumentException. "msg"))
        (and (seq? exception-form)
             (symbol? (first exception-form))
             (str/ends-with? (str (first exception-form)) "."))
        (let [classname (str (first exception-form))
              msg (second exception-form)]
          (cond
            (str/includes? classname "IllegalArgumentException")
            `(throw (ex-info ~(or msg "Illegal argument") {}))

            (str/includes? classname "IndexOutOfBoundsException")
            `(throw (ex-info "Index out of bounds" {}))

            (str/includes? classname "ClassCastException")
            `(throw (ex-info ~(or msg "Cast failed") {}))

            (str/includes? classname "UnsupportedOperationException")
            `(throw (ex-info ~(or msg "Unsupported operation") {}))

            (str/includes? classname "ArithmeticException")
            `(throw (ex-info ~(or msg "Arithmetic error") {}))

            (str/includes? classname "RuntimeException")
            `(throw (ex-info ~(or msg "Runtime error") {}))

            (str/includes? classname "IllegalAccessError")
            `(throw (ex-info ~(or msg "Illegal access") {}))

            :else nil))

        :else nil))))

(defn remove-inline-from-meta [m]
  "Remove :inline and :inline-arities from metadata map"
  (dissoc m :inline :inline-arities))

(def quoted-jvm-symbols
  "Quoted JVM symbols to transform - these appear in macro-generated code.
   Maps to actual clojure.types (t/) or clojure.protocols (p/) abstractions."
  {;; Protocols (interface equivalents)
   'clojure.lang.ISeq               'p/ISeq
   'clojure.lang.IChunk             'p/IChunk
   'clojure.lang.IFn                'p/IFn
   'clojure.lang.IDeref             'p/IDeref
   'clojure.lang.IMeta              'p/IMeta
   'clojure.lang.IObj               'p/IWithMeta
   'clojure.lang.Counted            'p/ICounted
   'clojure.lang.Indexed            'p/IIndexed
   'clojure.lang.Sequential         'p/ISequential
   'clojure.lang.Associative        'p/IAssociative
   'clojure.lang.IExceptionInfo     'p/IExceptionInfo

   ;; Types (use t/ type namespace)
   'clojure.lang.Var                't/Var
   'clojure.lang.MultiFn            't/MultiFn
   'clojure.lang.Keyword            't/Keyword
   'clojure.lang.Symbol             't/Symbol
   'clojure.lang.Volatile           't/Volatile
   'clojure.lang.Delay              't/Delay
   'clojure.lang.LazySeq            't/LazySeq
   'clojure.lang.Namespace          't/Namespace
   'clojure.lang.ExceptionInfo      't/ExceptionInfo
   'clojure.lang.Compiler$CompilerException 't/CompilerException

   ;; These don't have direct equivalents - map to internal symbols
   ;; that the portable core should define or handle specially
   'clojure.lang.Numbers            'clojure.numbers  ; internal
   'clojure.lang.RT                 'clojure.rt       ; internal
   'clojure.lang.Compiler           'clojure.compiler ; internal
   'clojure.lang.LockingTransaction 'clojure.stm      ; internal

   ;; Java types - map to portable equivalents or ex-info
   'java.lang.Object                     'Object
   'java.lang.IllegalStateException      'ex-info
   'java.lang.AssertionError             't/AssertionError
   'java.lang.ClassNotFoundException     't/ClassNotFoundException
   'java.lang.System                     't/System
   'java.lang.Enum                       't/Enum
   'java.lang.annotation.Annotation      't/Annotation
   'java.lang.annotation.Retention       't/Retention
   'java.lang.annotation.RetentionPolicy 't/RetentionPolicy
   'java.lang.reflect                    't/Reflect})

(defn transform-quote [form]
  "Transform (quote jvm-symbol) forms to portable equivalents"
  (when (and (seq? form)
             (= 'quote (first form))
             (= 2 (count form)))
    (let [sym (second form)]
      (when (symbol? sym)
        (when-let [replacement (get quoted-jvm-symbols sym)]
          `(quote ~replacement))))))

(defn transform-double-dot [form]
  "Transform (.. target method1 method2) forms with JVM classes"
  (when (and (seq? form) (= '.. (first form)))
    (let [[_ target & methods] form]
      (cond
        ;; (.. clojure.lang.Var create setDynamic) -> (t/create-dynamic-var)
        (and (= target 'clojure.lang.Var)
             (= methods '(create setDynamic)))
        '(t/create-dynamic-var)

        :else nil))))

(defn jvm-import? [import-spec]
  "Check if an import spec is JVM-specific"
  (cond
    ;; Simple symbol: java.lang.String
    (symbol? import-spec)
    (let [s (str import-spec)]
      (or (str/starts-with? s "clojure.lang.")
          (str/starts-with? s "java.")
          (str/starts-with? s "clojure.asm.")))

    ;; Quoted symbol: (quote java.lang.String)
    (and (seq? import-spec)
         (= 'quote (first import-spec)))
    (jvm-import? (second import-spec))

    ;; Prefix list: (java.lang String Integer) or (quote (java.lang.reflect Array))
    (seq? import-spec)
    (let [first-elem (first import-spec)]
      (cond
        ;; (quote (pkg Class1 Class2))
        (= 'quote first-elem)
        (jvm-import? (second import-spec))

        ;; (pkg Class1 Class2)
        (symbol? first-elem)
        (let [s (str first-elem)]
          (or (str/starts-with? s "clojure.lang")
              (str/starts-with? s "java.")
              (str/starts-with? s "clojure.asm")))

        :else false))

    :else false))

(defn transform-import [form]
  "Transform (import ...) forms to remove JVM classes"
  (when (and (seq? form)
             (let [f (first form)]
               (or (= f 'import)
                   (= f 'clojure.core/import))))
    (let [classes (rest form)
          portable-imports (remove jvm-import? classes)]
      (if (empty? portable-imports)
        '(comment "JVM imports removed")
        `(import ~@portable-imports)))))

(defn transform-defn-meta [form]
  "Remove :inline from defn metadata"
  (when (and (seq? form)
             (contains? #{'defn 'defn- 'defmacro} (first form)))
    (let [items (vec (rest form))
          ;; Find metadata map position
          [before-meta meta-and-after]
          (split-with #(not (and (map? %) (or (:inline %) (:inline-arities %)))) items)]
      (if (seq meta-and-after)
        (let [meta-map (first meta-and-after)
              cleaned-meta (remove-inline-from-meta meta-map)
              after-meta (rest meta-and-after)]
          (if (empty? cleaned-meta)
            `(~(first form) ~@before-meta ~@after-meta)
            `(~(first form) ~@before-meta ~cleaned-meta ~@after-meta)))
        nil))))

;; =============================================================================
;; Main Transform
;; =============================================================================

(defn transform-form [form]
  "Apply all transformations to a single form"
  (or (transform-dot-call form)
      (transform-new form)
      (transform-instance form)
      (transform-throw form)
      (transform-quote form)
      (transform-double-dot form)
      (transform-import form)
      (transform-metadata-map form)
      (transform-defn-meta form)
      ;; Symbol renames
      (when (symbol? form)
        (get symbol-renames form))
      ;; Handle constructor calls like ClassName.
      (when (and (symbol? form)
                 (str/ends-with? (str form) ".")
                 (let [s (str form)]
                   (or (str/includes? s "clojure.lang.")
                       (str/includes? s "java.lang."))))
        (let [s (str form)
              class-name (subs s 0 (dec (count s)))]
          (cond
            (str/includes? class-name "Compiler$CompilerException")
            't/compiler-exception
            (str/includes? class-name "IllegalStateException")
            'ex-info
            (str/includes? class-name "IllegalArgumentException")
            'ex-info
            :else form)))
      ;; Default: return as-is
      form))

(defn validate-form [form idx]
  "Validate a form can be read back correctly"
  (try
    (let [s (pr-str form)
          re-read (read-string s)]
      true)
    (catch Exception e
      (println "  ⚠ Form" idx "failed validation:" (.getMessage e))
      (println "    Form type:" (type form))
      (println "    First elem:" (when (seq? form) (first form)))
      false)))

(defn transform-tree [form]
  "Walk the entire form tree and transform, including metadata"
  (letfn [(walk-with-meta [x]
            (let [;; First transform any metadata
                  x-with-cleaned-meta
                  (if (instance? clojure.lang.IObj x)
                    (let [m (meta x)]
                      (if m
                        (let [transformed-meta (transform-tree m)]
                          (with-meta x transformed-meta))
                        x))
                    x)]
              ;; Then transform the form itself
              (-> x-with-cleaned-meta
                  remove-type-hints
                  transform-form)))]
    (walk/postwalk walk-with-meta form)))

(defn transform-with-validation [forms]
  "Transform all forms with progress reporting and validation"
  (let [total (count forms)
        results (atom {:transformed 0 :errors []})
        transformed
        (doall
         (map-indexed
          (fn [idx form]
            (when (zero? (mod idx 50))
              (println (format "  Transforming form %d/%d..." idx total)))
            (try
              (let [result (transform-tree form)]
                (when-not (validate-form result idx)
                  (swap! results update :errors conj idx))
                (swap! results update :transformed inc)
                result)
              (catch Exception e
                (println (format "  ✗ Error at form %d: %s" idx (.getMessage e)))
                (swap! results update :errors conj idx)
                form)))
          forms))]
    (println (format "  Transformed %d forms, %d errors"
                     (:transformed @results)
                     (count (:errors @results))))
    transformed))

;; =============================================================================
;; Reader & Writer
;; =============================================================================

(defn read-all-forms [file]
  "Read all forms from a file"
  (with-open [rdr (java.io.PushbackReader. (clojure.java.io/reader file))]
    (let [eof (Object.)]
      (loop [forms []]
        (let [form (try
                     (read {:eof eof :read-cond :preserve} rdr)
                     (catch Exception e
                       (println "Read error:" (.getMessage e))
                       eof))]
          (if (identical? form eof)
            forms
            (recur (conj forms form))))))))

(defn form->string [form]
  "Convert a form back to string representation"
  (binding [*print-meta* true
            *print-length* nil
            *print-level* nil]
    (pr-str form)))

(defn write-forms [forms file]
  "Write forms to file with proper formatting"
  (with-open [w (clojure.java.io/writer file)]
    (binding [*out* w]
      (println ";; =============================================================================")
      (println ";; PORTABLE CLOJURE CORE - Auto-generated via AST transformation")
      (println ";; =============================================================================")
      (println)
      (doseq [form forms]
        (println (form->string form))
        (println)))))

;; =============================================================================
;; Main
;; =============================================================================

(defn transform-file [input output]
  (println "Reading" input "...")
  (let [forms (read-all-forms input)
        _ (println "  Read" (count forms) "forms")
        _ (println "Transforming with validation...")
        transformed (transform-with-validation forms)]
    (println "Writing" output "...")
    (write-forms transformed output)
    (println "Checking output for JVM references...")
    ;; Walk the forms and count JVM symbols (not strings)
    (let [forms (read-all-forms output)
          jvm-symbols (atom [])
          string-refs (atom [])
          _ (doseq [form forms]
              (walk/postwalk
               (fn [x]
                 (cond
                   ;; Symbol containing clojure.lang
                   (and (symbol? x)
                        (let [s (str x)]
                          (or (str/includes? s "clojure.lang.")
                              (str/includes? s "java.lang."))))
                   (swap! jvm-symbols conj x)

                   ;; String containing clojure.lang (docstrings)
                   (and (string? x)
                        (str/includes? x "clojure.lang."))
                   (swap! string-refs conj x))
                 x)
               form))
          code-refs @jvm-symbols
          doc-refs @string-refs]
      (if (seq code-refs)
        (do
          (println (format "  ⚠ Found %d code JVM references:" (count code-refs)))
          (doseq [[ref cnt] (->> code-refs (map str) frequencies (sort-by second >) (take 10))]
            (println (format "    %s: %d" ref cnt))))
        (println "  ✓ No code JVM references found!"))
      (when (seq doc-refs)
        (println (format "  ℹ %d docstrings mention JVM types (documentation only)" (count doc-refs)))))
    (println "Done!")))

(defn -main [& args]
  (let [input (or (first args) "src/clj/clojure/core.cljc")
        output (or (second args) "src/clj/clojure/core_portable_v2.cljc")]
    (transform-file input output)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
