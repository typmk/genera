#!/usr/bin/env bb
;; Script to transform JVM-specific core.cljc to portable version
;; Uses protocols and host abstraction instead of clojure.lang.* calls

(ns portabilize-core
  (:require [clojure.string :as str]))

;; =============================================================================
;; Transformation rules
;; =============================================================================

(def rt-replacements
  "clojure.lang.RT method → replacement"
  {;; Sequence operations
   "(. clojure.lang.RT (first $1))"    "(let [s# (p/-seq $1)] (when s# (p/-first s#)))"
   "(. clojure.lang.RT (next $1))"     "(p/-next $1)"
   "(. clojure.lang.RT (more $1))"     "(p/-rest $1)"
   "(. clojure.lang.RT (seq $1))"      "(p/-seq $1)"
   "(. clojure.lang.RT (cons $1 $2))"  "(t/cons $1 $2)"

   ;; Collection operations
   "clojure.lang.RT/conj"              "p/-conj"
   "clojure.lang.RT/assoc"             "p/-assoc"
   "clojure.lang.RT/count"             "p/-count"
   "clojure.lang.RT/nth"               "p/-nth"
   "clojure.lang.RT/get"               "p/-lookup"
   "clojure.lang.RT/contains"          "p/-contains-key?"
   "clojure.lang.RT/dissoc"            "p/-dissoc"
   "clojure.lang.RT/peek"              "p/-peek"
   "clojure.lang.RT/pop"               "p/-pop"
   "clojure.lang.RT/keys"              "t/keys"
   "clojure.lang.RT/vals"              "t/vals"
   "clojure.lang.RT/find"              "t/find"
   "clojure.lang.RT/toArray"           "(h/-to-array (h/host))"

   ;; Type casts
   "clojure.lang.RT/intCast"           "int"
   "clojure.lang.RT/longCast"          "long"
   "clojure.lang.RT/floatCast"         "float"
   "clojure.lang.RT/doubleCast"        "double"
   "clojure.lang.RT/booleanCast"       "boolean"

   ;; Misc
   "clojure.lang.RT/isReduced"         "reduced?"
   "clojure.lang.RT/nextID"            "t/next-id"})

(def numbers-replacements
  "clojure.lang.Numbers method → replacement"
  {"(. clojure.lang.Numbers (add $1 $2))"       "(h/-add (h/host) $1 $2)"
   "(. clojure.lang.Numbers (minus $1 $2))"     "(h/-subtract (h/host) $1 $2)"
   "(. clojure.lang.Numbers (minus $1))"        "(h/-negate (h/host) $1)"
   "(. clojure.lang.Numbers (multiply $1 $2))"  "(h/-multiply (h/host) $1 $2)"
   "(. clojure.lang.Numbers (divide $1 $2))"    "(h/-divide (h/host) $1 $2)"
   "(. clojure.lang.Numbers (inc $1))"          "(h/-inc (h/host) $1)"
   "(. clojure.lang.Numbers (dec $1))"          "(h/-dec (h/host) $1)"
   "(. clojure.lang.Numbers (lt $1 $2))"        "(h/-lt (h/host) $1 $2)"
   "(. clojure.lang.Numbers (lte $1 $2))"       "(h/-lte (h/host) $1 $2)"
   "(. clojure.lang.Numbers (gt $1 $2))"        "(h/-gt (h/host) $1 $2)"
   "(. clojure.lang.Numbers (gte $1 $2))"       "(h/-gte (h/host) $1 $2)"
   "(. clojure.lang.Numbers (equiv $1 $2))"     "(h/-num-equiv (h/host) $1 $2)"
   "(. clojure.lang.Numbers (isZero $1))"       "(h/-zero? (h/host) $1)"
   "(. clojure.lang.Numbers (isPos $1))"        "(h/-pos? (h/host) $1)"
   "(. clojure.lang.Numbers (isNeg $1))"        "(h/-neg? (h/host) $1)"
   "(. clojure.lang.Numbers (max $1 $2))"       "(if (h/-gt (h/host) $1 $2) $1 $2)"
   "(. clojure.lang.Numbers (min $1 $2))"       "(if (h/-lt (h/host) $1 $2) $1 $2)"
   "clojure.lang.Numbers/abs"                   "abs"
   "clojure.lang.Numbers/quotient"              "(h/-quot (h/host))"
   "clojure.lang.Numbers/remainder"             "(h/-rem (h/host))"})

(def util-replacements
  "clojure.lang.Util method → replacement"
  {"clojure.lang.Util/identical"  "(h/-identical? (h/host))"
   "clojure.lang.Util/equiv"      "p/-equiv"
   "clojure.lang.Util/hash"       "p/-hash"
   "clojure.lang.Util/compare"    "p/-compare"})

(def instance-replacements
  "instance? checks → satisfies? checks"
  {"(instance? clojure.lang.ISeq $1)"              "(satisfies? p/ISeq $1)"
   "(instance? clojure.lang.IPersistentMap $1)"    "(satisfies? p/IMap $1)"
   "(instance? clojure.lang.IPersistentVector $1)" "(satisfies? p/IIndexed $1)"
   "(instance? clojure.lang.IPersistentSet $1)"    "(satisfies? p/ISet $1)"
   "(instance? clojure.lang.IPersistentCollection $1)" "(satisfies? p/ICollection $1)"
   "(instance? clojure.lang.IDeref $1)"            "(satisfies? p/IDeref $1)"
   "(instance? clojure.lang.IMeta $1)"             "(satisfies? p/IMeta $1)"
   "(instance? clojure.lang.IObj $1)"              "(satisfies? p/IWithMeta $1)"
   "(instance? clojure.lang.Counted $1)"           "(satisfies? p/ICounted $1)"
   "(instance? clojure.lang.Indexed $1)"           "(satisfies? p/IIndexed $1)"
   "(instance? clojure.lang.IFn $1)"               "(satisfies? p/IFn $1)"
   "(instance? clojure.lang.Sequential $1)"        "(satisfies? p/ISequential $1)"
   "(instance? clojure.lang.Associative $1)"       "(satisfies? p/IAssociative $1)"
   "(instance? clojure.lang.Reversible $1)"        "(satisfies? p/IReversible $1)"
   "(instance? clojure.lang.IEditableCollection $1)" "(satisfies? p/IEditableCollection $1)"
   "(instance? clojure.lang.ITransientCollection $1)" "(satisfies? p/ITransient $1)"
   "(instance? clojure.lang.Symbol $1)"            "(t/symbol? $1)"
   "(instance? clojure.lang.Keyword $1)"           "(t/keyword? $1)"
   "(instance? clojure.lang.Delay $1)"             "(t/delay? $1)"
   "(instance? clojure.lang.Var $1)"               "(t/var? $1)"
   "(instance? clojure.lang.Ratio $1)"             "(t/ratio? $1)"
   "(instance? clojure.lang.BigInt $1)"            "(t/bigint? $1)"
   "(instance? String $1)"                         "(t/string? $1)"
   "(instance? Character $1)"                      "(t/char? $1)"
   "(instance? Number $1)"                         "(t/number? $1)"
   "(instance? Boolean $1)"                        "(or (true? $1) (false? $1))"
   "(instance? Integer $1)"                        "(t/int? $1)"
   "(instance? Long $1)"                           "(t/long? $1)"
   "(instance? Double $1)"                         "(t/double? $1)"
   "(instance? Float $1)"                          "(t/float? $1)"})

(def constructor-replacements
  "Type constructors → portable versions"
  {"clojure.lang.LazilyPersistentVector/create"  "t/vec"
   "clojure.lang.PersistentVector/create"        "t/vec"
   "clojure.lang.PersistentHashMap/create"       "t/hash-map"
   "clojure.lang.PersistentHashSet/create"       "t/hash-set"
   "clojure.lang.PersistentTreeMap/create"       "t/sorted-map"
   "clojure.lang.PersistentTreeSet/create"       "t/sorted-set"
   "clojure.lang.Symbol/intern"                  "t/symbol"
   "clojure.lang.Keyword/intern"                 "t/keyword"
   "clojure.lang.Keyword/find"                   "t/find-keyword"
   "(new clojure.lang.LazySeq"                   "(t/lazy-seq*"
   "(new clojure.lang.Delay"                     "(t/delay*"
   "clojure.lang.ChunkBuffer."                   "t/chunk-buffer"
   "clojure.lang.ChunkedCons."                   "t/chunk-cons"})

(def misc-replacements
  "Other replacements"
  {;; Remove type hints that reference JVM classes
   "^clojure.lang.ISeq"           ""
   "^clojure.lang.IPersistentMap" ""
   "^clojure.lang.Symbol"         ""
   "^clojure.lang.Keyword"        ""
   "^clojure.lang.IFn"            ""
   "^clojure.lang.IObj"           ""
   "^clojure.lang.IMeta"          ""
   "^clojure.lang.ChunkBuffer"    ""
   "^clojure.lang.IChunk"         ""
   "^clojure.lang.IChunkedSeq"    ""
   "^Class"                       ""
   "^String"                      ""
   "^Object"                      ""
   "^Number"                      ""
   "^StringBuilder"               ""
   "^Boolean"                     ""

   ;; Java interop → host
   "(new StringBuilder"           "(t/string-builder"
   "(. sb (append"                "(t/sb-append sb"
   "(. sb (toString))"            "(t/sb-to-string sb)"
   "(. x (toString))"             "(t/to-string x)"
   "(. f (applyTo"                "(t/apply-to f"
   "(. c (isInstance x))"         "(h/-instance? (h/host) c x)"
   "(. c (cast x))"               "(if (h/-instance? (h/host) c x) x (throw (ex-info \"Cast failed\" {})))"

   ;; Exceptions
   "(throw (IllegalArgumentException."  "(throw (ex-info"
   "(throw (IndexOutOfBoundsException." "(throw (ex-info \"Index out of bounds\""
   "(throw (ClassCastException."        "(throw (ex-info \"Cast failed\""})

;; =============================================================================
;; Regex-based transformations
;; =============================================================================

(def regex-replacements
  "Patterns that need regex replacement"
  [;; RT method calls: (. clojure.lang.RT (method args))
   [#"\(\. clojure\.lang\.RT \(first ([^)]+)\)\)"
    "(let [s# (p/-seq $1)] (when s# (p/-first s#)))"]
   [#"\(\. clojure\.lang\.RT \(next ([^)]+)\)\)"
    "(p/-next $1)"]
   [#"\(\. clojure\.lang\.RT \(more ([^)]+)\)\)"
    "(p/-rest $1)"]
   [#"\(\. clojure\.lang\.RT \(seq ([^)]+)\)\)"
    "(p/-seq $1)"]
   [#"\(\. clojure\.lang\.RT \(cons ([^)]+) ([^)]+)\)\)"
    "(t/cons $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(conj ([^)]+) ([^)]+)\)\)"
    "(p/-conj $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(assoc ([^)]+) ([^)]+) ([^)]+)\)\)"
    "(p/-assoc $1 $2 $3)"]
   [#"\(\. clojure\.lang\.RT \(count ([^)]+)\)\)"
    "(p/-count $1)"]
   [#"\(\. clojure\.lang\.RT \(nth ([^)]+) ([^)]+)\)\)"
    "(p/-nth $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(nth ([^)]+) ([^)]+) ([^)]+)\)\)"
    "(p/-nth $1 $2 $3)"]
   [#"\(\. clojure\.lang\.RT \(get ([^)]+) ([^)]+)\)\)"
    "(p/-lookup $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(get ([^)]+) ([^)]+) ([^)]+)\)\)"
    "(p/-lookup $1 $2 $3)"]
   [#"\(\. clojure\.lang\.RT \(contains ([^)]+) ([^)]+)\)\)"
    "(p/-contains-key? $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(dissoc ([^)]+) ([^)]+)\)\)"
    "(p/-dissoc $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(peek ([^)]+)\)\)"
    "(p/-peek $1)"]
   [#"\(\. clojure\.lang\.RT \(pop ([^)]+)\)\)"
    "(p/-pop $1)"]
   [#"\(\. clojure\.lang\.RT \(keys ([^)]+)\)\)"
    "(t/keys $1)"]
   [#"\(\. clojure\.lang\.RT \(vals ([^)]+)\)\)"
    "(t/vals $1)"]
   [#"\(\. clojure\.lang\.RT \(find ([^)]+) ([^)]+)\)\)"
    "(t/find $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(toArray ([^)]+)\)\)"
    "(h/-to-array (h/host) $1)"]

   ;; RT static calls: clojure.lang.RT/method
   [#"clojure\.lang\.RT/conj" "p/-conj"]
   [#"clojure\.lang\.RT/assoc" "p/-assoc"]
   [#"clojure\.lang\.RT/count" "p/-count"]
   [#"clojure\.lang\.RT/nth" "p/-nth"]
   [#"clojure\.lang\.RT/get" "p/-lookup"]
   [#"clojure\.lang\.RT/contains" "p/-contains-key?"]
   [#"clojure\.lang\.RT/dissoc" "p/-dissoc"]
   [#"clojure\.lang\.RT/peek" "p/-peek"]
   [#"clojure\.lang\.RT/pop" "p/-pop"]
   [#"clojure\.lang\.RT/intCast" "int"]
   [#"clojure\.lang\.RT/longCast" "long"]
   [#"clojure\.lang\.RT/floatCast" "float"]
   [#"clojure\.lang\.RT/doubleCast" "double"]
   [#"clojure\.lang\.RT/booleanCast" "boolean"]
   [#"clojure\.lang\.RT/isReduced" "reduced?"]
   [#"clojure\.lang\.RT/subvec" "t/subvec"]
   [#"clojure\.lang\.RT/iter" "t/iterator"]

   ;; Numbers method calls
   [#"\(\. clojure\.lang\.Numbers \(add ([^)]+) ([^)]+)\)\)"
    "(h/-add (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(addP ([^)]+) ([^)]+)\)\)"
    "(h/-add (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(minus ([^)]+) ([^)]+)\)\)"
    "(h/-subtract (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(minusP ([^)]+) ([^)]+)\)\)"
    "(h/-subtract (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(minus ([^)]+)\)\)"
    "(h/-negate (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(minusP ([^)]+)\)\)"
    "(h/-negate (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(multiply ([^)]+) ([^)]+)\)\)"
    "(h/-multiply (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(multiplyP ([^)]+) ([^)]+)\)\)"
    "(h/-multiply (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(divide ([^)]+) ([^)]+)\)\)"
    "(h/-divide (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(inc ([^)]+)\)\)"
    "(h/-inc (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(incP ([^)]+)\)\)"
    "(h/-inc (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(dec ([^)]+)\)\)"
    "(h/-dec (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(decP ([^)]+)\)\)"
    "(h/-dec (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(lt ([^)]+) ([^)]+)\)\)"
    "(h/-lt (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(lte ([^)]+) ([^)]+)\)\)"
    "(h/-lte (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(gt ([^)]+) ([^)]+)\)\)"
    "(h/-gt (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(gte ([^)]+) ([^)]+)\)\)"
    "(h/-gte (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(equiv ([^)]+) ([^)]+)\)\)"
    "(h/-num-equiv (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(isZero ([^)]+)\)\)"
    "(h/-zero? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(isPos ([^)]+)\)\)"
    "(h/-pos? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(isNeg ([^)]+)\)\)"
    "(h/-neg? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(max ([^)]+) ([^)]+)\)\)"
    "(if (h/-gt (h/host) $1 $2) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(min ([^)]+) ([^)]+)\)\)"
    "(if (h/-lt (h/host) $1 $2) $1 $2)"]
   [#"clojure\.lang\.Numbers/abs" "abs"]
   [#"clojure\.lang\.Numbers/quotient" "quot"]
   [#"clojure\.lang\.Numbers/remainder" "rem"]

   ;; Util calls
   [#"clojure\.lang\.Util/identical" "(h/-identical? (h/host))"]
   [#"clojure\.lang\.Util/equiv" "p/-equiv"]
   [#"clojure\.lang\.Util/hash" "p/-hash"]
   [#"clojure\.lang\.Util/compare" "p/-compare"]

   ;; instance? checks → satisfies?
   [#"\(instance\? clojure\.lang\.ISeq ([^)]+)\)"
    "(satisfies? p/ISeq $1)"]
   [#"\(instance\? clojure\.lang\.IPersistentMap ([^)]+)\)"
    "(satisfies? p/IMap $1)"]
   [#"\(instance\? clojure\.lang\.IPersistentVector ([^)]+)\)"
    "(satisfies? p/IIndexed $1)"]
   [#"\(instance\? clojure\.lang\.IPersistentSet ([^)]+)\)"
    "(satisfies? p/ISet $1)"]
   [#"\(instance\? clojure\.lang\.IPersistentCollection ([^)]+)\)"
    "(satisfies? p/ICollection $1)"]
   [#"\(instance\? clojure\.lang\.IDeref ([^)]+)\)"
    "(satisfies? p/IDeref $1)"]
   [#"\(instance\? clojure\.lang\.IMeta ([^)]+)\)"
    "(satisfies? p/IMeta $1)"]
   [#"\(instance\? clojure\.lang\.IObj ([^)]+)\)"
    "(satisfies? p/IWithMeta $1)"]
   [#"\(instance\? clojure\.lang\.Counted ([^)]+)\)"
    "(satisfies? p/ICounted $1)"]
   [#"\(instance\? clojure\.lang\.Indexed ([^)]+)\)"
    "(satisfies? p/IIndexed $1)"]
   [#"\(instance\? clojure\.lang\.IFn ([^)]+)\)"
    "(satisfies? p/IFn $1)"]
   [#"\(instance\? clojure\.lang\.Sequential ([^)]+)\)"
    "(satisfies? p/ISequential $1)"]
   [#"\(instance\? clojure\.lang\.Associative ([^)]+)\)"
    "(satisfies? p/IAssociative $1)"]
   [#"\(instance\? clojure\.lang\.Reversible ([^)]+)\)"
    "(satisfies? p/IReversible $1)"]
   [#"\(instance\? clojure\.lang\.IEditableCollection ([^)]+)\)"
    "(satisfies? p/IEditableCollection $1)"]
   [#"\(instance\? clojure\.lang\.IChunkedSeq ([^)]+)\)"
    "(t/chunked-seq? $1)"]
   [#"\(instance\? clojure\.lang\.Symbol ([^)]+)\)"
    "(t/symbol? $1)"]
   [#"\(instance\? clojure\.lang\.Keyword ([^)]+)\)"
    "(t/keyword? $1)"]
   [#"\(instance\? clojure\.lang\.Delay ([^)]+)\)"
    "(t/delay? $1)"]
   [#"\(instance\? clojure\.lang\.Var ([^)]+)\)"
    "(t/var? $1)"]
   [#"\(instance\? clojure\.lang\.Ratio ([^)]+)\)"
    "(t/ratio? $1)"]
   [#"\(instance\? clojure\.lang\.BigInt ([^)]+)\)"
    "(t/bigint? $1)"]
   [#"\(instance\? clojure\.lang\.MultiFn ([^)]+)\)"
    "(t/multifn? $1)"]
   [#"\(instance\? String ([^)]+)\)"
    "(t/string? $1)"]
   [#"\(instance\? Character ([^)]+)\)"
    "(t/char? $1)"]
   [#"\(instance\? Number ([^)]+)\)"
    "(t/number? $1)"]
   [#"\(instance\? Boolean ([^)]+)\)"
    "(or (true? $1) (false? $1))"]
   [#"\(instance\? Integer ([^)]+)\)"
    "(t/int? $1)"]
   [#"\(instance\? Long ([^)]+)\)"
    "(t/long? $1)"]
   [#"\(instance\? Double ([^)]+)\)"
    "(t/double? $1)"]
   [#"\(instance\? Float ([^)]+)\)"
    "(t/float? $1)"]
   [#"\(instance\? BigDecimal ([^)]+)\)"
    "(t/bigdec? $1)"]
   [#"\(instance\? BigInteger ([^)]+)\)"
    "(t/biginteger? $1)"]

   ;; Type constructors
   [#"clojure\.lang\.LazilyPersistentVector/create" "t/vec"]
   [#"clojure\.lang\.PersistentVector/create" "t/vec"]
   [#"clojure\.lang\.PersistentHashMap/create" "t/hash-map"]
   [#"clojure\.lang\.PersistentHashSet/create" "t/hash-set"]
   [#"clojure\.lang\.PersistentTreeMap/create" "t/sorted-map"]
   [#"clojure\.lang\.PersistentTreeSet/create" "t/sorted-set"]
   [#"clojure\.lang\.PersistentArrayMap/create" "t/array-map"]
   [#"clojure\.lang\.Symbol/intern" "t/symbol"]
   [#"clojure\.lang\.Keyword/intern" "t/keyword"]
   [#"clojure\.lang\.Keyword/find" "t/find-keyword"]
   [#"\(new clojure\.lang\.LazySeq" "(t/lazy-seq*"]
   [#"\(new clojure\.lang\.Delay" "(t/delay*"]
   [#"clojure\.lang\.ChunkBuffer\." "t/chunk-buffer"]
   [#"clojure\.lang\.ChunkedCons\." "t/chunk-cons"]

   ;; Remove JVM type hints
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

   ;; Java exceptions → ex-info
   [#"\(throw \(IllegalArgumentException\. \"([^\"]+)\"\)\)"
    "(throw (ex-info \"$1\" {}))"]
   [#"\(throw \(IndexOutOfBoundsException\.\)\)"
    "(throw (ex-info \"Index out of bounds\" {}))"]
   [#"\(throw \(ClassCastException\. \"([^\"]+)\"\)\)"
    "(throw (ex-info \"Cast failed: $1\" {}))"]
   [#"\(throw \(UnsupportedOperationException\. \"([^\"]+)\"\)\)"
    "(throw (ex-info \"Unsupported: $1\" {}))"]
   [#"\(throw \(ArithmeticException\. \"([^\"]+)\"\)\)"
    "(throw (ex-info \"Arithmetic error: $1\" {}))"]
   [#"\(throw \(RuntimeException\. \"([^\"]+)\"\)\)"
    "(throw (ex-info \"$1\" {}))"]

   ;; RT cast function calls: (. clojure.lang.RT (intCast x))
   [#"\(\. clojure\.lang\.RT \(intCast ([^)]+)\)\)"
    "(int $1)"]
   [#"\(\. clojure\.lang\.RT \(longCast ([^)]+)\)\)"
    "(long $1)"]
   [#"\(\. clojure\.lang\.RT \(floatCast ([^)]+)\)\)"
    "(float $1)"]
   [#"\(\. clojure\.lang\.RT \(doubleCast ([^)]+)\)\)"
    "(double $1)"]
   [#"\(\. clojure\.lang\.RT \(booleanCast ([^)]+)\)\)"
    "(boolean $1)"]
   [#"\(\. clojure\.lang\.RT \(charCast ([^)]+)\)\)"
    "(char $1)"]
   [#"\(\. clojure\.lang\.RT \(shortCast ([^)]+)\)\)"
    "(short $1)"]
   [#"\(\. clojure\.lang\.RT \(byteCast ([^)]+)\)\)"
    "(byte $1)"]

   ;; Unchecked casts
   [#"\(\. clojure\.lang\.RT \(uncheckedIntCast ([^)]+)\)\)"
    "(unchecked-int $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedLongCast ([^)]+)\)\)"
    "(unchecked-long $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedFloatCast ([^)]+)\)\)"
    "(unchecked-float $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedDoubleCast ([^)]+)\)\)"
    "(unchecked-double $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedCharCast ([^)]+)\)\)"
    "(unchecked-char $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedShortCast ([^)]+)\)\)"
    "(unchecked-short $1)"]
   [#"\(\. clojure\.lang\.RT \(uncheckedByteCast ([^)]+)\)\)"
    "(unchecked-byte $1)"]
   [#"clojure\.lang\.RT/uncheckedLongCast" "unchecked-long"]
   [#"clojure\.lang\.RT/uncheckedIntCast" "unchecked-int"]

   ;; RT misc calls
   [#"\(\. clojure\.lang\.RT \(nextID\)\)"
    "(t/next-id)"]
   [#"\(\. clojure\.lang\.RT \(subvec ([^)]+) ([^)]+) ([^)]+)\)\)"
    "(t/subvec $1 $2 $3)"]
   [#"\(\. clojure\.lang\.RT \(chunkIteratorSeq ([^)]+)\)\)"
    "(t/chunk-iterator-seq $1)"]
   [#"clojure\.lang\.RT/chunkIteratorSeq" "t/chunk-iterator-seq"]
   [#"clojure\.lang\.RT/seqToTypedArray" "t/seq-to-array"]

   ;; Remove ALL :inline entries - they're JVM compilation hints
   ;; These patterns handle various inline formats
   [#"\s*:inline \(fn \[[^\]]*\][^\n]+\n?\s*" ""]
   [#"\s*:inline-arities[^\n]+\n?\s*" ""]

   ;; Symbol intern
   [#"\(\. clojure\.lang\.Symbol \(intern ([^)]+)\)\)"
    "(t/symbol $1)"]

   ;; PersistentList creator
   [#"\(\. clojure\.lang\.PersistentList creator\)"
    "t/list"]

   ;; clojure.lang.Cons -> t/cons
   [#"clojure\.lang\.Cons\." "t/cons"]

   ;; Arithmetic bitwise operations
   [#"\(\. clojure\.lang\.Numbers \(and ([^)]+) ([^)]+)\)\)"
    "(h/-bit-and (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(or ([^)]+) ([^)]+)\)\)"
    "(h/-bit-or (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(xor ([^)]+) ([^)]+)\)\)"
    "(h/-bit-xor (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(not ([^)]+)\)\)"
    "(h/-bit-not (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers \(shiftLeft ([^)]+) ([^)]+)\)\)"
    "(h/-bit-shift-left (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(shiftRight ([^)]+) ([^)]+)\)\)"
    "(h/-bit-shift-right (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unsignedShiftRight ([^)]+) ([^)]+)\)\)"
    "(h/-unsigned-bit-shift-right (h/host) $1 $2)"]

   ;; Numbers static calls
   [#"clojure\.lang\.Numbers/add" "h/-add (h/host)"]
   [#"clojure\.lang\.Numbers/multiply" "h/-multiply (h/host)"]
   [#"clojure\.lang\.Numbers/divide" "h/-divide (h/host)"]
   [#"clojure\.lang\.Numbers/minus" "h/-subtract (h/host)"]
   [#"clojure\.lang\.Numbers/inc" "h/-inc (h/host)"]
   [#"clojure\.lang\.Numbers/dec" "h/-dec (h/host)"]
   [#"clojure\.lang\.Numbers/lt" "h/-lt (h/host)"]
   [#"clojure\.lang\.Numbers/lte" "h/-lte (h/host)"]
   [#"clojure\.lang\.Numbers/gt" "h/-gt (h/host)"]
   [#"clojure\.lang\.Numbers/gte" "h/-gte (h/host)"]
   [#"clojure\.lang\.Numbers/equiv" "h/-num-equiv (h/host)"]
   [#"clojure\.lang\.Numbers/isZero" "h/-zero? (h/host)"]
   [#"clojure\.lang\.Numbers/isPos" "h/-pos? (h/host)"]
   [#"clojure\.lang\.Numbers/isNeg" "h/-neg? (h/host)"]
   [#"clojure\.lang\.Numbers/and" "h/-bit-and (h/host)"]
   [#"clojure\.lang\.Numbers/or" "h/-bit-or (h/host)"]
   [#"clojure\.lang\.Numbers/xor" "h/-bit-xor (h/host)"]
   [#"clojure\.lang\.Numbers/not" "h/-bit-not (h/host)"]
   [#"clojure\.lang\.Numbers/shiftLeft" "h/-bit-shift-left (h/host)"]
   [#"clojure\.lang\.Numbers/shiftRight" "h/-bit-shift-right (h/host)"]
   [#"clojure\.lang\.Numbers/unsignedShiftRight" "h/-unsigned-bit-shift-right (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_add" "h/-add (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_minus" "h/-subtract (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_multiply" "h/-multiply (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_inc" "h/-inc (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_dec" "h/-dec (h/host)"]
   [#"clojure\.lang\.Numbers/unchecked_negate" "h/-negate (h/host)"]

   ;; Method calls on objects - common patterns
   [#"\.withMeta" "p/-with-meta"]
   [#"\.meta" "p/-meta"]

   ;; Missing RT casts
   [#"clojure\.lang\.RT/shortCast" "short"]
   [#"clojure\.lang\.RT/byteCast" "byte"]
   [#"clojure\.lang\.RT/charCast" "char"]

   ;; Util methods
   [#"clojure\.lang\.Util/equals" "="]
   [#"\(clojure\.lang\.Util/equals ([^)]+) ([^)]+)\)" "(= $1 $2)"]

   ;; PersistentHashMap create form
   [#"\(\. clojure\.lang\.PersistentHashMap \(create ([^)]+)\)\)"
    "(t/hash-map $1)"]
   [#"\(\. clojure\.lang\.PersistentArrayMap \(create ([^)]+)\)\)"
    "(t/array-map $1)"]

   ;; Force on Delay
   [#"\(\. clojure\.lang\.Delay \(force ([^)]+)\)\)"
    "(t/force $1)"]

   ;; instance? IPersistentList
   [#"\(instance\? clojure\.lang\.IPersistentList ([^)]+)\)"
    "(t/list? $1)"]

   ;; Quoted class names in macro definitions - transform to t/ calls
   [#"'clojure\.lang\.LazySeq" "'t/LazySeq"]
   [#"'clojure\.lang\.Delay" "'t/Delay"]

   ;; Type tags - remove JVM class references in metadata
   [#"\{:tag clojure\.lang\.[A-Za-z]+\}" ""]
   [#":tag clojure\.lang\.[A-Za-z]+" ""]
   [#"\{:tag \"\[Ljava\.lang\.Object;\"\}" ""]
   [#":tag \"\[Ljava\.lang\.Object;\"" ""]
   [#":tag \"\[\[Ljava\.lang\.Object;\"" ""]

   ;; Java imports - comment them out
   [#"\(import '\(java\.lang\.reflect Array\)\)"
    ";; (import '(java.lang.reflect Array)) - replaced with host array fns"]

   ;; IllegalArgumentException patterns not yet covered
   [#"\(throw \(IllegalArgumentException\.\s*\(str ([^)]+)\)\)\)"
    "(throw (ex-info $1 {}))"]
   [#"\(throw \(IllegalArgumentException\.\s*([^\)]+)\)\)"
    "(throw (ex-info $1 {}))"]

   ;; Inline macros with clojure.lang.Numbers - these are JVM optimizations
   ;; Replace with portable inline forms or remove entirely
   [#"`\(\. clojure\.lang\.Numbers \(~op ~([a-z]+)\)\)"
    "`((resolve (symbol \"clojure.core\" (name ~'op))) ~~'$1)"]
   [#"`\(\. clojure\.lang\.Numbers \(~op ~([a-z]+) ~([a-z]+)\)\)"
    "`((resolve (symbol \"clojure.core\" (name ~'op))) ~~'$1 ~~'$2)"]

   ;; clojure.lang.IFn reference - likely in definterface or protocol
   [#"clojure\.lang\.IFn" "p/IFn"]

   ;; More RT unchecked casts
   [#"clojure\.lang\.RT/uncheckedByteCast" "unchecked-byte"]
   [#"clojure\.lang\.RT/uncheckedShortCast" "unchecked-short"]
   [#"clojure\.lang\.RT/uncheckedFloatCast" "unchecked-float"]
   [#"clojure\.lang\.RT/uncheckedDoubleCast" "unchecked-double"]

   ;; Numbers unchecked methods (method call form)
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_inc ([^)]+)\)\)"
    "(unchecked-inc $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_dec ([^)]+)\)\)"
    "(unchecked-dec $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_add ([^)]+) ([^)]+)\)\)"
    "(unchecked-add $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_subtract ([^)]+) ([^)]+)\)\)"
    "(unchecked-subtract $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_multiply ([^)]+) ([^)]+)\)\)"
    "(unchecked-multiply $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_negate ([^)]+)\)\)"
    "(unchecked-negate $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_inc ([^)]+)\)\)"
    "(unchecked-inc $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_dec ([^)]+)\)\)"
    "(unchecked-dec $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_add ([^)]+) ([^)]+)\)\)"
    "(unchecked-add $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_subtract ([^)]+) ([^)]+)\)\)"
    "(unchecked-subtract $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_multiply ([^)]+) ([^)]+)\)\)"
    "(unchecked-multiply $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_long_negate ([^)]+)\)\)"
    "(unchecked-negate $1)"]

   ;; Util compare and hasheq
   [#"\(\. clojure\.lang\.Util \(compare ([^)]+) ([^)]+)\)\)"
    "(p/-compare $1 $2)"]
   [#"\(\. clojure\.lang\.Util \(hasheq ([^)]+)\)\)"
    "(p/-hash $1)"]

   ;; ISeq tag in with-meta
   [#"'clojure\.lang\.ISeq" "'p/ISeq"]

   ;; Java array and class references
   [#"\[Ljava\.lang\.Object;" "Array"]
   [#"\(\. Class \(forName \"[^\"]+\"\)\)" "(h/-array-class (h/host))"]
   [#"java\.lang\.annotation\.Annotation" "t/Annotation"]

   ;; Remaining IllegalArgumentException patterns
   [#"\(IllegalArgumentException\.\s*$" "(ex-info"]
   [#"\(IllegalArgumentException\." "(ex-info"]

   ;; RT readString and alength
   [#"clojure\.lang\.RT/readString" "t/read-string"]
   [#"\(\. clojure\.lang\.RT \(readString ([^)]+)\)\)" "(t/read-string $1)"]
   [#"\(\. clojure\.lang\.RT \(readString ([^)]+) ([^)]+)\)\)" "(t/read-string $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(alength ([^)]+)\)\)" "(h/-alength (h/host) $1)"]
   [#"\(\. clojure\.lang\.RT \(aclone ([^)]+)\)\)" "(h/-aclone (h/host) $1)"]
   [#"\(\. clojure\.lang\.RT \(aget ([^)]+) ([^)]+)\)\)" "(h/-aget (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.RT \(aset ([^)]+) ([^)]+) ([^)]+)\)\)" "(h/-aset (h/host) $1 $2 $3)"]

   ;; Numbers unchecked without int/long prefix
   [#"\(\. clojure\.lang\.Numbers \(unchecked_inc ([^)]+)\)\)" "(unchecked-inc $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_dec ([^)]+)\)\)" "(unchecked-dec $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_minus ([^)]+)\)\)" "(unchecked-negate $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_add ([^)]+) ([^)]+)\)\)" "(unchecked-add $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_subtract ([^)]+) ([^)]+)\)\)" "(unchecked-subtract $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_multiply ([^)]+) ([^)]+)\)\)" "(unchecked-multiply $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_negate ([^)]+)\)\)" "(unchecked-negate $1)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_divide ([^)]+) ([^)]+)\)\)" "(unchecked-divide $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_remainder ([^)]+) ([^)]+)\)\)" "(unchecked-remainder $1 $2)"]

   ;; Java annotation reflection - comment out or replace
   [#"java\.lang\.annotation\.Retention" "t/Retention"]
   [#"java\.lang\.annotation\.RetentionPolicy/RUNTIME" ":runtime"]
   [#"\.getAnnotation" "t/get-annotation"]
   [#"\.value" "t/annotation-value"]

   ;; More RT methods
   [#"clojure\.lang\.RT/addURL" "t/add-url"]
   [#"\(\. clojure\.lang\.RT object_array ([^)]+)\)" "(h/-array (h/host) $1)"]
   [#"clojure\.lang\.RT/REQUIRE_LOCK" "t/require-lock"]

   ;; Numbers quotient and unchecked_int_divide/remainder
   [#"\(\. clojure\.lang\.Numbers \(quotient ([^)]+) ([^)]+)\)\)" "(quot $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(remainder ([^)]+) ([^)]+)\)\)" "(rem $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_divide ([^)]+) ([^)]+)\)\)" "(unchecked-divide-int $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers \(unchecked_int_remainder ([^)]+) ([^)]+)\)\)" "(unchecked-remainder-int $1 $2)"]

   ;; java.lang types
   [#"java\.lang\.Enum" "t/Enum"]
   [#"java\.lang\.Object" "t/Object"]
   [#"java\.lang\.Class" "t/Class"]

   ;; More RT methods
   [#"clojure\.lang\.RT/load" "t/load-resource"]
   [#"clojure\.lang\.RT/canSeq" "t/seqable?"]
   [#"clojure\.lang\.RT/baseLoader" "t/base-loader"]

   ;; Numbers rationalize and bitwise (with space before args)
   [#"\(\. clojure\.lang\.Numbers \(rationalize ([^)]+)\)\)" "(t/rationalize $1)"]
   [#"\(\. clojure\.lang\.Numbers not ([^)]+)\)" "(h/-bit-not (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers and ([^)]+) ([^)]+)\)" "(h/-bit-and (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers or ([^)]+) ([^)]+)\)" "(h/-bit-or (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers xor ([^)]+) ([^)]+)\)" "(h/-bit-xor (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers andNot ([^)]+) ([^)]+)\)" "(h/-bit-and-not (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers clearBit ([^)]+) ([^)]+)\)" "(h/-bit-clear (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers setBit ([^)]+) ([^)]+)\)" "(h/-bit-set (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers flipBit ([^)]+) ([^)]+)\)" "(h/-bit-flip (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers testBit ([^)]+) ([^)]+)\)" "(h/-bit-test (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers shiftLeftInt ([^)]+) ([^)]+)\)" "(bit-shift-left (int $1) $2)"]
   [#"\(\. clojure\.lang\.Numbers shiftRightInt ([^)]+) ([^)]+)\)" "(bit-shift-right (int $1) $2)"]
   [#"\(\. clojure\.lang\.Numbers unsignedShiftRightInt ([^)]+) ([^)]+)\)" "(unsigned-bit-shift-right (int $1) $2)"]

   ;; Numbers shift methods (space-separated form without parens around method)
   [#"\(\. clojure\.lang\.Numbers shiftLeft ([^\)]+) ([^\)]+)\)"
    "(h/-bit-shift-left (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers shiftRight ([^\)]+) ([^\)]+)\)"
    "(h/-bit-shift-right (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers unsignedShiftRight ([^\)]+) ([^\)]+)\)"
    "(h/-unsigned-bit-shift-right (h/host) $1 $2)"]

   ;; Other Numbers methods in space form
   [#"\(\. clojure\.lang\.Numbers add ([^\)]+) ([^\)]+)\)"
    "(h/-add (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers minus ([^\)]+) ([^\)]+)\)"
    "(h/-subtract (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers multiply ([^\)]+) ([^\)]+)\)"
    "(h/-multiply (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers divide ([^\)]+) ([^\)]+)\)"
    "(h/-divide (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers inc ([^\)]+)\)"
    "(h/-inc (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers dec ([^\)]+)\)"
    "(h/-dec (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers negate ([^\)]+)\)"
    "(h/-negate (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers lt ([^\)]+) ([^\)]+)\)"
    "(h/-lt (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers lte ([^\)]+) ([^\)]+)\)"
    "(h/-lte (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers gt ([^\)]+) ([^\)]+)\)"
    "(h/-gt (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers gte ([^\)]+) ([^\)]+)\)"
    "(h/-gte (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers equiv ([^\)]+) ([^\)]+)\)"
    "(h/-num-equiv (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers isZero ([^\)]+)\)"
    "(h/-zero? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers isPos ([^\)]+)\)"
    "(h/-pos? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers isNeg ([^\)]+)\)"
    "(h/-neg? (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers max ([^\)]+) ([^\)]+)\)"
    "(if (h/-gt (h/host) $1 $2) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers min ([^\)]+) ([^\)]+)\)"
    "(if (h/-lt (h/host) $1 $2) $1 $2)"]

   ;; Numbers coercion and typed arrays
   [#"\(\. clojure\.lang\.Numbers \(num ([^)]+)\)\)" "(t/num $1)"]
   [#"\(\. clojure\.lang\.Numbers float_array ([^\)]+)\)" "(h/-float-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers double_array ([^\)]+)\)" "(h/-double-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers int_array ([^\)]+)\)" "(h/-int-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers long_array ([^\)]+)\)" "(h/-long-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers short_array ([^\)]+)\)" "(h/-short-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers byte_array ([^\)]+)\)" "(h/-byte-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers char_array ([^\)]+)\)" "(h/-char-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers boolean_array ([^\)]+)\)" "(h/-boolean-array (h/host) $1)"]
   [#"\(\. clojure\.lang\.Numbers float_array ([^\)]+) ([^\)]+)\)" "(h/-float-array (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers double_array ([^\)]+) ([^\)]+)\)" "(h/-double-array (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers int_array ([^\)]+) ([^\)]+)\)" "(h/-int-array (h/host) $1 $2)"]
   [#"\(\. clojure\.lang\.Numbers long_array ([^\)]+) ([^\)]+)\)" "(h/-long-array (h/host) $1 $2)"]

   ;; Numbers type coercion macros (backtick quoted)
   [#"`\(\. clojure\.lang\.Numbers booleans ~([a-z]+)\)" "`(t/booleans ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers bytes ~([a-z]+)\)" "`(t/bytes ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers chars ~([a-z]+)\)" "`(t/chars ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers shorts ~([a-z]+)\)" "`(t/shorts ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers ints ~([a-z]+)\)" "`(t/ints ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers longs ~([a-z]+)\)" "`(t/longs ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers floats ~([a-z]+)\)" "`(t/floats ~$1)"]
   [#"`\(\. clojure\.lang\.Numbers doubles ~([a-z]+)\)" "`(t/doubles ~$1)"]

   ;; Doc strings referencing JVM exceptions - leave as is (just descriptions)
   ;; ClassCastException and RuntimeException in docstrings are OK

   ;; ==========================================================================
   ;; Additional JVM references that need portable equivalents
   ;; ==========================================================================

   ;; LazilyPersistentVector method call
   [#"\(\. clojure\.lang\.LazilyPersistentVector \(create ([^)]+)\)\)"
    "(t/vec $1)"]

   ;; clojure.lang.protocols → p/
   [#"clojure\.lang\.protocols/IMapEntry" "p/IMapEntry"]
   [#"clojure\.lang\.protocols/-key" "p/-key"]
   [#"clojure\.lang\.protocols/-val" "p/-val"]
   [#"\(satisfies\? clojure\.lang\.protocols/IMapEntry" "(satisfies? p/IMapEntry"]

   ;; MultiFn
   [#"\(new clojure\.lang\.MultiFn" "(t/multifn"]
   [#":tag 'clojure\.lang\.MultiFn" ""]
   [#"\{:tag 'clojure\.lang\.MultiFn\}" ""]

   ;; Var thread binding operations
   [#"clojure\.lang\.Var/pushThreadBindings" "t/push-thread-bindings"]
   [#"clojure\.lang\.Var/popThreadBindings" "t/pop-thread-bindings"]
   [#"clojure\.lang\.Var/getThreadBindings" "t/get-thread-bindings"]
   [#"\(\. clojure\.lang\.Var \(find ([^)]+)\)\)" "(t/find-var $1)"]
   [#"clojure\.lang\.Var/cloneThreadBindingFrame" "t/clone-thread-binding-frame"]
   [#"clojure\.lang\.Var/resetThreadBindingFrame" "t/reset-thread-binding-frame"]

   ;; Agent
   [#"\(new clojure\.lang\.Agent" "(t/agent"]
   [#"\(let \[a \(new clojure\.lang\.Agent" "(let [a (t/agent"]
   [#"clojure\.lang\.Agent/pooledExecutor" "t/pooled-executor"]
   [#"clojure\.lang\.Agent/soloExecutor" "t/solo-executor"]
   [#"clojure\.lang\.Agent/releasePendingSends" "t/release-pending-sends"]
   [#"\(\. clojure\.lang\.Agent shutdown\)" "(t/shutdown-agents)"]

   ;; Ref
   [#"\(new clojure\.lang\.Ref" "(t/ref"]
   [#"\(instance\? clojure\.lang\.IBlockingDeref" "(satisfies? p/IBlockingDeref"]

   ;; Atom
   [#"\(new clojure\.lang\.Atom" "(t/atom"]

   ;; LockingTransaction (STM)
   [#"\(\. clojure\.lang\.LockingTransaction" "(t/run-in-transaction"]
   [#"clojure\.lang\.LockingTransaction/isRunning" "t/in-transaction?"]

   ;; Volatile
   [#"clojure\.lang\.Volatile\." "t/volatile"]
   [#":tag 'clojure\.lang\.Volatile" ""]
   [#"\(instance\? clojure\.lang\.Volatile" "(t/volatile?"]

   ;; TransformerIterator
   [#"clojure\.lang\.TransformerIterator/create" "t/transformer-iterator"]
   [#"clojure\.lang\.TransformerIterator/createMulti" "t/transformer-iterator-multi"]

   ;; Reduced
   [#"clojure\.lang\.Reduced\." "t/reduced"]

   ;; IDrop interface
   [#"\(instance\? clojure\.lang\.IDrop" "(satisfies? p/IDrop"]

   ;; Lazy sequences
   [#"clojure\.lang\.Cycle/create" "t/cycle"]
   [#"clojure\.lang\.Repeat/create" "t/repeat"]
   [#"clojure\.lang\.Iterate/create" "t/iterate"]
   [#"clojure\.lang\.LongRange/create" "t/range"]
   [#"clojure\.lang\.Range/create" "t/range"]

   ;; Compiler host expression (JVM-specific type resolution)
   ;; These are used in sigs function for tag resolution - stub them out
   [#"clojure\.lang\.Compiler\$HostExpr/maybeSpecialTag" "t/maybe-special-tag"]
   [#"clojure\.lang\.Compiler\$HostExpr/maybeClass" "t/maybe-class"]

   ;; Compiler operations
   [#"\(\. clojure\.lang\.Compiler \(eval ([^)]+)\)\)" "(t/eval $1)"]
   [#"\(\. clojure\.lang\.Compiler \(macroexpand1 ([^)]+)\)\)" "(t/macroexpand-1 $1)"]
   [#"\(\. clojure\.lang\.Compiler \(load ([^)]+)\)\)" "(t/load-reader $1)"]

   ;; BigInt
   [#"clojure\.lang\.BigInt/fromBigInteger" "t/bigint-from-biginteger"]
   [#"clojure\.lang\.BigInt/valueOf" "t/bigint"]

   ;; LispReader
   [#"\(\. clojure\.lang\.LispReader \(read ([^)]+) ([^)]+) ([^)]+) ([^)]+)\)\)"
    "(t/read-clojure $1 $2 $3 $4)"]
   [#"\(\. clojure\.lang\.LispReader \(read ([^)]+) ([^)]+)\)\)"
    "(t/read-clojure $1 $2)"]

   ;; Reflector
   [#"clojure\.lang\.Reflector/prepRet" "t/prep-ret"]
   [#"\(\. Array \(get ([^)]+) ([^)]+)\)\)" "(h/-aget (h/host) $1 $2)"]

   ;; PersistentStructMap
   [#"\(\. clojure\.lang\.PersistentStructMap \(createSlotMap ([^)]+)\)\)"
    "(t/create-slot-map $1)"]
   [#"\(\. clojure\.lang\.PersistentStructMap \(create ([^)]+) ([^)]+)\)\)"
    "(t/create-struct $1 $2)"]
   [#"\(\. clojure\.lang\.PersistentStructMap \(construct ([^)]+) ([^)]+)\)\)"
    "(t/construct-struct $1 $2)"]
   [#"\(\. clojure\.lang\.PersistentStructMap \(getAccessor ([^)]+) ([^)]+)\)\)"
    "(t/get-accessor $1 $2)"]

   ;; LineNumberingPushbackReader
   [#"clojure\.lang\.LineNumberingPushbackReader\." "t/line-numbering-reader"]
   [#"\(instance\? clojure\.lang\.LineNumberingPushbackReader" "(t/line-numbering-reader?"]

   ;; IReduceInit
   [#"\(instance\? clojure\.lang\.IReduceInit" "(satisfies? p/IReduceInit"]

   ;; Namespace operations
   [#"clojure\.lang\.Namespace/find" "t/find-ns"]
   [#"clojure\.lang\.Namespace/findOrCreate" "t/find-or-create-ns"]
   [#"clojure\.lang\.Namespace/remove" "t/remove-ns"]
   [#"clojure\.lang\.Namespace/all" "t/all-ns"]
   [#"\(instance\? clojure\.lang\.Namespace" "(t/namespace?"]

   ;; IChunk type tag
   [#":tag 'clojure\.lang\.IChunk" ""]
   [#"\{:tag 'clojure\.lang\.IChunk\}" ""]

   ;; More LispReader patterns (alternative forms)
   [#"\(\. clojure\.lang\.LispReader \(read ([^\)]+)\)\)" "(t/read-clojure $1)"]
   ;; LispReader with complex args including nested parens
   [#"\(\. clojure\.lang\.LispReader \(read ([^)]+) \(boolean ([^)]+)\) ([^)]+) ([^)]+)\)\)"
    "(t/read-clojure $1 (boolean $2) $3 $4)"]

   ;; More Var patterns
   [#"\(\.\. clojure\.lang\.Var create setDynamic\)" "t/create-dynamic-var"]
   ;; pushThreadBindings with nested calls - match the full structure
   [#"\(\. clojure\.lang\.Var \(pushThreadBindings \{clojure\.lang\.Compiler/LOADER\s+\([^}]+\)\}\)\)"
    "(t/push-thread-bindings {t/current-loader $1})"]
   ;; Simple pushThreadBindings without nested calls
   [#"\(\. clojure\.lang\.Var \(pushThreadBindings ([^)]+)\)\)"
    "(t/push-thread-bindings $1)"]
   [#"\(\. clojure\.lang\.Var \(popThreadBindings\)\)" "(t/pop-thread-bindings)"]
   [#"clojure\.lang\.Var/intern" "t/intern-var"]

   ;; Compiler patterns
   [#"clojure\.lang\.Compiler/maybeResolveIn" "t/maybe-resolve-in"]
   [#"\(\. clojure\.lang\.Compiler specials\)" "t/special-symbols"]
   [#"clojure\.lang\.Compiler/LOADER" "t/current-loader"]
   [#"clojure\.lang\.Compiler/LINE" "t/current-line"]
   [#"clojure\.lang\.Compiler/COLUMN" "t/current-column"]
   [#"clojure\.lang\.Compiler\$CompilerException\." "t/compiler-exception"]

   ;; PersistentArrayMap statics
   [#"clojure\.lang\.PersistentArrayMap/EMPTY" "t/empty-map"]
   [#"\(\. clojure\.lang\.PersistentArrayMap EMPTY\)" "t/empty-map"]

   ;; Named interface
   [#"\(instance\? clojure\.lang\.Named" "(t/named?"]

   ;; Import statement - comment out
   [#"\(import clojure\.lang\.[A-Za-z]+ clojure\.lang\.[A-Za-z]+\)"
    ";; import commented out - using portable equivalents"]
   [#"\(import clojure\.lang\.[A-Za-z]+\)"
    ";; import commented out - using portable equivalents"]

   ;; Murmur3 hashing
   [#"clojure\.lang\.Murmur3/mixCollHash" "t/mix-coll-hash"]
   [#"clojure\.lang\.Murmur3/hashOrdered" "t/hash-ordered"]
   [#"clojure\.lang\.Murmur3/hashUnordered" "t/hash-unordered"]

   ;; EnumerationSeq
   [#"clojure\.lang\.EnumerationSeq/create" "t/enumeration-seq"]

   ;; Fn and Sorted instance checks
   [#"\(instance\? clojure\.lang\.Fn" "(satisfies? p/IFn"]
   [#"\(instance\? clojure\.lang\.Sorted" "(satisfies? p/ISorted"]

   ;; IReduce interface
   [#"\(instance\? clojure\.lang\.IReduce" "(satisfies? p/IReduce"]

   ;; Docstring references (keep as is - just descriptions)
   ;; "clojure.lang.Var" in docstrings is informational

   ;; Protocol/interface references in extend-type etc.
   [#"clojure\.lang\.IKVReduce" "p/IKVReduce"]
   [#"clojure\.lang\.IDeref" "p/IDeref"]
   [#"clojure\.lang\.IBlockingDeref" "p/IBlockingDeref"]
   [#"clojure\.lang\.IPending" "p/IPending"]
   [#"clojure\.lang\.IReduceInit" "p/IReduceInit"]
   [#"clojure\.lang\.Sequential" "p/ISequential"]
   [#"clojure\.lang\.Seqable" "p/ISeqable"]

   ;; TaggedLiteral and ReaderConditional
   [#"\(instance\? clojure\.lang\.TaggedLiteral" "(t/tagged-literal?"]
   [#"clojure\.lang\.TaggedLiteral/create" "t/tagged-literal"]
   [#"\(instance\? clojure\.lang\.ReaderConditional" "(t/reader-conditional?"]
   [#"clojure\.lang\.ReaderConditional/create" "t/reader-conditional"]

   ;; RT/vector
   [#"\(clojure\.lang\.RT/vector" "(t/vec"]

   ;; Java exceptions
   ;; IllegalAccessError - just replace constructor, throw is already there
   [#"\(new java\.lang\.IllegalAccessError" "(ex-info \"Illegal access\""]
   [#"\(java\.lang\.UnsupportedOperationException\." "(ex-info \"Unsupported operation\""]
   [#"\(catch IllegalArgumentException" "(catch Exception"]

   ])

;; =============================================================================
;; Transform function
;; =============================================================================

(defn remove-inlines [text]
  "Remove all :inline and :inline-arities entries from metadata maps.
   These are JVM-specific compilation hints. Runs BEFORE other transformations."
  (-> text
      ;; Pattern: whole line with :inline that ends with ))} - replace line with just }
      ;; This handles:  :inline (fn [x] (list 'clojure.lang.Util/identical x nil))}
      (str/replace #"\n\s+:inline \(fn [^\n]+\)\}" "}")
      ;; Pattern: :inline with backtick-quoted form ending in ))}
      (str/replace #"\n\s+:inline \(fn [^\n]+\`[^\n]+\)\}" "}")
      ;; {:inline (fn ...) :added ...} - inline is first, followed by :added
      (str/replace #"\{:inline \(fn [^}]+\)\)\s*\n\s*:added" "{:added")
      ;; {:inline ...)} when it's the whole metadata or last entry
      (str/replace #"\{:inline \(fn [^}]+\)\}" "{}")
      ;; :inline as middle entry followed by another key
      (str/replace #":inline \(fn [^:}]+\)\)\s*\n\s*:" ":")
      ;; Remove any remaining :inline entries on their own line (no trailing })
      (str/replace #"\n\s*:inline \(fn [^}]+\)\)" "")
      ;; Remove :inline-arities on any line
      (str/replace #"\n?\s*:inline-arities[^\n]*" "")
      ;; Clean up resulting empty lines/whitespace in metadata
      (str/replace #"\{\s*\n\s*\n" "{\n")
      (str/replace #"\{\s*\n\s*:added" "{:added")
      (str/replace #"\{\s+:added" "{:added")
      (str/replace #"\{\n:added" "{:added")
      ;; Clean up doubled newlines
      (str/replace #"\n\n\n+" "\n\n")))

(defn cleanup-whitespace [text]
  "Clean up artifacts from transformations:
   - [ param] -> [param]
   - (  expr) -> ( expr)
   - Multiple spaces"
  (-> text
      ;; Fix [ param] patterns in parameter vectors
      (str/replace #"\[\s+([a-z])" "[$1")
      ;; Fix loose spaces after opening parens
      (str/replace #"\(\s{2,}" "( ")
      ;; Fix multiple spaces in general
      (str/replace #"  +" " ")
      ;; DON'T remove orphaned } - they may be valid closing braces!
      ;; Collapse :static true\n} into :static true} for cleaner output
      (str/replace #":static true\s*\n\s+\}" ":static true}")
      ;; Remove empty lines in metadata
      (str/replace #"\{\s*\n\s*\n" "{\n")))

(defn apply-replacements [text]
  (-> text
      ;; Remove inlines FIRST before other transformations
      ;; (so the patterns can match the original JVM-specific code)
      remove-inlines
      ;; Then apply all regex replacements
      (as-> t (reduce (fn [t [pattern replacement]]
                        (str/replace t pattern replacement))
                      t
                      regex-replacements))
      cleanup-whitespace))

(defn add-requires [text]
  "Add protocol/host/types requires to ns form"
  (str/replace text
               #"(?s)\(ns \^[^)]+\}\s+clojure\.core\)"
               "(ns ^{:doc \"The core Clojure language.\"
       :author \"Rich Hickey\"}
  clojure.core
  (:require [clojure.protocols :as p]
            [clojure.host :as h]
            [clojure.types :as t]))"))

(defn add-header [text]
  "Add portability header comment"
  (str ";; =============================================================================
;; PORTABLE CLOJURE CORE - Auto-generated from JVM core.cljc
;; =============================================================================
;; Transformed using script/portabilize_core.clj
;; JVM-specific calls replaced with protocol/host abstractions.
;; =============================================================================

" text))

(def jvm-patterns
  "Patterns that should NOT appear in portable output"
  [["clojure.lang.RT" #"clojure\.lang\.RT"]
   ["clojure.lang.Numbers" #"clojure\.lang\.Numbers"]
   ["clojure.lang.Util" #"clojure\.lang\.Util"]
   ["clojure.lang.Symbol" #"clojure\.lang\.Symbol"]
   ["clojure.lang.Keyword" #"clojure\.lang\.Keyword"]
   ["clojure.lang.PersistentVector" #"clojure\.lang\.PersistentVector"]
   ["clojure.lang.PersistentHashMap" #"clojure\.lang\.PersistentHashMap"]
   ["clojure.lang.PersistentHashSet" #"clojure\.lang\.PersistentHashSet"]
   ["clojure.lang.PersistentList" #"clojure\.lang\.PersistentList"]
   ["clojure.lang.LazySeq" #"clojure\.lang\.LazySeq"]
   ["clojure.lang.Delay" #"clojure\.lang\.Delay"]
   ["clojure.lang.ISeq" #"clojure\.lang\.ISeq"]
   ["clojure.lang.IPersistent" #"clojure\.lang\.IPersistent"]
   ["clojure.lang.IFn" #"clojure\.lang\.IFn"]
   ["java.lang" #"java\.lang"]
   ["IllegalArgumentException" #"IllegalArgumentException"]
   ["UnsupportedOperationException" #"UnsupportedOperationException"]
   ["IndexOutOfBoundsException" #"IndexOutOfBoundsException"]
   ["ClassCastException" #"ClassCastException"]
   ["RuntimeException" #"RuntimeException"]])

(defn check-balanced [text]
  "Check for balanced parentheses, brackets, and braces.
   Handles strings, comments, character literals, and regex patterns."
  (let [opens {\( \) \[ \] \{ \}}
        closes (set (vals opens))
        stack (atom [])]
    (loop [chars (seq text)
           line 1
           col 1
           in-string false
           in-regex false
           escape false]
      (if-let [c (first chars)]
        (cond
          ;; Handle escape sequences in strings/regex
          escape
          (recur (rest chars) line (inc col) in-string in-regex false)

          ;; Handle string boundaries
          (and (= c \") (not in-regex))
          (recur (rest chars) line (inc col) (not in-string) false false)

          ;; Handle regex pattern start: #"
          (and (= c \#) (= (second chars) \") (not in-string))
          (recur (drop 2 chars) line (+ col 2) false true false)

          ;; Handle regex end
          (and in-regex (= c \"))
          (recur (rest chars) line (inc col) false false false)

          ;; Handle escape char in string or regex
          (and (or in-string in-regex) (= c \\))
          (recur (rest chars) line (inc col) in-string in-regex true)

          ;; Skip string/regex contents
          (or in-string in-regex)
          (recur (rest chars) (if (= c \newline) (inc line) line)
                 (if (= c \newline) 1 (inc col)) in-string in-regex false)

          ;; Character literal: \x or \newline etc
          (= c \\)
          (let [next-char (second chars)]
            (if (and next-char (Character/isLetter next-char))
              ;; Could be \newline, \space, \tab, etc - skip until non-letter
              (let [remaining (drop-while #(Character/isLetter %) (rest chars))]
                (recur remaining line (+ col (- (count chars) (count remaining))) false false false))
              ;; Single char like \a, \(, etc - skip 2 chars
              (recur (drop 2 chars) line (+ col 2) false false false)))

          ;; Handle comment to end of line
          (= c \;)
          (let [remaining (drop-while #(not= % \newline) chars)]
            (recur remaining (inc line) 1 false false false))

          ;; Track newlines
          (= c \newline)
          (recur (rest chars) (inc line) 1 false false false)

          ;; Opening delimiter
          (contains? opens c)
          (do (swap! stack conj {:char c :line line :col col})
              (recur (rest chars) line (inc col) false false false))

          ;; Closing delimiter
          (contains? closes c)
          (if (empty? @stack)
            {:error :unmatched-close :char c :line line :col col}
            (let [top (peek @stack)
                  expected (opens (:char top))]
              (if (= c expected)
                (do (swap! stack pop)
                    (recur (rest chars) line (inc col) false false false))
                {:error :mismatched :expected expected :got c
                 :line line :col col :opened-at top})))

          :else
          (recur (rest chars) line (inc col) false false false))

        ;; End of input
        (if (empty? @stack)
          {:ok true}
          {:error :unclosed :unclosed @stack})))))

(defn verify-syntax [text output-file]
  "Verify basic syntax validity using Clojure reader if available"
  (println "\n=== Syntax Check ===")
  ;; Basic bracket balance check (may have false positives due to reader macros)
  (let [result (check-balanced text)]
    (if (:ok result)
      (println "  ✓ Brackets appear balanced")
      (println "  ⚠ Possible bracket issue (may be false positive from reader macros)"
               "\n    " (pr-str (select-keys result [:error :line :col])))))
  ;; Note: For authoritative syntax check, use:
  ;; clj -M -e "(load-file \"<output-file>\")"
  (println (format "  → For authoritative check: clj -M -e \"(load-file \\\"%s\\\")\"" output-file)))

(defn count-forms [text]
  "Count definition forms in the output"
  (println "\n=== Form Counts ===")
  (let [defn-count (count (re-seq #"\(defn\s" text))
        defmacro-count (count (re-seq #"\(defmacro\s" text))
        def-count (count (re-seq #"\(def\s" text))
        deftype-count (count (re-seq #"\(deftype\s" text))
        defrecord-count (count (re-seq #"\(defrecord\s" text))
        defprotocol-count (count (re-seq #"\(defprotocol\s" text))]
    (println (format "  defn:       %d" defn-count))
    (println (format "  defmacro:   %d" defmacro-count))
    (println (format "  def:        %d" def-count))
    (println (format "  deftype:    %d" deftype-count))
    (println (format "  defrecord:  %d" defrecord-count))
    (println (format "  defprotocol:%d" defprotocol-count))))

(defn verify-output [text]
  "Check for remaining JVM-specific patterns and report them"
  (println "\n=== JVM Pattern Check ===")
  (let [lines (str/split-lines text)
        all-issues (atom [])]
    (doseq [[name pattern] jvm-patterns]
      (let [matches (filter #(re-find pattern %) lines)
            match-count (count matches)]
        (when (pos? match-count)
          (swap! all-issues conj [name match-count])
          (println (format "  ⚠ %s: %d occurrences" name match-count))
          ;; Show first 3 examples
          (doseq [line (take 3 matches)]
            (let [line-num (inc (.indexOf (vec lines) line))]
              (println (format "      L%d: %s" line-num (str/trim (subs line 0 (min 80 (count line)))))))))))
    (if (empty? @all-issues)
      (println "  ✓ No JVM-specific patterns found!")
      (do
        (println (format "\n  Total: %d pattern types with %d total occurrences"
                        (count @all-issues)
                        (reduce + (map second @all-issues))))
        (println "  Run script again after adding more replacement patterns.")))))

(defn transform [input-file output-file]
  (let [content (slurp input-file)
        transformed (-> content
                        add-requires
                        apply-replacements
                        add-header)]
    (spit output-file transformed)
    (println "Transformed" input-file "→" output-file)
    (println "  Input:" (count (str/split-lines content)) "lines")
    (println "  Output:" (count (str/split-lines transformed)) "lines")
    ;; Run all verifications
    (verify-syntax transformed output-file)
    (count-forms transformed)
    (verify-output transformed)))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [input (or (first args) "src/clj/clojure/core.cljc")
        output (or (second args) "src/clj/clojure/core_portable.cljc")]
    (transform input output)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
