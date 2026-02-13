;; Copyright (c) Rich Hickey. All rights reserved.
;; Eclipse Public License 1.0

(ns clojure.protocols
  "Core protocols for Clojure data types.

   These define WHAT things can do. Types implement these.
   Organized by capability, ~25 essential protocols.

   This is the public API. Implementation details in clojure.lang.protocols.")

;; =============================================================================
;; Sequences - the core abstraction
;; =============================================================================

(defprotocol ISeqable
  "Things that can produce a sequence."
  (-seq [o] "Returns a seq, or nil if empty."))

(defprotocol ISeq
  "A sequence - first/rest."
  (-first [o] "Returns the first element.")
  (-rest [o] "Returns the rest (never nil, may be empty)."))

(defprotocol INext
  "Efficient next (returns nil, not empty seq)."
  (-next [o] "Returns seq of rest, or nil."))

(defprotocol ISequential
  "Marker: sequential things (vectors, lists).")

;; =============================================================================
;; Collections
;; =============================================================================

(defprotocol ICounted
  "Collections with O(1) count."
  (-count [o] "Returns count in constant time."))

(defprotocol IIndexed
  "Collections with O(~1) indexed access."
  (-nth [o i] [o i not-found] "Returns element at index."))

(defprotocol ICollection
  "Collections supporting conj."
  (-conj [o v] "Returns collection with v added."))

(defprotocol IEmptyable
  "Collections that can produce empty version."
  (-empty [o] "Returns empty collection of same type."))

(defprotocol IStack
  "Stack operations."
  (-peek [o] "Returns top element.")
  (-pop [o] "Returns collection without top."))

(defprotocol IReversible
  "Reversible collections."
  (-rseq [o] "Returns seq in reverse."))

;; =============================================================================
;; Associative
;; =============================================================================

(defprotocol ILookup
  "Key-based lookup."
  (-lookup [o k] [o k not-found] "Returns value for key."))

(defprotocol IAssociative
  "Key-value association."
  (-contains-key? [o k] "Returns true if k present.")
  (-assoc [o k v] "Returns collection with k->v."))

(defprotocol IMap
  "Map operations."
  (-dissoc [o k] "Returns map without key k."))

(defprotocol ISet
  "Set operations."
  (-disjoin [o v] "Returns set without value v."))

(defprotocol IMapEntry
  "A key-value entry."
  (-key [o] "Returns the key.")
  (-val [o] "Returns the value."))

;; =============================================================================
;; Equality & Hashing
;; =============================================================================

(defprotocol IEquiv
  "Value equality."
  (-equiv [o other] "Returns true if equal by value."))

(defprotocol IHash
  "Hashing for hash-based collections."
  (-hash [o] "Returns hash code."))

(defprotocol IComparable
  "Ordered comparison."
  (-compare [o other] "Returns -1, 0, or 1."))

;; =============================================================================
;; Metadata
;; =============================================================================

(defprotocol IMeta
  "Things with metadata."
  (-meta [o] "Returns metadata map."))

(defprotocol IWithMeta
  "Things that can hold metadata."
  (-with-meta [o meta] "Returns object with given metadata."))

;; =============================================================================
;; References
;; =============================================================================

(defprotocol IDeref
  "Dereferenceable things."
  (-deref [o] "Returns current value."))

(defprotocol IReset
  "Resettable references."
  (-reset! [o v] "Sets value, returns new value."))

(defprotocol ISwap
  "Swappable references."
  (-swap! [o f] [o f a] [o f a b] [o f a b args]
    "Atomically swaps using f."))

(defprotocol IWatchable
  "Watchable references."
  (-add-watch [o key f] "Add watch function.")
  (-remove-watch [o key] "Remove watch function."))

;; =============================================================================
;; Functions
;; =============================================================================

(defprotocol IFn
  "Invocable things."
  (-invoke
    [f] [f a] [f a b] [f a b c] [f a b c d]
    [f a b c d e] [f a b c d e g] [f a b c d e g h]
    [f a b c d e g h i] [f a b c d e g h i j]
    [f a b c d e g h i j k] [f a b c d e g h i j k l]
    [f a b c d e g h i j k l m] [f a b c d e g h i j k l m n]
    [f a b c d e g h i j k l m n o] [f a b c d e g h i j k l m n o p]
    [f a b c d e g h i j k l m n o p q] [f a b c d e g h i j k l m n o p q r]
    [f a b c d e g h i j k l m n o p q r s] [f a b c d e g h i j k l m n o p q r s t]
    [f a b c d e g h i j k l m n o p q r s t rest]
    "Invoke with args."))

;; =============================================================================
;; Reduction
;; =============================================================================

(defprotocol IReduce
  "Reducible collections."
  (-reduce [o f] [o f init] "Reduce with f."))

(defprotocol IKVReduce
  "Key-value reduction."
  (-kv-reduce [o f init] "Reduce over [k v] pairs."))

;; =============================================================================
;; Transients
;; =============================================================================

(defprotocol IEditableCollection
  "Collections supporting transients."
  (-as-transient [o] "Returns transient version."))

(defprotocol ITransient
  "Transient operations."
  (-conj! [o v] "Add value, return this.")
  (-persistent! [o] "Return persistent version."))

(defprotocol ITransientAssociative
  "Transient associative."
  (-assoc! [o k v] "Add k->v, return this."))

(defprotocol ITransientMap
  "Transient map."
  (-dissoc! [o k] "Remove key, return this."))

(defprotocol ITransientSet
  "Transient set."
  (-disjoin! [o v] "Remove value, return this."))

;; =============================================================================
;; Lazy / Pending
;; =============================================================================

(defprotocol IPending
  "Things not yet realized."
  (-realized? [o] "Returns true if realized."))

;; =============================================================================
;; Named
;; =============================================================================

(defprotocol INamed
  "Named things (symbols, keywords)."
  (-name [o] "Returns name string.")
  (-namespace [o] "Returns namespace string or nil."))

;; =============================================================================
;; Printing
;; =============================================================================

(defprotocol IPrint
  "Custom print representation."
  (-pr-str [o] "Returns print string."))
