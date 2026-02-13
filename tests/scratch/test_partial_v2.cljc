;; =============================================================================
;; PORTABLE CLOJURE CORE - Auto-generated via AST transformation
;; =============================================================================

(ns ^{:doc "The core Clojure language.", :author "Rich Hickey"} clojure.core)

(def unquote)

(def unquote-splicing)

(def ^{:arglists (quote ([& items])), :doc "Creates a new list containing the items.", :added "1.0"} list t/list-creator)

(def ^{:arglists (quote ([x seq])), :doc "Returns a new seq where x is the first element and seq is\r\n    the rest.", :added "1.0", :static true} cons (fn* ^{:static true} cons [x seq] (t/cons x seq)))

(def ^{:macro true, :added "1.0"} let (fn* let [&form &env & decl] (cons (quote let*) decl)))

(def ^{:macro true, :added "1.0"} loop (fn* loop [&form &env & decl] (cons (quote loop*) decl)))

(def ^{:macro true, :added "1.0"} fn (fn* fn [&form &env & decl] (.withMeta (cons (quote fn*) decl) (.meta &form))))

(def ^{:arglists (quote ([coll])), :doc "Returns the first item in the collection. Calls seq on its\r\n    argument. If coll is nil, returns nil.", :added "1.0", :static true} first (fn ^{:static true} first [coll] (clojure.core/let [s__206__auto__ (p/-seq coll)] (clojure.core/when s__206__auto__ (p/-first s__206__auto__)))))

(def ^{:arglists (quote ([coll])), :doc "Returns a seq of the items after the first. Calls seq on its\r\n  argument.  If there are no more items, returns nil.", :added "1.0", :static true} next (fn ^{:static true} next [x] (p/-next x)))

(def ^{:arglists (quote ([coll])), :doc "Returns a possibly empty seq of the items after the first. Calls seq on its\r\n  argument.", :added "1.0", :static true} rest (fn ^{:static true} rest [x] (p/-rest x)))

(def ^{:arglists (quote ([] [coll] [coll x] [coll x & xs])), :doc "conj[oin]. Returns a new collection with the xs\r\n    'added'. (conj nil item) returns (item).\r\n    (conj coll) returns coll. (conj) returns [].\r\n    The 'addition' may happen at different 'places' depending\r\n    on the concrete type.", :added "1.0", :static true} conj (fn ^{:static true} conj ([] []) ([coll] coll) ([coll x] (p/-conj coll x)) ([coll x & xs] (if xs (recur (p/-conj coll x) (first xs) (next xs)) (p/-conj coll x)))))

(def ^{:doc "Same as (first (next x))", :arglists (quote ([x])), :added "1.0", :static true} second (fn ^{:static true} second [x] (first (next x))))

(def ^{:doc "Same as (first (first x))", :arglists (quote ([x])), :added "1.0", :static true} ffirst (fn ^{:static true} ffirst [x] (first (first x))))

(def ^{:doc "Same as (next (first x))", :arglists (quote ([x])), :added "1.0", :static true} nfirst (fn ^{:static true} nfirst [x] (next (first x))))

(def ^{:doc "Same as (first (next x))", :arglists (quote ([x])), :added "1.0", :static true} fnext (fn ^{:static true} fnext [x] (first (next x))))

(def ^{:doc "Same as (next (next x))", :arglists (quote ([x])), :added "1.0", :static true} nnext (fn ^{:static true} nnext [x] (next (next x))))

(def ^{:arglists (quote ([coll])), :doc "Returns a seq on the collection. If the collection is\r\n    empty, returns nil.  (seq nil) returns nil. seq also works on\r\n    Strings, native Java arrays (of reference types) and any objects\r\n    that implement Iterable. Note that seqs cache values, thus seq\r\n    should not be used on any Iterable whose iterator repeatedly\r\n    returns the same mutable object.", :added "1.0", :static true} seq (fn ^{:static true} seq [coll] (p/-seq coll)))

(def ^{:arglists (quote ([c x])), :doc "Evaluates x and tests if it is an instance of the class\r\n    c. Returns true or false", :added "1.0"} instance? (fn instance? [c x] (. c (isInstance x))))

(def ^{:arglists (quote ([x])), :doc "Return true if x implements ISeq", :added "1.0", :static true} seq? (fn ^{:static true} seq? [x] ([x] ^{:line 225, :column 45} (satisfies? p/ISeq x) x)))

(def ^{:arglists (quote ([x])), :doc "Return true if x is a Character", :added "1.0", :static true} char? (fn ^{:static true} char? [x] ([x] ^{:line 261, :column 45} (t/char? x) x)))

(def ^{:arglists (quote ([x])), :doc "Return true if x is a String", :added "1.0", :static true} string? (fn ^{:static true} string? [x] ([x] ^{:line 260, :column 45} (t/string? x) x)))

(def ^{:arglists (quote ([x])), :doc "Return true if x implements IPersistentMap", :added "1.0", :static true} map? (fn ^{:static true} map? [x] ([x] ^{:line 226, :column 45} (satisfies? p/IMap x) x)))

