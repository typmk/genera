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

(def
 ^{:arglists '([obj m])
   :doc "Returns an object of the same type and value as obj, with
    map m as its metadata."
   :added "1.0"
   :static true}
 with-meta (fn ^:static with-meta [x m]
             (p/-with-meta x m)))

(def ^{:private true :dynamic true}
  assert-valid-fdecl (fn [fdecl]))

(def
 ^{:private true}
 sigs
 (fn [fdecl]
   (assert-valid-fdecl fdecl)
   (let [asig
         (fn [fdecl]
           (let [arglist (first fdecl)
                 ;elide implicit macro args
                 arglist (if (= '&form (first arglist))
                           (t/subvec arglist 2 (p/-count arglist))
                           arglist)
                 body (next fdecl)]
             (if (map? (first body))
               (if (next body)
                 (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                 arglist)
               arglist)))
         resolve-tag (fn [argvec]
                        (let [m (meta argvec)
                              tag (:tag m)]
                          (if (t/symbol? tag)
                            (if (= (.indexOf (.getName tag) ".") -1)
                              ;; simplified - skip host-specific class resolution
                              argvec
                              argvec)
                            argvec)))]
     (if (seq? (first fdecl))
       (loop [ret [] fdecls fdecl]
         (if fdecls
           (recur (conj ret (resolve-tag (asig (first fdecls)))) (next fdecls))
           (seq ret)))
       (list (resolve-tag (asig fdecl)))))))


(def
 ^{:arglists '([coll])
   :doc "Return the last item in coll, in linear time"
   :added "1.0"
   :static true}
 last (fn ^:static last [s]
        (if (next s)
          (recur (next s))
          (first s))))

(def
 ^{:arglists '([coll])
   :doc "Return a seq of all but the last item in coll, in linear time"
   :added "1.0"
   :static true}
 butlast (fn ^:static butlast [s]
           (loop [ret [] s s]
             (if (next s)
               (recur (conj ret (first s)) (next s))
               (seq ret)))))

(def

 ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}
 defn (fn defn [&form &env name & fdecl]
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
        (if (t/symbol? name)
          nil
          (throw (ex-info "First argument to defn must be a symbol" {:got name})))
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (sigs fdecl))} m)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (= 'fn ifn)
                        (if (t/symbol? iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (t/symbol (str (t/name name) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta name) (meta name) {}) m)]
          (list 'def (with-meta name m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
                ;;(cons `fn fdecl)
                (with-meta (cons `fn fdecl) {:rettag (:tag m)})))))

(. (var defn) (setMacro))

;; =============================================================================
;; After defn is defined
;; =============================================================================

(defn to-array
  "Returns an array of Objects containing the contents of coll."
  {:added "1.0"
   :static true}
  [coll] (h/-to-array (h/host) coll))

(defn cast
  "Throws if x is not compatible with type, else returns x."
  {:added "1.0"
   :static true}
  [type x]
  (if (h/-instance? (h/host) type x)
    x
    (throw (ex-info "Cannot cast" {:type type :value x}))))

(defn vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
  ([a b c d e] [a b c d e])
  ([a b c d e f] [a b c d e f])
  ([a b c d e f & args]
   (t/vec (cons a (cons b (cons c (cons d (cons e (cons f args)))))))))

(defn vec
  "Creates a new vector containing the contents of coll."
  {:added "1.0"
   :static true}
  [coll]
  (if (vector? coll)
    (with-meta coll nil)
    (t/vec coll)))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([] {})
  ([& keyvals]
   (t/hash-map keyvals)))

(defn hash-set
  "Returns a new hash set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([] #{})
  ([& keys]
   (t/hash-set keys)))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  [& keyvals]
  (t/sorted-map keyvals))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied
  comparator.  If any keys are equal, they are handled as if by
  repeated uses of assoc."
  {:added "1.0"
   :static true}
  [comparator & keyvals]
  (t/sorted-map-by comparator keyvals))

(defn sorted-set
  "Returns a new sorted set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  [& keys]
  (t/sorted-set keys))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied
  comparator.  Any equal keys are handled as if by repeated uses of
  conj."
  {:added "1.1"
   :static true}
  [comparator & keys]
  (t/sorted-set-by comparator keys))

;;;;;;;;;;;;;;;;;;;;
(defn nil?
  "Returns true if x is nil, false otherwise."
  {:added "1.0"
   :static true}
  [x] (h/-identical? (h/host) x nil))

(def

 ^{:doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
   :arglists '([name doc-string? attr-map? [params*] body]
                 [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :added "1.0"}
 defmacro (fn [&form &env
                name & args]
             (let [prefix (loop [p (list name) args args]
                            (let [f (first args)]
                              (if (string? f)
                                (recur (cons f p) (next args))
                                (if (map? f)
                                  (recur (cons f p) (next args))
                                  p))))
                   fdecl (loop [fd args]
                           (if (string? (first fd))
                             (recur (next fd))
                             (if (map? (first fd))
                               (recur (next fd))
                               fd)))
                   fdecl (if (vector? (first fdecl))
                           (list fdecl)
                           fdecl)
                   add-implicit-args (fn [fd]
                             (let [args (first fd)]
                               (cons (vec (cons '&form (cons '&env args))) (next fd))))
                   add-args (fn [acc ds]
                              (if (nil? ds)
                                acc
                                (let [d (first ds)]
                                  (if (map? d)
                                    (conj acc d)
                                    (recur (conj acc (add-implicit-args d)) (next ds))))))
                   fdecl (seq (add-args [] fdecl))
                   decl (loop [p prefix d fdecl]
                          (if p
                            (recur (next p) (cons (first p) d))
                            d))]
               (list 'do
                     (cons `defn decl)
                     (list '. (list 'var name) '(setMacro))
                     (list 'var name)))))


(. (var defmacro) (setMacro))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test nil (cons 'do body)))

(defn false?
  "Returns true if x is the value false, false otherwise."
  {:added "1.0"
   :static true}
  [x] (h/-identical? (h/host) x false))

(defn true?
  "Returns true if x is the value true, false otherwise."
  {:added "1.0"
   :static true}
  [x] (h/-identical? (h/host) x true))

(defn boolean?
  "Return true if x is a Boolean"
  {:added "1.9"}
  [x] (or (true? x) (false? x)))

(defn not
  "Returns true if x is logical false, false otherwise."
  {:added "1.0"
   :static true}
  [x] (if x false true))

(defn some?
  "Returns true if x is not nil, false otherwise."
  {:added "1.6"
   :static true}
  [x] (not (nil? x)))

(defn any?
  "Returns true given any argument."
  {:added "1.9"}
  [x] true)

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:added "1.0"
   :static true}
  ([] "")
  ([x]
   (if (nil? x) "" (t/to-string x)))
  ([x & ys]
   (let [sb (t/string-builder (str x))]
     (loop [more ys]
       (if more
         (do (t/sb-append sb (str (first more)))
             (recur (next more)))
         (t/sb-to-string sb))))))

(defn symbol?
  "Return true if x is a Symbol"
  {:added "1.0"
   :static true}
  [x] (t/symbol? x))

(defn keyword?
  "Return true if x is a Keyword"
  {:added "1.0"
   :static true}
  [x] (t/keyword? x))

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (ex-info "cond requires an even number of forms" {})))
          (cons 'clojure.core/cond (next (next clauses))))))

(defn symbol
  "Returns a Symbol with the given namespace and name. Arity-1 works
  on strings, keywords, and vars."
  {:added "1.0"
   :static true}
  ([name]
   (cond
     (symbol? name) name
     (string? name) (t/symbol name)
     (t/var? name) (t/var->symbol name)
     (keyword? name) (t/keyword->symbol name)
     :else (throw (ex-info "no conversion to symbol" {:value name}))))
  ([ns name] (t/symbol ns name)))

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  {:added "1.0"
   :static true}
  ([] (gensym "G__"))
  ([prefix-string] (t/symbol (str prefix-string (t/next-id)))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  {:added "1.0"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (t/keyword name)
                (string? name) (t/keyword name)))
  ([ns name] (t/keyword ns name)))

(defn find-keyword
  "Returns a Keyword with the given namespace and name if one already
  exists.  This function will not intern a new keyword. If the keyword
  has not already been interned, it will return nil.  Do not use :
  in the keyword strings, it will be added automatically."
  {:added "1.3"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (t/find-keyword name)
                (string? name) (t/find-keyword name)))
  ([ns name] (t/find-keyword ns name)))

(defn spread
  {:private true
   :static true}
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (next arglist)))))

(defn list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  ([f args]
   (t/apply-to f (seq args)))
  ([f x args]
   (t/apply-to f (list* x args)))
  ([f x y args]
   (t/apply-to f (list* x y args)))
  ([f x y z args]
   (t/apply-to f (list* x y z args)))
  ([f a b c d & args]
   (t/apply-to f (cons a (cons b (cons c (cons d (spread args))))))))

(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
 {:added "1.0"
   :static true}
 [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?"
  {:added "1.0"}
  [& body]
  `(t/lazy-seq* (fn* [] ~@body)))

;; =============================================================================
;; Chunked sequences (optional optimization, can be no-op for some platforms)
;; =============================================================================

(defn chunk-buffer [capacity]
  (t/chunk-buffer capacity))

(defn chunk-append [b x]
  (t/chunk-append b x))

(defn chunk [b]
  (t/chunk b))

(defn chunk-first [s]
  (t/chunk-first s))

(defn chunk-rest [s]
  (t/chunk-rest s))

(defn chunk-next [s]
  (t/chunk-next s))

(defn chunk-cons [chunk rest]
  (if (zero? (p/-count chunk))
    rest
    (t/chunk-cons chunk rest)))

(defn chunked-seq? [s]
  (t/chunked-seq? s))

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  {:added "1.0"
   :static true}
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
    (lazy-seq
      (let [s (seq x)]
        (if s
          (if (chunked-seq? s)
            (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
            (cons (first s) (concat (rest s) y)))
          y))))
  ([x y & zs]
   (let [cat (fn cat [xys zs]
               (lazy-seq
                 (let [xys (seq xys)]
                   (if xys
                     (if (chunked-seq? xys)
                       (chunk-cons (chunk-first xys)
                                   (cat (chunk-rest xys) zs))
                       (cons (first xys) (cat (rest xys) zs)))
                     (when zs
                       (cat (first zs) (next zs)))))))]
     (cat (concat x y) zs))))

;;;;;;;;;;;;;;;;at this point all the support for syntax-quote exists;;;;;;;;;;;;;;;;;;;;;;

(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?"
  {:added "1.0"}
  [& body]
  `(t/delay* (fn* [] ~@body)))

(defn delay?
  "returns true if x is a Delay created with delay"
  {:added "1.0?"
   :static true}
  [x] (t/delay? x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  {:added "1.0"
   :static true}
  [x] (if (delay? x) (deref x) x))

;; =============================================================================
;; Identity & Equality
;; =============================================================================

(defn identical?
  "Tests if 2 arguments are the same object"
  {:added "1.0"
   :static true}
  [x y] (h/-identical? (h/host) x y))

(defn =
  "Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (if (nil? x)
           (nil? y)
           (or (identical? x y)
               (p/-equiv x y))))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))

(defn not=
  "Same as (not (= obj1 obj2))"
  {:added "1.0"
   :static true}
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

;; =============================================================================
;; Comparison
;; =============================================================================

(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Uses IComparable if available, otherwise falls back to platform comparison."
  {:added "1.0"
   :static true}
  [x y]
  (if (identical? x y)
    0
    (if (satisfies? p/IComparable x)
      (p/-compare x y)
      (h/-compare (h/host) x y))))

;; =============================================================================
;; Arithmetic Operations
;; =============================================================================

(defn zero?
  "Returns true if num is zero, else false"
  {:added "1.0"}
  [num] (h/-zero? (h/host) num))

(defn pos?
  "Returns true if num is greater than zero, else false"
  {:added "1.0"
   :static true}
  [num] (h/-pos? (h/host) num))

(defn neg?
  "Returns true if num is less than zero, else false"
  {:added "1.0"
   :static true}
  [num] (h/-neg? (h/host) num))

;; Private reduce for bootstrap (before full reduce is defined)
(defn- reduce1
  ([f coll]
   (let [s (seq coll)]
     (if s
       (reduce1 f (first s) (next s))
       (f))))
  ([f val coll]
   (let [s (seq coll)]
     (if s
       (recur f (f val (first s)) (next s))
       val))))

(defn +
  "Returns the sum of nums. (+) returns 0."
  {:added "1.0"
   :static true}
  ([] 0)
  ([x] x)
  ([x y] (h/-add (h/host) x y))
  ([x y & more]
   (reduce1 + (+ x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  {:added "1.0"
   :static true}
  ([x] (h/-negate (h/host) x))
  ([x y] (h/-subtract (h/host) x y))
  ([x y & more]
   (reduce1 - (- x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  {:added "1.0"
   :static true}
  ([] 1)
  ([x] x)
  ([x y] (h/-multiply (h/host) x y))
  ([x y & more]
   (reduce1 * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  {:added "1.0"
   :static true}
  ([x] (/ 1 x))
  ([x y] (h/-divide (h/host) x y))
  ([x y & more]
   (reduce1 / (/ x y) more)))

(defn inc
  "Returns a number one greater than num."
  {:added "1.0"
   :static true}
  [x] (h/-inc (h/host) x))

(defn dec
  "Returns a number one less than num."
  {:added "1.0"
   :static true}
  [x] (h/-dec (h/host) x))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (h/-lt (h/host) x y))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (h/-lte (h/host) x y))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (h/-gt (h/host) x y))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (h/-gte (h/host) x y))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn ==
  "Returns non-nil if nums all have the equivalent
  value (type-independent), otherwise false"
  {:added "1.0"
   :static true}
  ([x] true)
  ([x y] (h/-num-equiv (h/host) x y))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn max
  "Returns the greatest of the nums."
  {:added "1.0"
   :static true}
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce1 max (max x y) more)))

(defn min
  "Returns the least of the nums."
  {:added "1.0"
   :static true}
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce1 min (min x y) more)))

(defn abs
  "Returns the absolute value of a."
  {:added "1.11"
   :static true}
  [a]
  (if (neg? a) (- a) a))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  {:added "1.0"
   :static true}
  [num div]
  (h/-quot (h/host) num div))

(defn rem
  "remainder of dividing numerator by denominator."
  {:added "1.0"
   :static true}
  [num div]
  (h/-rem (h/host) num div))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  {:added "1.0"
   :static true}
  [num div]
  (h/-mod (h/host) num div))

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
  [n]
  (zero? (rem n 2)))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
  [n]
  (not (even? n)))

;; =============================================================================
;; Bit Operations
;; =============================================================================

(defn bit-not
  "Bitwise complement"
  {:added "1.0"
   :static true}
  [x] (h/-bit-not (h/host) x))

(defn bit-and
  "Bitwise and"
  {:added "1.0"
   :static true}
  ([x y] (h/-bit-and (h/host) x y))
  ([x y & more]
   (reduce1 bit-and (bit-and x y) more)))

(defn bit-or
  "Bitwise or"
  {:added "1.0"
   :static true}
  ([x y] (h/-bit-or (h/host) x y))
  ([x y & more]
   (reduce1 bit-or (bit-or x y) more)))

(defn bit-xor
  "Bitwise exclusive or"
  {:added "1.0"
   :static true}
  ([x y] (h/-bit-xor (h/host) x y))
  ([x y & more]
   (reduce1 bit-xor (bit-xor x y) more)))

(defn bit-shift-left
  "Bitwise shift left"
  {:added "1.0"
   :static true}
  [x n] (h/-bit-shift-left (h/host) x n))

(defn bit-shift-right
  "Bitwise shift right"
  {:added "1.0"
   :static true}
  [x n] (h/-bit-shift-right (h/host) x n))

(defn unsigned-bit-shift-right
  "Bitwise shift right, without sign-extension."
  {:added "1.0"
   :static true}
  [x n] (h/-unsigned-bit-shift-right (h/host) x n))

;; =============================================================================
;; Collection Operations
;; =============================================================================

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  {:added "1.0"
   :static true}
  [coll]
  (if (nil? coll)
    0
    (p/-count coll)))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied."
  {:added "1.0"
   :static true}
  ([coll index]
   (p/-nth coll index))
  ([coll index not-found]
   (p/-nth coll index not-found)))

(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  {:added "1.0"
   :static true}
  [coll]
  (when coll (p/-peek coll)))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item."
  {:added "1.0"
   :static true}
  [coll]
  (p/-pop coll))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false."
  {:added "1.0"
   :static true}
  [coll key]
  (if (nil? coll)
    false
    (p/-contains-key? coll key)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  {:added "1.0"
   :static true}
  ([map key]
   (when map (p/-lookup map key)))
  ([map key not-found]
   (if map (p/-lookup map key not-found) not-found)))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  {:added "1.0"
   :static true}
  ([map key]
   (p/-dissoc map key))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.0"
   :static true}
  ([set key]
   (p/-disjoin set key))
  ([set key & ks]
   (let [ret (disj set key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn find
  "Returns the map entry for key, or nil if key not present."
  {:added "1.0"
   :static true}
  [map key]
  (when (and map (contains? map key))
    (t/map-entry key (get map key))))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  {:added "1.0"
   :static true}
  [map keyseq]
  (loop [ret {} keys (seq keyseq)]
    (if keys
      (let [entry (find map (first keys))]
        (recur
         (if entry
           (conj ret entry)
           ret)
         (next keys)))
      (with-meta ret (meta map)))))

(defn keys
  "Returns a sequence of the map's keys, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map]
  (when (seq map)
    (t/keys map)))

(defn vals
  "Returns a sequence of the map's values, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map]
  (when (seq map)
    (t/vals map)))

(defn key
  "Returns the key of the map entry."
  {:added "1.0"
   :static true}
  [e]
  (p/-key e))

(defn val
  "Returns the value in the map entry."
  {:added "1.0"
   :static true}
  [e]
  (p/-val e))

(defn rseq
  "Returns, in constant time, a seq of the items in rev (which
  can be a vector or sorted-map), in reverse order. If rev is empty returns nil"
  {:added "1.0"
   :static true}
  [rev]
  (p/-rseq rev))

(defn name
  "Returns the name String of a string, symbol or keyword."
  {:added "1.0"
   :static true}
  [x]
  (if (string? x)
    x
    (p/-name x)))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:added "1.0"
   :static true}
  [x]
  (p/-namespace x))

;; =============================================================================
;; Higher-Order Functions
;; =============================================================================

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  {:added "1.0"
   :static true}
  [coll]
  (reduce1 conj () coll))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted."
  {:added "1.0"
   :static true}
  ([f coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (if (chunked-seq? s)
         (let [c (chunk-first s)
               size (count c)
               b (chunk-buffer size)]
           (dotimes [i size]
             (chunk-append b (f (nth c i))))
           (chunk-cons (chunk b) (map f (chunk-rest s))))
         (cons (f (first s)) (map f (rest s)))))))
  ([f c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (when (and s1 s2)
         (cons (f (first s1) (first s2))
               (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
       (when (and s1 s2 s3)
         (cons (f (first s1) (first s2) (first s3))
               (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns logical true."
  {:added "1.0"
   :static true}
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [v (nth c i)]
              (when (pred v)
                (chunk-append b v))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s)]
          (if (pred f)
            (cons f (filter pred (rest s)))
            (filter pred (rest s))))))))

(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns logical false."
  {:added "1.0"
   :static true}
  [pred coll]
  (filter (complement pred) coll))

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  {:added "1.0"
   :static true}
  [f]
  (fn
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  {:added "1.0"
   :static true}
  [x]
  (fn [& args] x))

(defn identity
  "Returns its argument."
  {:added "1.0"
   :static true}
  [x] x)

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns."
  {:added "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y & zs] (f (apply g x y zs)))))
  ([f g & fs]
   (reduce1 comp (list* f g fs))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args."
  {:added "1.0"
   :static true}
  ([f] f)
  ([f arg1]
   (fn [& args] (apply f arg1 args)))
  ([f arg1 arg2]
   (fn [& args] (apply f arg1 arg2 args)))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f arg1 arg2 arg3 args)))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns."
  {:added "1.0"
   :static true}
  ([f]
   (fn
     ([] [(f)])
     ([x] [(f x)])
     ([x y] [(f x y)])
     ([x y & zs] [(apply f x y zs)])))
  ([f g]
   (fn
     ([] [(f) (g)])
     ([x] [(f x) (g x)])
     ([x y] [(f x y) (g x y)])
     ([x y & zs] [(apply f x y zs) (apply g x y zs)])))
  ([f g h]
   (fn
     ([] [(f) (g) (h)])
     ([x] [(f x) (g x) (h x)])
     ([x y] [(f x y) (g x y) (h x y)])
     ([x y & zs] [(apply f x y zs) (apply g x y zs) (apply h x y zs)])))
  ([f g h & fs]
   (let [fs (list* f g h fs)]
     (fn
       ([] (reduce1 #(conj %1 (%2)) [] fs))
       ([x] (reduce1 #(conj %1 (%2 x)) [] fs))
       ([x y] (reduce1 #(conj %1 (%2 x y)) [] fs))
       ([x y & zs] (reduce1 #(conj %1 (apply %2 x y zs)) [] fs))))))

;; =============================================================================
;; Reduce
;; =============================================================================

(deftype Reduced [val]
  p/IDeref
  (-deref [_] val))

(defn reduced
  "Wraps x so that reduce will terminate with the value x"
  {:added "1.5"
   :static true}
  [x]
  (Reduced. x))

(defn reduced?
  "Returns true if x is the result of a call to reduced"
  {:added "1.5"
   :static true}
  [x]
  (instance? Reduced x))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  {:added "1.7"
   :static true}
  [x]
  (if (reduced? x) x (reduced x)))

(defn unreduced
  "If x is reduced?, returns (deref x), else returns x"
  {:added "1.7"
   :static true}
  [x]
  (if (reduced? x) (deref x) x))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc."
  {:added "1.0"
   :static true}
  ([f coll]
   (if (satisfies? p/IReduce coll)
     (p/-reduce coll f)
     (let [s (seq coll)]
       (if s
         (reduce f (first s) (next s))
         (f)))))
  ([f val coll]
   (if (satisfies? p/IReduce coll)
     (p/-reduce coll f val)
     (loop [acc val s (seq coll)]
       (if s
         (let [ret (f acc (first s))]
           (if (reduced? ret)
             (deref ret)
             (recur ret (next s))))
         acc)))))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc."
  {:added "1.4"
   :static true}
  [f init coll]
  (if (satisfies? p/IKVReduce coll)
    (p/-kv-reduce coll f init)
    (reduce (fn [ret e] (f ret (key e) (val e))) init coll)))

;; =============================================================================
;; Sequences
;; =============================================================================

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  {:added "1.0"
   :static true}
  [pred coll]
  (cond
    (nil? (seq coll)) true
    (pred (first coll)) (recur pred (next coll))
    :else false))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil."
  {:added "1.0"
   :static true}
  [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s)) (recur pred (next s)))))

(def
 ^{:doc "Returns false if (pred x) is logical true for every x in
  coll, else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?))

(def
 ^{:doc "Returns false if (pred x) is logical true for any x in coll,
  else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-any? (comp not some))

;; =============================================================================
;; More sequence functions
;; =============================================================================

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  {:added "1.0"
   :static true}
  [n coll]
  (lazy-seq
    (when (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take (dec n) (rest s)))))))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll."
  {:added "1.0"
   :static true}
  [n coll]
  (let [step (fn [n coll]
               (let [s (seq coll)]
                 (if (and (pos? n) s)
                   (recur (dec n) (rest s))
                   s)))]
    (lazy-seq (step n coll))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns logical true."
  {:added "1.0"
   :static true}
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (when (pred (first s))
        (cons (first s) (take-while pred (rest s)))))))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the
  first item for which (pred item) returns logical false."
  {:added "1.0"
   :static true}
  [pred coll]
  (let [step (fn [pred coll]
               (let [s (seq coll)]
                 (if (and s (pred (first s)))
                   (recur pred (rest s))
                   s)))]
    (lazy-seq (step pred coll))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
  [(take n coll) (drop n coll)])

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  {:added "1.0"
   :static true}
  [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  {:added "1.0"
   :static true}
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc."
  {:added "1.0"
   :static true}
  [f x]
  (lazy-seq (cons x (iterate f (f x)))))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1,
  and end to infinity."
  {:added "1.0"
   :static true}
  ([] (iterate inc 0))
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step]
   (lazy-seq
     (let [comp (if (pos? step) < >)]
       (when (comp start end)
         (cons start (range (+ start step) end step)))))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  {:added "1.0"
   :static true}
  [coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (concat s (cycle s)))))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  {:added "1.0"
   :static true}
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

;; =============================================================================
;; Transients
;; =============================================================================

(defn transient
  "Returns a new, transient version of the collection, in constant time."
  {:added "1.1"
   :static true}
  [coll]
  (p/-as-transient coll))

(defn persistent!
  "Returns a new, persistent version of the transient collection, in
  constant time. The transient collection cannot be used after this
  call, any such use will throw an exception."
  {:added "1.1"
   :static true}
  [tcoll]
  (p/-persistent! tcoll))

(defn conj!
  "Adds x to the transient collection, and return coll. The 'addition'
  may happen at different 'places' depending on the concrete type."
  {:added "1.1"
   :static true}
  ([] (transient []))
  ([coll] coll)
  ([coll x]
   (p/-conj! coll x)))

(defn assoc!
  "When applied to a transient map, adds mapping of key(s) to
  val(s). Returns coll."
  {:added "1.1"
   :static true}
  ([coll key val]
   (p/-assoc! coll key val))
  ([coll key val & kvs]
   (let [ret (assoc! coll key val)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn dissoc!
  "Returns a transient map that doesn't contain a mapping for key(s)."
  {:added "1.1"
   :static true}
  ([coll key]
   (p/-dissoc! coll key))
  ([coll key & ks]
   (let [ret (dissoc! coll key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn disj!
  "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.1"
   :static true}
  ([set key]
   (p/-disjoin! set key))
  ([set key & ks]
   (let [ret (disj! set key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

;; =============================================================================
;; More collection functions
;; =============================================================================

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"
   :static true}
  ([] [])
  ([to] to)
  ([to from]
   (if (satisfies? p/IEditableCollection to)
     (persistent! (reduce conj! (transient to) from))
     (reduce conj to from)))
  ([to xform from]
   (if (satisfies? p/IEditableCollection to)
     (persistent! (transduce xform conj! (transient to) from))
     (transduce xform conj to from))))

(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted."
  {:added "1.4"
   :static true}
  ([f coll]
   (persistent! (reduce (fn [v o] (conj! v (f o))) (transient []) coll)))
  ([f c1 c2]
   (into [] (map f c1 c2)))
  ([f c1 c2 c3]
   (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
   (into [] (apply map f c1 c2 c3 colls))))

(defn filterv
  "Returns a vector of the items in coll for which
  (pred item) returns logical true."
  {:added "1.4"
   :static true}
  [pred coll]
  (persistent! (reduce (fn [v o] (if (pred o) (conj! v o) v)) (transient []) coll)))

;; =============================================================================
;; Atoms (using kernel types)
;; =============================================================================

(defn atom
  "Creates and returns an Atom with an initial value of x."
  {:added "1.0"
   :static true}
  ([x] (t/atom x))
  ([x & {:keys [meta validator]}]
   (t/atom x :meta meta :validator validator)))

(defn deref
  "Returns the current value of ref."
  {:added "1.0"
   :static true}
  [ref]
  (p/-deref ref))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  {:added "1.0"
   :static true}
  [atom newval]
  (p/-reset! atom newval))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args)."
  {:added "1.0"
   :static true}
  ([atom f] (p/-swap! atom f))
  ([atom f x] (p/-swap! atom f x))
  ([atom f x y] (p/-swap! atom f x y))
  ([atom f x y & args] (p/-swap! atom f x y args)))

(defn add-watch
  "Adds a watch function to an agent/atom/var/ref reference."
  {:added "1.0"
   :static true}
  [reference key fn]
  (p/-add-watch reference key fn))

(defn remove-watch
  "Removes a watch function from a reference."
  {:added "1.0"
   :static true}
  [reference key]
  (p/-remove-watch reference key))

;; =============================================================================
;; Type Predicates
;; =============================================================================

(defn coll?
  "Returns true if x implements IPersistentCollection"
  {:added "1.0"
   :static true}
  [x] (satisfies? p/ICollection x))

(defn list?
  "Returns true if x implements IPersistentList"
  {:added "1.0"
   :static true}
  [x] (and (satisfies? p/ISeq x) (not (vector? x))))

(defn seqable?
  "Return true if the seq function is supported for x"
  {:added "1.9"
   :static true}
  [x] (satisfies? p/ISeqable x))

(defn ifn?
  "Returns true if x implements IFn."
  {:added "1.0"
   :static true}
  [x] (satisfies? p/IFn x))

(defn fn?
  "Returns true if x is a fn."
  {:added "1.0"
   :static true}
  [x] (or (satisfies? p/IFn x) (h/-fn? (h/host) x)))

(defn associative?
  "Returns true if coll implements Associative"
  {:added "1.0"
   :static true}
  [coll] (satisfies? p/IAssociative coll))

(defn sequential?
  "Returns true if coll implements Sequential"
  {:added "1.0"
   :static true}
  [coll] (satisfies? p/ISequential coll))

(defn sorted?
  "Returns true if coll implements Sorted"
  {:added "1.0"
   :static true}
  [coll] (satisfies? p/ISorted coll))

(defn counted?
  "Returns true if coll implements count in constant time"
  {:added "1.0"
   :static true}
  [coll] (satisfies? p/ICounted coll))

(defn empty?
  "Returns true if coll has no items."
  {:added "1.0"
   :static true}
  [coll] (not (seq coll)))

(defn reversible?
  "Returns true if coll implements Reversible"
  {:added "1.0"
   :static true}
  [coll] (satisfies? p/IReversible coll))

(defn indexed?
  "Return true if coll implements Indexed"
  {:added "1.9"
   :static true}
  [coll] (satisfies? p/IIndexed coll))

(defn set?
  "Returns true if x implements IPersistentSet"
  {:added "1.0"
   :static true}
  [x] (satisfies? p/ISet x))

(defn realized?
  "Returns true if x has been realized"
  {:added "1.3"
   :static true}
  [x]
  (p/-realized? x))

;; =============================================================================
;; Empty and Not-Empty
;; =============================================================================

(defn not-empty
  "If coll is empty, returns nil, else coll"
  {:added "1.0"
   :static true}
  [coll] (when (seq coll) coll))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  {:added "1.0"
   :static true}
  [coll]
  (when (satisfies? p/IEmptyable coll)
    (p/-empty coll)))

;; =============================================================================
;; Hash
;; =============================================================================

(defn hash
  "Returns the hash code of its argument."
  {:added "1.0"
   :static true}
  [x]
  (if (satisfies? p/IHash x)
    (p/-hash x)
    (h/-hash (h/host) x)))

;; =============================================================================
;; Print
;; =============================================================================

(defn pr-str
  "pr to a string"
  {:added "1.0"
   :static true}
  [& xs]
  (apply str (interpose " " (map #(if (satisfies? p/IPrint %) (p/-pr-str %) (str %)) xs))))

(defn print-str
  "print to a string"
  {:added "1.0"
   :static true}
  [& xs]
  (apply str (interpose " " xs)))

;; =============================================================================
;; Interpose/Interleave
;; =============================================================================

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep."
  {:added "1.0"
   :static true}
  [sep coll]
  (drop 1 (interleave (repeat sep) coll)))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (when (and s1 s2)
         (cons (first s1) (cons (first s2)
                                (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (map seq (conj colls c2 c1))]
       (when (every? identity ss)
         (concat (map first ss) (apply interleave (map rest ss))))))))

;; =============================================================================
;; TODO: Continue with more functions as needed...
;; =============================================================================
