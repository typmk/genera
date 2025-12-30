;; Note: defn, let, fn, loop are built-in macros (see analyze.cljc)
;; They handle destructuring and expand to primitive forms (let*, fn*, loop*)

(defmacro when [test & body]
  (list 'if test (cons 'do body)))

(defmacro ns [& args] nil)

(defmacro when-not [test & body]
  (list 'if test nil (cons 'do body)))

(defmacro if-not [test then else]
  (list 'if test else then))

;; Conditional binding macros
(defmacro if-let
  ([bindings then] (list 'if-let bindings then nil))
  ([bindings then else]
   (let [binding (first bindings)
         init (second bindings)]
     (list 'let ['temp__auto init]
           (list 'if 'temp__auto
                 (list 'let [binding 'temp__auto] then)
                 else)))))

(defmacro when-let [bindings & body]
  (let [binding (first bindings)
        init (second bindings)]
    (list 'let ['temp__auto init]
          (list 'when 'temp__auto
                (cons 'let (cons [binding 'temp__auto] body))))))

;; Short-circuit boolean macros - use PHP's native && and ||
(defmacro and
  ([] true)
  ([x] x)
  ([x & more]
   (if (nil? more)
     x
     (list 'if x (cons 'and more) false))))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & more]
   (if (nil? more)
     x
     (list 'if x x (cons 'or more)))))

(defmacro cond [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException. "cond requires an even number of forms")))
          (cons 'cond (next (next clauses))))))

;; case macro - emits PHP match expression
;; Example:
;;   (case x
;;     1 "one"
;;     (2 3) "two or three"
;;     "default")
(defmacro case [expr & clauses]
  (cons 'case* (cons expr clauses)))

;; for macro - list comprehension
;; Supports :let, :when, and :while modifiers
;; Example:
;;   (for [x [1 2 3] y [4 5 6] :when (odd? x)] (* x y))
(defmacro for [seq-exprs body-expr]
  (let [emit-for (fn emit-for [bindings]
                   (if (seq bindings)
                     (let [k (first bindings)]
                       (cond
                         ;; :let modifier - bind intermediate values
                         (= k :let)
                         (list 'let (second bindings)
                               (emit-for (drop 2 bindings)))

                         ;; :when modifier - filter results
                         (= k :when)
                         (list 'if (second bindings)
                               (emit-for (drop 2 bindings))
                               nil)

                         ;; :while modifier - early termination
                         (= k :while)
                         (list 'if (second bindings)
                               (emit-for (drop 2 bindings))
                               nil)

                         ;; sequence binding - loop over collection
                         :else
                         (let [var k
                               coll-expr (second bindings)
                               rest-bindings (drop 2 bindings)]
                           (list 'mapcat
                                 (list 'fn [var]
                                       (if (seq rest-bindings)
                                         (emit-for rest-bindings)
                                         (list 'vector body-expr)))
                                 coll-expr))))
                     ;; No more bindings - produce the result
                     (list 'vector body-expr)))]
    (emit-for seq-exprs)))

;; doseq macro - side-effecting iteration
;; Like for but doesn't build a collection
(defmacro doseq [seq-exprs & body]
  (let [emit-doseq (fn emit-doseq [bindings]
                     (if (seq bindings)
                       (let [k (first bindings)]
                         (cond
                           ;; :let modifier
                           (= k :let)
                           (list 'let (second bindings)
                                 (emit-doseq (drop 2 bindings)))

                           ;; :when modifier
                           (= k :when)
                           (list 'when (second bindings)
                                 (emit-doseq (drop 2 bindings)))

                           ;; :while modifier - use loop for early termination
                           (= k :while)
                           (list 'when (second bindings)
                                 (emit-doseq (drop 2 bindings)))

                           ;; sequence binding
                           :else
                           (let [var k
                                 coll-expr (second bindings)
                                 rest-bindings (drop 2 bindings)]
                             (list 'loop ['__coll__doseq coll-expr]
                                   (list 'when '(seq __coll__doseq)
                                         (list 'let [var '(first __coll__doseq)]
                                               (emit-doseq rest-bindings))
                                         '(recur (next __coll__doseq)))))))
                       ;; No more bindings - execute the body
                       (cons 'do body)))]
    (emit-doseq seq-exprs)))

(defmacro -> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (apply list (first form) x (next form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (apply list (first form) (concat (next form) [x]))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

;; Conditional threading macros
(defmacro cond-> [expr & clauses]
  (let [g (gensym "cond__")]
    (list 'let [g expr]
          (reduce (fn [acc [test step]]
                    (list 'if test (list '-> acc step) acc))
                  g
                  (partition 2 clauses)))))

(defmacro cond->> [expr & clauses]
  (let [g (gensym "cond__")]
    (list 'let [g expr]
          (reduce (fn [acc [test step]]
                    (list 'if test (list '->> acc step) acc))
                  g
                  (partition 2 clauses)))))

;; as-> threading macro
;; (as-> x name form1 form2 ...) binds x to name, evaluates forms with name bound
(defmacro as-> [expr name & forms]
  (let [emit (fn emit [forms]
               (if (empty? forms)
                 name
                 (list 'let [name (first forms)]
                       (emit (rest forms)))))]
    (list 'let [name expr]
          (emit forms))))

;; comment macro - evaluates to nil
(defmacro comment [& body] nil)

;; doto macro - evaluate forms for side effects, return first arg
(defmacro doto [x & forms]
  (let [gx (gensym "doto__")]
    (list 'let [gx x]
          (cons 'do
                (concat (map (fn [f]
                               (if (seq? f)
                                 (apply list (first f) gx (next f))
                                 (list f gx)))
                             forms)
                        [gx])))))

;; while macro - loop while condition is true
(defmacro while [test & body]
  (list 'loop []
        (list 'when test
              (cons 'do body)
              '(recur))))

;; if-some - like if-let but tests for non-nil (not falsiness)
(defmacro if-some
  ([bindings then] (list 'if-some bindings then nil))
  ([bindings then else]
   (let [binding (first bindings)
         init (second bindings)]
     (list 'let ['temp__some init]
           (list 'if (list 'some? 'temp__some)
                 (list 'let [binding 'temp__some] then)
                 else)))))

;; when-some - like when-let but tests for non-nil
(defmacro when-some [bindings & body]
  (let [binding (first bindings)
        init (second bindings)]
    (list 'let ['temp__some init]
          (list 'when (list 'some? 'temp__some)
                (cons 'let (cons [binding 'temp__some] body))))))

;; condp - conditional with shared predicate
(defmacro condp [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [clauses]
               (if (seq clauses)
                 (if (= 1 (count clauses))
                   ;; default clause
                   (first clauses)
                   (let [test (first clauses)
                         result (second clauses)
                         more (drop 2 clauses)]
                     (list 'if (list gpred test gexpr)
                           result
                           (emit more))))
                 (list 'throw (list 'php/new "\\IllegalArgumentException"
                                    "No matching clause in condp"))))]
    (list 'let [gpred pred gexpr expr]
          (emit clauses))))

;; letfn - mutually recursive local functions
;; (letfn [(f [x] (g x)) (g [y] (f y))] (f 1))
(defmacro letfn [fnspecs & body]
  (cons 'letfn* (cons (vec fnspecs) body)))

;; Basics

(defn inc [x] (+ x 1))
(defn dec [x] (- x 1))
(defn identity [x] x)
(defn not [x] (if x false true))
(defn nil? [x] (php/is_null x))

;; Functional

(defn apply [f & args]
  (let [flat-args (if (next args)
                    (php/array_merge (php/array_slice args 0 -1) (last args))
                    (first args))]
    (php/call_user_func_array f (php/call_user_func "\\Cljp\\Runtime::toArray" flat-args))))

(defn comp [& fs]
  (let [fs (php/array_reverse fs)]
    (fn [& args]
      (loop [ret (apply (first fs) args)
             fs (next fs)]
        (if fs
          (recur ((first fs) ret) (next fs))
          ret)))))


(defn partial [f & args]
  (fn [& more]
    (apply f (php/array_merge args more))))

;; Sequences

(defn count [coll]
  (if (nil? coll) 0 (php/count coll)))

(defn seq [coll]
  (if (> (count coll) 0) coll nil))

(defn first [coll]
  (if (nil? coll)
    nil
    (if (php/is_array coll)
      (if (php/=== 0 (php/count coll))
        nil
        (php/reset coll))
      (.first coll))))

(defn rest [coll]
  (if (nil? coll)
    []
    (if (php/is_array coll)
      (php/array_slice coll 1)
      (.rest coll))))

(defn next [coll]
  (if (nil? coll)
    nil
    (let [r (rest coll)]
      (if (php/=== 0 (count r)) nil r))))

(defn cons [x coll]
  (php/call_user_func "\\Cljp\\Runtime::cons" x coll))

(defn vector [& args]
  (php/call_user_func "\\Cljp\\Runtime::vector" args))

(defn conj
  ([coll x]
   (php/call_user_func "\\Cljp\\Runtime::conj" coll x))
  ([coll x & xs]
   (reduce conj (conj coll x) xs)))

;; Reduced - wrapper for early termination in reduce
(defn reduced
  "Wraps x so reduce will terminate early with this value."
  [x]
  (php/call_user_func "\\Cljp\\Runtime::reduced" x))

(defn reduced?
  "Returns true if x is the result of a call to reduced."
  [x]
  (php/call_user_func "\\Cljp\\Runtime::isReduced" x))

(defn unreduced
  "If x is reduced?, returns the value wrapped by reduced, else returns x."
  [x]
  (if (reduced? x)
    (php/call_user_func "\\Cljp\\Runtime::unreduced" x)
    x))

(defn reduce
  ([f coll]
   (if (seq coll)
     (reduce f (first coll) (next coll))
     (f)))
  ([f val coll]
   (loop [val val, coll coll]
     (if (seq coll)
       (let [ret (f val (first coll))]
         (if (reduced? ret)
           (unreduced ret)
           (recur ret (next coll))))
       val))))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3 arguments.
   Returns the result of applying f to init, the first key and value,
   then applying f to that result and the 2nd key and val, etc."
  [f init coll]
  (if (nil? coll)
    init
    (loop [acc init, ks (keys coll)]
      (if (seq ks)
        (let [k (first ks)
              ret (f acc k (get coll k))]
          (if (reduced? ret)
            (unreduced ret)
            (recur ret (next ks))))
        acc))))

(defn last [coll]
  (loop [coll coll]
    (if (next coll)
      (recur (next coll))
      (first coll))))

(defn second [coll]
  (first (next coll)))

;; Higher-order functions
(defn map [f coll]
  (loop [result []
         coll coll]
    (if (seq coll)
      (recur (conj result (f (first coll))) (next coll))
      result)))

(defn filter [pred coll]
  (loop [result []
         coll coll]
    (if (seq coll)
      (let [x (first coll)]
        (recur (if (pred x) (conj result x) result) (next coll)))
      result)))

(defn remove [pred coll]
  (filter (fn [x] (not (pred x))) coll))

(defn mapcat
  "Returns the result of applying concat to the result of applying map."
  [f coll]
  (apply concat (map f coll)))

(defn map-indexed
  "Returns a sequence of f applied to index and item."
  [f coll]
  (loop [result []
         idx 0
         coll coll]
    (if (seq coll)
      (recur (conj result (f idx (first coll)))
             (inc idx)
             (next coll))
      result)))

(defn keep
  "Returns a sequence of non-nil results of f."
  [f coll]
  (loop [result []
         coll coll]
    (if (seq coll)
      (let [v (f (first coll))]
        (recur (if (nil? v) result (conj result v))
               (next coll)))
      result)))

(defn keep-indexed
  "Returns a sequence of non-nil results of (f index item)."
  [f coll]
  (loop [result []
         idx 0
         coll coll]
    (if (seq coll)
      (let [v (f idx (first coll))]
        (recur (if (nil? v) result (conj result v))
               (inc idx)
               (next coll)))
      result)))

(defn flatten
  "Takes any nested combination of sequential things and returns their contents as a single, flat sequence."
  [coll]
  (loop [result []
         coll coll]
    (if (seq coll)
      (let [x (first coll)]
        (if (or (php/is_array x) (coll? x))
          (recur (into result (flatten x)) (next coll))
          (recur (conj result x) (next coll))))
      result)))

(defn interleave
  "Returns a sequence of the first item in each coll, then the second, etc."
  [c1 c2]
  (loop [result []
         s1 c1
         s2 c2]
    (if (and (seq s1) (seq s2))
      (recur (conj (conj result (first s1)) (first s2))
             (next s1)
             (next s2))
      result)))

(defn interpose
  "Returns a sequence of the elements of coll separated by sep."
  [sep coll]
  (if (seq coll)
    (loop [result [(first coll)]
           coll (next coll)]
      (if (seq coll)
        (recur (conj (conj result sep) (first coll)) (next coll))
        result))
    []))

;; Equality - uses Clojure's equality checker via Runtime
(defn = [& args]
  (if (< (php/count args) 2)
    true
    (let [first-arg (php/array_shift args)]
      (loop [args args]
        (if (seq args)
          (if (php/call_user_func "\\Cljp\\Runtime::equals" first-arg (first args))
            (recur (next args))
            false)
          true)))))

(defn not= [& args]
  (not (apply = args)))

;; Arithmetic

(defn atom [x]
  (php/new "Clojure\\Lang\\Variable" nil x))

(defn deref [x]
  (.deref x))

(defn reset! [x newval]
  (.set x newval)
  newval)

(defn swap! [x f & args]
  (let [old (.deref x)
        new (apply f old args)]
    (.set x new)
    new))

(defn assoc [map key val & kvs]
  (let [m (php/call_user_func "\\Cljp\\Runtime::assoc" map key val)]
    (if kvs
      (apply assoc m kvs)
      m)))

(defn update [m k f & args]
  (assoc m k (apply f (get m k) args)))

(defn get
  ([m k]
   (php/call_user_func "\\Cljp\\Runtime::get" m k))
  ([m k not-found]
   (let [val (php/call_user_func "\\Cljp\\Runtime::get" m k)]
     (if (nil? val) not-found val))))

;; str must be defined before print/println since they use it
(defn str [& args]
  (php/implode "" args))

(defn print [& more]
  (php/print (apply str more)))

(defn println [& more]
  (php/print (apply str more))
  (php/print php/PHP_EOL))

;; EDN-style printer functions
(defn pr-str [& xs]
  "Returns a string containing the printed representation of xs (EDN format)"
  (php/implode " "
    (php/call_user_func "\\Cljp\\Runtime::toArray"
      (map (fn [x] (php/call_user_func "\\Cljp\\Runtime::prStr" x)) xs))))

(defn prn-str [& xs]
  "Same as pr-str but with a trailing newline"
  (str (apply pr-str xs) php/PHP_EOL))

(defn pr [& xs]
  "Prints the object(s) in a machine-readable form"
  (php/print (apply pr-str xs)))

(defn prn [& xs]
  "Same as pr followed by newline"
  (php/print (apply pr-str xs))
  (php/print php/PHP_EOL))

(defn print-str [& xs]
  "Returns a string of the values printed by print"
  (apply str xs))

;; Note: +, -, *, / are now infix operators (mapped directly in analyze.cljc)
;; Defining them here as variadic functions for compatibility
(defn + [& xs]
  (apply reduce (fn [a b] (php/+ a b)) 0 xs))

(defn - [x & xs]
  (if xs
    (apply reduce (fn [a b] (php/- a b)) x xs)
    (- 0 x)))

(defn * [& xs]
  (apply reduce (fn [a b] (php/* a b)) 1 xs))

(defn / [x & xs]
  (if xs
    (apply reduce (fn [a b] (php// a b)) x xs)
    (/ 1 x)))

;; Missing arithmetic functions
(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  [num div]
  (let [m (php/% num div)]
    (if (or (php/=== 0 m)
            (php/=== (php/> num 0) (php/> div 0)))
      m
      (php/+ m div))))

(defn rem
  "Remainder of dividing num by div."
  [num div]
  (php/% num div))

(defn quot
  "Quotient of dividing num by div."
  [num div]
  (php/intdiv num div))

(defn max
  "Returns the greatest of the nums."
  ([x] x)
  ([x y] (if (php/> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  "Returns the least of the nums."
  ([x] x)
  ([x y] (if (php/< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

(defn abs
  "Returns the absolute value of a."
  [a]
  (php/abs a))

(defn zero?
  "Returns true if num is zero."
  [x]
  (php/=== 0 x))

(defn pos?
  "Returns true if num is greater than zero."
  [x]
  (php/> x 0))

(defn neg?
  "Returns true if num is less than zero."
  [x]
  (php/< x 0))

(defn even?
  "Returns true if n is even."
  [n]
  (php/=== 0 (php/% n 2)))

(defn odd?
  "Returns true if n is odd."
  [n]
  (php/=== 1 (php/abs (php/% n 2))))

;; ============================================================
;; Stage 2: some->, doseq, type predicates, keys/vals
;; ============================================================

;; Null-safe threading macros
(defmacro some-> [expr & forms]
  (if (empty? forms)
    expr
    (let [g (gensym "some__")
          step (fn [f] (list 'if g (list '-> g f) nil))]
      (list 'let [g expr]
            (if (= 1 (count forms))
              (step (first forms))
              (list 'some-> (step (first forms)) (next forms)))))))

(defmacro some->> [expr & forms]
  (if (empty? forms)
    expr
    (let [g (gensym "some__")
          step (fn [f] (list 'if g (list '->> g f) nil))]
      (list 'let [g expr]
            (if (= 1 (count forms))
              (step (first forms))
              (list 'some->> (step (first forms)) (next forms)))))))

;; Loop macros
;; Note: doseq is defined earlier with :let/:when/:while support

(defmacro dotimes [bindings & body]
  (let [var (first bindings)
        n (second bindings)]
    (list 'loop [var 0]
          (list 'when (list '< var n)
                (cons 'do body)
                (list 'recur (list 'inc var))))))

;; Type predicates
(defn string? [x] (php/is_string x))
(defn int? [x] (php/is_int x))
(defn integer? [x] (php/is_int x))
(defn float? [x] (php/is_float x))
(defn number? [x] (php/is_numeric x))
(defn boolean? [x] (php/is_bool x))
(defn true? [x] (php/=== x true))
(defn false? [x] (php/=== x false))
(defn array? [x] (php/is_array x))
(defn fn? [x] (php/is_callable x))
(defn symbol? [x] (php/is_string x))  ; symbols are strings in PHP
(defn keyword? [x] (php/instanceof x "\\Clojure\\Lang\\Keyword"))
(defn vector? [x] (php/instanceof x "\\Clojure\\Lang\\Collections\\Vector\\PersistentVectorInterface"))
(defn map? [x] (php/instanceof x "\\Clojure\\Lang\\Collections\\Map\\PersistentMapInterface"))
(defn set? [x] (php/instanceof x "\\Clojure\\Lang\\Collections\\HashSet\\PersistentHashSetInterface"))
(defn list? [x] (php/instanceof x "\\Clojure\\Lang\\Collections\\LinkedList\\PersistentListInterface"))
(defn coll? [x] (or (vector? x) (map? x) (set? x) (list? x) (php/is_array x)))
(defn seq? [x] (or (vector? x) (list? x)))
(defn empty? [x] (php/=== 0 (count x)))
(defn some? [x] (not (nil? x)))

;; Map operations
(defn keys [m]
  (if (nil? m)
    nil
    (php/call_user_func "\\Cljp\\Runtime::keys" m)))

(defn vals [m]
  (if (nil? m)
    nil
    (php/call_user_func "\\Cljp\\Runtime::vals" m)))

(defn contains? [coll key]
  (if (nil? coll)
    false
    (php/call_user_func "\\Cljp\\Runtime::contains" coll key)))

;; Sequence functions
(defn take [n coll]
  (loop [n n, coll coll, result []]
    (if (and (> n 0) (seq coll))
      (let [x (first coll)]
        (recur (dec n) (next coll) (conj result x)))
      result)))

(defn drop [n coll]
  (loop [n n, coll coll]
    (if (and (> n 0) (seq coll))
      (recur (dec n) (next coll))
      coll)))

(defn take-while [pred coll]
  (loop [coll coll, result []]
    (if (seq coll)
      (let [x (first coll)]
        (if (pred x)
          (recur (next coll) (conj result x))
          result))
      result)))

(defn drop-while [pred coll]
  (loop [coll coll]
    (if (seq coll)
      (if (pred (first coll))
        (recur (next coll))
        coll)
      coll)))

(defn concat [& colls]
  (reduce (fn [acc c]
            (loop [acc acc, c c]
              (if (seq c)
                (recur (conj acc (first c)) (next c))
                acc)))
          []
          colls))

(defn reverse [coll]
  (let [arr (php/call_user_func "\\Cljp\\Runtime::toArray" coll)]
    (php/call_user_func "\\Cljp\\Runtime::vector" (php/array_reverse arr))))

(defn nth
  ([coll n]
   (nth coll n nil))
  ([coll n not-found]
   (if (vector? coll)
     (if (and (>= n 0) (< n (count coll)))
       (php/call_user_func "\\Cljp\\Runtime::get" coll n)
       not-found)
     (loop [coll coll, n n]
       (if (seq coll)
         (if (php/=== n 0)
           (first coll)
           (recur (next coll) (dec n)))
         not-found)))))

;; Range function - use 3-arg form, wrapper functions for convenience
(defn range3 [start end step]
  (loop [n start, result []]
    (if (< n end)
      (let [current n]
        (recur (+ n step) (conj result current)))
      result)))

(defn range [end-or-start & args]
  (if (nil? (first args))
    ;; Single arg: (range end)
    (range3 0 end-or-start 1)
    (let [end (first args)
          step (or (second args) 1)]
      ;; Two or three args: (range start end) or (range start end step)
      (range3 end-or-start end step))))

(defn repeat [n x]
  (loop [i n, result []]
    (if (> i 0)
      (recur (dec i) (conj result x))
      result)))

(defn into [to from]
  (reduce conj to from))

(defn zipmap [keys vals]
  (loop [m {}, ks keys, vs vals]
    (if (and (seq ks) (seq vs))
      (recur (assoc m (first ks) (first vs)) (next ks) (next vs))
      m)))

(defn partition
  ([n coll]
   (partition n n coll))
  ([n step coll]
   (loop [coll coll, result []]
     (let [chunk (take n coll)]
       (if (php/=== n (count chunk))
         (recur (drop step coll) (conj result chunk))
         result))))
  ([n step pad coll]
   (loop [coll coll, result []]
     (let [chunk (take n coll)]
       (if (php/=== n (count chunk))
         (recur (drop step coll) (conj result chunk))
         (if (seq chunk)
           (conj result (take n (concat chunk pad)))
           result))))))

(defn distinct [coll]
  (loop [coll coll, seen #{}, result []]
    (if (seq coll)
      (let [x (first coll)]
        (if (contains? seen x)
          (recur (next coll) seen result)
          (recur (next coll) (conj seen x) (conj result x))))
      result)))

(defn group-by [f coll]
  (reduce (fn [m x]
            (let [k (f x)]
              (update m k (fn [v] (conj (or v []) x)))))
          {}
          coll))

(defn frequencies [coll]
  (reduce (fn [m x]
            (update m x (fn [v] (+ (or v 0) 1))))
          {}
          coll))

(defn sort
  ([coll]
   (let [arr (php/call_user_func "\\Cljp\\Runtime::toArray" coll)]
     (php/sort arr)
     (php/call_user_func "\\Cljp\\Runtime::vector" arr)))
  ([comp coll]
   (let [arr (php/call_user_func "\\Cljp\\Runtime::toArray" coll)]
     (php/usort arr comp)
     (php/call_user_func "\\Cljp\\Runtime::vector" arr))))

(defn sort-by [keyfn coll]
  (let [arr (php/call_user_func "\\Cljp\\Runtime::toArray" coll)]
    (php/usort arr (fn [a b]
                     (let [ka (keyfn a), kb (keyfn b)]
                       (if (< ka kb) -1 (if (> ka kb) 1 0)))))
    (php/call_user_func "\\Cljp\\Runtime::vector" arr)))

;; More map operations
(defn merge [& maps]
  (reduce (fn [m1 m2]
            (if m2
              (reduce (fn [m kv]
                        (assoc m (first kv) (second kv)))
                      m1
                      (seq m2))
              m1))
          {}
          maps))

(defn get-in [m ks]
  (reduce get m ks))

(defn assoc-in [m ks v]
  (let [k (first ks)
        ks (next ks)]
    (if ks
      (assoc m k (assoc-in (get m k) ks v))
      (assoc m k v))))

(defn update-in [m ks f & args]
  (let [k (first ks)
        ks (next ks)]
    (if ks
      (assoc m k (apply update-in (get m k) ks f args))
      (assoc m k (apply f (get m k) args)))))

(defn dissoc [m & ks]
  (reduce (fn [m k]
            (php/call_user_func "\\Cljp\\Runtime::dissoc" m k))
          m
          ks))

(defn select-keys [m ks]
  (reduce (fn [result k]
            (if (contains? m k)
              (assoc result k (get m k))
              result))
          {}
          ks))

;; Predicates on sequences
(defn every? [pred coll]
  (loop [coll coll]
    (if (seq coll)
      (if (pred (first coll))
        (recur (next coll))
        false)
      true)))

(defn some [pred coll]
  (loop [coll coll]
    (if (seq coll)
      (let [result (pred (first coll))]
        (if result
          result
          (recur (next coll))))
      nil)))

(defn not-every? [pred coll]
  (not (every? pred coll)))

(defn not-any? [pred coll]
  (not (some pred coll)))

;; Higher-order function utilities
(defn constantly [x]
  (fn [& args] x))

(defn complement [f]
  (fn [& args] (not (apply f args))))

(defn juxt [& fs]
  (fn [& args]
    (map (fn [f] (apply f args)) fs)))

(defn fnil [f x]
  (fn [arg & args]
    (apply f (if (nil? arg) x arg) args)))

(defn memoize
  "Returns a memoized version of a referentially transparent function.
   The memoized version will cache the return value for each unique set of arguments."
  [f]
  (let [cache (atom {})]
    (fn [& args]
      (let [key (apply vector args)  ; Convert to vector for hashing
            cached (get (deref cache) key)]
        (if (some? cached)
          cached
          (let [result (apply f args)]
            (swap! cache assoc key result)
            result))))))

(defn compare
  "Comparator. Returns a negative number, zero, or positive number when x is
   logically 'less than', 'equal to', or 'greater than' y."
  [x y]
  (cond
    (php/< x y) -1
    (php/> x y) 1
    :else 0))

(defn identical?
  "Tests if 2 arguments are the same object (reference equality)."
  [x y]
  (php/=== x y))

(defn name
  "Returns the name String of a keyword or symbol."
  [x]
  (cond
    (keyword? x) (.getName x)
    (symbol? x) (php/strval x)
    (string? x) x
    :else (php/strval x)))

(defn namespace
  "Returns the namespace String of a keyword or symbol, or nil if none."
  [x]
  (if (keyword? x)
    (.getNamespace x)
    nil))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
   at end (defaults to length of string), exclusive."
  ([s start]
   (php/substr s start))
  ([s start end]
   (php/substr s start (php/- end start))))

;; More sequence functions
(defn interleave
  ([c1 c2]
   (loop [c1 c1, c2 c2, result []]
     (if (and (seq c1) (seq c2))
       (recur (next c1) (next c2)
              (conj (conj result (first c1)) (first c2)))
       result)))
  ([c1 c2 & colls]
   (loop [colls (cons c1 (cons c2 colls)), result []]
     (if (every? seq colls)
       (recur (map next colls) (into result (map first colls)))
       result))))

(defn interpose [sep coll]
  (loop [coll coll, result [], first? true]
    (if (seq coll)
      (if first?
        (recur (next coll) (conj result (first coll)) false)
        (recur (next coll) (conj (conj result sep) (first coll)) false))
      result)))

(defn flatten [coll]
  (loop [coll coll, result []]
    (if (seq coll)
      (let [x (first coll)]
        (if (coll? x)
          (recur (next coll) (into result (flatten x)))
          (recur (next coll) (conj result x))))
      result)))

(defn mapcat [f coll]
  (apply concat (map f coll)))

(defn keep [f coll]
  (loop [coll coll, result []]
    (if (seq coll)
      (let [v (f (first coll))]
        (recur (next coll) (if (nil? v) result (conj result v))))
      result)))

(defn keep-indexed [f coll]
  (loop [coll coll, idx 0, result []]
    (if (seq coll)
      (let [v (f idx (first coll))]
        (recur (next coll) (inc idx) (if (nil? v) result (conj result v))))
      result)))

(defn map-indexed [f coll]
  (loop [coll coll, idx 0, result []]
    (if (seq coll)
      (recur (next coll) (inc idx) (conj result (f idx (first coll))))
      result)))

(defn partition-by [f coll]
  (loop [coll coll, result [], current [], current-val nil]
    (if (seq coll)
      (let [x (first coll)
            v (f x)]
        (if (or (nil? current-val) (= v current-val))
          (recur (next coll) result (conj current x) v)
          (recur (next coll) (conj result current) [x] v)))
      (if (seq current)
        (conj result current)
        result))))

(defn split-at [n coll]
  [(take n coll) (drop n coll)])

(defn split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn butlast [coll]
  (loop [coll coll, result []]
    (if (next coll)
      (recur (next coll) (conj result (first coll)))
      result)))

(defn shuffle [coll]
  (let [arr (php/call_user_func "\\Cljp\\Runtime::toArray" coll)]
    (php/shuffle arr)
    (php/call_user_func "\\Cljp\\Runtime::vector" arr)))

;; Set operations
(defn union [s1 s2]
  (reduce conj s1 s2))

(defn intersection [s1 s2]
  (reduce (fn [result x]
            (if (contains? s2 x)
              (conj result x)
              result))
          #{}
          s1))

(defn difference [s1 s2]
  (reduce (fn [result x]
            (if (contains? s2 x)
              result
              (conj result x)))
          #{}
          s1))

(defn subset? [s1 s2]
  (every? (fn [x] (contains? s2 x)) s1))

(defn superset? [s1 s2]
  (subset? s2 s1))

;; Misc utilities
(defn max [& xs]
  (reduce (fn [a b] (if (> a b) a b)) (first xs) (next xs)))

(defn min [& xs]
  (reduce (fn [a b] (if (< a b) a b)) (first xs) (next xs)))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn mod [n d]
  (php/% n d))

(defn quot [n d]
  (php/intdiv n d))

(defn rem [n d]
  (php/% n d))

(defn even? [n]
  (php/=== 0 (mod n 2)))

(defn odd? [n]
  (not (even? n)))

(defn pos? [n]
  (> n 0))

(defn neg? [n]
  (< n 0))

(defn zero? [n]
  (php/=== n 0))

(defn rand []
  (php// (php/rand) (php/getrandmax)))

(defn rand-int [n]
  (php/rand 0 (dec n)))

(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

;; EDN reader placeholder - full implementation requires parser
(defn read-string [s]
  "Parse an EDN string. Currently uses PHP's json_decode for JSON-compatible values."
  (php/json_decode s true))

;; ============================================================
;; Exception Functions
;; ============================================================

;; Create an ExceptionInfo with a message and data map.
;; Optionally takes a cause (another Throwable).
(defn ex-info
  ([msg data]
   (php/call_user_func "\\Cljp\\Runtime::exInfo" msg data))
  ([msg data cause]
   (php/call_user_func "\\Cljp\\Runtime::exInfo" msg data cause)))

;; Returns the data map from an ExceptionInfo, or nil for other exceptions.
(defn ex-data [ex]
  (php/call_user_func "\\Cljp\\Runtime::exData" ex))

;; Returns the message from an exception.
(defn ex-message [ex]
  (php/call_user_func "\\Cljp\\Runtime::exMessage" ex))

;; Returns the cause (previous exception) from an exception.
(defn ex-cause [ex]
  (php/call_user_func "\\Cljp\\Runtime::exCause" ex))

;; Converts a Throwable to a map representation (Throwable->map equivalent)
(defn Throwable->map [ex]
  (if (nil? ex)
    nil
    (let [data (ex-data ex)
          msg (ex-message ex)
          cause (ex-cause ex)
          class-name (php/get_class ex)]
      {:type class-name
       :message msg
       :data data
       :cause (when cause (Throwable->map cause))})))

;; Returns an analysis of the phase, error, cause, and location of an error.
;; Based on Clojure's ex-triage but simplified for ClojurePHP.
(defn ex-triage [throwable-map]
  (let [{:keys [type message data cause]} throwable-map
        phase (or (get data :cljp.error/phase) :execution)
        err-type (get data :cljp.error/type)
        file (get data :cljp.error/file)
        line (get data :cljp.error/line)
        col (get data :cljp.error/column)
        sym (get data :cljp.error/symbol)
        hint (get data :cljp.error/hint)]
    {:cljp.error/phase phase
     :cljp.error/type err-type
     :cljp.error/class type
     :cljp.error/message message
     :cljp.error/file file
     :cljp.error/line line
     :cljp.error/column col
     :cljp.error/symbol sym
     :cljp.error/hint hint
     :cljp.error/cause (when cause (ex-message {:message (:message cause)}))}))

;; Returns a formatted error string from triage data.
;; Based on Clojure's ex-str but simplified for ClojurePHP.
(defn ex-str [triage-data]
  (let [phase (get triage-data :cljp.error/phase)
        err-type (get triage-data :cljp.error/type)
        class (get triage-data :cljp.error/class)
        message (get triage-data :cljp.error/message)
        file (get triage-data :cljp.error/file)
        line (get triage-data :cljp.error/line)
        column (get triage-data :cljp.error/column)
        symbol (get triage-data :cljp.error/symbol)
        hint (get triage-data :cljp.error/hint)
        loc (str (or file "REPL") ":" (or line 1) (if column (str ":" column) ""))
        class-str (if class (str class) "")
        class-simple (php/basename (php/str_replace "\\\\" "/" class-str))
        type-str (if err-type (php/ltrim (str err-type) ":") "")
        type-label (if err-type
                     (str (php/ucfirst type-str) "Error")
                     (if class class-simple "Error"))]
    (str type-label ": " message " at (" loc ")"
         (if hint (str "\nHint: " hint) ""))))

;; Helper to convert an exception to a formatted error message string.
(defn err->msg [ex]
  (-> ex Throwable->map ex-triage ex-str))

;; ============================================================
;; REPL State Variables
;; ============================================================

;; These are global mutable references used by the REPL to track
;; result history and the last exception. In PHP, these are stored
;; in $GLOBALS and accessed via the Runtime.

;; Note: In a full implementation, these would be dynamic vars with
;; proper binding semantics. For now, they're mutable atoms accessed
;; via PHP globals during REPL sessions.

(def ^{:doc "The last value returned by the REPL"} *1 nil)
(def ^{:doc "The second-to-last value returned by the REPL"} *2 nil)
(def ^{:doc "The third-to-last value returned by the REPL"} *3 nil)
(def ^{:doc "The last exception caught by the REPL"} *e nil)

;; Current namespace (for future namespace support)
(def ^{:doc "The current namespace"} *ns* "user")

;; Print settings
(def ^{:doc "Maximum items to print in a collection"} *print-length* nil)
(def ^{:doc "Maximum depth to print nested structures"} *print-level* nil)
(def ^{:doc "Whether to print metadata"} *print-meta* false)

;; REPL bindings update helper (called by REPL after each eval)
(defn -push-repl-result [value]
  "Internal: Update REPL result history."
  ;; In PHP, this updates the $GLOBALS vars
  ;; The actual implementation is in the REPL bootstrap PHP
  value)
