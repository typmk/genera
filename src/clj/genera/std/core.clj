;; 00-core.clj — Macros + stdlib
;;
;; Primitives, derived special forms, dissolved builtins.
;; Loaded first — everything else depends on these.

;; ================================================================
;; Primitives that must precede macros
;; ================================================================

(defn list [& args] args)

;; Forward declaration (no-op: genera uses global env)
(defmacro declare [& names] nil)

;; ================================================================
;; Derived special forms (were C, now macros)
;; ================================================================

;; when: (when test body...) → (if test (do body...))
(defmacro when [test & body]
  `(if ~test (do ~@body)))

;; cond: (cond t1 e1 t2 e2 ...) → nested if
(defmacro cond [& clauses]
  (if (nil? clauses) nil
    (if (nil? (rest clauses)) (first clauses)
      (list 'if (first clauses)
        (first (rest clauses))
        (cons 'cond (rest (rest clauses)))))))

;; and: short-circuit, returns last truthy or first falsy
(defmacro and [& args]
  (if (nil? args) true
    (if (nil? (rest args)) (first args)
      (let [g (gensym)]
        `(let [~g ~(first args)]
           (if ~g (and ~@(rest args)) ~g))))))

;; or: short-circuit, returns first truthy or last falsy
(defmacro or [& args]
  (if (nil? args) nil
    (if (nil? (rest args)) (first args)
      (let [g (gensym)]
        `(let [~g ~(first args)]
           (if ~g ~g (or ~@(rest args))))))))

;; Utility macros
(defmacro not= [a b] (list 'not (list '= a b)))
(defmacro bit-and-not [x y] (list 'bit-and x (list 'bit-not y)))

;; ================================================================
;; Dissolved builtins (were C, now Clojure)
;; ================================================================

;; Numeric convenience
(defn inc [x] (+ x 1))
(defn dec [x] (- x 1))
(defn zero? [x] (= x 0))
(defn pos? [x] (> x 0))
(defn neg? [x] (< x 0))

;; Sequence functions
(defn reverse [coll]
  (loop [c (seq coll) acc nil]
    (if (nil? c) acc
      (recur (rest c) (cons (first c) acc)))))

(defn map [f coll]
  (loop [c (seq coll) acc []]
    (if (nil? c) acc
      (recur (rest c) (conj acc (f (first c)))))))

(defn filter [f coll]
  (loop [c (seq coll) acc []]
    (if (nil? c) acc
      (if (f (first c))
        (recur (rest c) (conj acc (first c)))
        (recur (rest c) acc)))))

(defn reduce [f init coll]
  (loop [c (seq coll) acc init]
    (if (nil? c) acc
      (recur (rest c) (f acc (first c))))))

(defn range [& args]
  (let [start (if (nil? (rest args)) 0 (first args))
        end   (if (nil? (rest args)) (first args) (first (rest args)))]
    (loop [i start acc []]
      (if (< i end)
        (recur (+ i 1) (conj acc i))
        acc))))

(defn into [to from]
  (reduce conj to from))

;; Collection constructors
(defn vec [coll] (into [] coll))

(defn hash-map [& args]
  (loop [a (seq args) m {}]
    (if (nil? a) m
      (recur (rest (rest a)) (assoc m (first a) (first (rest a)))))))

;; Map queries (pmap seq returns [k v] entry vectors)
(defn keys [m] (map first (seq m)))
(defn vals [m] (map (fn [e] (get e 1)) (seq m)))

;; Logic
(defn not [x] (if x false true))

;; Type predicates (via type builtin)
(defn int? [x] (= (type x) T_INT))
(defn fn? [x] (= (type x) T_FN))
(defn symbol? [x] (= (type x) T_SYM))
(defn vector? [x] (= (type x) T_PVEC))
(defn map? [x] (= (type x) T_PMAP))
(defn keyword? [x] (= (type x) T_KW))
(defn string? [x] (= (type x) T_STR))

;; Collection queries
(defn empty? [x] (nil? (seq x)))

(defn nth [coll idx]
  (if (vector? coll)
    (get coll idx)
    (loop [c coll i 0]
      (if (= i idx) (first c)
        (recur (rest c) (+ i 1))))))

;; Self-hosted collections (HAMT, pvec, cons) moved to rt/coll.clj
;; Not loaded at boot — use C builtins (pmap, pvec, cons, get, assoc, etc.)

;; ================================================================
;; Portable builtins — dissolve more C into Clojure
;; ================================================================

;; contains? — pmap key existence check
(defn clj-contains? [m key]
  (if (map? m)
    (not (nil? (get m key)))
    false))

;; second — convenience
(defn second [coll] (first (rest coll)))

;; take / drop
(defn take [n coll]
  (loop [i 0 c (seq coll) acc []]
    (if (and (< i n) (not (nil? c)))
      (recur (+ i 1) (rest c) (conj acc (first c)))
      acc)))

(defn drop [n coll]
  (loop [i 0 c (seq coll)]
    (if (and (< i n) (not (nil? c)))
      (recur (+ i 1) (rest c))
      c)))

;; some / every?
(defn some [pred coll]
  (loop [c (seq coll)]
    (if (nil? c) nil
      (let [r (pred (first c))]
        (if r r (recur (rest c)))))))

(defn every? [pred coll]
  (loop [c (seq coll)]
    (if (nil? c) true
      (if (pred (first c))
        (recur (rest c))
        false))))

;; complement
(defn complement [f]
  (fn [& args] (not (apply f args))))

;; identity / constantly
(defn identity [x] x)
(defn constantly [x] (fn [& args] x))

;; partition
(defn partition [n coll]
  (loop [c (seq coll) acc []]
    (if (nil? c) acc
      (let [chunk (take n c)
            remaining (drop n c)]
        (if (< (count chunk) n) acc
          (recur remaining (conj acc chunk)))))))

;; interleave
(defn interleave [c1 c2]
  (loop [a (seq c1) b (seq c2) acc []]
    (if (or (nil? a) (nil? b)) acc
      (recur (rest a) (rest b)
             (conj (conj acc (first a)) (first b))))))

;; zipmap
(defn zipmap [keys vals]
  (loop [ks (seq keys) vs (seq vals) m {}]
    (if (or (nil? ks) (nil? vs)) m
      (recur (rest ks) (rest vs) (assoc m (first ks) (first vs))))))

;; frequencies
(defn frequencies [coll]
  (reduce (fn [m x]
            (let [c (get m x)]
              (assoc m x (if (nil? c) 1 (+ c 1)))))
          {} coll))

;; group-by
(defn group-by [f coll]
  (reduce (fn [m x]
            (let [k (f x)
                  existing (get m k)]
              (assoc m k (conj (if (nil? existing) [] existing) x))))
          {} coll))

;; mapcat
(defn mapcat [f coll]
  (reduce (fn [acc x] (into acc (f x))) [] coll))

;; distinct
(defn distinct [coll]
  (loop [c (seq coll) seen {} acc []]
    (if (nil? c) acc
      (let [x (first c)]
        (if (contains? seen x)
          (recur (rest c) seen acc)
          (recur (rest c) (assoc seen x true) (conj acc x)))))))