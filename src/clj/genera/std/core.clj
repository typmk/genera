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

;; ================================================================
;; Self-hosted collections — HAMT + pvec built from Mem primitives
;;
;; This is the foundation: data structures ARE the language.
;; Everything above cons is expressed here using only:
;;   - Mem: bump, restore, u32@/!, val@/!, copy, fill
;;   - Bit ops: bit-and, bit-or, bit-shift-left/right, popcount, hash32
;;   - Cons: cons, first, rest (irreducible — the bootstrap seed)
;; ================================================================

;; --- Memory pools ---
;; Each pool is a Mem region. Offsets are u32 byte positions.

(def hamt-mem (mem-new! 67108864))   ;; 64MB node pool
(def leaf-mem (mem-new! 16777216))   ;; 16MB leaf pool
(def small-mem (mem-new! 8388608))   ;; 8MB small map pool

;; --- Constants ---
(def HAMT_BITS 5)
(def HAMT_WIDTH 32)
(def HAMT_MASK 31)
(def SMALL_MAX 16)
(def COLL_TAG_NODE 2147483648)   ;; (bit-shift-left 1 31)
(def COLL_NIL 4294967295)        ;; UINT32_MAX

;; --- Leaf: key(u32) + pad(u32) + value(Val=8b) = 16 bytes ---
;; Layout: [key:u32 @+0] [pad:u32 @+4] [value:Val @+8]

(defn leaf-alloc [key value]
  (let [off (mem-bump! leaf-mem 16)]
    (mem-u32! leaf-mem off key)
    (mem-val! leaf-mem (+ off 8) value)
    off))

(defn leaf-key [off] (mem-u32 leaf-mem off))
(defn leaf-val [off] (mem-val leaf-mem (+ off 8)))

;; --- Small map: count(u32) + keys[16](u32*16) + pad(u32*15 for align) + vals[16](Val*16) ---
;; Layout: [count:u32 @+0] [keys:u32*16 @+4] [pad @+68] [vals:Val*16 @+72]
;; Total: 72 + 128 = 200 bytes, round to 200

(def SMALL_KEYS_OFF 4)          ;; keys start at byte 4
(def SMALL_VALS_OFF 72)         ;; vals start at byte 72 (4 + 16*4 + 4 padding)
(def SMALL_SIZE 200)            ;; total bytes per small map

(defn small-alloc []
  (let [off (mem-bump! small-mem SMALL_SIZE)]
    (mem-u32! small-mem off 0)  ;; count = 0
    off))

(defn small-count [off] (mem-u32 small-mem off))

(defn small-key [off i]
  (mem-u32 small-mem (+ off SMALL_KEYS_OFF (+ (* i 4) 0))))

(defn small-val [off i]
  (mem-val small-mem (+ off SMALL_VALS_OFF (* i 8))))

(defn small-set-key [off i k]
  (mem-u32! small-mem (+ off SMALL_KEYS_OFF (* i 4)) k))

(defn small-set-val [off i v]
  (mem-val! small-mem (+ off SMALL_VALS_OFF (* i 8)) v))

(defn small-set-count [off n]
  (mem-u32! small-mem off n))

(defn small-get [off key]
  (loop [i 0]
    (if (< i (small-count off))
      (if (= (small-key off i) key)
        (small-val off i)
        (recur (+ i 1)))
      nil)))

(defn small-copy [src-off]
  (let [dst (mem-bump! small-mem SMALL_SIZE)]
    (mem-copy! small-mem dst src-off SMALL_SIZE)
    dst))

(defn small-put [src-off key val]
  (let [dst (small-copy src-off)
        cnt (small-count dst)]
    (loop [i 0]
      (if (< i cnt)
        (if (= (small-key dst i) key)
          (do (small-set-val dst i val) dst)
          (recur (+ i 1)))
        (do
          (small-set-key dst cnt key)
          (small-set-val dst cnt val)
          (small-set-count dst (+ cnt 1))
          dst)))))

;; --- HAMT Node: bitmap(u32) + owner(u32) + children(u32 * n) ---
;; Layout: [bitmap:u32 @+0] [owner:u32 @+4] [children:u32* @+8]

(defn node-alloc [n-children]
  (let [size (+ 8 (* n-children 4))
        off  (mem-bump! hamt-mem size)]
    (mem-u32! hamt-mem off 0)       ;; bitmap = 0
    (mem-u32! hamt-mem (+ off 4) 0) ;; owner = 0
    off))

(defn node-bitmap [off] (mem-u32 hamt-mem off))
(defn node-owner [off] (mem-u32 hamt-mem (+ off 4)))
(defn node-child [off i] (mem-u32 hamt-mem (+ off 8 (* i 4))))
(defn node-set-bitmap [off bm] (mem-u32! hamt-mem off bm))
(defn node-set-owner [off o] (mem-u32! hamt-mem (+ off 4) o))
(defn node-set-child [off i c] (mem-u32! hamt-mem (+ off 8 (* i 4)) c))

(defn coll-is-node [x] (pos? (bit-and x COLL_TAG_NODE)))
(defn coll-idx [x] (bit-and x (bit-not COLL_TAG_NODE)))

;; --- HAMT core ---

(defn hamt-leaf-node [hash shift leaf-idx]
  (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))
        off (node-alloc 1)]
    (node-set-bitmap off bit)
    (node-set-child off 0 leaf-idx)
    off))

(defn hamt-merge-leaves [shift h1 l1 h2 l2]
  (if (>= shift 30)
    ;; Collision node: bitmap=0, 2 children
    (let [off (node-alloc 2)]
      (node-set-child off 0 l1)
      (node-set-child off 1 l2)
      off)
    (let [b1 (bit-and (bit-shift-right h1 shift) HAMT_MASK)
          b2 (bit-and (bit-shift-right h2 shift) HAMT_MASK)]
      (if (= b1 b2)
        ;; Same bucket — recurse deeper
        (let [child (hamt-merge-leaves (+ shift HAMT_BITS) h1 l1 h2 l2)
              off (node-alloc 1)]
          (node-set-bitmap off (bit-shift-left 1 b1))
          (node-set-child off 0 (bit-or child COLL_TAG_NODE))
          off)
        ;; Different buckets — two children
        (let [off (node-alloc 2)]
          (node-set-bitmap off (bit-or (bit-shift-left 1 b1) (bit-shift-left 1 b2)))
          (if (< b1 b2)
            (do (node-set-child off 0 l1) (node-set-child off 1 l2))
            (do (node-set-child off 0 l2) (node-set-child off 1 l1)))
          off)))))

(defn hamt-put [node-off shift hash key val]
  (let [bm (node-bitmap node-off)]
    (if (= bm 0)
      ;; Collision node
      (let [c0 (node-child node-off 0)
            c1 (node-child node-off 1)]
        (if (= (leaf-key c0) key)
          (let [new-leaf (leaf-alloc key val)
                off (node-alloc 2)]
            (node-set-child off 0 new-leaf)
            (node-set-child off 1 c1)
            (cons off false))
          (if (= (leaf-key c1) key)
            (let [new-leaf (leaf-alloc key val)
                  off (node-alloc 2)]
              (node-set-child off 0 c0)
              (node-set-child off 1 new-leaf)
              (cons off false))
            ;; New collision entry
            (let [new-leaf (leaf-alloc key val)
                  off (node-alloc 3)]
              (node-set-child off 0 c0)
              (node-set-child off 1 c1)
              (node-set-child off 2 new-leaf)
              (cons off true)))))
      ;; Normal HAMT node
      (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))
            idx (popcount (bit-and bm (- bit 1)))
            cnt (popcount bm)]
        (if (= (bit-and bm bit) 0)
          ;; Empty slot — insert new leaf
          (let [leaf (leaf-alloc key val)
                off (node-alloc (+ cnt 1))]
            (node-set-bitmap off (bit-or bm bit))
            ;; Copy children before idx
            (loop [i 0]
              (when (< i idx)
                (node-set-child off i (node-child node-off i))
                (recur (+ i 1))))
            (node-set-child off idx leaf)
            ;; Copy children after idx
            (loop [i idx]
              (when (< i cnt)
                (node-set-child off (+ i 1) (node-child node-off i))
                (recur (+ i 1))))
            (cons off true))
          ;; Slot occupied
          (let [child (node-child node-off idx)]
            (if (coll-is-node child)
              ;; Sub-node — recurse
              (let [result (hamt-put (coll-idx child) (+ shift HAMT_BITS) hash key val)
                    new-child (first result)
                    added (rest result)
                    off (node-alloc cnt)]
                (node-set-bitmap off bm)
                (loop [i 0]
                  (when (< i cnt)
                    (node-set-child off i (node-child node-off i))
                    (recur (+ i 1))))
                (node-set-child off idx (bit-or new-child COLL_TAG_NODE))
                (cons off added))
              ;; Leaf
              (if (= (leaf-key child) key)
                ;; Same key — replace
                (if (= (leaf-val child) val)
                  (cons node-off false)  ;; identical value — no change
                  (let [new-leaf (leaf-alloc key val)
                        off (node-alloc cnt)]
                    (node-set-bitmap off bm)
                    (loop [i 0]
                      (when (< i cnt)
                        (node-set-child off i (node-child node-off i))
                        (recur (+ i 1))))
                    (node-set-child off idx new-leaf)
                    (cons off false)))
                ;; Different key — split
                (let [h2 (hash32 (leaf-key child))
                      new-leaf (leaf-alloc key val)
                      sub (hamt-merge-leaves (+ shift HAMT_BITS) h2 child hash new-leaf)
                      off (node-alloc cnt)]
                  (node-set-bitmap off bm)
                  (loop [i 0]
                    (when (< i cnt)
                      (node-set-child off i (node-child node-off i))
                      (recur (+ i 1))))
                  (node-set-child off idx (bit-or sub COLL_TAG_NODE))
                  (cons off true))))))))))

(defn hamt-get [node-off shift hash key]
  (let [bm (node-bitmap node-off)]
    (if (= bm 0)
      ;; Collision node — linear scan
      (let [c0 (node-child node-off 0)]
        (if (= (leaf-key c0) key)
          (leaf-val c0)
          (let [c1 (node-child node-off 1)]
            (if (= (leaf-key c1) key)
              (leaf-val c1)
              nil))))
      ;; Normal HAMT node
      (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))]
        (if (= (bit-and bm bit) 0)
          nil  ;; not found
          (let [idx (popcount (bit-and bm (- bit 1)))
                child (node-child node-off idx)]
            (if (coll-is-node child)
              (hamt-get (coll-idx child) (+ shift HAMT_BITS) hash key)
              (if (= (leaf-key child) key)
                (leaf-val child)
                nil))))))))

;; --- Public API: clj-pmap ---
;; A pmap handle is a cons list: (count root is-small)
;; where root is an offset into the appropriate Mem

(defn clj-pmap-empty [] (cons 0 (cons COLL_NIL (cons true nil))))

(defn clj-pmap-get [m key]
  (let [cnt    (first m)
        root   (first (rest m))
        small? (first (rest (rest m)))]
    (if (= cnt 0) nil
      (if small?
        (small-get root key)
        (hamt-get root 0 (hash32 key) key)))))

(defn clj-pmap-put [m key val]
  (let [cnt    (first m)
        root   (first (rest m))
        small? (first (rest (rest m)))]
    (if (= cnt 0)
      ;; First entry
      (let [sm (small-alloc)]
        (small-set-key sm 0 key)
        (small-set-val sm 0 val)
        (small-set-count sm 1)
        (cons 1 (cons sm (cons true nil))))
      (if small?
        ;; Small map path
        (let [old-cnt (small-count root)]
          ;; Check for existing key
          (loop [i 0]
            (if (< i old-cnt)
              (if (= (small-key root i) key)
                ;; Update existing
                (let [new-sm (small-copy root)]
                  (small-set-val new-sm i val)
                  (cons cnt (cons new-sm (cons true nil))))
                (recur (+ i 1)))
              ;; Key not found
              (if (< old-cnt SMALL_MAX)
                ;; Still fits in small map
                (let [new-sm (small-put root key val)]
                  (cons (+ cnt 1) (cons new-sm (cons true nil))))
                ;; Promote to HAMT
                (let [first-leaf (leaf-alloc (small-key root 0) (small-val root 0))
                      hamt-root  (hamt-leaf-node (hash32 (small-key root 0)) 0 first-leaf)
                      promoted   (loop [i 1 r hamt-root]
                                   (if (< i old-cnt)
                                     (let [k (small-key root i)
                                           v (small-val root i)
                                           lf (leaf-alloc k v)
                                           result (hamt-put r 0 (hash32 k) k v)]
                                       (recur (+ i 1) (first result)))
                                     r))
                      ;; Insert new key
                      result (hamt-put promoted 0 (hash32 key) key val)
                      new-root (first result)
                      added (rest result)]
                  (cons (+ cnt (if added 1 0)) (cons new-root (cons false nil))))))))
        ;; HAMT path
        (let [result (hamt-put root 0 (hash32 key) key val)
              new-root (first result)
              added (rest result)]
          (cons (+ cnt (if added 1 0)) (cons new-root (cons false nil))))))))

(defn clj-pmap-count [m] (first m))

;; ================================================================
;; Self-hosted PVEC — 32-way trie with tail, via Mem primitives
;; ================================================================

(def vec-mem (mem-new! 33554432))    ;; 32MB for vnodes + vleaves

;; VNode: 32 children (u32 indices), 128 bytes
;; VLeaf: 32 items (Val, 8 bytes each), 256 bytes
(def VNODE_SIZE 128)
(def VLEAF_SIZE 256)
(def VEC_WIDTH 32)
(def VEC_BITS 5)
(def VEC_MASK 31)

(defn vnode-alloc []
  (let [off (mem-bump! vec-mem VNODE_SIZE)]
    ;; Fill with COLL_NIL (0xFFFFFFFF)
    (mem-fill! vec-mem off 255 VNODE_SIZE)
    off))

(defn vleaf-alloc []
  (let [off (mem-bump! vec-mem VLEAF_SIZE)]
    (mem-fill! vec-mem off 0 VLEAF_SIZE)
    off))

(defn vnode-child [off i] (mem-u32 vec-mem (+ off (* i 4))))
(defn vnode-set-child [off i c] (mem-u32! vec-mem (+ off (* i 4)) c))

(defn vleaf-item [off i] (mem-val vec-mem (+ off (* i 8))))
(defn vleaf-set-item [off i v] (mem-val! vec-mem (+ off (* i 8)) v))

;; Copy a vnode
(defn vnode-copy [src]
  (let [dst (mem-bump! vec-mem VNODE_SIZE)]
    (mem-copy! vec-mem dst src VNODE_SIZE)
    dst))

;; Copy a vleaf
(defn vleaf-copy [src]
  (let [dst (mem-bump! vec-mem VLEAF_SIZE)]
    (mem-copy! vec-mem dst src VLEAF_SIZE)
    dst))

;; pvec handle: (count shift root tail tail-len)
(defn clj-pvec-empty []
  (cons 0 (cons VEC_BITS (cons COLL_NIL (cons COLL_NIL (cons 0 nil))))))

;; Accessors
(defn pv-count [v] (first v))
(defn pv-shift [v] (first (rest v)))
(defn pv-root [v] (first (rest (rest v))))
(defn pv-tail [v] (first (rest (rest (rest v)))))
(defn pv-tail-len [v] (first (rest (rest (rest (rest v))))))

;; Make pvec handle
(defn pv-make [cnt sh root tail tl]
  (cons cnt (cons sh (cons root (cons tail (cons tl nil))))))

;; Get
(defn clj-pvec-get [v i]
  (let [cnt (pv-count v)]
    (if (>= i cnt) nil
      (let [tail-off (- cnt (pv-tail-len v))]
        (if (>= i tail-off)
          ;; In tail
          (vleaf-item (pv-tail v) (- i tail-off))
          ;; Walk interior nodes
          (loop [node (pv-root v) level (pv-shift v)]
            (if (> level VEC_BITS)
              (recur (vnode-child node (bit-and (bit-shift-right i level) VEC_MASK))
                     (- level VEC_BITS))
              ;; Bottom level: children are vleaf indices
              (vleaf-item (vnode-child node (bit-and (bit-shift-right i VEC_BITS) VEC_MASK))
                          (bit-and i VEC_MASK)))))))))

;; new-path: create chain of vnodes down to a leaf
(defn pvec-new-path [shift tail]
  (if (= shift 0) tail
    (let [nid (vnode-alloc)]
      (vnode-set-child nid 0 (pvec-new-path (- shift VEC_BITS) tail))
      nid)))

;; push-tail: push old tail into the trie
(defn pvec-push-tail [count shift parent tail]
  (let [sub-idx (bit-and (bit-shift-right (- count 1) shift) VEC_MASK)
        nid (vnode-copy parent)]
    (if (= shift VEC_BITS)
      (do (vnode-set-child nid sub-idx tail) nid)
      (let [existing (vnode-child parent sub-idx)]
        (if (= existing COLL_NIL)
          (do (vnode-set-child nid sub-idx (pvec-new-path (- shift VEC_BITS) tail)) nid)
          (do (vnode-set-child nid sub-idx (pvec-push-tail count (- shift VEC_BITS) existing tail)) nid))))))

;; Append
(defn clj-pvec-append [v val]
  (let [n     (pv-count v)
        tl    (pv-tail-len v)
        shift (pv-shift v)
        root  (pv-root v)
        tail  (pv-tail v)]
    (if (< tl VEC_WIDTH)
      ;; Room in tail
      (let [new-tail (if (= tail COLL_NIL) (vleaf-alloc) (vleaf-copy tail))]
        (vleaf-set-item new-tail tl val)
        (pv-make (+ n 1) shift root new-tail (+ tl 1)))
      ;; Tail full — push into tree, start new tail
      (let [new-tail (vleaf-alloc)]
        (vleaf-set-item new-tail 0 val)
        (if (= root COLL_NIL)
          ;; First overflow: create root
          (let [new-root (vnode-alloc)]
            (vnode-set-child new-root 0 tail)
            (pv-make (+ n 1) VEC_BITS new-root new-tail 1))
          (if (> (bit-shift-right n VEC_BITS) (bit-shift-left 1 shift))
            ;; Tree full — grow height
            (let [new-root (vnode-alloc)]
              (vnode-set-child new-root 0 root)
              (vnode-set-child new-root 1 (pvec-new-path shift tail))
              (pv-make (+ n 1) (+ shift VEC_BITS) new-root new-tail 1))
            ;; Push tail into existing tree
            (let [new-root (pvec-push-tail n shift root tail)]
              (pv-make (+ n 1) shift new-root new-tail 1))))))))

(defn clj-pvec-count [v] (pv-count v))

;; ================================================================
;; Self-hosted CONS — the bootstrap seed, expressed in the language
;;
;; Layout: [car:Val @+0] [cdr:Val @+8] = 16 bytes
;; This proves cons is NOT irreducible — it's bump + 2 stores.
;; The JIT compiles this to 3-4 x86 instructions (inline).
;; ================================================================

(def cons-mem (mem-new! 33554432))   ;; 32MB cons pool

(defn clj-cons [a b]
  (let [off (mem-bump! cons-mem 16)]
    (mem-val! cons-mem off a)
    (mem-val! cons-mem (+ off 8) b)
    off))

(defn clj-car [c] (mem-val cons-mem c))
(defn clj-cdr [c] (mem-val cons-mem (+ c 8)))

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