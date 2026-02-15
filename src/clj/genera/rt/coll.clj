;; coll.clj — Persistent Data Structures
;;
;; Pure Clojure. JIT-compiled to native x86.
;; Uses mem.clj's emit-load-at/emit-store-at for node field access.
;; Uses mem.clj's bump for allocation from g_coll.
;;
;; Same shape as everything else:
;;   Write the algorithm in Clojure. The JIT compiles it.
;;   C eval interprets it during bootstrap. Then it compiles itself.
;;
;; Structural change from coll.c:
;;   C:  5 separate typed pools (node_mem, leaves, smalls, vnodes, vleaves)
;;       each with own counter, cap, base pointer
;;   Clj: ONE g_coll Mem instance, bump allocates everything inline
;;        No separate pools. Just bytes in an arena.
;;
;; The algorithms are identical. Only the allocation strategy changes.

;; ================================================================
;; Hash function: StrId → u32 (bit mixing)
;; ================================================================
;;
;; Keys are StrId (interned u32). We need a hash to distribute
;; them across HAMT levels. Standard Murmur3 finalizer.

(defn hash32 [x]
  (let [x (bit-xor x (bit-shift-right x 16))
        x (bit-and (* x 0x45d9f3b) 0xFFFFFFFF)
        x (bit-xor x (bit-shift-right x 16))]
    (bit-and x 0xFFFFFFFF)))

;; ================================================================
;; Constants
;; ================================================================

(def HAMT_BITS  5)
(def HAMT_WIDTH 32)
(def HAMT_MASK  31)
(def VEC_WIDTH  32)
(def VEC_BITS   5)
(def VEC_MASK   31)
(def SMALL_MAX  16)

;; ================================================================
;; Node layouts (bump-allocated from g_coll)
;; ================================================================
;;
;; CNode: { u32 bitmap, u32 owner } + u32[n_children] inline
;; Total: 8 + 4*n bytes, 4-byte aligned
;;
;; CLeaf: { u32 key, u32 _pad, Val value }
;; Total: 16 bytes (Val is u64, needs 8-byte alignment)
;;
;; CVNode: u32[32] = 128 bytes
;; CVLeaf: Val[32] = 256 bytes (u64 × 32)
;;
;; CPMap handle: { u32 count, u32 root, u32 txn, u32 is_small }
;; Total: 16 bytes
;;
;; CPVec handle: { u32 count, u32 shift, u32 root, u32 tail, u32 tail_len }
;; Total: 20 bytes

;; CNode offsets
(def CN_BITMAP   0)   ;; u32
(def CN_OWNER    4)   ;; u32
(def CN_CHILDREN 8)   ;; u32[] starts here
(def CN_CHILD_SZ 4)   ;; each child is u32

;; CLeaf offsets
(def CL_KEY   0)      ;; u32
(def CL_PAD   4)      ;; u32 padding
(def CL_VALUE 8)      ;; u64 (Val)
(def CL_SIZE  16)

;; CVNode: 32 × u32 = 128 bytes
(def VN_CHILD_SZ 4)
(def VN_SIZE 128)

;; CVLeaf: 32 × u64 = 256 bytes
(def VL_ITEM_SZ 8)
(def VL_SIZE 256)

;; CPMap handle offsets
(def PM_COUNT    0)    ;; u32
(def PM_ROOT     4)    ;; u32
(def PM_TXN      8)    ;; u32
(def PM_IS_SMALL 12)   ;; u32 (bool as u32)
(def PM_SIZE     16)

;; CPVec handle offsets
(def PV_COUNT    0)    ;; u32
(def PV_SHIFT    4)    ;; u32
(def PV_ROOT     8)    ;; u32
(def PV_TAIL     12)   ;; u32
(def PV_TAIL_LEN 16)   ;; u32
(def PV_SIZE     20)

;; Sentinel
(def NIL_IDX 0xFFFFFFFF)

;; High bit tags node vs leaf in child array
(def TAG_NODE 0x80000000)

;; ================================================================
;; Helpers
;; ================================================================

(defn node? [child-idx]
  (not (zero? (bit-and child-idx TAG_NODE))))

(defn node-idx [child-idx]
  (bit-and-not child-idx TAG_NODE))

;; popcount32 from epoch.clj (repeated here for standalone use)
(defn popcount32 [x]
  (loop [n x c 0]
    (if (zero? n) c
      (recur (bit-and n (dec n)) (inc c)))))

;; Node size in bytes for n children
(defn node-size [n-children]
  (let [sz (+ CN_CHILDREN (* n-children CN_CHILD_SZ))]
    (bit-and-not (+ sz 3) 3)))  ;; 4-byte align

;; ================================================================
;; Allocation from g_coll Mem
;; ================================================================
;;
;; All coll nodes live in one Mem arena. bump returns a byte offset.
;; We store/load fields relative to (g_coll_base + offset).
;;
;; In the eval path: these use load32/store32! builtins.
;; In the JIT path: the JIT compiles them to MOV instructions.
;;
;; g_coll_base and g_coll_ptr are set by the runtime at init:
;;   g_coll_base = load64(g_coll, MEM_BASE)
;;   g_coll_ptr  = address of the Mem struct itself

(defn coll-alloc-node [n-children coll-ptr]
  (let [sz (node-size n-children)
        off (bump coll-ptr sz)
        base (load64 coll-ptr MEM_BASE)
        ptr (+ base off)]
    ;; Zero owner (persistent)
    (store32! ptr CN_OWNER 0)
    off))

(defn coll-alloc-leaf [key value coll-ptr]
  (let [off (bump coll-ptr CL_SIZE)
        base (load64 coll-ptr MEM_BASE)
        ptr (+ base off)]
    (store32! ptr CL_KEY key)
    (store64! ptr CL_VALUE value)
    off))

(defn coll-alloc-vnode [coll-ptr]
  (let [off (bump coll-ptr VN_SIZE)
        base (load64 coll-ptr MEM_BASE)
        ptr (+ base off)]
    ;; Fill all children with NIL_IDX
    (loop [i 0]
      (when (< i VEC_WIDTH)
        (store32! ptr (* i VN_CHILD_SZ) NIL_IDX)
        (recur (inc i))))
    off))

(defn coll-alloc-vleaf [coll-ptr]
  (let [off (bump coll-ptr VL_SIZE)
        base (load64 coll-ptr MEM_BASE)
        ptr (+ base off)]
    ;; Zero all items
    (loop [i 0]
      (when (< i VEC_WIDTH)
        (store64! ptr (* i VL_ITEM_SZ) 0)
        (recur (inc i))))
    off))

;; Pointer to node/leaf at offset within g_coll
(defn coll-ptr-at [coll-ptr off]
  (+ (load64 coll-ptr MEM_BASE) off))

;; ================================================================
;; HAMT — Persistent Hash-Array Mapped Trie
;; ================================================================

;; --- Lookup ---

(defn chamt-get [coll-ptr node-off shift hash key]
  (let [ptr (coll-ptr-at coll-ptr node-off)
        bitmap (load32 ptr CN_BITMAP)]
    (if (zero? bitmap)
      ;; Collision node: linear scan children (2-3 leaves)
      (loop [i 0]
        (if (>= i 3) nil
          (let [child-off (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ)))]
            (if (zero? child-off) nil
              (let [leaf-ptr (coll-ptr-at coll-ptr child-off)
                    lk (load32 leaf-ptr CL_KEY)]
                (if (= lk key)
                  (load64 leaf-ptr CL_VALUE)
                  (recur (inc i))))))))
      ;; Normal node
      (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))]
        (if (zero? (bit-and bitmap bit))
          nil  ;; not present
          (let [idx (popcount32 (bit-and bitmap (dec bit)))
                child-val (load32 ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ)))]
            (if (node? child-val)
              ;; Recurse into sub-node
              (chamt-get coll-ptr (node-idx child-val) (+ shift HAMT_BITS) hash key)
              ;; Leaf
              (let [leaf-ptr (coll-ptr-at coll-ptr child-val)
                    lk (load32 leaf-ptr CL_KEY)]
                (if (= lk key)
                  (load64 leaf-ptr CL_VALUE)
                  nil)))))))))

;; --- Insert ---

;; Create single-leaf node at given hash level
(defn chamt-leaf-node [coll-ptr hash shift leaf-off]
  (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))
        off (coll-alloc-node 1 coll-ptr)
        ptr (coll-ptr-at coll-ptr off)]
    (store32! ptr CN_BITMAP bit)
    (store32! ptr (+ CN_CHILDREN 0) leaf-off)
    off))

;; Merge two leaves that collide at the current level
(defn chamt-merge-leaves [coll-ptr shift h1 l1 h2 l2]
  (if (>= shift 30)
    ;; Max depth: collision node (bitmap=0, two children)
    (let [off (coll-alloc-node 2 coll-ptr)
          ptr (coll-ptr-at coll-ptr off)]
      (store32! ptr CN_BITMAP 0)
      (store32! ptr (+ CN_CHILDREN 0) l1)
      (store32! ptr (+ CN_CHILDREN CN_CHILD_SZ) l2)
      off)
    ;; Distribute by hash bits at this level
    (let [b1 (bit-and (bit-shift-right h1 shift) HAMT_MASK)
          b2 (bit-and (bit-shift-right h2 shift) HAMT_MASK)]
      (if (= b1 b2)
        ;; Same slot: recurse deeper
        (let [child (chamt-merge-leaves coll-ptr (+ shift HAMT_BITS) h1 l1 h2 l2)
              off (coll-alloc-node 1 coll-ptr)
              ptr (coll-ptr-at coll-ptr off)]
          (store32! ptr CN_BITMAP (bit-shift-left 1 b1))
          (store32! ptr (+ CN_CHILDREN 0) (bit-or child TAG_NODE))
          off)
        ;; Different slots: two-child node
        (let [off (coll-alloc-node 2 coll-ptr)
              ptr (coll-ptr-at coll-ptr off)
              bitmap (bit-or (bit-shift-left 1 b1) (bit-shift-left 1 b2))]
          (store32! ptr CN_BITMAP bitmap)
          ;; Children ordered by bit position
          (if (< b1 b2)
            (do (store32! ptr (+ CN_CHILDREN 0) l1)
                (store32! ptr (+ CN_CHILDREN CN_CHILD_SZ) l2))
            (do (store32! ptr (+ CN_CHILDREN 0) l2)
                (store32! ptr (+ CN_CHILDREN CN_CHILD_SZ) l1)))
          off)))))

;; Persistent HAMT put. Returns (list new-root-off added?)
(defn chamt-put [coll-ptr node-off shift hash key val]
  (let [ptr (coll-ptr-at coll-ptr node-off)
        bitmap (load32 ptr CN_BITMAP)
        cnt (popcount32 bitmap)]

    ;; Collision node (bitmap = 0)
    (if (zero? bitmap)
      (let [;; Check existing children
            c0 (load32 ptr (+ CN_CHILDREN 0))
            c1 (load32 ptr (+ CN_CHILDREN CN_CHILD_SZ))
            lk0 (load32 (coll-ptr-at coll-ptr c0) CL_KEY)
            lk1 (load32 (coll-ptr-at coll-ptr c1) CL_KEY)]
        (cond
          ;; Replace existing key in slot 0
          (= lk0 key)
            (let [new-leaf (coll-alloc-leaf key val coll-ptr)
                  new-off (coll-alloc-node 2 coll-ptr)
                  new-ptr (coll-ptr-at coll-ptr new-off)]
              (store32! new-ptr CN_BITMAP 0)
              (store32! new-ptr (+ CN_CHILDREN 0) new-leaf)
              (store32! new-ptr (+ CN_CHILDREN CN_CHILD_SZ) c1)
              (list new-off false))
          ;; Replace existing key in slot 1
          (= lk1 key)
            (let [new-leaf (coll-alloc-leaf key val coll-ptr)
                  new-off (coll-alloc-node 2 coll-ptr)
                  new-ptr (coll-ptr-at coll-ptr new-off)]
              (store32! new-ptr CN_BITMAP 0)
              (store32! new-ptr (+ CN_CHILDREN 0) c0)
              (store32! new-ptr (+ CN_CHILDREN CN_CHILD_SZ) new-leaf)
              (list new-off false))
          ;; New key: expand collision to 3 children
          :else (let [new-leaf (coll-alloc-leaf key val coll-ptr)
                  new-off (coll-alloc-node 3 coll-ptr)
                  new-ptr (coll-ptr-at coll-ptr new-off)]
              (store32! new-ptr CN_BITMAP 0)
              (store32! new-ptr (+ CN_CHILDREN 0) c0)
              (store32! new-ptr (+ CN_CHILDREN CN_CHILD_SZ) c1)
              (store32! new-ptr (+ CN_CHILDREN (* 2 CN_CHILD_SZ)) new-leaf)
              (list new-off true))))

      ;; Normal node
      (let [bit (bit-shift-left 1 (bit-and (bit-shift-right hash shift) HAMT_MASK))
            idx (popcount32 (bit-and bitmap (dec bit)))]

        (if (zero? (bit-and bitmap bit))
          ;; Empty slot: insert new leaf
          (let [leaf (coll-alloc-leaf key val coll-ptr)
                new-off (coll-alloc-node (inc cnt) coll-ptr)
                new-ptr (coll-ptr-at coll-ptr new-off)]
            (store32! new-ptr CN_BITMAP (bit-or bitmap bit))
            ;; Copy children before idx
            (loop [i 0]
              (when (< i idx)
                (store32! new-ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))
                  (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))))
                (recur (inc i))))
            ;; Insert new leaf at idx
            (store32! new-ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ)) leaf)
            ;; Copy children after idx
            (loop [i idx]
              (when (< i cnt)
                (store32! new-ptr (+ CN_CHILDREN (* (inc i) CN_CHILD_SZ))
                  (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))))
                (recur (inc i))))
            (list new-off true))

          ;; Slot occupied
          (let [child-val (load32 ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ)))]
            (if (node? child-val)
              ;; Sub-node: recurse
              (let [result (chamt-put coll-ptr (node-idx child-val)
                                     (+ shift HAMT_BITS) hash key val)
                    new-child (first result)
                    added (second result)
                    ;; Clone node with updated child
                    new-off (coll-alloc-node cnt coll-ptr)
                    new-ptr (coll-ptr-at coll-ptr new-off)]
                (store32! new-ptr CN_BITMAP bitmap)
                ;; Copy all children
                (loop [i 0]
                  (when (< i cnt)
                    (store32! new-ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))
                      (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))))
                    (recur (inc i))))
                ;; Overwrite child at idx
                (store32! new-ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ))
                  (bit-or new-child TAG_NODE))
                (list new-off added))

              ;; Leaf: check if same key
              (let [leaf-ptr (coll-ptr-at coll-ptr child-val)
                    lk (load32 leaf-ptr CL_KEY)]
                (if (= lk key)
                  ;; Same key: replace value
                  (let [lv (load64 leaf-ptr CL_VALUE)]
                    (if (= lv val)
                      (list node-off false)  ;; identical value: no change
                      (let [new-leaf (coll-alloc-leaf key val coll-ptr)
                            new-off (coll-alloc-node cnt coll-ptr)
                            new-ptr (coll-ptr-at coll-ptr new-off)]
                        (store32! new-ptr CN_BITMAP bitmap)
                        (loop [i 0]
                          (when (< i cnt)
                            (store32! new-ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))
                              (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))))
                            (recur (inc i))))
                        (store32! new-ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ)) new-leaf)
                        (list new-off false))))
                  ;; Different key: split into sub-node
                  (let [h2 (hash32 lk)
                        new-leaf (coll-alloc-leaf key val coll-ptr)
                        sub (chamt-merge-leaves coll-ptr (+ shift HAMT_BITS)
                                                h2 child-val hash new-leaf)
                        new-off (coll-alloc-node cnt coll-ptr)
                        new-ptr (coll-ptr-at coll-ptr new-off)]
                    (store32! new-ptr CN_BITMAP bitmap)
                    (loop [i 0]
                      (when (< i cnt)
                        (store32! new-ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))
                          (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ))))
                        (recur (inc i))))
                    (store32! new-ptr (+ CN_CHILDREN (* idx CN_CHILD_SZ))
                      (bit-or sub TAG_NODE))
                    (list new-off true))))))))))))

;; ================================================================
;; Public pmap API
;; ================================================================
;;
;; Operates on CPMap handles (16 bytes, bump-allocated from g_step or g_main).
;; Tree nodes live in g_coll.
;;
;; Small map optimization: maps with ≤16 entries use a flat array
;; (linear scan). Promotes to HAMT when full.

;; pmap-get: lookup key in persistent map
;; handle-ptr = pointer to CPMap struct
;; coll-ptr = pointer to g_coll Mem struct
(defn pmap-get [handle-ptr coll-ptr key]
  (let [count (load32 handle-ptr PM_COUNT)]
    (if (zero? count) nil
      (let [root (load32 handle-ptr PM_ROOT)
            is-small (load32 handle-ptr PM_IS_SMALL)]
        (if (not (zero? is-small))
          ;; Small map: linear scan
          ;; Small maps store entries as CLeaf[] starting at root offset
          (let [base-ptr (coll-ptr-at coll-ptr root)]
            (loop [i 0]
              (if (>= i count) nil
                (let [entry-ptr (+ base-ptr (* i CL_SIZE))
                      lk (load32 entry-ptr CL_KEY)]
                  (if (= lk key)
                    (load64 entry-ptr CL_VALUE)
                    (recur (inc i)))))))
          ;; HAMT lookup
          (chamt-get coll-ptr root 0 (hash32 key) key))))))

;; pmap-assoc: immutable put, returns pointer to new CPMap handle
;; Allocates new handle from step-ptr (g_step Mem)
(defn pmap-assoc [handle-ptr coll-ptr step-ptr key val]
  (let [count (load32 handle-ptr PM_COUNT)
        root (load32 handle-ptr PM_ROOT)
        is-small (load32 handle-ptr PM_IS_SMALL)]

    (if (zero? count)
      ;; Empty: create first small map entry
      (let [leaf-off (coll-alloc-leaf key val coll-ptr)
            ;; Allocate new handle
            new-h (bump step-ptr PM_SIZE)
            h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
        (store32! h-ptr PM_COUNT 1)
        (store32! h-ptr PM_ROOT leaf-off)
        (store32! h-ptr PM_TXN 0)
        (store32! h-ptr PM_IS_SMALL 1)
        h-ptr)

      (if (not (zero? is-small))
        ;; Small map
        (let [base-ptr (coll-ptr-at coll-ptr root)]
          ;; Check if key exists
          (let [found (loop [i 0]
                       (if (>= i count) -1
                         (if (= (load32 (+ base-ptr (* i CL_SIZE)) CL_KEY) key)
                           i (recur (inc i)))))]
            (if (>= found 0)
              ;; Replace: copy all entries, update found
              (let [new-root (bump coll-ptr (* count CL_SIZE))
                    new-base (coll-ptr-at coll-ptr new-root)]
                ;; Copy entries
                (loop [i 0]
                  (when (< i count)
                    (let [src (+ base-ptr (* i CL_SIZE))
                          dst (+ new-base (* i CL_SIZE))]
                      (store32! dst CL_KEY (load32 src CL_KEY))
                      (store64! dst CL_VALUE
                        (if (= i found) val (load64 src CL_VALUE))))
                    (recur (inc i))))
                ;; New handle
                (let [new-h (bump step-ptr PM_SIZE)
                      h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
                  (store32! h-ptr PM_COUNT count)
                  (store32! h-ptr PM_ROOT new-root)
                  (store32! h-ptr PM_TXN 0)
                  (store32! h-ptr PM_IS_SMALL 1)
                  h-ptr))

              (if (< count SMALL_MAX)
                ;; Append: copy + add new entry
                (let [new-count (inc count)
                      new-root (bump coll-ptr (* new-count CL_SIZE))
                      new-base (coll-ptr-at coll-ptr new-root)]
                  ;; Copy existing
                  (loop [i 0]
                    (when (< i count)
                      (let [src (+ base-ptr (* i CL_SIZE))
                            dst (+ new-base (* i CL_SIZE))]
                        (store32! dst CL_KEY (load32 src CL_KEY))
                        (store64! dst CL_VALUE (load64 src CL_VALUE)))
                      (recur (inc i))))
                  ;; Add new entry
                  (let [dst (+ new-base (* count CL_SIZE))]
                    (store32! dst CL_KEY key)
                    (store64! dst CL_VALUE val))
                  ;; New handle
                  (let [new-h (bump step-ptr PM_SIZE)
                        h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
                    (store32! h-ptr PM_COUNT new-count)
                    (store32! h-ptr PM_ROOT new-root)
                    (store32! h-ptr PM_TXN 0)
                    (store32! h-ptr PM_IS_SMALL 1)
                    h-ptr))

                ;; Promote to HAMT: insert all small entries + new key
                (let [;; Create first leaf + node
                      k0 (load32 base-ptr CL_KEY)
                      v0 (load64 base-ptr CL_VALUE)
                      first-leaf (coll-alloc-leaf k0 v0 coll-ptr)
                      hamt-root (chamt-leaf-node coll-ptr (hash32 k0) 0 first-leaf)
                      ;; Insert remaining small entries
                      final-root
                        (loop [i 1 r hamt-root]
                          (if (>= i count) r
                            (let [src (+ base-ptr (* i CL_SIZE))
                                  ek (load32 src CL_KEY)
                                  ev (load64 src CL_VALUE)
                                  result (chamt-put coll-ptr r 0 (hash32 ek) ek ev)]
                              (recur (inc i) (first result)))))
                      ;; Insert new key
                      result (chamt-put coll-ptr final-root 0 (hash32 key) key val)
                      new-root (first result)
                      added (second result)
                      new-count (if added (inc count) count)
                      ;; New handle
                      new-h (bump step-ptr PM_SIZE)
                      h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
                  (store32! h-ptr PM_COUNT new-count)
                  (store32! h-ptr PM_ROOT new-root)
                  (store32! h-ptr PM_TXN 0)
                  (store32! h-ptr PM_IS_SMALL 0)
                  h-ptr)))))

        ;; HAMT put
        (let [result (chamt-put coll-ptr root 0 (hash32 key) key val)
              new-root (first result)
              added (second result)
              new-count (if added (inc count) count)
              new-h (bump step-ptr PM_SIZE)
              h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
          (store32! h-ptr PM_COUNT new-count)
          (store32! h-ptr PM_ROOT new-root)
          (store32! h-ptr PM_TXN 0)
          (store32! h-ptr PM_IS_SMALL 0)
          h-ptr)))))

;; ================================================================
;; PVEC — Persistent Vector (32-way trie with tail)
;; ================================================================

;; pvec-get: indexed access
(defn pvec-get [handle-ptr coll-ptr idx]
  (let [count (load32 handle-ptr PV_COUNT)]
    (if (>= idx count) nil
      (let [tail-len (load32 handle-ptr PV_TAIL_LEN)
            tail-off (- count tail-len)]
        (if (>= idx tail-off)
          ;; In tail
          (let [tail (load32 handle-ptr PV_TAIL)
                leaf-ptr (coll-ptr-at coll-ptr tail)]
            (load64 leaf-ptr (* (- idx tail-off) VL_ITEM_SZ)))
          ;; Walk interior nodes
          (let [root (load32 handle-ptr PV_ROOT)
                shift (load32 handle-ptr PV_SHIFT)]
            (loop [node-off root level shift]
              (if (<= level VEC_BITS)
                ;; Bottom level: children are CVLeaf offsets
                (let [node-ptr (coll-ptr-at coll-ptr node-off)
                      child-idx (bit-and (bit-shift-right idx level) VEC_MASK)
                      leaf-off (load32 node-ptr (* child-idx VN_CHILD_SZ))
                      leaf-ptr (coll-ptr-at coll-ptr leaf-off)]
                  (load64 leaf-ptr (* (bit-and idx VEC_MASK) VL_ITEM_SZ)))
                ;; Interior: descend
                (let [node-ptr (coll-ptr-at coll-ptr node-off)
                      child-idx (bit-and (bit-shift-right idx level) VEC_MASK)
                      child-off (load32 node-ptr (* child-idx VN_CHILD_SZ))]
                  (recur child-off (- level VEC_BITS)))))))))))

;; pvec-new-path: create a path of interior nodes down to a leaf
(defn pvec-new-path [coll-ptr shift tail-off]
  (if (= shift 0) tail-off
    (let [nid (coll-alloc-vnode coll-ptr)
          ptr (coll-ptr-at coll-ptr nid)]
      (store32! ptr 0 (pvec-new-path coll-ptr (- shift VEC_BITS) tail-off))
      nid)))

;; pvec-push-tail: push old tail into the trie
(defn pvec-push-tail [coll-ptr count shift parent-off tail-off]
  (let [sub-idx (bit-and (bit-shift-right (dec count) shift) VEC_MASK)
        nid (coll-alloc-vnode coll-ptr)
        new-ptr (coll-ptr-at coll-ptr nid)
        old-ptr (coll-ptr-at coll-ptr parent-off)]
    ;; Copy parent's children to new node
    (loop [i 0]
      (when (< i VEC_WIDTH)
        (store32! new-ptr (* i VN_CHILD_SZ)
          (load32 old-ptr (* i VN_CHILD_SZ)))
        (recur (inc i))))
    ;; Update the sub-idx slot
    (if (= shift VEC_BITS)
      ;; Bottom: point to tail directly
      (store32! new-ptr (* sub-idx VN_CHILD_SZ) tail-off)
      ;; Interior: recurse or create path
      (let [existing (load32 old-ptr (* sub-idx VN_CHILD_SZ))]
        (if (not= existing NIL_IDX)
          (store32! new-ptr (* sub-idx VN_CHILD_SZ)
            (pvec-push-tail coll-ptr count (- shift VEC_BITS) existing tail-off))
          (store32! new-ptr (* sub-idx VN_CHILD_SZ)
            (pvec-new-path coll-ptr (- shift VEC_BITS) tail-off)))))
    nid))

;; pvec-conj: append value, returns pointer to new CPVec handle
(defn pvec-conj [handle-ptr coll-ptr step-ptr val]
  (let [count    (load32 handle-ptr PV_COUNT)
        shift    (load32 handle-ptr PV_SHIFT)
        root     (load32 handle-ptr PV_ROOT)
        tail     (load32 handle-ptr PV_TAIL)
        tail-len (load32 handle-ptr PV_TAIL_LEN)]

    (if (< tail-len VEC_WIDTH)
      ;; Room in tail: copy tail + append
      (let [new-tail (coll-alloc-vleaf coll-ptr)
            new-tail-ptr (coll-ptr-at coll-ptr new-tail)]
        ;; Copy existing tail items
        (when (not= tail NIL_IDX)
          (let [old-tail-ptr (coll-ptr-at coll-ptr tail)]
            (loop [i 0]
              (when (< i tail-len)
                (store64! new-tail-ptr (* i VL_ITEM_SZ)
                  (load64 old-tail-ptr (* i VL_ITEM_SZ)))
                (recur (inc i))))))
        ;; Append new value
        (store64! new-tail-ptr (* tail-len VL_ITEM_SZ) val)
        ;; New handle
        (let [new-h (bump step-ptr PV_SIZE)
              h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
          (store32! h-ptr PV_COUNT (inc count))
          (store32! h-ptr PV_SHIFT shift)
          (store32! h-ptr PV_ROOT root)
          (store32! h-ptr PV_TAIL new-tail)
          (store32! h-ptr PV_TAIL_LEN (inc tail-len))
          h-ptr))

      ;; Tail full: push old tail into trie, start new tail
      (let [new-tail (coll-alloc-vleaf coll-ptr)
            new-tail-ptr (coll-ptr-at coll-ptr new-tail)]
        (store64! new-tail-ptr 0 val)  ;; single item in new tail
        (let [new-h (bump step-ptr PV_SIZE)
              h-ptr (+ (load64 step-ptr MEM_BASE) new-h)]
          (if (= root NIL_IDX)
            ;; First trie node
            (let [new-root (coll-alloc-vnode coll-ptr)
                  root-ptr (coll-ptr-at coll-ptr new-root)]
              (store32! root-ptr 0 tail)  ;; old tail becomes child 0
              (store32! h-ptr PV_COUNT (inc count))
              (store32! h-ptr PV_SHIFT VEC_BITS)
              (store32! h-ptr PV_ROOT new-root)
              (store32! h-ptr PV_TAIL new-tail)
              (store32! h-ptr PV_TAIL_LEN 1)
              h-ptr)
            ;; Trie exists
            (if (> (bit-shift-right count VEC_BITS)
                   (bit-shift-left 1 shift))
              ;; Overflow: grow root
              (let [new-root (coll-alloc-vnode coll-ptr)
                    root-ptr (coll-ptr-at coll-ptr new-root)]
                (store32! root-ptr 0 root)
                (store32! root-ptr VN_CHILD_SZ
                  (pvec-new-path coll-ptr shift tail))
                (store32! h-ptr PV_COUNT (inc count))
                (store32! h-ptr PV_SHIFT (+ shift VEC_BITS))
                (store32! h-ptr PV_ROOT new-root)
                (store32! h-ptr PV_TAIL new-tail)
                (store32! h-ptr PV_TAIL_LEN 1)
                h-ptr)
              ;; Push tail into existing trie
              (let [new-root (pvec-push-tail coll-ptr count shift root tail)]
                (store32! h-ptr PV_COUNT (inc count))
                (store32! h-ptr PV_SHIFT shift)
                (store32! h-ptr PV_ROOT new-root)
                (store32! h-ptr PV_TAIL new-tail)
                (store32! h-ptr PV_TAIL_LEN 1)
                h-ptr)))))))))

;; ================================================================
;; HAMT Iteration
;; ================================================================
;;
;; Walk all leaves in HAMT, applying f to each (key, value).
;; Used by pmap-seq, pmap-foreach, and epoch rotation.

(defn chamt-iter [coll-ptr node-off f]
  (let [ptr (coll-ptr-at coll-ptr node-off)
        bitmap (load32 ptr CN_BITMAP)]
    (if (zero? bitmap)
      ;; Collision: iterate children directly
      (loop [i 0]
        (when (< i 3)
          (let [child (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ)))]
            (when (not (zero? child))
              (let [leaf-ptr (coll-ptr-at coll-ptr child)]
                (f (load32 leaf-ptr CL_KEY)
                   (load64 leaf-ptr CL_VALUE)))))
          (recur (inc i))))
      ;; Normal: iterate by popcount
      (let [cnt (popcount32 bitmap)]
        (loop [i 0]
          (when (< i cnt)
            (let [child (load32 ptr (+ CN_CHILDREN (* i CN_CHILD_SZ)))]
              (if (node? child)
                (chamt-iter coll-ptr (node-idx child) f)
                (let [leaf-ptr (coll-ptr-at coll-ptr child)]
                  (f (load32 leaf-ptr CL_KEY)
                     (load64 leaf-ptr CL_VALUE)))))
            (recur (inc i))))))))

;; ================================================================
;; Atom — mutable reference
;; ================================================================
;;
;; Atom is trivial: { u32 version, u32 _pad, Val value } = 16 bytes
;; The interesting part is that atoms ARE the handle to pmap/pvec.
;; atom(pmap) → the serializable managed heap.

(def AT_VERSION 0)   ;; u32
(def AT_PAD     4)   ;; u32
(def AT_VALUE   8)   ;; Val (u64)
(def AT_SIZE    16)

(defn atom-deref [atom-ptr]
  (load64 atom-ptr AT_VALUE))

(defn atom-reset! [atom-ptr val]
  (let [v (load32 atom-ptr AT_VERSION)]
    (store32! atom-ptr AT_VERSION (inc v))
    (store64! atom-ptr AT_VALUE val)
    val))
