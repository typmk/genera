;; epoch.clj — Coll Reclamation (V_LIVE copy)
;;
;; Pure Clojure. JIT-compiled to native x86.
;; Uses mem.clj's emit-load-at/emit-store-at for node access.
;;
;; Same shape as every other view traversal:
;;   Scan a set, test a predicate, act.
;;
;; | View   | Scans     | Predicate           | Action                |
;; |--------|-----------|---------------------|-----------------------|
;; | V_PURE | GNode[]   | all children pure?  | mark pure             |
;; | V_DEAD | GNode[]   | referenced?         | mark dead             |
;; | V_LIVE | coll Mem  | reachable from root? | copy to other epoch  |
;;
;; Why this works:
;;   - 90%+ of allocations are step temps — never reach V_LIVE
;;   - Coll nodes are u32-indexed — no forwarding table needed
;;   - Walks HANDLES (few), not heap — O(handles × tree depth)
;;   - Depth = log32(count) ≈ 3-4 for millions of entries
;;   - Triggered when coll Mem fills, not periodic

;; Constants and popcount32 provided by coll.clj (loaded before epoch.clj).
;; bump/mark/restore! provided by mem.clj.

;; ================================================================
;; copy-pmap-node: recursively copy HAMT node from old → new epoch
;; ================================================================
;;
;; Walks the tree depth-first. For each node:
;;   1. Allocate new node in destination epoch (bump)
;;   2. Copy bitmap + owner
;;   3. For each child:
;;      - If leaf: allocate + copy leaf in destination
;;      - If sub-node: recurse
;;      - Rewrite child index to new location
;;
;; Returns: new node offset in destination epoch

(defn copy-pmap-node [src-base dst-mem node-off]
  ;; Read bitmap from source
  (let [bitmap (load32 (+ src-base node-off) CN_BITMAP)
        n-children (popcount32 bitmap)
        ;; Allocate new node in destination
        new-size (+ CN_CHILDREN (* n-children CN_CHILD_SZ))
        new-off  (bump dst-mem new-size)]
    ;; Copy header (bitmap + owner)
    (let [dst-base (load64 dst-mem MEM_BASE)
          new-ptr  (+ dst-base new-off)
          old-ptr  (+ src-base node-off)]
      ;; Copy bitmap
      (store32! new-ptr CN_BITMAP bitmap)
      ;; Set owner to 0 (persistent after copy)
      (store32! new-ptr CN_OWNER 0)
      ;; Copy/recurse children
      (loop [i 0]
        (when (< i n-children)
          (let [child-off (+ CN_CHILDREN (* i CN_CHILD_SZ))
                child (load32 old-ptr child-off)]
            (if (not (zero? (bit-and child TAG_NODE)))
              ;; Sub-node: recurse
              (let [sub-off (bit-and-not child TAG_NODE)
                    new-sub (copy-pmap-node src-base dst-mem sub-off)]
                (store32! new-ptr child-off
                  (bit-or new-sub TAG_NODE)))
              ;; Leaf: copy leaf to destination
              (let [new-leaf (copy-leaf src-base dst-mem child)]
                (store32! new-ptr child-off new-leaf))))
          (recur (inc i)))))
    new-off))

;; ================================================================
;; copy-leaf: copy a CLeaf from old → new epoch
;; ================================================================

(defn copy-leaf [src-base dst-mem leaf-idx]
  ;; TODO: leaf pool is separate array, indexed differently
  ;; For now: stub that returns the same index
  ;; (In Mem model, leaves are inline in the same arena)
  leaf-idx)

;; ================================================================
;; epoch-rotate: the top-level operation
;; ================================================================
;;
;; Walk all pmap/pvec handles in global env.
;; Copy reachable tree nodes from g_coll[epoch] to g_coll[1-epoch].
;; RESTORE old epoch. Swap.

(defn epoch-rotate [env coll-pair epoch]
  (let [src (if (zero? epoch) (first coll-pair) (second coll-pair))
        dst (if (zero? epoch) (second coll-pair) (first coll-pair))
        src-base (load64 src MEM_BASE)]
    ;; Reset destination
    (store32! dst MEM_USED 0)
    ;; Walk env, copy reachable coll nodes
    ;; (env-walk would iterate all bindings, check type tags,
    ;;  and call copy-pmap-node / copy-pvec-node for each handle)
    ;;
    ;; For each binding in env:
    ;;   val = env-get(binding)
    ;;   if pmap? → copy-pmap-node(src-base, dst, handle.root)
    ;;   if pvec? → copy-pvec-trie(src-base, dst, handle.root, handle.tail)
    ;;   rewrite handle to point to new locations
    ;;
    ;; Then: swap epoch
    (- 1 epoch)))

;; bump/mark/restore! now in std/mem.clj.
;; load32/store32!/load64/store64! are C builtins (bootstrap)
;; that dissolve into inline MOV when the JIT compiles itself.
