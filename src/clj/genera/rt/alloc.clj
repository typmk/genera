;; alloc.clj — Allocation Views
;;
;; Extends the pass system (boot.clj) with allocation analysis.
;; Same mechanism as V_PURE, V_DEAD — bitmask scan, test predicate, produce bits.
;;
;; These five views push every node as high up the cascade as possible.
;; The JIT/eval/cc reads the classification and emits the corresponding op.
;;
;; C constants required:
;;   V_ALLOC      — does this node allocate? (1-bit per node)
;;   V_ALLOC_SIZE — how many bytes? (u32 per node, stored in val[])
;;   V_ALLOC_OFF  — offset into buffer (prefix sum, stored in val[])
;;   V_SCOPE      — doesn't escape scope (MARK/RESTORE)
;;   V_DYNAMIC    — unknown count (runtime bump)
;;   V_LIVE       — reachable from root (survives epoch)

;; ================================================================
;; Node kind → allocation size
;; ================================================================
;;
;; What the views can see:
;;
;; | Node kind          | Allocates    | Size         | Static? |
;; |--------------------|-------------|-------------|---------|
;; | int, sym, kw, bool | nothing     | 0           | yes     |
;; | call to builtin    | nothing     | 0           | yes     |
;; | call to closure    | 1 Env       | sizeof(Env) | yes     |
;; | fn (closure)       | 1 FnObj     | 24B         | yes     |
;; | quoted list        | N cons      | N * 16B     | yes     |
;; | let binding        | 1 Env       | sizeof(Env) | yes     |
;; | def                | 1 commit    | walk size   | yes     |
;; | non-tail recursion | unbounded   | unknown     | no      |
;; | dynamic dispatch   | unknown     | unknown     | no      |

(def SIZE_CONS 16)     ;; sizeof(Cons): car + cdr
(def SIZE_ENV  24)     ;; sizeof(Env): parent + count + capacity (min)
(def SIZE_FN   24)     ;; sizeof(FnObj): type + params + body + env

(defn alloc-size [id]
  (let [k (gn-kind id)]
    (cond
      ;; Immediates: no allocation
      (= k NK_NUM)   0
      (= k NK_IDENT) 0
      (= k NK_KW)    0
      (= k NK_STR)   0   ;; interned — no heap alloc

      ;; fn (closure): one FnObj
      (and (= k NK_LIST) (gn-child id)
           (= (gn-kind (gn-child id)) NK_IDENT)
           (= (gn-sym (gn-child id)) SYM_FN))
        SIZE_FN

      ;; let/loop: one Env
      (and (= k NK_LIST) (gn-child id)
           (= (gn-kind (gn-child id)) NK_IDENT)
           (or (= (gn-sym (gn-child id)) SYM_LET)
               (= (gn-sym (gn-child id)) SYM_LOOP)))
        SIZE_ENV

      ;; Quoted list: count children × SIZE_CONS
      (and (= k NK_QUOTE) (gn-child id))
        (let [n (loop [c (gn-child id) count 0]
                  (if (= c 0) count
                    (recur (gn-next c) (inc count))))]
          (* n SIZE_CONS))

      ;; Vec literal: pvec handle (but NOT binding vectors in let/loop)
      (= k NK_VEC)
        (let [p (gn-parent id)]
          (if (and (not (= p 0))
                   (= (gn-kind p) NK_LIST)
                   (let [fc (gn-child p)]
                     (and (not (= fc 0))
                          (= (gn-kind fc) NK_IDENT)
                          (or (= (gn-sym fc) SYM_LET)
                              (= (gn-sym fc) SYM_LOOP)
                              (= (gn-sym fc) SYM_FN)
                              (= (gn-sym fc) SYM_DEFN)
                              (= (gn-sym fc) SYM_DEFMACRO)))))
            0            ;; binding/param vector — no runtime alloc
            20))         ;; data vector — sizeof(CPVec)

      ;; Map literal: pmap handle
      (= k NK_MAP) 12    ;; sizeof(CPMap)

      1 0)))

;; ================================================================
;; Pass: V_ALLOC + V_ALLOC_OFF (prefix sum)
;; ================================================================
;;
;; Single forward scan. For each node that allocates, record the
;; offset into the step buffer. Total = budget for this step.
;;
;; After this pass, the JIT can emit:
;;   - One bounds check per step (budget vs remaining capacity)
;;   - Per allocation site: one LEA (base + offset), no call, no branch

(defn clj-pass-alloc [n]
  (loop [i 0 off 0]
    (when (< i n)
      (let [sz (alloc-size i)]
        (if (> sz 0)
          (do (view-set! V_ALLOC i)
              (val-set! i off)         ;; V_ALLOC_OFF stored in val[]
              (recur (inc i) (+ off sz)))
          (recur (inc i) off))))))

;; ================================================================
;; Pass: V_SCOPE (escape analysis)
;; ================================================================
;;
;; Nodes marked V_SCOPE don't escape their enclosing fn/let scope.
;; They can be reclaimed with MARK/RESTORE at scope exit.
;;
;; A node escapes if:
;;   - It's the return value of a function
;;   - It's stored in a def'd binding
;;   - It's passed to a function that might capture it
;;   - It's stored in a persistent collection
;;
;; A node is V_SCOPE if:
;;   - It's in tail position of a let (but not a defn/def)
;;   - All its consumers are within the same scope
;;
;; Conservative: if unsure, don't mark V_SCOPE (safe default = V_DYNAMIC).

(defn clj-pass-scope-alloc [n]
  (loop [i 0]
    (when (< i n)
      (when (view? V_ALLOC i)
        ;; If this node is inside a let/loop but NOT in tail position
        ;; of a defn, and NOT referenced by a def, it's scope-local.
        (let [sc (scope-get i)]
          (when (and (not (= sc 0))
                     (not (view? V_TAIL i))
                     (not (view? V_DEF i)))
            (view-set! V_SCOPE i))))
      (recur (inc i)))))

;; ================================================================
;; V_DYNAMIC: fallback for unknown allocation
;; ================================================================
;;
;; Nodes that allocate but can't be statically sized:
;;   - Non-tail recursion (unbounded env chain)
;;   - Dynamic dispatch (unknown target, unknown allocation)
;;   - Varargs with unknown count
;;
;; These use runtime bump allocation (3 instructions + 1 branch).
;; Still fast — just not as fast as V_ALLOC_OFF (1 instruction).

(defn clj-pass-dynamic [n]
  (loop [i 0]
    (when (< i n)
      ;; Non-tail recursive calls that allocate
      (when (and (view? V_CALL i)
                 (not (view? V_TAIL i))
                 (not (view? V_PURE i)))
        (view-set! V_DYNAMIC i))
      (recur (inc i)))))

;; ================================================================
;; Entry point: run all alloc passes
;; ================================================================

(defn analyze-alloc! []
  (let [n (gn-count)]
    (clj-pass-alloc n)
    (clj-pass-scope-alloc n)
    (clj-pass-dynamic n)))
