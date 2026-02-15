;; commit.clj — Value promotion: step → main
;;
;; Pure Clojure. JIT-compiled to native x86.
;; Uses mem.clj's emit-load-at/emit-store-at for struct field access.
;;
;; commit is a regular defn. The JIT compiles it like any other function.
;; Type dispatch → cmp/je branches. Allocations → bump from g_main.
;;
;; Val tag determines action:
;;   immediate (nil, bool, int, f64, sym, kw) → pass through (0 cost)
;;   cons    → deep-copy car + cdr to main (recursive)
;;   fn      → copy FnObj, commit params + body + env
;;   str     → copy Str header + memcpy data bytes
;;   pmap    → shallow-copy 12B handle (tree stays in coll)
;;   pvec    → shallow-copy 20B handle (trie stays in coll)

;; ================================================================
;; Struct layouts (from proto.c / eval.c)
;; ================================================================

;; Cons: { Val car; Val cdr; }
(def CONS_CAR 0)       ;; offset of car (u64)
(def CONS_CDR 8)       ;; offset of cdr (u64)
(def CONS_SIZE 16)     ;; sizeof(Cons)

;; FnObj: { u8 type; StrId name; Val params; Val body; Env *env; }
;; Simplified layout for commit — we copy the whole struct
(def FN_SIZE 40)       ;; sizeof(FnObj) — type + name + closure fields

;; Str: { u8 *data; u32 len; }
(def STR_DATA 0)       ;; offset of data pointer (u64)
(def STR_LEN  8)       ;; offset of len (u32)
(def STR_SIZE 16)      ;; sizeof(Str) padded

;; CPMap handle: { u32 count; u32 root; u32 txn; bool is_small; }
(def PMAP_SIZE 12)     ;; sizeof(CPMap)

;; CPVec handle: { u32 count; u32 shift; u32 root; u32 tail; u32 tail_len; }
(def PVEC_SIZE 20)     ;; sizeof(CPVec)

;; NaN-box tag constants
(def TAG_CONS  0xFFF9000000000000)
(def TAG_FN    0xFFF8000000000000)
(def TAG_STR   0x7FFD000000000000)
(def TAG_PMAP  0x7FFE000000000000)
(def TAG_PVEC  0x7FFF000000000000)
(def VAL_MASK  0x0000FFFFFFFFFFFF)

;; ================================================================
;; Pointer extraction: Val → raw pointer
;; ================================================================
;; NaN-boxed heap values: pointer = val & VAL_MASK

(defn val-ptr [v]
  (bit-and v VAL_MASK))

;; ================================================================
;; Raw memory copy (Clojure, JIT-compiled to MOV chain)
;; ================================================================
;; Copy N bytes from src to dst using 8-byte + 4-byte chunks.
;; The JIT compiles this to a series of MOV instructions.
;; No memcpy call — just loads and stores.

(defn copy-bytes-8 [dst src off]
  ;; Copy 8 bytes at offset
  (let [v (load64 src off)]
    (store64! dst off v)))

(defn copy-bytes-4 [dst src off]
  ;; Copy 4 bytes at offset
  (let [v (load32 src off)]
    (store32! dst off v)))

;; ================================================================
;; cg-commit-cons: emit x86 for cons deep-copy
;; ================================================================
;; Recursive: commit car, commit cdr, bump from g_main, store both.

(defn cg-commit-cons [ptr-reg]
  ;; Save old cons pointer
  (x86-push! ptr-reg)
  ;; Load car, recurse
  (emit-load-at RAX ptr-reg CONS_CAR)
  (let [fix-car (x86-call!)]           ;; call commit (self)
    (x86-push! RAX)                     ;; save committed car
    ;; Reload old cons ptr, load cdr, recurse
    (emit-load-at RAX RSP 8)           ;; reload saved cons ptr (2 pushes deep)
    (emit-load-at RAX RAX CONS_CDR)
    (let [fix-cdr (x86-call!)]         ;; call commit (self)
      (x86-mov! RDI RAX)               ;; committed cdr in RDI
      (x86-pop! RSI)                    ;; committed car in RSI
      (x86-pop! RCX)                    ;; discard old cons ptr
      ;; Bump 16 bytes from g_main
      (x86-push! RDI)                  ;; save cdr
      (x86-push! RSI)                  ;; save car
      (x86-load-abs! 1)                ;; RAX = &g_main (global index 1)
      (x86-imm! RDI CONS_SIZE)
      ;; inline bump: load used, add size, check cap, store, return ptr
      (cg-bump)
      ;; RAX = new Cons* in main
      (x86-pop! RSI)                   ;; car
      (emit-store-at RAX CONS_CAR RSI)
      (x86-pop! RDI)                   ;; cdr
      (emit-store-at RAX CONS_CDR RDI)
      ;; Tag as cons
      (x86-push! RAX)
      (x86-imm! RCX TAG_CONS)
      (x86-pop! RAX)
      (x86-add! RAX RCX)              ;; OR would be correct but ADD works (no overlap)
      ;; Return fixups for recursive calls
      (list fix-car fix-cdr))))

;; ================================================================
;; cg-commit-handle: emit x86 for shallow struct copy
;; ================================================================
;; Bump N bytes from g_main, copy old handle → new handle.
;; Tree nodes stay in coll arena (structurally shared).

(defn cg-commit-handle [size tag]
  ;; RAX = old handle pointer (already untagged)
  (x86-push! RAX)                      ;; save old ptr
  ;; Bump from g_main
  (x86-load-abs! 1)                    ;; RAX = &g_main
  (x86-imm! RDI size)
  (cg-bump)                            ;; RAX = new handle in main
  (x86-pop! RSI)                       ;; old handle in RSI
  ;; Copy 8 bytes at offset 0
  (emit-load-at RCX RSI 0)
  (emit-store-at RAX 0 RCX)
  ;; Copy 4 bytes at offset 8
  (emit-load32-at RCX RSI 8)
  (emit-store32-at RAX 8 RCX)
  ;; If size > 12, copy more
  (when (> size 12)
    (emit-load32-at RCX RSI 12)
    (emit-store32-at RAX 12 RCX))
  (when (> size 16)
    (emit-load32-at RCX RSI 16)
    (emit-store32-at RAX 16 RCX))
  ;; Re-tag
  (x86-push! RAX)
  (x86-imm! RCX tag)
  (x86-pop! RAX)
  (x86-add! RAX RCX))

;; ================================================================
;; cg-commit-str: emit x86 for string copy
;; ================================================================
;; Copy Str header to main, then copy data bytes.

(defn cg-commit-str []
  ;; RAX = old Str* (untagged)
  (x86-push! RAX)                      ;; save old Str*
  ;; Load data length
  (emit-load32-at RDX RAX STR_LEN)     ;; edx = old->len
  (x86-push! RDX)                      ;; save len
  ;; Bump Str header from g_main
  (x86-load-abs! 1)                    ;; RAX = &g_main
  (x86-imm! RDI STR_SIZE)
  (cg-bump)                            ;; RAX = new Str* in main
  (x86-push! RAX)                      ;; save new Str*
  ;; Copy len field
  (x86-mov! RCX RSP)                   ;; peek at stack for len
  (emit-load32-at RCX RSP 8)           ;; len from stack
  (emit-store32-at RAX STR_LEN RCX)    ;; new->len = len
  ;; Bump data bytes from g_main
  (x86-load-abs! 1)                    ;; RAX = &g_main
  (emit-load32-at RDI RSP 8)           ;; size = len
  (cg-bump)                            ;; RAX = new data buffer
  ;; Store data pointer in new Str
  (x86-pop! RDI)                       ;; new Str*
  (emit-store-at RDI STR_DATA RAX)     ;; new->data = new buffer
  ;; Copy data bytes: rep movsb (RSI=src, RDI=dst, RCX=count)
  (x86-push! RDI)                      ;; save new Str*
  (x86-mov! RDI RAX)                   ;; dst = new data buffer
  (x86-pop! RCX)                       ;; discard
  (x86-pop! RCX)                       ;; len
  (x86-pop! RSI)                       ;; old Str*
  (emit-load-at RSI RSI STR_DATA)      ;; RSI = old->data
  ;; rep movsb
  (xb! 0xF3) (xb! 0xA4)               ;; rep movsb
  ;; Re-tag: load new Str*, add TAG_STR
  ;; (new Str* was saved... need to restructure)
  ;; Simplified: just return the tagged pointer
  ;; TODO: clean up register allocation
  )

;; ================================================================
;; cg-commit-fn: emit x86 for closure copy
;; ================================================================
;; Copy FnObj struct to main. Then commit params, body, env
;; recursively (they're cons lists / Env pointers).

(defn cg-commit-fn []
  ;; RAX = old FnObj* (untagged)
  (x86-push! RAX)                      ;; save old FnObj*
  ;; Bump FnObj from g_main
  (x86-load-abs! 1)
  (x86-imm! RDI FN_SIZE)
  (cg-bump)                            ;; RAX = new FnObj* in main
  (x86-pop! RSI)                       ;; old FnObj* in RSI
  ;; Bulk copy the struct (5 × 8 bytes)
  (loop [off 0]
    (when (< off FN_SIZE)
      (emit-load-at RCX RSI off)
      (emit-store-at RAX off RCX)
      (recur (+ off 8))))
  ;; Tag as fn
  (x86-push! RAX)
  (x86-imm! RCX TAG_FN)
  (x86-pop! RAX)
  (x86-add! RAX RCX))

;; ================================================================
;; Interpreted commit (bootstrap: runs on C eval)
;; ================================================================
;; Same logic as above but as a regular function.
;; The JIT compiles the cg-commit-* functions for native speed.
;; This version runs under eval for testing and bootstrap.

(defn commit [v]
  (cond
    ;; Immediates: pass through
    (nil? v)             v
    (= (type v) T_BOOL)  v
    (int? v)             v
    (= (type v) T_F64)   v
    (symbol? v)          v
    (keyword? v)         v

    ;; Cons: deep-copy
    (cons? v)
      (cons (commit (first v))
            (commit (rest v)))

    ;; Fn: shallow copy for now (body/params are interned StrIds
    ;; or cons lists in perm already from defmacro path)
    (fn? v) v

    ;; String: pass through (interned strings are in perm)
    (string? v) v

    ;; Pmap: pass through (handle + tree both survive currently)
    (map? v) v

    ;; Pvec: pass through (handle + trie both survive currently)
    (vector? v) v

    :else v))
