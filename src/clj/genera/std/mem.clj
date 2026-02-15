;; mem.clj — Memory as View
;;
;; The allocator, written in Clojure, compiled to x86 by the JIT.
;; No C builtins. Same x86 emitter as cg-expr.
;;
;; Mem layout (16 bytes):
;;   offset 0: base (u64, pointer)
;;   offset 8: used (u32)
;;   offset 12: cap (u32)
;;
;; Global Mem addresses are JIT globals (like def'd values).
;; The JIT loads/stores them via x86-load-abs!/x86-store-abs!.
;;
;; Three ops, each provably minimal:
;;   bump:    3 instructions + 1 branch
;;   RESTORE: 1 store
;;   copy:    memcpy speed (~20 GB/s)

;; ================================================================
;; x86 helpers: pointer-relative load/store
;; ================================================================
;;
;; x86-load!/x86-store! use [RBP+off] (stack frame).
;; For Mem fields we need [reg+off] (pointer-relative).
;; Encoding: REX.W + MOV + ModR/M(disp8, dst, base)

;; emit: mov dst, [base + off8]   (load u64 from ptr+offset)
(defn emit-load-at [dst base off]
  (xb! 0x48)                          ;; REX.W
  (xb! 0x8B)                          ;; MOV r64, r/m64
  (xb! (+ (* dst 8) base 0x40))       ;; ModR/M: mod=01 (disp8), reg=dst, rm=base
  (xb! off))                          ;; disp8

;; emit: mov [base + off8], src   (store u64 to ptr+offset)
(defn emit-store-at [base off src]
  (xb! 0x48)                          ;; REX.W
  (xb! 0x89)                          ;; MOV r/m64, r64
  (xb! (+ (* src 8) base 0x40))       ;; ModR/M: mod=01 (disp8), reg=src, rm=base
  (xb! off))                          ;; disp8

;; emit: mov eax, [base + off8]  (load u32 from ptr+offset)
(defn emit-load32-at [dst base off]
  (xb! 0x8B)                          ;; MOV r32, r/m32 (no REX.W → 32-bit)
  (xb! (+ (* dst 8) base 0x40))       ;; ModR/M
  (xb! off))

;; emit: mov [base + off8], eax  (store u32 to ptr+offset)
(defn emit-store32-at [base off src]
  (xb! 0x89)                          ;; MOV r/m32, r32 (no REX.W → 32-bit)
  (xb! (+ (* src 8) base 0x40))       ;; ModR/M
  (xb! off))

;; ================================================================
;; Mem struct offsets
;; ================================================================

(def MEM_BASE 0)    ;; u64 *base  — offset 0
(def MEM_USED 8)    ;; u32 used   — offset 8
(def MEM_CAP  12)   ;; u32 cap    — offset 12
(def MEM_SIZE 16)   ;; total struct size

;; ================================================================
;; cg-bump: compile bump allocator
;; ================================================================
;;
;; Input: RAX = pointer to Mem struct, RDI = size (bytes)
;; Output: RAX = pointer to allocated region (base + old_used)
;;
;; Emits:
;;   mov  ecx, [rax+8]       ; load used
;;   lea  edx, [rcx+rdi]     ; end = used + size
;;   cmp  edx, [rax+12]      ; compare with cap
;;   ja   overflow
;;   mov  [rax+8], edx       ; store new used
;;   mov  rax, [rax]         ; load base
;;   add  rax, rcx           ; return base + old_used

(defn cg-bump []
  (emit-load32-at RCX RAX MEM_USED)   ;; ecx = m->used
  ;; lea edx, [rcx + rdi]
  (xb! 0x8D) (xb! 0x14) (xb! 0x39)   ;; lea edx, [rcx+rdi]
  (emit-load32-at RSI RAX MEM_CAP)    ;; esi = m->cap  (using RSI as temp)
  (x86-cmp! RDX RSI)                  ;; cmp end, cap
  (let [overflow (x86-jcc! CC_G)]     ;; ja overflow
    (emit-store32-at RAX MEM_USED RDX) ;; m->used = end
    (x86-mov! RCX RCX)                ;; zero-extend ecx → rcx (u32 → u64)
    (emit-load-at RAX RAX MEM_BASE)   ;; rax = m->base
    (x86-add! RAX RCX)                ;; rax = base + old_used
    (let [done (x86-jmp!)]
      (x86-patch! overflow)
      (x86-imm! RAX 0)                ;; overflow: return NULL (or signal)
      (x86-patch! done))))

;; ================================================================
;; cg-restore: compile RESTORE (bulk free)
;; ================================================================
;;
;; Input: RAX = pointer to Mem struct, RDI = mark (u32)
;; Emits:  mov [rax+8], edi   — one store

(defn cg-restore []
  (emit-store32-at RAX MEM_USED RDI))

;; ================================================================
;; cg-mark: compile MARK (capture current position)
;; ================================================================
;;
;; Input: RAX = pointer to Mem struct
;; Output: EAX = current used offset
;; Emits:  mov eax, [rax+8]   — one load

(defn cg-mark []
  (emit-load32-at RAX RAX MEM_USED))

;; ================================================================
;; JIT integration: allocating from specific Mem instances
;; ================================================================
;;
;; The JIT compiles (cons a d) to:
;;   1. Evaluate a → push
;;   2. Evaluate d → push
;;   3. bump(&g_step, 16, 8) → get pointer
;;   4. Store car, cdr at pointer
;;
;; With Mem globals registered as JIT globals:
;;   (def G_STEP  (jit-register-global! :g-step))
;;   (def G_MAIN  (jit-register-global! :g-main))
;;   (def G_COLL  (jit-register-global! :g-coll))
;;
;; cg-cons emits:
;;   x86-load-abs! G_STEP     ; rax = &g_step (Mem pointer)
;;   x86-imm! RDI 16          ; size = sizeof(Cons)
;;   call bump                 ; rax = allocated pointer
;;   pop cdr → store at [rax+8]
;;   pop car → store at [rax+0]
;;   tag as cons (OR with TAG_CONS)

;; ================================================================
;; Commit: JIT-compiled value walk
;; ================================================================
;;
;; commit is a regular Clojure function. When JIT-compiled, it becomes
;; native x86 that walks Val types and copies heap values from step to main.
;;
;; The JIT compiles the type dispatch (cond on type tags) to a sequence
;; of cmp/je branches — the same code GCC would emit for the C switch.
;;
;; No C "arena-copy!" builtin. The Clojure IS the copy logic.
;; The JIT IS the compiler. The x86 IS the allocator.

;; commit will be defined in commit.clj using these primitives.

;; ================================================================
;; Interpreted memory ops (bootstrap)
;; ================================================================
;; JIT versions: cg-bump, cg-mark, cg-restore (above).
;; These run under the C eval during bootstrap.

(defn bump [mem-ptr size]
  (let [used (load32 mem-ptr MEM_USED)
        end  (+ used size)]
    (if (> end (load32 mem-ptr MEM_CAP))
      nil
      (do (store32! mem-ptr MEM_USED end)
          (+ (load64 mem-ptr MEM_BASE) used)))))

(defn mark [mem-ptr]
  (load32 mem-ptr MEM_USED))

(defn restore! [mem-ptr saved]
  (store32! mem-ptr MEM_USED saved))

;; ================================================================
;; Step boundary: MARK/RESTORE
;; ================================================================
;;
;; The REPL loop (or any step boundary) does:
;;   mark = cg-mark(g_step)
;;   ... eval ...
;;   cg-restore(g_step, mark)
;;
;; One load, one store. The 90%+ of allocations that are step temps
;; die here. Free.

;; ================================================================
;; Epoch rotation: copy live coll nodes
;; ================================================================
;;
;; Same shape as every other view traversal.
;; Walk handles in global env → for each pmap/pvec handle:
;;   copy reachable tree nodes from g_coll[epoch] to g_coll[1-epoch]
;;   rewrite u32 indices during copy (no forwarding table)
;; Then: RESTORE g_coll[epoch], swap epoch.
;;
;; All Clojure. The JIT compiles the walk + copy to native x86.
;; The copy itself is memcpy-speed (just MOV instructions in a loop).
