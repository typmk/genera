;; shim.clj — Pure Clojure implementations of genera's C builtins
;;
;; Load this BEFORE genera.clj when running under bb.
;; Provides: memory, bit ops, type system, grammar stubs, x86 stubs.

(import '[java.nio ByteBuffer ByteOrder])

;; ================================================================
;; Virtual Memory — flat address space via ByteBuffer
;; ================================================================
;; All "pointers" are integer offsets into one buffer.
;; Same semantics as C: load8(addr), store64(base, off, val).

(def ^:private vmem-size (* 512 1024 1024))
(def ^:private ^ByteBuffer vmem
  (doto (ByteBuffer/allocateDirect vmem-size)
    (.order ByteOrder/LITTLE_ENDIAN)))
(def ^:private vmem-top (atom 0))

(defn ^:private vmem-alloc! [size]
  (let [addr @vmem-top
        aligned (bit-and (+ size 7) (bit-not 7))]  ;; 8-byte align
    (swap! vmem-top + aligned)
    addr))

;; --- Raw load/store ---

(defn load8 [addr]
  (bit-and (.get vmem (int addr)) 0xFF))

(defn load32
  ([addr] (.getInt vmem (int addr)))
  ([base offset] (.getInt vmem (int (+ base offset)))))

(defn load64
  ([addr] (.getLong vmem (int addr)))
  ([base offset] (.getLong vmem (int (+ base offset)))))

(defn store8 [addr val]
  (.put vmem (int addr) (unchecked-byte val))
  nil)

(defn store32
  ([addr val] (.putInt vmem (int addr) (int val)) nil)
  ([base offset val] (.putInt vmem (int (+ base offset)) (int val)) nil))

(defn store64
  ([addr val] (.putLong vmem (int addr) (long val)) nil)
  ([base offset val] (.putLong vmem (int (+ base offset)) (long val)) nil))

;; Bang variants (same functions, genera registers both names)
(def store8! store8)
(def store32! store32)
(def store64! store64)


;; ================================================================
;; Mem struct: {base:u64 @0, used:u32 @8, cap:u32 @12} = 16 bytes
;; ================================================================
;; mem-new! allocates a Mem struct + arena from vmem.
;; Returns address of Mem struct (integer).

(def ^:private MEM_HDR 16)

(defn mem-new! [size]
  (let [addr (vmem-alloc! size)
        arena (+ addr MEM_HDR)
        cap (- size MEM_HDR)]
    (store64 addr 0 arena)    ;; base
    (store32 addr 8 0)        ;; used
    (store32 addr 12 cap)     ;; cap
    addr))

(defn mem-base [mem] (load64 mem 0))
(defn mem-used [mem] (load32 mem 8))

(defn mem-bump! [mem size]
  (let [used (load32 mem 8)
        end (+ used size)
        cap (load32 mem 12)]
    (when (> end cap)
      (throw (ex-info "mem-bump!: OOM" {:used used :size size :cap cap})))
    (store32 mem 8 end)
    (+ (load64 mem 0) used)))

(defn mem-restore! [mem saved]
  (store32 mem 8 saved))

(defn mem-u32 [mem offset]
  (.getInt vmem (int (+ (load64 mem 0) offset))))

(defn mem-u32! [mem offset val]
  (.putInt vmem (int (+ (load64 mem 0) offset)) (int val))
  nil)

;; --- Val storage: JVM objects can't be stored as bytes ---
;; Side table: absolute-address → JVM object
(def ^:private val-store (atom {}))

(defn mem-val [mem offset]
  (let [addr (+ (load64 mem 0) offset)]
    (get @val-store addr)))

(defn mem-val! [mem offset val]
  (let [addr (+ (load64 mem 0) offset)]
    (swap! val-store assoc addr val)
    nil))

(defn mem-copy! [mem dst-off src-off len]
  (let [base (int (load64 mem 0))
        src (+ base (int src-off))
        dst (+ base (int dst-off))]
    ;; Copy bytes
    (dotimes [i (int len)]
      (.put vmem (int (+ dst i)) (.get vmem (int (+ src i)))))
    ;; Copy val-store entries in range
    (let [vs @val-store]
      (doseq [a (range src (+ src (int len)) 8)]
        (when-let [v (get vs a)]
          (swap! val-store assoc (+ dst (- a src)) v)))))
  nil)

(defn mem-fill! [mem offset byte-val len]
  (let [base (int (load64 mem 0))
        start (+ base (int offset))
        b (unchecked-byte byte-val)]
    (dotimes [i (int len)]
      (.put vmem (int (+ start i)) b)))
  nil)


;; ================================================================
;; Bit ops
;; ================================================================

(defn popcount [x] (Long/bitCount (long x)))

(defn hash32 [x]
  ;; FNV-1a style hash for u32 keys
  (let [h (int x)
        h (unchecked-int (bit-xor h (bit-shift-right h 16)))
        h (unchecked-int (* h (unchecked-int 0x45d9f3b)))
        h (unchecked-int (bit-xor h (bit-shift-right h 16)))
        h (unchecked-int (* h (unchecked-int 0x45d9f3b)))
        h (unchecked-int (bit-xor h (bit-shift-right h 16)))]
    (bit-and h 0xFFFFFFFF)))

;; bit-and, bit-or, bit-xor, bit-not, bit-shift-left, bit-shift-right
;; are already in clojure.core


;; ================================================================
;; Type constants + type function
;; ================================================================

(def T_NIL  0)
(def T_BOOL 1)
(def T_INT  2)
(def T_SYM  3)
(def T_KW   4)
(def T_STR  5)
(def T_PMAP 6)
(def T_PVEC 7)
(def T_FN   8)
(def T_CONS 9)
(def T_F64  10)

;; Override clojure.core/type to return genera tag indices
(defn type [x]
  (cond
    (nil? x)          T_NIL
    (boolean? x)      T_BOOL
    (int? x)          T_INT
    (symbol? x)       T_SYM
    (keyword? x)      T_KW
    (string? x)       T_STR
    (map? x)          T_PMAP
    (vector? x)       T_PVEC
    (fn? x)           T_FN
    (seq? x)          T_CONS
    (float? x)        T_F64
    :else             -1))

;; Genera cons supports non-seq cdr: (cons 5 true) → pair
;; Clojure's cons requires seqable second arg.
;; Solution: use clojure.core/cons when possible, vector pair otherwise.

(defrecord Pair [car cdr])

(let [cc clojure.core/cons
      cf clojure.core/first
      cn clojure.core/next]

  (defn cons [a b]
    (if (or (nil? b) (sequential? b) (instance? clojure.lang.ISeq b))
      (cc a b)
      (->Pair a b)))

  (defn first [x]
    (if (instance? Pair x) (.-car ^Pair x) (cf x)))

  (defn rest [x]
    (if (instance? Pair x) (.-cdr ^Pair x) (cn x)))  ;; cn = next (returns nil at end)

  (defn seq [x]
    (cond
      (nil? x) nil
      (instance? Pair x) x
      :else (clojure.core/seq x))))

(defn cons? [x]
  (or (instance? Pair x) (instance? clojure.lang.Cons x) (list? x)))


;; ================================================================
;; Register constants
;; ================================================================

(def RAX 0) (def RCX 1) (def RDX 2) (def RBX 3)
(def RSP 4) (def RBP 5) (def RSI 6) (def RDI 7)
(def R12 12) (def R13 13) (def R14 14) (def R15 15)

(def CC_E  0x4) (def CC_NE 0x5)
(def CC_L  0xC) (def CC_GE 0xD)
(def CC_LE 0xE) (def CC_G  0xF)


;; ================================================================
;; x86 Code Buffer — accumulates bytes for ELF output
;; ================================================================

(def ^:private code-buf (atom {:bytes (byte-array (* 1024 1024)) :pos 0}))

(defn xb! [b]
  (let [{:keys [^bytes bytes pos]} @code-buf]
    (aset bytes (int pos) (unchecked-byte b))
    (swap! code-buf update :pos inc))
  nil)

(defn x32! [v]
  (let [{:keys [^bytes bytes pos]} @code-buf
        p (int pos)]
    (aset bytes p       (unchecked-byte (bit-and v 0xFF)))
    (aset bytes (+ p 1) (unchecked-byte (bit-and (bit-shift-right v 8) 0xFF)))
    (aset bytes (+ p 2) (unchecked-byte (bit-and (bit-shift-right v 16) 0xFF)))
    (aset bytes (+ p 3) (unchecked-byte (bit-and (bit-shift-right v 24) 0xFF)))
    (swap! code-buf update :pos + 4))
  nil)

(defn code-pos [] (:pos @code-buf))

(defn code-reset! [] (swap! code-buf assoc :pos 0) nil)

;; x86 instruction emitters — write to code-buf
;; These mirror the C functions in sys/x86.c

(defn x86-imm! [reg val]
  (if (and (>= val -2147483648) (<= val 2147483647))
    ;; mov reg, imm32 (sign-extended)
    (do (xb! 0x48) (xb! 0xC7) (xb! (+ 0xC0 reg)) (x32! val))
    ;; mov reg, imm64
    (do (xb! (+ 0x48 (if (>= reg 8) 1 0)))
        (xb! (+ 0xB8 (bit-and reg 7)))
        (x32! (bit-and val 0xFFFFFFFF))
        (x32! (bit-and (bit-shift-right val 32) 0xFFFFFFFF)))))

(defn x86-mov! [dst src]
  (xb! 0x48) (xb! 0x89) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-add! [dst src]
  (xb! 0x48) (xb! 0x01) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-sub! [dst src]
  (xb! 0x48) (xb! 0x29) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-cmp! [a b]
  (xb! 0x48) (xb! 0x39) (xb! (+ 0xC0 (* b 8) a)))

(defn x86-test! [a b]
  (xb! 0x48) (xb! 0x85) (xb! (+ 0xC0 (* b 8) a)))

(defn x86-imul! [dst src]
  (xb! 0x48) (xb! 0x0F) (xb! 0xAF) (xb! (+ 0xC0 (* dst 8) src)))

(defn x86-idiv! [src]
  (xb! 0x48) (xb! 0xF7) (xb! (+ 0xF8 src)))

(defn x86-neg! [reg]
  (xb! 0x48) (xb! 0xF7) (xb! (+ 0xD8 reg)))

(defn x86-push! [reg]
  (if (>= reg 8) (xb! 0x41))
  (xb! (+ 0x50 (bit-and reg 7))))

(defn x86-pop! [reg]
  (if (>= reg 8) (xb! 0x41))
  (xb! (+ 0x58 (bit-and reg 7))))

(defn x86-load! [dst off]
  ;; mov dst, [rbp + off]
  (xb! 0x48) (xb! 0x8B) (xb! (+ 0x85 (* dst 8))) (x32! off))

(defn x86-store! [off src]
  ;; mov [rbp + off], src
  (xb! 0x48) (xb! 0x89) (xb! (+ 0x85 (* src 8))) (x32! off))

(defn x86-load-abs! [dst addr]
  ;; mov dst, [addr]  (RIP-relative or absolute)
  (xb! 0x48) (xb! 0x8B) (xb! (+ 0x04 (* dst 8))) (xb! 0x25) (x32! addr))

(defn x86-store-abs! [addr src]
  (xb! 0x48) (xb! 0x89) (xb! (+ 0x04 (* src 8))) (xb! 0x25) (x32! addr))

(defn x86-jmp! []
  (xb! 0xE9) (let [p (code-pos)] (x32! 0) p))

(defn x86-call! [target]
  (xb! 0xE8) (x32! (- target (+ (code-pos) 4))))

(defn x86-ret! [] (xb! 0xC3))

(defn x86-jcc! [cc]
  (xb! 0x0F) (xb! (+ 0x80 cc)) (let [p (code-pos)] (x32! 0) p))

(defn x86-patch! [at]
  (let [cur (code-pos)
        rel (- cur at 4)
        {:keys [^bytes bytes]} @code-buf
        p (int at)]
    (aset bytes p       (unchecked-byte (bit-and rel 0xFF)))
    (aset bytes (+ p 1) (unchecked-byte (bit-and (bit-shift-right rel 8) 0xFF)))
    (aset bytes (+ p 2) (unchecked-byte (bit-and (bit-shift-right rel 16) 0xFF)))
    (aset bytes (+ p 3) (unchecked-byte (bit-and (bit-shift-right rel 24) 0xFF)))))

(defn x86-jmp-to! [target]
  (xb! 0xE9) (x32! (- target (+ (code-pos) 4))))

(defn x86-setcc! [cc dst]
  (xb! 0x0F) (xb! (+ 0x90 cc)) (xb! (+ 0xC0 dst))
  ;; movzx dst, dst8
  (xb! 0x48) (xb! 0x0F) (xb! 0xB6) (xb! (+ 0xC0 (* dst 8) dst)))

(defn x86-prologue! [frame-size]
  (x86-push! RBP)
  (x86-mov! RBP RSP)
  (when (> frame-size 0)
    (xb! 0x48) (xb! 0x81) (xb! 0xEC) (x32! frame-size)))

(defn x86-epilogue! []
  (x86-mov! RSP RBP)
  (x86-pop! RBP)
  (x86-ret!))

(defn x86-add-i8! [reg imm]
  (xb! 0x48) (xb! 0x83) (xb! (+ 0xC0 reg)) (xb! (bit-and imm 0xFF)))

(defn x86-sub-i8! [reg imm]
  (xb! 0x48) (xb! 0x83) (xb! (+ 0xE8 reg)) (xb! (bit-and imm 0xFF)))

(def x86-push-r! x86-push!)
(def x86-pop-r! x86-pop!)
(def x86-mov-rr! x86-mov!)

(defn x86-and! [dst src]
  (xb! 0x48) (xb! 0x21) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-or! [dst src]
  (xb! 0x48) (xb! 0x09) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-xor! [dst src]
  (xb! 0x48) (xb! 0x31) (xb! (+ 0xC0 (* src 8) dst)))

(defn x86-bit-not! [reg]
  (xb! 0x48) (xb! 0xF7) (xb! (+ 0xD0 reg)))

(defn x86-shl-cl! [reg]
  (xb! 0x48) (xb! 0xD3) (xb! (+ 0xE0 reg)))

(defn x86-shr-cl! [reg]
  (xb! 0x48) (xb! 0xD3) (xb! (+ 0xE8 reg)))

(defn x86-and-i! [reg imm]
  (xb! 0x48) (xb! 0x81) (xb! (+ 0xE0 reg)) (x32! imm))

(defn x86-popcnt! [dst src]
  (xb! 0xF3) (xb! 0x48) (xb! 0x0F) (xb! 0xB8) (xb! (+ 0xC0 (* dst 8) src)))

(defn x86-load-base! [dst base off]
  (xb! 0x48) (xb! 0x8B) (xb! (+ 0x80 (* dst 8) base)) (x32! off))

(defn x86-store-base! [base off src]
  (xb! 0x48) (xb! 0x89) (xb! (+ 0x80 (* src 8) base)) (x32! off))

(defn x86-load32-base! [dst base off]
  (xb! 0x8B) (xb! (+ 0x80 (* dst 8) base)) (x32! off))

(defn x86-store32-base! [base off src]
  (xb! 0x89) (xb! (+ 0x80 (* src 8) base)) (x32! off))

(defn x86-load8-base! [dst base off]
  (xb! 0x48) (xb! 0x0F) (xb! 0xB6) (xb! (+ 0x80 (* dst 8) base)) (x32! off))

(defn x86-store8-base! [base off src]
  (when (>= src 4) (xb! 0x40))
  (xb! 0x88) (xb! (+ 0x80 (* src 8) base)) (x32! off))


;; ================================================================
;; Compiler state stubs
;; ================================================================

(def ^:private comp-state
  (atom {:locals {}       ;; name → slot
         :next-slot 0
         :cs []           ;; callee-saved pool
         :cs-mode false
         :loop-start 0
         :in-loop false
         :loop-count 0
         :loop-vars []}))

(defn comp-reset! []
  (reset! comp-state {:locals {} :next-slot 0 :cs [] :cs-mode false
                       :loop-start 0 :in-loop false :loop-count 0 :loop-vars []})
  nil)

(defn comp-alloc-slot! [name]
  (let [slot (:next-slot @comp-state)]
    (swap! comp-state update :locals assoc name slot)
    (swap! comp-state update :next-slot inc)
    slot))

(defn comp-find-local [name]
  (get (:locals @comp-state) name -1))

(defn comp-find-cs [name]
  ;; Find in callee-saved pool
  -1)

(defn comp-alloc-cs-temp! [] 0)
(defn comp-free-cs-temp! [slot] nil)
(defn comp-save-locals! [] nil)
(defn comp-restore-locals! [] nil)
(defn comp-set-slot! [name slot]
  (swap! comp-state update :locals assoc name slot) nil)

(defn comp-cs-setup! [] nil)
(defn comp-cs-name! [slot name] nil)
(defn comp-cs-mode? [] (:cs-mode @comp-state))

(defn comp-loop-start [] (:loop-start @comp-state))
(defn comp-set-loop-start! [pos]
  (swap! comp-state assoc :loop-start pos) nil)
(defn comp-in-loop? [] (:in-loop @comp-state))
(defn comp-set-in-loop! [v]
  (swap! comp-state assoc :in-loop v) nil)
(defn comp-loop-var! [name slot]
  (swap! comp-state update :loop-vars clojure.core/conj [name slot]) nil)
(defn comp-loop-count [] (:loop-count @comp-state))
(defn comp-set-loop-count! [n]
  (swap! comp-state assoc :loop-count n) nil)
(defn comp-loop-off [i]
  (get-in @comp-state [:loop-vars i 1] 0))
(defn comp-loop-save! [] nil)
(defn comp-loop-restore! [] nil)

(def N_CS 5)


;; ================================================================
;; JIT registry stubs
;; ================================================================

(def ^:private jit-state
  (atom {:fns {}        ;; name → address
         :globals {}    ;; name → address
         :gen 0
         :fixes []}))

(defn jit-gen-inc! []
  (swap! jit-state update :gen inc)
  (:gen @jit-state))

(defn jit-fix-reset! []
  (swap! jit-state assoc :fixes []) nil)

(defn jit-register-fn! [name addr]
  (swap! jit-state update :fns assoc name addr) nil)

(defn jit-find-fn [name]
  (get (:fns @jit-state) name 0))

(defn jit-register-global! [name]
  (let [addr (vmem-alloc! 8)]
    (swap! jit-state update :globals assoc name addr)
    addr))

(defn jit-find-global [name]
  (get (:globals @jit-state) name 0))

(defn jit-global-addr [name]
  (get (:globals @jit-state) name 0))

(defn jit-add-fix! [name pos]
  (swap! jit-state update :fixes clojure.core/conj [name pos]) nil)

(defn jit-patch-calls! [] nil)

(defn jit-exec! [addr & args]
  ;; Can't execute x86 in bb — stub
  (println "WARN: jit-exec! called in bb (no-op)")
  0)

(defn arg-reg [i]
  (case (int i)
    0 RDI  1 RSI  2 RDX  3 RCX  4 8  5 9  ;; R8=8, R9=9
    RDI))

(defn cs-pool [] [RBX R12 R13 R14 R15])


;; ================================================================
;; Grammar / GNode stubs
;; ================================================================
;; Full parser TBD. For now, stubs that let all files load.

(def ^:private gram-state
  (atom {:nodes []     ;; [{:kind :child :next :parent :sym :pos :len}]
         :source ""
         :views {}     ;; view-id → #{node-ids}
         :vals {}      ;; node-id → val
         :scopes {}    ;; node-id → scope-id
         :binds {}}))  ;; node-id → def-id

;; Node kind constants
(def NK_ROOT 0) (def NK_LIST 1) (def NK_VEC 2) (def NK_MAP 3)
(def NK_QUOTE 4) (def NK_SYNTAX_QUOTE 5) (def NK_UNQUOTE 6) (def NK_SPLICE 7)
(def NK_IDENT 8) (def NK_NUM 9) (def NK_STR 10) (def NK_OP 11) (def NK_KW 12)

;; View constants
(def V_DEF 0) (def V_REF 1) (def V_CALL 2)
(def V_TAIL 3) (def V_PURE 4) (def V_CONST 5) (def V_DEAD 6)
(def V_INT 7) (def V_VEC 8) (def V_MAP 9) (def V_FN 10)
(def V_ALLOC 11) (def V_SCOPE 12) (def V_DYNAMIC 13) (def V_LIVE 14)

;; Symbol constants (interned string IDs — sequential for now)
(def ^:private sym-table (atom {:next 100 :name->id {} :id->name {}}))

(defn ^:private ensure-sym [name id]
  (swap! sym-table update :name->id assoc name id)
  (swap! sym-table update :id->name assoc id name))

;; Pre-register known symbols with fixed IDs
(def SYM_NIL 1) (ensure-sym "nil" 1)
(def SYM_TRUE 2) (ensure-sym "true" 2)
(def SYM_FALSE 3) (ensure-sym "false" 3)
(def SYM_FN 4) (ensure-sym "fn" 4)
(def SYM_DEF 5) (ensure-sym "def" 5)
(def SYM_DEFN 6) (ensure-sym "defn" 6)
(def SYM_IF 7) (ensure-sym "if" 7)
(def SYM_LET 8) (ensure-sym "let" 8)
(def SYM_DO 9) (ensure-sym "do" 9)
(def SYM_LOOP 10) (ensure-sym "loop" 10)
(def SYM_QUOTE 11) (ensure-sym "quote" 11)
(def SYM_AND 12) (ensure-sym "and" 12)
(def SYM_OR 13) (ensure-sym "or" 13)
(def SYM_COND 14) (ensure-sym "cond" 14)
(def SYM_WHEN 15) (ensure-sym "when" 15)
(def SYM_DEFMACRO 16) (ensure-sym "defmacro" 16)
(def SYM_SYNTAX_QUOTE 17) (ensure-sym "syntax-quote" 17)
(def SYM_ADD 18) (ensure-sym "+" 18)
(def SYM_SUB 19) (ensure-sym "-" 19)
(def SYM_MUL 20) (ensure-sym "*" 20)
(def SYM_DIV 21) (ensure-sym "/" 21)
(def SYM_MOD 22) (ensure-sym "mod" 22)
(def SYM_EQ 23) (ensure-sym "=" 23)
(def SYM_LT 24) (ensure-sym "<" 24)
(def SYM_GT 25) (ensure-sym ">" 25)
(def SYM_LTE 26) (ensure-sym "<=" 26)
(def SYM_GTE 27) (ensure-sym ">=" 27)
(def SYM_NOT 28) (ensure-sym "not" 28)
(def SYM_INC 29) (ensure-sym "inc" 29)
(def SYM_DEC 30) (ensure-sym "dec" 30)
(def SYM_ZEROQ 31) (ensure-sym "zero?" 31)
(def SYM_POSQ 32) (ensure-sym "pos?" 32)
(def SYM_NEGQ 33) (ensure-sym "neg?" 33)
(def SYM_RECUR 34) (ensure-sym "recur" 34)
(def SYM_PRINTLN 35) (ensure-sym "println" 35)
(def SYM_STAR 20) ;; alias for SYM_MUL
(def SYM_BAND 36) (ensure-sym "bit-and" 36)
(def SYM_BOR 37) (ensure-sym "bit-or" 37)
(def SYM_BXOR 38) (ensure-sym "bit-xor" 38)
(def SYM_BNOT 39) (ensure-sym "bit-not" 39)
(def SYM_BSHL 40) (ensure-sym "bit-shift-left" 40)
(def SYM_BSHR 41) (ensure-sym "bit-shift-right" 41)
(def SYM_POPCNT 42) (ensure-sym "popcount" 42)
(def SYM_LOAD64 43) (ensure-sym "load64" 43)
(def SYM_LOAD32 44) (ensure-sym "load32" 44)
(def SYM_LOAD8 45) (ensure-sym "load8" 45)
(def SYM_STORE64 46) (ensure-sym "store64" 46)
(def SYM_STORE32 47) (ensure-sym "store32" 47)
(def SYM_STORE8 48) (ensure-sym "store8" 48)

;; GNode accessors (stub — operate on gram-state)
(defn gn-kind [id] (get-in (:nodes @gram-state) [id :kind] 0))
(defn gn-child [id] (get-in (:nodes @gram-state) [id :child] 0))
(defn gn-next [id] (get-in (:nodes @gram-state) [id :next] 0))
(defn gn-parent [id] (get-in (:nodes @gram-state) [id :parent] 0))
(defn gn-sym [id] (get-in (:nodes @gram-state) [id :sym] 0))
(defn gn-count [] (count (:nodes @gram-state)))
(defn gn-parse-int [id] (get-in (:nodes @gram-state) [id :int-val] 0))
(defn gn-has-dot [id] (get-in (:nodes @gram-state) [id :has-dot] false))
(defn gn-end [id] (get-in (:nodes @gram-state) [id :end] 0))
(defn gn-text [id] (get-in (:nodes @gram-state) [id :text] ""))
(defn gn-has-recur? [id] false)  ;; stub

(defn sym-name [id] (get (:id->name @sym-table) id "?"))
(defn str-intern [s]
  (let [st @sym-table]
    (if-let [id (get (:name->id st) s)]
      id
      (let [id (:next st)]
        (swap! sym-table assoc :next (inc id))
        (ensure-sym s id)
        id))))

(defn gram-src-ptr []
  ;; Return vmem address where source is stored
  (get @gram-state :src-addr 0))

(defn gram-src-len []
  (count (:source @gram-state)))

(defn gram-render [id] "")  ;; stub
(defn gram-outline [] "")   ;; stub

;; View operations
(defn view? [view-id node-id]
  (contains? (get (:views @gram-state) view-id #{}) node-id))

(defn view-set! [view-id node-id]
  (swap! gram-state update-in [:views view-id] (fnil clojure.core/conj #{}) node-id)
  nil)

(defn val-get [node-id]
  (get (:vals @gram-state) node-id 0))

(defn val-set! [node-id val]
  (swap! gram-state assoc-in [:vals node-id] val)
  nil)

(defn scope-get [node-id]
  (get (:scopes @gram-state) node-id 0))

(defn scope-set! [node-id scope-id]
  (swap! gram-state assoc-in [:scopes node-id] scope-id)
  nil)

(defn bind-get [node-id]
  (get (:binds @gram-state) node-id 0))

(defn bind-set! [node-id def-id]
  (swap! gram-state assoc-in [:binds node-id] def-id)
  nil)

;; Query builtins
(defn pure-bi? [sym] false)   ;; stub — no pure builtins yet
(defn int-ret? [sym] false)   ;; stub
(defn special? [sym]
  (contains? #{SYM_DEF SYM_DEFN SYM_FN SYM_IF SYM_LET SYM_DO
               SYM_LOOP SYM_QUOTE SYM_SYNTAX_QUOTE SYM_DEFMACRO
               SYM_AND SYM_OR SYM_COND SYM_WHEN SYM_RECUR} sym))

(defn all-args-view? [fc view-id]
  ;; Check all args after fc have view-id set
  false)  ;; stub

(defn cv-eval [sym fc]
  ;; Constant-fold evaluation
  0)  ;; stub


;; ================================================================
;; Output buffer
;; ================================================================

(def ^:private out-buf (atom (StringBuilder.)))

(defn out-emit [s]
  (.append ^StringBuilder @out-buf (str s))
  nil)

(defn out-nl []
  (.append ^StringBuilder @out-buf "\n")
  nil)

(defn out-name [id]
  (out-emit (sym-name id)))

(defn out-reset []
  (reset! out-buf (StringBuilder.))
  nil)

(defn out-str []
  (.toString ^StringBuilder @out-buf))


;; ================================================================
;; Syscall shims (JVM equivalents)
;; ================================================================

(defn sys-write [fd buf len]
  ;; In bb, just print to stdout/stderr
  (when (or (= fd 1) (= fd 2))
    (let [sb (StringBuilder.)]
      (dotimes [i len]
        (.append sb (char (load8 (+ buf i)))))
      (if (= fd 1)
        (print (.toString sb))
        (binding [*out* *err*] (print (.toString sb))))))
  len)

(defn sys-read [fd buf len]
  ;; Stub — read from stdin
  0)

(defn sys-mmap [len]
  ;; Allocate from vmem
  (vmem-alloc! len))

(defn sys-exit [code]
  (System/exit code))


;; ================================================================
;; File loading — genera uses (load "path") to eval files
;; ================================================================

(defn load [path]
  (clojure.core/load-file path))


;; ================================================================
;; Misc
;; ================================================================

(defn macro? [x] false)  ;; stub for bb

(println "shim: loaded")
