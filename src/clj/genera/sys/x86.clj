;; sys.clj — System Calls from Clojure
;;
;; Pure Clojure. JIT-compiled to native x86.
;;
;; Linux x86_64 syscall convention:
;;   RAX = syscall number
;;   RDI = arg1, RSI = arg2, RDX = arg3
;;   R10 = arg4, R8 = arg5, R9 = arg6
;;   Clobbers: RAX, RCX, R11
;;   Return: RAX
;;
;; The instruction: 0F 05 (syscall)
;;
;; That's it. No libc. No wrapper. Just two bytes.

;; ================================================================
;; Syscall numbers (Linux x86_64)
;; ================================================================

(def SYS_READ    0)
(def SYS_WRITE   1)
(def SYS_OPEN    2)
(def SYS_CLOSE   3)
(def SYS_MMAP    9)
(def SYS_MUNMAP  11)
(def SYS_EXIT    60)
(def SYS_GETPID  39)

;; mmap flags
(def PROT_READ   1)
(def PROT_WRITE  2)
(def PROT_EXEC   4)
(def MAP_PRIVATE   0x02)
(def MAP_ANONYMOUS 0x20)

;; File descriptors
(def STDIN  0)
(def STDOUT 1)
(def STDERR 2)

;; ================================================================
;; x86 emission: syscall instruction
;; ================================================================

(defn emit-syscall! []
  (xb! 0x0F) (xb! 0x05))

;; ================================================================
;; cg-syscall: emit syscall with N args already in registers
;; ================================================================
;;
;; Caller is responsible for loading args into RDI, RSI, RDX, R10, R8, R9.
;; This just loads the syscall number and emits the instruction.
;;
;; Input: RAX = syscall number (already loaded)
;; Output: RAX = return value

(defn cg-syscall0 [nr]
  (x86-imm! RAX nr)
  (emit-syscall!))

(defn cg-syscall1 [nr]
  ;; RDI already loaded by caller
  (x86-imm! RAX nr)
  (emit-syscall!))

(defn cg-syscall2 [nr]
  ;; RDI, RSI already loaded
  (x86-imm! RAX nr)
  (emit-syscall!))

(defn cg-syscall3 [nr]
  ;; RDI, RSI, RDX already loaded
  (x86-imm! RAX nr)
  (emit-syscall!))

;; ================================================================
;; Compiled primitives
;; ================================================================

;; cg-write: write(fd, buf, len) → bytes written
;; Input: RDI=fd, RSI=buf, RDX=len
(defn cg-write []
  (cg-syscall3 SYS_WRITE))

;; cg-read: read(fd, buf, len) → bytes read
(defn cg-read []
  (cg-syscall3 SYS_READ))

;; cg-exit: exit(code) — does not return
(defn cg-exit []
  (cg-syscall1 SYS_EXIT))

;; cg-mmap: mmap(addr, len, prot, flags, fd, offset) → ptr
;; For anonymous mapping: addr=0, fd=-1, offset=0
;; Uses R10 for flags, R8 for fd, R9 for offset
(defn cg-mmap-anon [size]
  ;; RDI = addr (0 = let kernel choose)
  (x86-imm! RDI 0)
  ;; RSI = length
  (x86-imm! RSI size)
  ;; RDX = prot (READ|WRITE)
  (x86-imm! RDX (bit-or PROT_READ PROT_WRITE))
  ;; R10 = flags (PRIVATE|ANONYMOUS)
  ;; REX: 0x49 for R10
  (xb! 0x49) (xb! 0xC7) (xb! 0xC2)  ;; mov r10, imm32
  (x32! (bit-or MAP_PRIVATE MAP_ANONYMOUS))
  ;; R8 = fd (-1 for anonymous)
  (xb! 0x49) (xb! 0xC7) (xb! 0xC0)  ;; mov r8, imm32
  (x32! 0xFFFFFFFF)
  ;; R9 = offset (0)
  (xb! 0x49) (xb! 0xC7) (xb! 0xC1)  ;; mov r9, imm32
  (x32! 0)
  ;; syscall
  (x86-imm! RAX SYS_MMAP)
  (emit-syscall!))

;; cg-munmap: munmap(addr, len) → 0 on success
(defn cg-munmap []
  (cg-syscall2 SYS_MUNMAP))

;; cg-mmap-exec: mmap with PROT_EXEC for JIT code buffer
(defn cg-mmap-exec [size]
  (x86-imm! RDI 0)
  (x86-imm! RSI size)
  (x86-imm! RDX (bit-or PROT_READ (bit-or PROT_WRITE PROT_EXEC)))
  (xb! 0x49) (xb! 0xC7) (xb! 0xC2)
  (x32! (bit-or MAP_PRIVATE MAP_ANONYMOUS))
  (xb! 0x49) (xb! 0xC7) (xb! 0xC0)
  (x32! 0xFFFFFFFF)
  (xb! 0x49) (xb! 0xC7) (xb! 0xC1)
  (x32! 0)
  (x86-imm! RAX SYS_MMAP)
  (emit-syscall!))

;; ================================================================
;; Interpreted versions (for eval bootstrap)
;; ================================================================
;;
;; These need C builtins during bootstrap because the eval
;; interpreter can't execute raw syscall instructions.
;; Once the JIT compiles sys.clj, the C versions are dead.
;;
;; The 4 bootstrap builtins:
;;   sys-write  (fd buf len) → bytes
;;   sys-read   (fd buf len) → bytes
;;   sys-mmap   (len)        → ptr
;;   sys-exit   (code)       → void
;;
;; These are ~4 lines of C each (just the syscall wrapper).
;; After JIT compilation: zero C.

;; ================================================================
;; Mem initialization from Clojure
;; ================================================================
;;
;; Create a new Mem instance by mmap'ing memory.
;; Returns pointer to Mem struct (allocated at start of the region).
;;
;; Layout: first 16 bytes = Mem struct, rest = usable arena.
;;   Mem.base = region + 16
;;   Mem.used = 0
;;   Mem.cap  = size - 16

(defn make-mem [size]
  ;; Interpreted version: uses sys-mmap builtin
  (let [region (sys-mmap size)
        mem-ptr region
        arena-base (+ region MEM_SIZE)
        arena-cap (- size MEM_SIZE)]
    (store64! mem-ptr MEM_BASE arena-base)
    (store32! mem-ptr MEM_USED 0)
    (store32! mem-ptr MEM_CAP arena-cap)
    mem-ptr))

;; ================================================================
;; Init: create the three Mem arenas
;; ================================================================
;;
;; g_step: 16 MB — step temporaries, RESTORE'd every step
;; g_main: 16 MB — committed values, long-lived
;; g_coll: 64 MB × 2 — collection nodes, epoch pair
;;
;; After init, these are JIT globals accessible via x86-load-abs!.

(def STEP_SIZE (* 16 1024 1024))
(def MAIN_SIZE (* 16 1024 1024))
(def COLL_SIZE (* 64 1024 1024))

(defn init-mem []
  (let [step (make-mem STEP_SIZE)
        main (make-mem MAIN_SIZE)
        coll0 (make-mem COLL_SIZE)
        coll1 (make-mem COLL_SIZE)]
    (list step main coll0 coll1)))

;; ================================================================
;; Output: write to stdout
;; ================================================================
;;
;; print-str: write a string (pointer + length) to stdout
;; The JIT compiles this to:
;;   mov edi, 1       ; fd = stdout
;;   mov rsi, <buf>   ; pointer
;;   mov edx, <len>   ; length
;;   mov eax, 1       ; SYS_WRITE
;;   syscall

(defn print-str [buf len]
  (sys-write STDOUT buf len))

;; print-int: write an integer as decimal to stdout
;; Converts to decimal digits in a stack buffer, then write.
(defn print-int [n]
  ;; Simple: convert to digits in reverse, then write
  ;; Uses store32! on a small stack buffer
  ;; For bootstrap: delegated to C's pp_val
  nil)
