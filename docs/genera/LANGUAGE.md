# genera Language Reference

genera's dialect IS Clojure. Nothing illegal. Pure Clojure code compiles
on JVM, ClojureScript, or genera. Low-level builtins are ordinary functions
that need stubs for portability.

Two layers:
  1. **Pure Clojure** — portable, standard forms and naming
  2. **genera builtins** — low-level, stub-able for other hosts

## Reader

Standard Clojure reader. No extensions.

```
()        list
[]        vector
{}        map
'x        (quote x)
`x        syntax-quote
~x        unquote
~@x       splice-unquote
:kw       keyword
"str"     string
42        integer
3.14      float
foo       symbol
;; comment
```

## Specials

8 irreducible forms. All valid Clojure.

```
(def name value)                ;; global binding
(fn [params] body...)           ;; closure
(if test then else?)            ;; branch
(let [name val ...] body...)    ;; local bindings
(do body...)                    ;; sequence
(quote x)                       ;; literal data
(loop [name val ...] body...)   ;; recur target
(recur arg...)                  ;; tail call
```

`defmacro` is the 9th — needed for bootstrap, standard Clojure:

```
(defmacro name [params] body...)
```

Everything else is a macro or function defined in boot.clj.

## Derived Forms (boot.clj macros)

```
(defn name [p] body)    →  (def name (fn [p] body))
(when test body...)     →  (if test (do body...))
(cond t1 e1 t2 e2 ...) →  (if t1 e1 (if t2 e2 ...))
(and a b ...)           →  short-circuit if chain
(or a b ...)            →  short-circuit if chain
```

## Values

10 types. NaN-boxed u64 on genera, normal objects on JVM.

```
nil  true  false            singletons
42  -7                      integer
3.14                        float
:keyword                    keyword (interned)
symbol                      symbol (interned)
"string"                    string
(1 2 3)                     cons list
[1 2 3]                     persistent vector
{:a 1 :b 2}                persistent map
(fn [x] x)                 function
```

## Core Functions

All standard Clojure names. Implemented in boot.clj or as builtins.

### Arithmetic
```
+  -  *  /  mod
inc  dec
```

### Comparison
```
=  <  >  <=  >=
zero?  pos?  neg?
```

### Logic
```
not  and  or
```

### Sequence
```
first  rest  cons  seq  count
conj  into  nth  reverse
map  filter  reduce  range
empty?
```

### Collections
```
get  assoc  contains?
keys  vals
vec  hash-map
```

### Type
```
type  nil?  cons?
int?  fn?  symbol?  keyword?  string?
vector?  map?
```

### String / IO
```
str  pr-str  println
```

### Meta
```
apply  gensym  macroexpand-1
```

### Bit Operations
```
bit-and  bit-or  bit-xor  bit-not
bit-shift-left  bit-shift-right
```

## genera Builtins

Low-level functions. Valid Clojure identifiers. On JVM, backed by
Java arrays/objects. On genera, backed by raw memory or x86 emission.

### Naming Convention

```
name       eval-time (executes now)
emit-name  emits x86 instruction(s)
cg-name    codegen: emits x86 for compound operation
name!      mutates
name?      predicate
```

### Memory (4 primitives)

The irreducible interface to hardware. Everything else is built on these.

```
(load32 ptr off)         → u32
(store32! ptr off val)   → nil
(load64 ptr off)         → u64
(store64! ptr off val)   → nil
```

On JVM: array access. On genera eval: C wrapper (~4 lines each).
On genera JIT: dissolved into MOV instructions.

### Allocator (3 ops)

```
(bump ptr size)          → offset into arena
(mark ptr)               → current position (u32)
(restore! ptr pos)       → nil (bulk free)
```

### Grammar Introspection

Read-only access to parse tree. On JVM: backed by Java objects.

```
gn-kind  gn-child  gn-next  gn-parent  gn-sym
gn-count  gn-parse-int  gn-has-dot  gn-text
```

### Views (bitmask system)

```
view?      view-set!
val-get    val-set!
scope-get  scope-set!
bind-get   bind-set!
```

### x86 Emission

JIT-only. Raw byte emission + typed instructions.

```
;; Raw bytes
(xb! byte)              emit one byte
(x32! dword)            emit 4 bytes (little-endian)

;; Instructions (27)
x86-imm!  x86-mov!  x86-add!  x86-sub!
x86-cmp!  x86-test! x86-imul! x86-idiv!  x86-neg!
x86-push! x86-pop!  x86-push-r! x86-pop-r! x86-mov-rr!
x86-load! x86-store!
x86-load-abs!  x86-store-abs!
x86-jmp!  x86-call! x86-ret!  x86-jcc!
x86-patch! x86-jmp-to! x86-setcc!
x86-prologue! x86-epilogue!
x86-add-i8!  x86-sub-i8!
```

### x86 Pointer-Relative (defined in mem.clj)

```
;; Emit: mov dst, [base + off8]
(emit-load-at dst base off)
(emit-store-at base off src)
(emit-load32-at dst base off)
(emit-store32-at base off src)
```

### Codegen (compound operations)

Convention: `cg-` prefix for functions that emit x86 for
a Clojure construct or operation.

```
;; Allocator
cg-bump  cg-mark  cg-restore

;; Expressions (boot.clj JIT walker)
cg-expr  cg-tail  cg-if  cg-let  cg-do  cg-loop
cg-and  cg-or  cg-cond  cg-when  cg-recur
cg-binop  cg-cmp

;; Value operations (commit.clj)
cg-commit-cons  cg-commit-handle  cg-commit-str  cg-commit-fn

;; System (sys.clj)
cg-write  cg-read  cg-exit  cg-mmap-anon
```

### Compiler State

```
code-pos  code-reset!
comp-reset!  comp-alloc-slot!  comp-find-local
comp-save-locals!  comp-restore-locals!
comp-loop-start  comp-set-loop-start!
comp-in-loop?  comp-set-in-loop!
comp-loop-var!  comp-loop-count  comp-set-loop-count!
comp-loop-off  comp-loop-save!  comp-loop-restore!
comp-find-cs  comp-cs-name!  comp-cs-mode?
```

### JIT Runtime

```
jit-register-fn!  jit-find-fn
jit-register-global!  jit-find-global
jit-add-fix!  jit-patch-calls!  jit-exec!
arg-reg  cs-pool
```

## Constants

All exposed as `def`'d values (integers).

### Type IDs
```
T_NIL  T_BOOL  T_INT  T_F64  T_SYM  T_KW
T_STR  T_PMAP  T_PVEC  T_FN  T_CONS
```

### Node Kinds
```
NK_ROOT  NK_LIST  NK_VEC  NK_MAP
NK_QUOTE  NK_SYNTAX_QUOTE  NK_UNQUOTE  NK_SPLICE
NK_IDENT  NK_NUM  NK_STR  NK_OP  NK_KW
```

### View Bits
```
V_DEF  V_REF  V_CALL  V_TAIL
V_PURE  V_CONST  V_DEAD  V_INT
V_VEC  V_MAP  V_FN
```

### Registers
```
RAX  RCX  RDX  RBX  RSP  RBP  RSI  RDI
R12  R13  R14  R15
```

### Condition Codes
```
CC_E  CC_NE  CC_L  CC_GE  CC_LE  CC_G
```

## The Elegant Pattern

Maps as dispatch. One mechanism for everything.

```clojure
;; sig: handler table
(def handlers {name handler-fn ...})
((get handlers name) data)

;; JIT walker: node kind → codegen
(def codegen {NK_NUM cg-num  NK_IDENT cg-ident  NK_LIST cg-list})
((get codegen (gn-kind id)) id)

;; View pass: view bit → pass function
(def passes [scope-pass type-pass flow-pass alloc-pass])
(reduce (fn [_ p] (p n)) nil passes)
```

dispatch = map lookup + call. Same as sig, same as views,
same as the JIT walker. pmap IS the dispatch table.
pmap is implemented in Clojure (coll.clj).
The dispatch mechanism is written in the language it dispatches.

## File Structure

```
boot.clj     language: macros, derived forms, passes, JIT walker
mem.clj      allocator: bump, restore, mark, pointer-relative x86
alloc.clj    analysis: allocation passes (V_ALLOC, V_SCOPE, V_DYNAMIC)
commit.clj   lifetime: value promotion step → main
epoch.clj    reclamation: V_LIVE coll copy
coll.clj     data structures: HAMT pmap, pvec, atom
sig.clj      dispatch: facts about names
step.clj     step loop: mark/eval/commit/restore/epoch
sys.clj      system: read, write, mmap, exit (raw syscall)
```
