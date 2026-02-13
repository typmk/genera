# Genera

A native Lisp runtime in Rust, inspired by [Symbolics Genera](https://en.wikipedia.org/wiki/Genera_(operating_system)) and targeting SBCL-level parity.

> *"Genera" - named after the legendary Lisp Machine operating system from Symbolics, representing the pinnacle of Lisp-based computing.*

## SBCL C Runtime Comparison

Goal: 100% parity with SBCL's C runtime layer.

| SBCL C File      | LOC   | Rust Equivalent       | LOC  | Status |
|------------------|-------|-----------------------|------|--------|
| gencgc.c         | 5,800 | gc/collector.rs + heap.rs | 1,873 | ✅ Done |
| alloc.c          | 1,200 | gc/alloc.rs           | 810  | ✅ Done |
| arena.c          | 400   | gc/arena.rs           | 108  | ✅ Done |
| safepoint.c      | 800   | gc/safepoint.rs       | 456  | ✅ Done |
| gc-common.c      | 600   | gc/mod.rs + weak.rs   | 880  | ✅ Done |
| thread.c         | 1,500 | runtime/thread.rs     | ~465 | ✅ Done |
| interrupt.c      | 1,800 | runtime/interrupt.rs  | ~353 | ✅ Done |
| funcall.c        | 300   | runtime/funcall.rs    | ~336 | ✅ Done |
| print.c          | 1,200 | runtime/print.rs      | ~577 | ✅ Done |
| math.c           | 800   | runtime/math.rs       | ~446 | ✅ Done |
| dynbind.c        | 400   | types/var.rs          | 256  | ✅ Done |
| globals.c        | 300   | types/namespace.rs    | 240  | ✅ Done |
| breakpoint.c     | 500   | runtime/breakpoint.rs | ~373 | ✅ Done |
| main.c           | 600   | main.rs               | 46   | ⚠️ Minimal |
| runtime.c        | 600   | lib.rs                | 59   | ⚠️ Minimal |

**Total SBCL C: ~16,800 LOC**
**Current Rust: ~10,000 LOC**
**Target Rust: ~14,000 LOC**

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       Lisp Code                             │
│  (reader, compiler, core library - written in Lisp)         │
├─────────────────────────────────────────────────────────────┤
│                      Rust Runtime                           │
│  ┌─────────────────────────────────────────────────────────┐│
│  │  runtime/                                               ││
│  │  ├── thread.rs      Thread state, TLS, dynamic bindings││
│  │  ├── interrupt.rs   Signals, Ctrl-C, async interrupts  ││
│  │  ├── funcall.rs     Function call dispatch             ││
│  │  ├── print.rs       Low-level value printing           ││
│  │  ├── breakpoint.rs  Debugger support                   ││
│  │  └── math.rs        Overflow-safe arithmetic           ││
│  └─────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────┐│
│  │  gc/                                                    ││
│  │  ├── heap.rs        Page tables, card marks, regions   ││
│  │  ├── collector.rs   Copying collector, generations     ││
│  │  ├── alloc.rs       Bump allocation, region types      ││
│  │  ├── safepoint.rs   GC safepoints, stop-the-world      ││
│  │  ├── weak.rs        Weak refs, finalizers              ││
│  │  └── arena.rs       Manual memory regions              ││
│  └─────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────┐│
│  │  types/                                                 ││
│  │  ├── symbol.rs      Interned symbols                   ││
│  │  ├── keyword.rs     Interned keywords                  ││
│  │  ├── cons.rs        Cons cells, lists                  ││
│  │  ├── vector.rs      Persistent vectors (HAMT)          ││
│  │  ├── map.rs         Persistent hash maps (HAMT)        ││
│  │  ├── set.rs         Persistent hash sets               ││
│  │  ├── var.rs         Vars, dynamic bindings             ││
│  │  ├── atom.rs        Atoms (atomic refs)                ││
│  │  ├── agent.rs       Agents (async state)               ││
│  │  ├── namespace.rs   Namespace registry                 ││
│  │  └── ...            BigInt, Ratio, Regex, etc.         ││
│  └─────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────┐│
│  │  value.rs           Tagged pointer Value type          ││
│  │  error.rs           Error types                        ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

## Design Principles

1. **SBCL-style split**: Minimal Rust (~15%), maximal Lisp (~85%)
2. **Lisp-agnostic**: Runtime works for any Lisp dialect
3. **GC-first**: All operations are GC-safe with proper safepoints
4. **Native performance**: Tagged pointers, inline caching, native codegen

## Value Representation

Tagged pointers with 3-bit tags (like SBCL lowtags):

| Tag | Binary | Type            |
|-----|--------|-----------------|
| 0   | 000    | Fixnum (61-bit) |
| 1   | 001    | Cons/List       |
| 2   | 010    | Vector          |
| 3   | 011    | Map             |
| 4   | 100    | Symbol          |
| 5   | 101    | Keyword         |
| 6   | 110    | Function        |
| 7   | 111    | Other (widetag) |

## Garbage Collection

SBCL-compatible generational GC (gencgc):

- 7 generations (0 = nursery, 6 = pseudo-static)
- Card marking for remembered sets
- Copying collector with Cheney's algorithm
- Safepoints for stop-the-world
- Weak references and finalizers

## Building

```bash
cd src/rs/genera
cargo build --release
cargo test
```

With JIT support:
```bash
cargo build --release --features jit
```

Binary name: `genera`

## Status

- [x] Value representation (tagged pointers)
- [x] GC (generational, card marking)
- [x] Types (Symbol, Keyword, Cons, Vector, Map, Set, etc.)
- [x] Dynamic variables (Var, bindings)
- [x] Atoms, Agents
- [x] Threading (thread.rs)
- [x] Interrupts (interrupt.rs)
- [x] Function calls (funcall.rs)
- [x] Math operations (math.rs)
- [x] Value printing (print.rs)
- [x] Debugger (breakpoint.rs)
- [ ] Native codegen (JIT)

## SBCL-Style Split: Rust vs Lisp

Following SBCL's architecture, Genera separates concerns between native code (Rust)
and high-level code (Lisp). The split mirrors SBCL's C/Lisp boundary.

### What's in Rust (like SBCL's C runtime)

Only memory primitives and OS interfaces:

| Category | Rust | SBCL C Equivalent |
|----------|------|-------------------|
| **Memory** | GC, allocation, tagged pointers | gencgc.c, alloc.c |
| **Primitives** | cons, simple-vector, symbol, fixnum | objdef.lisp layouts |
| **Threading** | OS threads, TLS, safepoints | thread.c |
| **Signals** | Ctrl-C, SIGSEGV | interrupt.c |
| **Codegen** | Cranelift JIT | VOP compilation |

**Mutable collections**: Only `MutableVector` (simple-vector) and `Cons` with
rplaca/rplacd. Hash tables and sets are built in Lisp on top of vectors.

**Persistent collections**: `PersistentVector`, `PersistentMap`, `PersistentSet`
(HAMT-based) for Clojure-style immutability. These are complex data structures
that benefit from Rust's rpds library.

### What's in Lisp (like SBCL's Lisp layer)

Everything else:

| Category | Built in Lisp | Notes |
|----------|---------------|-------|
| **Reader** | S-expression parsing | Uses runtime string/symbol primitives |
| **Compiler** | HIR → native code | Emits calls to runtime functions |
| **Core library** | map, filter, reduce, etc. | Operates on runtime primitives |
| **Hash tables** | Mutable hash-table | Built on MutableVector |
| **Printer** | pr-str, print dispatch | Uses runtime print primitives |
| **REPL** | Read-eval-print loop | Orchestrates reader/compiler/printer |
| **Debugger** | SLIME/nREPL protocols | Uses runtime breakpoint support |

### The Boundary

Lisp code manipulates opaque `Value` handles through runtime functions:

```
Lisp: (vector-ref v 0)     →  Rust: vector_nth(v, 0) -> Value
Lisp: (hash-ref m k)       →  Rust: map_get(m, k) -> Value
Lisp: (make-hash-table)    →  Lisp: (built on MutableVector)
```

The compiler emits calls to Rust runtime functions. Lisp never sees Rust internals.
