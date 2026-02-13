# Genera Architecture

A native Lisp runtime combining the best of SBCL, BEAM, Genera, and Clojure.

## Vision

> **Actors own *control flow*; the shared heap owns *values*.**
> Mutation is only through explicit cells (STM/CAS), never through normal data.

This gives us:
- **BEAM's** reliability & structure (actors, supervision, fault tolerance)
- **Genera's** inspectable semantic world (live image, deep introspection)
- **SBCL's** performance (gencgc, native compilation)
- **Clojure's** simplicity (persistent data structures, STM)

## What Doesn't Exist Today

| Feature | GHC | Pharo | BEAM | SBCL | **Genera** |
|---------|-----|-------|------|------|------------|
| Live image | - | Yes | - | Partial | Yes |
| STM | Yes | - | - | - | Yes |
| Actors/supervision | - | - | Yes | - | Yes |
| Persistent DS | Yes | - | - | - | Yes |
| gencgc-class GC | Partial | - | Partial | Yes | Yes |
| Deep introspection | - | Yes | Partial | Partial | Yes |
| Multi-dialect | - | - | - | - | Yes |

## Memory Model

```
+-------------------------------------------------------------+
|                    Shared Immutable Heap                    |
|         (persistent DS, symbols, code, interned)            |
|                      gencgc managed                         |
|                   globally inspectable                      |
+-------------------------------------------------------------+
|  Actor 1      |  Actor 2      |  Actor 3      |  ...        |
|  +---------+  |  +---------+  |  +---------+  |             |
|  | Nursery |  |  | Nursery |  |  | Nursery |  |             |
|  | (local) |  |  | (local) |  |  | (local) |  |             |
|  +---------+  |  +---------+  |  +---------+  |             |
|  Mailbox      |  Mailbox      |  Mailbox      |             |
|  Stack        |  Stack        |  Stack        |             |
+-------------------------------------------------------------+
|                    Binary Arena                             |
|            (large buffers, shared, refcounted)              |
+-------------------------------------------------------------+
```

### Shared Immutable Heap (Genera-style)

- Persistent data structure nodes
- Symbols, keywords, interned strings
- Code objects (bytecode chunks, compiled functions)
- Managed by gencgc (generational, card-marking)
- **Globally reachable and inspectable**

Enables:
- "Who references this?" queries
- Global heap exploration
- Image save/restore
- Sharing without copying

### Per-Actor Nursery (BEAM-style)

Each actor allocates into a small private young space:
- Extremely fast bump allocation
- Frequent minor collections per actor
- Objects that "escape" get promoted to shared heap

Enables:
- Low-latency GC for ephemeral work
- Isolation of allocation storms
- BEAM-like "most garbage dies young locally"

### Binary Arena

Large byte buffers live in a separate arena:
- Reference-counted or specially traced
- Shared across actors without copying
- Avoids pinning the main heap

## Message Passing

Because values are immutable, message passing doesn't require copying:

```
Actor A                    Actor B
   |                          |
   |  (send B {:foo 42})      |
   |------------------------->|
   |     (reference to        |
   |      shared heap)        |
```

**Escape on send**: If an object is in actor A's nursery, sending it
triggers escape/promotion to the shared heap.

## Compilation Pipeline

```
Source (Clojure/CL/Scheme)
         |
         v
+-------------------+
|      Reader       |  (Lisp code)
+-------------------+
         |
         v
+-------------------+
|       HIR         |  High-level IR, Lisp semantics
|  {:op :fn ...}    |  Types known, purity info
+-------------------+
         |
         v
+-------------------+
|       MIR         |  Lowered: locals, jumps, SSA
+-------------------+
         |
    +----+----+
    |         |
    v         v
+--------+ +----------+
|Bytecode| |Cranelift |  (Stage 2-3)
+--------+ +----------+
    |           |
    v           v
+--------+ +----------+
|   VM   | |  Native  |
+--------+ +----------+
```

### Stage 1: VM (Bytecode Interpreter)

- ~70 opcodes, stack-based
- Bootstrap, mobile/WASM, debugging
- Full observability
- ~10-50x slower than native

### Stage 2: JIT (MIR -> Cranelift)

- MIR directly to Cranelift IR
- Type info preserved for optimization
- ~1-2x slower than hand-tuned

### Stage 3: Native (MIR -> VOPs)

- Hand-tuned code generation
- SBCL-style VOPs for hot paths
- C-level performance

## Opcode Comparison

| System | Opcodes | Notes |
|--------|---------|-------|
| JVM | ~202 | Portable bytecode |
| CLR | ~220 | 1-2 byte opcodes |
| BEAM (Erlang) | ~170 | Functional VM |
| Ivory (Genera) | ~190 | Lisp machine hardware |
| SBCL | ~500 VOPs/arch | Direct to native |
| Cranelift | ~188 | Compiler IR |
| **Genera (ours)** | ~70 | Minimal bytecode |

Our 70 opcodes are higher-level; most map to a few Cranelift ops or
runtime calls. JVM/CLR have type-specialized variants we avoid.

## Actor Runtime (BEAM-inspired)

From BEAM's `erl_process.h`:

```c
struct process {
    Eterm *htop;           // Heap top
    Eterm *stop;           // Stack top
    Sint32 fcalls;         // Reductions left (preemption budget)
    Uint32 flags;          // Process flags
    Uint reds;             // Total reductions executed
    Process *next;         // Next in run queue
    // ...
    Eterm *heap;           // Heap start
    Eterm *hend;           // Heap end
    Eterm *high_water;     // GC high water mark
    Eterm *old_heap;       // Old generation heap
};
```

### Our Actor Struct (Rust)

```rust
pub struct Actor {
    // Scheduling
    pub reductions: i32,        // Budget for preemption
    pub priority: Priority,     // max/high/normal/low
    pub status: ActorStatus,    // running/waiting/suspended

    // Memory (per-actor nursery)
    pub heap: *mut Value,       // Nursery start
    pub htop: *mut Value,       // Nursery allocation pointer
    pub hend: *mut Value,       // Nursery end
    pub stack: Vec<Value>,      // Evaluation stack

    // Messaging
    pub mailbox: Mailbox,       // Message queue
    pub links: Vec<ActorId>,    // Linked actors
    pub monitors: Vec<Monitor>, // Monitors

    // Execution
    pub pc: usize,              // Program counter
    pub frame: Frame,           // Current call frame
    pub catch_stack: Vec<CatchInfo>,
}
```

### Scheduler

- Multiple run queues (one per OS thread)
- Work stealing between queues
- Preemption by reduction counting (not time slices)
- Priority levels: max, high, normal, low

### Supervision (Clojure library)

```clojure
(defsupervisor my-sup
  :strategy :one-for-one
  :children [{:id :worker
              :start (fn [] (spawn worker-fn))
              :restart :permanent}])
```

Strategies:
- `:one-for-one` - restart only failed child
- `:one-for-all` - restart all if one fails
- `:rest-for-one` - restart failed and later children

## STM Integration

STM coordinates shared mutable state (Refs):

```clojure
(def account (ref 100))

(dosync
  (alter account + 50))
```

### Where STM Lives

- `Ref`, `Atom`, `Var` are the only mutable cells
- Everything else is persistent/immutable
- Actors can use pure message passing OR STM
- Cross-actor transactions are explicit

## Live Image

### Save

```
+-------------------------------------------+
|              Image File (.vlod)           |
+-------------------------------------------+
|  Header (version, flags)                  |
|  Load Map (address ranges, opcodes)       |
|  Heap (tagged Values, objects, symbols)   |
|  Bytecode (all compiled functions)        |
|  Actor State (optional snapshots)         |
|  NO native code                           |
+-------------------------------------------+
```

### Load

1. Deserialize heap + bytecode
2. Start in VM mode (instant)
3. Background JIT hot functions

Native code is not saved - regenerated on load via JIT.

## Component Map

```
+-------------------------------------------------------------+
|                    Clojure (on Genera)                      |
+-------------------------------------------------------------+
|  clojure.core          | Standard library                   |
|  clojure.actors        | spawn, send, receive, link         |
|  clojure.supervisor    | Supervision trees                  |
|  clojure.stm           | ref, dosync, alter                 |
+-------------------------------------------------------------+
|                    Compiler (Clojure)                       |
|  reader.clj            | S-expr parsing                     |
|  analyzer.clj          | Source -> HIR                      |
|  lowering.clj          | HIR -> MIR                         |
|  bytecode.clj          | MIR -> Bytecode                    |
|  cranelift.clj         | MIR -> Cranelift (native)          |
+-------------------------------------------------------------+
|                    Genera Runtime (Rust)                    |
|  gc/                   | gencgc + per-actor nurseries       |
|  types/                | PersistentVector, Map, Set, etc.   |
|  vm/                   | Bytecode interpreter               |
|  actors/               | Scheduler, mailboxes, reductions   |
|  stm/                  | Transaction manager                |
|  jit/                  | Cranelift backend                  |
+-------------------------------------------------------------+
```

## Build Order

1. [x] Genera runtime basics (GC, types, VM)
2. [ ] Clojure compiler (bootstrap from JVM Clojure)
3. [ ] clojure.core (standard library)
4. [ ] Actor runtime (Rust scheduler + Clojure library)
5. [ ] Supervision (Clojure library)
6. [ ] JIT (MIR -> Cranelift)
7. [ ] Native VOPs (hot paths)

## References

### BEAM (Erlang/OTP)
- `erts/emulator/beam/erl_process.h` - Process struct, scheduling
- `erts/emulator/beam/erl_gc.h` - Per-process GC
- `erts/emulator/beam/erl_message.h` - Mailboxes

### SBCL
- `src/compiler/x86-64/*.lisp` - ~500 VOPs
- `src/runtime/gencgc.c` - Generational GC
- `src/compiler/ir1*.lisp` - HIR
- `src/compiler/ir2*.lisp` - MIR

### Symbolics Genera
- VLM source: github.com/hanshuebner/vlm
- Ivory ISA: ~190 opcodes
- World files: `.vlod` format

### Clojure Libraries
- **otplike**: github.com/suprematic/otplike
  - OTP-style actors for Clojure on core.async
  - Proves actor/supervision can be a library layer
  - API reference: `spawn`, `!`, `receive`, `link`, `monitor`
  - gen_server-style state serialization
  - We can port this API, replacing core.async with native actors

### Related Research
- "Multicore GC with Local Heaps" (Marlow et al.)
- "Garbage Collection for Multicore NUMA Machines" (Manticore)
- "Places: Adding Message-Passing Parallelism to Racket"
