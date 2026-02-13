# Clojure Kernel Contract

The kernel is the minimal set of types that form the foundation of Clojure.
**Everything**, including the kernel types themselves, is pure Clojure compiled
to the target platform. The only platform-specific code is the **emitter**.

## Philosophy

Following SBCL's architecture, but taken further:
- **SBCL**: ~15% C (because it was easier), ~85% Lisp
- **ClojurePHP**: ~0% native code, 100% Clojure compiled to target

The key insight: For transpilation targets (PHP, JS), the host already provides
GC, closures, strings, arrays, and numbers. We don't need native code - we just
need an emitter that outputs the target language's syntax.

```
┌─────────────────────────────────────────┐
│ Compiler (analyzer, emitter)            │  ← Clojure
├─────────────────────────────────────────┤
│ Kernel (Cons, Sym, Kw, Atom)            │  ← Clojure (deftype)
├─────────────────────────────────────────┤
│ clojure.lang.* / clojure.core           │  ← Clojure
├─────────────────────────────────────────┤
│ User code                               │  ← Clojure
└─────────────────────────────────────────┘
         ↓ all compiles to
┌─────────────────────────────────────────┐
│ PHP / JS / Rust / WASM                  │
└─────────────────────────────────────────┘
```

**Adding a platform = writing an emitter in Clojure.**

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        User Code (.cljc)                        │
├─────────────────────────────────────────────────────────────────┤
│                     clojure/core.cljc                           │
│   map, filter, reduce, defprotocol, deftype, lazy-seq, etc.     │
├─────────────────────────────────────────────────────────────────┤
│                   clojure/lang/*.cljc                           │
│     PersistentVector, PersistentHashMap, LazySeq, etc.          │
├─────────────────────────────────────────────────────────────────┤
│                   Platform Abstractions                         │
│        system.cljc, io.cljc, shell.cljc, process.cljc           │
│            (protocols with platform mappings)                   │
├─────────────────────────────────────────────────────────────────┤
│                     Kernel (deftype)                            │
│              Cons, Sym, Kw, Atom - pure Clojure                 │
├─────────────────────────────────────────────────────────────────┤
│                         Compiler                                │
│                    HIR → Emitter (per target)                   │
├──────────┬──────────┬──────────┬──────────┬─────────────────────┤
│   PHP    │    JS    │   Rust   │   JVM    │   (other targets)   │
│ emitter  │ emitter  │ emitter  │ emitter  │                     │
│ (Clojure)│ (Clojure)│ (Clojure)│ (Clojure)│      (Clojure)      │
├──────────┴──────────┴──────────┴──────────┴─────────────────────┤
│                     Host Platform                               │
│          GC, closures, strings, arrays, numbers                 │
└─────────────────────────────────────────────────────────────────┘
```

## Kernel Types

The kernel consists of 4 irreducible types. These are written in Clojure using
`deftype` and compiled to each target platform:

```clojure
;; clojure/lang/kernel.cljc
(deftype Cons [head tail])
(deftype Sym [ns name])
(deftype Kw [ns name])
(deftype Atom [^:mutable val])
```

### Why These 4 Types?

These types are irreducible - they cannot be built from each other or host primitives:

| Type | Why Primitive? |
|------|----------------|
| **Cons** | Lisp's fundamental pair. Lists are Cons chains. Code-as-data requires it. Can't build from host arrays (need immutable semantics + nil termination). |
| **Sym** | Identity/names for code, macros, namespaces. Not just strings - interned, have namespace/name structure. |
| **Kw** | Self-evaluating names. Clojure idiom uses everywhere (:keys, options, map keys). Like Sym but different semantics. |
| **Atom** | The ONE mutable primitive. Everything else immutable. Need exactly one escape hatch for state. |

### What's NOT Primitive?

| Type | Built From... |
|------|---------------|
| PersistentVector | arrays + protocols (HAMT) |
| PersistentHashMap | arrays + protocols (HAMT) |
| LazySeq | Cons + closures |
| Numbers | host provides |
| Strings | host provides |
| Functions | host provides |
| Arrays | host provides |

## Compiled Output

The same Clojure `deftype` compiles to different target syntax:

### PHP
```php
class Cons { public $head; public $tail; }
class Sym  { public $ns; public $name; }
class Kw   { public $ns; public $name; }
class Atom { public $val; }
```

### JavaScript
```javascript
class Cons { constructor(head, tail) { this.head = head; this.tail = tail; } }
class Sym  { constructor(ns, name) { this.ns = ns; this.name = name; } }
class Kw   { constructor(ns, name) { this.ns = ns; this.name = name; } }
class Atom { constructor(val) { this.val = val; } }
```

### Rust
```rust
struct Cons { head: Value, tail: Value }
struct Sym  { ns: Option<String>, name: String }
struct Kw   { ns: Option<String>, name: String }
struct Atom { val: AtomicCell<Value> }
```

**The source is the same. Only the emitter differs.**

## Field Access Convention

Clojure code accesses kernel fields via:
```clojure
(.-head cons-cell)   ; read head
(.-tail cons-cell)   ; read tail
(.-name sym)         ; read name
```

The compiler emits appropriate field access for each target:
- PHP: `$obj->head`
- JS: `obj.head`
- Rust: `obj.head`

## What's NOT in the Kernel

Everything else is pure Clojure in `core.cljc`:

- `cons`, `first`, `rest`, `seq` (functions using kernel types)
- `vector`, `hash-map`, `hash-set` (persistent data structures - HAMT)
- `map`, `filter`, `reduce` (sequence operations)
- `defprotocol`, `deftype`, `defrecord` (extensibility)
- `+`, `-`, `*`, `/` (arithmetic - uses platform primitives)
- `lazy-seq`, `delay`, `force` (laziness)
- `atom`, `swap!`, `reset!` (state - wraps kernel Atom)

## Compilation Flow

```
core.cljc (pure Clojure)
       ↓
   compiler
       ↓
┌──────┴──────┐
│   emitter   │
└──────┬──────┘
       ↓
target output + kernel
```

Example for PHP:
```
output/
  clojure/
    Cons.php    ← kernel
    Sym.php     ← kernel
    Kw.php      ← kernel
    Atom.php    ← kernel
    core.php    ← compiled from core.cljc
  your_app.php  ← compiled from your code
```

## Implementation Status

The core.cljc rewrite is **COMPLETE**. All `clojure.lang.*` types have been
reimplemented in pure Clojure using the kernel types.

### Implemented Files

| File | Purpose | Status |
|------|---------|--------|
| `clojure/lang/protocols.cljc` | 35+ protocols (ISeq, IFn, IDeref, etc.) | ✅ Complete |
| `clojure/lang/kernel.cljc` | Cons, Sym, Kw, Atom, EmptyList | ✅ Complete |
| `clojure/lang/core.cljc` | seq, first, rest, cons, conj, reduce, etc. | ✅ Complete |
| `clojure/lang/numbers.cljc` | +, -, *, /, inc, dec, bit ops | ✅ Complete |
| `clojure/lang/lazy.cljc` | LazySeq, Delay, Volatile, Range, Repeat, Iterate, Cycle | ✅ Complete |
| `clojure/lang/persistent_vector.cljc` | HAMT-based PersistentVector | ✅ Complete |
| `clojure/lang/persistent_map.cljc` | HAMT-based PersistentHashMap | ✅ Complete |
| `clojure/lang/persistent_set.cljc` | PersistentHashSet (backed by map) | ✅ Complete |
| `clojure/lang/transient.cljc` | Transient collections | ✅ Complete |
| `clojure/lang/var.cljc` | Var, dynamic binding | ✅ Complete |
| `clojure/lang/namespace.cljc` | Namespace management | ✅ Complete |
| `clojure/lang/multifn.cljc` | MultiFn, hierarchy, derive, isa? | ✅ Complete |
| `clojure/lang/ref.cljc` | Ref, STM transactions | ✅ Complete |
| `clojure/lang/agent.cljc` | Agent, send, await | ✅ Complete |
| `clojure/lang/seqs.cljc` | map, filter, take, drop, partition, etc. | ✅ Complete |
| `clojure/lang/string.cljc` | String operations | ✅ Complete |
| `clojure/lang/ex.cljc` | ExceptionInfo, ex-info, ex-data | ✅ Complete |
| `clojure/lang.cljc` | Unified exports namespace | ✅ Complete |

### Type Mappings

| clojure.lang.* (JVM) | Implementation |
|----------------------|----------------|
| `Cons` | `clojure.lang.kernel/Cons` |
| `Symbol` | `clojure.lang.kernel/Sym` |
| `Keyword` | `clojure.lang.kernel/Kw` |
| `Atom` | `clojure.lang.kernel/Atom` |
| `PersistentList` | `clojure.lang.kernel/EmptyList` + `Cons` |
| `PersistentVector` | `clojure.lang.persistent-vector/PersistentVector` |
| `PersistentHashMap` | `clojure.lang.persistent-map/PersistentHashMap` |
| `PersistentHashSet` | `clojure.lang.persistent-set/PersistentHashSet` |
| `LazySeq` | `clojure.lang.lazy/LazySeq` |
| `Delay` | `clojure.lang.lazy/Delay` |
| `Volatile` | `clojure.lang.lazy/Volatile` |
| `Range` | `clojure.lang.lazy/Range` |
| `Var` | `clojure.lang.var/Var` |
| `Ref` | `clojure.lang.ref/Ref` |
| `Agent` | `clojure.lang.agent/Agent` |
| `Namespace` | `clojure.lang.namespace/Namespace` |
| `MultiFn` | `clojure.lang.multifn/MultiFn` |
| `ExceptionInfo` | `clojure.lang.ex/ExceptionInfo` |

### Protocol Mappings

| clojure.lang.I* (JVM) | Protocol |
|-----------------------|----------|
| `ISeq` | `clojure.lang.protocols/ISeq` |
| `ISeqable` | `clojure.lang.protocols/ISeqable` |
| `ICounted` | `clojure.lang.protocols/ICounted` |
| `IIndexed` | `clojure.lang.protocols/IIndexed` |
| `ILookup` | `clojure.lang.protocols/ILookup` |
| `IAssociative` | `clojure.lang.protocols/IAssociative` |
| `ICollection` | `clojure.lang.protocols/ICollection` |
| `IStack` | `clojure.lang.protocols/IStack` |
| `IFn` | `clojure.lang.protocols/IFn` |
| `IDeref` | `clojure.lang.protocols/IDeref` |
| `IRef` | `clojure.lang.protocols/IRef` |
| `IMeta` | `clojure.lang.protocols/IMeta` |
| `IWithMeta` | `clojure.lang.protocols/IWithMeta` |
| ... | (35+ protocols total) |

## Original Rewrite Plan (Completed)

### Phase 1: Bootstrap ✅
- `cons`, `first`, `rest`, `seq`, `conj` - Done in `core.cljc`

### Phase 2: Arithmetic ✅
- `+`, `-`, `*`, `/`, `inc`, `dec`, bit operations - Done in `numbers.cljc`

### Phase 3: Collections ✅
- PersistentVector (HAMT) - Done in `persistent_vector.cljc`
- PersistentHashMap (HAMT) - Done in `persistent_map.cljc`
- PersistentHashSet - Done in `persistent_set.cljc`
- Transients - Done in `transient.cljc`

### Phase 4: Protocols & Types ✅
- 35+ protocols defined in `protocols.cljc`
- All types implement appropriate protocols

### Phase 5: State & Concurrency ✅
- Atom - Done in `kernel.cljc`
- Var - Done in `var.cljc`
- Ref/STM - Done in `ref.cljc`
- Agent - Done in `agent.cljc`

### Phase 6: Platform Abstractions ✅
- `clojure/system.cljc` - ISystem (time, env, exit)
- `clojure/io.cljc` - IO protocols
- `clojure/shell.cljc` - Shell execution
- `clojure/process.cljc` - Process management

## Adding a New Platform

**Write an emitter. That's it.**

```
TODAY (traditional approach):
─────────────────────────────
1. Port 4 kernel types (~50 LOC native)
2. Write emitter (~500 LOC Clojure)
3. Platform abstractions (~200 LOC)

Time: 1-3 months

CORRECT APPROACH:
─────────────────
1. Write emitter (~500 LOC Clojure)
2. Add platform mappings (~100 LOC)

Time: 1-2 weeks

Everything else compiles from .cljc automatically.
```

The emitter's job:
1. Output target language syntax for each HIR node type
2. Handle platform-specific idioms (PHP `$` prefix, JS `this`, etc.)
3. Map abstract operations to platform calls via platform mappings

## Platform Mappings

Each platform has a `.cljc` file mapping abstract operations to platform calls:

```clojure
;; platforms/php.cljc
(ns platforms.php)

(def mappings
  {:string/upper-case  'strtoupper
   :string/lower-case  'strtolower
   :io/read-file       'file_get_contents
   :io/write-file      'file_put_contents
   :type/string?       'is_string
   :type/number?       'is_numeric})
```

The emitter loads its platform mappings at require time. Same code, different targets:

```bash
cljpc --target php app.cljc   # → app.php
cljpc --target js  app.cljc   # → app.js
cljpc --target dart app.cljc  # → app.dart
```

## Targets

| Target | Emitter | Platform Mappings | Output |
|--------|---------|-------------------|--------|
| PHP | `clojure.php.emit` | `platforms/php.cljc` | `.php` |
| JavaScript | `clojure.js.emit` | `platforms/js.cljc` | `.js` |
| Rust/Genera | `clojure.rust.emit` | `platforms/rust.cljc` | bytecode |
| JVM | pass-through | N/A | `.class` |
| WASM | `clojure.wasm.emit` | `platforms/wasm.cljc` | `.wasm` |

## Bootstrap Complexity by Platform Type

| Platform Type | What Host Provides | Bootstrap Size |
|---------------|-------------------|----------------|
| **VM targets** (PHP, JS, Dart) | GC, closures, strings, arrays, numbers | Minimal (~500 LOC emitter) |
| **Native targets** (Rust, WASM) | CPU, memory | Larger (~2000 LOC - must implement GC, closures) |

For transpilation targets, the host does the heavy lifting. We just output syntax.

## References

- SBCL: ~15% C + ~85% Lisp (C chosen for ease, not necessity)
- ClojureDart: Only 2 Java files for bootstrap, rest is Clojure
- Our approach: 0% native, 100% Clojure compiled to target
