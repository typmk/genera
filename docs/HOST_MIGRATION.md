# Host Architecture Migration Guide

## Overview

The new `clojure.host` architecture consolidates platform primitives into ONE protocol,
replacing scattered `*engine*` vars with a unified approach.

```
┌────────────────────────────────────────────────────────┐
│  clojure.time  clojure.io  clojure.regex  ...          │  User API
├────────────────────────────────────────────────────────┤
│                    clojure.host                        │  ONE protocol
├──────────────────┬──────────────────┬──────────────────┤
│  clojure.jvm.*   │  clojure.php.*   │  clojure.js.*    │  Backends
└──────────────────┴──────────────────┴──────────────────┘
```

## File Migration Map

### Abstractions (keep, simplify)

| Current | Change | Notes |
|---------|--------|-------|
| `clojure.time` | Remove `TimeEngine`, use `*host*` | Keep API functions |
| `clojure.io` | Remove `IOEngine`, use `*host*` | Keep API functions |
| `clojure.regex` | Remove `RegexEngine`, use `*host*` | Keep API functions |
| `clojure.uuid` | Remove `UUIDEngine`, use `*host*` | Keep API functions |
| `clojure.uri` | Remove `URIEngine`, use `*host*` | Keep API functions |
| `clojure.math` | Remove `MathEngine`, use `*host*` | Keep API functions |
| `clojure.concurrent` | Remove `ConcurrentEngine`, use `*host*` | Keep API functions |
| `clojure.process` | Remove `ProcessEngine`, use `*host*` | Keep API functions |

### Platform Backends (consolidate)

| Current | New |
|---------|-----|
| `clojure.jvm.time` | `clojure.host.jvm` |
| `clojure.jvm.io` | `clojure.host.jvm` |
| `clojure.jvm.uuid` | `clojure.host.jvm` |
| `clojure.jvm.regex` | `clojure.host.jvm` |
| `clojure.jvm.math` | `clojure.host.jvm` |
| `clojure.jvm.concurrent` | `clojure.host.jvm` |
| `clojure.php.host.time` | `clojure.php.host` |
| etc. | etc. |

### Lang Files (restructure)

| Current | New | Notes |
|---------|-----|-------|
| `clojure.lang` | **DELETE** | Users use `clojure.core` |
| `clojure.lang.protocols` | `clojure.protocols` | Slim down to ~25 core |
| `clojure.lang.kernel` | `clojure.types.kernel` | Cons, Sym, Kw |
| `clojure.lang.RT` | Split | Host primitives → `host`, logic → `core` |
| `clojure.lang.vector` | `clojure.types.vector` | Uses `clojure.host` for arrays |
| `clojure.lang.hashmap` | `clojure.types.hashmap` | Uses `clojure.host` for arrays |
| `clojure.lang.hashset` | `clojure.types.hashset` | Uses `clojure.host` for arrays |
| `clojure.lang.lazy` | `clojure.types.lazy` | LazySeq, Delay, Volatile |
| `clojure.lang.transient` | `clojure.types.transient` | Transient collections |
| `clojure.lang.var` | `clojure.types.var` | |
| `clojure.lang.ref` | `clojure.types.ref` | |
| `clojure.lang.agent` | `clojure.types.agent` | Uses `*host*` for concurrency |
| `clojure.lang.namespace` | `clojure.types.namespace` | |
| `clojure.lang.multifn` | `clojure.types.multifn` | |
| `clojure.lang.ex` | `clojure.types.ex` | |
| `clojure.lang.seqs` | `clojure.core` | Seq functions in core |
| `clojure.lang.string` | `clojure.string` | Already exists |
| `clojure.lang.numbers` | `clojure.host` + `clojure.core` | |
| `clojure.lang.array` | `clojure.host` | Array ops in host |

## Before/After Examples

### Before: clojure.time

```clojure
(ns clojure.time)

(defprotocol TimeEngine
  (-now [e])
  (-instant-from-epoch-ms [e ms])
  ...)

(def ^:dynamic *time-engine* nil)

(defn now []
  (-now *time-engine*))
```

### After: clojure.time

```clojure
(ns clojure.time
  (:require [clojure.host :as h]))

(defn now []
  (h/-time-now (h/host)))

(defn instant [epoch-ms]
  (h/-time-from-ms (h/host) epoch-ms))
```

### Before: clojure.lang.vector (uses platform arrays)

```clojure
(ns clojure.lang.vector
  (:require [clojure.lang.array :as arr]))

(deftype PersistentVector [arr cnt ...]
  IIndexed
  (-nth [_ i]
    (arr/aget arr i)))
```

### After: clojure.types.vector

```clojure
(ns clojure.types.vector
  (:require [clojure.host :as h]
            [clojure.protocols :refer [IIndexed -nth]]))

(deftype PersistentVector [arr cnt ...]
  IIndexed
  (-nth [_ i]
    (h/aget arr i)))  ; Uses host for array access
```

## New Directory Structure

```
src/clj/clojure/
├── host.cljc              # The ONE platform protocol (public)
│
├── php/                   # PHP backend
│   ├── host.cljc          # Host implementation (internal)
│   ├── emit.cljc          # Compiler
│   ├── main.cljc          # CLI
│   └── ...
│
├── jvm/                   # JVM backend
│   ├── host.cljc          # Host implementation (future)
│   └── ...
│
├── protocols.cljc         # Core ~25 protocols (ISeq, IMap, etc.)
│
├── types/                 # Pure Clojure data types
│   ├── kernel.cljc, vector.cljc, hashmap.cljc, ...
│
├── core.cljc              # User-facing API
│
└── [abstractions]
    ├── time.cljc, io.cljc, regex.cljc, ...
```

## Adding a New Platform

1. Create `clojure.<platform>/host.cljc`
2. Implement the `Host` protocol with native calls
3. Call `(clojure.host/set-host! my-host)` at startup
4. Done!

```clojure
;; Example: Adding Rust/WASM platform
(ns clojure.wasm.host
  "WASM host implementation."
  (:require [clojure.host :refer [Host]]))

(def wasm-host
  (reify Host
    (-array [_ size] (wasm/alloc_array size))
    (-aget [_ arr i] (wasm/array_get arr i))
    ...))

(defn init! []
  (clojure.host/set-host! wasm-host))
```

## Benefits

1. **Simple mental model**: One protocol = one platform
2. **Quick backend**: Implement `Host`, get everything
3. **Thin abstractions**: `clojure.time` etc. are just 10-20 lines
4. **No scattered engines**: One `*host*` instead of many `*engine*` vars
5. **Easy testing**: Mock the host, test everything

## Migration Steps

1. Create `clojure.host` ✓
2. Create `clojure.php.host` ✓
3. Update abstractions to use `*host*` ✓
   - `clojure.time` - simplified
   - `clojure.io` - simplified
   - `clojure.regex` - simplified
   - `clojure.uuid` - simplified
   - `clojure.uri` - simplified
   - `clojure.math` - simplified
   - `clojure.concurrent` - simplified
   - `clojure.process` - simplified
4. Create `clojure.protocols` (slim ~25 protocols) ✓
5. Create `clojure.types/` with example `kernel.cljc` ✓
6. Move remaining `clojure.lang.*` to `clojure.types.*` ✓
   - `clojure.types.vector` - PersistentVector
   - `clojure.types.hashmap` - PersistentHashMap, MapEntry
   - `clojure.types.hashset` - PersistentHashSet, set operations
   - `clojure.types.lazy` - LazySeq, Delay, Volatile, Range, Repeat, Iterate, Cycle
   - `clojure.types.var` - Var, dynamic bindings
   - `clojure.types.ref` - Ref, STM
   - `clojure.types.agent` - Agent
   - `clojure.types.namespace` - Namespace
   - `clojure.types.multifn` - MultiFn, hierarchy
   - `clojure.types.ex` - ExceptionInfo
   - `clojure.types.transient` - TransientVector, TransientHashMap, TransientHashSet
7. Delete old `clojure.lang/` files ✓
   - Deleted: protocols.cljc, numbers.cljc, lazy.cljc, var.cljc, namespace.cljc,
     multifn.cljc, ref.cljc, agent.cljc, transient.cljc, seqs.cljc, string.cljc,
     ex.cljc, hashmap.cljc, hashset.cljc, kernel.cljc, RT.cljc, array.cljc, vector.cljc
   - Deleted: clojure.php.host.array.cljc, clojure.jvm.array.cljc (obsolete)
   - Updated: clojure.lang.cljc facade to use clojure.types.*
8. Update dependencies ✓
   - clojure.jvm.lang - uses clojure.protocols
   - clojure.core - uses clojure.host for buffer/sort/shuffle ops
   - clojure.host - added buffer, asort, shuffle operations
   - clojure.php.host - implemented buffer, asort, shuffle
9. Update compiler to use new paths ✓
   - Verified: Compiler emits to `\\Clojure\\Php` runtime namespace (unchanged)
   - JVM-only code paths correctly use `clojure.lang.*` Java classes
   - No source namespace references needed updating

## Migration Complete ✓

## Files Created/Modified

### Core Architecture
- `src/clj/clojure/host.cljc` - ONE protocol for all platform primitives
- `src/clj/clojure/php/host.cljc` - PHP host implementation (internal)
- `src/clj/clojure/protocols.cljc` - Slim ~25 core protocols

### Type System (clojure.types.*)
- `src/clj/clojure/types/kernel.cljc` - Cons, Sym, Kw, Atom
- `src/clj/clojure/types/vector.cljc` - PersistentVector, ChunkedSeq, RSeq
- `src/clj/clojure/types/hashmap.cljc` - PersistentHashMap, MapEntry, HAMT nodes
- `src/clj/clojure/types/hashset.cljc` - PersistentHashSet, set operations
- `src/clj/clojure/types/lazy.cljc` - LazySeq, Delay, Volatile, Range, Repeat, Iterate, Cycle
- `src/clj/clojure/types/var.cljc` - Var, Unbound, dynamic binding
- `src/clj/clojure/types/ref.cljc` - Ref, Transaction, STM
- `src/clj/clojure/types/agent.cljc` - Agent, async actions
- `src/clj/clojure/types/namespace.cljc` - Namespace, intern/refer
- `src/clj/clojure/types/multifn.cljc` - MultiFn, hierarchy, dispatch
- `src/clj/clojure/types/ex.cljc` - ExceptionInfo, exception utilities
- `src/clj/clojure/types/transient.cljc` - TransientVector, TransientHashMap, TransientHashSet

### Shared Macros (clojure.compiler.macros)
- `src/clj/clojure/compiler/macros.cljc` - ALL macro expansions
  - Destructuring: let→let*, fn→fn*, loop→loop*, defn→def+fn*
  - Control flow: when, when-not, if-not, cond, and, or
  - Conditional binding: if-let, when-let, if-some, when-some
  - Conditionals: condp, case
  - Threading: ->, ->>, as->, cond->, cond->>, some->, some->>
  - Iteration: dotimes, while, for, doseq
  - Misc: comment, doto, letfn, declare, ns, defn-
- `src/clj/clojure/compiler/analyzer.cljc` - Registers macros in *macros* atom

### PHP Runtime (simplified)
- `src/clj/clojure/php/runtime.cljc` - PHP-specific functions
  - Removed all duplicated macros (now in clojure.compiler.macros)
  - Kept: lazy-seq, binding, with-bindings, defonce, with-open
  - Contains all core functions (map, filter, reduce, etc.)

### Simplified Abstractions (removed Engine protocols)
- `src/clj/clojure/time.cljc` - was 260 lines, now ~100
- `src/clj/clojure/io.cljc` - was 420 lines, now ~110
- `src/clj/clojure/regex.cljc` - was 210 lines, now ~90
- `src/clj/clojure/uuid.cljc` - was 175 lines, now ~75
- `src/clj/clojure/uri.cljc` - was 195 lines, now ~85
- `src/clj/clojure/math.cljc` - was 325 lines, now ~120
- `src/clj/clojure/concurrent.cljc` - was 290 lines, now ~125
- `src/clj/clojure/process.cljc` - was 165 lines, now ~95
