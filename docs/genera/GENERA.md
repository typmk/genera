# genera — Working Document

> No distinction between the compiler and the thing being compiled.
> Both can see each other. This is genera.

## What genera IS

There is no compiler. There is no runtime. There is no GC. There is memory,
and rules that derive facts from facts. The memory that holds the source,
the views, and the machine code is the same memory that the machine code
executes in. The bitmask scan that computes "is this node pure?" and the
bitmask scan that computes "is this allocation live?" are the same operation
on the same address space. The system that compiles the program IS the program.

A live image environment. Clojure the language, Genera the philosophy.

The name works three ways:
- **genus** — the root form from which specific instances emerge
- **Genera** — the Symbolics Lisp Machine where everything was one inspectable substrate
- **general** — universal, not specialized to one domain

The C runtime is the machine — memory layouts, JIT, reader, arena.
The Lisp is the mind — macros, conditions, protocols, image, debugger.

Once the machine bootstraps eval, the mind writes itself.

### The Reflexive Property

The compiler holds GNodes in memory and scans them with bitmask views.
The compiled code, when it runs, holds data in the same memory. The
"GC" is the same bitmask scan. The "allocator" is the same bitmask scan.
The "profiler" is the same bitmask scan. There is one operation — query
facts, derive facts — applied to different regions of the same flat memory.

Compilation is not a phase that produces a separate artifact. It is an
ongoing annotation of memory. The annotation "these bytes are instructions"
is the same kind as "these bytes are GNodes" is the same kind as "these
bytes are live allocations." The rules engine does not know which it is doing.

### One Substrate

Handlers, memory state, dispatch — all GNodes. Not separate systems.

```
KIND_DEF       (def x 42)          — names a value
KIND_FN        (fn [x] (+ x 1))   — names behavior
KIND_HANDLER   (on :page handler)  — names a dispatch target
KIND_MEM       (mem :step)         — names a memory region
```

All are nodes in the grammar. All have val[] entries. All are scannable
by the same bitmask views. The "sig dispatch table" is a view query:
"find HANDLER nodes for this name." The "allocator state" is a view
query: "read val[] of MEM nodes." No separate tables. No separate systems.

```
Code facts:     v_pure[node],  v_dead[node],  v_alloc[node]
Handler facts:  v_handler[node] — is this a HANDLER node for name N?
Memory facts:   v_live[node] — is this allocation reachable?
Dispatch trace: v_dispatched[node] — was this HANDLER invoked this step?
```

Same bitmask. Same scan. Same ops (BM_GET, POPCOUNT, AND, OR).
Self-observation: Clojure queries these bitmasks through builtins.
Programs observe their own compilation, dispatch, and memory — reflexively.

The irreducible C kernel (~300 lines) bootstraps the first Mem, the first
GNodes, the first views. Above the kernel, everything is GNodes.

### Everything Is a Grammar

Parse grammars, view rules, backend emission, syscall tables, x86 encoding,
GC policy, memory layout — all are the same shape: pattern → output.

```
Grammar (EDN)  = structure description (data at rest)
View (Clojure) = computation over structure (data in motion)
```

Clojure IS EDN with evaluation. The distinction dissolves. Grammars are
EDN. Views bring them to life. That is all.

| Grammar | Pattern over | Produces |
|---------|-------------|----------|
| Parse | bytes | GNodes |
| Views | GNodes + views | bits (derived facts) |
| Backend | GNodes + views | Val / x86 / C text |
| Syntax | GNodes | display (parens, outline, treemap, flamechart) |
| Syscalls | operation + target | nr + arg regs |
| x86 encode | instruction + operands | bytes |
| Dispatch | HANDLER nodes + name | handler Val (sig = view query) |
| Memory | MEM nodes + roots | liveness bits (GC = view query) |
| Layout | access trace + liveness | memory arrangement |

One engine applies all of them. Rules as Clojure/EDN data, parsed by
genera's own parser, stored in its own persistent structures. The system
defines itself.

### Syntax Is a View

The GNodes are the program. Parentheses, indentation, Python-like syntax,
treemap, flamechart — these are all bidirectional views of the same tree.
Edit through any view, GNodes change, all other views update.

```
GNodes  ↔  (defn add [a b] (+ a b))          s-expr view
        ↔  defn add / params: a b / body: +   outline view
        ↔  def add(a, b): return a + b         python-like view
        ↔  [visual: area=size, color=purity]   treemap view
        ↔  [visual: width=time, stack=depth]   flamechart view
```

### The Query/Transformation Collapse

There is no line between query and transformation. A view rule reads facts
(query) and produces facts (transformation). In a monotone system — facts
only accumulate, never retract — this distinction dissolves. Every rule is
both. The fixed-point engine applies all rules until no new facts emerge.

This is Datalog. The bitmasks are the fact database. The rules are Horn
clauses. The fixed-point iteration is bottom-up evaluation. The "compiler"
is Datalog over syntax facts producing code facts.

### The Irreducible Kernel

~300 lines of C. Everything else is Clojure compiling itself.

1. Arena allocator (~50 lines)
2. EDN reader (~150 lines)
3. Bitmask ops (~50 lines — AND, OR, POP, SET, CLR, SCAN)
4. write(2) — emit bytes to fd or memory

The bootstrap: 300 lines of C reads Clojure grammar/view/backend
definitions → applies views to itself → emits native binary → that
binary can do the same.

---

## The Model: World → Step → World

One operation. No modes. No phases. No separate "compiler" or "runtime" or "GC."

```
world → step → world
```

A **world** is a persistent value containing facts and derived views.
A **step** reads facts, derives new facts, asserts them. Produces the next world.

What we call "parse" is a step that asserts syntax facts.
What we call "analyze" is a step that derives view facts.
What we call "execute" is a step that derives value facts.
What we call "collect" is a step that retracts dead facts.
What we call "save" is a step that serializes the world.

They are all the same operation: read facts, derive, assert.

### Facts and Views

```
FACTS (ground truth, must be stored):
  Source bytes, GNode structure, intern table, external input

VIEWS (derived, recomputable — optional caches):
  V_CONST, V_PURE, V_DEAD, bind[], scope[], const_val[], code_off[]
```

Facts are authoritative. Views are derivable from facts.
Whether a view is held in memory or recomputed is a cache decision, not a semantic one.
Same as L1/L2/L3 — storage tier, not meaning.

### The Algebra

Six operations. Five are single CPU instructions.

```
AND    — intersection of fact sets     (filter)       → 1 cycle
OR     — union of fact sets            (merge)        → 1 cycle
POP    — count facts in a set          (aggregate)    → 1 cycle
SET    — assert a fact                 (define)       → 1 cycle
CLR    — retract a fact                (free)         → 1 cycle
SCAN   — walk set bits                 (iterate)      → CTZ + shift loop
```

This is simultaneously the query language, the memory model, and the compilation model.
They were never different things.

### HAMT for State, Bitmasks for Query

Two structures, complementary roles:

```
HAMT (pmap):     WHAT the facts are (versioned, structural sharing, history)
Bitmask views:   WHICH facts match a predicate (O(1) query, AND/OR/POPCOUNT)
```

HAMT = store. Bitmasks = indices. Same as a database has tables and indices.
Each step produces a new HAMT root. Views are derived per-version.
Old versions persist via structural sharing. History is nearly free.

### Scratch / Transient

Within a step, mutable scratch for speed:
- Allocate in arena (bump pointer, fast)
- Mutate freely (transient)
- Produce persistent result (new world root)
- Reset arena (bulk discard)

Arena IS the transient. `persistent!` IS arena → HAMT promotion.

### The Effect Boundary

Pure computation stays in the world — reversible, cacheable, reorderable.
Effects cross the boundary — I/O, syscalls, external state changes.

```
Pure:   (+ 2 3) → derives val[id] = 5              (stays in world)
Effect: (println "hello") → bytes to fd 1           (crosses boundary)
```

V_PURE and V_IO already mark this boundary in genera's analysis passes.

### sys = Same Bytes, Different Destination

A syscall at runtime and a compiled syscall are the same bytes.
`mov rax, 1; syscall` is identical whether GCC compiled it or genera's JIT emits it.

```
emit(exec_page, bytes)  → execute (runtime)
emit(code_buf, bytes)   → compile (code generation)
emit(file, bytes)       → persist (binary output)
emit(arena, bytes)      → allocate (memory)
```

One verb. The destination determines the interpretation.

### Self-Reference

The step function itself is facts in the world — parseable, analyzable, compilable by the same mechanism. The system processing itself is just another step. lambda(lambda).

### Retention Policy

```
Epoch drop (default):    Each step = epoch. Arena reset between steps.
Ring buffer (interactive): Keep last N world versions. Undo = previous root.
Checkpoint (persistence): Serialize current version to .gen file.
```

No tracing GC needed. GNodes are shared (immutable). Views are per-version (small, dropped wholesale). Scratch is arena-reset.

### Implementation Status

**Step 1: World struct.** DONE. `g_gram_scratch` → `g_world` (World = Gram + version + epoch).

**Step 2: val[] for all nodes.** DONE. `const_val[id]` → `val[id]`. Any node can hold a computed value.

**Step 3: eval on GNodes.** DONE. `eval_node(g, id, env)` walks GNodes directly — same tree walk as JIT's `cg_expr`. `entity_to_val` retained only for: quote (data), closure body capture, macro args. Zero external call sites. Eval tests ~40% faster (entity_to_val copy eliminated).

**Step 4: Unified step.** DONE. `world_step(source, analyze)` — common front-end for all backends:
```c
Gram *g = world_step(source, true);   // JIT/CC: parse + index + analyze
Gram *g = world_step(source, false);  // eval: parse only (V_DEAD would suppress REPL defs)
```
eval_string, compile_program, cc_emit all use world_step. No more duplicated Lang init / parse boilerplate.

**Step 5: sys as data table.** NOT STARTED. Unify `_sc3()` (runtime) and `x_imm()+xb()` (compile) into a single operation table:
```c
typedef struct { u16 nr; u8 n_args; u8 arg_regs[6]; } SysOp;
SysOp sys_ops[] = { [1] = {SYS_write, 3, {RDI, RSI, RDX}}, ... };
```
Runtime: fill regs from table, syscall instruction. JIT: emit reg fills from table, emit syscall bytes. Same table, different destination.

---

## Primitives

Three primitives. Everything else is derived.

```
1. GNode  — a node in the grammar (code, handlers, memory, dispatch — all the same)
2. Val    — universal value (NaN-boxed u64, attached to nodes via val[])
3. Mem    — {base, used, cap} — the irreducible atom (you need memory to store GNodes)
```

| Concept | What it is | Derived from |
|---|---|---|
| sig / dispatch | HANDLER GNodes queried by name | view over grammar |
| env / bindings | DEF GNodes with val[] entries | view over grammar |
| memory management | MEM GNodes + V_LIVE view | view over grammar |
| image | serialize all GNodes + Mem regions | contiguous memory |
| conditions | HANDLER return values | function calls |
| namespaces | DEF GNodes scoped by prefix | name hierarchy |
| history | World.epoch + structural sharing | append-only GNodes |

## What's Built (the C machine)

31 files, ~11.6K lines, 128KB binary, 499 tests.

```
sys/    — types, syscalls (linux/x86_64), api, x86 encoding
std/    — Mem, Val, arena, intern, arr, fmt, tap, cmd
lang/   — grammar, eval, coll (pmap/pvec/atom/sig), proto, jit, cc, repl, cli, test, bench
```

### Capabilities

| Layer | File(s) | What |
|---|---|---|
| Types | std/val.c | NaN-boxed u64: nil, bool, int, f64, sym, kw, str, cons, fn, pvec, pmap |
| Memory | std/mem.c | Mem atom {base, used, cap}, bump/RESTORE/copy |
| Intern | std/str.c | String interning → StrId (u32), O(1) compare, parent links |
| Collections | coll.c | pmap (HAMT), pvec (32-way trie), atom (CAS), transient/persistent! |
| Dispatch | coll.c | sig: StrId → handler array, ~1ns lookup. Dissolves into HANDLER GNodes. |
| Conditions | coll.c | g_signal/g_signal_val — condition/restart protocol (return values) |
| Protocols | proto.c | Type-dispatch: get, first, rest, count, seq, assoc per type |
| Reader | grammar.c | Multi-lang parser (lisp, c, bf), bitmask-indexed GNodes |
| Views | grammar.c | V_DEF, V_REF, V_CALL, V_CONST, V_INT, V_FN, V_PURE, V_TAIL, V_DEAD |
| Passes | grammar.c | pass_scope → pass_type → pass_flow (cascading bitmask analysis) |
| Eval | eval.c | McCarthy eval/apply, 13 special forms, 50+ builtins |
| JIT | jit.c | x86-64 emission + Clojure walker (cg-expr → x86). Both walkers: 499 tests. |
| CC | cc.c | C source emission → gcc → capture output |
| REPL | repl.c, cmd.c | Interactive line editor, undo/redo (world atom), command dispatch |
| Test | test.c | 499 tests across 24+ groups |
| Bench | bench.c | pmap, pvec, dispatch benchmarks |

### Architecture Rhymes

| genera C | PHP Moss | SBCL/SWANK | Genera (Symbolics) |
|---|---|---|---|
| coll.c (pmap + sig) | coll.php (pmap + sig) | symbol table + function cells | obarray + CLOS |
| eval.c | session.php (gate + eval) | eval + compiler | evaluator |
| grammar.c views | — | — | presentation types |
| jit.c / cc.c | — | compiler | native compiler |
| cmd.c REPL | wire.php (transports) | SWANK protocol | Listener |
| g_signal | sig conditions | CL condition system | condition system |
| atom(pmap) | atom(pmap) Worker 0 | image (heap) | world |

## What's Next (graduation to genera Lisp)

### Phase 1: defmacro — the gateway

Add macro support to eval.c (~30 lines). Then graduate C special forms to genera macros.

**C changes:**
- Add MACRO flag to FnObj (1 bit)
- In eval(): if head resolves to macro → call with unevaluated args → eval result
- `defmacro` special form: like `defn` but sets MACRO flag
- `macroexpand-1` builtin: expand once, return form

**First macros (written in genera Lisp):**
```clojure
(defmacro when [test & body]
  (list 'if test (cons 'do body)))

(defmacro cond [& clauses]
  (if (first clauses)
    (list 'if (first clauses) (nth clauses 1)
      (cons 'cond (rest (rest clauses))))))

;; and/or need gensym to avoid double-eval
(defmacro and
  ([] true)
  ([x] x)
  ([x & rest] (list 'let ['__and x] (list 'if '__and (cons 'and rest) '__and))))
```

**Delete from C:** sf_when, sf_cond, sf_and, sf_or (4 handlers → 4 lines of genera)

### Phase 2: syntax-quote (backtick)

Without backtick, macros require verbose `(list 'if ...)`. With it:

```clojure
(defmacro when [test & body]
  `(if ~test (do ~@body)))
```

**C changes:**
- Reader: `` ` `` → syntax-quote form, `~` → unquote, `~@` → unquote-splicing
- eval: syntax-quote walks the form, evaluating unquoted parts

### Phase 3: genera.rt — expose the machine

C builtins that expose internals as genera data:

```clojure
;; Values
(rt/val-bits 42)             ; => raw u64
(rt/type-tag 42)             ; => :int
(rt/val-from-bits 0x...)     ; => reconstruct Val

;; Memory
(rt/arena-stats)             ; => {:perm N :req N :peak N}
(rt/arena-reset! :req)       ; => reset request arena

;; Dispatch
(rt/sig-list)                ; => list of all registered handlers
(rt/sig-get 'def)            ; => handler info

;; Intern
(rt/intern-stats)            ; => {:count N :bytes N}
(rt/str-from-id 42)          ; => string for StrId 42
```

### Phase 4: genera.grammar — parser as data

Expose grammar.c nodes and views as genera data:

```clojure
(def g (grammar/parse "(defn f [x] (+ x 1))"))

(grammar/nodes g)            ; => pvec of node maps
(grammar/views g)            ; => {:def #{...} :ref #{...} :const #{...}}
(grammar/view g :tail)       ; => bitmask as pvec of node indices
(grammar/text g 3)           ; => source text of node 3
(grammar/children g 0)       ; => child nodes of root

;; Analysis
(grammar/analyze g)          ; => run all passes, return enriched grammar
(grammar/has-recur? g 5)     ; => does subtree contain recur?
```

### Phase 5: destructuring

```clojure
(let [{:keys [a b]} {:a 1 :b 2}]
  (+ a b))

(let [[x y & rest] [1 2 3 4 5]]
  rest)  ; => (3 4 5)
```

Implement as macro expansion → nested let/get/nth calls.

### Phase 6: condition system

```clojure
;; Signal a condition (before-unwind — stack preserved)
(signal :divide-by-zero {:numerator 42 :denominator 0})

;; Handle conditions
(handler-bind [:divide-by-zero
               (fn [c] (restart :use-value 1))]
  (/ 42 0))

;; Establish restarts
(restart-case (/ x y)
  (:use-value [v] v)
  (:retry [] (/ x (inc y))))
```

Maps to existing g_signal mechanism. Handlers stored in pmap. Restarts are return values (not closures — cross-worker safe, same as PHP sig).

### Phase 7: namespaces

```clojure
(ns genera.core)           ; => switch to namespace (a pmap)
(ns genera.rt)

;; Namespaces are pmaps in a root pmap
;; (get @image 'genera.core) => pmap of bindings
;; Qualified lookup: genera.core/map → get-in image ['genera.core 'map]
```

### Phase 8: protocols in genera Lisp

```clojure
(defprotocol ISeq
  (first [coll])
  (rest [coll]))

(extend-type :pvec
  ISeq
  (first [v] (nth v 0))
  (rest [v] (into [] (drop 1 v))))
```

Macro that generates protocol dispatch entries in pmap + proto.c bindings.

### Phase 9: image persistence

```clojure
(image/save "genera.img")   ; => serialize pmap to file
(image/load "genera.img")   ; => restore pmap from file
(image/snapshot)             ; => frozen pmap value
(image/diff v1 v2)           ; => structural diff
```

pmap is pure data — serialization is walking the HAMT, writing key-val pairs. Deserialization rebuilds. Structural sharing means snapshots are cheap.

### Phase 10: debugger

```clojure
(debug/break 'my-fn)        ; => set breakpoint (condition on dispatch)
(debug/frames)               ; => current stack as pvec of pmaps
(debug/locals 3)             ; => bindings in frame 3 as pmap
(debug/eval-in-frame 3 'x)  ; => value of x in frame 3
(debug/restarts)             ; => available restarts
(debug/invoke-restart 0)     ; => invoke restart by index
```

Built on conditions (phase 6) + rt (phase 3) + image (phase 9).

## Roadmap (collapsed)

Three steps. Step 2 is the gate.

Grammar to grammar. The grammar engine doesn't just replace the walker —
it IS the walker. The shared walker refactor is unnecessary; the engine
replaces eval/jit/cc entirely.

```
1 (defmacro + syntax-quote, ~90 lines C)
  ↓
2 THE ENGINE (~200 lines C) — pattern match on spans, produce facts
  ↓
3 feed it rules (views, backends, syscalls, syntax, memory, itself)
```

### Step 1: defmacro + syntax-quote (~90 lines C)

The gate to writing genera in genera. Delete sf_when, sf_cond, sf_and,
sf_or from C. Everything after this can be Clojure.

### Step 2: The grammar engine (~200 lines C)

One engine. All it knows: spans and facts about spans.

- Grammar = bytes → spans (already done: gram_parse)
- Engine = spans + rules → facts (the new piece)
- Rules are Clojure/EDN, parsed by genera's own reader into GNodes
- The engine pattern-matches on span structure, produces new facts (bits)
- Iterates all rules to fixed point (no explicit ordering)
- Doesn't know what domain it's operating in

The engine replaces:
- gram_analyze() — views become rules
- eval.c — eval becomes a backend rule set
- jit.c — JIT becomes a backend rule set
- cc.c — CC becomes a backend rule set

No intermediate "shared walker" step. The engine IS the walker.

### Step 3: Feed it rules

Each domain is just a rule table. The engine interprets them all the same way.

| Domain | Rules | Replaces |
|--------|-------|----------|
| Views | ~20 lines Clojure | ~240 lines C (gram_analyze) |
| Eval backend | ~80 lines Clojure | ~1,446 lines C (eval.c) |
| JIT backend | ~100 lines Clojure | ~680 lines C (jit.c) |
| CC backend | ~80 lines Clojure | ~534 lines C (cc.c) |
| Syscalls | EDN table (exists: `src/c/sys/syscalls.edn`) | ~888 lines C (sys/) |
| x86 encoding | EDN table | ~203 lines C (x86.c) |
| Syntax views | ~30 lines Clojure per syntax | new capability |
| GC/memory | rules over allocation bitmasks | new capability |
| Itself | rules describing pattern-match + bitmask ops | self-compilation |

### Size trajectory

| State | C lines | Clojure lines | Total |
|-------|---------|---------------|-------|
| Current | ~10,900 | 0 | 10,900 |
| After step 1 | ~11,000 | ~20 | 11,020 |
| After step 2 | ~11,200 | ~20 | 11,220 (engine added) |
| After step 3 | ~300 | ~600 | 900 (everything else is rules) |

## Key Design Decisions

1. **Everything is a GNode** — code, handlers, memory state, dispatch targets. One substrate. One query mechanism (bitmask views).
2. **Names are StrId (u32)** — interned integers. Hierarchy via parent links (precomputed at intern). Dispatch, env, sig — all name lookup.
3. **Val is homoiconic** — code (lists) and data (maps, vectors) are the same type. Macros transform Val → Val.
4. **Memory: one atom (Mem), three ops (bump, RESTORE, copy)** — provably minimal. Mem instances derived from views hitting constraints. See `ALLOCATOR.md`.
5. **sig = view query over HANDLER nodes** — no separate dispatch table. `send(name)` = find HANDLER GNode + parent walk. Conditions = return values.
6. **C is the bootstrap** — ~300 lines irreducible kernel. Everything else is Clojure compiling itself.
7. **Grammar views are the universal IR** — bitmask passes derive facts about code, memory, dispatch without transforming anything. Source is truth.
8. **Self-observation** — programs query their own views through Clojure builtins. Profiler, debugger, tracer = Clojure code, not runtime.

## Build & Test

```bash
# Build
gcc -O3 -march=native -nostdlib -static -o genera src/c/moss.c

# Test
./genera test

# REPL
./genera

# Eval
./genera eval "(+ 1 2 3)"

# JIT
./genera jit "(fn [x] (+ x 1))"

# Emit C
./genera emit "(defn f [x] (+ x 1))"
```

## References

- `ref/defnet/native/clojure-fast/docs/VISION.md` — the original architecture
- `ref/defnet/native/clojure-fast/docs/GENERA_REPRESENTATION.md` — presentations, structure editing
- `ref/defnet/native/clojure-fast/docs/CLOJURE_IDEAS.md` — Clojure concepts → implementation
- `docs/GLASS.md` — grammar views architecture (bitmask passes)
