# GLASS

> The system doesn't have a debugger. The system IS a debugger.
> You don't add observability — observability is the substrate.

## The Insight

All languages have parens. They just use different delimiters — some braces, some indentation. The tree is the tree. A 256-byte char class table + delimiter pairs = complete language spec. Parse any language into the same flat node array. Annotate with bitmask views. Emit through any surface.

The grammar parses any language. The analysis is a program. The program is in the language. The language parses the analysis. The analysis analyzes itself. **lambda(lambda).**

Every pass is observable. Every view is data. Every layer is transparent. From source bytes to machine code to live runtime — nothing hidden, nothing lost, every bit traceable from register back to source character.

Pre-debugging. Because the program was never opaque.

---

## Architecture: Source ↔ Views ↔ Machine Code

```
source bytes
  → intern (strings → StrIds, O(1) forever after)
    → parse (GNodes, byte offsets, bitmask indexes)
      → pass 1 (scope, binding — produces views, consumes indexes)
        → pass 2 (types, purity — produces views, consumes pass 1)
          → pass 3 (flow, dead — produces views, consumes pass 2)
            → emit (consumes everything, produces machine code)
```

Each pass ADDS data. Nothing transformed, nothing lowered, nothing lost. The source bytes are still there. The GNodes still point into them. The views still point into GNodes. The machine code still maps back to GNodes. Every layer is an annotation on the layer below.

**Bidirectional.** Source ↔ GNode ↔ Views ↔ Machine Code. You can go any direction. The debug info IS the analysis IS the optimization data. Same bits.

### Comparison with Traditional Compilers

```
Traditional:  Source → AST → IR → Optimized IR → Machine Code
              Four representations. Three translations. Each a loss.

GLASS:        Source → GNode + bitmasks → Machine Code
              Two representations. One translation. Analysis is annotation.
```

### Comparison with Genera

Symbolics Genera: entire OS in Lisp. Every object inspectable. The compiler, debugger, editor, window system — all Lisp, all transparent. ~800K lines.

GLASS: same idea. Same transparency. ~4K lines. 200x smaller. Because the bitmask representation compresses everything — views, queries, analysis, dispatch — down to bit operations on flat arrays. No object system. No class hierarchy. No GC. Just bits.

| Genera | GLASS |
|--------|-------|
| Lisp listener (REPL) | REPL (cmd.c + repl.c) |
| Describe (inspect anything) | Bitmask views (inspect any node) |
| Flavors/CLOS (polymorphism) | Protocols (proto.c) |
| Compiler in Lisp | Compiler in genera (self-hosting) |
| Everything is Lisp | Everything is GNodes + bitmasks |
| ~800K lines | ~4K lines |

---

## Views = Derived Bitmasks

A "view" is a bitmask computed by an analysis pass over the GNode tree. Same shape as `g->m[NK_IDENT]`, but with semantic meaning.

### Structural Views (grammar.c — already exist)

```c
g->m[NK_LIST]      // all list nodes
g->m[NK_IDENT]     // all identifiers
g->m[NK_NUM]       // all number literals
g->m_group         // all group nodes (list/vec/map)
g->m_leaf          // all leaf nodes (ident/num/str/kw)
g->m_first         // first-child nodes
```

### Semantic Views (analysis passes — new)

```c
View v_def;         // definition sites (defn, def, let bindings)
View v_ref;         // reference sites (symbol uses)
View v_tail;        // nodes in tail position
View v_pure;        // side-effect-free subtrees
View v_const;       // compile-time-known values
View v_dead;        // unreachable code
View v_int;         // known-int expressions
View v_vec;         // known-pvec expressions
View v_map;         // known-pmap expressions
View v_fn;          // known-fn expressions
```

### Query API: Three Operations (Complete)

```
PROJECTION:   bm_and(a, b)     — what matches BOTH properties
COMBINATION:  bm_or(a, b)      — what matches EITHER property
REDUCTION:    bm_pop(m)        — HOW MANY match
```

AND, OR, popcount. Three bit operations. Complete for set queries.

```c
// O(1) queries on any subtree
gn_has(g, v_tail.m, fn_id)              // does fn contain tail calls?
gn_sub_count(g, v_dead.m, fn_id)        // how many dead nodes in fn?

// Composable
bm_and(tmp, v_tail.m, v_pure.m, nw);    // tail AND pure
gn_has(g, tmp, fn_id);                   // does fn have pure tail calls?

// Derived
bm_or(safe, v_int.m, v_const.m, nw);    // int OR known-const
```

This IS Datalog, but on hardware:

| Datalog | Bitmask |
|---------|---------|
| Relation (set of tuples) | `u64 *m` (set of node IDs) |
| Join | `bm_and` |
| Union | `bm_or` |
| Project | `bm_next` iterate |
| Count | `bm_pop_range` |
| Contains | `bm_any_range` |

### SIMD Performance

One AVX2 instruction processes 256 nodes. At ~4 ops/node/pass, ~3 passes = ~12 ops/node total. At 1 GHz effective throughput: ~20B node-ops/s ÷ 12 = ~1.6B nodes/s. At ~5-10 nodes per line of code: **~2B LOC/s**. The analysis is faster than reading the file from SSD.

---

## The Three Passes

Each pass produces views. Each depends only on the pass before it.

### Pass 1: Scope + Binding (~80 lines)

```
Input:  GNode tree + m[NK_IDENT]
Output: v_def, v_ref, bind[] (ident → def site), scope[] (node → enclosing fn)
```

Walk the tree once. For each `(defn name ...)`, `(def name ...)`, `(let [name ...] ...)`, `(fn [name ...] ...)` — mark the name node in `v_def`. For each identifier that's NOT in head position of a list AND NOT a def site — mark in `v_ref`. Build `bind[ref_id] = def_id` using scope chain.

### Pass 2: Type + Purity (~100 lines)

```
Input:  GNode tree + bind[] + scope[]
Output: v_int, v_vec, v_map, v_fn, v_pure, v_const, type[] (node → type tag)
```

Bottom-up walk (leaves first, then parents):
- Literal `42` → v_int. Literal `[1 2 3]` → v_vec. `{:a 1}` → v_map.
- `(+ a b)` where a,b ∈ v_int → v_int. `(conj v x)` where v ∈ v_vec → v_vec.
- Pure: builtins that don't I/O + args all pure → v_pure.
- Const: pure + args all literal → v_const.

### Pass 3: Flow + Dead Code (~60 lines)

```
Input:  GNode tree + type[] + v_const
Output: v_tail, v_dead
```

Top-down walk for tail position:
- Last expression in `(do ...)` body → v_tail
- Both branches of `(if test then else)` when `if` is in tail → v_tail
- `(let [...] body)` last body expr → v_tail

Dead code:
- `(if true then else)` where test ∈ v_const and false → else in v_dead
- `(when false ...)` → body in v_dead

---

## Emission Uses Views

```c
static void emit_node(Comp *cc, Gram *g, u32 id) {
    if (BM_GET(v_dead.m, id)) return;                   // dead code elimination
    if (BM_GET(v_const.m, id)) {                         // constant folding
        x_imm(cc->cb, RAX, const_val[id]);
        return;
    }
    if (gn_kind(g, id) == NK_LIST) {
        StrId sym = gn_intern(g, gn_child(g, id));
        if (sym == S_ADD && BM_GET(v_int.m, id)) {       // type-specialized
            emit_int_add(cc, g, id);
            return;
        }
    }
    emit_generic(cc, g, id);                             // generic path
}
```

### Why This Can Beat gcc for This Domain

1. **gcc doesn't know your types.** Views say "this is int" in O(1).
2. **gcc handles aliasing, UB, pointer escape.** Immutable data — skip all that.
3. **gcc's 400 passes handle C's complexity.** ~3 passes for Clojure's simplicity.
4. **Bitmask queries = SIMD-width analysis.** 256 nodes per AVX2 instruction.

---

## Homoiconicity: Three Levels

### Level 1: Structure is Data

Source text IS the tree. GNodes are byte offsets into source. No separate AST, no lowering, no translation. The analysis results live ON the source — just more bits per node.

### Level 2: Analysis is Code is Data

An analysis pass is a program in the language, operating on the language's own parse tree:

```clj
(defn mark-tail [g node]
  (let [k (kind g node)]
    (cond
      (= k :if)   (do (mark-tail g (nth-child g node 2))
                       (mark-tail g (nth-child g node 3)))
      (= k :do)   (mark-tail g (last-child g node))
      (= k :let)  (mark-tail g (last-child g node))
      :else        (set-bit! v-tail node))))
```

### Level 3: lambda(lambda)

The grammar parses the analysis pass into GNodes. The analysis runs on those GNodes. The JIT compiles the analysis. The compiled analysis runs at native speed on OTHER programs' GNode trees. Including on itself.

```
parse(pass-src) → GNodes → analyze(GNodes) → views → jit(GNodes, views) → native
       │                                                      │
       └──────────────────────────────────────────────────────┘
                    the compiled pass IS the pass
```

gcc's passes are C++. They can't analyze themselves. They can't be JIT-compiled. They're frozen at build time.

GLASS passes are GLASS. They analyze themselves. They JIT themselves. They improve themselves. **The self-application produces the fixed point.**

---

## Universal Rendering

The grammar is parameterized by a Lang spec (~270 bytes). Parse: surface → structure. Render: structure → surface. Same spec, opposite direction.

```
GNode tree ← parse(src, lang_lisp)
           → render(tree, lang_lisp)    →  (defn foo [x] (+ x 1))
           → render(tree, lang_c)       →  int foo(int x) { return x + 1; }
           → render(tree, lang_outline)  →  ▸ foo(x)
                                               ▸ + x 1
           → render(tree, lang_json)    →  {"fn":"foo","params":["x"],...}
           → render(tree, lang_treemap) →  [visual treemap]
```

ONE tree. Many surfaces. Switch view = swap ~270 bytes. The structure doesn't change. The views don't change. The analysis doesn't change. Only the pixels change.

Any source can be viewed as C-like or Lisp or outline/Roam-like or tree/treemap. The delimiter shape doesn't matter. The tree is the tree.

---

## Runtime Views

Static views (compile time): bitmask over GNodes.
Runtime views (execution time): bitmask over allocations/frames/objects.

Same query primitives:

```
leaked   = bm_and(allocated, unreachable)     // what's allocated but unreachable?
hot      = bm_and(allocated, frequently_hit)  // what's hot?
source   = node_of[alloc_id]                  // which source line allocated this?
```

Every allocation traces back to a GNode. Every GNode traces back to a byte offset in source. Every byte offset has views. **The runtime view IS the compile-time view, animated.**

Toggle any layer. Combine any layers. View in any surface:

```
source     →  "here's your code"
+ pass 1   →  "here's what binds to what"     (scope lines, colored)
+ pass 2   →  "here's the types"              (annotations, heatmap)
+ pass 3   →  "here's what's dead"            (greyed out)
+ emit     →  "here's the machine code"       (interleaved with source)
+ runtime  →  "here's what's executing now"   (live counters, flow)
```

---

## Consolidation: Current → GLASS

### What Dissolves

**read.c → grammar.c.** read.c's `CHAR_CLASS[256]` = grammar.c's `Lang.cc[256]`. read.c's `read_form()` + `classify()` = grammar.c's `entity_to_val()` + `entity_classify()`. The reader is the grammar without indexes. Delete the reader, keep symbol interning. **-220 lines.**

**image.c → views.** image.c manually walks Val cons lists to build `calls[32]`/`callers[32]` arrays. Fixed size, linear search. As bitmask views: "all call-site nodes" is one `u64 *m`. "Calls in function F" is `bm_any_range(m_call, F.id, F.end)`. No manual walk, no fixed arrays. **-100 lines.**

**eval.c / jit.c / cc.c triple dispatch → single walker + backends.** All three have identical recursive structure handling the same special forms (if, let, do, fn, defn, loop/recur, cond, when, and, or). Same tree walk, three times. Factor into one walk function dispatching to backend-specific leaf actions. **-800 lines.**

**test.c / bench.c → programs in the language.** `check_i64("add", jit_run("(+ 1 2)"), 3)` becomes `(assert (= 3 (+ 1 2)))`. Tests become data. Benchmarks become data. Runnable by any backend. Self-checking. Self-describing. Harness is ~40 lines. **-800 lines.**

**tap.c → runtime views.** The trace ring buffer generalizes to runtime bitmasks over GNode IDs. "Which source nodes executed?" = one bit per node. This IS code coverage IS profiling IS tracing. Same data structure as static views.

### The Map

```
BEFORE (14 lang/ files, ~4,880 lines)     AFTER (10 lang/ files, ~2,800 lines)

read.c (298)    ─── DISSOLVES ──→     grammar.c absorbs syms + classification
grammar.c (515) ─── STAYS ────→       grammar.c (universal parse + render)
image.c (140)   ─── DISSOLVES ──→     view.c (analysis passes, ~240 lines)
eval.c (833)    ─── SHRINKS ───→      eval.c (eval backend + builtins, ~400)
jit.c (510)     ─── SHRINKS ───→      jit.c (jit backend, ~300)
cc.c (424)      ─── SHRINKS ───→      cc.c (cc backend, ~250)
test.c (~500)   ─── DATA ─────→      tests.gen (in language) + harness (~30)
bench.c (~442)  ─── DATA ─────→      bench.gen (in language) + harness (~20)
proto.c         ─── STAYS ────→       proto.c
coll.c (757)    ─── STAYS ────→       coll.c
repl.c (234)    ─── STAYS ────→       repl.c
cli.c (101)     ─── STAYS ────→       cli.c
                                  NEW: walk.c (shared tree walker, ~100)
                                  NEW: view.c (passes + views, ~240)
```

### Size Budget

| Layer | Before | After | Change |
|-------|--------|-------|--------|
| sys/ | ~520 | ~520 | — |
| std/ | ~700 | ~700 | — |
| lang/ | ~4,880 | ~2,800 | **-2,080** |
| **Total** | **~6,100** | **~4,020** | **-34%** |

The ~4,020 lines does MORE than the 6,100:
- Universal parse + render (any language → any surface)
- Analysis passes with bitmask views (3 passes, O(1) queries)
- Dead code elimination, type specialization, constant folding
- Runtime views (profiling, coverage, tracing)
- Self-hosting tests and benchmarks
- Self-analyzing analysis passes (lambda(lambda))

---

## The Grounding

```
Lang spec (~270 bytes)   — a few integers define a language's surface
GNode (28 bytes)         — one node in the parse tree
View (u64 *m)            — one bit per node, one property
Pass (function)          — λ: GNodes → Views
Emit (function)          — λ: GNodes + Views → Machine Code

u64 word = 64 nodes per bitmask operation
AVX2     = 256 nodes per instruction
Cache line = 512 nodes per L1 access
```

Everything is bits. The Lang spec is bits. The GNode is bits. The View is bits. The machine code is bits. The analysis is bit operations on bits. The emission is bit operations producing bits.

```
PROJECTION:   bm_and    — what matches both     (AND instruction)
COMBINATION:  bm_or     — what matches either   (OR instruction)
REDUCTION:    bm_pop    — how many match         (POPCNT instruction)
```

Three CPU instructions. Complete for set queries over the entire program. Hardware IS the query engine.

---

## What Rhymes (php.php ↔ GLASS)

| php.php (PHP) | GLASS (C) | Pattern |
|---------------|-----------|---------|
| `$this->d[$id]` entity store | `g->nodes[id]` GNode array | Flat indexed storage |
| `$this->k[$kind]` kind index | `g->m[kind]` bitmask | Set-per-kind |
| `$this->p[$parent]` parent index | `gn_child/gn_next` links | Hierarchy |
| `$this->T[$target]` target index | `bind[ref] = def` | References |
| `$this->c["key"]` cached derived | `views[V_TAIL].m` bitmask | Lazy materialized view |
| `inferScope()` type pass | Pass 2: bottom-up type propagation | Flow-sensitive types |
| `cfg()` + `reachable()` | Pass 3: tail + dead | Control flow |
| `check()` rules | Pluggable analysis rules | Extensible checks |
| `edit()` + `inject()` + `trace()` | Source→source transform | Code modification |
| `$this->c = []` invalidate | Recompute views on re-parse | Incremental strategy |

Same architecture. php.php in PHP arrays (~1,900 lines). GLASS in bitmasks (~240 lines). The bitmask version is 100-1000x faster per query.

---

## Implementation Order

1. **Dissolve read.c into grammar.c** — move syms, delete reader
2. **Add view infrastructure** — View struct, analyze() shell, is/has/how_many wrappers (~30 lines)
3. **Pass 1: Scope + Binding** (~80 lines)
4. **Pass 2: Type + Purity** (~100 lines)
5. **Pass 3: Flow + Dead** (~60 lines)
6. **Factor shared tree walker** — extract from eval/jit/cc (~100 lines, saves ~800)
7. **Wire views to JIT** — type specialization, dead code elimination
8. **Dissolve image.c** — replace with Pass 1 views
9. **Move tests to language** — test harness + .gen test files
10. **Move benchmarks to language** — bench harness + .gen bench files
11. **Add render** — grammar.c render path (structure → surface via Lang spec)
12. **Runtime views** — extend tap.c to bitmask-over-GNodes

Each step is independently verifiable. Build + test after every step.
