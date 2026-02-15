# Compiler Passes Reference

> What GCC's ~400 passes actually are, what TCC's 1 pass actually is,
> and why genera is a graph database on bits.

Reference sources: `ref/gcc` (shallow submodule, 1.3M lines) and `ref/tcc` (shallow submodule, 64K lines).

---

## genera: Graph Database on Bits

After `str_intern()` at the parse boundary, zero strings remain. The entire
system is integers and bitmasks.

### The Representation

```
GNode[N]        flat array, 28 bytes/node, integer-indexed
StrId           u32, interned — symbol comparison is ==
view[V].m       u64*, 1 bit per node per property
bind[ref]       u32 → u32 (reference → definition site)
scope[node]     u32 → u32 (node → enclosing function)
const_val[node] Val (u64), parallel array for rich values
type[node]      u8, parallel array for type tags
```

### EAV Model (collapsed to bits)

| EAV | genera | Storage | Cost |
|-----|--------|---------|------|
| Entity | node index | dense u32 | O(0) — IS the array position |
| Attribute | view ID (V_DEF, V_PURE...) | selects which bitmask | O(0) — IS the pointer |
| Value | is/isn't | 1 bit | O(0) — IS the bit |
| Rich value | const_val[id], type[id] | parallel u64/u8 array | O(1) — array index |

### The Three Operations (complete for all queries)

| Op | Bit | SQL | Graph DB | Cost |
|----|-----|-----|----------|------|
| PROJECTION | AND | WHERE a AND b | MATCH...WHERE | 1 SIMD instruction / 256 nodes |
| COMBINATION | OR | WHERE a OR b | UNION | 1 SIMD instruction / 256 nodes |
| REDUCTION | POPCOUNT | COUNT(*) | count() | 1 SIMD instruction / 256 nodes |

### Why O(1) / O(0)

"Is node 7042 pure?" = `BM_GET(v_pure.m, 7042)` = load + shift + AND. O(1). No hash. No string compare. No traversal.

"Is node 7042 pure?" is really O(0) — the answer IS the bit. It was precomputed into the bitmask. The query doesn't compute anything, it reads a bit that already exists.

"How many pure tail calls in function F?" =
```c
bm_and(tmp, v_pure.m, v_tail.m, mw);   // pure AND tail
bm_pop_range(tmp, F.start, F.end);      // count in range
```
Two SIMD passes over the range. O(range / 256). For a 1K-node function: 4 instructions.

Compare GCC: `pass_local_pure_const` (walks IR, builds side-effect summary, 2500 lines) + `pass_tail_recursion` (walks IR, pattern-matches, 1800 lines). Two full IR walks with pointer chasing, hash lookups, string comparisons.

### The Graph Database

| Graph DB concept | genera | Storage |
|-----------------|--------|---------|
| Node | GNode | flat array, integer-indexed |
| Node property | view bit | bitmask, SIMD-queryable |
| Edge | child/next/parent/bind | u32 → u32, array-indexed |
| Path query | walk indices + check bits | no allocation |
| Pattern match | composed bitmask AND/OR | SIMD instructions |
| Aggregate | POPCOUNT | 1 instruction per 64 nodes |
| Result set | bitmask | no allocation, IS the result |
| Index | bitmask per view | built once, O(0) query |
| Join | bm_and | 1 instruction per 256 nodes |
| Union | bm_or | 1 instruction per 256 nodes |

```
Neo4j:   MATCH (d:Def)-[:BINDS]->(f:Fn) WHERE f.pure RETURN count(d)
genera:  bm_and(t, v_def.m, v_fn.m, mw); bm_and(t, t, v_pure.m, mw); bm_pop(t, mw)
         // 3 SIMD instructions. No allocation. No strings. No hash lookups.
```

### GCC Passes = Mutable Graph Transforms. genera Views = Immutable Graph Queries.

GCC's passes walk a pointer-chased IR, compute analysis results into hash maps and
side tables, then REWRITE the IR for the next pass. 4 IRs, 3 translations, each
destructive. After CCP, the original GIMPLE is gone.

genera's views annotate the SAME GNode array with bitmasks. Nothing is rewritten.
Nothing is lost. The source bytes, the GNodes, the views, and the machine code all
coexist. Every layer is an annotation on the layer below. Bidirectional: source ↔
GNode ↔ view ↔ machine code.

The optimization decisions are identical. "This is constant — emit immediate."
"This is dead — skip." "This is tail — emit jump." GCC expresses these by
rewriting IR. genera expresses them as bits that the emitter reads.

### Universal Compiler, Not "Simpler Language"

genera is not "a Lisp compiler that skips GCC's passes because Lisp is simple."
It is a universal compiler: `lang_lisp()`, `lang_c()`, `lang_bf()` — 270-byte
Lang spec → parse ANY language into the same GNode flat array → analyze with
bitmask views → emit machine code.

For C through genera, you still need alias analysis. But it's a view:

```c
// points-to sets as bitmasks (one u64* per pointer variable)
// "does p alias q?" = do their points-to sets overlap?
bm_and(tmp, pointsto[p], pointsto[q], mw);  // non-zero → alias
// Same mechanism as V_PURE. Same SIMD. Same L1.
```

View count scales with language complexity, not with the mechanism:

| Language | View passes | Views | L1 footprint (10K nodes) |
|----------|------------|-------|--------------------------|
| Brainfuck | ~1 | ~3 | ~4 KB |
| Lisp | ~3 | ~11 | ~14 KB |
| C | ~10 | ~30 | ~38 KB |
| C++ | ~15 | ~50 | ~63 KB |

All fit in L1. All SIMD-queryable. All iterate to fixed point in microseconds.

GCC's ~386 passes exist not because C needs 386 analyses, but because each
analysis is implemented as an IR walk + hash map + IR rewrite. genera does the
same analyses as bitmask operations on flat arrays.

**Same analysis. Different representation. 100-5000x faster per iteration.**

### Why genera Can Match or Beat GCC's Optimization Quality

**1. More iteration, faster.**

```
10K node function:
  View:  1.25 KB (fits L1)
  One pass: 40 AVX2 instructions = ~10 ns
  10 iterations to fixed point: ~100 ns

GCC's CCP on same function: ~100 μs per run × 5 = ~500 μs
genera: 5000x faster per iteration, can afford 500 iterations where GCC does 5
```

More iterations = more precise fixed point = better constants propagated.

**2. Cross-domain queries are free.**

GCC: CCP doesn't see VRP results during its run. Each pass is isolated.
genera: V_CONST can read V_INT in the same cache line. Compose any views any time.

**3. Whole-program in L1.**

Typical genera program: 1K-10K nodes. Views: 25 KB total. All in L1.
GCC works function-at-a-time because its IR is too large for cache.
genera does whole-program analysis at L1 speed.

**4. Incremental.**

Edit one function → reparse its GNodes → invalidate affected views → recompute.
GCC can't do this — IR mutations cascade through the entire pipeline.

---

## GCC: 386 Pass Invocations, 297 Unique

From `ref/gcc/gcc/passes.def` (577 lines, the canonical pass schedule):

### The Five Pass Lists

| # | List | Passes | IR | Scope |
|---|------|--------|----|-------|
| 1 | `all_lowering_passes` | 15 | GIMPLE | Per-function |
| 2 | `all_small_ipa_passes` | 74 | GIMPLE SSA | Per-function + early IPA |
| 3 | `all_regular_ipa_passes` | 19 | GIMPLE SSA | Whole-program |
| 4 | `all_late_ipa_passes` | 2 | GIMPLE SSA | Post-partition |
| 5 | `all_passes` | 276 | GIMPLE SSA → RTL | Per-function |
| | **Total** | **386** | | |

### Four IRs in Sequence

```
Source → Frontend → GENERIC (lang-independent AST)
  → Gimplifier → GIMPLE (3-address, high-level)
    → pass_build_ssa → GIMPLE SSA (single static assignment)
      → [~250 GIMPLE passes]
        → pass_expand → RTL (register transfer language)
          → [~97 RTL passes]
            → pass_final → Assembly
```

Each IR is a different language. Each transition is a lossy translation. Once you're in RTL, the GIMPLE is gone. Once you're in assembly, the RTL is gone.

### Pass Types (C++ classes in `tree-pass.h`)

| Class | Scope | When |
|-------|-------|------|
| `gimple_opt_pass` | Per-function | On GIMPLE/SSA trees |
| `rtl_opt_pass` | Per-function | On RTL |
| `simple_ipa_opt_pass` | Whole-program, single stage | Before/after WPA |
| `ipa_opt_pass_d` | Whole-program, multi-stage (summary/write/read/transform) | LTO-aware |

### Most-Invoked Passes (why they repeat)

| Pass | Runs | Why repeated |
|------|------|-------------|
| `pass_dce` (dead code elimination) | 8 | Every optimization creates new dead code |
| `pass_fre` (full redundancy elimination) | 5 | Each transform exposes new redundancies |
| `pass_forwprop` (forward propagation) | 5 | New single-use values after each pass |
| `pass_dse` (dead store elimination) | 5 | Stores become dead after DCE/copy-prop |
| `pass_ccp` (conditional constant prop) | 5 | New constants exposed after inlining |
| `pass_copy_prop` | 4 | Threading/dominator create copies |
| `pass_lim` (loop invariant motion) | 4 | New invariants after each loop transform |
| `pass_phiopt` (PHI optimization) | 4 | New PHI patterns after each SSA pass |

The pattern: **A exposes opportunity for B, B exposes opportunity for A.** So you run A-B-A-B. GCC's pass ordering is empirically tuned over decades.

---

## Phase 1: Lowering (15 passes)

Lower high-level GIMPLE constructs to forms the optimizer can handle. Always runs first.

| Pass | Source file | What it does |
|------|-----------|-------------|
| `pass_warn_unused_result` | `tree-cfg.cc` | Warn on ignored `[[nodiscard]]` returns |
| `pass_diagnose_omp_blocks` | `omp-low.cc` | Validate OpenMP block structure |
| `pass_diagnose_tm_blocks` | `trans-mem.cc` | Validate transactional memory blocks |
| `pass_omp_oacc_kernels_decompose` | `omp-oacc-kernels-decompose.cc` | Decompose OpenACC kernels |
| `pass_lower_omp` | `omp-low.cc` | Lower OpenMP → explicit control flow |
| `pass_lower_cf` | `gimple-low.cc` | Lower control flow constructs |
| `pass_lower_tm` | `trans-mem.cc` | Lower transactional memory constructs |
| `pass_refactor_eh` | `tree-eh.cc` | Refactor exception handling edges |
| `pass_lower_eh` | `tree-eh.cc` | TRY_FINALLY/TRY_CATCH → explicit flow |
| `pass_coroutine_lower_builtins` | `coroutine-passes.cc` | Lower C++20 coroutine builtins |
| `pass_build_cfg` | `tree-cfg.cc` | Decompose function into basic blocks + edges |
| `pass_warn_function_return` | `tree-cfg.cc` | Warn on missing return |
| `pass_coroutine_early_expand_ifns` | `coroutine-passes.cc` | Expand coroutine internal fns |
| `pass_expand_omp` | `omp-expand.cc` | Expand remaining OpenMP |
| `pass_build_cgraph_edges` | `cgraphbuild.cc` | Build call graph for IPA |

**genera equivalent:** `gram_parse()` directly produces the GNode tree. Language-specific
lowering (if the Lang has exceptions, OMP, etc.) would be additional view passes over
the same GNodes — not IR rewrites. For Lisp: 0 lowering views. For C: V_EH, V_CFG_EDGE
etc. Same mechanism, more views.

---

## Phase 2: Small IPA + Early Opts (74 passes)

Build SSA, run early per-function optimizations, prepare for IPA.

### SSA Construction (`pass_build_ssa_passes`, 10 passes)

| Pass | What |
|------|------|
| `pass_fixup_cfg` | Fix CFG after lowering |
| `pass_build_ssa` | **Convert to SSA form** — each variable assigned exactly once |
| `pass_walloca` | Warn on problematic `alloca()` |
| `pass_warn_printf` | Validate printf format strings |
| `pass_warn_nonnull_compare` | Warn on null comparisons |
| `pass_early_warn_uninitialized` | First uninit detection |
| `pass_warn_access` | Bounds/dangling pointer warnings |
| `pass_ubsan` | Instrument for UB sanitizer |
| `pass_nothrow` | Mark nothrow functions |
| `pass_rebuild_cgraph_edges` | Refresh call graph |

**genera equivalent:** For Lisp: `let` bindings ARE SSA. For C: SSA construction is a
view pass — V_DEF + V_USE + reaching_def[] as parallel bitmask arrays. Same mechanism,
~60 lines. GCC's `pass_build_ssa`: ~3,000 lines (tree-into-ssa.cc).

### Early Optimizations (`pass_all_early_optimizations`, 20 passes)

| Pass | What | genera? |
|------|------|---------|
| `pass_ccp` | Conditional constant propagation | **V_CONST view** |
| `pass_forwprop` | Forward propagation | Implicit (no mutable vars) |
| `pass_early_thread_jumps` | Jump threading | N/A (no gotos) |
| `pass_sra_early` | Scalar replacement of aggregates | N/A (no structs) |
| `pass_phiprop` | Propagate through PHI nodes | N/A (no PHI, no assignment) |
| `pass_fre` | Full redundancy elimination | Implicit (pure functions) |
| `pass_early_vrp` | Value range propagation | **V_INT view** |
| `pass_merge_phi` | Merge PHI nodes | N/A |
| `pass_dse` | Dead store elimination | N/A (no mutable stores) |
| `pass_cd_dce` | Dead code elimination | **V_DEAD view** |
| `pass_phiopt` | PHI → conditional | N/A |
| `pass_cleanup_eh` | Clean empty EH | N/A |
| `pass_tail_recursion` | Tail recursion → loop | **V_TAIL view → recur form** |
| `pass_if_to_switch` | Convert if chains to switch | Future: pattern matching |
| `pass_convert_switch` | Canonicalize switch | N/A |
| `pass_profile` | Branch probabilities | Future: runtime views |
| `pass_local_pure_const` | Detect pure/const functions | **V_PURE view** |
| `pass_modref` | Track memory side effects | Implicit (immutable) |
| `pass_split_functions` | Split cold code | Future |

**genera equivalent:** 6 of these 20 map directly to GLASS views, computed in
passes 1-3 (~240 lines total for Lisp). For C input, most of the other 14 become
additional views: V_MUTABLE, V_ADDRESSOF, V_AGGREGATE, V_PHI — same bitmask mechanism,
~20-40 lines each. The mechanism scales; the per-analysis cost stays ~20 lines.

### IPA Setup (remaining ~44 passes in small_ipa)

Inlining decisions, profile-guided opt, visibility analysis, transactional memory. Most are N/A for genera's single-image model.

---

## Phase 3: Regular IPA (19 passes)

Whole-program interprocedural optimization. The heavyweight cross-function analysis.

| Pass | Source | What | genera? |
|------|--------|------|---------|
| `pass_analyzer` | `analyzer/` (~120K lines) | `-fanalyzer` static analysis | Future: genera analyzer |
| `pass_ipa_whole_program_visibility` | `ipa-visibility.cc` | What's visible outside CU | N/A (single image) |
| `pass_ipa_profile` | `ipa-profile.cc` | Profile-guided | Future: runtime views |
| `pass_ipa_icf` | `ipa-icf.cc` | Identical code folding | Future: interned fns |
| `pass_ipa_devirt` | `ipa-devirt.cc` | Devirtualize calls | N/A (no vtables) |
| `pass_ipa_cdtor_merge` | `ipa-cdtor-merge.cc` | Merge ctors/dtors | N/A |
| `pass_ipa_cp` | `ipa-cp.cc` | Interprocedural constant prop | **V_CONST across fns** |
| `pass_ipa_sra` | `ipa-sra.cc` | Interprocedural scalar replace | N/A (no aggregates) |
| `pass_ipa_fn_summary` | `ipa-fnsummary.cc` | Compute inline costs | image.c call graph |
| `pass_ipa_inline` | `ipa-inline.cc` | **THE inliner** | Future: inline view |
| `pass_ipa_pure_const` | `ipa-pure-const.cc` | Detect pure/const | **V_PURE transitively** |
| `pass_ipa_modref` | `ipa-modref.cc` | Track reads/writes | Implicit |
| `pass_ipa_reference` | `ipa-reference.cc` | Whole-program refs | V_REF + bind[] |
| `pass_ipa_single_use` | `ipa-utils.cc` | Single-use functions | bm_pop(V_CALL) == 1 |
| `pass_ipa_comdats` | `ipa-comdats.cc` | COMDAT privatization | N/A |

**genera equivalent:** image.c already does call graph analysis. V_PURE + V_CONST propagate interprocedurally through `bind[]`. The bitmask query `bm_and(v_call.m, v_pure.m)` = "pure calls" is one instruction vs ipa-pure-const.cc (2,500 lines).

---

## Phase 4: GIMPLE Optimization (123 passes within `pass_all_optimizations`)

The bulk of per-function optimization. Grouped by category:

### Scalar Dataflow (~40 passes)

| Category | Passes | Total runs | What |
|----------|--------|-----------|------|
| Constant propagation | `pass_ccp` | 5 | Fold constants, propagate |
| Redundancy elimination | `pass_fre` | 5 | Value numbering, CSE |
| Dead code | `pass_dce`, `pass_cd_dce` | 8+3 | Remove dead code |
| Dead stores | `pass_dse` | 5 | Remove dead writes |
| Copy propagation | `pass_copy_prop` | 4 | Replace copies with originals |
| Forward propagation | `pass_forwprop` | 5 | Substitute single-use defs |
| Value range | `pass_vrp` | 2 | Track `x ∈ [lo,hi]` |
| Dominator opts | `pass_dominator` | 3 | Combined const/copy/thread |
| PHI optimization | `pass_phiopt` | 4 | PHI → cmov/cselect |
| Reassociation | `pass_reassoc` | 2 | Rewrite `(a+b)+c` → `a+(b+c)` |

**genera equivalent:** V_CONST (constant fold), V_DEAD (dead code), V_INT (type specialization). No copies, no stores, no PHI nodes, no aliasing. **~3 views replace ~40 passes.**

### Loop Optimization (~30 passes within `pass_tree_loop`)

| Pass | What | Lines |
|------|------|-------|
| `pass_tree_unswitch` | Hoist invariant conditions out | tree-ssa-loop-unswitch.cc |
| `pass_loop_split` | Split loop on condition | tree-ssa-loop-split.cc |
| `pass_scev_cprop` | Scalar evolution + const prop | tree-scalar-evolution.cc |
| `pass_loop_versioning` | Specialize loops | tree-loop-versioning.cc |
| `pass_loop_jam` | Fuse adjacent loops | tree-loop-jam.cc |
| `pass_iv_canon` | Canonicalize induction vars | tree-ssa-loop-ivcanon.cc |
| `pass_loop_distribution` | Split loop into parts | tree-loop-distribution.cc |
| `pass_crc_optimization` | Recognize CRC patterns | gimple-crc-optimization.cc |
| `pass_linterchange` | Swap loop nesting | tree-loop-interchange.cc |
| `pass_graphite` | Polyhedral transforms | graphite*.cc |
| `pass_parallelize_loops` | Auto-parallelize | tree-parloops.cc |
| `pass_if_conversion` | if→branchless (for vectorizer) | tree-if-conv.cc |
| `pass_vectorize` | **Auto-vectorization** | tree-vect-*.cc (~60K lines) |
| `pass_slp_vectorize` | Superword-level parallelism | tree-vect-slp.cc |
| `pass_predcom` | Predictive commoning | tree-predcom.cc |
| `pass_complete_unroll` | Fully unroll small loops | tree-ssa-loop-ivcanon.cc |
| `pass_loop_prefetch` | Insert prefetch instructions | tree-ssa-loop-prefetch.cc |
| `pass_iv_optimize` | Minimize IV cost | tree-ssa-loop-ivopts.cc |
| `pass_lim` | Loop invariant motion | tree-ssa-loop-im.cc |

**genera equivalent (Lisp):** `loop`/`recur` = tail jump. `map`/`reduce` = function calls.
**genera equivalent (C):** Loop analysis as views — V_LOOP_HEADER, V_BACK_EDGE,
V_INDUCTION_VAR, V_LOOP_INVARIANT. Each ~30 lines. Vectorization: V_VECTORIZABLE =
loop body fits SIMD width AND no cross-iteration deps (bitmask check). Unrolling:
emit N copies when V_SMALL_BODY AND V_TRIP_COUNT_KNOWN. Same GNode array, more views.

### Algebraic / Strength Reduction (~10 passes)

| Pass | What |
|------|------|
| `pass_backprop` | Backward propagation of unused bits |
| `pass_expand_pow` | `pow(x,3)` → `x*x*x` |
| `pass_optimize_bswap` | Recognize byte-swap patterns |
| `pass_cse_sincos` | `sin(x); cos(x)` → `sincos(x)` |
| `pass_cse_reciprocals` | `a/x; b/x` → `r=1/x; a*r; b*r` |
| `pass_optimize_widening_mul` | Widening multiply patterns |
| `pass_strength_reduction` | Replace expensive ops with cheap |
| `pass_store_merging` | Merge adjacent stores |

**genera equivalent:** For Lisp, numeric builtins map to machine instructions directly.
For C: algebraic identities as pattern-match on GNode subtrees + const_val[].
`pow(x,3)` → check V_CONST on arg2, value==3 → emit `x*x*x`. `x*8` → check
V_CONST, value is power-of-2 → emit shift. Same as GCC's logic, but the
const-check is `BM_GET(v_const.m, arg2) && const_val[arg2] == 3`. O(0).

### Sanitizers (~6 passes)

`pass_asan`, `pass_tsan`, `pass_ubsan`, `pass_sancov`, `pass_sanopt`. Instrument for runtime checking.

**genera equivalent (Lisp runtime):** Arena = no use-after-free. Immutable = no races.
NaN-boxing = no type confusion. Built into runtime.
**genera equivalent (compiling C):** Sanitizer instrumentation as view-driven emission.
V_ADDRESSOF nodes get bounds metadata. V_DEREF nodes get bounds checks at emit.
Same analysis GCC does, expressed as views instead of IR instrumentation passes.

---

## Phase 5: RTL Optimization (97 passes within `pass_rest_of_compilation`)

### The Boundary: `pass_expand` (GIMPLE SSA → RTL)

~55,000 lines in `cfgexpand.cc`. Converts SSA into register-transfer-language instructions. After this, everything is machine-oriented.

### RTL Pass Categories

| Category | Key passes | What |
|----------|-----------|------|
| **CSE** | `pass_cse`, `pass_cse2`, `pass_gcse2` | Common subexpression elimination on RTL |
| **Propagation** | `pass_rtl_cprop` ×3, `pass_rtl_fwprop` | Copy/forward prop at machine level |
| **Motion** | `pass_rtl_pre`, `pass_rtl_hoist`, `pass_rtl_store_motion` | Code hoisting/sinking on RTL |
| **Combine** | `pass_combine`, `pass_late_combine` | **Merge 2-3 insns into 1** using machine desc |
| **Loops** | `pass_rtl_move_loop_invariants`, `pass_rtl_unroll_loops`, `pass_rtl_doloop` | RTL-level loop opts |
| **Scheduling** | `pass_sched`, `pass_sched2`, `pass_sms` | Instruction scheduling (pre/post RA) |
| **Regalloc** | `pass_ira`, `pass_reload` | **Graph-coloring register allocator** |
| **Post-RA** | `pass_peephole2`, `pass_regrename`, `pass_cprop_hardreg`, `pass_fast_rtl_dce` | Clean up after regalloc |
| **Layout** | `pass_reorder_blocks`, `pass_duplicate_computed_gotos`, `pass_split_crit_edges` | Block ordering for cache |
| **Frame** | `pass_thread_prologue_and_epilogue`, `pass_stack_adjustments` | Prologue/epilogue |
| **Final** | `pass_shorten_branches`, `pass_dwarf2_frame`, `pass_final` | Emit assembly + debug info |

**genera equivalent:** Currently `jit.c` does TCC-style direct emission (3 regs + spill).

The RTL passes map to views + view-driven emission:
- **CSE/redundancy:** V_REDUNDANT = expression already computed. Emit load instead of recompute.
- **Combine:** Pattern table at emit time. `load+add+store` → `add [mem]`. ~50 lines.
- **Regalloc:** Liveness as bitmask per value. Interference = `bm_and`. Linear scan. ~200 lines.
- **Scheduling:** Dependency bitmask per node. List scheduler. ~100 lines.
- **Peephole:** Post-emit pattern scan on flat code buffer. ~50 lines.

Total: ~400 lines. GCC's RTL pipeline: 97 passes, ~50K lines.
Same optimization decisions. Bitmask representation instead of RTL IR.

---

## TCC: 1 Fused Pass (the other extreme)

From `ref/tcc/` (64K lines total, 27K lines core):

### Architecture

```c
// libtcc.c:794 — THE compilation entry point
static int tcc_compile(TCCState *s1, ...) {
    preprocess_start(s1, filetype);  // set up preprocessor
    tccgen_init(s1);                 // init codegen
    tccelf_begin_file(s1);           // set up ELF sections
    tccgen_compile(s1);              // ← THE ONE PASS
    tccelf_end_file(s1);             // finalize ELF
    tccgen_finish(s1);               // cleanup
}

// tccgen.c:405 — parse + emit simultaneously
ST_FUNC int tccgen_compile(TCCState *s1) {
    next();              // get first token from preprocessor
    decl(VT_CONST);      // recursive descent: parse + emit machine code
    gen_inline_functions(s1);  // deferred inline bodies
    return 0;
}
```

### Key Design Decisions

| Decision | Consequence |
|----------|-----------|
| No AST | Can't analyze after parsing |
| No IR | Can't optimize between representation levels |
| No SSA | Can't do dataflow analysis |
| Value stack only (`SValue vstack[8]`) | 3 register budget, spill on overflow |
| Forward refs via backpatching | Single pass possible |
| Built-in linker + assembler | No external tools needed |

### TCC Source Map

| File | Lines | Role |
|------|-------|------|
| `tccgen.c` | 8,920 | Parser + codegen (THE compiler) |
| `tccpp.c` | 4,005 | Preprocessor (interleaved with parse) |
| `tccelf.c` | 4,109 | ELF linker |
| `tccdbg.c` | 2,676 | Debug info |
| `x86_64-gen.c` | 2,313 | x86-64 backend |
| `libtcc.c` | 2,272 | Driver + orchestration |
| `tccasm.c` | 1,466 | Assembler |
| `tccrun.c` | 1,556 | JIT execution (`-run` mode) |

### TCC's Optimizations (total)

1. Constant folding: `3 + 4` → `7`
2. Strength reduction: `x * 8` → `x << 3`
3. Comparison flag caching

That's it. No DCE, no CSE, no inlining, no vectorization, no loop opts.

---

## The Spectrum

```
TCC             genera/GLASS          GCC
1 pass          ~3 view passes        386 IR-rewrite passes
0 IRs           1 (GNodes, immutable) 4 IRs (destructive transforms)
0 analysis      graph db queries      hash maps + pointer walks
64K lines       ~4K lines             1.3M lines
~100 KB binary  ~128 KB binary        ~100 MB binary
~30 ns/line     ~50 ns/line (est)     ~1200 ns/line (-O2)
2-5x slower     target: ~1.2x         baseline runtime
```

### Why genera's ~3 Passes Can Match GCC's ~386

**Two advantages compound:**

1. **Language semantics guarantee what GCC must prove** — eliminates ~250 passes
2. **Bitmask queries on flat arrays** — does the remaining work 100-5000x faster per iteration

**Language guarantees:**

| GCC problem | Why it exists | genera answer |
|-------------|--------------|---------------|
| SSA construction | C has mutable variables | `let` bindings ARE SSA |
| Alias analysis | Pointers alias arbitrarily | No pointers. Immutable data. |
| Dead store elim | Stores can be overwritten | No mutable stores |
| Exception handling | C++ exceptions, setjmp | Condition/restart via sig |
| OMP/TM/coroutines | Language features | Not needed |
| Scalar replacement | Struct fields accessed individually | No structs (pmap keys) |
| Loop optimization | C loops have complex semantics | `loop`/`recur` = tail jump |
| Register allocation | Unlimited SSA → limited regs | Direct emission (like TCC) |
| Scheduling | Pipeline hazards, latency | Future (~100 lines) |
| PHI optimization | SSA merge points | No merge (immutable) |
| Copy propagation | Copies from SSA lowering | No copies (values are values) |
| Sanitizers | C memory unsafety | Built into runtime |

**What remains (the ~3 view passes):**

| Pass | Produces | GCC equivalent | Lines |
|------|----------|---------------|-------|
| **Pass 1: Scope + Binding** | V_DEF, V_REF, bind[], scope[] | `build_ssa` + `ipa_reference` + `local_pure_const` | ~80 |
| **Pass 2: Type + Purity** | V_INT, V_VEC, V_MAP, V_FN, V_PURE, V_CONST | `ccp` ×5 + `vrp` ×2 + `ipa_pure_const` + `modref` | ~100 |
| **Pass 3: Flow + Dead** | V_TAIL, V_DEAD | `dce` ×8 + `cd_dce` ×3 + `tail_recursion` | ~60 |
| **Total** | 11 views, ~25 KB for 10K nodes (L1) | **~30 GCC passes, ~60K lines** | **~240** |

But "3 passes" is misleading. Each pass can iterate to fixed point in microseconds
(40 AVX2 ops per pass on a 10K function). So genera effectively runs ~30-50
"pass equivalents" in the time GCC runs 5. The views refine each other:

```
V_CONST₁ → V_DEAD₁ → V_CONST₂ (nodes constant because dead branch removed)
         → V_DEAD₂ (more dead because more constant) → ... → fixed point
```

Where GCC's ~386 passes go:
1. ~286: same analysis, expressed as bitmask views (~20 lines each vs ~2000 lines each)
2. ~60: IR bookkeeping (rebuild_cgraph, fixup_cfg, release_ssa) — not needed when IR is immutable
3. ~40: container passes (pass_all_optimizations, pass_tree_loop, etc.) — scheduling, not analysis

### The Key Insight

GCC's 386 passes don't exist because C needs 386 different analyses. They exist
because each analysis is **an IR walk + hash map + IR rewrite**, and the rewrite
invalidates previous results, forcing re-analysis. The pass count is an artifact
of the representation, not the problem.

genera does the same analyses. The representation is different:

| Analysis | GCC mechanism | genera mechanism |
|----------|--------------|-----------------|
| "Is this pure?" | Walk IR, build side-effect summary, store in hash map. 2,500 lines. | Bottom-up bitmask pass. V_PURE. ~20 lines. |
| "Is this constant?" | Walk IR, lattice meet, replace uses, iterate. 3,200 lines, runs 5×. | Bottom-up bitmask pass. V_CONST. ~20 lines. Iterate 50× in same time. |
| "Is this dead?" | Walk IR, mark live, delete dead. 1,800 lines, runs 8×. | Top-down bitmask pass. V_DEAD. ~15 lines. |
| "Does p alias q?" | Build points-to graph, solve constraints. 8,000 lines. | `bm_and(pointsto[p], pointsto[q], mw)`. ~60 lines for the pass. |
| "Is this a tail call?" | Pattern-match call in return position. 1,800 lines. | Top-down mark last-in-body. V_TAIL. ~15 lines. |

The line counts differ 50-100x not because genera skips the analysis,
but because the bitmask representation makes each analysis trivial to express.
AND, OR, POPCOUNT — three operations, complete for all set queries.

For Lisp input: ~3 passes, ~11 views, ~240 lines.
For C input: ~10 passes, ~30 views, ~800 lines (est).
For C++ input: ~15 passes, ~50 views, ~1,500 lines (est).
GCC for C: ~386 passes, 4 IRs, ~60,000 lines of pass code.

The compression ratio holds across languages because it comes from the
REPRESENTATION (bitmasks on flat arrays vs pointer-chased IR rewrites),
not from the LANGUAGE being compiled.

---

## Mapping to genera Views (for `grammar.c` / `view.c`)

### Current Views (already in grammar.c)

```c
// Structural (from parse)
g->m[NK_LIST]    g->m[NK_IDENT]    g->m[NK_NUM]    g->m[NK_STR_NODE]
g->m[NK_VEC]     g->m[NK_MAP]      g->m[NK_KW]     g->m[NK_OP]
g->m_group       g->m_leaf         g->m_first

// Semantic (from analysis passes)
V_DEF    V_REF    V_CALL    V_TAIL
V_PURE   V_CONST  V_DEAD    V_INT
V_VEC    V_MAP    V_FN
```

### Mapping GCC Passes → genera Views

| GCC pass family | genera view | How |
|----------------|-------------|-----|
| CCP (constant propagation) | `V_CONST` | Bottom-up: literal + pure op + const args |
| VRP (value range) | `V_INT` | Bottom-up: int literal, int op on int args |
| DCE (dead code) | `V_DEAD` | Top-down: const-false branch, after-return |
| Purity analysis | `V_PURE` | Bottom-up: pure builtin + pure args |
| Tail call detection | `V_TAIL` | Top-down: last position in body/if/let/do |
| Def/use analysis | `V_DEF`, `V_REF`, `bind[]` | Walk: binding forms → def, references → ref |
| Type inference | `V_INT`, `V_VEC`, `V_MAP`, `V_FN` | Bottom-up: literal type + operator return type |
| Call graph | `V_CALL` + `image_calls()` | `bm_and(m[NK_LIST], m_first_is_sym)` |
| Inline candidates | `bm_and(V_PURE, small)` | Future: size estimate per V_CALL site |
| Hot code | runtime `tap_to_bitmask()` | Observation → bitmask, same shape as views |

### Composed Queries (what GCC needs multi-pass for)

```c
// "pure tail calls" — GCC needs pass_tail_recursion + pass_local_pure_const
bm_and(tmp, v_tail.m, v_pure.m, mw);

// "dead constant expressions" — GCC needs pass_dce + pass_ccp
bm_and(tmp, v_dead.m, v_const.m, mw);

// "type-specialized hot calls" — GCC needs VRP + PGO + devirt
bm_and(tmp, v_int.m, hot_mask, mw);
bm_and(tmp, tmp, v_call.m, mw);

// "inlinable pure functions called once"
// GCC: ipa-inline.cc + ipa-pure-const.cc + ipa-utils.cc (~10K lines)
for (u32 id = bm_next(v_call.m, 0, mw); id < g->n; id = bm_next(v_call.m, id+1, mw)) {
    StrId fn = gn_intern(g, gn_child(g, id));
    if (BM_GET(v_pure.m, bind[id]) && call_count[fn] == 1) { /* inline */ }
}
```

---

## GCC Pass Schedule as Pseudocode

For reference when building genera's emission pipeline. Simplified from `passes.def`:

```
LOWER:
  lower_omp → lower_eh → lower_tm → build_cfg → build_cgraph

BUILD SSA:
  fixup_cfg → build_ssa → warnings → ubsan

EARLY OPTS (run once):
  ccp → forwprop → thread_jumps → sra → phiprop → fre →
  early_vrp → merge_phi → dse → cd_dce → phiopt →
  cleanup_eh → tail_recursion → if_to_switch →
  profile → local_pure_const → split_functions

IPA:
  analyzer → icf → devirt → cp → sra → fn_summary →
  INLINE → pure_const → modref → reference

MAIN OPTS (most run 2-5 times):
  ccp → object_sizes → forwprop → build_alias → fre →
  thread_jumps → vrp → dse → dce → stdarg → cselim →
  copy_prop → ifcombine → phiopt → tail_recursion →
  ch → lower_complex → sra → thread_jumps → dominator →
  copy_prop → isolate_erroneous → reassoc → dce →
  forwprop → phiopt → ccp → bswap → lim → pre →
  sink → asan → tsan → dse → dce

LOOPS:
  unswitch → split → scev_cprop → versioning → jam →
  iv_canon → distribution → interchange → graphite →
  parallelize → if_conversion → VECTORIZE → predcom →
  unroll → slp_vectorize → prefetch → iv_optimize → lim

POST-LOOP:
  lower_vector → lower_switch → sincos → reciprocals →
  reassoc → strength_reduction → split_paths → tracer →
  fre → thread_jumps → dominator → strlen → vrp →
  ccp → dse → dce → forwprop → sink → phiopt →
  store_merging → cd_dce → tail_calls → uncprop

EXPAND: GIMPLE SSA → RTL

RTL EARLY:
  virtual_regs → jump → lower_subreg → cse → fwprop →
  cprop → pre → hoist → cprop → store_motion → cse →
  ifcvt

RTL LOOPS:
  loop_invariants → unroll → doloop

PRE-RA:
  lower_subreg → web → cprop → cse2 → dse → fwprop →
  inc_dec → ud_dce → ext_dce → COMBINE → late_combine →
  if_after_combine → split_insns → lower_subreg

SCHEDULE + RA:
  mode_switching → sms → live_range_shrinkage → SCHED →
  early_remat → IRA → RELOAD

POST-RA:
  cse → late_combine → gcse → split → ree →
  compare_elim → prologue_epilogue → dse →
  stack_adjustments → sched_fusion → PEEPHOLE2 →
  if_after_reload → regrename → fold_mem → cprop →
  fast_dce → reorder_blocks → SCHED2 → stack_regs

FINAL:
  zero_regs → alignments → variable_tracking → free_cfg →
  machine_reorg → delay_slots → shorten_branches →
  dwarf2_frame → FINAL (emit .s)
```

---

## For genera: What To Build Next

**Current state:** Passes 1-3 exist in grammar.c. Views drive jit.c emission.

**Missing pieces — all expressible as views or view-driven emission:**

| Feature | GCC equiv | genera approach | Lines | Why fast |
|---------|-----------|----------------|-------|----------|
| Inlining | `ipa_inline` (5K) | V_INLINE = small AND pure AND called-once. Copy GNodes at emit. | ~100 | View query decides; emit copies nodes |
| Regalloc | `ira` (15K) | Liveness as bitmask per value. Interference = bm_and(live_a, live_b). Linear scan. | ~200 | SIMD interference graph |
| Scheduling | `sched` (8K) | Dependency = bitmask per node. Critical path = bm_pop chain. List scheduler. | ~100 | SIMD dependency check |
| Peephole | `peephole2` (2K) | Pattern table: sequence → replacement. Post-emit scan. | ~50 | Flat code buffer |
| Const fold at emit | `ccp` at RTL | Read const_val[id] at emit time | ~30 | Already computed |
| Escape analysis | `ipa_pta` (8K) | V_ESCAPE: closures whose refs leave scope. Stack-alloc non-escaping. | ~60 | Bitmask over bind[] |
| Unboxing | `sra` (5K) | V_UNBOXED = V_INT AND all uses expect int. Skip NaN-box/unbox. | ~40 | View query at emit |

Total: ~580 lines. GCC equivalent: ~48K lines. Ratio: ~80x.

But the real gain is iteration speed. Every one of these "features" is a view computation
that fits in L1 and runs at SIMD width. Add a view, iterate all views to fixed point,
emit. The fixed point IS the optimization. The views ARE the passes. The GNode array
IS the AST. And it's all bits — zero strings after the parse boundary.

```
Source bytes → intern → GNodes (immutable flat array)
                          ↕
                   Views (bitmask annotations, L1-resident)
                          ↕
                   Machine code (emitter reads GNodes + views)
                          ↕
                   Runtime observations (same bitmask shape)
```

Every layer annotates. Nothing transforms. Nothing is lost. Every bit traces from
register back to source character. The system doesn't have a debugger. The system
IS a debugger. It doesn't have an optimizer. The views ARE the optimizer.

The graph database IS the compiler IS the debugger IS the profiler.
One representation. Bits all the way down.
