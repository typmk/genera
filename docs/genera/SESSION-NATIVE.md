# Native Runtime Session

## What We Built

Clojure-to-native compilation via freestanding C. No libc. Raw syscalls. 128KB static binary. Single file build.

```
src/c/moss.c → glass/ → emit/ → lang/ → platform/ → base/
```

**Build:** `gcc -O3 -march=native -nostdlib -static -o moss src/c/moss.c`

## Architecture

### Dependency Graph (File Pilot pattern)

```
moss.c                          Entry point (39 lines)
  └─ glass/glass.c              DX layer manifest
       ├─ glass/test.c          175 self-tests
       ├─ glass/bench.c         9 benchmark suites
       ├─ glass/repl.c          12 REPL commands
       ├─ glass/cli.c           CLI dispatch (7 modes)
       └─ emit/emit.c           Code generation manifest
            ├─ emit/x86.c       x86-64 encoding primitives
            ├─ emit/jit.c       JIT compiler + regalloc
            ├─ emit/cc.c        C emitter + gcc driver
            └─ lang/lang.c      Language processing manifest
                 ├─ lang/grammar.c  Universal grammar + bitmask queries
                 ├─ lang/read.c     S-expr reader + symbols + classify
                 ├─ lang/eval.c     Tree-walking eval + builtins
                 ├─ lang/image.c    Program database (call graph)
                 └─ platform/platform.c  Runtime primitives manifest
                      ├─ platform/proto.c  Cons + protocol dispatch (3 tiers)
                      ├─ platform/coll.c   pmap (HAMT), pvec, atom
                      └─ base/base.c       Foundation manifest
                           ├─ base/sys.c   Types + syscalls + _start
                           ├─ base/fmt.c   OutBuf, pf(), macros
                           ├─ base/mem.c   Arena (blocks, alloc, reset)
                           ├─ base/str.c   Str + Intern + StrBuild
                           ├─ base/val.c   NaN boxing (12 types)
                           ├─ base/arr.c   DynArray + HashMap
                           ├─ base/tap.c   Trace ring buffer, TAP macro
                           └─ base/cmd.c   Command registry, REPL loop
```

### File Map

| Layer | File | Lines | Purpose |
|-------|------|-------|---------|
| **base/** | sys.c | 326 | Types, syscalls, `_start` |
| | fmt.c | 152 | OutBuf, `pf()`, LIKELY/hash32 |
| | mem.c | 96 | Arena (blocks, alloc, reset, temp) |
| | str.c | 176 | Str + Intern + StrBuild |
| | val.c | 69 | NaN boxing (12 types in 64 bits) |
| | arr.c | 123 | DynArray + HashMap (arena-backed) |
| | tap.c | 81 | Trace ring buffer, TAP macro |
| | cmd.c | 179 | Command registry, REPL loop, init/cleanup |
| **platform/** | proto.c | 175 | Cons + Protocol dispatch (X-macros, 3 tiers) |
| | coll.c | 465 | pmap (HAMT), pvec (32-way trie), atom |
| **lang/** | grammar.c | 515 | Universal grammar + bitmask indexes + entity_to_val |
| | read.c | 242 | S-expr reader, symbols, classify |
| | eval.c | 488 | Tree-walking eval, builtins, closures |
| | image.c | 139 | Program database, call graph, entity queries |
| **emit/** | x86.c | 184 | x86-64 encoding primitives |
| | jit.c | 477 | Regalloc + expression compiler + program compiler |
| | cc.c | 395 | C source emitter + gcc driver |
| **glass/** | test.c | 655 | ALL self-tests (175 tests) |
| | bench.c | 313 | ALL benchmarks (9 suites) |
| | repl.c | 234 | Extended REPL commands (12 commands) |
| | cli.c | 101 | CLI argument dispatch (7 modes) |
| | **moss.c** | 39 | Entry point |
| | **Total** | **5,707** | **27 files** |

### Binary

```
128 KB static, no libc, x86-64 Linux
175 self-tests, 9 benchmark suites, 17 REPL commands
```

## DX: Single Binary

```bash
./moss                         # Interactive REPL (17 commands)
./moss test                    # Run ALL 175 self-tests
./moss bench                   # Run ALL benchmarks
./moss eval "(+ 1 2)"         # One-shot eval → 3
./moss jit  "(fib 35)"        # JIT compile + execute
./moss emit file.clj          # Emit C source
./moss run  file.clj          # Compile via gcc + run
./moss parse "(defn f [x] x)" # Show entity tree
```

**REPL commands:**

| Command | What |
|---------|------|
| eval / jit / reval | Execute (tree-walk / JIT / gcc) |
| parse / tree / nodes / stats | Grammar introspection |
| query | Bitmask queries (recur detection, etc.) |
| image / defns | Program database |
| tap on/off, trace N | Glass box observability |
| arena / intern | Memory introspection |
| lang lisp/c/bf | Switch grammar language |
| help | List all commands |

## Numbers

| Metric | Value |
|--------|-------|
| fib(35) JIT | 42 ms |
| fib(35) gcc -O2 | 13 ms |
| JIT/gcc ratio | 3.2x |
| compile fib | 0.6 us |
| arena_alloc(64B) | 1.2 ns |
| hashmap get (100 keys) | 1.4 ns |
| str_eq (8B match) | 0.9 ns |
| strid_eq | 0.3 ns |
| parse 196B lisp | 3961 ns (49.5 MB/s) |
| bitmask popcount | 0.8 ns |
| range query | ~0 ns |
| self-tests | 175/175 |
| binary size | 128 KB |

## Completed Features

### Register Allocation (emit/jit.c)
- Callee-saved (CS) register pool: rbx, r12-r15
- Non-TCO functions: params in CS regs (no stack spill)
- Fused compare-and-branch: `(if (< a b) ...)` → `cmp a,b; jge` (2 insns vs 5)
- 1-arg call optimization: `mov rdi,rax; call` (no push/pop)
- Generation counter: O(1) reset instead of 32KB memset (compile 5.4us → 0.6us)

### Universal Grammar (lang/grammar.c)
- One engine, any language: 256-byte char class table + delimiter pairs = lang spec
- Flat node array (24-byte GNode), pre-order indexed
- Bitmask indexes: AND/OR/POPCOUNT/range-scan for O(1) classification
- 3 language specs: `lang_bf()`, `lang_lisp()`, `lang_c()`
- Query API: `gn_child`, `gn_next`, `gn_has` (subtree range), `gn_sub_count`
- Entity → Val bridge: `entity_to_val()` (~40 lines) converts GNode → Val cons lists

### Glass Box (base/tap.c + base/cmd.c)
- TAP macro: zero-cost when off (~0.3ns branch-not-taken), logs to ring buffer when on
- Trace ring buffer: 4096 events, 24 bytes each, newest-first iteration
- Command registry: `cmd_register("name", fn, "help")` — StrId → function pointer
- REPL: line-buffered stdin, command dispatch, works piped and interactive

### Persistent Data (platform/coll.c)
- pmap: HAMT with 32-way branching, arena-allocated
- pvec: 32-way trie, tail optimization
- atom: atomic swap with CAS semantics
- All freestanding (no libc dependency)

### Protocol Dispatch (platform/proto.c)
- X-macro spec generates SoA dispatch tables
- Tier 1: table lookup (~2ns) — `P_first[type](val)`
- Tier 2: fast-path (~0.1ns) — `p_first(val)` with type check
- Tier 3: fused operations — `p_first_rest(seq, &first, &rest)`

## Architecture: Self-Reflective C

### The Vision: Query IS Execution

Like php.php for PHP, this is c.c for C: parse → entities → modify → recompile → execute → observe.

```
source text
  → gram_parse() → entities + bitmask indexes      (parse)
  → bitmask query + transform                       (analyze/modify)
  → emit_x86() or emit_c()                          (compile)
  → JIT call via function pointer                   (execute)
  → TAP/trace → observable entities                 (observe)
  → modify source → repeat                          (evolve)
```

### Entity Model (Convergence)

Three systems arrived at the same shape:

| System | Entity | Kind | Position | Parent | Identity |
|--------|--------|------|----------|--------|----------|
| php.php | `$d[id]` | `k` | `o, s` | `p` | index |
| clojure-fast | `Token` | `type` | `pos, len` | implicit | index |
| grammar.c | `GNode` | `kind` | `start, len` | `parent` | index |

Universal: `Entity = {kind, start, len, parent, child, next, end}`

### sig Equivalence in C

| PHP sig | C equivalent | File |
|---------|-------------|------|
| Handler registry | Command registry (StrId → CmdFn) | base/cmd.c |
| `tap()` / `trace()` | TAP macro + trace ring buffer | base/tap.c |
| Hierarchy dispatch | Entity bitmask query | lang/grammar.c |
| `#describe` | `stats`, `nodes`, `trace` commands | glass/repl.c |
| Condition system | TAP points at error sites | base/tap.c |
| REPL / eval | `repl()` + `cmd_*` | base/cmd.c + glass/repl.c |

## Archive

18 files (11,541 lines) archived to `src/c/archive/`:
- Old monoliths: base.c, sys.c, read.c, emit_x86.c, emit_c.c, x86.c, clj.c
- Graduated prototypes: proto_base, proto_dispatch, proto_macro, proto_x86, proto_clj, proto_grammar, proto_cc, proto_eval, proto_image, proto_regalloc, proto_coll

Kept for Phase 2 optimization:
- `test/proto_coll2.c` — SIMD persistent data structures
- `test/proto_seq.c` — Sequence abstractions

## 64-bit Builtin Fix

`__builtin_popcount` → `__builtin_popcountll` (was silently truncating u64 to u32).
`__builtin_ctz` → `__builtin_ctzll` (TZCNT(0)=32 caused infinite loop in bm_next).
Fixed in base/fmt.c POPCOUNT/CTZ/CLZ macros.
