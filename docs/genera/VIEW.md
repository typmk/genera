# VIEW — Everything Is a View

> There are no "views" separate from "facts."
> A view is a fact you derived. A query is the derivation. The pmap holds both.
> Execution is the fixed point — where derivation produces nothing new.

---

## The Collapse

The fact/view distinction is artificial. In a persistent hash map:

- A **fact** is an entry in the pmap.
- A **view** is also an entry in the pmap — one that happens to be derivable from other entries.
- A **query** is the six ops (AND/OR/POP/SET/CLR/SCAN) applied to entries, producing new entries.
- A **step** is: query entries → assert results → new pmap root.

There is one thing: **entries in the pmap.** Some are ground truth (source bytes, user input). Some are derived (V_CONST, V_DEAD, bind[], code offsets). The pmap doesn't distinguish — it stores both the same way. Structural sharing makes storing derived results nearly free.

```
pmap v0: {source: bytes, nodes: GNodes}
  → query (pass_scope) → assert V_DEF, V_REF, bind[]
pmap v1: {source: bytes, nodes: GNodes, V_DEF: mask, V_REF: mask, bind: [...]}
  → query (pass_type) → assert V_INT, V_CONST, val[]
pmap v2: {... + V_INT: mask, V_CONST: mask, val: [...]}
  → query (pass_flow) → assert V_TAIL, V_DEAD
pmap v3: {... + V_TAIL: mask, V_DEAD: mask}
  → query (emit) → assert code_offsets, machine_bytes
pmap v4: {... + code: bytes, offsets: [...]}
```

Each step reads entries, derives new entries, asserts them into the next version. The pmap grows monotonically within a step. Old versions persist via structural sharing. **Every intermediate state is a value you can inspect, diff, or roll back to.**

The "cache or recompute?" question becomes: is derivation cheaper than a pmap lookup? For a bitmask AND over two `u64*` arrays: ~1 cycle per 64 entries. For a pmap get: ~3.4ns (112x slower than native). So bitmask views are cheaper to recompute. HAMT-stored results are cheaper to look up. The pmap IS the cost-tier decision.

### Datalog in Hardware

This is Datalog. Exactly.

```
Datalog:   facts + rules → derived facts → more rules → fixed point
genera:    entries + queries → derived entries → more queries → fixed point
```

| Datalog | genera | CPU |
|---|---|---|
| Relation (set of tuples) | `u64 *mask` (set of node IDs) | memory |
| Rule (horn clause) | pass function (scope, type, flow) | code |
| Join | `bm_and(a, b)` | AND instruction |
| Union | `bm_or(a, b)` | OR instruction |
| Project/Iterate | `bm_next(m, pos)` | TZCNT instruction |
| Count | `bm_pop(m)` | POPCNT instruction |
| Assert | `BM_SET(m, id)` | OR instruction |
| Retract | `BM_CLR(m, id)` | ANDN instruction |
| Stratification | pass 1 → pass 2 → pass 3 | cascading queries |
| Fixed point | no new derivations | execution |

Datalog's semi-naive evaluation: only derive from NEW facts each round, not all facts. genera's cascading passes: pass 2 only reads what pass 1 produced, pass 3 only reads what pass 2 produced. Same optimization. Termination guaranteed because the node set is finite and the views are monotone (bits only get set, never cleared within a pass).

The pmap is the EDB (extensional database — stored facts) AND the IDB (intensional database — derived facts). They're the same map. Structural sharing means keeping both is nearly free.

### One Table

| Domain | Entries in the pmap | Queries (six ops) | What gets derived |
|---|---|---|---|
| **Source** | bytes, GNodes | AND/OR over kind masks | "which nodes are lists?" |
| **Scope** | GNodes + kind masks | walk + SET | V_DEF, V_REF, bind[] |
| **Types** | + V_DEF, V_REF, bind[] | bottom-up AND/OR + SET | V_INT, V_CONST, val[] |
| **Flow** | + V_INT, V_CONST | top-down AND/OR + SET | V_TAIL, V_DEAD |
| **Emission** | + all views | AND views → key → emit | code bytes, offsets |
| **Interning** | raw strings | hash + SET | StrId table (O(1) compare forever) |
| **HAMT nodes** | key-value pairs | POPCOUNT(bitmap & (bit-1)) | child index |
| **Registers** | physical reg file | TZCNT(~live), POPCOUNT(live) | allocation, pressure |
| **Syscalls** | operation semantics | target × convention lookup | nr + arg regs |
| **Runtime** | execution counters | POP(hot ∧ node_range) | "which source lines are hot?" |

Every row: entries in → query → entries out. Same six ops. The "domain" is just which entries you're looking at.

### Memory = Retention Policy

The pmap holds everything. But not everything needs to be in the pmap at the same time. The memory hierarchy is a set of retention policies over the same entries:

```
Register     — derived entry held per-instruction, recomputed constantly
L1 cache     — entries held by hardware, ~1ns, automatic eviction
L2/L3 cache  — wider working set, ~4-12ns
Arena        — entries held until epoch boundary, then bulk CLR
HAMT/pmap    — entries held with structural sharing, versioned, persistent
Disk/image   — entries serialized, survives process death
```

Each tier: same entries, different cost, different lifetime.

- **Arena** = entries you'll retract in bulk (scratch within a step). CLR = reset pointer.
- **HAMT** = entries you want versioned (world state across steps). SET = path-copy + new root.
- **gencgc** = entries with unknown lifetime, tracked by generation bitmask + card table.
  - AND = "allocated AND in-gen0 AND referenced-from-gen1"
  - POP = "how many survive?" (decides promotion)
  - CLR = bulk free dead generation
- **Interning** = entries derived once, retained forever. SET first time, AND (compare) forever after.
- **L1 cache** = hardware's own retention policy. Same entries as RAM, closer.

The cache decision, the GC decision, the analysis decision, and the arena decision are the same decision: **which derived entries are worth keeping at which cost point.**

genera's model: arena for scratch (within a step), pmap for state (across steps), image for persistence (across sessions). Three tiers. No GC needed — the pmap is immutable (structural sharing), the arena is bulk-reset, and the image is a frozen pmap root.

### Execution = Fixed Point

A step reads entries, derives new entries, asserts them. When the step function *itself* is entries in the pmap (source code stored as GNodes), the system can:

1. Parse its own passes into GNodes (entries)
2. Analyze those GNodes with its own passes (derive more entries)
3. Compile those entries to machine code (derive code bytes)
4. Execute the compiled passes on *its own* GNodes

Step 4 = step 2 at native speed. The derivation deriving itself. `lambda(lambda)`.

```
entries₀ → query(entries₀) → entries₁ → query(entries₁) → entries₂ → ...
```

The **fixed point** is when `query(entriesₙ) = entriesₙ`. No new entries derivable. The compiled pass produces the same results as the interpreted pass. The optimized analysis of the analysis = the analysis. Execution is not a mode — it's the state where derivation has converged.

```
parse(pass_src) → GNodes → analyze(GNodes) → views → jit(views) → native_pass
       |                                                    |
       +────────────────────────────────────────────────────+
              native_pass(GNodes) = analyze(GNodes)
              the derived entries ARE the entries
```

In the pmap, every version along this path is a value. You can inspect the world before analysis, after analysis, after compilation. Diff any two versions. Roll back. The convergence to fixed point is visible as a sequence of pmap roots.

---

## x86_64 Reference

### How Many Instructions Exist?

| What you count | x86_64 | ARM64 |
|---|---|---|
| Unique mnemonics | **~1,500** | ~442 base, ~1,200 with SVE |
| Encoding forms (mnemonic + operand combos) | **~6,000** | ~3,000 |
| What a JIT actually uses | **50-80** | ~40-60 |

The ISA grew by accretion:

```
8086 (1978)       ~81    — the base
386 (1985)        ~130   — 32-bit, BSF/BSR, SETcc
Pentium Pro       ~178   — CMOVcc
MMX (1997)        ~235   — packed integer SIMD
SSE-SSE4 (1999-08) ~546  — 128-bit float/int SIMD
AVX/AVX2 (2011-13) ~780+ — 256-bit, FMA, BMI
AVX-512 (2016+)   ~1,200+— 512-bit, 18 subsets
Today             ~1,500+— crypto, matrix, CET, etc.
```

### Categorized x86_64

| Category | Mnemonics | What matters for genera |
|---|---|---|
| **Base integer** | ~320 | MOV, ADD, SUB, IMUL, IDIV, CMP, TEST, AND, OR, XOR, SHL, SHR, LEA, CMOVcc, SETcc, Jcc, CALL, RET |
| **Bit manipulation** (BMI1/2) | ~16 | TZCNT, LZCNT, POPCNT, BLSI, BLSR, ANDN, PDEP, PEXT, BZHI |
| **SSE/SSE2/3/4** | ~297 | ADDSD, MULSD, MOVDQA, PSHUFB, PTEST, PCMPEQ |
| **AVX/AVX2** | ~215 new | VPAND, VPOR, VPTEST, VBROADCAST, VPGATHER |
| **AVX-512** (all subsets) | ~400-600 | VPOPCNTQ, VPTERNLOGD, VP2INTERSECT |
| **FMA** | ~36 | VFMADD132/213/231 PS/PD/SS/SD |
| **x87 float** | ~89 | Legacy, replaced by SSE scalar |
| **Crypto** | ~21 | AESENC, SHA256RNDS2, PCLMULQDQ |
| **System/privileged** | ~46 | RDTSC, CPUID, SYSCALL |
| **Other** (AMX, CET, VMX) | ~70+ | Matrix, control-flow enforcement, virtualization |

### The Counting Confusion

"Opcode", "instruction", and "instruction form" mean different things:

| Term | What it counts | Example |
|---|---|---|
| **Mnemonic** | Human name | `MOV` = 1 mnemonic, but 34 encodings |
| **Opcode** | Encoding byte(s) | ADD has ~10 opcode variants (AL/imm, r/m+r, r+r/m, imm8, imm32...) |
| **Instruction form** (XED iform) | Mnemonic + operand types + sizes | MOV = 22 iforms (GPR8_GPR8, MEMv_GPRv, ...) |

People say "hundreds of opcodes" because for any practical workload you interact with ~100-200 mnemonics. The full ISA is ~1,500 mnemonics / ~6,000 encoding forms.

---

## genera Today: 22 Instructions

| Category | Instructions | Count |
|---|---|---|
| Move | `mov reg,imm` / `mov reg,reg` | 2 |
| Arithmetic | `add` / `sub` / `imul` / `idiv` / `neg` | 5 |
| Compare | `cmp` / `test` | 2 |
| Conditional | `setcc` + `movzx` / `jcc` | 2 |
| Stack | `push` / `pop` | 2 |
| Memory | `load [rbp+off]` / `store [rbp+off]` / `load_abs` / `store_abs` | 4 |
| Control | `jmp` / `call` / `ret` | 3 |
| Frame | `prologue` / `epilogue` | 2 |

Compiler-side (not emitted, used for view operations):

| Op | Builtin | CPU Instruction |
|----|---------|-----------------|
| AND | `&` operator | AND |
| OR | `\|` operator | OR |
| POP | `__builtin_popcountll` | POPCNT |
| SET | `\|= (1ULL << bit)` | OR |
| CLR | `&= ~(1ULL << bit)` | AND + NOT |
| SCAN | `__builtin_ctzll` | TZCNT/BSF |

---

## SBCL's VOP System (Reference)

### What is a VOP?

A **V**irtual **OP**eration. SBCL's IR between Lisp semantics and machine code.

```
Lisp source
  -> IR1 (high-level: type inference, inlining, constant folding)
    -> IR2 (VOPs: virtual machine instructions with register constraints)
      -> Machine code (actual x86 bytes)
```

A VOP specifies:
- **Operand types** (fixnum? unsigned-64? double-float?)
- **Storage classes** (register? stack? constant?)
- **Cost** (for selection among alternatives)
- **Policy** (:fast, :safe, :fast-safe, :small, :small-safe)
- **Generator** (code that emits x86 instructions)
- **Guard** (runtime feature check — does CPU have AVX2?)

### VOP Counts

| Architecture | VOPs | Instruction definitions |
|---|---|---|
| x86-64 | **622** | ~213 |
| arm64 | **529** | — |
| generic | **13** | — |

### x86-64 VOPs by Category

| File | VOPs | What |
|------|-------|------|
| `arith.lisp` | **201** | Integer arithmetic, bignum, bit ops, overflow |
| `float.lisp` | **62** | SSE float arithmetic, conversions, comparisons |
| `type-vops.lisp` | **50** | Type predicates (fixnump, consp, symbolp...) |
| `array.lisp` | **46** | Array access, bounds checking |
| `cell.lisp` | **36** | Object slot access (symbols, closures) |
| `system.lisp` | **35** | Intrinsics (rdtsc, cpuid, barriers) |
| `call.lisp` | **29** | Function call/return (local, full, tail) |
| `char.lisp` | **18** | Character operations |
| `sap.lisp` | **15** | Raw memory access |
| `c-call.lisp` | **15** | Foreign function interface |
| `nlx.lisp` | **13** | Non-local exit (catch/throw) |
| `move.lisp` | **13** | Movement between storage classes |
| `tls.lisp` | **12** | Thread-local storage |
| `simd-pack*.lisp` | **23** | SIMD (128-bit SSE + 256-bit AVX2) |
| `pred.lisp` | **11** | Branch, conditional move, eq/eql |
| `alloc.lisp` | **11** | Heap/stack allocation |
| `memory.lisp` | **9** | GC barriers, initialization |
| Others | ~23 | Debug, values, macros, subprim |

### Why 201 Arithmetic VOPs?

One Lisp function -> many VOPs. `+` maps to:

```
fast-+/fixnum=>fixnum         — tagged + tagged (cost 2)
fast-+-c/fixnum=>fixnum       — tagged + constant (cost 1)
fast-+/signed=>signed         — signed-64 + signed-64 (cost 3)
fast-+-c/signed=>signed       — signed-64 + constant (cost 2)
fast-+/unsigned=>unsigned     — unsigned-64 + unsigned-64 (cost 3)
fast-+-c/unsigned=>unsigned   — unsigned-64 + constant (cost 2)
+/signed=>integer             — may overflow to bignum
+/unsigned=>integer           — may overflow to bignum
overflow+-fixnum              — overflow detection
overflow+-signed              — overflow detection
overflow+-unsigned            — overflow detection
...plus cross-type variants
```

All emit the **same x86 ADD** — but with different operand constraints, overflow handling, representation conversions. The `define-binop` macro generates 6 VOPs per arithmetic op.

### VOP Selection: Type-Directed + Policy-Directed

```lisp
;; Policy from optimization declarations:
(if (zerop safety)
    (if (>= speed space) :fast :small)
    (if (>= speed space) :fast-safe :small-safe))
```

For each known function call:
1. Get all VOPs registered for that function, sorted by cost
2. For each VOP, check: guard passes? arg types match? result types match? policy compatible?
3. First match wins (cheapest compatible VOP)
4. If nothing matches -> full function call (100x slower)

### The Mapping: Many-to-Many

```
One Lisp function  ->  ~6-20 VOPs (type-specialized)
One VOP            ->  1-10 CPU instructions (with moves, branches, type checks)
One CPU instruction <-  used by many VOPs
```

Example: `move-to-word/integer` (unbox an integer) emits 6 instructions:

```asm
MOV rax, operand       ; load tagged value
TEST rax, 0x01         ; check fixnum tag
JZ  bignum_path        ; not fixnum? branch
SAR rax, 1             ; untag fixnum (shift right 1)
JMP done
bignum_path:
MOV rax, [rax+8]       ; load bignum value from heap
done:
```

### SBCL Data Structures

```
Template/VOP-Info:
  name          — e.g., fast-+/fixnum=>fixnum
  translate     — Lisp function (e.g., +)
  arg-types     — primitive type restrictions
  result-types  — what it produces
  ltn-policy    — :fast, :safe, :fast-safe, :small, :small-safe
  cost          — integer (lower = preferred)
  guard         — runtime CPU feature check
  generator     — function that emits x86 instructions

Storage Class (SC):
  any-reg       — any GPR (tagged Lisp objects)
  descriptor-reg — GC-visible pointer
  unsigned-reg  — untagged unsigned integer
  signed-reg    — untagged signed integer
  single-reg    — SSE register for single-float
  double-reg    — SSE register for double-float
  control-stack — stack slot

TN (Temporary Name):
  Virtual register. Has an SC. Register allocator assigns to physical regs.
```

---

## genera vs. SBCL

### The Structural Contrast

| Dimension | SBCL | genera |
|---|---|---|
| **IR count** | 2 (IR1 + IR2/VOPs) | 1 (GNodes + bitmask views) |
| **VOP/op count** | 622 specialized | 6 fundamental (AND/OR/POP/SET/CLR/SCAN) |
| **Selection mechanism** | Type inference -> template matching | View bitmask queries |
| **Why the explosion** | Dynamic typing: fixnum, bignum, float... | Untyped: everything is bits from the start |
| **Cost of unknown types** | Full function call (100x slower) | N/A — no type dispatch |
| **Instruction definitions** | ~213 in assembler DSL | 22 direct byte emitters |
| **Abstraction direction** | Ascending (Lisp types -> SCs -> registers) | Descending (toward hardware) |

### What genera takes from SBCL

**1. Guard mechanism.** SBCL's `:guard` checks CPU features at compile time:

```lisp
(define-vop (fast-popcnt)
  (:guard (member :popcnt *backend-subfeatures*))
  (:generator 1 (inst popcnt r x)))
```

genera equivalent — CPU feature probing as a bitmask view:

```c
// At startup: probe -> set bits
if (cpuid_has(CPUID_BMI1))  BM_SET(g_cpu, F_BMI1);
if (cpuid_has(CPUID_AVX2))  BM_SET(g_cpu, F_AVX2);

// During emission: same view query
if (BM_GET(g_cpu, F_BMI1))
    emit_blsr(...)       // fast scan
else
    emit_ctz_and_mask()  // fallback
```

**2. Constant-operand specialization.** SBCL generates `-c/` variants per arithmetic op. genera already does this via `V_CONST` — same mechanism, expressed as a view query instead of a separate VOP.

**3. Cost model.** SBCL: each VOP has an integer cost, cheapest matching template wins. genera: derive cost from view properties:

```c
u32 cost = 0;
if (!BM_GET(g->v[V_INT], id))   cost += 10;  // needs boxing
if (!BM_GET(g->v[V_CONST], id)) cost += 2;   // can't fold
if (!BM_GET(g->v[V_PURE], id))  cost += 5;   // can't reorder
```

**4. The 622-to-6 ratio is the tax on dynamic typing.** SBCL needs `+/fixnum`, `+/signed`, `+/unsigned`, cross-type variants, overflow variants — ~20 VOPs for addition alone. genera doesn't need this: V_INT says "this is an integer" and there's one code path.

### What genera can do that SBCL can't

- **VOPs are predefined at build time.** Can't add new ones at runtime. genera's views are **computed per-program** — analysis adapts to what the code does.
- **VOP selection is per-node.** genera's views are **whole-program** (every node classified simultaneously). "Are ALL calls in this function pure?" is O(1) — one AND + POP. SBCL's per-node matching can't express this.
- **VOPs are data-opaque.** The template matching is procedural. genera's views ARE data — queryable, composable, inspectable. The analysis IS the debugging tool.

---

## Instruction Additions for genera

### Tier 1: BMI1 — Accelerate the Six Ops

Available on Haswell+ (2013). These directly improve the core view algebra.

```
TZCNT  dst, src         — trailing zero count (SCAN)
                          Already using via __builtin_ctzll
                          Key: TZCNT(0) = 64 (defined). BSF(0) = UB.

BLSI   dst, src         — extract lowest set bit
                          dst = src & (-src)
                          Isolates one fact from a view

BLSR   dst, src         — reset lowest set bit
                          dst = src & (src - 1)
                          Consume one fact, advance iterator

BLSMSK dst, src         — mask up to lowest set bit
                          dst = src ^ (src - 1)

ANDN   dst, src1, src2  — AND NOT (3-operand!)
                          dst = ~src1 & src2
                          Set difference in one instruction
                          Replaces: tmp = ~a; dst = tmp & b
```

SCAN loop improvement:

```c
// Current: CTZ + shift + mask + branch per fact
while (wi < nw) {
    u64 w = m[wi] >> bit;
    if (w) return wi * 64 + bit + CTZ(w);
    wi++; bit = 0;
}

// With BMI1: TZCNT + BLSR per fact (2 instructions)
while (word) {
    u32 bit = TZCNT(word);
    process(base + bit);
    word = BLSR(word);        // clear lowest, advance
}
```

### Tier 2: BMI2 — Parallel Bit Manipulation

```
PEXT   dst, src, mask   — parallel bit extract (gather)
                          Collects bits at mask positions, packs contiguous
                          USE: HAMT child indexing
                          Current: POPCOUNT(bitmap & (bit - 1))

PDEP   dst, src, mask   — parallel bit deposit (scatter)
                          Reverse of PEXT
                          USE: set specific facts by ordinal

BZHI   dst, src, idx    — zero high bits from position
                          dst = src & ((1 << idx) - 1)
                          USE: range queries in bm_pop_range

SARX/SHLX/SHRX         — shift without clobbering flags
                          USE: view SCAN without destroying CMP flags
```

### Tier 3: Branchless Emission — CMOVcc and LEA

For emitted user-program code (what the JIT produces).

```
CMOVcc dst, src          — conditional move (16 conditions)
                          Eliminates branches in simple if-then-else
                          View-driven: use when V_CONST on both branches,
                          or both sides are simple loads

LEA dst, [base+idx*scale+disp]  — address math without flags
                          scale in {1, 2, 4, 8}
                          Strength reduction: *3 = [rax+rax*2]
                                              *5 = [rax+rax*4]
                                              *9 = [rax+rax*8]
```

### Tier 4: Shifts and Bitwise (for Emitted Code)

Currently the JIT has NO shift or bitwise instructions.

```
SHL  dst, cl/imm       — shift left (multiply by 2^n)
SHR  dst, cl/imm       — logical shift right (unsigned divide by 2^n)
SAR  dst, cl/imm       — arithmetic shift right (signed)
AND  dst, src           — bitwise AND
OR   dst, src           — bitwise OR
XOR  dst, src           — bitwise XOR
NOT  dst                — bitwise NOT
XOR  reg, reg           — 2-byte zero (vs 5-byte mov imm 0)
```

These expose the six ops to the language itself:

```clojure
(bit-and a b) -> AND rax, rcx      ; 1 instruction when V_INT on both
(bit-or a b)  -> OR rax, rcx
(bit-count x) -> POPCNT rax, rax
(bit-scan x)  -> TZCNT rax, rax
```

### Tier 5: AVX2 — Bulk View Operations

Process 256 GNodes per instruction for view algebra.

```
VMOVDQA  ymm, [mem]              — aligned 256-bit load
VPAND    ymm, ymm, ymm/mem      — 256-bit AND (bm_and: 4 words at once)
VPOR     ymm, ymm, ymm/mem      — 256-bit OR  (bm_or: 4 words at once)
VPXOR    ymm, ymm, ymm/mem      — 256-bit XOR
VPANDN   ymm, ymm, ymm/mem      — 256-bit AND NOT (set difference)
VPTEST   ymm, ymm               — test if all zeros (skip empty 256-node blocks)
```

bm_and today vs. AVX2:

```c
// Current: 1 word per iteration
for (u32 i = 0; i < nw; i++) dst[i] = a[i] & b[i];

// AVX2: 4 words per iteration (4x throughput)
for (; i + 4 <= nw; i += 4) {
    __m256i va = _mm256_load_si256(a + i);
    __m256i vb = _mm256_load_si256(b + i);
    _mm256_store_si256(dst + i, _mm256_and_si256(va, vb));
}
```

VPTEST for sparse views (V_DEAD, V_TAIL — typically <5% of nodes):

```c
// Skip empty 256-node blocks in O(1)
__m256i chunk = _mm256_load_si256(view + i);
if (_mm256_testz_si256(chunk, chunk)) continue;
// Only drop to scalar TZCNT/BLSR when bits are actually set
```

---

## View-Driven Instruction Selection

### The Dispatch Key

Compute a key from views -> O(1) instruction strategy:

```c
u32 key = 0;
if (BM_GET(g->v[V_DEAD],  id)) key |= 1;
if (BM_GET(g->v[V_CONST], id)) key |= 2;
if (BM_GET(g->v[V_INT],   id)) key |= 4;
if (BM_GET(g->v[V_PURE],  id)) key |= 8;
if (BM_GET(g->v[V_TAIL],  id)) key |= 16;
if (BM_GET(g->v[V_CALL],  id)) key |= 32;

// 6-bit key -> function pointer
emit_handlers[key & 0x3F](cc, id);
```

The selection matrix:

```
V_DEAD                        -> skip (emit 0)
V_CONST ^ V_INT               -> emit immediate from val[id]
V_INT ^ V_PURE ^ both_const   -> fold at compile time
V_INT ^ ~V_CONST              -> emit integer arithmetic (no boxing)
V_TAIL ^ V_CALL               -> emit tail call (jmp, not call)
V_TAIL ^ is_self_call         -> emit loop (TCO)
V_PURE                        -> safe to reorder / hoist / CSE
~V_PURE                       -> must preserve evaluation order
```

Where SBCL has 622 VOPs, genera has 64 possible view keys — and most map to the same handful of emission strategies. The views ARE the VOP selector.

### Register Allocation via Bitmask

Track live variables as a bitmask over the register file:

```c
u16 live_regs = 0;  // 16 bits for 16 x86_64 GPRs

u8 alloc_reg(u16 *live) {
    u16 free = ~(*live) & ALLOCABLE_MASK;  // ANDN
    if (!free) return SPILL;
    u8 r = TZCNT(free);                   // first free
    *live |= (1 << r);                     // SET
    return r;
}

void free_reg(u16 *live, u8 r) {
    *live &= ~(1 << r);                   // CLR
}

u32 pressure(u16 live) {
    return POPCOUNT(live);                 // POP
}
```

The six view ops ARE the register allocator. Same algebra, different domain.

### Strength Reduction (V_CONST-driven)

When `V_CONST` is set on one operand:

```
(* x 0)   -> XOR rax, rax                    val=0
(* x 1)   -> nop                              val=1
(* x 2)   -> SHL rax, 1  or  ADD rax, rax    val=2
(* x 3)   -> LEA rax, [rax+rax*2]            val=3
(* x 4)   -> SHL rax, 2                      val=4
(* x 5)   -> LEA rax, [rax+rax*4]            val=5
(* x 8)   -> SHL rax, 3                      val=8
(* x 9)   -> LEA rax, [rax+rax*8]            val=9
(* x 2^n) -> SHL rax, n                      val=pow2
(/ x 2^n) -> SAR rax, n  (signed)            val=pow2
(+ x 0)   -> nop                              val=0
(- x 0)   -> nop                              val=0
```

Automatic: V_CONST says WHEN. The constant says WHICH rule.

### Future Views -> Future Instructions

```
V_INLINE     — small function, inline body instead of CALL
               bm_pop(fn_body_range) < threshold

V_LOOP_INV   — loop-invariant expression, hoist to callee-saved reg
               V_PURE ^ "no V_DEF in loop body for any V_REF in expr"

V_BOUNDED    — integer in known range [lo, hi]
               enables: unsigned ops, smaller immediates, bounds elision

V_ALIAS_FREE — no aliasing (immutable data)
               enables: reorder loads, eliminate redundant loads
               genera: TRUE everywhere (immutable model)

V_HOT        — runtime feedback: frequently executed
               enables: more registers, loop unrolling
               stored as: runtime bitmask (same format as static views)
```

---

## Priority

| # | What | Instructions | Impact |
|---|------|-------------|--------|
| 1 | BMI1 SCAN optimization | BLSR + TZCNT | 2x faster fact iteration |
| 2 | BMI1 set difference | ANDN | 1-instruction CLR + view difference |
| 3 | Emit CMOVcc | CMOVE/NE/L/G/LE/GE | Branchless if for simple cases |
| 4 | Emit shifts | SHL/SHR/SAR | Power-of-2 multiply/divide |
| 5 | Emit LEA | LEA [base+idx*scale+disp] | Strength reduction x3,5,9 |
| 6 | Emit XOR reg,reg | XOR | 2-byte zero vs 5-byte mov 0 |
| 7 | Emit bitwise ops | AND/OR/XOR/NOT | Language-level bitmask ops |
| 8 | BMI2 BZHI | BZHI | Faster range queries |
| 9 | BMI2 PEXT/PDEP | PEXT/PDEP | O(1) HAMT index, bit scatter/gather |
| 10 | AVX2 bulk views | VPAND/VPOR/VPTEST | 4x throughput on view algebra |
| 11 | View dispatch table | 6-bit key -> handler | O(1) automatic instruction selection |
| 12 | Bitmask regalloc | u16 live set | Same six ops for register management |

Items 1-7: small additions to x86.c (a few lines each).
Items 8-9: need VEX encoding support.
Item 10: needs YMM register definitions.
Items 11-12: compiler architecture changes using existing instructions.

---

## The Core Insight

Six operations. Every domain.

| Op | CPU | Source | Memory | DB | Syscalls |
|---|---|---|---|---|---|
| **AND** | AND | "tail AND pure nodes" | "allocated AND unreachable" | join | "this op AND this target" |
| **OR** | OR | "int OR const nodes" | "gen0 OR gen1 survivors" | union | "linux OR darwin" |
| **POP** | POPCNT | "how many dead nodes?" | "how many live objects?" | count | "how many args?" |
| **SET** | OR bit | "mark as V_CONST" | "allocate (bump)" | insert | "assert fact" |
| **CLR** | ANDN | "unmark V_DEAD" | "reset arena (bulk free)" | delete | "retract fact" |
| **SCAN** | TZCNT | "next tail-call node" | "next live object" | iterate | "next matching target" |

The algebra doesn't change. The domain does. Registers, GNodes, heap objects, database tuples, syscall targets — all are sets of facts queried with the same six operations.

SBCL needs 622 VOPs because it fights dynamic types at every operation. genera needs 6 ops because the data format IS the CPU operation. SBCL's abstraction ascends (Lisp types -> SCs -> registers). genera's abstraction descends (toward hardware: integers, arrays, bits).

Descent always terminates at the same place. AND, OR, POPCOUNT. Then the view views itself, and that's the fixed point.

The memory hierarchy (register -> L1 -> L2 -> L3 -> RAM -> disk) is not separate infrastructure. It's the view retention policy — same data, different cost tier, different lifetime. L1 is a hardware view. An arena is a software view. A HAMT is a versioned view. An image file is a frozen view. The cache decision and the analysis decision and the GC decision are the same decision: **which derived facts are worth keeping at which cost point.**

```
bit op: ~0.3 ns   register (view held per-instruction)
L1:     ~1 ns     hardware cache (automatic view)
L2:     ~4 ns     wider cache
L3:     ~12 ns    last-level cache
RAM:    ~100 ns   all facts
SSD:    ~20 us    serialized view (image)
```

There is no "memory subsystem" separate from "compilation" separate from "query engine." There is one thing — views over facts — at different cost points. The six ops are the interface at every layer.

---

## Syscalls Are Views Too

The effect boundary (I/O, syscalls) follows the same pattern. `syscalls.edn` already frames this:

> Syscalls ARE a grammar. Operations = semantic rules (what). Targets = syntax variants (how on this platform). Porting = transpilation between syntaxes.

```
:operations                          — semantic view (what)
  :read, :write, :mmap, :exit_group

:targets                             — platform view (where)
  :linux/x86_64, :linux/aarch64, :darwin/aarch64, :windows/x86_64

:conventions                         — calling convention view (how)
  x86_64:  syscall, nr=rax, args=[rdi,rsi,rdx,r10,r8,r9], clobbers={rcx,r11}
  aarch64: svc #0,  nr=w8,  args=[x0,x1,x2,x3,x4,x5]
  riscv64: ecall,   nr=a7,  args=[a0,a1,a2,a3,a4,a5,a6]

:constants                           — divergent literal view (what differs)
  MAP_ANON: linux=0x20, darwin=0x1000
  PAGE_SIZE: x86_64=4096, darwin/aarch64=16384
  AT_FDCWD: linux=-100, darwin=-2

:equivalences                        — cross-platform semantic mapping
  file-watch: linux=inotify, darwin=kqueue, windows=IOCP
  jit-memory: linux=mmap(RWX), darwin=mmap(RWX|MAP_JIT), windows=VirtualAlloc
```

Same structure at every layer:

| Layer | Facts | Views | Ops |
|---|---|---|---|
| **Source** | bytes | GNodes + bitmask indices | AND/OR/POP/SET/CLR/SCAN |
| **Analysis** | GNodes | V_CONST, V_INT, V_TAIL, V_DEAD... | same six |
| **Emission** | instruction bytes | view key -> handler | same six |
| **Registers** | physical regs | u16 live bitmask | same six |
| **Syscalls** | operation semantics | target x convention x constants | same six (platform = bitmask) |
| **Runtime** | allocations/frames | execution bitmasks over GNode IDs | same six |

The grammar doesn't end at machine code. It continues through the effect boundary. `emit(exec)` = run. `emit(buf)` = compile. `emit(kernel)` = syscall. Same bytes, different destination.

Step 5 in GENERA.md ("sys as data table") unifies this — a single SysOp table queried the same way views are queried. Runtime fills regs from the table and does `syscall`. JIT emits reg fills from the table and emits `syscall` bytes. Same table, same query, different destination.

See: `src/c/sys/syscalls.edn` (the grammar), `docs/SYSCALLS.md` (the reference).
