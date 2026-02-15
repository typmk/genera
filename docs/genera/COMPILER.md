# COMPILER — The Grammar Engine as Universal Compiler

> Views are queries. The grammar is the cost model. Specificity is efficiency.
> The whole compiler is a query engine over a flat array with bitmask materialization.
> An IR is a materialized view that forgot its query.

Working document. Extends VIEW.md (Datalog model) and PASSES.md (GCC analysis).

---

## The Collapse

The compiler has one mechanism: **rules**. A rule is pattern -> output.

- Parse rules: bytes -> GNodes (syntax)
- View rules: GNodes -> bits (analysis)
- Emit rules: GNodes + bits -> bytes (codegen)
- Lower rules: GNodes -> new GNodes (decomposition)

Same engine evaluates all of them. The "compiler" is:

```
parse:  bytes -> N                        (grammar: bytes -> nodes)
step:   s* = lfp(T) over 2^(N x V)       (rules: nodes -> views, to fixed point)
emit:   for-each n. emit(n, s*)           (grammar: nodes + views -> bytes)
```

Three lines. The rest is rules.

---

## Formal Model

Clean enough for an index card.

**State space.** N = nodes (finite, from parse). V = view identifiers. State is:

```
S in 2^(N x V)
```

Powerset lattice. Each element (n, v) means "view v holds for node n."
Ordered by subset-of, with bottom = empty, top = N x V.

**Rules.** A rule r is a monotone endomorphism:

```
r : 2^(N x V) -> 2^(N x V)
    where s <= s'  =>  r(s) <= r(s')     (monotone)
    and   s <= r(s)                       (inflationary)
```

Rules only add, never remove. Bits only set, never cleared.

**Step.** Apply all rules, union results:

```
T(s) = s  U  r_1(s)  U  r_2(s)  U  ...  U  r_k(s)
```

T is monotone (composition/union of monotone functions).

**Fixed point.** By Knaster-Tarski, T has a least fixed point:

```
s* = lfp(T) = bottom  U  T(bottom)  U  T^2(bottom)  U  ...
```

Convergence in at most |N| x |V| steps (finite lattice). In practice, 3-10 rounds.

**Emit.** Emit rules are (pattern, output) pairs, ordered by decreasing specificity:

```
E = { (P_1, code_1), (P_2, code_2), ..., (P_m, code_m) }
    where P_1 > P_2 > ... > P_m   (decreasing specificity)
```

For node n, emit the first rule whose pattern is satisfied:

```
emit(n) = code_j   where j = min{i : P_i <= s*(n)}
```

More specific pattern (larger P) -> matched first -> better code.

**Stratification.** Partition rules into strata S_0, S_1, ..., S_k where
stratum i only reads views produced by strata < i. Compute each stratum's
fixed point in sequence. Same result, fewer iterations.

| Concept | Math | genera | CPU |
|---------|------|--------|-----|
| State | s in 2^(N x V) | bitmask arrays | memory |
| Rule | monotone f: 2^(N x V) -> 2^(N x V) | view pass | code |
| Join | s U s' | bm_or | OR |
| Meet | s ^ s' | bm_and | AND |
| Cardinality | \|s\| | bm_pop | POPCNT |
| Step | T(s) = U r_i(s) | evaluate all rules | loop |
| Fixed point | T(s*) = s* | no new bits | termination |
| Emit | max-specificity match | if/else on views | branch |

---

## Views Are Queries

Views are not a thing the system has. They are queries the system runs.

A view definition IS a Datalog rule IS a grammar rule:

```
-- Datalog
v_const(N) :- literal(N).
v_const(N) :- call(N), v_pure(callee(N)), forall(arg(N,A), v_const(A)).

-- SQL
CREATE VIEW v_const AS
  SELECT id FROM nodes WHERE kind = 'literal'
  UNION
  SELECT id FROM nodes n
  WHERE kind = 'call'
    AND callee(n) IN (SELECT id FROM v_pure)
    AND ALL args(n) IN (SELECT id FROM v_const)

-- genera (bitmask)
BM_SET(v_const, id) when:
  BM_GET(NK_NUM, id)                                          -- literal
  OR (BM_GET(NK_LIST, id) && BM_GET(v_pure, callee(id))      -- pure call
      && all_args_const(id))                                   -- with const args
```

The bitmask IS the materialized query result. Composing views IS composing queries:

```sql
-- "pure constant tail calls"
SELECT id FROM v_const INTERSECT v_pure INTERSECT v_tail
```
```c
// genera: 3 SIMD instructions
bm_and(tmp, v_const, v_pure, mw);
bm_and(tmp, tmp, v_tail, mw);
```

"Running a view pass" = materializing a query.
"Pass ordering" = query dependency resolution.
"Optimization" = materializing more queries -> more specific emit rules match.
"Fixed point" = all useful queries materialized.

---

## The Grammar IS the Cost Model

There is no separate cost mechanism. The grammar's specificity ordering IS cost.

Emit rules ordered by specificity:

```
{V_CALL, V_INT, V_CONST}   ->  fold at compile time (0 instructions)
{V_CALL, V_INT}             ->  machine integer op   (1 instruction)
{V_CALL, V_PURE}            ->  inlineable call      (N instructions)
{V_CALL}                    ->  generic dispatch     (many instructions)
```

More view bits in the pattern = more specific match = less code = faster.
The "cost" of a node = how specific is the rule that matched it.
Specificity = information. More views set = more information = better code.

**Consequence: no cost arrays, no template tables, no SBCL `:generator N`.**
The grammar rules, ordered by specificity, ARE the cost model.

**Which views are worth computing?** Ask the grammar:

```c
// "Could computing view V enable a better rule for any node?"
potential = bm_and(rule.other_conditions, current_views);  // other conditions met
useful    = bm_andnot(potential, V.m);                     // but V not yet set
// If bm_pop(useful) > 0, view V could enable better rules for those nodes
```

One bitmask query. The grammar tells you whether a view pass is useful.

**What is the optimization level?** How many views you compute before emitting.
- 0 views = only generic rules match = TCC-like output
- all views = most specific rules match = maximum quality
- One continuous dimension: depth of derivation before emit

**SBCL comparison.** SBCL assigns cost per VOP (`:generator 1/2/3`), sorts templates
by cost, picks first match. Same principle -- specificity IS cost -- but SBCL uses
explicit integers while genera derives cost from rule structure. SBCL's 4 policies
(fast/small x safe/unsafe) correspond to "which emit grammar to use."

---

## Query Order: Why It Doesn't Matter

**GCC's problem:** pass order matters because passes REWRITE the IR. CCP changes
GIMPLE, so DCE sees different input depending on whether CCP ran first.

**genera's answer:** with immutable data and monotone rules, order doesn't affect
correctness. Proof: two monotone rules A, B on state s:

```
A(s) U B(s) = B(s) U A(s)            -- union is commutative
A(B(s) U s) >= A(s)                   -- monotonicity
```

Fixed point is the same regardless of evaluation order.

**Between strata:** order computed from rule dependencies (topological sort).
**Within a stratum:** order irrelevant for correctness. For speed: semi-naive
evaluation (only process NEW facts each round).

```
round 0: ground facts (from parse)
round 1: rules applied to ground facts -> new_1
round 2: rules applied to new_1 only   -> new_2
...
round k: new_k = empty                 -> fixed point
```

GCC's "run CCP 5 times, DCE 8 times" IS manually-scheduled fixed-point iteration.
genera computes the same fixed point automatically.

---

## IR Comparison: GCC, SBCL, genera

**An IR is a materialized view that forgot its query.**

### Three Systems, One Pattern

| | GCC | SBCL | genera |
|--|-----|------|--------|
| **IRs** | 3 (TREE -> GIMPLE -> RTL) | 2 (IR1 -> IR2/VOP) | 1 (GNode + views) |
| **Node size** | 24-56 bytes (varies by IR) | ~200 bytes (IR1) | 28 bytes (fixed) |
| **Total/expr** | ~100-150 bytes across IRs | ~250+ bytes (IR1+IR2+TN) | ~61 bytes (node+metadata) |
| **Node kinds** | 270+ tree codes, ~30 gimple, ~200 rtx | ~15 node types | 14 (syntax only) |
| **Semantics in** | the node code itself | node type + derived-type | views (bitmasks, separate) |
| **Translation** | destructive (nesting lost) | semi-destructive (IR1 kept) | none (views are additive) |

### What Each IR Actually Is

**GCC TREE** (tree_base: code:16 + 40 flag bits + type pointer): The parse tree.
270+ codes because GCC encodes semantics IN the code field (PLUS_EXPR, POINTER_TYPE).
The flags (constant_flag, side_effects_flag, volatile_flag, pure_flag) are views
inlined into the node struct instead of parallel arrays. In genera: GNode + per-node flags.

**GCC GIMPLE** (gimple: code:8 + subcode:16 + bb pointer + next/prev): Flattened
3-address code with SSA. This is the TREE with two transformations: flatten nested
expressions (Shape 7: DECOMPOSE) and compute reaching definitions (Shape 8: DERIVE
-> SSA). In genera: the original array + lowering rules + V_SSA_DEF view.

**GCC RTL** (rtx_def: mode + code + 8 flags + variable operands): Register-level
operations. Machine modes instead of types. This is emit output kept as nodes for
one more round of analysis (regalloc + scheduling). In genera: the emit step's output.

**SBCL IR1** (NODE/CTRAN/LVAR/CBLOCK): Smarter split. Control flow (CTRAN: what
executes next) separated from data flow (LVAR: values). Blocks carry dataflow
sets and dominator trees. `derived-type` = a computed view. `reoptimize` flag =
"this node's views need recomputation." In genera: GNode with control (next/child)
and data (bind[]) already separate.

**SBCL IR2/VOP** (VOP-INFO template -> VOP instance -> TN): The template IS a
grammar rule. arg-types pattern + guard -> generator = genera's emit rule.
The VOP instance is a matched rule. The TN is a virtual register awaiting allocation.
SBCL keeps a back-pointer (vop-node). genera doesn't need one -- views are ON the
original nodes.

### The Relationship to Representation

| System | What it does | What it loses |
|--------|-------------|---------------|
| **GCC** | Creates new structure, destroys old | Nesting (TREE->GIMPLE), types (GIMPLE->RTL) |
| **SBCL** | Annotates, links new to old | Nothing permanent, but duplicates |
| **genera** | Queries in place | Nothing. Views are additive. |

GCC's three IRs are three snapshots of understanding. Each destroys the previous.
SBCL is halfway -- IR1 survives into IR2. genera: the bits are fixed, understanding
is a query. Multiple interpretations coexist. None destroys the territory.

### Sea of Nodes: The Views Without the Array

Sea of Nodes (Click 1995; V8/Turbofan, HotSpot) is the inverse of genera. It
destroys structure to gain freedom, then reconstructs structure at the end.

SoN replaces CFG with a dependency graph. Three edge types (value, control, effect).
Pure operations "float" -- no fixed position. The optimizer reorders freely. A
scheduling phase reconstructs execution order before codegen.

V8 abandoned SoN in 2024 (Turboshaft). The reasons map precisely to genera's design:

| SoN problem | Why | genera |
|-------------|-----|--------|
| 3-7x L1 dcache misses | Scattered nodes after in-place mutation | Dense flat array, never reallocated |
| 20:1 visit-to-change ratio | Walk from returns upward, revisit same nodes | Single forward pass over array |
| Pass interaction bugs | Graph mutation means order matters | Monotone rules: same fixed point regardless of order |
| Nearly all nodes on effect chain | JS side effects pin nodes anyway | Tree order IS execution order; views mark what's free |
| Scheduling phase needed | Must reconstruct order after destroying it | Never destroyed it |
| Compile time 2x slower than CFG | All of the above | All of the above |

**SoN structurally encodes what genera queries:**

| SoN structural property | genera query |
|-------------------------|--------------|
| Node floats (no control edge) | `BM_GET(v[V_PURE], id)` |
| Node unreachable (no value edge) | `BM_GET(v[V_DEAD], id)` |
| Two nodes merge (CSE) | `bind[dup] → canonical` via `expr_hash[]` (Shape 6) |
| Node above loop (hoisted) | `BM_GET(v[V_LOOP_INV], id)` (Shape 5) |
| Effect chain orders two stores | `BM_GET(v[V_NOALIAS], pair)` -- no alias = free to reorder |

SoN builds a new structure to represent these facts. genera asks questions about
the existing structure and stores answers as bits. Same lattice, same fixed point:

```
SoN:     the graph IS the state     →  mutate graph to advance
genera:  bitmasks ARE the state     →  set bits to advance
```

**The views are the sea. The flat array is dry land.**

SoN is a materialized view that forgot its query. genera keeps the query.

### How 14 Kinds Cover Every Language

GCC needs 270+ tree codes because it encodes WHAT something MEANS in the node type.
genera's 14 kinds describe WHAT something LOOKS LIKE syntactically:

```
Atoms:     ident, num, str, op, kw, other    (6 kinds -- leaves)
Compounds: list, vec, map                     (3 kinds -- containers)
Quoting:   quote, syntax-quote, unquote, splice  (4 kinds -- macro system)
Structure: root                               (1 kind)
```

Every language has atoms and compounds. Whether `+` is an operator (C), a function
(Lisp), or a method (Ruby) -- syntactically it's an ident or op. The MEANING goes
into views. Same node, different views, different emit rules match.

---

## The Meta-Grammar: Node Kinds as Rules

The 14 GNode kinds are a hardcoded grammar. The parser (gram_parse) is a chain
of `if (cl & CL_X)` decisions:

```
cc[256] -> 6 char classes -> 14 node kinds
```

These ARE grammar rules compiled into C instead of expressed as data:

```
CL_DIG+ ('.' CL_DIG+)?       -> NK_NUM
CL_ID (CL_ID | CL_DIG)*      -> NK_IDENT
':' CL_ID+                    -> NK_KW
open(d) form* close(d)        -> dkind[d]  (NK_LIST/NK_VEC/NK_MAP)
qc form                       -> NK_QUOTE
```

The Lang struct is halfway there -- char classes and delimiters are DATA. But
the rules mapping classes to kinds are CODE (gram_parse, ~200 lines of C).
This is where self-description breaks.

**The fixed point of the structure:** make node kinds open, not a closed enum.

```clj
;; Currently: kind is u8 index into fixed enum
;; Fixed point: kind IS a StrId (interned symbol)

;; Parse rules DEFINE kinds by producing them:
(defrule atom-num   [CL_DIG+]              -> {:kind (intern "num")})
(defrule atom-ident [CL_ID (CL_ID|CL_DIG)*] -> {:kind (intern "ident")})

;; Lowering rules introduce NEW kinds:
(defrule lower-for  {:kind "list", :child ["for" init cond step body]}
                    -> [{:kind "block"} init {:kind "loop" :child [cond body step]}])
```

When kind = StrId, node kinds and symbol names are the same mechanism.
The kind vocabulary grows with the grammar. The grammar that produces nodes
and the grammar that classifies symbols are the same grammar. Self-reference closes.

---

## The Bootstrap: lambda(lambda)

Goal: everything written in Clojure, compiled to x86. No C at runtime.

### Current Layers

```
Layer 0:  C code (gram_parse, eval, builtins)     -- irreducible kernel
Layer 1:  Clojure evaluated by C eval              -- boots views, JIT walker
Layer 2:  Clojure compiled by JIT to x86           -- user programs
```

### What C Currently Provides

| C function | What it is | Clojure equivalent |
|-----------|-----------|-------------------|
| gram_parse() | bytes -> GNodes | load8, store32!, loops |
| eval_node() | tree-walk eval | recursion + dispatch |
| bi_x86_* | x86 encoding builtins | already Clojure-driven |
| Comp state | compiler locals/slots | already Clojure-driven |
| arena_alloc | bump allocator | bump/mark/restore builtins |

The x86 encoding and compiler state are ALREADY bridges -- C functions that
Clojure calls. The Clojure JIT walker (cg-expr, cg-compile-defn) IS the compiler.

### Bootstrap Sequence

```
Step 0:  C kernel parses + evals genera.clj
         -> Clojure environment exists

Step 1:  C kernel parses parser.clj (parser written in Clojure)
         -> GNodes of the parser source

Step 2:  Clojure JIT compiles parser.clj -> x86
         -> native parser, as fast as C

Step 3:  x86 parser parses emit.clj, compile.clj, etc.
         -> GNodes of the compiler

Step 4:  Clojure JIT compiles the compiler -> x86
         -> native compiler

Step 5:  Native compiler compiles itself
         -> same binary (FIXED POINT)
```

After Step 2, the C parser is dead code. After Step 4, the C evaluator is dead.
The C kernel bootstrapped the system and dissolved.

### The Fixed Point

```
compile(compile_source) = compile_binary
compile_binary(compile_source) = compile_binary     <- fixed point
```

This is f(f) = f. Not the Omega combinator (which diverges) -- the Y combinator
(which finds the fixed point). Convergence guaranteed because:
1. The lattice is finite (N nodes x V views)
2. Rules are monotone (only set bits)
3. Knaster-Tarski guarantees lfp exists

### Can Clojure Match C Speed?

gram_parse hot loop: load byte (1 cycle), table lookup (~4 cycles L1),
test+branch (~1 cycle), scan loop, store node. The JIT has every operation:
x86-load8-base!, x86-and-i!, x86-jcc!, x86-store32-base!, cg-loop + recur.

GCC -O2 produces: movzx, test, je -- exactly what the JIT would emit.

The gap: register allocation. The parser uses ~8 live variables. Adding ~200 lines
of liveness-based register allocation (Shape 9) closes it. After that: same
instructions, same speed, zero C.

### The True Irreducible Kernel

What ACTUALLY can't be written in Clojure:

```
1. mmap(PROT_EXEC)    -- syscall for executable memory
2. _start             -- ELF entry point
3. sys_write/read     -- I/O syscalls
```

~30 lines of assembly. Everything else is Clojure compiled by the same engine.

---

## The Full Stack as Grammar

`(rule pattern -> output)` at every level, from user code to electrons.

### Every Level Is a Grammar

```
Level          What the user reads                     What it IS
-----          ---------------------                   ----------
source         (+ (* x 2) 7)                           their code
parse          (defrule num [CL_DIG+] -> :num)          how text becomes tree
view           (defn v-const ...)                       what the compiler knows
emit           (defn cg-add [id] ... (x86-add! ...))   what code it produces
instruction    x86-add! = REX.W 01 /r                  what bytes the CPU sees
microarch      ADD r,r -> port 0156, 1 cycle            what the silicon does
gate           full adder = XOR + AND + OR              what the electrons do
```

One syntax. Same (name args body) Clojure form. The user zooms in or out by
reading different rules in the same language.

### CPU Architectures as Grammars

An instruction decoder IS a grammar engine. Byte patterns -> operations:

```clj
;; x86 (Intel 1978, still growing)
(rule [0x01 ?modrm]              -> {:op :add, :width 32})
(rule [0x48 0x01 ?modrm]        -> {:op :add, :width 64})

;; ARM (different grammar, same operations)
(rule [?cond 0000 00 0 ?rn ?rd 00000000 ?rm] -> {:op :add, :rd ?rd, :rn ?rn, :rm ?rm})

;; RISC-V (third grammar, cleanest encoding)
(rule [?imm ?rs1 000 ?rd 0010011]            -> {:op :addi, :rd ?rd, :rs1 ?rs1})
```

Three architectures. Three grammars. Same semantic operations.

### CPU Generations as Grammar Inheritance

Each generation EXTENDS the previous:

```clj
;; x86 base (1978)
(rule [0x01 ?m] -> {:op :add, :width 32})

;; x86-64 extends (2003): REX prefix unlocks 64-bit
(rule [REX.W 0x01 ?m] -> {:op :add, :width 64})

;; AVX extends (2011): VEX prefix, 3-operand, wider
(rule [VEX.256 0x58 ?m] -> {:op :vaddps, :width 256})

;; AVX-512 extends (2016): EVEX, masking, 512-bit
(rule [EVEX.512 0x58 ?m] -> {:op :vaddps, :width 512, :mask ?k})
```

Grammar inheritance. Each generation is a superset. The ISA is a monotonically
growing grammar -- bits only set, never cleared. Same property as views.

### CPU Families as Grammar Refinement (Microarchitecture)

Same architecture, different execution rules:

```clj
;; Architecture (shared by ALL x86 CPUs)
(rule {:op :add, :width 64} -> {:flags [:CF :OF :SF :ZF :AF :PF]})

;; Haswell (Intel 2013)
(rule {:op :add, :width 64} -> {:ports [0 1 5 6], :latency 1, :throughput 0.25})

;; Zen 4 (AMD 2022)
(rule {:op :add, :width 64} -> {:ports [0 1 2 3], :latency 1, :throughput 0.25})

;; Apple M1 (ARM -- different arch grammar, same rule shape)
(rule {:op :add, :width 64} -> {:ports [:E0 :E1 :P0-P3], :latency 1})
```

### Operating Systems as Grammars

A syscall interface IS a grammar: register pattern -> kernel operation.

```clj
;; Linux
(rule {:rax 1,  :rdi ?fd, :rsi ?buf, :rdx ?len}   -> {:syscall :write})
(rule {:rax 0,  :rdi ?fd, :rsi ?buf, :rdx ?len}   -> {:syscall :read})
(rule {:rax 9,  :rdi ?addr, :rsi ?len, :rdx ?prot} -> {:syscall :mmap})

;; macOS (different numbers, same operations)
(rule {:rax 0x2000004, :rdi ?fd, :rsi ?buf, :rdx ?len} -> {:syscall :write})

;; Windows (different mechanism entirely)
(rule {:rax ?n, :r10 ?rcx-save}  -> {:syscall (ntdll-lookup ?n)})
```

Kernel subsystems are also grammars:

```clj
;; VMM: the MMU IS a hardware grammar engine (page table walker)
(rule {:vaddr [?pgd ?pud ?pmd ?pte ?off]} -> {:paddr [(lookup ...) ?off]})

;; VFS: path pattern -> operations
(rule {:path "/proc/" ?pid "/" ?attr}  -> {:fs :procfs, :handler (proc-read ?pid ?attr)})

;; Scheduler: CFS = pick process with smallest vruntime
(rule {:event :tick, :runqueue ?q}     -> {:next (min-vruntime ?q)})

;; Network: packet pattern -> handler
(rule {:eth-type 0x0800, :ip-proto 6, :dst-port ?p}  -> {:handler (tcp-lookup ?p)})
```

### Compilation as Grammar Composition

```clj
;; Target = language + OS + architecture + microarchitecture
(def linux-x86-zen4
  (compose lang/clojure os/linux arch/x86-64 uarch/zen4))

(def macos-arm-m1
  (compose lang/clojure os/macos arch/aarch64 uarch/apple-m1))
```

Views (V_CONST, V_DEAD, V_PURE) work on ALL targets -- they're about the SOURCE.
Only emit rules consult the target grammar. **Cross-compilation is grammar substitution.**

### Grammar Diff

```clj
;; "What does Zen5 add over Zen4?"
(grammar-diff uarch/zen5 uarch/zen4)
;; -> wider AVX-512 throughput rules

;; "What does Linux have that macOS doesn't?"
(grammar-diff os/linux os/macos)
;; -> io_uring, clone3, etc.
```

---

## Cross-Disciplinary Names

`(rule pattern -> output)` applied to fixed point. Independently discovered everywhere.

| Discipline | What they call the rule | What they call the fixed point |
|-----------|------------------------|-------------------------------|
| Formal languages | Production / rewrite rule | Closure |
| Logic | Inference rule | Deductive closure |
| Databases | Query / view definition | Materialized view |
| Type theory | Typing judgment | Principal type |
| Physics | Law / equation of motion | Equilibrium |
| Biology | Genetic code (codon -> amino acid) | Homeostasis |
| Chemistry | Reaction rule | Chemical equilibrium |
| Control theory | Transfer function | Steady state |
| Economics | Strategy / payoff rule | Nash equilibrium |
| Category theory | Morphism / functor | Fixed point of endofunctor |
| Digital design | Truth table / LUT | Stable state |
| Neural networks | Weight matrix | Convergence / attractor |
| Thermodynamics | Boltzmann distribution | Maximum entropy |

The three fundamental names:

```
The rule:         morphism (math), inference (logic), law (physics),
                  function (CS), enzyme (bio), production (grammar)
                  All: structure-preserving map from pattern to output.

The fixed point:  lfp(T) (math), Cn(G) (logic), equilibrium (physics),
                  saturation (CS), homeostasis (bio)
                  All: the state where applying rules produces no change.

The lattice:      powerset (math), theory space (logic), phase space (physics),
                  type lattice (CS), fitness landscape (bio)
                  All: partially ordered set where rules move upward monotonically.
```

---

## Grammar Equivalence and Transpilation

### The Simple Version

Two grammars are equivalent when they produce the same GNode structure:

```
Clojure:  (+ 1 2)     -> [LIST [IDENT +] [NUM 1] [NUM 2]]
Scheme:   (+ 1 2)     -> [LIST [IDENT +] [NUM 1] [NUM 2]]    ;; identical
```

Transpilation between equivalent grammars = re-serialization.
Print the same tree in different syntax. Trivially correct.

```
source_A ->[parse A]-> GNodes ->[unparse B]-> source_B
```

### Grammar Distance

For non-equivalent grammars, structural differences require bridging rules:

```
Clojure:  (+ 1 2)     -> [LIST [IDENT +] [NUM 1] [NUM 2]]     ;; prefix
Python:   1 + 2        -> [OP  [NUM 1] [IDENT +] [NUM 2]]      ;; infix
```

ONE bridging rule: `[OP ?a ?op ?b] <-> [LIST ?op ?a ?b]` (infix <-> prefix).

```
distance(A, B) = |bridging rules needed to map A's GNodes <-> B's GNodes|
```

| Language pair | Distance | Why |
|--------------|----------|-----|
| Clojure <-> Scheme | ~5 | naming: defn<->define, fn<->lambda |
| Clojure <-> Common Lisp | ~15 | naming + defmacro + namespace |
| Clojure <-> Python | ~30 | infix, indentation, statements |
| Clojure <-> C | ~80 | types, mutability, pointers, headers |
| Python <-> Ruby | ~20 | close syntax families |
| C <-> Rust | ~50 | ownership, lifetimes, traits |

### Three Cases

**Case 1: Equivalent** (distance ~ 0). Parse + unparse. No views needed.
Clojure -> Clojure (pretty-print), JSON -> EDN.

**Case 2: Close** (distance < 30). Bridging rules rearrange tree structure
without changing meaning. Invertible. Clojure <-> Scheme, Python <-> Ruby.

**Case 3: Distant** (distance > 30). Views needed for semantic gap. Lowering
rules bridge features missing in target. Not always invertible.
Clojure -> C (lower persistent structures), Python -> Rust (infer ownership).

### Transpilation Pipeline

```
source_A ->[parse A]-> GNodes ->[views]-> GNodes+bits ->[bridge]-> GNodes' ->[emit B]-> source_B
```

Steps 1 (parse) and 4 (emit) already exist per language.
Step 2 (views) is the same engine as compilation.
Step 3 (bridge) is derivable from grammar comparison.
**No new machinery.**

---

## Runtimes as Grammars

### What a Runtime Is

A runtime is the set of operations a language assumes exist -- its axioms.
When transpiling, the gap between source axioms and target axioms = runtime needed.

```
Source axioms <= Target axioms  ->  trivial transpilation
Source axioms >  Target axioms  ->  runtime fills the difference
```

The runtime IS grammar rules: definitions of missing axioms in terms of the
target's axioms.

```clj
;; "assoc" axiom, defined for different targets:
(rule {:target :jvm, :op :assoc}    -> (clojure.lang.RT/assoc ...))    ;; native
(rule {:target :python, :op :assoc} -> (pyrsistent.assoc ...))         ;; library
(rule {:target :c, :op :assoc}      -> (hamt_assoc ...))               ;; include runtime
(rule {:target :x86, :op :assoc}    -> (cg-hamt-assoc ...))            ;; inline
```

### The Axiom Hierarchy

```
Layer 4: Language abstractions    transducers, protocols, multimethods
         defined by v
Layer 3: Language primitives      pmap, pvec, atom, seq, cons
         defined by v
Layer 2: Data structures          HAMT, trie, CAS, rope
         defined by v
Layer 1: Machine primitives       integer, array, pointer, atomic
         defined by v
Layer 0: Hardware                 register, memory, ALU, cache
```

Each layer is a grammar over the layer below. The runtime for a transpilation =
the layers the target is missing.

| Transpile to | Missing layers | Runtime needed |
|-------------|---------------|----------------|
| JVM | none | none (Clojure is native) |
| JavaScript | 2-3 | ClojureScript runtime (~5K lines) |
| Python | 2-3 | pyrsistent or custom HAMT |
| C | 2-3 | hamt.c + pvec.c + atom.c (~800 lines) |
| x86 | 1-3 | genera's coll.c + arena (~2K lines) |

Layer 1 (machine) exists everywhere. The gap is always layers 2-3.

### The Axiom Catalogue

The compiler maintains: for each axiom, what does it become on each target?

```clj
(def axiom-catalogue
  ;; axiom    target   level   implementation
  {:assoc    {:jvm     3       'clojure.lang.RT/assoc
              :js      3       'cljs.core/-assoc
              :python  2       'hamt_assoc          ;; needs runtime
              :c       2       'hamt_assoc          ;; needs runtime
              :x86     1       'cg-hamt-assoc}      ;; inline

   :+        {:jvm     1       'iadd                ;; bytecode
              :js      1       '+                   ;; native
              :c       0       '+'                  ;; native
              :x86     0       'x86-add!}})         ;; native
```

The level number IS the amount of unfolding needed. `+` is level 0-1 everywhere
(universal axiom). `assoc` ranges from level 1 (x86, inline) to level 3 (JVM).

### Three Transpilation Strategies

**INCLUDE**: emit runtime rules into the output.
```
Clojure -> C with :runtime :include
Output: your_code.c + hamt.c + pvec.c + atom.c
```

**REFERENCE**: point to an existing library.
```
Clojure -> Python with :runtime {:pmap "pyrsistent.PMap"}
Output: from pyrsistent import PMap; your_code.py
```

**ACCEPT**: use target's nearest equivalent, accept semantic differences.
```
Clojure -> Python with :runtime :accept
pmap -> dict (MUTABLE), pvec -> list (MUTABLE)
WARNING: mutability semantics differ
```

---

## genera as Universal Runtime

### The Key Insight

genera operates at level 0-1 (integers, arrays, bits). Its primitives (HAMT, pvec,
arena, sig) are optimal for the hardware. A HAMT is a HAMT. The optimal x86 for
HAMT lookup is the same regardless of whether Clojure, Scala, or Haskell needs it.

Because it's at the fundamental level, it can't be beaten.

### Two Deployment Modes

**Mode 1: genera AS the runtime** (programs run inside genera)

```
Your program (any language)
    -> parse with that language's grammar
    -> GNodes (universal)
    -> views (language-specific analysis)
    -> JIT to x86
    -> runs on genera's arena, HAMT, pvec, sig

Total: 128KB binary + your program = complete executable
```

Like the JVM, but 1000x smaller. No interpreter (everything JIT'd). No GC
(arena allocator). No startup cost.

**Mode 2: genera's primitives IN the output** (genera as library)

```
Transpile Clojure -> C?
    -> include genera's hamt.c (~200 lines)
    -> include genera's pvec.c (~150 lines)
    -> include genera's arena.c (~100 lines)
    -> standalone binary, no genera dependency
```

This IS what compilers do: GCC ships libgcc, Go ships its scheduler,
Rust ships its allocator. genera ships its primitives. The difference:
genera's runtime is ~700 lines, not 200MB of JVM.

### Why It's So Small

| Runtime | Size | Why |
|---------|------|-----|
| JVM | ~200MB | Interpreter + GC + class library + JIT |
| Go | ~2MB | GC + goroutine scheduler + networking |
| Rust | ~200KB | Allocator + panic handler + core traits |
| C (libc) | ~2MB | POSIX interface + math + stdio + locale |
| genera | ~128KB | Arena + HAMT + pvec + sig + x86 encoding |

genera is small because: no GC (arena: 3 ops), no interpreter (JIT everything),
no abstraction tax (primitives = machine ops), no OS dependency (freestanding).

### Tree-Shaking at the Grammar Level

Rules that don't match produce no output:

```clj
;; Program uses only arithmetic
(defn f [x] (+ (* x 2) 7))
;; -> LEA, RET. No HAMT, no pvec, no arena. ~20 bytes of x86.

;; Program uses persistent maps
(defn g [m] (assoc m :x 1))
;; -> HAMT + arena. ~800 bytes of x86.

;; Full program using everything -> ~128KB.
```

The runtime isn't a monolithic blob. It's grammar rules that fire (or don't)
based on what the program uses. Dead code elimination is free.

### The Universal Primitives

~700 lines covers the runtime needs of essentially every language:

| Primitive | Who needs it | genera impl |
|-----------|-------------|-------------|
| Integer arithmetic | Everyone | x86 native (0 lines) |
| Array access | Everyone | x86 native (0 lines) |
| Hash map | Clojure, Python, JS, Ruby | HAMT (~200 lines) |
| Vector/list | Everyone | pvec (~150 lines) |
| Concurrency | Clojure, Go, Erlang | atom/CAS (~50 lines) |
| Memory mgmt | Everyone | arena (~100 lines) |
| String | Everyone | interned StrId (~80 lines) |
| I/O | Everyone | syscall wrappers (~30 lines) |
| Dispatch | Polymorphic languages | sig (~100 lines) |

Every language runtime is an implementation of some subset of this table.
genera implements all of them, optimally, at the hardware level.

### genera Is Not a Compiler

genera is a grammar engine. The compiler, the runtime, the transpiler, the
query engine -- these are all the same mechanism: rules applied to fixed point.

```
Source (any language)
    -> grammar -> GNodes -> views -> GNodes+bits -> emit -> output (any target)
                            |
              genera runtime (~700 lines)
              HAMT  pvec  arena  atom  sig  StrId  syscall  x86
              included as needed (tree-shaken)

Output options:
  -> x86 binary (genera IS the runtime, 128KB)
  -> C source + genera primitives as .c files
  -> Python source + genera as C extension
  -> WASM + genera as WASM module
  -> any target + genera runtime in target's language
```

The compiler and the runtime are the same thing: rules that transform patterns
into output. The compiler rules produce code. The runtime rules produce data
structure operations. Same engine. One thing, small enough to carry anywhere,
powerful enough to run anything.

---

## The 10 Rule Shapes

Every pass in GCC (all ~400) is an instance of one of 10 shapes.
Only 6 are optimization patterns. The other 4 handle lowering, structure, allocation, layout.

### Optimization Shapes (1-6)

All native to the graph + bitmask engine:

**Shape 1: PROPAGATE DOWN** (node + children -> node)

```
P(N) :- literal(N).
P(N) :- op(N), P(left(N)), P(right(N)), compatible(op(N)).
```

Direction: bottom-up (leaves to root). Match: span (children contiguous).
GCC: CCP x5, VRP x3, pure_const x3, nothrow, modref x4. (~15 invocations)

**Shape 2: PROPAGATE UP** (uses -> node)

```
dead(N)  :- def(N), not-exists use(N, _).
needed(N) :- use(N, U), needed(U).
```

Direction: top-down (root to leaves, or uses to defs). Match: NOT a span (uses anywhere).
Mechanism: `uses[]` back-edge array (inverted `bind[]`).
GCC: DCE x8, cd_dce x3, DSE x5, tail x2, sink x2, rtl variants. (~25 invocations)

**Shape 3: FOLLOW CHAIN** (edge -> edge -> ...)

```
equiv(N) :- copy(N, M), equiv(M).
equiv(N) :- single_use(N), value(N).
```

Direction: forward along edges, multi-hop. Match: NOT a span.
Mechanism: `bind[]` + fixed-point iteration.
GCC: copy_prop x4, forwprop x5, sccopy x2, phiprop x2, phiopt x4, merge_phi x2,
     rtl_cprop x3, rtl_fwprop x2, cprop_hardreg. (~25 invocations)

**Shape 4: MATCH SUBTREE** (algebraic template)

```
match: (A * const) where is_pow2(const) -> (A << log2(const))
match: (A - A) -> 0
match: (A + 0) -> A
```

Direction: local (node + immediate children). Match: span (subtrees contiguous).
GCC: reassoc x2, expand_pow, bswap, sincos, reciprocals, widening_mul,
     strength_reduction, store_merging, combine x2, peephole2, backprop. (~15 invocations)

**Shape 5: REGION QUERY** (property over bounded set)

```
loop_invariant(N, L) :- in_loop(N, L), forall dep(N, D). not in_loop(D, L).
escape(N, S) :- ref(N), scope(N) = S, exists use(N, U). scope(U) != S.
```

Direction: range check. Match: span (loops/functions are contiguous), test is universal.
Mechanism: `bm_and(view, range)` -- range quantification on bitmask.
GCC: lim x3, unswitch, loop_split, scev, versioning, jam, iv_canon, distribution,
     interchange, unroll x2, vectorize x3, sra x2, ch x2, prefetch, predcom. (~25 invocations)

**Shape 6: REDUNDANCY** (value identity across locations)

```
redundant(N) :- exists M. M != N, same_value(N, M), dominates(M, N).
```

Direction: global (compare N against all others). Match: NOT a span.
Mechanism: `expr_hash[]` parallel array for value numbering.
GCC: FRE x5, CSE x4, PRE, gcse2, dominator x2, rtl_pre, rtl_hoist, ipa_icf. (~15 invocations)

### Non-optimization shapes (7-10)

**Shape 7: DECOMPOSE** (lower high-level -> low-level)

Creates NEW GNodes. The grammar running in the parse direction (pattern -> new nodes).

| Pass | What | C relevance |
|------|------|-------------|
| lower_switch | switch -> if-chain / jump table | Essential |
| lower_cf | break/continue/goto -> basic blocks | Essential |
| lower_eh | try/catch -> save/restore + landing pads | C++ / setjmp |
| lower_complex | _Complex -> real + imag | C99 |
| lower_bitint | _BitInt(N) -> word sequences | C23 |
| lower_vector | vector -> scalar fallback | SIMD intrinsics |
| lower_subreg | wide refs -> narrow ops | Essential |
| lower_vaarg | va_arg -> explicit stack access | Essential |
| sra | struct -> scalar members | Essential |
| build_ssa | mutable vars -> SSA + PHI | Essential for C |
| expand | GIMPLE -> RTL | The big translation |

For genera: lowering rules are grammar rules whose output is new GNodes.
Same engine, different grammar.

**Shape 8: DERIVE STRUCTURE** (compute graph edges)

Rules that produce parallel arrays (edges) instead of single bitmasks.

| Pass | What | C relevance |
|------|------|-------------|
| build_cfg | Basic blocks + CFG edges | Essential (C has goto) |
| build_cgraph | Call graph edges | Essential (all languages) |
| build_alias | Alias information (pointer -> target) | Essential for C |
| build_ssa | Def-use chains, PHI nodes | Essential for C |
| ipa_pta | Points-to sets (pointer -> objects) | Important for C |

For genera: rules that produce parallel arrays. Same mechanism, wider output.

**Alias analysis is the gate for C optimization.** Without it, every pointer write
may invalidate every pointer read. With it, shapes 1-6 unlock for memory operations.

```
rule: points_to
  reads: V_DEF, V_REF, V_ADDRESSOF, bind[]
  writes: points_to[ptr] = bitmask of possible targets

rule: may_alias
  reads: points_to[]
  writes: V_NOALIAS (for pairs where points_to sets don't overlap)
  test: bm_and(points_to[p], points_to[q]) == 0  ->  V_NOALIAS(p, q)
```

One SIMD instruction per pointer pair.

**Shape 9: ALLOCATE** (assign physical resources)

```
rule: register_allocate
  reads: V_LIVE (liveness per instruction), interference graph
  writes: reg[node_id] = physical register (u8 parallel array)
```

Register allocation via liveness bitmasks:
- Interference: `bm_and(live[a], live[b]) != 0` -> a and b can't share register
- Coloring: assign registers such that no two interfering values collide
- Spill: if live count > register count, store to stack

~200 lines. The one genuinely procedural algorithm (graph coloring).

**Shape 10: LAYOUT** (order code for hardware)

Operates on emitted code buffer, not GNodes.
- Instruction scheduling (avoid pipeline stalls): dependency bitmask per insn
- Block reordering (hot path falls through): weighted topological sort
- Branch offset computation: linear pass, fix up relative addresses

~100 lines. Second pass of same engine on different flat array.

### Summary table

| # | Shape | Produces | GCC invocations | Native? |
|---|-------|----------|-----------------|---------|
| 1 | Propagate down | bits | ~15 | Yes - child edges |
| 2 | Propagate up | bits | ~25 | Yes - use edges (inverted bind) |
| 3 | Follow chain | bits | ~25 | Yes - bind + iteration |
| 4 | Match subtree | bits or code | ~15 | Yes - span pattern |
| 5 | Region query | bits | ~25 | Yes - bm_and over range |
| 6 | Redundancy | bits | ~15 | Yes - hash + bitmask |
| 7 | Decompose | new GNodes | ~30 | Grammar (parse direction) |
| 8 | Derive edges | parallel arrays | ~25 | Rule -> array |
| 9 | Allocate | u8 array | ~10 | Procedural (~200 lines) |
| 10 | Layout | reordered buffer | ~10 | Weighted sort (~100 lines) |

GCC's remaining ~200 pass slots: infrastructure bookkeeping (rebuild_cfg, fixup,
release_ssa -- not needed with immutable data), warnings/diagnostics, sanitizer
instrumentation, IPA framework.

---

## What C Compilation Needs Beyond Lisp

| Need | Why | genera mechanism | Shape |
|------|-----|-----------------|-------|
| SSA construction | C has mutable locals | V_SSA_DEF, V_PHI, reaching_def[] | 7, 8 |
| Control flow graph | C has if/for/goto/break | cfg_succ[], cfg_pred[], V_BLOCK | 8 |
| Alias analysis | C has unrestricted pointers | points_to[], V_NOALIAS | 8, 1 |
| Struct decomposition | C has aggregates | SRA: struct -> scalars | 7 |
| Switch lowering | C switch is complex | -> if-chain / jump table | 7 |
| Variadic functions | C va_arg | -> explicit stack access | 7 |
| Complex arithmetic | C99 _Complex | -> real + imag pairs | 7 |
| Register allocation | Higher pressure (mutables) | Liveness bitmask coloring | 9 |

Alias analysis is the single biggest addition. It gates whether shapes 1-6
can safely apply to memory operations. Everything else is lowering rules (shape 7)
that the grammar engine already supports.

View count scales with language complexity:

| Language | View passes | Views | Est. rule lines |
|----------|------------|-------|-----------------|
| Brainfuck | ~1 | ~3 | ~30 |
| Lisp | ~3 | ~11 | ~240 |
| C | ~10 | ~30 | ~800 |
| C++ | ~15 | ~50 | ~1,500 |

---

## Source Organization

The boundaries between directories dissolve. `lang/`, `rt/`, `plt/` impose categories
the architecture rejects. Everything is "scan set, test predicate, act."

The real boundaries:

1. **sys/** -- the effect boundary (x86 encoding, syscalls, mmap).
   Real and permanent. Where facts become electrons.

2. **Everything else** -- the language operating on itself.
   Load order is the only structure. Views, passes, emit, coll, sig -- all
   "same operation, different domain."

At scale (hundreds of grammars, hundreds of views), organize by WHAT rules describe,
not WHAT rules DO (they all do the same thing):

```
grammars/                 rules (growing, numerous)
  lang/                   syntax rules (one per language, independent)
    c.edn
    clojure.edn
    python.edn
    ...
  view/                   property rules (one per analysis)
    const.clj             V_CONST -- constant propagation
    dead.clj              V_DEAD -- dead code
    pure.clj              V_PURE -- purity analysis
    alias.clj             V_NOALIAS -- alias analysis (for C)
    escape.clj            V_ESCAPE -- escape analysis
    loop_inv.clj          V_LOOP_INV -- loop invariant motion
    redundant.clj         V_REDUNDANT -- common subexpression
    ...                   each ~20-80 lines, declares reads/writes
  target/                 machine descriptions
    x86.edn
    arm.edn
    wasm.edn
  runtime/                axiom implementations per target
    hamt.clj              persistent hash map (HAMT algorithm)
    pvec.clj              persistent vector (trie + tail)
    arena.clj             bump/restore/copy allocator
    atom.clj              compare-and-swap reference

src/                      the engine (small, stable)
  genera.clj              boot entry
  sys.clj                 effect boundary
  core.clj                macros, stdlib
  mem.clj                 allocator
  engine.clj              rule evaluator + stratifier + fixed point
```

The engine computes rule ordering from declared dependencies:

```clj
(defview escape
  :reads  #{:V_ALLOC :V_CALL :V_REF}
  :writes #{:V_ESCAPE :V_LOCAL}
  :pass   (fn [g id] ...))
```

Engine builds dependency DAG, computes strata, evaluates to fixed point.
No manual pass scheduling. No passes.def. Add a rule, the engine integrates it.

---

## Concrete Example: C Through genera

```c
int process(int x) {
    int a = x * 2;       // strength reduce: shl
    int b = 3 + 4;       // const fold: 7
    int c = a + b;
    if (b > 10) {        // const fold: false -> dead branch
        c = 999;
    }
    return c;             // -> (x << 1) + 7
}
```

### Step 1: Parse -> GNodes

```
id  kind    name     left  right  parent
0   FN      process   -     -      -
1   PARAM   x         -     -      0
2   BINOP   *         1     3      0       a = x * 2
3   NUM     2         -     -      2
4   BINOP   +         5     6      0       b = 3 + 4
5   NUM     3         -     -      4
6   NUM     4         -     -      4
7   BINOP   +         2     4      0       c = a + b
8   BINOP   >         4     9      0       b > 10
9   NUM     10        -     -      8
10  IF      -         8     11     0
11  NUM     999       -     -      10
12  RET     -         7     -      0
```

13 nodes. 364 bytes. All strings interned. Zero strings after this point.

### Step 2: Evaluate rules to fixed point

Stratum 0 -- scope rules:
```
V_DEF:  {1, 2, 4, 7}     bind[]: 2->1, 7->2, 7->4, etc.
V_REF:  {uses of x, a, b, c}
```

Stratum 1 -- type rules (3 rounds):
```
Round 1: V_INT: {3,5,6,9,11}  V_CONST: {3,5,6,9,11}  val: 3->2, 5->3, 6->4, 9->10, 11->999
Round 2: V_INT: +{2,4}  V_CONST: +{4(=7), 8(=false)}   val: 4->7, 8->false
Round 3: V_INT: +{7}    (no new V_CONST -- node 7 depends on x)
Round 4: no new bits. Fixed point.
```

Stratum 2 -- flow rules (1 round):
```
V_DEAD: {10, 11}    (if-branch: condition is V_CONST false)
```

Final state:
```
Node: 0  1  2  3  4  5  6  7  8  9  10 11 12
      FN x  *  2  +  3  4  +  >  10 if 999 ret
INT:  .  1  1  1  1  1  1  1  .  1  .  1  .
CONST:.  .  .  1  1  1  1  .  1  1  .  1  .
DEAD: .  .  .  .  .  .  .  .  .  .  1  1  .
```

4 rounds, ~50 ns total.

### Step 3: Emit (most specific rule wins)

```
node 0  (FN):       prologue
node 1  (x):        param in rdi
node 2  (x*2):      V_INT, right=V_CONST(2), pow2 -> SHL     emit: shl rdi, 1
node 3  (2):        V_CONST ^ V_INT -> skip (folded)
node 4  (3+4):      V_CONST ^ V_INT -> skip (val=7)
node 5  (3):        V_CONST -> skip
node 6  (4):        V_CONST -> skip
node 7  (a+b):      V_INT, right=V_CONST(7) -> ADD imm       emit: add rdi, 7
node 8  (b>10):     V_CONST -> skip
node 9  (10):       V_CONST -> skip
node 10 (if):       V_DEAD -> skip
node 11 (999):      V_DEAD -> skip
node 12 (ret):      RET                                       emit: mov rax, rdi; ret
```

Peephole: `shl 1 + add imm` -> `lea`:

```asm
process:
    lea  eax, [rdi+rdi+7]
    ret
```

Same result as GCC -O2. No pass scheduling. No IR rewriting.

### What happened to GCC's 6 passes?

| GCC pass | genera rule | How |
|----------|------------|-----|
| CCP (const prop) | v_const, round 2 | val[4]=7, val[8]=false |
| DCE (dead code) | v_dead, round 1 | V_DEAD on nodes 10,11 |
| forwprop | implicit | emitter reads val[] directly |
| copy prop | not needed | no mutable variables |
| strength reduction | emit rule | * pow2 -> SHL in grammar |
| combine (LEA) | peephole emit rule | SHL+ADD -> LEA |

---

## GCC Pass Decomposition

386 pass invocations. Strip non-optimization:

| Category | Count | What |
|----------|-------|------|
| Infrastructure | ~25 | Rebuild CFG/SSA/callgraph (not needed: immutable) |
| Warnings | ~15 | Diagnostics |
| Lowering | ~30 | Language feature decomposition |
| Sanitizers | ~10 | Runtime instrumentation |
| Machine-specific | ~45 | Regalloc, scheduling, frame, final |
| IPA framework | ~20 | Visibility, summaries, LTO |
| **Optimization** | **~120** | **The real patterns** |

120 optimization invocations. ~40 unique passes. 6 shapes.
10 shapes total including lowering, structure, allocation, layout.

The ~120 optimization invocations by shape:

| Shape | Invocations | Unique passes | Example |
|-------|-------------|---------------|---------|
| Propagate down | ~15 | CCP, VRP, pure_const | v_const, v_pure |
| Propagate up | ~25 | DCE, DSE, tail, sink | v_dead, v_tail |
| Follow chain | ~25 | copy_prop, forwprop, phiopt | equivalence chain |
| Match subtree | ~15 | reassoc, combine, peephole | algebraic identity |
| Region query | ~25 | lim, vectorize, unroll, sra | loop/scope properties |
| Redundancy | ~15 | FRE, CSE, PRE, dominator | value numbering |

---

## Applications

The grammar engine enables these as views over the same flat data:

- **Hardware introspection**: cache hierarchy as specificity-ordered rules (L1→L2→L3→RAM),
  trace events as facts, aggregations as view queries over bitmasks
- **Code intelligence**: language grammars as Lang structs, call/def graphs as HAMT triples,
  analysis (purity, liveness, trust) as views computed to fixed point
- **Self-observation**: genera inspecting its own cache behavior, analyzing its own source,
  using the same engine it's built on

---

## References

- VIEW.md -- Datalog model, six operations, memory tiers
- PASSES.md -- GCC/TCC comparison, current views, line-count analysis
- LANGUAGE.md -- Clojure dialect, load order
- GENERA.md -- overall architecture, world model
- SBCL ref (C:\Proj\genera\ref\sbcl) -- cost model: per-VOP cost, LTN policy, template selection
- GCC ref (ref/gcc/gcc/passes.def) -- 577-line canonical pass schedule
