# Allocator — Memory as View

> The bitmask scan that computes "is this node pure?" and the
> bitmask scan that computes "is this allocation live?" are the
> same operation on the same address space.

## One Atom

```c
typedef struct { u8 *base; u32 used, cap; } Mem;
```

Three fields. Instantiate N times. Views tell you what to do with each.
`peak` (high-water mark) is a derived fact — `max(used)` from sig events, not a struct field.

## Three Ops

```c
// bump — allocate (3 instructions + 1 branch, provably minimal safe alloc)
void *mem(Mem *m, u32 size, u32 align);

// RESTORE — bulk free (1 store, provably minimal)
#define MARK(m)       ((m)->used)
#define RESTORE(m, v) ((m)->used = (v))

// copy — move live data between Mem instances (memcpy speed, ~20 GB/s)
// commit() and epoch rotation are both "copy"
```

| Op | Cost | Can you beat it? |
|---|---|---|
| V_CONST (eliminated) | 0 | No — allocation doesn't exist |
| V_ALLOC_OFF (LEA) | 1 instruction | No — one add is minimum |
| RESTORE | 1 store | No — one write is minimum bulk free |
| bump (mem()) | 3 + branch | No — align + compare + store |
| copy | ~20 GB/s | No — memory bandwidth is the floor |

Everything else is views choosing which op, and sig handling conditions.

## Views Classify

Same bitmask scan as V_PURE, V_DEAD. Same infrastructure. Different timing.

```
View            When              What it derives
────            ────              ───────────────
V_CONST         analyze time      value known — no allocation
V_ALLOC_OFF     analyze time      offset known — one LEA, no call
V_SCOPE         analyze time      doesn't escape — MARK/RESTORE
V_DYNAMIC       analyze time      unknown count — runtime bump
V_LIVE          runtime           reachable from root — survives
```

These aren't five strategies. They're five outputs of one mechanism
(scan a set, test a predicate, produce bits). The view pushes every
node as high up the cascade as possible. The JIT/eval/cc reads the
classification and emits the corresponding op.

### What the views can see

| Node kind | Allocates | Size | Static? |
|---|---|---|---|
| int, sym, kw, bool, nil | nothing (immediate) | 0 | yes |
| call to builtin | nothing (most) | 0 | yes |
| call to closure | 1 Env | sizeof(Env) | yes |
| fn (closure) | 1 FnObj | 24B | yes |
| quoted list | N cons | N × 16B | yes (count children) |
| let binding | 1 Env | sizeof(Env) | yes |
| def | 1 commit | walk size | yes |
| non-tail recursion | unbounded Env | unknown | no → V_DYNAMIC |
| dynamic dispatch | unknown target | unknown | no → V_DYNAMIC |

### Computing V_ALLOC_OFF

```c
V_ALLOC,        // does this node allocate? (1-bit per node)
V_ALLOC_SIZE,   // how many bytes? (u32 per node, from node kind)
V_ALLOC_OFF,    // offset into buffer (prefix sum of V_ALLOC_SIZE)

u32 off = 0;
for (u32 i = 0; i < g->n; i++) {
    if (BM_GET(g->v[V_ALLOC], i)) {
        g->val[i] = off;
        off += alloc_size(g, i);
    }
}
u32 step_budget = off;   // total bytes this step needs
```

One bounds check per step, not per allocation. The JIT emits:

```c
// One bounds check for entire step
u8 *base = g_step.base + g_step.used;
if (step_budget > g_step.cap - g_step.used) {
    send(S_MEM_OVERFLOW, ...);
    return NIL;
}
g_step.used += step_budget;

// Per allocation site: one LEA, no call, no branch
//   lea rax, [r12 + 48]    ; r12 = base, 48 = offset
```

### Manual arenas

```clojure
(with-arena
  (let [big (build-expensive-thing)]
    (extract-result big)))
; big reclaimed at scope exit, result escapes
```

Compiles to MARK at entry, RESTORE at exit. The views see the
scope boundary. Escape analysis determines what dies vs escapes.
Same mechanism whether the view derives it (V_SCOPE) or the
programmer requests it (`with-arena`).

## Derivation

Start with one Mem. Each problem forces a split.

**Problem 1:** Step temps accumulate. Pool fills after a few evals.

V_STEP marks nodes that don't escape the step. They're all at the
top (bump order). → RESTORE to pre-step offset. O(1).

→ **Split: step Mem** (stack discipline, RESTORE reclaims)

**Problem 2:** `def`'d values must survive RESTORE.

V_COMMIT marks nodes reached by `def`. They can't live in the
RESTORE zone. → Copy them somewhere RESTORE doesn't touch.

→ **Split: main Mem** (bump only, committed values)

**Problem 3:** Coll nodes die in scattered positions.

`(assoc m :key val)` creates new HAMT path nodes. Old path nodes
are garbage IF no other handle shares them. Structural sharing means
dead nodes are interleaved with live ones — can't RESTORE (not stack
discipline). V_LIVE marks reachable nodes. → Copy live to fresh Mem,
RESTORE old.

→ **Split: two coll Mems** (copy live between them)

**Problem 4:** Concurrent readers hold references to old coll nodes.

Can't RESTORE the old Mem while readers are in it. → sig: readers
announce entry/exit. Writer waits for quiescence. Then RESTORE.

→ **World.epoch + sig announcements** (already in the model)

Four problems. Four splits. Four consequences derived from views
hitting physical constraints:

```
V_STEP    + stack discipline  → RESTORE      → step Mem
V_COMMIT  + survives restore  → copy out     → main Mem
V_LIVE    + scattered dead    → copy + reset  → two coll Mems
quiescence + concurrent reads → sig announce  → World.epoch
```

Not chosen. Derived.

## commit(): The Copy Boundary

When `def` binds a name, copy the value from step to main so it
survives step RESTORE. Walk guided by val tags:

```c
static Val commit(Val v) {
    if (!val_is_heap(v)) return v;   // immediates: sym, kw, int, f64, bool, nil

    if (val_is_cons(v)) {
        Cons *c = PUSH(&g_main, Cons);
        c->car = commit(car(v));
        c->cdr = commit(cdr(v));
        return val_cons(c);
    }
    if (val_is_fn(v)) {
        FnObj *old = val_as_fn(v);
        if (old->type == FN_BUILTIN) return v;   // already in main
        FnObj *f = PUSH(&g_main, FnObj);
        *f = *old;
        f->closure.params = commit(old->closure.params);
        f->closure.body   = commit(old->closure.body);
        f->closure.env    = commit_env(old->closure.env);
        return val_fn(f);
    }
    if (val_is_str(v)) {
        Str *old = val_as_str(v);
        Str *s = PUSH(&g_main, Str);
        s->data = PUSH_N(&g_main, u8, old->len);
        memcpy(s->data, old->data, old->len);
        s->len = old->len;
        return val_str(s);
    }
    if (val_is_pmap(v)) {
        CPMap *m = PUSH(&g_main, CPMap);
        *m = *val_as_pmap(v);       // handle only (12 bytes)
        return val_pmap(m);         // tree data shared, in coll Mem
    }
    if (val_is_pvec(v)) {
        CPVec *vec = PUSH(&g_main, CPVec);
        *vec = *val_as_pvec(v);     // handle only (20 bytes)
        return val_pvec(vec);       // trie data shared, in coll Mem
    }
    return v;
}
```

**Coll handles vs tree data:** commit copies the small handle (12-20B)
to main. The tree nodes stay in coll Mem — structurally shared across
versions. Reclaimed by V_LIVE copy when no handle references them.

**Cost:** Typical `defn` body = 10-50 cons cells referencing interned
StrIds (u32). 160-800 bytes. Trivial.

## V_LIVE: Coll Reclamation

Same shape as every other view. Scan a set, test a predicate, act.

| View | Scans | Predicate | Action |
|---|---|---|---|
| V_PURE | GNode[] | all children pure? | mark pure |
| V_DEAD | GNode[] | referenced? | mark dead |
| V_LIVE | coll Mem | reachable from handle? | copy to other Mem |

```c
static void epoch_rotate(void) {
    Mem *from = &g_coll[g_epoch];
    Mem *to   = &g_coll[1 - g_epoch];
    to->used  = 0;

    // Walk all pmap/pvec handles in g_global_env
    // Copy reachable coll nodes from → to
    // u32-indexed — rewrite indices during copy, no forwarding table
    for (Env *e = g_global_env; e; e = e->parent) {
        for (u32 i = 0; i < e->count; i++) {
            Val v = e->vals[i];
            if (val_is_pmap(v))  copy_pmap_nodes(val_as_pmap(v), from, to);
            if (val_is_pvec(v))  copy_pvec_nodes(val_as_pvec(v), from, to);
        }
    }

    g_epoch = 1 - g_epoch;
}
```

**Why this works:**
- 90%+ of allocations are step temps — never reach V_LIVE
- Transients batch mutations, reuse nodes in place — less garbage
- Coll nodes are u32-indexed — no forwarding table, indices rewritten during copy
- Walks HANDLES (few), not heap — O(handles × tree depth)
- Depth = log32(count) ≈ 3-4 for millions of entries
- Triggered when coll Mem fills, not periodic — zero cost until then

## sig: Conditions + Events

Memory events, fired at low frequency (per-step, not per-object).
Zero overhead when no handlers registered.

```
Signal          When                        Data
──────          ────                        ────
mem:step        step begins                 {budget, used, cap}
mem:commit      def promotes a value        {name, size}
mem:overflow    allocation fails            {requested, available}
mem:epoch       coll reclamation runs       {copied, freed, handles}
mem:scope       with-arena exits            {bytes_used}
```

### Overflow as condition

When `mem()` returns NULL, fire a condition. Handler chooses a
restart — same CL condition/restart protocol. Stack preserved.

```c
if (UNLIKELY(end > m->cap)) {
    Val restart = send(S_MEM_OVERFLOW, make_data(size));
    if (restart == S_EPOCH_ROTATE) {
        epoch_rotate();
        return mem(m, size, align);     // retry after rotation
    }
    g_signal = SIGNAL_ERROR;
    return NULL;
}
```

### Budget-limited scopes

```clojure
(with-budget 4096
  (do-work))
```

Pre-step budget check. If `step_budget > limit`, fire `mem:overflow`
before any side effects.

## Memory Safety

No dangling references. Three cases, each proven safe:

- **RESTORE:** view (V_STEP/V_SCOPE) proves nothing references the
  region. Escape analysis at analyze time, not runtime.
- **commit:** deep-copies escaping values before RESTORE. Self-contained
  after copy — no cross-Mem references.
- **V_LIVE copy:** copies all reachable nodes before resetting old Mem.
  Concurrent: sig quiescence ensures no readers in old Mem.

No free(). No use-after-free. No double-free. Safety by construction.

## Concurrency

Same atom, same ops. Thread isolation is natural:

| Mem instance | Model | Why |
|---|---|---|
| step | thread-local | each worker gets its own |
| main | single writer (primary) | others read copies |
| coll (reads) | lock-free | persistent nodes are immutable |
| coll (reclaim) | sig quiescence | readers announce, writer waits |

World.epoch already exists. sig already does cross-worker announcements.
Quiescence = bitmask query: "all reader bits clear for epoch N?"
Same infrastructure. No new machinery.

## Image

All state lives in contiguous Mem instances. Coll nodes use u32
indices (position-independent). To serialize:

```c
// Save: write each Mem's [base..base+used] + roots
write(fd, m->base, m->used);

// Load: mmap, set base pointers
m->base = mmap_region;
```

sig = all behavior as names (serializable). Mem = all state as bytes.
Together = the image. Same concept as Moss (sig + pmap = managed heap),
expressed as contiguous memory.

## Val Tag → Mem Instance

The NaN-boxed type tag determines where a value lives.
No parameter needed — the type IS the policy.

| Val tag | Mem instance | Derived from |
|---|---|---|
| nil, bool, int, f64, sym, kw | nowhere (immediate) | — |
| cons, fn (closure), str | step | V_STEP — dies at boundary |
| fn (builtin) | main | permanent (init-time) |
| pmap/pvec handle | step | V_COMMIT — copied by commit |
| coll nodes (HAMT, trie) | coll[epoch] | V_LIVE — scattered dead |

## Init

```c
static Mem g_step;
static Mem g_main;
static Mem g_coll[2];
static u32 g_epoch;

static void mem_init(void) {
    u32 step_size = 8 << 20;     //   8 MB step scratch
    u32 main_size = 16 << 20;    //  16 MB committed values
    u32 coll_size = 256 << 20;   // 256 MB per coll epoch

    u8 *base = (u8 *)sys_alloc(step_size + main_size + 2 * coll_size);
    u8 *p = base;
    g_step    = (Mem){ p, 0, step_size };  p += step_size;
    g_main    = (Mem){ p, 0, main_size };  p += main_size;
    g_coll[0] = (Mem){ p, 0, coll_size };  p += coll_size;
    g_coll[1] = (Mem){ p, 0, coll_size };
    g_epoch   = 0;
}
```

One mmap. Contiguous. Demand-paged — only touched pages consume RAM.
Main is small (committed values are few). Coll epochs are large but
only the active one uses physical memory.

## Call Sites

```c
// Step: eval temps
Val  cons_new(Val a, Val d)              { Cons *c = PUSH(&g_step, Cons); ... }
Val  make_closure(Val p, Val b, Env *e)  { FnObj *f = PUSH(&g_step, FnObj); ... }
Env *env_create(Env *parent)             { Env *e = PUSH(&g_step, Env); ... }

// Main: init-time builtins
Val  make_builtin(StrId n, BuiltinFn f)  { FnObj *fn = PUSH(&g_main, FnObj); ... }

// Coll: persistent data structure nodes
u32 coll_alloc_node(u32 n)         { ... PUSH_N(&g_coll[g_epoch], ...); ... }
u32 coll_alloc_leaf(u32 k, Val v)  { ... PUSH(&g_coll[g_epoch], CLeaf); ... }

// Promotion
env_set(g_global_env, name, commit(val));

// Step boundary
RESTORE(&g_step, 0);                                    // O(1)
if (g_coll[g_epoch].used > g_coll[g_epoch].cap * 3 / 4)
    epoch_rotate();                                      // V_LIVE
```

## Migration

Current: `Arena g_temp, g_req, g_perm` + `g_coll` (5 sub-pools).

1. Add `Mem g_step, g_main, g_coll[2]` and `mem_init()`. Keep old arenas.
2. Replace `arena_alloc(&g_req, ...)` → `mem(&g_step, ...)`.
3. Replace `arena_alloc(&g_perm, ...)` → `mem(&g_main, ...)`.
4. Replace `g_coll` alloc functions → `mem(&g_coll[g_epoch], ...)`.
5. Replace `arena_reset(&g_req)` → `RESTORE(&g_step, 0)`.
6. Add `commit()` to `sf_def` / `sf_defn`.
7. Add `epoch_rotate()` (V_LIVE copy).
8. Remove Arena, ArenaBlock, ArenaMark structs.
9. Remove `coll_init` / `coll_cleanup` (absorbed into `mem_init`).

Each step independently testable.

## Measured (parity/bench_mem.c)

```
Operation                        ns          vs malloc
─────────                        ──          ─────────
V_CONST (eliminated)             0.2         —
V_ALLOC_OFF (pre-computed)       1.0         7.3x
MARK/RESTORE (scoped)            0.3         27x (vs malloc+free)
bump (V_DYNAMIC)                 1.3         5.5x
malloc                           7.3         baseline
commit (per cons cell)           0.6–1.3     —
V_LIVE copy (per handle)         1.6–7.0     —
copy throughput                  15–21 GB/s  near memcpy

Full cycle (10% committed):      1.3 ns/alloc vs 9.2 ns (7.2x)
```

The cascade is real. Each level is a measurable step down.
The view's job: push every node as high as possible.
Step RESTORE handles 90%+. V_LIVE handles the rest — incrementally.
