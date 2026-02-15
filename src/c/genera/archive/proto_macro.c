/**
 * proto_macro.c — X-Macro + Pool Allocator Exploration
 *
 * Three File Pilot patterns mapped to our codebase:
 *
 *   1. X-macro protocol spec   → "command spec → dispatch table"
 *      One definition generates: enum, tables, init, tier-1 dispatch, extend.
 *      Adding a protocol = 1 line.  Adding a type = 1 line.  Adding an impl = 1 line.
 *
 *   2. Pool allocator           → POOL_* for fixed-size objects (Cons cells)
 *      Intrusive free list over arena slabs. Same bump speed, better density.
 *
 *   3. X-macro special forms    → compiler dispatch from spec
 *      One table defines form→handler. The if-chain compiles itself.
 *
 * Build: gcc -O3 -march=native -nostdlib -static -o proto_macro test/proto_macro.c
 */

// Pull in runtime (sys → std → rt) + reader
#include "../rt/rt.c"
#include "../lang/read.c"

// ============================================================================
// 1. X-MACRO PROTOCOL SPEC
// ============================================================================
//
// THE spec. One place. Everything else is generated.
//
// Convention:
//   PROTO_<arity>(X)  — declares protocols by arity (needed for default fn)
//   TAG_TYPES(X)      — declares type enum entries
//   PROTO_IMPLS(X)    — declares type→protocol→fn bindings
//
// These three macros replace:
//   - The TI_ enum (manual)
//   - The P_* table declarations (manual)
//   - The proto_init() function (manual, error-prone)
//   - The extend() calls scattered through code

// --- The Spec ---

// X(name)  — unary protocols: Val → Val
#define XPROTO_1(X)  X(first) X(rest) X(seq) X(count)

// X(name)  — binary protocols: (Val, Val) → Val
#define XPROTO_2(X)  X(get)

// X(name)  — ternary protocols: (Val, Val, Val) → Val
#define XPROTO_3(X)  X(assoc)

// X(name, index)  — all NaN-box types
// Index must match val_tag_idx() output.
// NOTE: in base.c these are already defined. Here we show how to generate them.
#define XTAG_TYPES(X)  \
    X(nil,  0)  \
    X(bool, 1)  \
    X(int,  2)  \
    X(sym,  3)  \
    X(kw,   4)  \
    X(str,  5)  \
    X(pmap, 6)  \
    X(pvec, 7)  \
    X(fn,   8)  \
    X(cons, 9)  \
    X(f64, 10)

// X(type, protocol, fn)  — all impl registrations
// This IS the command spec → dispatch table.
#define XPROTO_IMPLS(X)               \
    X(nil,  first, _nil_first)        \
    X(nil,  rest,  _nil_rest)         \
    X(nil,  seq,   _nil_seq)          \
    X(nil,  get,   _nil_get)          \
    X(nil,  count, _nil_count)        \
    X(cons, first, _cons_first)       \
    X(cons, rest,  _cons_rest)        \
    X(cons, seq,   _cons_seq)         \
    X(cons, count, _cons_count)       \
    X(str,  count, _str_count)        \
    X(str,  seq,   _str_seq)          \
    X(kw,   get,   _kw_get)

// --- Generated Code ---

// 1a. Enum (would replace the manual enum in base.c)
//     Generated: XI_nil=0, XI_bool=1, ... XI_f64=10
#define X_ENUM(name, idx) XI_##name = idx,
enum { XTAG_TYPES(X_ENUM) XI_COUNT };
#undef X_ENUM

// 1b. Table declarations (would replace manual P_* lines)
//     Generated: static PFn1 XP_first[XI_COUNT]; ...
#define X_TABLE1(name) static PFn1 XP_##name[XI_COUNT];
#define X_TABLE2(name) static PFn2 XP_##name[XI_COUNT];
#define X_TABLE3(name) static PFn3 XP_##name[XI_COUNT];
XPROTO_1(X_TABLE1)
XPROTO_2(X_TABLE2)
XPROTO_3(X_TABLE3)
#undef X_TABLE1
#undef X_TABLE2
#undef X_TABLE3

// 1c. extend macro — same as base.c, works with generated tables
#define xextend(method, type_idx, fn) (XP_##method)[type_idx] = (fn)

// 1d. Init function — ENTIRELY generated from the spec
static void xproto_init(void) {
    // Defaults: fill all slots with error handler
    #define X1(name) for (u32 i = 0; i < XI_COUNT; i++) XP_##name[i] = _perr1;
    XPROTO_1(X1)
    #undef X1
    #define X2(name) for (u32 i = 0; i < XI_COUNT; i++) XP_##name[i] = _perr2;
    XPROTO_2(X2)
    #undef X2
    #define X3(name) for (u32 i = 0; i < XI_COUNT; i++) XP_##name[i] = _perr3;
    XPROTO_3(X3)
    #undef X3

    // Impls: one line per binding, expanded from XPROTO_IMPLS
    #define X_IMPL(type, proto, fn) XP_##proto[XI_##type] = (void *)(fn);
    XPROTO_IMPLS(X_IMPL)
    #undef X_IMPL
}

// 1e. Tier 1 dispatch — generated
#define X_DISPATCH1(name) \
    ALWAYS_INLINE Val xt_##name(Val v) { return XP_##name[val_tag_idx(v)](v); }
XPROTO_1(X_DISPATCH1)
#undef X_DISPATCH1

#define X_DISPATCH2(name) \
    ALWAYS_INLINE Val xt_##name(Val a, Val b) { return XP_##name[val_tag_idx(a)](a, b); }
XPROTO_2(X_DISPATCH2)
#undef X_DISPATCH2

// 1f. Tier 2 fast path — these are hand-written (pattern-specific)
//     The spec generates the table; the fast path is opt-in per protocol.
//     This is the right split: spec for the generic, hand-craft the hot path.

// ============================================================================
// 2. POOL ALLOCATOR
// ============================================================================
//
// File Pilot's POOL_* pattern: intrusive free list over arena-backed slabs.
// For fixed-size objects (Cons = 16 bytes), this gives:
//   - Same bump speed as arena (free list hit = 2 loads + 1 store)
//   - Better density (no alignment gaps for small objects)
//   - Free individual objects back to pool (impossible with arena)
//   - Slab = 4096 bytes / 16 = 256 Cons cells per page
//
// The free list is intrusive: when a Cons cell is free, its first 8 bytes
// store the next-free pointer. When allocated, those 8 bytes are car.
// This works because sizeof(Cons) >= sizeof(void*).

typedef struct {
    void *free_list;    // head of intrusive free list
    Arena *arena;       // backing allocator for slabs
    u32 obj_size;       // bytes per object (≥ 8 for free list link)
    u32 slab_count;     // objects per slab
} Pool;

NOINLINE static void pool_grow(Pool *p) {
    // Allocate a slab from the arena
    u32 total = p->obj_size * p->slab_count;
    u8 *slab = (u8 *)arena_alloc(p->arena, total, 8);

    // Thread all objects into free list
    for (u32 i = 0; i < p->slab_count; i++) {
        void **obj = (void **)(slab + i * p->obj_size);
        *obj = p->free_list;
        p->free_list = obj;
    }
}

ALWAYS_INLINE void *pool_alloc(Pool *p) {
    if (UNLIKELY(!p->free_list)) pool_grow(p);
    void *obj = p->free_list;
    p->free_list = *(void **)obj;
    return obj;
}

ALWAYS_INLINE void pool_free(Pool *p, void *obj) {
    *(void **)obj = p->free_list;
    p->free_list = obj;
}

static Pool pool_create(Arena *arena, u32 obj_size, u32 slab_count) {
    if (obj_size < 8) obj_size = 8;  // minimum for free list link
    Pool p = {NULL, arena, obj_size, slab_count};
    return p;
}

static void pool_reset(Pool *p) {
    p->free_list = NULL;
    // Slabs are in the arena — arena_reset reclaims everything
}

// Pool-backed Cons allocation
static Pool g_cons_pool;

ALWAYS_INLINE Val pcons_new(Val a, Val d) {
    Cons *c = (Cons *)pool_alloc(&g_cons_pool);
    c->car = a; c->cdr = d;
    return val_cons(c);
}

// ============================================================================
// 3. X-MACRO SPECIAL FORMS (compiler dispatch)
// ============================================================================
//
// The c_expr/emit_expr dispatchers are 30+ if-chains. An X-macro table
// can generate the dispatch, making it trivial to add new special forms.
//
// Pattern: SPECIAL_FORMS(X) defines form → handler mapping.
// The dispatcher becomes a loop over a static table.

// For demonstration: show how special form dispatch works as data.
// In practice, the inline if-chain is faster for <20 entries (branch predictor
// learns the pattern). But the X-macro makes the spec VISIBLE.

typedef void (*SpecialFn)(Val args);

// X(sym_var, handler_fn)
// This would reference the actual S_IF, S_LET etc. from read.c
// and the c_if, c_let etc. from emit_x86.c
#define SPECIAL_FORMS(X) \
    X(S_IF,    "if")     \
    X(S_LET,   "let")    \
    X(S_DO,    "do")     \
    X(S_AND,   "and")    \
    X(S_OR,    "or")     \
    X(S_LOOP,  "loop")

// Generated: a table of {StrId, name} for introspection
typedef struct { StrId *sym; const char *name; } SpecialEntry;

#define X_SPECIAL(sym, name) {&sym, name},
static SpecialEntry g_specials[] = { SPECIAL_FORMS(X_SPECIAL) };
#undef X_SPECIAL
static const u32 N_SPECIALS = sizeof(g_specials) / sizeof(g_specials[0]);

// The dispatch pattern would be:
//   for (u32 i = 0; i < N_SPECIALS; i++)
//       if (sym == *g_specials[i].sym) { g_specials[i].handler(cc, args); return; }
//
// But we'd keep the if-chain for hot path and use the table for:
// - REPL introspection ("what special forms exist?")
// - Error messages ("unknown form 'xyz', did you mean 'if'?")
// - Doc generation

// ============================================================================
// Tests
// ============================================================================

static int t_pass, t_fail;

static void check_val(const char *name, Val got, Val expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s\n", name); t_fail++; }
}

static void check_eq(const char *name, i64 got, i64 expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s: expected %lld, got %lld\n", name, (long long)expected, (long long)got); t_fail++; }
}

static void run_tests(void) {
    pf("=== x-macro + pool tests ===\n");
    t_pass = t_fail = 0;

    // --- X-macro protocol tests ---
    xproto_init();

    // Verify generated enum matches base.c enum
    check_eq("xi-nil",  XI_nil,  TI_NIL);
    check_eq("xi-cons", XI_cons, TI_CONS);
    check_eq("xi-str",  XI_str,  TI_STR);
    check_eq("xi-f64",  XI_f64,  TI_F64);
    check_eq("xi-count", XI_COUNT, N_TAGS);

    // Verify generated dispatch works
    {
        Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
        check_val("xp-first", xt_first(list), val_int(10));
        check_val("xp-rest",  xt_first(xt_rest(list)), val_int(20));
        check_val("xp-nil",   xt_first(val_nil()), val_nil());
        check_val("xp-count", xt_count(list), val_int(2));
        check_val("xp-seq",   xt_seq(val_nil()), val_nil());

        Str *s = arena_push(&g_req, Str); *s = STR_LIT("hello");
        check_val("xp-str-count", xt_count(val_str(s)), val_int(5));
    }

    // Verify extend works with generated tables
    {
        Val always99(Val v) { (void)v; return val_int(99); }
        xextend(count, XI_int, always99);
        check_val("xp-extend", xt_count(val_int(42)), val_int(99));
    }

    // --- Pool allocator tests ---
    g_cons_pool = pool_create(&g_req, sizeof(Cons), 256);  // 256 cells per slab

    // Basic alloc
    {
        Val list = pcons_new(val_int(1), pcons_new(val_int(2), pcons_new(val_int(3), val_nil())));
        check_val("pool-first", car(list), val_int(1));
        check_val("pool-rest",  car(cdr(list)), val_int(2));
        check_val("pool-count", val_int(list_len(list)), val_int(3));
    }

    // Free + realloc
    {
        Val c = pcons_new(val_int(42), val_nil());
        void *ptr = val_as_cons(c);
        pool_free(&g_cons_pool, ptr);
        Val c2 = pcons_new(val_int(99), val_nil());
        // Should reuse the same memory
        check_eq("pool-reuse", (i64)val_as_cons(c2), (i64)ptr);
        check_val("pool-val", car(c2), val_int(99));
    }

    // Slab growth: allocate more than one slab
    {
        Val head = val_nil();
        for (u32 i = 0; i < 500; i++) {
            head = pcons_new(val_int(i), head);
        }
        // Verify first and last
        check_val("pool-500-first", car(head), val_int(499));
        Val tail = head;
        for (u32 i = 0; i < 499; i++) tail = cdr(tail);
        check_val("pool-500-last", car(tail), val_int(0));
    }

    // --- Special form table tests ---
    check_eq("n-specials", (i64)N_SPECIALS, 6);
    // Verify the table entries match
    check_eq("sf-if", (i64)*g_specials[0].sym, (i64)S_IF);
    check_eq("sf-let", (i64)*g_specials[1].sym, (i64)S_LET);

    pf("  %d passed, %d failed\n", t_pass, t_fail);
}

// ============================================================================
// Benchmarks
// ============================================================================

#define BENCH(label, N, body) do {                  \
    u64 _best = ~(u64)0;                            \
    for (u32 _r = 0; _r < 5; _r++) {               \
        u64 _t0 = now_ns();                         \
        for (u32 _i = 0; _i < (N); _i++) { body; } \
        u64 _t1 = now_ns();                         \
        if (_t1 - _t0 < _best) _best = _t1 - _t0;  \
    }                                                \
    buf_s(&g_print_buf, "  ");                       \
    buf_s(&g_print_buf, label);                      \
    u32 _pad = 28 - (u32)strlen(label);              \
    while (_pad-- > 0) buf_c(&g_print_buf, ' ');     \
    buf_f1(&g_print_buf, (f64)_best / (N));          \
    buf_s(&g_print_buf, " ns/op\n");                 \
    buf_flush(&g_print_buf, 1);                      \
} while(0)

static void run_bench(void) {
    pf("\n=== benchmarks ===\n");

    u32 N = 10000000;

    // --- Pool vs Arena for Cons ---
    pf("\n  -- cons allocation --\n");

    BENCH("arena cons_new:", N, {
        arena_reset(&g_req);
        SINK(cons_new(val_int(1), val_nil()));
    });

    BENCH("pool pcons_new:", N, {
        // Don't reset — pool_free handles reuse
        Val c = pcons_new(val_int(1), val_nil());
        pool_free(&g_cons_pool, val_as_cons(c));
        SINK(c);
    });

    // Batch: build a 10-element list
    BENCH("arena 10-list:", N/10, {
        arena_reset(&g_req);
        Val h = val_nil();
        for (u32 j = 0; j < 10; j++) h = cons_new(val_int(j), h);
        SINK(h);
    });

    // Reset pool for batch test
    pool_reset(&g_cons_pool);
    g_cons_pool = pool_create(&g_req, sizeof(Cons), 256);

    BENCH("pool 10-list:", N/10, {
        Val h = val_nil();
        for (u32 j = 0; j < 10; j++) h = pcons_new(val_int(j), h);
        // Free all 10
        Val s = h;
        while (val_is_cons(s)) { Val n = cdr(s); pool_free(&g_cons_pool, val_as_cons(s)); s = n; }
        SINK(h);
    });

    // --- Protocol dispatch: generated vs manual ---
    pf("\n  -- protocol dispatch: xmacro vs manual --\n");

    // Re-init for clean state
    xproto_init();
    Val list = cons_new(val_int(10), cons_new(val_int(20), cons_new(val_int(30), val_nil())));

    BENCH("manual t_first:", N, SINK(t_first(list)));
    BENCH("xmacro xt_first:", N, SINK(xt_first(list)));

    BENCH("manual p_first:", N, SINK(p_first(list)));

    // Seq walk comparison
    BENCH("manual seq walk:", N, {
        Val s = list; i64 sum = 0;
        while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
        SINK(sum);
    });

    BENCH("xmacro seq walk:", N, {
        Val s = list; i64 sum = 0;
        while (!val_is_nil(s)) { sum += val_as_int(xt_first(s)); s = xt_rest(s); }
        SINK(sum);
    });

    // --- Special form lookup: table vs if-chain ---
    pf("\n  -- special form lookup --\n");

    // Simulate: given a StrId, find if it's a special form
    // If-chain (what we do now)
    BENCH("if-chain lookup:", N, {
        StrId sym = S_LOOP;  // worst case: last in chain
        i64 found = 0;
        if (sym == S_IF) found = 1;
        else if (sym == S_LET) found = 2;
        else if (sym == S_DO) found = 3;
        else if (sym == S_AND) found = 4;
        else if (sym == S_OR) found = 5;
        else if (sym == S_LOOP) found = 6;
        SINK(found);
    });

    // Table scan (from X-macro)
    BENCH("table scan lookup:", N, {
        StrId sym = S_LOOP;
        i64 found = 0;
        for (u32 i = 0; i < N_SPECIALS; i++) {
            if (sym == *g_specials[i].sym) { found = (i64)(i + 1); break; }
        }
        SINK(found);
    });

    // Bitmap (StrId < 64, use bitmask)
    // Build bitmap once
    u64 special_mask = 0;
    for (u32 i = 0; i < N_SPECIALS; i++) {
        StrId id = *g_specials[i].sym;
        if (id < 64) special_mask |= (1ULL << id);
    }

    BENCH("bitmap is_special:", N, {
        StrId sym = S_LOOP;
        i64 found = (sym < 64) && (special_mask & (1ULL << sym));
        SINK(found);
    });

    pf("\n  protocol tables: %u bytes (manual)\n",
       (u32)(6 * N_TAGS * sizeof(void *)));
    pf("  protocol tables: %u bytes (xmacro)\n",
       (u32)(6 * XI_COUNT * sizeof(void *)));
    pf("  special form table: %u bytes\n",
       (u32)(N_SPECIALS * sizeof(SpecialEntry)));
    pf("  special form bitmap: 8 bytes\n");
}

// ============================================================================
// Summary
// ============================================================================

static void print_summary(void) {
    pf("\n=== pattern summary ===\n\n");

    pf("  1. X-MACRO PROTOCOL SPEC\n");
    pf("     Spec:  3 macros (XPROTO_1, XPROTO_2, XPROTO_3)\n");
    pf("     Types: 1 macro  (XTAG_TYPES)\n");
    pf("     Impls: 1 macro  (XPROTO_IMPLS)\n");
    pf("     Generated: enum + tables + init + dispatch + extend\n");
    pf("     Add protocol: 1 line in XPROTO_<n>\n");
    pf("     Add type:     1 line in XTAG_TYPES\n");
    pf("     Add impl:     1 line in XPROTO_IMPLS\n\n");

    pf("  2. POOL ALLOCATOR\n");
    pf("     For: fixed-size objects (Cons = 16 bytes)\n");
    pf("     Slab: 256 cells/page, intrusive free list\n");
    pf("     Alloc: 1 load + 1 store (free list hit)\n");
    pf("     Free:  1 store (push to free list)\n");
    pf("     Reset: set free_list = NULL, arena_reset reclaims\n\n");

    pf("  3. SPECIAL FORM TABLE\n");
    pf("     Spec:  SPECIAL_FORMS(X) macro\n");
    pf("     Use:   introspection, docs, error messages\n");
    pf("     Hot path: keep if-chain (branch predictor wins)\n");
    pf("     Bitmap: is_special() in 1 cycle for StrId < 64\n");
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    base_init();
    init_syms();

    g_cons_pool = pool_create(&g_req, sizeof(Cons), 256);

    run_tests();
    run_bench();
    print_summary();

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    base_cleanup();
    return t_fail ? 1 : 0;
}
