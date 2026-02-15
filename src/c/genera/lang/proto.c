/**
 * proto.c — Cons Cells + Protocol Dispatch
 *
 * Clojure polymorphism via X-macro spec + bitmap-indexed tables.
 * Three tiers: table (~2ns), fast-path (~0.1ns), fused (one check, two loads).
 * Depends on: base/base.c
 */
#ifndef PROTO_C_INCLUDED
#define PROTO_C_INCLUDED

// ============================================================================
// 1. Cons Cells — fundamental sequence type
// ============================================================================

typedef struct { Val car; Val cdr; } Cons;
#define NIL val_nil()

// Switchable arena for Val allocations (cons, pvec, pmap).
// Default: g_req (per-eval). Set to g_perm for permanent closures.
static Arena *g_val_arena = NULL;  // NULL = use &g_req (avoids init-order issue)
#define VAL_ARENA (g_val_arena ? g_val_arena : &g_req)

ALWAYS_INLINE Val cons_new(Val a, Val d) {
    Cons *c = arena_push(VAL_ARENA, Cons);
    c->car = a; c->cdr = d;
    return val_cons(c);
}
ALWAYS_INLINE Val car(Val v) { return ((Cons *)val_as_cons(v))->car; }
ALWAYS_INLINE Val cdr(Val v) { return ((Cons *)val_as_cons(v))->cdr; }

static u32 list_len(Val v) {
    u32 n = 0;
    while (val_is_cons(v)) { n++; v = cdr(v); }
    return n;
}

// ============================================================================
// 2. Protocol Dispatch — Clojure polymorphism
// ============================================================================
//
// X-macro spec: add protocol = 1 line. Add type impl = 1 line.
// Init generates itself. Tables generate themselves. Never out of sync.
//
// Three tiers:
//   Tier 1 (t_*):  table dispatch — generic, extensible        (~2 ns)
//   Tier 2 (p_*):  inline cons/nil fast path + table fallback  (~0.1 ns mono)
//   Tier 3 (fused): one type check → car+cdr from same cache line

#define N_TAGS 11

ALWAYS_INLINE u32 val_tag_idx(Val v) {
    if (LIKELY((v & QNAN) == QNAN))
        return (u32)(v >> 63 << 3) | (u32)((v >> 48) & 7);
    return 10;
}

// --- THE SPEC: one place defines everything ---

// X(name) — unary protocols: Val → Val
#define PROTO_1(X)  X(first) X(rest) X(seq) X(count)
// X(name) — binary protocols: (Val, Val) → Val
#define PROTO_2(X)  X(get)
// X(name) — ternary protocols: (Val, Val, Val) → Val
#define PROTO_3(X)  X(assoc)

// X(NAME, idx) — NaN-box tag types (must match val_tag_idx output)
#define TAG_TYPES(X) \
    X(NIL_T,0) X(BOOL,1) X(INT,2) X(SYM,3) X(KW,4) X(STR,5) \
    X(PMAP,6) X(PVEC,7) X(FN,8) X(CONS,9) X(F64,10)

// Generated: enum { TI_NIL_T=0, TI_BOOL=1, ... TI_F64=10 }
#define X(NAME, idx) TI_##NAME = idx,
enum { TAG_TYPES(X) };
#undef X

// Function pointer types
typedef Val (*PFn1)(Val);
typedef Val (*PFn2)(Val, Val);
typedef Val (*PFn3)(Val, Val, Val);

static Val _perr1(Val v) { (void)v; return NIL; }
static Val _perr2(Val a, Val b) { (void)a; (void)b; return NIL; }
static Val _perr3(Val a, Val b, Val c) { (void)a; (void)b; (void)c; return NIL; }

// Generated: SoA dispatch tables (88 bytes each, 2 cache lines)
#define X(name) static PFn1 P_##name[N_TAGS];
PROTO_1(X)
#undef X
#define X(name) static PFn2 P_##name[N_TAGS];
PROTO_2(X)
#undef X
#define X(name) static PFn3 P_##name[N_TAGS];
PROTO_3(X)
#undef X

// Runtime extension: extend(P_first, TI_PVEC, my_pvec_first)
#define extend(method, type_idx, fn) (method)[type_idx] = (fn)

// --- Built-in implementations ---
static Val _nil_first(Val v) { (void)v; return NIL; }
static Val _nil_rest(Val v)  { (void)v; return NIL; }
static Val _nil_seq(Val v)   { (void)v; return NIL; }
static Val _nil_get(Val v, Val k) { (void)v; (void)k; return NIL; }
static Val _nil_count(Val v) { (void)v; return val_int(0); }

static Val _cons_first(Val v) { return car(v); }
static Val _cons_rest(Val v)  { return cdr(v); }
static Val _cons_seq(Val v)   { return v; }
static Val _cons_count(Val v) { return val_int(list_len(v)); }

static Val _str_count(Val v) { return val_int(val_as_str(v)->len); }
static Val _str_seq(Val v)   { return val_as_str(v)->len ? v : NIL; }

static Val _kw_get(Val kw, Val coll) {
    return P_get[val_tag_idx(coll)](coll, kw);
}

// X(type, protocol, fn) — all type→protocol bindings
#define PROTO_IMPLS(X)                                                  \
    X(NIL_T, first, _nil_first)  X(NIL_T, rest, _nil_rest)             \
    X(NIL_T, seq,   _nil_seq)    X(NIL_T, get,  _nil_get)              \
    X(NIL_T, count, _nil_count)                                         \
    X(CONS, first, _cons_first) X(CONS, rest, _cons_rest)               \
    X(CONS, seq,   _cons_seq)   X(CONS, count, _cons_count)             \
    X(STR,  count, _str_count)  X(STR,  seq,   _str_seq)                \
    X(KW,   get,   _kw_get)

// Generated: proto_init() — fills defaults, registers all impls
static void proto_init(void) {
#define X(name) for (u32 i = 0; i < N_TAGS; i++) P_##name[i] = _perr1;
    PROTO_1(X)
#undef X
#define X(name) for (u32 i = 0; i < N_TAGS; i++) P_##name[i] = _perr2;
    PROTO_2(X)
#undef X
#define X(name) for (u32 i = 0; i < N_TAGS; i++) P_##name[i] = _perr3;
    PROTO_3(X)
#undef X
#define X(type, proto, fn) P_##proto[TI_##type] = fn;
    PROTO_IMPLS(X)
#undef X
}

// --- Generated: Tier 1 table dispatch ---
#define X(name) ALWAYS_INLINE Val t_##name(Val v) { return P_##name[val_tag_idx(v)](v); }
PROTO_1(X)
#undef X

// --- Tier 2: Fast path (hand-written — hot path specific) ---
NOINLINE static Val _p_first_slow(Val v) { return P_first[val_tag_idx(v)](v); }
NOINLINE static Val _p_rest_slow(Val v)  { return P_rest[val_tag_idx(v)](v); }

ALWAYS_INLINE Val p_first(Val v) {
    if (LIKELY(val_is_cons(v))) return ((Cons *)val_as_cons(v))->car;
    return _p_first_slow(v);
}
ALWAYS_INLINE Val p_rest(Val v) {
    if (LIKELY(val_is_cons(v))) return ((Cons *)val_as_cons(v))->cdr;
    return _p_rest_slow(v);
}
ALWAYS_INLINE Val p_seq(Val v)   { return P_seq[val_tag_idx(v)](v); }
ALWAYS_INLINE Val p_count(Val v) { return P_count[val_tag_idx(v)](v); }
ALWAYS_INLINE Val p_get(Val coll, Val key) { return P_get[val_tag_idx(coll)](coll, key); }
ALWAYS_INLINE Val p_assoc(Val c, Val k, Val v) { return P_assoc[val_tag_idx(c)](c, k, v); }

// --- Tier 3: Fused first+rest (one check, two loads from same cache line) ---
ALWAYS_INLINE bool p_first_rest(Val v, Val *first, Val *rest) {
    if (LIKELY(val_is_cons(v))) {
        Cons *c = (Cons *)val_as_cons(v);
        *first = c->car;
        *rest  = c->cdr;
        return true;
    }
    if (val_is_nil(v)) return false;
    *first = P_first[val_tag_idx(v)](v);
    *rest  = P_rest[val_tag_idx(v)](v);
    return true;
}

#endif // PROTO_C_INCLUDED
