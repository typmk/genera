/**
 * base.c — Layer 0: Custom Base Layer
 *
 * Arena, Str/Intern, NaN Boxing, DynArray, HashMap, Output Formatting.
 * FREESTANDING: no #include of system headers. Depends only on sys.c.
 *
 * Sections: macros → fmt → arena → strings → NaN box → dynarray → hashmap → init
 * Each section depends only on previous sections.
 *
 * ZERO-INIT: every type is designed so {0} is a valid state.
 *   OutBuf   {0} = silently discards writes (cap=0 → buf_c is no-op)
 *   Arena    {0} = lazy: first alloc creates a 1MB block automatically
 *   Str      {0} = empty string (len=0, all ops safe)
 *   StrBuild {0} = silently discards (no arena → writes are no-ops)
 *   T* arr   NULL = empty array (count=0, push auto-grows)
 *   HashMap  {0} = empty (get→false, del→false, put auto-inits if .arena set)
 *   Val      0x0 = f64 +0.0 (NOT nil — nil is TAG_NIL. By design: Val is a
 *                  numeric type where zero = zero, not "missing".)
 */
#ifndef BASE_C_INCLUDED
#define BASE_C_INCLUDED

// ============================================================================
// 1. Macros
// ============================================================================

#define ALIGN_UP(x, a)    (((x) + ((a)-1)) & ~((a)-1))
#define LIKELY(x)         __builtin_expect(!!(x), 1)
#define UNLIKELY(x)       __builtin_expect(!!(x), 0)
#define POPCOUNT(x)       __builtin_popcountll(x)
#define CTZ(x)            __builtin_ctzll(x)
#define CLZ(x)            __builtin_clzll(x)
#define ALWAYS_INLINE     __attribute__((always_inline)) static inline
#define NOINLINE          __attribute__((noinline))
#define ALIGNED(n)        __attribute__((aligned(n)))
#define MIN(a, b)         ((a) < (b) ? (a) : (b))
#define MAX(a, b)         ((a) > (b) ? (a) : (b))
#define CLAMP(x, lo, hi) (MIN(MAX(x, lo), hi))

#define UINT32_MAX        0xFFFFFFFFU

#define now_ns sys_time_ns

static volatile i64 g_sink;
#define SINK(x) (g_sink = (i64)(x))

ALWAYS_INLINE u32 hash32(u32 x) {
    x ^= x >> 16; x *= 0x85ebca6b;
    x ^= x >> 13; x *= 0xc2b2ae35;
    x ^= x >> 16; return x;
}

// ============================================================================
// 2. Output Buffer + Formatting (replaces printf)
// ============================================================================

typedef struct { char *buf; u32 pos; u32 cap; } OutBuf;

ALWAYS_INLINE void buf_c(OutBuf *b, char c) {
    if (LIKELY(b->pos < b->cap)) b->buf[b->pos++] = c;
}

static void buf_s(OutBuf *b, const char *s) {
    while (*s) buf_c(b, *s++);
}

static void buf_n(OutBuf *b, const char *s, u32 n) {
    for (u32 i = 0; i < n; i++) buf_c(b, s[i]);
}

static void buf_u(OutBuf *b, u64 n) {
    char tmp[20]; int i = 0;
    if (n == 0) { buf_c(b, '0'); return; }
    while (n > 0) { tmp[i++] = '0' + (char)(n % 10); n /= 10; }
    while (i > 0) buf_c(b, tmp[--i]);
}

static void buf_i(OutBuf *b, i64 n) {
    if (n < 0) { buf_c(b, '-'); buf_u(b, (u64)(-n)); }
    else buf_u(b, (u64)n);
}

static void buf_x(OutBuf *b, u64 n) {
    static const char hex[] = "0123456789abcdef";
    buf_s(b, "0x");
    if (n == 0) { buf_c(b, '0'); return; }
    char tmp[16]; int i = 0;
    while (n > 0) { tmp[i++] = hex[n & 0xF]; n >>= 4; }
    while (i > 0) buf_c(b, tmp[--i]);
}

// Fixed-point: 1 decimal place (for "12.3 ms" style output)
static void buf_f1(OutBuf *b, f64 d) {
    if (d < 0) { buf_c(b, '-'); d = -d; }
    i64 whole = (i64)d;
    i64 frac = (i64)((d - (f64)whole) * 10.0 + 0.5);
    if (frac >= 10) { whole++; frac = 0; }
    buf_i(b, whole);
    buf_c(b, '.');
    buf_c(b, '0' + (char)frac);
}

static void buf_flush(OutBuf *b, int fd) {
    if (b->pos > 0) {
        sys_write(fd, b->buf, b->pos);
        b->pos = 0;
    }
}

static void buf_reset(OutBuf *b) { b->pos = 0; if (b->buf) b->buf[0] = '\0'; }

static void buf_hex(OutBuf *b, u64 v) {
    const char h[] = "0123456789abcdef";
    char tmp[16]; int n = 0;
    if (v == 0) { buf_c(b, '0'); return; }
    while (v) { tmp[n++] = h[v & 0xF]; v >>= 4; }
    while (n-- > 0) buf_c(b, tmp[n]);
}

// Mini vfmt: handles %s %d %u %x %c %lld %% — no width/padding
static void buf_vfmt(OutBuf *b, const char *fmt, va_list ap) {
    while (*fmt) {
        if (*fmt != '%') { buf_c(b, *fmt++); continue; }
        fmt++;
        if (*fmt == '%') { buf_c(b, '%'); fmt++; continue; }
        if (*fmt == 'c') { buf_c(b, (char)va_arg(ap, int)); fmt++; continue; }
        if (*fmt == 's') { buf_s(b, va_arg(ap, const char *)); fmt++; continue; }
        if (*fmt == 'd') { buf_i(b, va_arg(ap, int)); fmt++; continue; }
        if (*fmt == 'u') { buf_u(b, va_arg(ap, unsigned)); fmt++; continue; }
        if (*fmt == 'x') { buf_hex(b, va_arg(ap, unsigned)); fmt++; continue; }
        if (*fmt == 'l' && *(fmt+1) == 'l' && *(fmt+2) == 'd') {
            buf_i(b, va_arg(ap, i64)); fmt += 3; continue;
        }
        if (*fmt == 'l' && *(fmt+1) == 'l' && *(fmt+2) == 'x') {
            buf_hex(b, va_arg(ap, u64)); fmt += 3; continue;
        }
        // Unknown: pass through
        buf_c(b, '%'); buf_c(b, *fmt++);
    }
}

static void buf_fmt(OutBuf *b, const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(b, fmt, ap);
    va_end(ap);
}

// Global stdout buffer
static char g_print_data[8192];
static OutBuf g_print_buf;

static void print_init(void) {
    g_print_buf = (OutBuf){g_print_data, 0, sizeof(g_print_data)};
}

// pf = printf replacement. Auto-flushes.
static void pf(const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(&g_print_buf, fmt, ap);
    va_end(ap);
    buf_flush(&g_print_buf, 1);
}
static void print_flush(void) { buf_flush(&g_print_buf, 1); }

// ============================================================================
// 3. Arena — Bump allocator with block chaining
// ============================================================================

typedef struct ArenaBlock {
    struct ArenaBlock *prev;
    u32 size, used;
} ArenaBlock;

typedef struct { ArenaBlock *current; u32 default_size; } Arena;
typedef struct { Arena *arena; ArenaBlock *block; u32 used; } ArenaMark;

#define BLOCK_DATA(b) ((u8 *)(b) + sizeof(ArenaBlock))

static ArenaBlock *arena_block_new(u32 size) {
    u32 total = PAGE_ALIGN(sizeof(ArenaBlock) + size);
    ArenaBlock *b = (ArenaBlock *)sys_alloc(total);
    b->prev = NULL;
    b->size = total - (u32)sizeof(ArenaBlock);
    b->used = 0;
    return b;
}

static void arena_block_free(ArenaBlock *b) {
    sys_free(b, sizeof(ArenaBlock) + b->size);
}

static Arena arena_create(u32 default_size) {
    Arena a; a.default_size = default_size;
    a.current = arena_block_new(default_size);
    return a;
}

ALWAYS_INLINE u32 arena_align_offset(ArenaBlock *b, u32 used, u32 align) {
    u64 base = (u64)BLOCK_DATA(b);
    return (u32)(ALIGN_UP(base + used, align) - base);
}

#define ARENA_DEFAULT_SIZE (1 << 20)  // 1 MB — used when Arena is {0}

NOINLINE static void *arena_alloc_slow(Arena *a, u32 size, u32 align) {
    u32 def = a->default_size ? a->default_size : ARENA_DEFAULT_SIZE;
    if (!a->default_size) a->default_size = def;
    u32 block_size = MAX(def, size + align + 64);
    ArenaBlock *b = arena_block_new(block_size);
    b->prev = a->current; a->current = b;
    u32 off = arena_align_offset(b, b->used, align);
    b->used = off + size;
    return BLOCK_DATA(b) + off;
}

ALWAYS_INLINE void *arena_alloc(Arena *a, u32 size, u32 align) {
    ArenaBlock *b = a->current;
    if (UNLIKELY(!b)) return arena_alloc_slow(a, size, align);
    u32 off = arena_align_offset(b, b->used, align);
    u32 end = off + size;
    if (LIKELY(end <= b->size)) { b->used = end; return BLOCK_DATA(b) + off; }
    return arena_alloc_slow(a, size, align);
}

static void arena_reset(Arena *a) {
    ArenaBlock *b = a->current;
    if (!b) return;  // zero-init Arena: no-op
    while (b->prev) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    b->used = 0; a->current = b;
}

static ArenaMark arena_begin_temp(Arena *a) {
    return (ArenaMark){a, a->current, a->current ? a->current->used : 0};
}

static void arena_end_temp(ArenaMark mark) {
    ArenaBlock *b = mark.arena->current;
    if (!b) return;  // nothing was allocated since begin_temp
    while (b != mark.block) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    mark.arena->current = mark.block;
    if (mark.block) mark.block->used = mark.used;
}

static void arena_destroy(Arena *a) {
    ArenaBlock *b = a->current;
    while (b) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    a->current = NULL;
}

#define arena_push(a, T)       ((T *)arena_alloc((a), sizeof(T), _Alignof(T)))
#define arena_push_n(a, T, n)  ((T *)arena_alloc((a), sizeof(T) * (n), _Alignof(T)))

static Arena g_temp, g_req, g_perm;

// ============================================================================
// 4. Str + Intern
// ============================================================================

typedef struct { u8 *data; u32 len; } Str;
typedef u32 StrId;

#define STR_EMPTY ((Str){NULL, 0})
#define STR_LIT(s) ((Str){(u8 *)(s), sizeof(s) - 1})

ALWAYS_INLINE bool str_eq(Str a, Str b) {
    if (a.len != b.len) return false;
    if (a.data == b.data) return true;
    return memcmp(a.data, b.data, a.len) == 0;
}

ALWAYS_INLINE Str str_slice(Str s, u32 start, u32 len) {
    if (start >= s.len) return STR_EMPTY;
    return (Str){s.data + start, MIN(len, s.len - start)};
}

ALWAYS_INLINE u32 str_hash(Str s) {
    u32 h = 2166136261u;
    for (u32 i = 0; i < s.len; i++) { h ^= s.data[i]; h *= 16777619u; }
    return h;
}

ALWAYS_INLINE Str str_dup(Arena *a, Str s) {
    u8 *copy = arena_push_n(a, u8, s.len);
    memcpy(copy, s.data, s.len);
    return (Str){copy, s.len};
}

// --- Branchless find ---
// Return values chosen so the common slicing operation works without if/else.

// First index of byte, or s.len if not found.
// str_slice(s, 0, idx)        = everything before  (whole string if absent)
// str_slice(s, idx+1, s.len)  = everything after    (empty if absent — safe: slice clamps)
ALWAYS_INLINE u32 str_find(Str s, u8 ch) {
    for (u32 i = 0; i < s.len; i++) if (s.data[i] == ch) return i;
    return s.len;
}

// Last index of byte, or s.len if not found.
// str_slice(s, 0, idx) = dir path without trailing sep (whole string if absent)
ALWAYS_INLINE u32 str_rfind(Str s, u8 ch) {
    for (u32 i = s.len; i > 0; i--) if (s.data[i-1] == ch) return i - 1;
    return s.len;
}

// Index past first occurrence, or 0 if not found.
// str_slice(s, idx, s.len - idx) = suffix after delimiter (whole string if absent)
ALWAYS_INLINE u32 str_after(Str s, u8 ch) {
    for (u32 i = 0; i < s.len; i++) if (s.data[i] == ch) return i + 1;
    return 0;
}

// Index past last occurrence, or 0 if not found.
// str_slice(s, idx, s.len - idx) = filename from path (whole string if no separator)
ALWAYS_INLINE u32 str_after_last(Str s, u8 ch) {
    for (u32 i = s.len; i > 0; i--) if (s.data[i-1] == ch) return i;
    return 0;
}

ALWAYS_INLINE bool str_contains(Str s, u8 ch) { return str_find(s, ch) < s.len; }

ALWAYS_INLINE bool str_starts_with(Str s, Str pre) {
    return s.len >= pre.len && memcmp(s.data, pre.data, pre.len) == 0;
}

ALWAYS_INLINE bool str_ends_with(Str s, Str suf) {
    return s.len >= suf.len && memcmp(s.data + s.len - suf.len, suf.data, suf.len) == 0;
}

static Str str_trim(Str s) {
    while (s.len && (s.data[0] == ' ' || s.data[0] == '\t' || s.data[0] == '\n' || s.data[0] == '\r'))
        { s.data++; s.len--; }
    while (s.len && (s.data[s.len-1] == ' ' || s.data[s.len-1] == '\t' || s.data[s.len-1] == '\n' || s.data[s.len-1] == '\r'))
        s.len--;
    return s;
}

static Str str_cat(Arena *a, Str x, Str y) {
    u8 *buf = arena_push_n(a, u8, x.len + y.len);
    memcpy(buf, x.data, x.len);
    memcpy(buf + x.len, y.data, y.len);
    return (Str){buf, x.len + y.len};
}

// --- StringBuilder: incremental construction on arena ---

typedef struct { u8 *data; u32 len, cap; Arena *arena; } StrBuild;

static StrBuild strbuild(Arena *a, u32 cap) {
    if (cap < 64) cap = 64;
    return (StrBuild){arena_push_n(a, u8, cap), 0, cap, a};
}

static void sb_grow(StrBuild *sb) {
    u32 new_cap = sb->cap * 2;
    u8 *new_data = arena_push_n(sb->arena, u8, new_cap);
    memcpy(new_data, sb->data, sb->len);
    sb->data = new_data;
    sb->cap = new_cap;
}

ALWAYS_INLINE void sb_byte(StrBuild *sb, u8 ch) {
    if (UNLIKELY(sb->len >= sb->cap)) {
        if (!sb->arena) return;  // zero-init: discard
        sb_grow(sb);
    }
    sb->data[sb->len++] = ch;
}

static void sb_str(StrBuild *sb, Str s) {
    if (!s.len) return;
    if (!sb->arena && sb->len + s.len > sb->cap) return;  // zero-init: discard
    while (sb->len + s.len > sb->cap) sb_grow(sb);
    memcpy(sb->data + sb->len, s.data, s.len);
    sb->len += s.len;
}

static void sb_cstr(StrBuild *sb, const char *s) {
    while (*s) sb_byte(sb, (u8)*s++);
}

ALWAYS_INLINE Str sb_finish(StrBuild *sb) {
    return (Str){sb->data, sb->len};
}

#define INTERN_CAP       (1 << 16)
#define INTERN_TABLE_CAP (1 << 17)

typedef struct {
    Str *strings;
    u32  count;
    u32 *table;
    u32  table_mask;
} InternTable;

static InternTable g_intern;

static void intern_init(void) {
    // sys_alloc returns zeroed pages (mmap MAP_ANONYMOUS)
    g_intern.strings = (Str *)sys_alloc(INTERN_CAP * sizeof(Str));
    g_intern.count = 0;
    g_intern.table = (u32 *)sys_alloc(INTERN_TABLE_CAP * sizeof(u32));
    g_intern.table_mask = INTERN_TABLE_CAP - 1;
}

static void intern_free(void) {
    sys_free(g_intern.strings, INTERN_CAP * sizeof(Str));
    sys_free(g_intern.table, INTERN_TABLE_CAP * sizeof(u32));
}

static StrId str_intern(Str s) {
    u32 h = str_hash(s), slot = h & g_intern.table_mask;
    while (1) {
        u32 entry = g_intern.table[slot];
        if (entry == 0) break;
        u32 id = entry - 1;
        if (str_eq(g_intern.strings[id], s)) return id;
        slot = (slot + 1) & g_intern.table_mask;
    }
    StrId id = g_intern.count++;
    g_intern.strings[id] = str_dup(&g_perm, s);
    g_intern.table[slot] = id + 1;
    return id;
}

ALWAYS_INLINE Str str_from_id(StrId id) { return g_intern.strings[id]; }
ALWAYS_INLINE bool strid_eq(StrId a, StrId b) { return a == b; }

// ============================================================================
// 5. NaN Boxing — 12 types in 64 bits
// ============================================================================

typedef u64 Val;

#define QNAN       ((u64)0x7FF8000000000000ULL)
#define SIGN_BIT   ((u64)0x8000000000000000ULL)
#define TAG_MASK   ((u64)0xFFFF000000000000ULL)
#define VAL_MASK   ((u64)0x0000FFFFFFFFFFFFULL)

#define TAG_NIL    ((u64)0x7FF8000000000000ULL)
#define TAG_BOOL   ((u64)0x7FF9000000000000ULL)
#define TAG_INT    ((u64)0x7FFA000000000000ULL)
#define TAG_SYM    ((u64)0x7FFB000000000000ULL)
#define TAG_KW     ((u64)0x7FFC000000000000ULL)
#define TAG_STR    ((u64)0x7FFD000000000000ULL)
#define TAG_PMAP   ((u64)0x7FFE000000000000ULL)
#define TAG_PVEC   ((u64)0x7FFF000000000000ULL)
#define TAG_FN     ((u64)0xFFF8000000000000ULL)
#define TAG_CONS   ((u64)0xFFF9000000000000ULL)

ALWAYS_INLINE Val val_nil(void)       { return TAG_NIL; }
ALWAYS_INLINE Val val_bool(bool b)    { return TAG_BOOL | (u64)b; }
ALWAYS_INLINE Val val_true(void)      { return TAG_BOOL | 1; }
ALWAYS_INLINE Val val_false(void)     { return TAG_BOOL; }
ALWAYS_INLINE Val val_int(i64 n)      { return TAG_INT | ((u64)n & VAL_MASK); }
ALWAYS_INLINE Val val_sym(StrId id)   { return TAG_SYM | (u64)id; }
ALWAYS_INLINE Val val_kw(StrId id)    { return TAG_KW  | (u64)id; }
ALWAYS_INLINE Val val_str(Str *s)     { return TAG_STR | (u64)s; }
ALWAYS_INLINE Val val_pmap(void *p)   { return TAG_PMAP| (u64)p; }
ALWAYS_INLINE Val val_pvec(void *p)   { return TAG_PVEC| (u64)p; }
ALWAYS_INLINE Val val_fn(void *p)     { return TAG_FN  | ((u64)p & VAL_MASK); }
ALWAYS_INLINE Val val_cons(void *p)   { return TAG_CONS | ((u64)p & VAL_MASK); }
ALWAYS_INLINE Val val_f64(f64 d)      { Val v; memcpy(&v, &d, 8); return v; }

ALWAYS_INLINE bool val_is_f64(Val v)  { return (v & QNAN) != QNAN; }
ALWAYS_INLINE bool val_is_nil(Val v)  { return v == TAG_NIL; }
ALWAYS_INLINE bool val_is_bool(Val v) { return (v & TAG_MASK) == TAG_BOOL; }
ALWAYS_INLINE bool val_is_int(Val v)  { return (v & TAG_MASK) == TAG_INT; }
ALWAYS_INLINE bool val_is_sym(Val v)  { return (v & TAG_MASK) == TAG_SYM; }
ALWAYS_INLINE bool val_is_kw(Val v)   { return (v & TAG_MASK) == TAG_KW; }
ALWAYS_INLINE bool val_is_str(Val v)  { return (v & TAG_MASK) == TAG_STR; }
ALWAYS_INLINE bool val_is_pmap(Val v) { return (v & TAG_MASK) == TAG_PMAP; }
ALWAYS_INLINE bool val_is_pvec(Val v) { return (v & TAG_MASK) == TAG_PVEC; }
ALWAYS_INLINE bool val_is_fn(Val v)   { return (v & TAG_MASK) == TAG_FN; }
ALWAYS_INLINE bool val_is_cons(Val v) { return (v & TAG_MASK) == TAG_CONS; }

ALWAYS_INLINE f64   val_as_f64(Val v)  { f64 d; memcpy(&d, &v, 8); return d; }
ALWAYS_INLINE bool  val_as_bool(Val v) { return (bool)(v & 1); }
ALWAYS_INLINE i64   val_as_int(Val v)  { i64 raw = (i64)(v & VAL_MASK); return (raw << 16) >> 16; }
ALWAYS_INLINE StrId val_as_sym(Val v)  { return (StrId)(v & VAL_MASK); }
ALWAYS_INLINE StrId val_as_kw(Val v)   { return (StrId)(v & VAL_MASK); }
ALWAYS_INLINE Str  *val_as_str(Val v)  { return (Str *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_pmap(Val v) { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_pvec(Val v) { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_fn(Val v)   { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_cons(Val v) { return (void *)(u64)(v & VAL_MASK); }

ALWAYS_INLINE bool val_truthy(Val v) {
    return !val_is_nil(v) && !(val_is_bool(v) && !val_as_bool(v));
}

// ============================================================================
// 6. Cons Cells — fundamental sequence type
// ============================================================================

typedef struct { Val car; Val cdr; } Cons;
#define NIL val_nil()

ALWAYS_INLINE Val cons_new(Val a, Val d) {
    Cons *c = arena_push(&g_req, Cons);
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
// 7. Protocol Dispatch — Clojure polymorphism
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
    X(NIL,0) X(BOOL,1) X(INT,2) X(SYM,3) X(KW,4) X(STR,5) \
    X(PMAP,6) X(PVEC,7) X(FN,8) X(CONS,9) X(F64,10)

// Generated: enum { TI_NIL=0, TI_BOOL=1, ... TI_F64=10 }
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
// Add impl = 1 line. This IS the command spec → dispatch table.
#define PROTO_IMPLS(X)                                                  \
    X(NIL,  first, _nil_first)  X(NIL,  rest, _nil_rest)               \
    X(NIL,  seq,   _nil_seq)    X(NIL,  get,  _nil_get)                \
    X(NIL,  count, _nil_count)                                          \
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

// ============================================================================
// 8. DynArray — Arena-backed stretchy buffer
// ============================================================================

typedef struct { Arena *arena; u32 count, cap; } ArrHdr;

#define ARR_HDR(a) ((ArrHdr *)((u8 *)(a) - sizeof(ArrHdr)))
#define arr_count(a) ((a) ? ARR_HDR(a)->count : 0)

#define arr__grow(a, arena_ptr) do {                                    \
    u32 old_cap = (a) ? ARR_HDR(a)->cap : 0;                           \
    u32 new_cap = old_cap ? old_cap * 2 : 8;                           \
    u32 elem_sz = sizeof(*(a));                                         \
    u8 *mem = (u8 *)arena_alloc((arena_ptr),                            \
        sizeof(ArrHdr) + elem_sz * new_cap, 8);                        \
    ArrHdr *hdr = (ArrHdr *)mem;                                        \
    hdr->arena = (arena_ptr); hdr->cap = new_cap;                      \
    if (a) { hdr->count = ARR_HDR(a)->count;                           \
             memcpy(mem + sizeof(ArrHdr), (a), elem_sz * hdr->count);  \
    } else { hdr->count = 0; }                                         \
    (a) = (void *)(mem + sizeof(ArrHdr));                               \
} while(0)

#define arr_push(a, val, arena_ptr) do {                                \
    if (!(a) || ARR_HDR(a)->count >= ARR_HDR(a)->cap)                   \
        arr__grow(a, arena_ptr);                                        \
    (a)[ARR_HDR(a)->count++] = (val);                                   \
} while(0)

#define arr_pop(a)   ((a)[--ARR_HDR(a)->count])
#define arr_last(a)  ((a)[ARR_HDR(a)->count - 1])
#define arr_clear(a) do { if (a) ARR_HDR(a)->count = 0; } while(0)

// ============================================================================
// 9. HashMap — Open-addressing, SoA, arena-backed
// ============================================================================

#define MAP_EMPTY     UINT32_MAX
#define MAP_TOMBSTONE (UINT32_MAX - 1)

typedef struct {
    u32 *keys; Val *vals;
    u32 cap, count, mask;
    Arena *arena;
} HashMap;

static HashMap hashmap_create(Arena *arena, u32 initial_cap) {
    u32 cap = initial_cap < 16 ? 16 : initial_cap;
    cap--; cap |= cap>>1; cap |= cap>>2; cap |= cap>>4; cap |= cap>>8; cap |= cap>>16; cap++;
    HashMap m = {0}; m.arena = arena; m.cap = cap; m.mask = cap - 1;
    m.keys = arena_push_n(arena, u32, cap);
    m.vals = arena_push_n(arena, Val, cap);
    memset(m.keys, 0xFF, sizeof(u32) * cap);
    return m;
}

ALWAYS_INLINE bool hashmap_get(const HashMap *m, u32 key, Val *out) {
    if (!m->keys) return false;  // zero-init: empty
    u32 slot = hash32(key) & m->mask;
    __builtin_prefetch(&m->vals[slot], 0, 3);  // prefetch val while probing keys
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY) return false;
        if (k == key) { *out = m->vals[slot]; return true; }
        slot = (slot + 1) & m->mask;
    }
}

static void hashmap_grow(HashMap *m);

// Auto-inits on first put if arena is set. Usage: HashMap m = {.arena = &a};
static void hashmap_put(HashMap *m, u32 key, Val val) {
    if (!m->keys) {
        // Auto-init: first put on a zero-init map
        u32 cap = 16;
        m->cap = cap; m->mask = cap - 1; m->count = 0;
        m->keys = arena_push_n(m->arena, u32, cap);
        m->vals = arena_push_n(m->arena, Val, cap);
        memset(m->keys, 0xFF, sizeof(u32) * cap);
    }
    if (m->count * 4 >= m->cap * 3) hashmap_grow(m);
    u32 slot = hash32(key) & m->mask;
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY || k == MAP_TOMBSTONE) {
            m->keys[slot] = key; m->vals[slot] = val; m->count++; return;
        }
        if (k == key) { m->vals[slot] = val; return; }
        slot = (slot + 1) & m->mask;
    }
}

static bool hashmap_del(HashMap *m, u32 key) {
    if (!m->keys) return false;  // zero-init: empty
    u32 slot = hash32(key) & m->mask;
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY) return false;
        if (k == key) { m->keys[slot] = MAP_TOMBSTONE; m->count--; return true; }
        slot = (slot + 1) & m->mask;
    }
}

static void hashmap_grow(HashMap *m) {
    u32 old_cap = m->cap; u32 *old_keys = m->keys; Val *old_vals = m->vals;
    m->cap *= 2; m->mask = m->cap - 1; m->count = 0;
    m->keys = arena_push_n(m->arena, u32, m->cap);
    m->vals = arena_push_n(m->arena, Val, m->cap);
    memset(m->keys, 0xFF, sizeof(u32) * m->cap);
    for (u32 i = 0; i < old_cap; i++)
        if (old_keys[i] != MAP_EMPTY && old_keys[i] != MAP_TOMBSTONE)
            hashmap_put(m, old_keys[i], old_vals[i]);
}

// ============================================================================
// 10. Trace + Tap — Observable Execution (sig equivalent for C)
// ============================================================================
//
// sig in PHP: tap() intercepts dispatch, trace() records history.
// sig in C:   TAP() emits to ring buffer, queryable with same bitmask ops.
//
// Zero cost when off: single branch-not-taken in hot path (~0.3 ns).
// When on: records {kind, arg0, arg1, arg2, timestamp} to ring buffer.
// Trace events ARE entities — same shape, same query engine.

#define TRACE_CAP 4096  // ring buffer entries (power of 2)
#define TRACE_MASK (TRACE_CAP - 1)

typedef struct {
    u32 kind;       // StrId of event name (e.g. "parse", "alloc", "call")
    u32 arg0;       // context-dependent (entity id, size, tag, ...)
    u32 arg1;
    u32 arg2;
    u64 ts;         // timestamp (ns) — for profiling
} TraceEv;          // 24 bytes

static TraceEv g_trace[TRACE_CAP];
static u32     g_trace_pos;     // next write position
static u32     g_trace_total;   // total events (for ring detection)
static bool    g_tap_on;        // master switch

NOINLINE static void trace_emit_slow(u32 kind, u32 a0, u32 a1, u32 a2) {
    u32 i = g_trace_pos & TRACE_MASK;
    g_trace[i] = (TraceEv){kind, a0, a1, a2, now_ns()};
    g_trace_pos++;
    g_trace_total++;
}

// TAP: zero-cost when off. Place at any observation point.
#define TAP(kind, a0, a1, a2) do { \
    if (UNLIKELY(g_tap_on)) trace_emit_slow(kind, a0, a1, a2); \
} while(0)

// TAP0/TAP1/TAP2: convenience for fewer args
#define TAP0(kind)           TAP(kind, 0, 0, 0)
#define TAP1(kind, a0)       TAP(kind, a0, 0, 0)
#define TAP2(kind, a0, a1)   TAP(kind, a0, a1, 0)

// Tap control
ALWAYS_INLINE void tap_on(void)    { g_tap_on = true; }
ALWAYS_INLINE void tap_off(void)   { g_tap_on = false; }
ALWAYS_INLINE void tap_reset(void) { g_trace_pos = 0; g_trace_total = 0; }

// Query the trace ring buffer
static u32 trace_count(void) {
    return g_trace_total < TRACE_CAP ? g_trace_total : TRACE_CAP;
}

// Iterate trace entries (newest first). Returns pointer or NULL when exhausted.
// Usage: TraceEv *e; u32 cursor = 0; while ((e = trace_iter(&cursor))) { ... }
static TraceEv *trace_iter(u32 *cursor) {
    u32 n = trace_count();
    if (*cursor >= n) return NULL;
    u32 idx = (g_trace_pos - 1 - *cursor) & TRACE_MASK;
    (*cursor)++;
    return &g_trace[idx];
}

// Count events matching a kind (linear scan, could use bitmask index for large traces)
static u32 trace_count_kind(u32 kind) {
    u32 c = 0, n = trace_count();
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        if (g_trace[idx].kind == kind) c++;
    }
    return c;
}

// Print last N trace events
static void trace_print(u32 max) {
    u32 n = trace_count();
    if (max && max < n) n = max;
    pf("  trace (%u events, %u total):\n", n, g_trace_total);
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        TraceEv *e = &g_trace[idx];
        Str name = str_from_id(e->kind);
        pf("    [%u] ", g_trace_total - 1 - i);
        for (u32 j = 0; j < name.len && j < 20; j++) buf_c(&g_print_buf, name.data[j]);
        pf(" %u %u %u\n", e->arg0, e->arg1, e->arg2);
    }
}

// ============================================================================
// 11. Command Registry + REPL — Interactive Glass Box
// ============================================================================
//
// Commands = StrId → function pointer. Same pattern as sig handlers.
// REPL reads stdin, parses command + args, dispatches.

typedef void (*CmdFn)(Str args);

typedef struct {
    StrId name;
    CmdFn fn;
    const char *help;
} Cmd;

#define CMD_CAP 64
static Cmd g_cmds[CMD_CAP];
static u32 g_cmd_count;

static void cmd_register(const char *name, CmdFn fn, const char *help) {
    if (g_cmd_count >= CMD_CAP) return;
    Str s = {(u8 *)name, (u32)strlen(name)};
    g_cmds[g_cmd_count++] = (Cmd){str_intern(s), fn, help};
}

// Built-in commands
static void cmd_help(Str args) {
    (void)args;
    pf("commands:\n");
    for (u32 i = 0; i < g_cmd_count; i++) {
        Str n = str_from_id(g_cmds[i].name);
        pf("  ");
        for (u32 j = 0; j < n.len; j++) buf_c(&g_print_buf, n.data[j]);
        pf(" — %s\n", g_cmds[i].help);
    }
}

static void cmd_trace(Str args) {
    u32 n = 20;
    if (args.len) {
        n = 0;
        for (u32 i = 0; i < args.len; i++)
            if (args.data[i] >= '0' && args.data[i] <= '9')
                n = n * 10 + (args.data[i] - '0');
    }
    trace_print(n);
}

static void cmd_tap(Str args) {
    if (args.len >= 2 && args.data[0] == 'o' && args.data[1] == 'n')
        { tap_on(); pf("  tap: on\n"); }
    else if (args.len >= 3 && args.data[0] == 'o' && args.data[1] == 'f')
        { tap_off(); pf("  tap: off\n"); }
    else if (args.len >= 1 && args.data[0] == 'r')
        { tap_reset(); pf("  tap: reset\n"); }
    else
        pf("  tap: %s  (usage: tap on|off|reset)\n", g_tap_on ? "on" : "off");
}

static void cmd_arena(Str args) {
    (void)args;
    ArenaBlock *bt = g_temp.current, *br = g_req.current, *bp = g_perm.current;
    pf("  temp:  %u / %u bytes\n", bt ? bt->used : 0, bt ? bt->size : 0);
    pf("  req:   %u / %u bytes\n", br ? br->used : 0, br ? br->size : 0);
    pf("  perm:  %u / %u bytes\n", bp ? bp->used : 0, bp ? bp->size : 0);
}

static void cmd_intern(Str args) {
    (void)args;
    pf("  intern table: %u / %u entries\n", g_intern.count, INTERN_CAP);
}

// REPL: read line → split command + args → dispatch
// Handles both interactive (tty) and piped input (multi-line buffer).

static bool g_repl_quit;

// Returns true if should continue, false if quit requested
static bool repl_dispatch(Str line) {
    line = str_trim(line);
    if (!line.len) return true;

    // Split: first word = command, rest = args
    u32 sp = 0;
    while (sp < line.len && line.data[sp] != ' ' && line.data[sp] != '\t') sp++;
    Str cmd_name = {line.data, sp};
    Str cmd_args = {line.data + sp, line.len - sp};
    cmd_args = str_trim(cmd_args);

    // Quit
    if (str_eq(cmd_name, STR_LIT("q")) || str_eq(cmd_name, STR_LIT("quit"))
        || str_eq(cmd_name, STR_LIT("exit")))
        return false;

    // Dispatch
    StrId id = str_intern(cmd_name);
    for (u32 i = 0; i < g_cmd_count; i++) {
        if (g_cmds[i].name == id) {
            g_cmds[i].fn(cmd_args);
            return true;
        }
    }
    pf("  unknown: ");
    for (u32 i = 0; i < cmd_name.len; i++) buf_c(&g_print_buf, cmd_name.data[i]);
    pf(" (try: help)\n");
    return true;
}

static void repl(void) {
    char buf[4096];
    u32 buf_len = 0;
    g_repl_quit = false;
    pf("> ");
    print_flush();
    while (!g_repl_quit) {
        i64 n = sys_read(0, buf + buf_len, sizeof(buf) - 1 - buf_len);
        if (n <= 0) {
            if (buf_len > 0) {
                buf[buf_len] = 0;
                repl_dispatch((Str){(u8 *)buf, buf_len});
            }
            break;
        }
        buf_len += (u32)n;
        buf[buf_len] = 0;

        // Process complete lines
        u32 start = 0;
        for (u32 i = 0; i < buf_len && !g_repl_quit; i++) {
            if (buf[i] == '\n') {
                buf[i] = 0;
                u32 end = i;
                if (end > start && buf[end-1] == '\r') { buf[end-1] = 0; end--; }
                Str line = {(u8 *)buf + start, end - start};
                if (!repl_dispatch(line)) { g_repl_quit = true; break; }
                pf("> ");
                print_flush();
                start = i + 1;
            }
        }
        if (start > 0) {
            buf_len -= start;
            if (buf_len > 0) memcpy(buf, buf + start, buf_len);
        }
    }
    print_flush();
}

static void repl_init(void) {
    cmd_register("help",   cmd_help,   "list commands");
    cmd_register("trace",  cmd_trace,  "show trace (trace [N])");
    cmd_register("tap",    cmd_tap,    "tap on|off|reset");
    cmd_register("arena",  cmd_arena,  "show arena usage");
    cmd_register("intern", cmd_intern, "show intern table stats");
}

// ============================================================================
// 12. Init / Cleanup
// ============================================================================

static void base_init(void) {
    print_init();
    g_temp = arena_create(64 * 1024);
    g_req  = arena_create(1 << 20);
    g_perm = arena_create(1 << 20);
    intern_init();
    proto_init();
    repl_init();
}

static void base_cleanup(void) {
    intern_free();
    arena_destroy(&g_temp);
    arena_destroy(&g_req);
    arena_destroy(&g_perm);
}

#endif // BASE_C_INCLUDED
