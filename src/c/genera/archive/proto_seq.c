/**
 * proto_seq.c — Descent on Sequence Abstraction
 *
 * What rhymes: protocol dispatch, Cons walk, arena bump, JIT loop.
 * All are: INDIRECTION → eliminate one level → everything speeds up.
 *
 * Hierarchy of indirection (each level ≈ 4-15 cycles):
 *   0: inline code, array index, bump pointer
 *   1: direct call, pool alloc, pointer load
 *   2: vtable/protocol dispatch, linked list walk
 *   3: hash table (hash+probe+compare), tree walk
 *
 * This file descends through 6 levels of the same operation:
 *   "sum a sequence of N integers"
 *
 * Each level removes one indirection:
 *   L0  Cons walk + protocol dispatch     (2 indirections per element)
 *   L1  Cons walk + inline fast path      (1 indirection: pointer chase)
 *   L2  Cons walk, fused first+rest       (same but 1 fewer call)
 *   L3  Chunked seq — 32-wide array scan  (pointer chase every 32 elements)
 *   L4  Flat array — no pointer chase     (0 indirections, sequential memory)
 *   L5  SIMD — 4 ints per cycle           (hardware parallelism)
 *   L6  Closed form — no loop at all      (n*(n-1)/2, the compiler IS the transducer)
 *
 * What animates: first + rest + empty? — THE fundamental loop.
 * The descent: how close can we get to the hardware while keeping the abstraction?
 *
 * Build: gcc -O3 -march=native -mavx2 -nostdlib -static -o proto_seq test/proto_seq.c
 */

// Pull in runtime (sys → std → rt)
#include "../rt/rt.c"

// AVX2 via GCC vector extensions — no system headers needed (freestanding)
typedef i64 v4i64 __attribute__((vector_size(32)));

ALWAYS_INLINE v4i64 v4_load(const i64 *p) {
    v4i64 v;
    memcpy(&v, p, 32);
    return v;
}
ALWAYS_INLINE void v4_store(i64 *p, v4i64 v) { memcpy(p, &v, 32); }
ALWAYS_INLINE v4i64 v4_zero(void) { return (v4i64){0, 0, 0, 0}; }
ALWAYS_INLINE v4i64 v4_add(v4i64 a, v4i64 b) { return a + b; }
ALWAYS_INLINE i64 v4_hsum(v4i64 v) { return v[0] + v[1] + v[2] + v[3]; }

// ============================================================================
// L3: Chunked Sequence
// ============================================================================
//
// Clojure's ChunkedSeq: 32 elements per node, pointer between nodes.
// Walk = array scan (prefetcher-friendly) + 1 pointer chase per 32 elements.
// 32x fewer indirections than Cons.

#define CHUNK_SIZE 32

typedef struct Chunk {
    Val items[CHUNK_SIZE];
    struct Chunk *next;
    u32 count;
} Chunk;

static Chunk *chunk_new(Arena *a) {
    Chunk *c = arena_push(a, Chunk);
    c->next = NULL;
    c->count = 0;
    return c;
}

// Build chunked sequence from integer range [0, n)
static Chunk *chunk_range(Arena *a, u32 n) {
    if (n == 0) return NULL;
    Chunk *first = chunk_new(a);
    Chunk *cur = first;
    for (u32 i = 0; i < n; i++) {
        if (cur->count >= CHUNK_SIZE) {
            Chunk *next = chunk_new(a);
            cur->next = next;
            cur = next;
        }
        cur->items[cur->count++] = val_int(i);
    }
    return first;
}

// Build Cons list from integer range [0, n)
static Val cons_range(u32 n) {
    Val list = val_nil();
    for (i32 i = (i32)n - 1; i >= 0; i--)
        list = cons_new(val_int(i), list);
    return list;
}

// Build flat array from integer range [0, n)
static Val *flat_range(Arena *a, u32 n) {
    Val *arr = arena_push_n(a, Val, n);
    for (u32 i = 0; i < n; i++) arr[i] = val_int(i);
    return arr;
}

// Build flat i64 array (unboxed) from integer range [0, n)
static i64 *raw_range(Arena *a, u32 n) {
    i64 *arr = arena_push_n(a, i64, n);
    for (u32 i = 0; i < n; i++) arr[i] = (i64)i;
    return arr;
}

// ============================================================================
// Protocol-level: chunk support
// ============================================================================
//
// The chunked seq participates in the protocol system.
// first = items[0], rest = advance index or next chunk.
// This shows: the protocol abstraction works at ANY indirection level.

// For this exploration we just benchmark the raw walk patterns.
// Protocol integration = wrapping these in t_first/t_rest via TAG_PVEC or a new tag.

// ============================================================================
// Tests
// ============================================================================

static int t_pass, t_fail;

static void check_eq(const char *name, i64 got, i64 expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s: expected %lld, got %lld\n", name, (long long)expected, (long long)got); t_fail++; }
}

static void run_tests(void) {
    pf("=== sequence descent tests ===\n");
    t_pass = t_fail = 0;

    i64 expected_100 = 100 * 99 / 2;  // sum [0..99] = 4950
    i64 expected_1000 = 1000 * 999 / 2;

    // L0: Cons + protocol dispatch
    {
        Val list = cons_range(100);
        i64 sum = 0;
        Val s = list;
        while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
        check_eq("L0-cons-proto", sum, expected_100);
    }

    // L1: Cons + inline fast path
    {
        Val list = cons_range(100);
        i64 sum = 0;
        Val s = list;
        while (!val_is_nil(s)) { sum += val_as_int(p_first(s)); s = p_rest(s); }
        check_eq("L1-cons-fast", sum, expected_100);
    }

    // L2: Cons + fused first+rest
    {
        Val list = cons_range(100);
        i64 sum = 0;
        Val s = list; Val f;
        while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
        check_eq("L2-cons-fused", sum, expected_100);
    }

    // L3: Chunked sequence
    {
        Chunk *ch = chunk_range(&g_req, 100);
        i64 sum = 0;
        while (ch) {
            for (u32 i = 0; i < ch->count; i++)
                sum += val_as_int(ch->items[i]);
            ch = ch->next;
        }
        check_eq("L3-chunk", sum, expected_100);
    }

    // L4: Flat array
    {
        Val *arr = flat_range(&g_req, 100);
        i64 sum = 0;
        for (u32 i = 0; i < 100; i++) sum += val_as_int(arr[i]);
        check_eq("L4-flat", sum, expected_100);
    }

    // L5: SIMD (unboxed)
    {
        i64 *arr = raw_range(&g_req, 100);
        v4i64 vsum = v4_zero();
        u32 i = 0;
        for (; i + 4 <= 100; i += 4)
            vsum = v4_add(vsum, v4_load(arr + i));
        i64 sum = v4_hsum(vsum);
        for (; i < 100; i++) sum += arr[i];
        check_eq("L5-simd", sum, expected_100);
    }

    // L6: Closed form
    {
        i64 n = 100;
        i64 sum = n * (n - 1) / 2;
        check_eq("L6-closed", sum, expected_100);
    }

    // Larger test: 1000 elements
    {
        Val list = cons_range(1000);
        i64 sum = 0;
        Val s = list; Val f;
        while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
        check_eq("L2-1000", sum, expected_1000);

        Chunk *ch = chunk_range(&g_req, 1000);
        sum = 0;
        while (ch) {
            for (u32 i = 0; i < ch->count; i++)
                sum += val_as_int(ch->items[i]);
            ch = ch->next;
        }
        check_eq("L3-1000", sum, expected_1000);
    }

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
    u32 _pad = 32 - (u32)strlen(label);              \
    while (_pad-- > 0) buf_c(&g_print_buf, ' ');     \
    buf_f1(&g_print_buf, (f64)_best / (N));          \
    buf_s(&g_print_buf, " ns/iter\n");               \
    buf_flush(&g_print_buf, 1);                      \
} while(0)

static void run_bench(void) {
    pf("\n=== descent benchmarks ===\n");
    pf("  (sum N integers through each abstraction level)\n");

    // --- Small: N=10 (fits in L1, measures per-element overhead) ---
    {
        pf("\n  -- N=10 (L1 hot, overhead-dominated) --\n");
        u32 N = 10000000;

        Val list10 = cons_range(10);
        Chunk *ch10 = chunk_range(&g_req, 10);
        Val *flat10 = flat_range(&g_req, 10);
        i64 *raw10 = raw_range(&g_req, 10);

        BENCH("L0 cons+proto:", N, {
            Val s = list10; i64 sum = 0;
            while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
            SINK(sum);
        });

        BENCH("L1 cons+fast:", N, {
            Val s = list10; i64 sum = 0;
            while (!val_is_nil(s)) { sum += val_as_int(p_first(s)); s = p_rest(s); }
            SINK(sum);
        });

        BENCH("L2 cons+fused:", N, {
            Val s = list10; Val f; i64 sum = 0;
            while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
            SINK(sum);
        });

        BENCH("L3 chunked:", N, {
            Chunk *c = ch10; i64 sum = 0;
            while (c) {
                for (u32 i = 0; i < c->count; i++) sum += val_as_int(c->items[i]);
                c = c->next;
            }
            SINK(sum);
        });

        BENCH("L4 flat array:", N, {
            i64 sum = 0;
            for (u32 i = 0; i < 10; i++) sum += val_as_int(flat10[i]);
            SINK(sum);
        });

        BENCH("L5 SIMD (unboxed):", N, {
            v4i64 vs = v4_zero();
            vs = v4_add(vs, v4_load(raw10));
            vs = v4_add(vs, v4_load(raw10 + 4));
            i64 sum = v4_hsum(vs) + raw10[8] + raw10[9];
            SINK(sum);
        });

        BENCH("L6 closed form:", N, {
            i64 sum = 10 * 9 / 2;
            SINK(sum);
        });
    }

    // --- Medium: N=1000 (shows cache effects) ---
    {
        pf("\n  -- N=1000 (shows pointer-chase vs sequential) --\n");
        u32 N = 100000;

        arena_reset(&g_req);
        Val list1k = cons_range(1000);
        Chunk *ch1k = chunk_range(&g_req, 1000);
        Val *flat1k = flat_range(&g_req, 1000);
        i64 *raw1k = raw_range(&g_req, 1000);

        BENCH("L0 cons+proto:", N, {
            Val s = list1k; i64 sum = 0;
            while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
            SINK(sum);
        });

        BENCH("L1 cons+fast:", N, {
            Val s = list1k; i64 sum = 0;
            while (!val_is_nil(s)) { sum += val_as_int(p_first(s)); s = p_rest(s); }
            SINK(sum);
        });

        BENCH("L2 cons+fused:", N, {
            Val s = list1k; Val f; i64 sum = 0;
            while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
            SINK(sum);
        });

        BENCH("L3 chunked:", N, {
            Chunk *c = ch1k; i64 sum = 0;
            while (c) {
                for (u32 i = 0; i < c->count; i++) sum += val_as_int(c->items[i]);
                c = c->next;
            }
            SINK(sum);
        });

        BENCH("L4 flat array:", N, {
            i64 sum = 0;
            for (u32 i = 0; i < 1000; i++) sum += val_as_int(flat1k[i]);
            SINK(sum);
        });

        BENCH("L5 SIMD (unboxed):", N, {
            v4i64 vs = v4_zero();
            u32 i = 0;
            for (; i + 4 <= 1000; i += 4)
                vs = v4_add(vs, v4_load(raw1k + i));
            i64 sum = v4_hsum(vs);
            for (; i < 1000; i++) sum += raw1k[i];
            SINK(sum);
        });

        BENCH("L6 closed form:", N, {
            i64 sum = 1000LL * 999 / 2;
            SINK(sum);
        });
    }

    // --- Large: N=100000 (saturates cache hierarchy) ---
    {
        pf("\n  -- N=100000 (saturates memory bandwidth) --\n");
        u32 N = 1000;

        arena_reset(&g_req);
        Val list100k = cons_range(100000);
        Chunk *ch100k = chunk_range(&g_req, 100000);
        Val *flat100k = flat_range(&g_req, 100000);
        i64 *raw100k = raw_range(&g_req, 100000);

        BENCH("L0 cons+proto:", N, {
            Val s = list100k; i64 sum = 0;
            while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
            SINK(sum);
        });

        BENCH("L2 cons+fused:", N, {
            Val s = list100k; Val f; i64 sum = 0;
            while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
            SINK(sum);
        });

        BENCH("L3 chunked:", N, {
            Chunk *c = ch100k; i64 sum = 0;
            while (c) {
                for (u32 i = 0; i < c->count; i++) sum += val_as_int(c->items[i]);
                c = c->next;
            }
            SINK(sum);
        });

        BENCH("L4 flat array:", N, {
            i64 sum = 0;
            for (u32 i = 0; i < 100000; i++) sum += val_as_int(flat100k[i]);
            SINK(sum);
        });

        BENCH("L5 SIMD (unboxed):", N, {
            v4i64 vs = v4_zero();
            u32 i = 0;
            for (; i + 4 <= 100000; i += 4)
                vs = v4_add(vs, v4_load(raw100k + i));
            i64 sum = v4_hsum(vs);
            for (; i < 100000; i++) sum += raw100k[i];
            SINK(sum);
        });

        BENCH("L6 closed form:", N, {
            i64 sum = 100000LL * 99999 / 2;
            SINK(sum);
        });
    }
}

// ============================================================================
// Analysis
// ============================================================================

static void print_analysis(void) {
    pf("\n=== what rhymes ===\n\n");

    pf("  The same shape at every level:\n\n");
    pf("    INDIRECTION is the cost.\n");
    pf("    Eliminate one level → everything speeds up.\n\n");

    pf("  Level │ Indirections │ What\n");
    pf("  ──────┼──────────────┼────────────────────────────\n");
    pf("  L0    │ 2/element    │ protocol table + pointer chase\n");
    pf("  L1    │ 1/element    │ inline check + pointer chase\n");
    pf("  L2    │ 1/element    │ fused: 1 check, 2 loads, same cache line\n");
    pf("  L3    │ 1/32 elements│ array scan within chunk\n");
    pf("  L4    │ 0            │ sequential array access\n");
    pf("  L5    │ 0            │ SIMD: 4 elements per instruction\n");
    pf("  L6    │ 0            │ no loop: closed-form\n\n");

    pf("  The same pattern in other domains:\n\n");
    pf("    sig dispatch:     name → hierarchy → handler array → call\n");
    pf("    protocol dispatch: tag → index → table → fn ptr → call\n");
    pf("    malloc:           bin → freelist → pointer chase\n");
    pf("    arena:            bump pointer — 0 indirections\n");
    pf("    hash table:       hash → probe → compare → found\n");
    pf("    interned strings: integer compare — 0 indirections\n\n");

    pf("  Descent always terminates at:\n");
    pf("    integers, arrays, bits.\n\n");

    pf("  For a Clojure runtime, this means:\n");
    pf("    1. Cons lists are correct but slow (pointer-per-element)\n");
    pf("    2. Chunked seqs are the sweet spot (Clojure does this)\n");
    pf("    3. Flat arrays (pvec) are faster still\n");
    pf("    4. The JIT can specialize to L5/L6 for numeric code\n");
    pf("    5. Transducers = compose at L3-L4 without intermediate alloc\n\n");

    pf("  Memory per element:\n");
    pf("    Cons:    16 B/element (car + cdr pointer)\n");
    pf("    Chunk:   8.8 B/element (8B val + overhead/32)\n");
    pf("    Flat:    8 B/element (just the Val)\n");
    pf("    Unboxed: 8 B/element (just the i64)\n\n");

    pf("  The hierarchy is not just speed — it's also memory:\n");
    pf("    less indirection = less memory = more L1 hits = faster\n");
    pf("    Self-reinforcing: compression IS performance.\n");
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    base_init();
    init_syms();

    run_tests();
    run_bench();
    print_analysis();

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    base_cleanup();
    return t_fail ? 1 : 0;
}
