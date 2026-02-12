/**
 * proto_dispatch.c — Protocol Dispatch Benchmark
 *
 * Tests + benchmarks for the protocol dispatch now in base.c.
 * Three tiers: table (t_*), fast path (p_*), fused (p_first_rest).
 *
 * Build: gcc -O3 -march=native -nostdlib -static -o proto_dispatch test/proto_dispatch.c
 */

#include "../src/c/sys.c"
#include "../src/c/base.c"
#include "../src/c/read.c"

// ============================================================================
// Alternative dispatchers (for comparison only)
// ============================================================================

static Val switch_first(Val v) {
    switch (val_tag_idx(v)) {
        case TI_NIL:  return val_nil();
        case TI_CONS: return car(v);
        default:      return val_nil();
    }
}

static Val ifelse_first(Val v) {
    if (val_is_nil(v))  return val_nil();
    if (val_is_cons(v)) return car(v);
    return val_nil();
}

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
    pf("=== protocol dispatch tests ===\n");
    t_pass = t_fail = 0;

    // Tag index
    check_eq("idx-nil",  val_tag_idx(val_nil()),  TI_NIL);
    check_eq("idx-bool", val_tag_idx(val_true()), TI_BOOL);
    check_eq("idx-int",  val_tag_idx(val_int(42)),TI_INT);
    check_eq("idx-sym",  val_tag_idx(val_sym(0)), TI_SYM);
    check_eq("idx-kw",   val_tag_idx(val_kw(0)),  TI_KW);
    check_eq("idx-f64",  val_tag_idx(val_f64(3.14)), TI_F64);
    {
        Val c = cons_new(val_int(1), val_nil());
        check_eq("idx-cons", val_tag_idx(c), TI_CONS);
    }

    // Tier 1: table dispatch
    {
        Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
        check_val("t1-first", t_first(list), val_int(10));
        check_val("t1-nil",   t_first(val_nil()), val_nil());
        check_val("t1-rest",  t_first(t_rest(list)), val_int(20));
    }

    // Tier 2: fast path
    {
        Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
        check_val("t2-first", p_first(list), val_int(10));
        check_val("t2-nil",   p_first(val_nil()), val_nil());
        check_val("t2-rest",  p_first(p_rest(list)), val_int(20));
        check_val("t2-rnil",  p_rest(val_nil()), val_nil());
    }

    // Tier 3: fused
    {
        Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
        Val f, r;
        check_eq("t3-cons", p_first_rest(list, &f, &r), true);
        check_val("t3-car", f, val_int(10));
        check_eq("t3-nil",  p_first_rest(val_nil(), &f, &r), false);
        i64 sum = 0;
        Val s = list;
        while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
        check_eq("t3-sum", sum, 30);
    }

    // count
    {
        Val list = cons_new(val_int(1), cons_new(val_int(2), cons_new(val_int(3), val_nil())));
        check_val("count-3", p_count(list), val_int(3));
        check_val("count-0", p_count(val_nil()), val_int(0));
        Str *s = arena_push(&g_req, Str); *s = STR_LIT("hello");
        check_val("count-s", p_count(val_str(s)), val_int(5));
    }

    // extend at runtime
    {
        Val one(Val v) { (void)v; return val_int(1); }
        extend(P_count, TI_INT, one);
        check_val("extend", p_count(val_int(42)), val_int(1));
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
    u32 _pad = 24 - (u32)strlen(label);              \
    while (_pad-- > 0) buf_c(&g_print_buf, ' ');     \
    buf_f1(&g_print_buf, (f64)_best / (N));          \
    buf_s(&g_print_buf, " ns/op\n");                 \
    buf_flush(&g_print_buf, 1);                      \
} while(0)

static void run_bench(void) {
    pf("\n=== benchmarks ===\n");

    Val list = cons_new(val_int(1), cons_new(val_int(2), cons_new(val_int(3), val_nil())));
    Val mixed[8] = {
        list, val_nil(), list, val_nil(),
        cons_new(val_int(7), val_nil()), val_nil(), list, val_nil()
    };
    u32 N = 10000000;

    pf("\n  -- single dispatch: first() --\n");
    BENCH("direct car():", N, SINK(car(list)));
    BENCH("tier1 table (mono):", N, SINK(t_first(list)));
    BENCH("tier1 table (poly):", N, SINK(t_first(mixed[_i & 7])));
    BENCH("tier2 fast (mono):", N, SINK(p_first(list)));
    BENCH("tier2 fast (poly):", N, SINK(p_first(mixed[_i & 7])));
    BENCH("switch (poly):", N, SINK(switch_first(mixed[_i & 7])));
    BENCH("if-else (poly):", N, SINK(ifelse_first(mixed[_i & 7])));

    pf("\n  -- seq walk: sum 3-element list --\n");

    BENCH("direct car+cdr:", N, {
        Val s = list; i64 sum = 0;
        while (!val_is_nil(s)) { sum += val_as_int(car(s)); s = cdr(s); }
        SINK(sum);
    });

    BENCH("tier1 first+rest:", N, {
        Val s = list; i64 sum = 0;
        while (!val_is_nil(s)) { sum += val_as_int(t_first(s)); s = t_rest(s); }
        SINK(sum);
    });

    BENCH("tier2 first+rest:", N, {
        Val s = list; i64 sum = 0;
        while (!val_is_nil(s)) { sum += val_as_int(p_first(s)); s = p_rest(s); }
        SINK(sum);
    });

    BENCH("tier3 fused:", N, {
        Val s = list; Val f; i64 sum = 0;
        while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
        SINK(sum);
    });

    pf("\n  dispatch table: %u bytes\n", (u32)(6 * N_TAGS * sizeof(void *)));
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

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    base_cleanup();
    return t_fail ? 1 : 0;
}
