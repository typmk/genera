/**
 * bench.c — ALL Benchmarks
 *
 * Covers: arena, str, intern, nanbox, hashmap, grammar, jit, cc.
 * Run via: ./moss bench
 *
 * Depends on: emit/emit.c (and everything below)
 */
#ifndef BENCH_C_INCLUDED
#define BENCH_C_INCLUDED

// ============================================================================
// 1. Base Benchmarks
// ============================================================================

static void bench_arena(void) {
    pf("\n--- arena ---\n");
    Arena a = arena_create(1 << 20);

    u32 N = 50000000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        void *p = arena_alloc(&a, 64, 8);
        SINK((i64)(u64)p);
        if (UNLIKELY(a.current->used + 128 > a.current->size))
            a.current->used = 0;
    }
    u64 dt = now_ns() - t0;
    pf("  arena_alloc(64B):  "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    u32 M = 10000000;
    t0 = now_ns();
    for (u32 i = 0; i < M; i++) {
        a.current->used = 512;
        arena_reset(&a);
    }
    dt = now_ns() - t0;
    pf("  arena_reset:       "); buf_f1(&g_print_buf, (f64)dt / M); pf(" ns/op\n");

    arena_destroy(&a);
}

static void bench_str(void) {
    pf("\n--- str ---\n");
    Str a = STR_LIT("hello!!!");
    Str b = STR_LIT("hello!!!");
    Str c = STR_LIT("goodbye!");

    u32 N = 50000000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) { bool eq = str_eq(a, b); SINK(eq); }
    u64 dt = now_ns() - t0;
    pf("  str_eq (8B match): "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { bool eq = str_eq(a, c); SINK(eq); }
    dt = now_ns() - t0;
    pf("  str_eq (mismatch): "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    StrId id1 = str_intern(a);
    StrId id2 = str_intern(a);
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { bool eq = strid_eq(id1, id2); SINK(eq); }
    dt = now_ns() - t0;
    pf("  strid_eq:          "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");
}

static void bench_intern(void) {
    pf("\n--- intern ---\n");
    // Pre-intern 100 keys
    StrId ids[100];
    for (u32 i = 0; i < 100; i++) {
        char buf[16];
        u32 len = 0;
        buf[len++] = 'b'; buf[len++] = '_';
        if (i >= 10) buf[len++] = '0' + (i / 10);
        buf[len++] = '0' + (i % 10);
        ids[i] = str_intern((Str){(u8 *)buf, len});
    }

    u32 N = 20000000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        u32 idx = i % 100;
        Str s = str_from_id(ids[idx]);
        StrId re = str_intern(s);
        SINK(re);
    }
    u64 dt = now_ns() - t0;
    pf("  intern lookup:     "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");
}

static void bench_nanbox(void) {
    pf("\n--- nanbox ---\n");
    u32 N = 100000000;

    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) { Val v = val_int(i); SINK(v); }
    u64 dt = now_ns() - t0;
    pf("  val_int pack:      "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    Val vi = val_int(42);
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { i64 n = val_as_int(vi); SINK(n); }
    dt = now_ns() - t0;
    pf("  val_as_int:        "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { bool is = val_is_int(vi); SINK(is); }
    dt = now_ns() - t0;
    pf("  val_is_int:        "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");
}

static void bench_hashmap(void) {
    pf("\n--- hashmap ---\n");
    Arena a = arena_create(1 << 24);
    HashMap m = hashmap_create(&a, 256);
    for (u32 i = 0; i < 100; i++) hashmap_put(&m, i + 1000, val_int(i));

    u32 N = 20000000;
    Val v;

    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) { hashmap_get(&m, (u32)(i % 100) + 1000, &v); SINK(v); }
    u64 dt = now_ns() - t0;
    pf("  get (100 keys):    "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { hashmap_put(&m, (u32)(i % 100) + 1000, val_int(i)); }
    dt = now_ns() - t0;
    pf("  put (update):      "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    arena_destroy(&a);
}

// ============================================================================
// 2. Protocol Dispatch Benchmarks
// ============================================================================

static void bench_dispatch(void) {
    pf("\n--- dispatch ---\n");
    Val list = cons_new(val_int(1), cons_new(val_int(2), cons_new(val_int(3), val_nil())));
    u32 N = 10000000;

    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) { SINK(car(list)); }
    u64 dt = now_ns() - t0;
    pf("  direct car():      "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) { SINK(p_first(list)); }
    dt = now_ns() - t0;
    pf("  tier2 fast:        "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        Val s = list; Val f; i64 sum = 0;
        while (p_first_rest(s, &f, &s)) sum += val_as_int(f);
        SINK(sum);
    }
    dt = now_ns() - t0;
    pf("  tier3 fused walk:  "); buf_f1(&g_print_buf, (f64)dt / N); pf(" ns/op\n");
}

// ============================================================================
// 3. Grammar Benchmarks
// ============================================================================

static void bench_grammar(void) {
    pf("\n--- grammar ---\n");
    Lang l; lang_lisp(&l);
    const char *src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) "
        "(if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s)))))\n"
        "(solve 1000)";
    u32 slen = strlen(src);

    Gram g = gram_new(4096);
    gram_parse(&g, &l, src, slen);

    u32 N = 200000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) { g.n = 0; gram_parse(&g, &l, src, slen); }
    u64 t1 = now_ns();
    pf("  parse (%u bytes, %u nodes):\n", slen, g.n);
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns/parse\n");
    pf("    "); buf_f1(&g_print_buf, (f64)slen * N / ((f64)(t1 - t0) / 1e9) / 1e6);
    pf(" MB/s\n");

    N = 500000;
    gram_index(&g);
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) gram_index(&g);
    t1 = now_ns();
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns/index\n");

    // Bitmask popcount
    N = 2000000;
    u32 total = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) total += bm_pop(g.m[NK_LIST], g.mw);
    t1 = now_ns();
    SINK(total);
    pf("  bitmask popcount:  "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns\n");

    // Range query
    u32 defn_node = gn_child(&g, 0);
    N = 5000000;
    u32 hits = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) hits += gn_has(&g, g.m[NK_IDENT], defn_node);
    t1 = now_ns();
    SINK(hits);
    pf("  range query:       "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns\n");
}

// ============================================================================
// 4. JIT Benchmarks
// ============================================================================

static void bench_jit(void) {
    pf("\n--- jit ---\n");
    const char *fib_src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 35)";
    u32 entry = compile_program(fib_src);
    JitFn0 fn = (JitFn0)(g_code.code + entry);

    i64 result = fn();
    pf("  fib(35) = %lld %s\n", (long long)result, result == 9227465 ? "OK" : "WRONG");

    u32 N = 5;
    u64 best = ~(u64)0;
    for (u32 i = 0; i < N; i++) {
        u64 t0 = now_ns();
        i64 r = fn();
        u64 t1 = now_ns();
        SINK(r);
        if (t1 - t0 < best) best = t1 - t0;
    }
    pf("  fib(35) JIT:       "); buf_f1(&g_print_buf, (f64)best / 1e6); pf(" ms\n");

    N = 10000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) compile_program(fib_src);
    u64 t1 = now_ns();
    pf("  compile fib:       "); buf_f1(&g_print_buf, (f64)(t1 - t0) / (N * 1e3)); pf(" us\n");

    compile_program(fib_src);
    pf("  code size:         %u bytes\n", g_code.pos);
}

// ============================================================================
// 5. C Emitter Benchmarks
// ============================================================================

static void bench_cc(void) {
    pf("\n--- c emitter ---\n");
    const char *fib_src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(println (fib 35))";

    u32 N = 10000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        arena_reset(&g_req);
        classify(fib_src);
        emit_program();
    }
    u64 dt = now_ns() - t0;
    pf("  source -> C emit:  "); buf_f1(&g_print_buf, (f64)dt / N / 1000.0); pf(" us/op\n");
    pf("  C output size:     %u bytes\n", g_out.pos);

    // Full compile
    t0 = now_ns();
    arena_reset(&g_req);
    classify(fib_src);
    emit_program();
    sys_write_file("/tmp/_clj_out.c", g_out.buf, g_out.pos);
    char *gcc_argv[] = {"/usr/bin/gcc", "-O2", "-w", "-o", "/tmp/_clj_out", "/tmp/_clj_out.c", NULL};
    sys_run("/usr/bin/gcc", gcc_argv);
    dt = now_ns() - t0;
    pf("  source -> binary:  "); buf_f1(&g_print_buf, (f64)dt / 1e6); pf(" ms\n");

    // Run
    t0 = now_ns();
    char *run_argv[] = {"/tmp/_clj_out", NULL};
    char devnull[4]; u32 devnull_len;
    sys_run_capture("/tmp/_clj_out", run_argv, devnull, sizeof(devnull), &devnull_len);
    dt = now_ns() - t0;
    pf("  fib(35) native:    "); buf_f1(&g_print_buf, (f64)dt / 1e6); pf(" ms\n");
}

// ============================================================================
// Run All Benchmarks
// ============================================================================

static void run_all_bench(void) {
    pf("=== moss benchmarks ===\n");

    bench_arena();
    bench_str();
    bench_intern();
    bench_nanbox();
    bench_hashmap();
    bench_dispatch();
    bench_grammar();
    bench_jit();
    bench_cc();

    pf("\n=== done ===\n");
}

#endif // BENCH_C_INCLUDED
