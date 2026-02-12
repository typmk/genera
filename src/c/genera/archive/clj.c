/**
 * clj.c — Clojure-to-C Compiler
 *
 * Source → intern → query → emit C → gcc → native binary.
 * Free C FFI: output IS C.
 * FREESTANDING: no libc. Raw syscalls for everything including gcc subprocess.
 *
 * Build:
 *   gcc -O3 -march=native -o clj src/c/clj.c
 *
 * Usage:
 *   ./clj                    # tests + benchmarks
 *   ./clj --emit file.clj    # emit C to stdout
 *   ./clj --run file.clj     # compile and run
 */

#include "sys.c"
#include "base.c"
#include "read.c"
#include "emit_c.c"

// ============================================================================
// Driver — compile + capture output
// ============================================================================

static char g_captured[65536];
static u32 g_captured_len;

static int compile_and_capture(const char *source) {
    arena_reset(&g_req);
    classify(source);
    emit_program();

    // Write generated C to temp file
    sys_write_file("/tmp/_clj_out.c", g_out.buf, g_out.pos);

    // Run gcc
    char *gcc_argv[] = {"/usr/bin/gcc", "-O2", "-w", "-o", "/tmp/_clj_out", "/tmp/_clj_out.c", NULL};
    int rc = sys_run("/usr/bin/gcc", gcc_argv);
    if (rc != 0) {
        memcpy(g_captured, "COMPILE_ERROR", 14);
        g_captured_len = 13;
        return -1;
    }

    // Run binary and capture output
    char *run_argv[] = {"/tmp/_clj_out", NULL};
    rc = sys_run_capture("/tmp/_clj_out", run_argv, g_captured, sizeof(g_captured), &g_captured_len);

    // Strip trailing newlines
    while (g_captured_len > 0 && g_captured[g_captured_len - 1] == '\n')
        g_captured[--g_captured_len] = '\0';

    return rc;
}

// ============================================================================
// Tests
// ============================================================================

static int t_pass, t_fail;

static void test_e2e(const char *name, const char *source, const char *expected) {
    int rc = compile_and_capture(source);
    if (rc != 0 && strcmp(g_captured, "COMPILE_ERROR") == 0) {
        pf("  FAIL %s (compile error)\n", name);
        pf("    source: %s\n", source);
        t_fail++; return;
    }
    if (strcmp(g_captured, expected) == 0) {
        t_pass++;
    } else {
        pf("  FAIL %s\n    expected: '%s'\n    got:      '%s'\n", name, expected, g_captured);
        t_fail++;
    }
}

static void run_tests(void) {
    pf("=== clj tests (Clojure → C → native) ===\n");
    t_pass = t_fail = 0;

    // Arithmetic
    test_e2e("add", "(println (+ 1 2))", "3");
    test_e2e("mul", "(println (* 6 7))", "42");
    test_e2e("sub", "(println (- 10 3))", "7");
    test_e2e("div", "(println (/ 10 3))", "3");
    test_e2e("mod", "(println (mod 10 3))", "1");
    test_e2e("neg", "(println (- 5))", "-5");
    test_e2e("nested", "(println (+ (* 2 3) (- 10 4)))", "12");
    test_e2e("variadic", "(println (+ 1 2 3 4 5))", "15");

    // Comparison
    test_e2e("lt", "(println (< 1 2))", "1");
    test_e2e("gt", "(println (> 1 2))", "0");
    test_e2e("eq", "(println (= 42 42))", "1");
    test_e2e("lte", "(println (<= 5 5))", "1");
    test_e2e("not0", "(println (not 0))", "1");
    test_e2e("not42", "(println (not 42))", "0");

    // Control flow
    test_e2e("if-t", "(println (if 1 10 20))", "10");
    test_e2e("if-f", "(println (if 0 10 20))", "20");
    test_e2e("let", "(println (let [a 1 b 2] (+ a b)))", "3");
    test_e2e("nested-let", "(println (let [a 1] (let [b (+ a 1)] (* a b))))", "2");
    test_e2e("do", "(println (do 1 2 42))", "42");
    test_e2e("and-t", "(println (and 1 2 3))", "3");
    test_e2e("and-f", "(println (and 1 0 3))", "0");
    test_e2e("or-t", "(println (or 0 0 42))", "42");
    test_e2e("or-f", "(println (or 7 0 42))", "7");

    // Def
    test_e2e("def", "(def x 42) (println x)", "42");
    test_e2e("def-expr", "(def x (+ 20 22)) (println x)", "42");

    // Functions
    test_e2e("defn", "(defn double [x] (* x 2)) (println (double 21))", "42");
    test_e2e("multi-defn",
        "(defn double [x] (* x 2))\n(defn quad [x] (double (double x)))\n(println (quad 10))", "40");
    test_e2e("fact-rec",
        "(defn fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))\n(println (fact 10))", "3628800");
    test_e2e("fib-rec",
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(println (fib 10))", "55");
    test_e2e("defn-let",
        "(defn calc [x y] (let [tmp (+ x y)] (* tmp 2)))\n(println (calc 10 11))", "42");

    // TCO
    test_e2e("tco-fact",
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n(println (fact 10 1))", "3628800");
    test_e2e("tco-sum",
        "(defn sum-to [n acc] (if (<= n 0) acc (recur (- n 1) (+ acc n))))\n(println (sum-to 100 0))", "5050");

    // Loop/recur
    test_e2e("loop",
        "(println (loop [i 0 s 0] (if (>= i 10) s (recur (+ i 1) (+ s i)))))", "45");
    test_e2e("loop-let",
        "(println (loop [i 0 s 0]\n  (if (>= i 5) s\n    (let [sq (* i i)] (recur (+ i 1) (+ s sq))))))", "30");

    // Strings
    test_e2e("str", "(println \"hello\")", "hello");
    test_e2e("str-mixed", "(println \"result:\" (+ 40 2))", "result: 42");

    // Predicates
    test_e2e("zero?", "(println (zero? 0))", "1");
    test_e2e("pos?", "(println (pos? 42))", "1");
    test_e2e("neg?", "(println (neg? -1))", "1");
    test_e2e("inc", "(println (inc 41))", "42");
    test_e2e("dec", "(println (dec 43))", "42");

    pf("  %d passed, %d failed\n", t_pass, t_fail);
}

// ============================================================================
// Benchmarks
// ============================================================================

static void run_bench(void) {
    pf("\n=== benchmarks ===\n");

    const char *fib_src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(println (fib 35))";

    // Emit speed
    int N = 10000;
    u64 t0 = now_ns();
    for (int i = 0; i < N; i++) {
        arena_reset(&g_req);
        classify(fib_src);
        emit_program();
    }
    u64 dt = now_ns() - t0;
    pf("  source -> C emit:  ");
    buf_f1(&g_print_buf, (f64)dt / N / 1000.0);
    pf(" us/op  (%d iters)\n", N);
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
    pf("  source -> binary:  ");
    buf_f1(&g_print_buf, (f64)dt / 1e6);
    pf(" ms  (includes gcc)\n");

    // Run
    t0 = now_ns();
    char *run_argv[] = {"/tmp/_clj_out", NULL};
    char devnull[4];
    u32 devnull_len;
    sys_run_capture("/tmp/_clj_out", run_argv, devnull, sizeof(devnull), &devnull_len);
    dt = now_ns() - t0;
    pf("  fib(35) native:    ");
    buf_f1(&g_print_buf, (f64)dt / 1e6);
    pf(" ms\n");
}

// ============================================================================
// Main
// ============================================================================

static void *alloc_for_file(u64 size) { return sys_alloc(size); }

int main(int argc, char **argv) {
    base_init();
    init_syms();

    if (argc > 2 && strcmp(argv[1], "--emit") == 0) {
        FileData f = sys_read_file(argv[2], alloc_for_file);
        if (!f.data) { pf("Cannot open: %s\n", argv[2]); return 1; }
        classify(f.data);
        emit_program();
        sys_write(1, g_out.buf, g_out.pos);
        sys_free(f.data, f.len + 1);
        base_cleanup();
        return 0;
    }

    if (argc > 2 && strcmp(argv[1], "--run") == 0) {
        FileData f = sys_read_file(argv[2], alloc_for_file);
        if (!f.data) { pf("Cannot open: %s\n", argv[2]); return 1; }
        int rc = compile_and_capture(f.data);
        if (rc != 0) { pf("Error\n"); sys_free(f.data, f.len + 1); return 1; }
        pf("%s\n", g_captured);
        sys_free(f.data, f.len + 1);
        base_cleanup();
        return 0;
    }

    pf("clj — Clojure-to-C Compiler (freestanding)\n");
    pf("Pipeline: source -> intern -> query -> emit C -> gcc -> native\n\n");

    run_tests();
    run_bench();

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    base_cleanup();
    return t_fail ? 1 : 0;
}
