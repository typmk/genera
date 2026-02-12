/**
 * x86.c — Clojure-to-x86-64 JIT Compiler
 *
 * Source → intern → query as x86 → mmap → execute. No gcc. No assembler.
 * Phase 3: we ARE the compiler.
 * FREESTANDING: no libc. Raw syscalls only.
 *
 * Build:
 *   gcc -O3 -march=native -o x86 src/c/x86.c
 *
 * Usage:
 *   ./x86     # tests + benchmarks
 */

#include "sys.c"
#include "base.c"
#include "read.c"
#include "emit_x86.c"

// ============================================================================
// Tests
// ============================================================================

static int t_pass, t_fail;

static void check(const char *name, const char *source, i64 expected) {
    i64 got = jit_run(source);
    if (got == expected) {
        t_pass++;
    } else {
        pf("  FAIL %s: expected %lld, got %lld\n", name, (long long)expected, (long long)got);
        t_fail++;
    }
}

static void run_tests(void) {
    pf("=== x86 tests (Clojure -> x86-64 JIT, freestanding) ===\n");
    t_pass = t_fail = 0;

    // Literals
    check("int", "42", 42);
    check("neg", "-7", -7);
    check("zero", "0", 0);

    // Arithmetic
    check("add", "(+ 1 2)", 3);
    check("sub", "(- 10 3)", 7);
    check("mul", "(* 6 7)", 42);
    check("div", "(/ 20 4)", 5);
    check("mod", "(mod 17 5)", 2);
    check("neg-unary", "(- 5)", -5);
    check("add3", "(+ 1 2 3)", 6);
    check("mul3", "(* 2 3 4)", 24);
    check("nested", "(+ (* 3 4) (- 10 5))", 17);

    // Comparisons
    check("lt-t", "(< 1 2)", 1);
    check("lt-f", "(< 2 1)", 0);
    check("gt-t", "(> 5 3)", 1);
    check("eq-t", "(= 7 7)", 1);
    check("eq-f", "(= 7 8)", 0);
    check("lte", "(<= 3 3)", 1);
    check("gte", "(>= 5 3)", 1);

    // If
    check("if-t", "(if (< 1 2) 42 99)", 42);
    check("if-f", "(if (> 1 2) 42 99)", 99);
    check("if-nested", "(if (< 1 2) (if (< 3 4) 10 20) 30)", 10);

    // Let
    check("let", "(let [x 10] x)", 10);
    check("let2", "(let [x 10 y 20] (+ x y))", 30);
    check("let-nested", "(let [x 10] (let [y 20] (+ x y)))", 30);
    check("let-shadow", "(let [x 10] (let [x 20] x))", 20);

    // Do
    check("do", "(do 1 2 3)", 3);

    // And / Or
    check("and-t", "(and 1 2 3)", 3);
    check("and-f", "(and 1 0 3)", 0);
    check("or-t", "(or 0 0 5)", 5);
    check("or-f", "(or 0 0 0)", 0);

    // Predicates
    check("not-t", "(not 0)", 1);
    check("not-f", "(not 5)", 0);
    check("inc", "(inc 41)", 42);
    check("dec", "(dec 43)", 42);
    check("zero?-t", "(zero? 0)", 1);
    check("zero?-f", "(zero? 5)", 0);
    check("pos?-t", "(pos? 5)", 1);
    check("neg?-t", "(neg? -3)", 1);

    // Defn + call
    check("defn", "(defn add [a b] (+ a b)) (add 3 4)", 7);
    check("defn2", "(defn sq [x] (* x x)) (sq 6)", 36);
    check("multi-fn",
          "(defn double [x] (* x 2)) (defn triple [x] (* x 3)) (+ (double 5) (triple 3))", 19);

    // Recursion
    check("fib10",
          "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)", 55);

    // TCO
    check("tco-fact",
          "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc)))) (fact 20 1)",
          2432902008176640000LL);

    // Loop/recur
    check("loop",
          "(loop [i 0 s 0] (if (>= i 10) s (recur (inc i) (+ s i))))", 45);

    // Def
    check("def", "(def x 42) x", 42);
    check("def-expr", "(def x (+ 10 20)) (+ x 5)", 35);

    // Complex
    check("euler1",
          "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) (if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s))))) (solve 1000)",
          233168);

    pf("  %d passed, %d failed\n", t_pass, t_fail);
}

// ============================================================================
// Benchmarks
// ============================================================================

static void run_bench(void) {
    pf("\n=== benchmarks ===\n");

    // fib(35) execution
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
        u64 dt = t1 - t0;
        if (dt < best) best = dt;
    }
    pf("  fib(35) JIT:       ");
    buf_f1(&g_print_buf, (f64)best / 1e6);
    pf(" ms\n");

    // Compilation speed
    N = 10000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++)
        compile_program(fib_src);
    u64 t1 = now_ns();
    pf("  compile fib:       ");
    buf_f1(&g_print_buf, (f64)(t1 - t0) / (N * 1e3));
    pf(" us  (source -> x86 bytes)\n");

    // Code size
    compile_program(fib_src);
    pf("  code size:         %u bytes (fib + entry)\n", g_code.pos);
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    base_init();
    init_syms();

    // Allocate executable code buffer via raw mmap
    g_code.code = (u8 *)sys_alloc_exec(CODE_SIZE);
    g_code.cap = CODE_SIZE;
    g_code.pos = 0;

    if (!g_code.code) {
        pf("mmap failed\n");
        return 1;
    }

    run_tests();
    run_bench();

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    sys_free_exec(g_code.code, CODE_SIZE);
    base_cleanup();
    return t_fail ? 1 : 0;
}
