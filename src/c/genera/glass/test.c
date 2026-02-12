/**
 * test.c — ALL Self-Tests
 *
 * ~325 tests covering every layer: base, platform, lang, emit.
 * Run via: ./moss test
 *
 * Depends on: emit/emit.c (and everything below)
 */
#ifndef TEST_C_INCLUDED
#define TEST_C_INCLUDED

// ============================================================================
// Test Harness (freestanding)
// ============================================================================

static int t_pass, t_fail;

#define TEST_CHECK(name, cond) do { \
    if (cond) { t_pass++; } \
    else { pf("  FAIL: %s\n", name); t_fail++; } \
} while(0)

static void check_i64(const char *name, i64 got, i64 expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s: expected %lld, got %lld\n", name,
              (long long)expected, (long long)got); t_fail++; }
}

static void check_val(const char *name, Val got, Val expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s\n", name); t_fail++; }
}

// ============================================================================
// 1. Arena Tests (7 tests)
// ============================================================================

static void test_arena(void) {
    pf("\n--- arena ---\n");
    Arena a = arena_create(4096);

    void *p1 = arena_alloc(&a, 3, 8);
    void *p2 = arena_alloc(&a, 5, 8);
    TEST_CHECK("align-8", ((u64)p1 & 7) == 0 && ((u64)p2 & 7) == 0);

    void *p3 = arena_alloc(&a, 1, 64);
    TEST_CHECK("align-64", ((u64)p3 & 63) == 0);

    arena_reset(&a);
    void *p4 = arena_alloc(&a, 16, 8);
    TEST_CHECK("reset-reuse", a.current->used == 16 && p4 != NULL);

    arena_reset(&a);
    void *big = arena_alloc(&a, 8192, 8);
    TEST_CHECK("block-chain", big != NULL && a.current->prev != NULL);

    arena_reset(&a);
    arena_alloc(&a, 100, 8);
    ArenaMark mark = arena_begin_temp(&a);
    arena_alloc(&a, 200, 8);
    arena_end_temp(mark);
    TEST_CHECK("temp-mark", a.current->used == mark.used);

    arena_reset(&a);
    u64 *v = arena_push(&a, u64);
    *v = 0xDEADBEEF;
    TEST_CHECK("push-typed", *v == 0xDEADBEEF);

    arena_reset(&a);
    u32 *arr = arena_push_n(&a, u32, 100);
    for (int i = 0; i < 100; i++) arr[i] = i;
    bool ok = true;
    for (int i = 0; i < 100; i++) if (arr[i] != (u32)i) ok = false;
    TEST_CHECK("push-n", ok);

    arena_destroy(&a);
}

// ============================================================================
// 2. Str Tests (8 tests)
// ============================================================================

static void test_str(void) {
    pf("\n--- str ---\n");

    Str a = STR_LIT("hello"), b = STR_LIT("hello"), c = STR_LIT("world");
    TEST_CHECK("str-eq-same", str_eq(a, b));
    TEST_CHECK("str-eq-diff", !str_eq(a, c));

    Str d = STR_LIT("hell");
    TEST_CHECK("str-eq-len", !str_eq(a, d));

    Str s = STR_LIT("hello world");
    Str sl = str_slice(s, 6, 5);
    TEST_CHECK("str-slice", str_eq(sl, STR_LIT("world")));

    Str sl2 = str_slice(s, 20, 5);
    TEST_CHECK("str-slice-past", sl2.len == 0);

    u32 h1 = str_hash(a), h2 = str_hash(a);
    TEST_CHECK("str-hash-consistent", h1 == h2 && h1 != 0);

    u32 h3 = str_hash(c);
    TEST_CHECK("str-hash-diff", h1 != h3);

    Arena tmp = arena_create(1024);
    Str dup = str_dup(&tmp, a);
    TEST_CHECK("str-dup", str_eq(dup, a) && dup.data != a.data);
    arena_destroy(&tmp);
}

// ============================================================================
// 3. Intern Tests (5 tests)
// ============================================================================

static void test_intern(void) {
    pf("\n--- intern ---\n");

    StrId id1 = str_intern(STR_LIT("test_foo"));
    StrId id2 = str_intern(STR_LIT("test_foo"));
    TEST_CHECK("intern-same", id1 == id2);

    StrId id3 = str_intern(STR_LIT("test_bar"));
    TEST_CHECK("intern-diff", id1 != id3);

    Str recovered = str_from_id(id1);
    TEST_CHECK("intern-roundtrip", str_eq(recovered, STR_LIT("test_foo")));

    TEST_CHECK("strid-eq", strid_eq(id1, id2) && !strid_eq(id1, id3));

    // Many interned strings
    bool all_unique = true;
    StrId ids[100];
    char buf[32];
    for (int i = 0; i < 100; i++) {
        u32 len = 0;
        buf[len++] = 't'; buf[len++] = '_';
        if (i >= 10) buf[len++] = '0' + (i / 10);
        buf[len++] = '0' + (i % 10);
        ids[i] = str_intern((Str){(u8 *)buf, len});
    }
    for (int i = 0; i < 100; i++)
        for (int j = i + 1; j < 100 && j < i + 5; j++)
            if (ids[i] == ids[j]) all_unique = false;
    TEST_CHECK("intern-100-unique", all_unique);
}

// ============================================================================
// 4. NaN Boxing Tests (14 tests)
// ============================================================================

static void test_nanbox(void) {
    pf("\n--- nanbox ---\n");

    Val nil = val_nil();
    TEST_CHECK("nil", val_is_nil(nil) && !val_is_int(nil) && !val_is_f64(nil));

    Val vt = val_true(), vf = val_false();
    TEST_CHECK("bool", val_is_bool(vt) && val_as_bool(vt) && val_is_bool(vf) && !val_as_bool(vf));

    TEST_CHECK("int-pos", val_is_int(val_int(42)) && val_as_int(val_int(42)) == 42);
    TEST_CHECK("int-neg", val_is_int(val_int(-7)) && val_as_int(val_int(-7)) == -7);
    TEST_CHECK("int-zero", val_is_int(val_int(0)) && val_as_int(val_int(0)) == 0);
    TEST_CHECK("int-large", val_as_int(val_int(140737488355327LL)) == 140737488355327LL);
    TEST_CHECK("int-neg-large", val_as_int(val_int(-140737488355328LL)) == -140737488355328LL);

    TEST_CHECK("f64-pi", val_is_f64(val_f64(3.14159)) && val_as_f64(val_f64(3.14159)) == 3.14159);
    TEST_CHECK("f64-zero", val_is_f64(val_f64(0.0)) && val_as_f64(val_f64(0.0)) == 0.0);
    TEST_CHECK("f64-neg", val_is_f64(val_f64(-2.5)) && val_as_f64(val_f64(-2.5)) == -2.5);

    StrId sym = str_intern(STR_LIT("test_sym"));
    TEST_CHECK("sym", val_is_sym(val_sym(sym)) && val_as_sym(val_sym(sym)) == sym);

    StrId kw = str_intern(STR_LIT("test_kw"));
    TEST_CHECK("kw", val_is_kw(val_kw(kw)) && val_as_kw(val_kw(kw)) == kw);

    // Mutual exclusivity
    Val vals[] = {val_nil(), val_true(), val_int(1), val_f64(1.0), val_sym(0), val_kw(0)};
    bool (*checks[])(Val) = {val_is_nil, val_is_bool, val_is_int, val_is_f64, val_is_sym, val_is_kw};
    bool exclusive = true;
    for (int i = 0; i < 6; i++)
        for (int j = 0; j < 6; j++)
            if (i == j ? !checks[j](vals[i]) : checks[j](vals[i])) exclusive = false;
    TEST_CHECK("exclusive", exclusive);

    TEST_CHECK("truthiness",
        val_truthy(val_true()) && !val_truthy(val_false()) &&
        !val_truthy(val_nil()) && val_truthy(val_int(0)) && val_truthy(val_int(42)));
}

// ============================================================================
// 5. DynArray Tests (6 tests)
// ============================================================================

static void test_dynarray(void) {
    pf("\n--- dynarray ---\n");
    Arena a = arena_create(4096);

    u32 *arr = NULL;
    arr_push(arr, 10, &a);
    arr_push(arr, 20, &a);
    arr_push(arr, 30, &a);
    TEST_CHECK("push-count", arr_count(arr) == 3 && arr[0] == 10 && arr[1] == 20 && arr[2] == 30);

    u32 v = arr_pop(arr);
    TEST_CHECK("pop", v == 30 && arr_count(arr) == 2);

    arr_clear(arr);
    for (int i = 0; i < 100; i++) arr_push(arr, (u32)i, &a);
    bool ok = arr_count(arr) == 100;
    for (int i = 0; i < 100; i++) if (arr[i] != (u32)i) ok = false;
    TEST_CHECK("grow-100", ok);

    TEST_CHECK("last", arr_last(arr) == 99);

    arr_clear(arr);
    TEST_CHECK("clear", arr_count(arr) == 0);

    u32 *null_arr = NULL;
    TEST_CHECK("null-count", arr_count(null_arr) == 0);

    arena_destroy(&a);
}

// ============================================================================
// 6. HashMap Tests (7 tests)
// ============================================================================

static void test_hashmap(void) {
    pf("\n--- hashmap ---\n");
    Arena a = arena_create(1 << 20);
    HashMap m = hashmap_create(&a, 16);
    Val v;

    hashmap_put(&m, 42, val_int(100));
    TEST_CHECK("put-get", hashmap_get(&m, 42, &v) && val_as_int(v) == 100);

    TEST_CHECK("get-missing", !hashmap_get(&m, 99, &v));

    hashmap_put(&m, 42, val_int(200));
    hashmap_get(&m, 42, &v);
    TEST_CHECK("update", val_as_int(v) == 200 && m.count == 1);

    hashmap_put(&m, 50, val_int(500));
    TEST_CHECK("delete", hashmap_del(&m, 50) && !hashmap_get(&m, 50, &v) && m.count == 1);

    TEST_CHECK("del-missing", !hashmap_del(&m, 999));

    HashMap m2 = hashmap_create(&a, 16);
    for (u32 i = 0; i < 100; i++) hashmap_put(&m2, i + 1000, val_int(i));
    bool ok = true;
    for (u32 i = 0; i < 100; i++)
        if (!hashmap_get(&m2, i + 1000, &v) || val_as_int(v) != (i64)i) ok = false;
    TEST_CHECK("collision-100", ok && m2.count == 100);

    HashMap m3 = hashmap_create(&a, 16);
    for (u32 i = 0; i < 200; i++) hashmap_put(&m3, i, val_int(i));
    TEST_CHECK("grow", m3.count == 200 && m3.cap >= 256);

    arena_destroy(&a);
}

// ============================================================================
// 7. Protocol Dispatch Tests (16 tests)
// ============================================================================

static void test_dispatch(void) {
    pf("\n--- dispatch ---\n");

    // Tag index
    check_i64("idx-nil",  val_tag_idx(val_nil()),  TI_NIL_T);
    check_i64("idx-bool", val_tag_idx(val_true()), TI_BOOL);
    check_i64("idx-int",  val_tag_idx(val_int(42)),TI_INT);
    check_i64("idx-sym",  val_tag_idx(val_sym(0)), TI_SYM);
    check_i64("idx-kw",   val_tag_idx(val_kw(0)),  TI_KW);
    check_i64("idx-f64",  val_tag_idx(val_f64(3.14)), TI_F64);

    Val c = cons_new(val_int(1), val_nil());
    check_i64("idx-cons", val_tag_idx(c), TI_CONS);

    // Tier 1: table dispatch
    Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
    check_val("t1-first", t_first(list), val_int(10));
    check_val("t1-nil",   t_first(val_nil()), val_nil());
    check_val("t1-rest",  t_first(t_rest(list)), val_int(20));

    // Tier 2: fast path
    check_val("t2-first", p_first(list), val_int(10));
    check_val("t2-nil",   p_first(val_nil()), val_nil());
    check_val("t2-rest",  p_first(p_rest(list)), val_int(20));
    check_val("t2-rnil",  p_rest(val_nil()), val_nil());

    // Tier 3: fused
    Val f, r;
    TEST_CHECK("t3-cons", p_first_rest(list, &f, &r) == true);
    check_val("t3-car", f, val_int(10));
}

// ============================================================================
// 8. Grammar Tests — BF (4 tests)
// ============================================================================

static void test_grammar_bf(void) {
    pf("\n--- grammar: bf ---\n");
    Lang l; lang_bf(&l);
    const char *src = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    TEST_CHECK("bf-parsed", g.n > 1);
    TEST_CHECK("bf-root", gn_kind(&g, 0) == NK_ROOT);

    u32 ops = bm_pop(g.m[NK_OP], g.mw);
    u32 vecs = bm_pop(g.m[NK_VEC], g.mw);
    TEST_CHECK("bf-ops", ops > 10);
    TEST_CHECK("bf-loops", vecs >= 3);
}

// ============================================================================
// 9. Grammar Tests — Lisp (10 tests)
// ============================================================================

static void test_grammar_lisp(void) {
    pf("\n--- grammar: lisp ---\n");
    Lang l; lang_lisp(&l);
    const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    TEST_CHECK("lisp-parsed", g.n > 1);

    u32 defn = gn_child(&g, 0);
    TEST_CHECK("lisp-defn-list", gn_kind(&g, defn) == NK_LIST);

    u32 head = gn_child(&g, defn);
    TEST_CHECK("lisp-head-ident", gn_kind(&g, head) == NK_IDENT);
    TEST_CHECK("lisp-head=defn", str_eq(gn_text(&g, head), STR_LIT("defn")));

    u32 fname = gn_next(&g, head);
    TEST_CHECK("lisp-name=fib", str_eq(gn_text(&g, fname), STR_LIT("fib")));

    u32 params = gn_next(&g, fname);
    TEST_CHECK("lisp-params-vec", gn_kind(&g, params) == NK_VEC);
    TEST_CHECK("lisp-param-n", str_eq(gn_text(&g, gn_child(&g, params)), STR_LIT("n")));

    // Negative number
    const char *s2 = "(+ -7 3)";
    Gram g2 = gram_new(64);
    gram_parse(&g2, &l, s2, strlen(s2));
    u32 neg = gn_nth(&g2, gn_child(&g2, 0), 1);
    TEST_CHECK("lisp-negnum", gn_kind(&g2, neg) == NK_NUM);
    TEST_CHECK("lisp-negnum-text", str_eq(gn_text(&g2, neg), STR_LIT("-7")));
}

// ============================================================================
// 10. Grammar Tests — C (4 tests)
// ============================================================================

static void test_grammar_c(void) {
    pf("\n--- grammar: c ---\n");
    Lang l; lang_c(&l);
    const char *src = "int fib(int n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    TEST_CHECK("c-parsed", g.n > 5);
    u32 idents = bm_pop(g.m[NK_IDENT], g.mw);
    u32 nums = bm_pop(g.m[NK_NUM], g.mw);
    u32 groups = bm_pop(g.m_group, g.mw);
    TEST_CHECK("c-idents", idents > 0);
    TEST_CHECK("c-nums", nums > 0);
    TEST_CHECK("c-groups", groups > 0);
}

// ============================================================================
// 11. Grammar Query Tests (6 tests)
// ============================================================================

static void test_grammar_query(void) {
    pf("\n--- grammar: query ---\n");
    Lang l; lang_lisp(&l);
    const char *src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(defn add [a b] (+ a b))\n"
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n"
        "(def PI 314)\n"
        "(fib 10)";

    Gram g = gram_new(4096);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    StrId s_defn  = str_intern(STR_LIT("defn"));
    StrId s_def   = str_intern(STR_LIT("def"));
    StrId s_recur = str_intern(STR_LIT("recur"));

    // Build recur bitmask
    u64 *m_recur = bm_new(g.mw);
    for (u32 i = bm_next(g.m[NK_LIST], g.mw, 0); i < g.n;
             i = bm_next(g.m[NK_LIST], g.mw, i + 1)) {
        u32 fc = gn_child(&g, i);
        if (fc && gn_kind(&g, fc) == NK_IDENT && gn_intern(&g, fc) == s_recur)
            BM_SET(m_recur, i);
    }

    // Classify top-level
    u32 n_defns = 0, n_defs = 0, n_mains = 0;
    u32 defn_nodes[16];
    u32 ch = gn_child(&g, 0);
    while (ch) {
        if (gn_kind(&g, ch) == NK_LIST) {
            u32 fc = gn_child(&g, ch);
            if (fc && gn_kind(&g, fc) == NK_IDENT) {
                StrId name = gn_intern(&g, fc);
                if (name == s_defn) { defn_nodes[n_defns++] = ch; ch = gn_next(&g, ch); continue; }
                if (name == s_def)  { n_defs++; ch = gn_next(&g, ch); continue; }
            }
        }
        n_mains++;
        ch = gn_next(&g, ch);
    }

    TEST_CHECK("query-3defns", n_defns == 3);
    TEST_CHECK("query-1def", n_defs == 1);
    TEST_CHECK("query-1main", n_mains == 1);

    TEST_CHECK("query-fib-no-recur", !gn_has(&g, m_recur, defn_nodes[0]));
    TEST_CHECK("query-add-no-recur", !gn_has(&g, m_recur, defn_nodes[1]));
    TEST_CHECK("query-fact-recur", gn_has(&g, m_recur, defn_nodes[2]));
}

// ============================================================================
// 12. x86 JIT Tests (48 tests)
// ============================================================================

static void check_jit(const char *name, const char *source, i64 expected) {
    i64 got = jit_run(source);
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s: expected %lld, got %lld\n", name,
              (long long)expected, (long long)got); t_fail++; }
}

static void test_jit(void) {
    pf("\n--- x86 jit ---\n");

    // Literals
    check_jit("int", "42", 42);
    check_jit("neg", "-7", -7);
    check_jit("zero", "0", 0);

    // Arithmetic
    check_jit("add", "(+ 1 2)", 3);
    check_jit("sub", "(- 10 3)", 7);
    check_jit("mul", "(* 6 7)", 42);
    check_jit("div", "(/ 20 4)", 5);
    check_jit("mod", "(mod 17 5)", 2);
    check_jit("neg-unary", "(- 5)", -5);
    check_jit("add3", "(+ 1 2 3)", 6);
    check_jit("mul3", "(* 2 3 4)", 24);
    check_jit("nested", "(+ (* 3 4) (- 10 5))", 17);

    // Comparisons
    check_jit("lt-t", "(< 1 2)", 1);
    check_jit("lt-f", "(< 2 1)", 0);
    check_jit("gt-t", "(> 5 3)", 1);
    check_jit("eq-t", "(= 7 7)", 1);
    check_jit("eq-f", "(= 7 8)", 0);
    check_jit("lte", "(<= 3 3)", 1);
    check_jit("gte", "(>= 5 3)", 1);

    // If
    check_jit("if-t", "(if (< 1 2) 42 99)", 42);
    check_jit("if-f", "(if (> 1 2) 42 99)", 99);
    check_jit("if-nested", "(if (< 1 2) (if (< 3 4) 10 20) 30)", 10);

    // Let
    check_jit("let", "(let [x 10] x)", 10);
    check_jit("let2", "(let [x 10 y 20] (+ x y))", 30);
    check_jit("let-nested", "(let [x 10] (let [y 20] (+ x y)))", 30);
    check_jit("let-shadow", "(let [x 10] (let [x 20] x))", 20);

    // Do
    check_jit("do", "(do 1 2 3)", 3);

    // And / Or
    check_jit("and-t", "(and 1 2 3)", 3);
    check_jit("and-f", "(and 1 0 3)", 0);
    check_jit("or-t", "(or 0 0 5)", 5);
    check_jit("or-f", "(or 0 0 0)", 0);

    // Predicates
    check_jit("not-t", "(not 0)", 1);
    check_jit("not-f", "(not 5)", 0);
    check_jit("inc", "(inc 41)", 42);
    check_jit("dec", "(dec 43)", 42);
    check_jit("zero?-t", "(zero? 0)", 1);
    check_jit("zero?-f", "(zero? 5)", 0);
    check_jit("pos?-t", "(pos? 5)", 1);
    check_jit("neg?-t", "(neg? -3)", 1);

    // Defn + call
    check_jit("defn", "(defn add [a b] (+ a b)) (add 3 4)", 7);
    check_jit("defn2", "(defn sq [x] (* x x)) (sq 6)", 36);
    check_jit("multi-fn",
        "(defn double [x] (* x 2)) (defn triple [x] (* x 3)) (+ (double 5) (triple 3))", 19);

    // Recursion
    check_jit("fib10",
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)", 55);

    // TCO
    check_jit("tco-fact",
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc)))) (fact 20 1)",
        2432902008176640000LL);

    // Loop/recur
    check_jit("loop",
        "(loop [i 0 s 0] (if (>= i 10) s (recur (inc i) (+ s i))))", 45);

    // Def
    check_jit("def", "(def x 42) x", 42);
    check_jit("def-expr", "(def x (+ 10 20)) (+ x 5)", 35);

    // Complex
    check_jit("euler1",
        "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) (if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s))))) (solve 1000)",
        233168);

    // Cond
    check_jit("cond-1", "(cond (< 5 3) 0 (> 5 3) 1)", 1);
    check_jit("cond-else", "(cond 0 99 1 42)", 42);
    check_jit("cond-none", "(cond 0 1 0 2)", 0);

    // When
    check_jit("when-t", "(when 1 42)", 42);
    check_jit("when-f", "(when 0 42)", 0);
}

// ============================================================================
// 13. C Emitter Tests (41 tests)
// ============================================================================

static void check_cc(const char *name, const char *source, const char *expected) {
    int rc = compile_and_capture(source);
    if (rc != 0 && strcmp(g_captured, "COMPILE_ERROR") == 0) {
        pf("  FAIL %s (compile error)\n", name);
        t_fail++; return;
    }
    if (strcmp(g_captured, expected) == 0) {
        t_pass++;
    } else {
        pf("  FAIL %s\n    expected: '%s'\n    got:      '%s'\n", name, expected, g_captured);
        t_fail++;
    }
}

static void test_cc(void) {
    pf("\n--- clj -> c -> native ---\n");

    // Arithmetic
    check_cc("add", "(println (+ 1 2))", "3");
    check_cc("mul", "(println (* 6 7))", "42");
    check_cc("sub", "(println (- 10 3))", "7");
    check_cc("div", "(println (/ 10 3))", "3");
    check_cc("mod", "(println (mod 10 3))", "1");
    check_cc("neg", "(println (- 5))", "-5");
    check_cc("nested", "(println (+ (* 2 3) (- 10 4)))", "12");
    check_cc("variadic", "(println (+ 1 2 3 4 5))", "15");

    // Comparison
    check_cc("lt", "(println (< 1 2))", "1");
    check_cc("gt", "(println (> 1 2))", "0");
    check_cc("eq", "(println (= 42 42))", "1");
    check_cc("lte", "(println (<= 5 5))", "1");
    check_cc("not0", "(println (not 0))", "1");
    check_cc("not42", "(println (not 42))", "0");

    // Control flow
    check_cc("if-t", "(println (if 1 10 20))", "10");
    check_cc("if-f", "(println (if 0 10 20))", "20");
    check_cc("let", "(println (let [a 1 b 2] (+ a b)))", "3");
    check_cc("nested-let", "(println (let [a 1] (let [b (+ a 1)] (* a b))))", "2");
    check_cc("do", "(println (do 1 2 42))", "42");
    check_cc("and-t", "(println (and 1 2 3))", "3");
    check_cc("and-f", "(println (and 1 0 3))", "0");
    check_cc("or-t", "(println (or 0 0 42))", "42");
    check_cc("or-f", "(println (or 7 0 42))", "7");

    // Def
    check_cc("def", "(def x 42) (println x)", "42");
    check_cc("def-expr", "(def x (+ 20 22)) (println x)", "42");

    // Functions
    check_cc("defn", "(defn double [x] (* x 2)) (println (double 21))", "42");
    check_cc("multi-defn",
        "(defn double [x] (* x 2))\n(defn quad [x] (double (double x)))\n(println (quad 10))", "40");
    check_cc("fact-rec",
        "(defn fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))\n(println (fact 10))", "3628800");
    check_cc("fib-rec",
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(println (fib 10))", "55");
    check_cc("defn-let",
        "(defn calc [x y] (let [tmp (+ x y)] (* tmp 2)))\n(println (calc 10 11))", "42");

    // TCO
    check_cc("tco-fact",
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n(println (fact 10 1))", "3628800");
    check_cc("tco-sum",
        "(defn sum-to [n acc] (if (<= n 0) acc (recur (- n 1) (+ acc n))))\n(println (sum-to 100 0))", "5050");

    // Loop/recur
    check_cc("loop",
        "(println (loop [i 0 s 0] (if (>= i 10) s (recur (+ i 1) (+ s i)))))", "45");
    check_cc("loop-let",
        "(println (loop [i 0 s 0]\n  (if (>= i 5) s\n    (let [sq (* i i)] (recur (+ i 1) (+ s sq))))))", "30");

    // Strings
    check_cc("str", "(println \"hello\")", "hello");
    check_cc("str-mixed", "(println \"result:\" (+ 40 2))", "result: 42");

    // Predicates
    check_cc("zero?", "(println (zero? 0))", "1");
    check_cc("pos?", "(println (pos? 42))", "1");
    check_cc("neg?", "(println (neg? -1))", "1");
    check_cc("inc", "(println (inc 41))", "42");
    check_cc("dec", "(println (dec 43))", "42");

    // Cond
    check_cc("cond", "(println (cond (< 5 3) 0 (> 5 3) 42))", "42");
    check_cc("cond-else", "(println (cond 0 1 1 42))", "42");

    // When
    check_cc("when-t", "(println (when 1 42))", "42");
    check_cc("when-f", "(println (when 0 42))", "0");
}

// ============================================================================
// 14. Eval / Interpreter Tests (12 tests)
// ============================================================================

static void check_eval(const char *name, const char *source, const char *expected) {
    arena_reset(&g_req);
    Val result = eval_string(source, g_global_env);
    const char *got = print_val(result);
    if (strcmp(got, expected) == 0) { t_pass++; }
    else { pf("  FAIL %s: expected '%s', got '%s'\n", name, expected, got); t_fail++; }
}

static void test_eval_basic(void) {
    pf("\n--- eval: basic ---\n");
    check_eval("int", "42", "42");
    check_eval("neg", "-7", "-7");
    check_eval("bool", "true", "true");
    check_eval("nil", "nil", "nil");
    check_eval("add", "(+ 1 2 3)", "6");
    check_eval("if-t", "(if true 1 2)", "1");
    check_eval("if-f", "(if false 1 2)", "2");
    check_eval("let", "(let [x 10 y 20] (+ x y))", "30");
    check_eval("fn", "((fn [x] (* x x)) 5)", "25");
    check_eval("defn", "(defn sq [x] (* x x)) (sq 7)", "49");
    check_eval("and", "(and 1 2 3)", "3");
    check_eval("or", "(or false nil 5)", "5");
}

// ============================================================================
// 15. Collection Tests (20 tests)
// ============================================================================

static void test_collections(void) {
    pf("\n--- collections ---\n");

    // Vector literals
    check_eval("vec-lit", "[1 2 3]", "[1 2 3]");
    check_eval("vec-empty", "[]", "[]");
    check_eval("vec-nested", "[[1 2] [3 4]]", "[[1 2] [3 4]]");

    // Map literals
    check_eval("map-lit", "{:a 1 :b 2}", "{:a 1 :b 2}");
    check_eval("map-empty", "{}", "{}");

    // Keyword self-eval
    check_eval("kw", ":foo", ":foo");

    // get
    check_eval("get-map", "(get {:a 1 :b 2} :a)", "1");
    check_eval("get-vec", "(get [10 20 30] 1)", "20");
    check_eval("get-miss", "(get {:a 1} :z)", "nil");

    // assoc
    check_eval("assoc-map", "(get (assoc {:a 1} :b 2) :b)", "2");
    check_eval("assoc-vec", "(get (assoc [10 20 30] 1 99) 1)", "99");

    // conj
    check_eval("conj-vec", "(conj [1 2] 3)", "[1 2 3]");
    check_eval("conj-list", "(first (conj (list 2 3) 1))", "1");

    // nth
    check_eval("nth-vec", "(nth [10 20 30] 2)", "30");
    check_eval("nth-list", "(nth (list 10 20 30) 1)", "20");

    // count
    check_eval("count-vec", "(count [1 2 3])", "3");
    check_eval("count-map", "(count {:a 1 :b 2})", "2");
    check_eval("count-list", "(count (list 1 2 3 4))", "4");

    // vec
    check_eval("vec-from-list", "(vec (list 1 2 3))", "[1 2 3]");

    // empty?
    check_eval("emptyq", "(empty? [])", "true");
    check_eval("not-emptyq", "(empty? [1])", "false");
}

// ============================================================================
// 16. Collection Advanced Tests (10 tests)
// ============================================================================

static void test_collections_adv(void) {
    pf("\n--- collections: advanced ---\n");

    // keys / vals
    check_eval("keys", "(count (keys {:a 1 :b 2 :c 3}))", "3");
    check_eval("vals", "(count (vals {:a 1 :b 2 :c 3}))", "3");

    // contains?
    check_eval("contains-t", "(contains? {:a 1} :a)", "true");
    check_eval("contains-f", "(contains? {:a 1} :b)", "false");

    // type predicates
    check_eval("vecq", "(vector? [1 2])", "true");
    check_eval("mapq", "(map? {:a 1})", "true");
    check_eval("kwq", "(keyword? :foo)", "true");
    check_eval("not-vecq", "(vector? 42)", "false");

    // into
    check_eval("into-vec", "(into [] (list 1 2 3))", "[1 2 3]");

    // hash-map
    check_eval("hash-map", "(get (hash-map :x 10 :y 20) :x)", "10");
}

// ============================================================================
// 17. Core Language Form Tests (15 tests)
// ============================================================================

static void test_core_forms(void) {
    pf("\n--- core forms ---\n");

    // cond
    check_eval("cond-1", "(cond (< 5 3) \"no\" (> 5 3) \"yes\")", "\"yes\"");
    check_eval("cond-else", "(cond false 1 :else 42)", "42");
    check_eval("cond-nil", "(cond false 1)", "nil");

    // when
    check_eval("when-t", "(when (> 5 3) 42)", "42");
    check_eval("when-f", "(when (< 5 3) 42)", "nil");
    check_eval("when-body", "(when true 1 2 3)", "3");

    // str
    check_eval("str-cat", "(str \"hello\" \" \" \"world\")", "\"hello world\"");
    check_eval("str-mix", "(str \"x=\" 42)", "\"x=42\"");

    // range
    check_eval("range-1", "(count (range 5))", "5");
    check_eval("range-2", "(first (range 5))", "0");
    check_eval("range-start", "(first (range 3 7))", "3");
    check_eval("range-len", "(count (range 3 7))", "4");

    // mixed arithmetic
    check_eval("add-f64", "(+ 1.5 2.5)", "4.0");
    check_eval("mul-mix", "(* 2 3.5)", "7.0");
    check_eval("lt-f64", "(< 1.5 2.0)", "true");
}

// ============================================================================
// 18. Protocol Dispatch on Collections (6 tests)
// ============================================================================

static void test_coll_proto(void) {
    pf("\n--- coll protocol ---\n");

    // first/rest on vectors
    check_eval("vec-first", "(first [10 20 30])", "10");
    check_eval("vec-rest", "(first (rest [10 20 30]))", "20");

    // seq on empty
    check_eval("vec-seq-nil", "(nil? (first []))", "true");

    // reduce over range
    check_eval("reduce-range", "(reduce + 0 (range 10))", "45");

    // map over vector (returns list)
    check_eval("map-vec", "(first (map (fn [x] (* x 2)) (list 3 4 5)))", "6");

    // filter
    check_eval("filter-list", "(count (filter (fn [x] (> x 2)) (list 1 2 3 4 5)))", "3");
}

// ============================================================================
// Run All Tests
// ============================================================================

static int run_all_tests(void) {
    t_pass = t_fail = 0;
    pf("=== moss test suite ===\n");

    // Base layer
    test_arena();
    test_str();
    test_intern();
    test_nanbox();
    test_dynarray();
    test_hashmap();

    // Platform layer
    test_dispatch();

    // Lang layer
    test_grammar_bf();
    test_grammar_lisp();
    test_grammar_c();
    test_grammar_query();

    // Lang layer: eval
    test_eval_basic();
    test_collections();
    test_collections_adv();
    test_core_forms();
    test_coll_proto();

    // Emit layer
    test_jit();
    test_cc();

    pf("\n=== %d passed, %d failed ===\n", t_pass, t_fail);
    return t_fail;
}

#endif // TEST_C_INCLUDED
