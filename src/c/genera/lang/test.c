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
// Test Framework — describe / it
// ============================================================================

static int t_pass, t_fail, t_groups;
static u64 t_group_t0;

static void describe(const char *name) {
    t_groups++;
    t_group_t0 = now_ns();
    pfc(C_BOLD); pf("  %s\n", name); pfc(C_RESET);
}

static void it(const char *name, bool ok) {
    if (ok) { t_pass++; return; }
    pfc(C_RED); pf("    FAIL"); pfc(C_RESET);
    pf(": %s\n", name);
    t_fail++;
}

static void it_eq_i64(const char *name, i64 got, i64 exp) {
    if (got == exp) { t_pass++; return; }
    pfc(C_RED); pf("    FAIL %s", name); pfc(C_RESET);
    pf(": got "); pfc(C_YELLOW); pf("%lld", (long long)got); pfc(C_RESET);
    pf(", expected "); pfc(C_GREEN); pf("%lld", (long long)exp); pfc(C_RESET);
    pf("\n");
    t_fail++;
}

static void it_eq_val(const char *name, Val got, Val exp) {
    if (got == exp) { t_pass++; return; }
    pfc(C_RED); pf("    FAIL %s", name); pfc(C_RESET);
    pf("\n");
    t_fail++;
}

static void it_eq_str(const char *name, const char *got, const char *exp) {
    if (strcmp(got, exp) == 0) { t_pass++; return; }
    pfc(C_RED); pf("    FAIL %s", name); pfc(C_RESET);
    pf(": got "); pfc(C_YELLOW); pf("'%s'", got); pfc(C_RESET);
    pf(", expected "); pfc(C_GREEN); pf("'%s'", exp); pfc(C_RESET);
    pf("\n");
    t_fail++;
}

// ============================================================================
// 1. Arena Tests (7 tests)
// ============================================================================

static void test_arena(void) {
    describe("arena");
    Arena a = arena_create(4096);

    void *p1 = arena_alloc(&a, 3, 8);
    void *p2 = arena_alloc(&a, 5, 8);
    it("align-8", ((u64)p1 & 7) == 0 && ((u64)p2 & 7) == 0);

    void *p3 = arena_alloc(&a, 1, 64);
    it("align-64", ((u64)p3 & 63) == 0);

    arena_reset(&a);
    void *p4 = arena_alloc(&a, 16, 8);
    it("reset-reuse", a.current->used == 16 && p4 != NULL);

    arena_reset(&a);
    void *big = arena_alloc(&a, 8192, 8);
    it("block-chain", big != NULL && a.current->prev != NULL);

    arena_reset(&a);
    arena_alloc(&a, 100, 8);
    ArenaMark mark = arena_begin_temp(&a);
    arena_alloc(&a, 200, 8);
    arena_end_temp(mark);
    it("temp-mark", a.current->used == mark.used);

    arena_reset(&a);
    u64 *v = arena_push(&a, u64);
    *v = 0xDEADBEEF;
    it("push-typed", *v == 0xDEADBEEF);

    arena_reset(&a);
    u32 *arr = arena_push_n(&a, u32, 100);
    for (int i = 0; i < 100; i++) arr[i] = i;
    bool ok = true;
    for (int i = 0; i < 100; i++) if (arr[i] != (u32)i) ok = false;
    it("push-n", ok);

    arena_destroy(&a);
}

// ============================================================================
// 2. Str Tests (8 tests)
// ============================================================================

static void test_str(void) {
    describe("str");

    Str a = STR_LIT("hello"), b = STR_LIT("hello"), c = STR_LIT("world");
    it("str-eq-same", str_eq(a, b));
    it("str-eq-diff", !str_eq(a, c));

    Str d = STR_LIT("hell");
    it("str-eq-len", !str_eq(a, d));

    Str s = STR_LIT("hello world");
    Str sl = str_slice(s, 6, 5);
    it("str-slice", str_eq(sl, STR_LIT("world")));

    Str sl2 = str_slice(s, 20, 5);
    it("str-slice-past", sl2.len == 0);

    u32 h1 = str_hash(a), h2 = str_hash(a);
    it("str-hash-consistent", h1 == h2 && h1 != 0);

    u32 h3 = str_hash(c);
    it("str-hash-diff", h1 != h3);

    Arena tmp = arena_create(1024);
    Str dup = str_dup(&tmp, a);
    it("str-dup", str_eq(dup, a) && dup.data != a.data);
    arena_destroy(&tmp);
}

// ============================================================================
// 3. Intern Tests (5 tests)
// ============================================================================

static void test_intern(void) {
    describe("intern");

    StrId id1 = str_intern(STR_LIT("test_foo"));
    StrId id2 = str_intern(STR_LIT("test_foo"));
    it("intern-same", id1 == id2);

    StrId id3 = str_intern(STR_LIT("test_bar"));
    it("intern-diff", id1 != id3);

    Str recovered = str_from_id(id1);
    it("intern-roundtrip", str_eq(recovered, STR_LIT("test_foo")));

    it("strid-eq", strid_eq(id1, id2) && !strid_eq(id1, id3));

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
    it("intern-100-unique", all_unique);
}

// ============================================================================
// 4. NaN Boxing Tests (14 tests)
// ============================================================================

static void test_nanbox(void) {
    describe("nanbox");

    Val nil = val_nil();
    it("nil", val_is_nil(nil) && !val_is_int(nil) && !val_is_f64(nil));

    Val vt = val_true(), vf = val_false();
    it("bool", val_is_bool(vt) && val_as_bool(vt) && val_is_bool(vf) && !val_as_bool(vf));

    it("int-pos", val_is_int(val_int(42)) && val_as_int(val_int(42)) == 42);
    it("int-neg", val_is_int(val_int(-7)) && val_as_int(val_int(-7)) == -7);
    it("int-zero", val_is_int(val_int(0)) && val_as_int(val_int(0)) == 0);
    it("int-large", val_as_int(val_int(140737488355327LL)) == 140737488355327LL);
    it("int-neg-large", val_as_int(val_int(-140737488355328LL)) == -140737488355328LL);

    it("f64-pi", val_is_f64(val_f64(3.14159)) && val_as_f64(val_f64(3.14159)) == 3.14159);
    it("f64-zero", val_is_f64(val_f64(0.0)) && val_as_f64(val_f64(0.0)) == 0.0);
    it("f64-neg", val_is_f64(val_f64(-2.5)) && val_as_f64(val_f64(-2.5)) == -2.5);

    StrId sym = str_intern(STR_LIT("test_sym"));
    it("sym", val_is_sym(val_sym(sym)) && val_as_sym(val_sym(sym)) == sym);

    StrId kw = str_intern(STR_LIT("test_kw"));
    it("kw", val_is_kw(val_kw(kw)) && val_as_kw(val_kw(kw)) == kw);

    // Mutual exclusivity
    Val vals[] = {val_nil(), val_true(), val_int(1), val_f64(1.0), val_sym(0), val_kw(0)};
    bool (*checks[])(Val) = {val_is_nil, val_is_bool, val_is_int, val_is_f64, val_is_sym, val_is_kw};
    bool exclusive = true;
    for (int i = 0; i < 6; i++)
        for (int j = 0; j < 6; j++)
            if (i == j ? !checks[j](vals[i]) : checks[j](vals[i])) exclusive = false;
    it("exclusive", exclusive);

    it("truthiness",
        val_truthy(val_true()) && !val_truthy(val_false()) &&
        !val_truthy(val_nil()) && val_truthy(val_int(0)) && val_truthy(val_int(42)));
}

// ============================================================================
// 5. DynArray Tests (6 tests)
// ============================================================================

static void test_dynarray(void) {
    describe("dynarray");
    Arena a = arena_create(4096);

    u32 *arr = NULL;
    arr_push(arr, 10, &a);
    arr_push(arr, 20, &a);
    arr_push(arr, 30, &a);
    it("push-count", arr_count(arr) == 3 && arr[0] == 10 && arr[1] == 20 && arr[2] == 30);

    u32 v = arr_pop(arr);
    it("pop", v == 30 && arr_count(arr) == 2);

    arr_clear(arr);
    for (int i = 0; i < 100; i++) arr_push(arr, (u32)i, &a);
    bool ok = arr_count(arr) == 100;
    for (int i = 0; i < 100; i++) if (arr[i] != (u32)i) ok = false;
    it("grow-100", ok);

    it("last", arr_last(arr) == 99);

    arr_clear(arr);
    it("clear", arr_count(arr) == 0);

    u32 *null_arr = NULL;
    it("null-count", arr_count(null_arr) == 0);

    arena_destroy(&a);
}

// ============================================================================
// 6. HashMap Tests (7 tests)
// ============================================================================

static void test_hashmap(void) {
    describe("hashmap");
    Arena a = arena_create(1 << 20);
    HashMap m = hashmap_create(&a, 16);
    Val v;

    hashmap_put(&m, 42, val_int(100));
    it("put-get", hashmap_get(&m, 42, &v) && val_as_int(v) == 100);

    it("get-missing", !hashmap_get(&m, 99, &v));

    hashmap_put(&m, 42, val_int(200));
    hashmap_get(&m, 42, &v);
    it("update", val_as_int(v) == 200 && m.count == 1);

    hashmap_put(&m, 50, val_int(500));
    it("delete", hashmap_del(&m, 50) && !hashmap_get(&m, 50, &v) && m.count == 1);

    it("del-missing", !hashmap_del(&m, 999));

    HashMap m2 = hashmap_create(&a, 16);
    for (u32 i = 0; i < 100; i++) hashmap_put(&m2, i + 1000, val_int(i));
    bool ok = true;
    for (u32 i = 0; i < 100; i++)
        if (!hashmap_get(&m2, i + 1000, &v) || val_as_int(v) != (i64)i) ok = false;
    it("collision-100", ok && m2.count == 100);

    HashMap m3 = hashmap_create(&a, 16);
    for (u32 i = 0; i < 200; i++) hashmap_put(&m3, i, val_int(i));
    it("grow", m3.count == 200 && m3.cap >= 256);

    arena_destroy(&a);
}

// ============================================================================
// 7. Protocol Dispatch Tests (16 tests)
// ============================================================================

static void test_dispatch(void) {
    describe("dispatch");

    // Tag index
    it_eq_i64("idx-nil",  val_tag_idx(val_nil()),  TI_NIL_T);
    it_eq_i64("idx-bool", val_tag_idx(val_true()), TI_BOOL);
    it_eq_i64("idx-int",  val_tag_idx(val_int(42)),TI_INT);
    it_eq_i64("idx-sym",  val_tag_idx(val_sym(0)), TI_SYM);
    it_eq_i64("idx-kw",   val_tag_idx(val_kw(0)),  TI_KW);
    it_eq_i64("idx-f64",  val_tag_idx(val_f64(3.14)), TI_F64);

    Val c = cons_new(val_int(1), val_nil());
    it_eq_i64("idx-cons", val_tag_idx(c), TI_CONS);

    // Tier 1: table dispatch
    Val list = cons_new(val_int(10), cons_new(val_int(20), val_nil()));
    it_eq_val("t1-first", t_first(list), val_int(10));
    it_eq_val("t1-nil",   t_first(val_nil()), val_nil());
    it_eq_val("t1-rest",  t_first(t_rest(list)), val_int(20));

    // Tier 2: fast path
    it_eq_val("t2-first", p_first(list), val_int(10));
    it_eq_val("t2-nil",   p_first(val_nil()), val_nil());
    it_eq_val("t2-rest",  p_first(p_rest(list)), val_int(20));
    it_eq_val("t2-rnil",  p_rest(val_nil()), val_nil());

    // Tier 3: fused
    Val f, r;
    it("t3-cons", p_first_rest(list, &f, &r) == true);
    it_eq_val("t3-car", f, val_int(10));
}

// ============================================================================
// 8. Grammar Tests — BF (4 tests)
// ============================================================================

static void test_grammar_bf(void) {
    describe("grammar: bf");
    Lang l; lang_bf(&l);
    const char *src = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    it("bf-parsed", g.n > 1);
    it("bf-root", gn_kind(&g, 0) == NK_ROOT);

    u32 ops = bm_pop(g.m[NK_OP], g.mw);
    u32 vecs = bm_pop(g.m[NK_VEC], g.mw);
    it("bf-ops", ops > 10);
    it("bf-loops", vecs >= 3);
}

// ============================================================================
// 9. Grammar Tests — Lisp (10 tests)
// ============================================================================

static void test_grammar_lisp(void) {
    describe("grammar: lisp");
    Lang l; lang_lisp(&l);
    const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    it("lisp-parsed", g.n > 1);

    u32 defn = gn_child(&g, 0);
    it("lisp-defn-list", gn_kind(&g, defn) == NK_LIST);

    u32 head = gn_child(&g, defn);
    it("lisp-head-ident", gn_kind(&g, head) == NK_IDENT);
    it("lisp-head=defn", str_eq(gn_text(&g, head), STR_LIT("defn")));

    u32 fname = gn_next(&g, head);
    it("lisp-name=fib", str_eq(gn_text(&g, fname), STR_LIT("fib")));

    u32 params = gn_next(&g, fname);
    it("lisp-params-vec", gn_kind(&g, params) == NK_VEC);
    it("lisp-param-n", str_eq(gn_text(&g, gn_child(&g, params)), STR_LIT("n")));

    // Negative number
    const char *s2 = "(+ -7 3)";
    Gram g2 = gram_new(64);
    gram_parse(&g2, &l, s2, strlen(s2));
    u32 neg = gn_nth(&g2, gn_child(&g2, 0), 1);
    it("lisp-negnum", gn_kind(&g2, neg) == NK_NUM);
    it("lisp-negnum-text", str_eq(gn_text(&g2, neg), STR_LIT("-7")));
}

// ============================================================================
// 10. Grammar Tests — C (4 tests)
// ============================================================================

static void test_grammar_c(void) {
    describe("grammar: c");
    Lang l; lang_c(&l);
    const char *src = "int fib(int n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    it("c-parsed", g.n > 5);
    u32 idents = bm_pop(g.m[NK_IDENT], g.mw);
    u32 nums = bm_pop(g.m[NK_NUM], g.mw);
    u32 groups = bm_pop(g.m_group, g.mw);
    it("c-idents", idents > 0);
    it("c-nums", nums > 0);
    it("c-groups", groups > 0);
}

// ============================================================================
// 11. Grammar Query Tests (6 tests)
// ============================================================================

static void test_grammar_query(void) {
    describe("grammar: query");
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

    it("query-3defns", n_defns == 3);
    it("query-1def", n_defs == 1);
    it("query-1main", n_mains == 1);

    it("query-fib-no-recur", !gn_has(&g, m_recur, defn_nodes[0]));
    it("query-add-no-recur", !gn_has(&g, m_recur, defn_nodes[1]));
    it("query-fact-recur", gn_has(&g, m_recur, defn_nodes[2]));
}

// ============================================================================
// 11b. View Infrastructure Tests
// ============================================================================

static void test_views(void) {
    describe("views");
    Lang l; lang_lisp(&l);
    const char *src = "(defn add [a b] (+ a b)) (def x 42) (add x 1)";

    Gram g = gram_new(4096);
    gram_parse(&g, &l, src, strlen(src));
    gram_analyze(&g);

    // Views allocated
    it("views-allocated", g.analyzed);
    for (u32 i = 0; i < V_COUNT; i++)
        it(V_NAME[i], g.v[i] != 0);

    // All passes run; V_DEAD empty for this source (no dead code)
    it("pass1-filled", bm_pop(g.v[V_DEF], g.mw) > 0);
    it("dead-empty", bm_pop(g.v[V_DEAD], g.mw) == 0);

    // Manual set + query round-trip (use V_DEAD — empty for this source)
    BM_SET(g.v[V_DEAD], 1);
    BM_SET(g.v[V_DEAD], 3);
    it("view_is-set", view_is(&g, V_DEAD, 1));
    it("view_is-unset", !view_is(&g, V_DEAD, 2));

    // Subtree queries via view_has/view_count
    it("view_has-root", view_has(&g, V_DEAD, 0));
    it_eq_i64("view_count-root", view_count(&g, V_DEAD, 0), 2);

    // Compose views: AND two views
    BM_SET(g.v[V_FN], 1);  // node 1 in both DEAD and FN (synthetic)
    u64 both[64] = {0};
    bm_and(both, g.v[V_DEAD], g.v[V_FN], g.mw);
    it("view-and-compose", BM_GET(both, 1));
    it("view-and-excludes", !BM_GET(both, 3));
}

// ============================================================================
// 11c. Pass 1: Scope + Binding Tests
// ============================================================================

static void test_pass_scope(void) {
    describe("pass 1: scope");
    Lang l; lang_lisp(&l);

    // defn: name + params → V_DEF, body refs → V_REF, call → V_CALL
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(defn add [a b] (+ a b))", 24);
        gram_analyze(&g);
        it_eq_i64("defn-defs", bm_pop(g.v[V_DEF], g.mw), 3);   // add, a, b
        it_eq_i64("defn-refs", bm_pop(g.v[V_REF], g.mw), 2);   // a, b in body
        it_eq_i64("defn-calls", bm_pop(g.v[V_CALL], g.mw), 1); // (+ a b)
    }

    // def: name → V_DEF
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(def x 42)", 10);
        gram_analyze(&g);
        it_eq_i64("def-defs", bm_pop(g.v[V_DEF], g.mw), 1);
    }

    // let: bindings → V_DEF, body refs → V_REF
    {
        Gram g = gram_new(256);
        const char *src = "(let [x 1 y 2] (+ x y))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("let-defs", bm_pop(g.v[V_DEF], g.mw), 2);   // x, y
        it_eq_i64("let-refs", bm_pop(g.v[V_REF], g.mw), 2);   // x, y in body
        it_eq_i64("let-calls", bm_pop(g.v[V_CALL], g.mw), 1); // (+ x y)
    }

    // fn: params → V_DEF
    {
        Gram g = gram_new(256);
        const char *src = "(fn [x] (+ x 1))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("fn-defs", bm_pop(g.v[V_DEF], g.mw), 1);    // x
        it_eq_i64("fn-refs", bm_pop(g.v[V_REF], g.mw), 1);    // x in body
    }

    // binding resolution: all refs bound
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (let [y 1] (+ x y)))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 bound = 0, unbound = 0;
        for (u32 i = 0; i < g.n; i++) {
            if (BM_GET(g.v[V_REF], i)) {
                if (g.bind[i]) bound++; else unbound++;
            }
        }
        it_eq_i64("nested-bound", bound, 2);      // x, y
        it_eq_i64("nested-unbound", unbound, 0);
    }

    // closure: inner fn sees outer params
    {
        Gram g = gram_new(256);
        const char *src = "(fn [a] (fn [b] (+ a b)))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 bound = 0;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_REF], i) && g.bind[i]) bound++;
        it_eq_i64("closure-bound", bound, 2);      // a and b both resolved
    }

    // special forms not marked V_CALL
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(if true 1 2)", 13);
        gram_analyze(&g);
        it_eq_i64("if-no-call", bm_pop(g.v[V_CALL], g.mw), 0);
    }

    // recursive defn: name visible in own body
    {
        Gram g = gram_new(512);
        const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("recur-defs", bm_pop(g.v[V_DEF], g.mw), 2); // fib, n
        it_eq_i64("recur-refs", bm_pop(g.v[V_REF], g.mw), 4); // n × 4
        it_eq_i64("recur-calls", bm_pop(g.v[V_CALL], g.mw), 6);
    }

    // scope[] points to enclosing fn
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (+ x 1))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 defn_node = g.nodes[0].child;
        bool ok = true;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_REF], i) && g.scope[i] != defn_node) ok = false;
        it("scope-to-fn", ok);
    }

    // sequential defs: later sees earlier
    {
        Gram g = gram_new(256);
        const char *src = "(def x 1) (def y x)";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 bound = 0;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_REF], i) && g.bind[i]) bound++;
        it_eq_i64("seq-def-bound", bound, 1);   // y's value x resolves to def x
    }

    // loop: bindings visible in body
    {
        Gram g = gram_new(256);
        const char *src = "(loop [i 0] (recur (+ i 1)))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("loop-defs", bm_pop(g.v[V_DEF], g.mw), 1);  // i
        u32 bound = 0;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_REF], i) && g.bind[i]) bound++;
        it_eq_i64("loop-bound", bound, 1);  // i in body
    }
}

// ============================================================================
// 11d. Pass 2: Type + Purity Tests
// ============================================================================

static void test_pass_type(void) {
    describe("pass 2: type");
    Lang l; lang_lisp(&l);

    // Numbers → V_INT + V_CONST
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(+ 1 2)", 7);
        gram_analyze(&g);
        u32 ints = bm_pop(g.v[V_INT], g.mw);
        it("num-int", ints >= 2);  // 1 and 2
        u32 consts = bm_pop(g.v[V_CONST], g.mw);
        it("num-const", consts >= 2);
    }

    // String → V_CONST
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(println \"hello\")", 17);
        gram_analyze(&g);
        bool found = false;
        for (u32 i = 0; i < g.n; i++)
            if (g.nodes[i].kind == NK_STR_NODE && BM_GET(g.v[V_CONST], i)) found = true;
        it("str-const", found);
    }

    // Vec literal → V_VEC
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "[1 2 3]", 7);
        gram_analyze(&g);
        it_eq_i64("vec-lit", bm_pop(g.v[V_VEC], g.mw), 1);
    }

    // Map literal → V_MAP
    {
        Gram g = gram_new(256);
        const char *src = "{:a 1}";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("map-lit", bm_pop(g.v[V_MAP], g.mw), 1);
    }

    // fn form → V_FN
    {
        Gram g = gram_new(256);
        const char *src = "(fn [x] (+ x 1))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("fn-form", bm_pop(g.v[V_FN], g.mw), 1);
    }

    // Pure builtin call → V_PURE
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(+ 1 2)", 7);
        gram_analyze(&g);
        it_eq_i64("pure-call", bm_pop(g.v[V_PURE], g.mw), 1);
    }

    // println is NOT pure
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(println 42)", 12);
        gram_analyze(&g);
        it_eq_i64("println-impure", bm_pop(g.v[V_PURE], g.mw), 0);
    }

    // Pure call with const args → V_CONST
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(+ 1 2)", 7);
        gram_analyze(&g);
        bool call_const = false;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_CONST], i)) call_const = true;
        it("const-fold", call_const);
    }

    // Int-returning call with int args → V_INT
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(+ 1 2)", 7);
        gram_analyze(&g);
        bool call_int = false;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_INT], i)) call_int = true;
        it("int-call", call_int);
    }

    // Nested const propagation: (+ (+ 1 2) 3) → outer is also const + int
    {
        Gram g = gram_new(256);
        const char *src = "(+ (+ 1 2) 3)";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        // Root list is the outer call
        u32 outer = g.nodes[0].child;
        it("nested-const", BM_GET(g.v[V_CONST], outer));
        it("nested-int", BM_GET(g.v[V_INT], outer));
    }

    // Non-const: variable arg prevents const propagation
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (+ x 1))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        // (+ x 1) — x is a ref, not const
        bool any_const_call = false;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_CONST], i)) any_const_call = true;
        it("non-const-ref", !any_const_call);
    }

    // nil/true/false → V_CONST
    {
        Gram g = gram_new(256);
        const char *src = "(if true nil false)";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 consts = bm_pop(g.v[V_CONST], g.mw);
        it("literal-const", consts >= 3);  // true, nil, false
    }
}

// ============================================================================
// 11e. Pass 3: Flow + Dead Code Tests
// ============================================================================

static void test_pass_flow(void) {
    describe("pass 3: flow");
    Lang l; lang_lisp(&l);

    // defn: last body expr is V_TAIL
    {
        Gram g = gram_new(256);
        gram_parse(&g, &l, "(defn f [x] (+ x 1))", 20);
        gram_analyze(&g);
        it("defn-tail", bm_pop(g.v[V_TAIL], g.mw) > 0);
        // (+ x 1) should be tail
        bool call_tail = false;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_TAIL], i)) call_tail = true;
        it("call-is-tail", call_tail);
    }

    // fn: last body expr is V_TAIL
    {
        Gram g = gram_new(256);
        const char *src = "(fn [x] (+ x 1))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it("fn-tail", bm_pop(g.v[V_TAIL], g.mw) > 0);
    }

    // if in tail: then and else are V_TAIL
    {
        Gram g = gram_new(512);
        const char *src = "(defn f [x] (if (< x 0) 1 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 tails = bm_pop(g.v[V_TAIL], g.mw);
        it("if-tail-branches", tails >= 3);  // if + then + else
    }

    // do in tail: only last is V_TAIL
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (do 1 2 3))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        // The number 3 (last) is tail; 1 and 2 are not
        bool found_non_tail_num = false, found_tail_num = false;
        for (u32 i = 0; i < g.n; i++) {
            if (g.nodes[i].kind == NK_NUM) {
                if (BM_GET(g.v[V_TAIL], i)) found_tail_num = true;
                else found_non_tail_num = true;
            }
        }
        it("do-last-tail", found_tail_num);
        it("do-non-last-not", found_non_tail_num);
    }

    // let in tail: last body is V_TAIL
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (let [y 1] (+ x y)))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        bool call_tail = false;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_TAIL], i)) call_tail = true;
        it("let-body-tail", call_tail);
    }

    // loop: last body is V_TAIL
    {
        Gram g = gram_new(256);
        const char *src = "(loop [i 0] (if (< i 10) (recur (inc i)) i))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it("loop-tail", bm_pop(g.v[V_TAIL], g.mw) > 0);
    }

    // Dead code: (if true then else) → else is V_DEAD
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [] (if true 1 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("if-true-dead", bm_pop(g.v[V_DEAD], g.mw), 1);
    }

    // Dead code: (if false then else) → then is V_DEAD
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [] (if false 1 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("if-false-dead", bm_pop(g.v[V_DEAD], g.mw), 1);
    }

    // Dead code: (if nil then else) → then is V_DEAD
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [] (if nil 1 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("if-nil-dead", bm_pop(g.v[V_DEAD], g.mw), 1);
    }

    // No dead code in normal if
    {
        Gram g = gram_new(256);
        const char *src = "(defn f [x] (if (< x 0) 1 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        it_eq_i64("if-normal-no-dead", bm_pop(g.v[V_DEAD], g.mw), 0);
    }

    // cond: each expr in tail
    {
        Gram g = gram_new(512);
        const char *src = "(defn f [x] (cond (< x 0) 1 (> x 0) 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        // Both 1 and 2 should be tail
        u32 tail_nums = 0;
        for (u32 i = 0; i < g.n; i++)
            if (g.nodes[i].kind == NK_NUM && BM_GET(g.v[V_TAIL], i)) tail_nums++;
        it_eq_i64("cond-tails", tail_nums, 2);
    }

    // Nested fn: each has independent tail
    {
        Gram g = gram_new(512);
        const char *src = "(defn f [x] (+ x 1)) (fn [y] (* y 2))";
        gram_parse(&g, &l, src, strlen(src));
        gram_analyze(&g);
        u32 tail_calls = 0;
        for (u32 i = 0; i < g.n; i++)
            if (BM_GET(g.v[V_CALL], i) && BM_GET(g.v[V_TAIL], i)) tail_calls++;
        it_eq_i64("multi-fn-tails", tail_calls, 2);
    }
}

// ============================================================================
// 12. Data-Driven Tests — tests ARE data, grammar+eval IS the runner
// ============================================================================

typedef struct { const char *name; const char *src; const char *exp; } EvalCase;
typedef struct { const char *name; const char *src; i64 exp; } JitCase;

static void run_eval_suite(const char *group, const EvalCase *t, u32 n) {
    describe(group);
    for (u32 i = 0; i < n; i++) {
        arena_reset(&g_req);
        g_sig = SIG_NONE; g_depth = 0;
        Val result = eval_string(t[i].src, g_global_env);
        if (g_sig) { g_sig = SIG_NONE; print_flush(); }
        it_eq_str(t[i].name, print_val(result), t[i].exp);
    }
}

static void run_jit_suite(const JitCase *t, u32 n) {
    describe("x86 jit");
    for (u32 i = 0; i < n; i++) {
        it_eq_i64(t[i].name, jit_run(t[i].src), t[i].exp);
    }
}

static void run_cc_suite(const EvalCase *t, u32 n) {
    describe("clj -> c -> native");
    for (u32 i = 0; i < n; i++) {
        int rc = compile_and_capture(t[i].src);
        if (rc != 0 && strcmp(g_captured, "COMPILE_ERROR") == 0)
            it(t[i].name, false);
        else
            it_eq_str(t[i].name, g_captured, t[i].exp);
    }
}

#define SUITE(arr) (arr), (sizeof(arr)/sizeof(arr[0]))

// --- Eval test data ---

static const EvalCase T_EVAL[] = {
    {"int", "42", "42"},
    {"neg", "-7", "-7"},
    {"bool", "true", "true"},
    {"nil", "nil", "nil"},
    {"add", "(+ 1 2 3)", "6"},
    {"if-t", "(if true 1 2)", "1"},
    {"if-f", "(if false 1 2)", "2"},
    {"let", "(let [x 10 y 20] (+ x y))", "30"},
    {"fn", "((fn [x] (* x x)) 5)", "25"},
    {"defn", "(defn sq [x] (* x x)) (sq 7)", "49"},
    {"and", "(and 1 2 3)", "3"},
    {"or", "(or false nil 5)", "5"},
};

static const EvalCase T_COLL[] = {
    {"vec-lit", "[1 2 3]", "[1 2 3]"},
    {"vec-empty", "[]", "[]"},
    {"vec-nested", "[[1 2] [3 4]]", "[[1 2] [3 4]]"},
    {"map-lit", "{:a 1 :b 2}", "{:a 1 :b 2}"},
    {"map-empty", "{}", "{}"},
    {"kw", ":foo", ":foo"},
    {"get-map", "(get {:a 1 :b 2} :a)", "1"},
    {"get-vec", "(get [10 20 30] 1)", "20"},
    {"get-miss", "(get {:a 1} :z)", "nil"},
    {"assoc-map", "(get (assoc {:a 1} :b 2) :b)", "2"},
    {"assoc-vec", "(get (assoc [10 20 30] 1 99) 1)", "99"},
    {"conj-vec", "(conj [1 2] 3)", "[1 2 3]"},
    {"conj-list", "(first (conj (list 2 3) 1))", "1"},
    {"nth-vec", "(nth [10 20 30] 2)", "30"},
    {"nth-list", "(nth (list 10 20 30) 1)", "20"},
    {"count-vec", "(count [1 2 3])", "3"},
    {"count-map", "(count {:a 1 :b 2})", "2"},
    {"count-list", "(count (list 1 2 3 4))", "4"},
    {"vec-from-list", "(vec (list 1 2 3))", "[1 2 3]"},
    {"emptyq", "(empty? [])", "true"},
    {"not-emptyq", "(empty? [1])", "false"},
};

static const EvalCase T_COLL_ADV[] = {
    {"keys", "(count (keys {:a 1 :b 2 :c 3}))", "3"},
    {"vals", "(count (vals {:a 1 :b 2 :c 3}))", "3"},
    {"contains-t", "(contains? {:a 1} :a)", "true"},
    {"contains-f", "(contains? {:a 1} :b)", "false"},
    {"vecq", "(vector? [1 2])", "true"},
    {"mapq", "(map? {:a 1})", "true"},
    {"kwq", "(keyword? :foo)", "true"},
    {"not-vecq", "(vector? 42)", "false"},
    {"into-vec", "(into [] (list 1 2 3))", "[1 2 3]"},
    {"hash-map", "(get (hash-map :x 10 :y 20) :x)", "10"},
};

static const EvalCase T_FORMS[] = {
    {"cond-1", "(cond (< 5 3) \"no\" (> 5 3) \"yes\")", "\"yes\""},
    {"cond-else", "(cond false 1 :else 42)", "42"},
    {"cond-nil", "(cond false 1)", "nil"},
    {"when-t", "(when (> 5 3) 42)", "42"},
    {"when-f", "(when (< 5 3) 42)", "nil"},
    {"when-body", "(when true 1 2 3)", "3"},
    {"str-cat", "(str \"hello\" \" \" \"world\")", "\"hello world\""},
    {"str-mix", "(str \"x=\" 42)", "\"x=42\""},
    {"range-1", "(count (range 5))", "5"},
    {"range-2", "(first (range 5))", "0"},
    {"range-start", "(first (range 3 7))", "3"},
    {"range-len", "(count (range 3 7))", "4"},
    {"loop-sum", "(loop [i 0 s 0] (if (= i 10) s (recur (inc i) (+ s i))))", "45"},
    {"loop-vec", "(loop [v [] i 0] (if (= i 5) v (recur (conj v i) (inc i))))", "[0 1 2 3 4]"},
    {"loop-nested", "(loop [i 0 t 0] (if (= i 3) t (recur (inc i) (+ t (loop [j 0 s 0] (if (= j i) s (recur (inc j) (+ s 1))))))))", "3"},
    {"inc", "(inc 41)", "42"},
    {"dec", "(dec 43)", "42"},
    {"zero?-t", "(zero? 0)", "true"},
    {"pos?-t", "(pos? 5)", "true"},
    {"neg?-t", "(neg? -3)", "true"},
    {"overflow", "((fn [x] (x x)) (fn [x] (x x)))", "nil"},
    {"add-f64", "(+ 1.5 2.5)", "4.0"},
    {"mul-mix", "(* 2 3.5)", "7.0"},
    {"lt-f64", "(< 1.5 2.0)", "true"},
};

static const EvalCase T_PROTO[] = {
    {"vec-first", "(first [10 20 30])", "10"},
    {"vec-rest", "(first (rest [10 20 30]))", "20"},
    {"vec-seq-nil", "(nil? (first []))", "true"},
    {"reduce-range", "(reduce + 0 (range 10))", "45"},
    {"map-vec", "(first (map (fn [x] (* x 2)) (list 3 4 5)))", "6"},
    {"filter-list", "(count (filter (fn [x] (> x 2)) (list 1 2 3 4 5)))", "3"},
};

static const EvalCase T_TRANSIENT[] = {
    {"t-basic", "(let [m {:a 1 :b 2}] (let [t (transient m)] (let [t2 (assoc! t :c 3)] (get (persistent! t2) :c))))", "3"},
    {"t-immutable", "(let [m {:a 1 :b 2}] (let [t (transient m)] (assoc! t :c 3) (get m :c)))", "nil"},
    {"t-batch", "(let [t (transient {})] (assoc! t :a 1) (assoc! t :b 2) (assoc! t :c 3) (count (persistent! t)))", "3"},
    {"t-update", "(let [t (transient {:a 1})] (assoc! t :a 99) (get (persistent! t) :a))", "99"},
    {"t-persist-get", "(let [t (transient {})] (assoc! t :x 42) (let [p (persistent! t)] (get p :x)))", "42"},
    {"t-promote", "(let [t (transient {})] (assoc! t :a 1) (assoc! t :b 2) (assoc! t :c 3) (assoc! t :d 4) (assoc! t :e 5) (assoc! t :f 6) (assoc! t :g 7) (assoc! t :h 8) (assoc! t :i 9) (assoc! t :j 10) (assoc! t :k 11) (assoc! t :l 12) (assoc! t :m 13) (assoc! t :n 14) (assoc! t :o 15) (assoc! t :p 16) (assoc! t :q 17) (count (persistent! t)))", "17"},
    {"t-persist-assoc", "(get (assoc {:a 1} :b 2) :b)", "2"},
    {"t-empty", "(let [t (transient {})] (assoc! t :x 1) (get (persistent! t) :x))", "1"},
    {"t-nested-val", "(let [t (transient {})] (assoc! t :v [1 2 3]) (first (get (persistent! t) :v)))", "1"},
    {"t-chain", "(let [m (persistent! (let [t (transient {:a 1})] (assoc! t :b 2) t))] (+ (get m :a) (get m :b)))", "3"},
};

static const EvalCase T_HOF[] = {
    {"range-vec", "(vector? (range 5))", "true"},
    {"range-get", "(get (range 5) 2)", "2"},
    {"reduce-pvec", "(reduce + 0 (range 10))", "45"},
    {"reduce-pvec-100", "(reduce + 0 (range 100))", "4950"},
    {"map-pvec", "(first (map (fn [x] (* x 2)) [3 4 5]))", "6"},
    {"map-range", "(reduce + 0 (map (fn [x] (* x x)) (range 5)))", "30"},
    {"filter-pvec", "(count (filter (fn [x] (> x 2)) [1 2 3 4 5]))", "3"},
    {"filter-range", "(count (filter (fn [x] (> x 50)) (range 100)))", "49"},
    {"into-from-pvec", "(count (into [] (range 5)))", "5"},
    {"compose-fr", "(reduce + 0 (filter (fn [x] (= 0 (mod x 2))) (range 10)))", "20"},
};

// --- JIT test data ---

static const JitCase T_JIT[] = {
    {"int", "42", 42}, {"neg", "-7", -7}, {"zero", "0", 0},
    {"add", "(+ 1 2)", 3}, {"sub", "(- 10 3)", 7}, {"mul", "(* 6 7)", 42},
    {"div", "(/ 20 4)", 5}, {"mod", "(mod 17 5)", 2}, {"neg-unary", "(- 5)", -5},
    {"add3", "(+ 1 2 3)", 6}, {"mul3", "(* 2 3 4)", 24},
    {"nested", "(+ (* 3 4) (- 10 5))", 17},
    {"lt-t", "(< 1 2)", 1}, {"lt-f", "(< 2 1)", 0}, {"gt-t", "(> 5 3)", 1},
    {"eq-t", "(= 7 7)", 1}, {"eq-f", "(= 7 8)", 0},
    {"lte", "(<= 3 3)", 1}, {"gte", "(>= 5 3)", 1},
    {"if-t", "(if (< 1 2) 42 99)", 42}, {"if-f", "(if (> 1 2) 42 99)", 99},
    {"if-nested", "(if (< 1 2) (if (< 3 4) 10 20) 30)", 10},
    {"let", "(let [x 10] x)", 10}, {"let2", "(let [x 10 y 20] (+ x y))", 30},
    {"let-nested", "(let [x 10] (let [y 20] (+ x y)))", 30},
    {"let-shadow", "(let [x 10] (let [x 20] x))", 20},
    {"do", "(do 1 2 3)", 3},
    {"and-t", "(and 1 2 3)", 3}, {"and-f", "(and 1 0 3)", 0},
    {"or-t", "(or 0 0 5)", 5}, {"or-f", "(or 0 0 0)", 0},
    {"not-t", "(not 0)", 1}, {"not-f", "(not 5)", 0},
    {"inc", "(inc 41)", 42}, {"dec", "(dec 43)", 42},
    {"zero?-t", "(zero? 0)", 1}, {"zero?-f", "(zero? 5)", 0},
    {"pos?-t", "(pos? 5)", 1}, {"neg?-t", "(neg? -3)", 1},
    {"defn", "(defn add [a b] (+ a b)) (add 3 4)", 7},
    {"defn2", "(defn sq [x] (* x x)) (sq 6)", 36},
    {"multi-fn", "(defn double [x] (* x 2)) (defn triple [x] (* x 3)) (+ (double 5) (triple 3))", 19},
    {"fib10", "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)", 55},
    {"tco-fact", "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc)))) (fact 20 1)", 2432902008176640000LL},
    {"loop", "(loop [i 0 s 0] (if (>= i 10) s (recur (inc i) (+ s i))))", 45},
    {"def", "(def x 42) x", 42}, {"def-expr", "(def x (+ 10 20)) (+ x 5)", 35},
    {"euler1", "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) (if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s))))) (solve 1000)", 233168},
    {"cond-1", "(cond (< 5 3) 0 (> 5 3) 1)", 1},
    {"cond-else", "(cond 0 99 1 42)", 42}, {"cond-none", "(cond 0 1 0 2)", 0},
    {"when-t", "(when 1 42)", 42}, {"when-f", "(when 0 42)", 0},
};

// --- C emitter test data ---

static const EvalCase T_CC[] = {
    {"add", "(println (+ 1 2))", "3"}, {"mul", "(println (* 6 7))", "42"},
    {"sub", "(println (- 10 3))", "7"}, {"div", "(println (/ 10 3))", "3"},
    {"mod", "(println (mod 10 3))", "1"}, {"neg", "(println (- 5))", "-5"},
    {"nested", "(println (+ (* 2 3) (- 10 4)))", "12"},
    {"variadic", "(println (+ 1 2 3 4 5))", "15"},
    {"lt", "(println (< 1 2))", "1"}, {"gt", "(println (> 1 2))", "0"},
    {"eq", "(println (= 42 42))", "1"}, {"lte", "(println (<= 5 5))", "1"},
    {"not0", "(println (not 0))", "1"}, {"not42", "(println (not 42))", "0"},
    {"if-t", "(println (if 1 10 20))", "10"}, {"if-f", "(println (if 0 10 20))", "20"},
    {"let", "(println (let [a 1 b 2] (+ a b)))", "3"},
    {"nested-let", "(println (let [a 1] (let [b (+ a 1)] (* a b))))", "2"},
    {"do", "(println (do 1 2 42))", "42"},
    {"and-t", "(println (and 1 2 3))", "3"}, {"and-f", "(println (and 1 0 3))", "0"},
    {"or-t", "(println (or 0 0 42))", "42"}, {"or-f", "(println (or 7 0 42))", "7"},
    {"def", "(def x 42) (println x)", "42"},
    {"def-expr", "(def x (+ 20 22)) (println x)", "42"},
    {"defn", "(defn double [x] (* x 2)) (println (double 21))", "42"},
    {"multi-defn", "(defn double [x] (* x 2))\n(defn quad [x] (double (double x)))\n(println (quad 10))", "40"},
    {"fact-rec", "(defn fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))\n(println (fact 10))", "3628800"},
    {"fib-rec", "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(println (fib 10))", "55"},
    {"defn-let", "(defn calc [x y] (let [tmp (+ x y)] (* tmp 2)))\n(println (calc 10 11))", "42"},
    {"tco-fact", "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n(println (fact 10 1))", "3628800"},
    {"tco-sum", "(defn sum-to [n acc] (if (<= n 0) acc (recur (- n 1) (+ acc n))))\n(println (sum-to 100 0))", "5050"},
    {"loop", "(println (loop [i 0 s 0] (if (>= i 10) s (recur (+ i 1) (+ s i)))))", "45"},
    {"loop-let", "(println (loop [i 0 s 0]\n  (if (>= i 5) s\n    (let [sq (* i i)] (recur (+ i 1) (+ s sq))))))", "30"},
    {"str", "(println \"hello\")", "hello"},
    {"str-mixed", "(println \"result:\" (+ 40 2))", "result: 42"},
    {"zero?", "(println (zero? 0))", "1"}, {"pos?", "(println (pos? 42))", "1"},
    {"neg?", "(println (neg? -1))", "1"},
    {"inc", "(println (inc 41))", "42"}, {"dec", "(println (dec 43))", "42"},
    {"cond", "(println (cond (< 5 3) 0 (> 5 3) 42))", "42"},
    {"cond-else", "(println (cond 0 1 1 42))", "42"},
    {"when-t", "(println (when 1 42))", "42"}, {"when-f", "(println (when 0 42))", "0"},
};

// ============================================================================
// Run All Tests
// ============================================================================

static int run_all_tests(void) {
    t_pass = t_fail = t_groups = 0;
    pfc(C_BOLD); pf("=== moss test suite ==="); pfc(C_RESET);

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
    test_views();
    test_pass_scope();
    test_pass_type();
    test_pass_flow();

    // Lang layer: eval (data-driven)
    run_eval_suite("eval: basic",          SUITE(T_EVAL));
    run_eval_suite("collections",          SUITE(T_COLL));
    run_eval_suite("collections: advanced", SUITE(T_COLL_ADV));
    run_eval_suite("core forms",           SUITE(T_FORMS));
    run_eval_suite("coll protocol",        SUITE(T_PROTO));
    run_eval_suite("transient",            SUITE(T_TRANSIENT));
    run_eval_suite("hof+pvec",             SUITE(T_HOF));

    // Emit layer (data-driven)
    run_jit_suite(SUITE(T_JIT));
    run_cc_suite(SUITE(T_CC));

    pf("\n");
    pfc(t_fail ? C_YELLOW : C_GREEN);
    pf("  %d passed", t_pass);
    pfc(C_RESET);
    if (t_fail) { pfc(C_RED); pf(", %d failed", t_fail); pfc(C_RESET); }
    else { pfc(C_DIM); pf(", 0 failed"); pfc(C_RESET); }
    pfc(C_DIM); pf(" (%d groups)", t_groups); pfc(C_RESET);
    pf("\n");
    return t_fail;
}

#endif // TEST_C_INCLUDED
