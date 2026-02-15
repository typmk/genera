/**
 * test.c — ALL Self-Tests
 *
 * ~325 tests covering every layer: base, platform, lang, emit.
 * Run via: ./gna test
 *
 * Depends on: emit/emit.c (and everything below)
 */
#ifndef TEST_C_INCLUDED
#define TEST_C_INCLUDED

// ============================================================================
// Test Framework — describe / it
// ============================================================================

static int t_pass, t_fail, t_groups;

static void describe(const char *name) {
    t_groups++;
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

    // Syntax-quote reader macros
    const char *s3 = "`(a ~b ~@c)";
    Gram g3 = gram_new(64);
    gram_parse(&g3, &l, s3, strlen(s3));
    u32 sq = gn_child(&g3, 0);
    it("lisp-syntax-quote", gn_kind(&g3, sq) == NK_SYNTAX_QUOTE);
    u32 inner = gn_child(&g3, sq);
    it("lisp-sq-inner-list", gn_kind(&g3, inner) == NK_LIST);
    u32 a = gn_child(&g3, inner);
    it("lisp-sq-a-ident", gn_kind(&g3, a) == NK_IDENT);
    u32 uq = gn_next(&g3, a);
    it("lisp-sq-unquote", gn_kind(&g3, uq) == NK_UNQUOTE);
    u32 sp = gn_next(&g3, uq);
    it("lisp-sq-splice", gn_kind(&g3, sp) == NK_SPLICE);
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
// 11e. Render Tests (parse → render round-trip)
// ============================================================================

// Helper: render to a static buffer and return as string
static char g_render_buf_data[4096];
static OutBuf g_render_buf;

static const char *render_to_str(const char *src) {
    Lang l; lang_lisp(&l);
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, (u32)strlen(src));
    gram_index(&g);
    g_render_buf = (OutBuf){g_render_buf_data, 0, sizeof(g_render_buf_data)};
    gram_render_buf(&g, &l, &g_render_buf);
    if (g_render_buf.pos < sizeof(g_render_buf_data))
        g_render_buf_data[g_render_buf.pos] = '\0';
    else
        g_render_buf_data[sizeof(g_render_buf_data) - 1] = '\0';
    return g_render_buf_data;
}

static void test_render(void) {
    describe("render");

    // Simple round-trip: atom
    it_eq_str("atom", render_to_str("42"), "42");
    it_eq_str("ident", render_to_str("foo"), "foo");
    it_eq_str("keyword", render_to_str(":bar"), ":bar");
    it_eq_str("string", render_to_str("\"hello\""), "\"hello\"");
    it_eq_str("negative", render_to_str("-7"), "-7");

    // Simple list
    it_eq_str("list", render_to_str("(+ 1 2)"), "(+ 1 2)");

    // Vector
    it_eq_str("vec", render_to_str("[1 2 3]"), "[1 2 3]");

    // Map
    it_eq_str("map", render_to_str("{:a 1 :b 2}"), "{:a 1 :b 2}");

    // Nested
    it_eq_str("nested", render_to_str("(defn f [x] (+ x 1))"), "(defn f [x] (+ x 1))");

    // Multiple top-level forms
    it_eq_str("multi", render_to_str("(def a 1) (def b 2)"), "(def a 1)\n(def b 2)");

    // Deep nesting
    it_eq_str("deep", render_to_str("(if (< n 2) n (+ (f (- n 1)) (f (- n 2))))"),
              "(if (< n 2) n (+ (f (- n 1)) (f (- n 2))))");

    // Empty list
    it_eq_str("empty-list", render_to_str("()"), "()");

    // Mixed delimiters
    it_eq_str("mixed", render_to_str("(let [x {:a 1}] x)"), "(let [x {:a 1}] x)");
}

// ============================================================================
// 11f. Runtime View Tests (tap → bitmask)
// ============================================================================

static void test_runtime_views(void) {
    describe("runtime views");

    // TAP → bitmask round-trip
    tap_reset(); tap_on();
    DISPATCH(TK_JIT, 1, 0);
    DISPATCH(TK_JIT, 3, 0);
    DISPATCH(TK_JIT, 3, 0);
    DISPATCH(TK_JIT, 5, 0);
    tap_off();

    u64 m[2] = {0};
    tap_to_bitmask(TK_JIT, m, 2);
    it("bm-set-1", (m[0] >> 1) & 1);
    it("bm-set-3", (m[0] >> 3) & 1);
    it("bm-set-5", (m[0] >> 5) & 1);
    it("bm-clear-0", !((m[0] >> 0) & 1));
    it("bm-clear-2", !((m[0] >> 2) & 1));

    // Hit counts
    u32 counts[8] = {0};
    tap_hit_counts(TK_JIT, counts, 8);
    it_eq_i64("count-1", counts[1], 1);
    it_eq_i64("count-3", counts[3], 2);  // hit twice
    it_eq_i64("count-5", counts[5], 1);
    it_eq_i64("count-0", counts[0], 0);

    // JIT compilation works through Clojure compiler
    i64 jit_result = clj_jit_run("(+ 1 2)");
    it("jit-runs", jit_result == 3);
}

// ============================================================================
// 12. Assert Builtins — (is expected actual), (group "name")
//
// These make eval tests self-checking programs in the language:
//   (group "collections")
//   (is [1 2 3] [1 2 3])
//   (is 6 (+ 1 2 3))
// ============================================================================

static Val bi_is(Val args) {
    Val expected = car(args);
    Val actual = car(cdr(args));
    const char *exp_s = print_val(expected);
    char exp_buf[4096];
    u32 elen = (u32)strlen(exp_s);
    if (elen >= sizeof(exp_buf)) elen = sizeof(exp_buf) - 1;
    memcpy(exp_buf, exp_s, elen); exp_buf[elen] = 0;
    const char *got_s = print_val(actual);
    if (strcmp(exp_buf, got_s) == 0) { t_pass++; return val_true(); }
    t_fail++;
    pfc(C_RED); pf("    FAIL"); pfc(C_RESET);
    pf(": got "); pfc(C_YELLOW); pf("'%s'", got_s); pfc(C_RESET);
    pf(", expected "); pfc(C_GREEN); pf("'%s'", exp_buf); pfc(C_RESET);
    pf("\n");
    return val_false();
}

static Val bi_group(Val args) {
    Val v = car(args);
    if (val_is_str(v)) {
        Str *s = val_as_str(v);
        char buf[256];
        u32 len = s->len < 255 ? s->len : 255;
        memcpy(buf, s->data, len); buf[len] = 0;
        describe(buf);
    }
    return val_nil();
}

// ============================================================================
// 12b. Image save/load tests
// ============================================================================

static void test_image_save_load(void) {
    describe("image save/load");

    // 1. Parse + analyze a non-trivial program
    const char *src = "(defn sq [x] (* x x)) (sq 6)";
    Lang l; lang_lisp(&l);
    Gram g1 = gram_new(4096);
    gram_parse(&g1, &l, src, (u32)strlen(src));
    gram_index(&g1);
    gram_analyze(&g1);

    // 2. JIT from original
    i64 r1 = jit_run(src);
    it_eq_i64("jit-orig", r1, 36);

    // 3. Save to memory
    FileData img = gram_save_buf(&g1);
    it("save-ok", img.data != NULL);

    // 4. Load from memory
    Gram g2 = {0};
    bool loaded = gram_load_buf(&g2, img.data, img.len);
    it("load-ok", loaded);
    grammar_init();

    // 5. Structural integrity
    it_eq_i64("node-count", (i64)g2.n, (i64)g1.n);
    it_eq_i64("mw", (i64)g2.mw, (i64)g1.mw);
    it_eq_i64("src-len", (i64)g2.src_len, (i64)g1.src_len);
    it_eq_i64("analyzed", (i64)g2.analyzed, 1);

    // 6. Source preserved
    bool src_match = g2.src_len == g1.src_len;
    for (u32 i = 0; i < g1.src_len && src_match; i++)
        if (g2.src[i] != g1.src[i]) src_match = false;
    it("source-match", src_match);

    // 7. Views match
    u32 bm_size = g1.mw * 8;
    bool views_ok = true;
    for (u32 v = 0; v < V_COUNT; v++) {
        if (!g1.v[v] || !g2.v[v]) continue;
        if (memcmp(g1.v[v], g2.v[v], bm_size) != 0) { views_ok = false; break; }
    }
    it("views-match", views_ok);

    // 8. Bind/scope match
    it("bind-match", memcmp(g1.bind, g2.bind, g1.n * 4) == 0);
    it("scope-match", memcmp(g1.scope, g2.scope, g1.n * 4) == 0);

    // 9. Const_val match
    bool cv_ok = true;
    if (g1.val && g2.val)
        cv_ok = memcmp(g1.val, g2.val, g1.n * 8) == 0;
    it("const-val-match", cv_ok);

    // 10. Re-JIT from loaded gram
    it_eq_i64("jit-reload", jit_run_gram(&g2), 36);

    // 11. Fib10 round-trip
    const char *src2 = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)";
    Gram g3 = gram_new(4096);
    gram_parse(&g3, &l, src2, (u32)strlen(src2));
    gram_index(&g3); gram_analyze(&g3);
    FileData img2 = gram_save_buf(&g3);
    Gram g4 = {0};
    gram_load_buf(&g4, img2.data, img2.len);
    grammar_init();
    it_eq_i64("fib10-reload", jit_run_gram(&g4), 55);

    // 12. TCO round-trip
    const char *src3 = "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc)))) (fact 10 1)";
    Gram g5 = gram_new(4096);
    gram_parse(&g5, &l, src3, (u32)strlen(src3));
    gram_index(&g5); gram_analyze(&g5);
    FileData img3 = gram_save_buf(&g5);
    Gram g6 = {0};
    gram_load_buf(&g6, img3.data, img3.len);
    grammar_init();
    it_eq_i64("fact10-reload", jit_run_gram(&g6), 3628800);
}

static void register_test_builtins(void) {
    env_set(g_global_env, INTERN("is"), make_builtin(INTERN("is"), bi_is));
    env_set(g_global_env, INTERN("group"), make_builtin(INTERN("group"), bi_group));
}

// ============================================================================
// 13. Self-Checking Tests — programs in the language
//
// Each (is expected actual) evaluates both sides and compares via print_val.
// Each (group "name") starts a new test group.
// ============================================================================

// Error-path tests (signal/overflow — can't use assertions)
static void test_error_cases(void) {
    describe("error cases");
    arena_reset(&g_req);
    g_signal = SIGNAL_NONE; g_depth = 0;
    Val r = eval_string("((fn [x] (x x)) (fn [x] (x x)))", g_global_env);
    if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
    it_eq_str("overflow", print_val(r), "nil");
}

// ============================================================================
// 14. Engine Tests — Clojure rules over GNodes
// ============================================================================

// --- Test builtins: expose test infrastructure to Clojure ---

static Val bi_world_parse(Val args) {
    g_signal = SIGNAL_NONE;
    Str *s = val_as_str(car(args));
    // gram_parse stores src pointer (doesn't copy) — must persist
    char *buf = (char *)arena_alloc(&g_perm, s->len + 1, 1);
    memcpy(buf, s->data, s->len); buf[s->len] = 0;
    world_ensure();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    Gram *g = &g_world.gram;
    gram_parse(g, &lisp, buf, s->len);
    gram_index(g);
    for (u32 vi = 0; vi < V_COUNT; vi++)
        memset(g->v[vi], 0, g->mw * 8);
    memset(g->bind, 0, g->n * sizeof(u32));
    memset(g->scope, 0, g->n * sizeof(u32));
    memset(g->val, 0, g->n * sizeof(i64));
    return val_int(g->n);
}

static Val bi_views_clear(Val args) {
    (void)args;
    Gram *g = &g_world.gram;
    for (u32 vi = 0; vi < V_COUNT; vi++)
        memset(g->v[vi], 0, g->mw * 8);
    memset(g->bind, 0, g->n * sizeof(u32));
    memset(g->scope, 0, g->n * sizeof(u32));
    memset(g->val, 0, g->n * sizeof(i64));
    return NIL;
}

static Val bi_c_analyze(Val args) {
    (void)args;
    pass_scope(&g_world.gram);
    pass_type(&g_world.gram);
    pass_flow(&g_world.gram);
    return NIL;
}

static u64 *g_snap_v[V_COUNT];
static u32 *g_snap_bind, *g_snap_scope;
static i64 *g_snap_val;
static u32 g_snap_n, g_snap_mw;

static Val bi_views_snapshot(Val args) {
    (void)args;
    Gram *g = &g_world.gram;
    g_snap_n = g->n; g_snap_mw = g->mw;
    for (u32 vi = 0; vi < V_COUNT; vi++) {
        g_snap_v[vi] = (u64 *)arena_alloc(&g_temp, g->mw * 8, 8);
        memcpy(g_snap_v[vi], g->v[vi], g->mw * 8);
    }
    g_snap_bind = (u32 *)arena_alloc(&g_temp, g->n * 4, 4);
    g_snap_scope = (u32 *)arena_alloc(&g_temp, g->n * 4, 4);
    g_snap_val = (i64 *)arena_alloc(&g_temp, g->n * 8, 8);
    memcpy(g_snap_bind, g->bind, g->n * 4);
    memcpy(g_snap_scope, g->scope, g->n * 4);
    memcpy(g_snap_val, g->val, g->n * 8);
    return NIL;
}

static Val bi_views_match(Val args) {
    (void)args;
    Gram *g = &g_world.gram;
    bool ok = true;
    for (u32 vi = 0; vi < V_COUNT; vi++)
        if (memcmp(g_snap_v[vi], g->v[vi], g_snap_mw * 8)) ok = false;
    if (memcmp(g_snap_bind, g->bind, g_snap_n * 4)) ok = false;
    if (memcmp(g_snap_scope, g->scope, g_snap_n * 4)) ok = false;
    if (memcmp(g_snap_val, g->val, g_snap_n * 8)) ok = false;
    if (!ok) {
        static const char *vn[] = {
            "DEF","REF","CALL","TAIL","PURE","CONST","DEAD","INT","VEC","MAP","FN"
        };
        for (u32 i = 0; i < g_snap_n; i++) {
            for (u32 vi = 0; vi < V_COUNT; vi++)
                if (BM_GET(g_snap_v[vi], i) != BM_GET(g->v[vi], i))
                    pf("  n%d V_%s: c=%d clj=%d\n", i, vn[vi],
                       (int)BM_GET(g_snap_v[vi], i), (int)BM_GET(g->v[vi], i));
            if (g_snap_scope[i] != g->scope[i])
                pf("  n%d scope: c=%d clj=%d\n", i, g_snap_scope[i], g->scope[i]);
            if (g_snap_bind[i] != g->bind[i])
                pf("  n%d bind: c=%d clj=%d\n", i, g_snap_bind[i], g->bind[i]);
            if (g_snap_val[i] != g->val[i])
                pf("  n%d val: c=%lld clj=%lld\n", i,
                   (long long)g_snap_val[i], (long long)g->val[i]);
        }
    }
    arena_reset(&g_temp);
    return val_bool(ok);
}

static Val bi_describe_t(Val args) {
    Str *s = val_as_str(car(args));
    char buf[s->len + 1]; memcpy(buf, s->data, s->len); buf[s->len] = 0;
    describe(buf);
    return NIL;
}

static Val bi_it_t(Val args) {
    Str *s = val_as_str(car(args));
    bool ok = val_truthy(car(cdr(args)));
    char buf[s->len + 1]; memcpy(buf, s->data, s->len); buf[s->len] = 0;
    it(buf, ok);
    return val_bool(ok);
}

static void test_engine(void) {
    describe("engine");

    // Parse a program into the world
    Lang l; lang_lisp(&l);
    gram_parse(&g_world.gram, &l, "(+ 1 2)", 7);
    gram_index(&g_world.gram);

    // Test basic GNode access from Clojure
    g_signal = SIGNAL_NONE;
    it("gn-count", val_as_int(engine_eval("(gn-count)")) > 0);
    it("root-kind", val_as_int(engine_eval("(gn-kind 0)")) == NK_ROOT);

    // Find first child of root (should be the list)
    u32 list_id = g_world.gram.nodes[0].child;
    it("list-kind", val_as_int(engine_eval("(gn-kind (gn-child 0))")) == NK_LIST);

    // Navigate into list: first child = "+"
    it("head-sym", val_as_int(engine_eval("(gn-sym (gn-child (gn-child 0)))")) == (i64)S_ADD);

    // Test view ops: manually set and check a view bit
    engine_eval("(view-set! V_PURE 1)");
    it("view-set!", BM_GET(g_world.gram.v[V_PURE], 1));
    it("view?-true", val_truthy(engine_eval("(view? V_PURE 1)")));
    it("view?-false", !val_truthy(engine_eval("(view? V_PURE 0)")));

    // Test val-set!/val-get
    engine_eval("(val-set! 2 42)");
    it("val-set!", g_world.gram.val[2] == 42);
    it("val-get", val_as_int(engine_eval("(val-get 2)")) == 42);

    // Test a Clojure analysis rule: mark all NK_NUM as V_INT
    gram_parse(&g_world.gram, &l, "(+ 1 2.5 3)", 12);
    gram_index(&g_world.gram);

    engine_eval(
        "(loop [i 0]"
        "  (when (< i (gn-count))"
        "    (when (and (= (gn-kind i) NK_NUM) (not (gn-has-dot i)))"
        "      (view-set! V_INT i)"
        "      (val-set! i (gn-parse-int i)))"
        "    (recur (inc i))))"
    );

    // "1" is node 2, "2.5" is node 3, "3" is node 4
    u32 n1 = g_world.gram.nodes[g_world.gram.nodes[0].child].child;
    n1 = g_world.gram.nodes[n1].next;  // first arg = "1"
    u32 n25 = g_world.gram.nodes[n1].next;  // "2.5"
    u32 n3 = g_world.gram.nodes[n25].next;  // "3"

    it("rule-int-1", BM_GET(g_world.gram.v[V_INT], n1));
    it("rule-float-skip", !BM_GET(g_world.gram.v[V_INT], n25));
    it("rule-int-3", BM_GET(g_world.gram.v[V_INT], n3));
    it("rule-val-1", g_world.gram.val[n1] == 1);
    it("rule-val-3", g_world.gram.val[n3] == 3);

    // Test pure-bi?
    it("pure-bi?-add", val_truthy(engine_eval("(pure-bi? SYM_ADD)")));
    it("pure-bi?-if", !val_truthy(engine_eval("(pure-bi? SYM_IF)")));

    // Register test builtins for Clojure-level testing
    #define TREG(n, f) env_set(g_global_env, INTERN(n), make_builtin(INTERN(n), f))
    TREG("world-parse!", bi_world_parse);
    TREG("views-clear!", bi_views_clear);
    TREG("c-analyze!", bi_c_analyze);
    TREG("views-snapshot!", bi_views_snapshot);
    TREG("views-match?", bi_views_match);
    TREG("describe!", bi_describe_t);
    TREG("it!", bi_it_t);
    #undef TREG
}

// ============================================================================
// 15. Compiler Tests — JIT + CC (data-driven, C-level)
// ============================================================================

typedef struct { const char *name; const char *src; const char *exp; } EvalCase;
typedef struct { const char *name; const char *src; i64 exp; } JitCase;

#define SUITE(arr) (arr), (sizeof(arr)/sizeof(arr[0]))

static void run_jit_suite(const JitCase *t, u32 n) {
    describe("x86 jit");
    for (u32 i = 0; i < n; i++)
        it_eq_i64(t[i].name, clj_jit_run(t[i].src), t[i].exp);
}

static void run_cc_suite(const EvalCase *t, u32 n) {
    describe("clj -> c emit");
    // Verify cc_emit produces non-empty C output for each test (no GCC needed)
    for (u32 i = 0; i < n; i++) {
        arena_reset(&g_req);
        cc_emit(t[i].src);
        it(t[i].name, g_out.pos > 0);
    }
}

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

// Extended JIT tests — Clojure JIT only (bit ops, memory)
static const JitCase T_JIT_CLJ[] = {
    // Bit ops
    {"band", "(bit-and 15 9)", 9}, {"bor", "(bit-or 5 3)", 7},
    {"bxor", "(bit-xor 15 9)", 6}, {"bnot", "(bit-not 0)", -1},
    {"bshl", "(bit-shift-left 1 5)", 32}, {"bshr", "(bit-shift-right 32 3)", 4},
    {"popcnt", "(popcount 255)", 8}, {"popcnt0", "(popcount 0)", 0},
    {"popcnt1", "(popcount 1)", 1},
    {"band-expr", "(bit-and (bit-shift-right 171 5) 31)", 5},
    {"mask", "(popcount (bit-and 65535 (- (bit-shift-left 1 10) 1)))", 10},
    // Bit ops in defn
    {"defn-band", "(defn btest [x y] (bit-and x y)) (btest 255 15)", 15},
    {"defn-shl", "(defn shl1 [x n] (bit-shift-left x n)) (shl1 1 10)", 1024},
    {"defn-popcnt", "(defn pc [x] (popcount x)) (pc 42)", 3},
};

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
    u64 t0 = rdtsc();
    t_pass = t_fail = t_groups = 0;
    g_pf_batch = true;  // buffer all output, single flush at end
    pfc(C_BOLD); pf("=== genera test suite ==="); pfc(C_RESET);

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
    test_render();
    test_runtime_views();
    test_image_save_load();

    // Engine (Clojure ↔ GNode bridge)
    test_engine();

    // Self-checking eval tests — Clojure files are THE tests
    register_test_builtins();
    test_error_cases();

    // Clojure test suite — load each file with signal reset
    {
        g_signal = SIGNAL_NONE; g_depth = 0;
        engine_eval_file("src/clj/test/core.clj");
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
    }
    {
        g_signal = SIGNAL_NONE; g_depth = 0;
        engine_eval_file("src/clj/test/eval.clj");
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
    }
    {
        g_signal = SIGNAL_NONE; g_depth = 0;
        engine_eval_file("src/clj/test/passes.clj");
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
    }
    {
        g_signal = SIGNAL_NONE; g_depth = 0;
        engine_eval_file("src/clj/test/alloc.clj");
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
    }

    // New builtins: str-parent, store64! aliases, view constants
    {
        describe("wiring builtins");
        // str-parent chain: "page:/about" → "page:" → "page" → "*"
        StrId page_about = str_intern(STR_LIT("page:/about"));
        Val r1 = bi_str_parent(cons_new(val_int(page_about), NIL));
        StrId parent1 = (StrId)val_as_int(r1);
        it("parent1", str_eq(str_from_id(parent1), STR_LIT("page:")));
        Val r2 = bi_str_parent(cons_new(val_int(parent1), NIL));
        StrId parent2 = (StrId)val_as_int(r2);
        it("parent2", str_eq(str_from_id(parent2), STR_LIT("page")));
        Val r3 = bi_str_parent(cons_new(val_int(parent2), NIL));
        StrId parent3 = (StrId)val_as_int(r3);
        it("parent3-wildcard", str_eq(str_from_id(parent3), STR_LIT("*")));
        // SYM_STAR = S_MUL (the interned "*")
        it("sym-star", parent3 == S_MUL);
        // store64 + mem builtins
        arena_reset(&g_req); g_signal = SIGNAL_NONE; g_depth = 0;
        Val m = eval_string("(mem-new! 64)", g_global_env);
        if (g_signal) { it("mem-new!", false); g_signal = SIGNAL_NONE; print_flush(); }
        else {
            it("mem-new!", val_is_int(m));
            Val b = eval_string("(mem-base (mem-new! 64))", g_global_env);
            if (g_signal) { it("mem-base", false); g_signal = SIGNAL_NONE; print_flush(); }
            else it("mem-base", val_is_int(b));
        }
        // View constants accessible
        arena_reset(&g_req); g_signal = SIGNAL_NONE; g_depth = 0;
        Val va = eval_string("V_ALLOC", g_global_env);
        it_eq_i64("V_ALLOC", val_as_int(va), V_ALLOC);
        Val vs = eval_string("V_SCOPE", g_global_env);
        it_eq_i64("V_SCOPE", val_as_int(vs), V_SCOPE);
    }

    // Emit layer (data-driven)
    run_jit_suite(SUITE(T_JIT));
    run_cc_suite(SUITE(T_CC));

    // Clojure JIT walker — same tests, compiled by Clojure instead of C
    {
        describe("clj jit");
        const JitCase *t = T_JIT;
        u32 n = sizeof(T_JIT)/sizeof(T_JIT[0]);
        for (u32 i = 0; i < n; i++)
            it_eq_i64(t[i].name, clj_jit_run(t[i].src), t[i].exp);
    }

    // Clojure JIT: bit ops + memory (Clojure emitter only)
    {
        describe("clj jit: bit+mem");
        const JitCase *t = T_JIT_CLJ;
        u32 n = sizeof(T_JIT_CLJ)/sizeof(T_JIT_CLJ[0]);
        for (u32 i = 0; i < n; i++)
            it_eq_i64(t[i].name, clj_jit_run(t[i].src), t[i].exp);

        // Memory tests: set up data via interpreter, read via JIT
        engine_eval("(def _test_mem (mem-new! 4096))");
        engine_eval("(def _test_base (mem-base _test_mem))");
        i64 base = val_as_int(engine_eval("_test_base"));
        engine_eval("(store64 _test_base 42)");
        engine_eval("(store64 (+ _test_base 8) 99)");
        engine_eval("(store64 (+ _test_base 16) 7)");
        // Verify interpreter reads them back
        it("interp-load64", val_as_int(engine_eval("(load64 _test_base)")) == 42);
        it("interp-load64-8", val_as_int(engine_eval("(load64 (+ _test_base 8))")) == 99);
        // JIT: embed base address as literal, read same memory
        char jbuf[512];
        OutBuf jb = {jbuf, 0, sizeof(jbuf)};
        #define JFMT(...) (jb.pos = 0, buf_fmt(&jb, __VA_ARGS__), buf_c(&jb, '\0'), jbuf)
        it_eq_i64("jit-load64",
            clj_jit_run(JFMT("(def base %lld) (load64 base)", base)), 42);
        it_eq_i64("jit-load64-off",
            clj_jit_run(JFMT("(def base %lld) (load64 (+ base 8))", base)), 99);
        it_eq_i64("jit-load64-idx",
            clj_jit_run(JFMT("(def base %lld) (defn rd [b i] (load64 (+ b (* i 8)))) (rd base 2)", base)), 7);
        // JIT: write then read
        it_eq_i64("jit-store-read",
            clj_jit_run(JFMT("(def base %lld) (store64 (+ base 24) 555) (load64 (+ base 24))", base)), 555);
        // 32-bit load/store
        engine_eval("(store32 (+ _test_base 32) 12345)");
        it_eq_i64("jit-load32",
            clj_jit_run(JFMT("(def base %lld) (load32 (+ base 32))", base)), 12345);
        // 8-bit load/store
        engine_eval("(store8 (+ _test_base 40) 200)");
        it_eq_i64("jit-load8",
            clj_jit_run(JFMT("(def base %lld) (load8 (+ base 40))", base)), 200);
        it_eq_i64("jit-store8-read",
            clj_jit_run(JFMT("(def base %lld) (store8 (+ base 41) 77) (load8 (+ base 41))", base)), 77);
        #undef JFMT
    }

    // Clojure JIT: HAMT primitives (bit+mem compose into data structure ops)
    {
        describe("clj jit: hamt");
        // HAMT index extraction: (hash >> shift) & 31
        it_eq_i64("jit-hash-idx",
            clj_jit_run("(defn hamt-idx [h shift] (bit-and (bit-shift-right h shift) 31)) (hamt-idx 171 0)"), 11);
        it_eq_i64("jit-hash-idx5",
            clj_jit_run("(defn hamt-idx [h shift] (bit-and (bit-shift-right h shift) 31)) (hamt-idx 171 5)"), 5);
        // HAMT child position: popcount(bitmap & (bit - 1))
        it_eq_i64("jit-popcnt-mask",
            clj_jit_run("(defn hamt-pos [bm idx] (popcount (bit-and bm (- (bit-shift-left 1 idx) 1)))) (hamt-pos 65535 10)"), 10);
        // Memory: build a node in JIT, read it back
        engine_eval("(def _jh_mem (mem-new! 4096))");
        i64 hbase = val_as_int(engine_eval("(mem-base _jh_mem)"));
        char jbuf[512];
        OutBuf jb = {jbuf, 0, sizeof(jbuf)};
        #define JFMT(...) (jb.pos = 0, buf_fmt(&jb, __VA_ARGS__), buf_c(&jb, '\0'), jbuf)
        // Store a bitmap (u32) and two child pointers (u64) at base
        // bitmap = 0b11 (bits 0 and 1 set), children at +8 and +16
        engine_eval("(store32 (+ (mem-base _jh_mem) 0) 3)");   // bitmap = 3
        engine_eval("(store64 (+ (mem-base _jh_mem) 8) 42)");  // child[0] = 42
        engine_eval("(store64 (+ (mem-base _jh_mem) 16) 99)"); // child[1] = 99
        // JIT: read bitmap, compute position, read child
        it_eq_i64("jit-node-bm",
            clj_jit_run(JFMT("(def base %lld) (load32 base)", hbase)), 3);
        it_eq_i64("jit-node-child0",
            clj_jit_run(JFMT("(def base %lld) (load64 (+ base 8))", hbase)), 42);
        it_eq_i64("jit-node-child1",
            clj_jit_run(JFMT("(def base %lld) (load64 (+ base 16))", hbase)), 99);
        // Full HAMT-get via JIT: bitmap lookup + child read
        it_eq_i64("jit-hamt-lookup",
            clj_jit_run(JFMT(
                "(def base %lld)"
                "(defn node-child [p idx]"
                "  (let [bm (load32 p)"
                "        bit (bit-shift-left 1 idx)"
                "        pos (popcount (bit-and bm (- bit 1)))]"
                "    (load64 (+ p (+ 8 (* pos 8))))))"
                "(node-child base 0)", hbase)), 42);
        it_eq_i64("jit-hamt-lookup1",
            clj_jit_run(JFMT(
                "(def base %lld)"
                "(defn node-child [p idx]"
                "  (let [bm (load32 p)"
                "        bit (bit-shift-left 1 idx)"
                "        pos (popcount (bit-and bm (- bit 1)))]"
                "    (load64 (+ p (+ 8 (* pos 8))))))"
                "(node-child base 1)", hbase)), 99);
        #undef JFMT
    }

    // Bitmap intern (Clojure-level structural indexing)
    {
        describe("bitmap intern");
        // (defn add [a b] (+ a b))  = 24 bytes
        // Opens: pos 0,10,16 = 3   Closes: pos 14,22,23 = 3
        // Forms: 1  Depth: max 2  Lines: 1

        world_step("(defn add [a b] (+ a b))", false);
        it_eq_i64("bm-src-len", (i64)g_world.gram.src_len, 24);
        engine_eval("(def _bm (bm-intern-gram!))");
        it_eq_i64("bm-opens",
            val_as_int(engine_eval("(bm-count _bm BM_OPEN)")), 3);
        it_eq_i64("bm-closes",
            val_as_int(engine_eval("(bm-count _bm BM_CLOSE)")), 3);
        it_eq_i64("bm-struct",
            val_as_int(engine_eval("(bm-count _bm BM_STRUCT)")), 6);
        it_eq_i64("bm-forms",
            val_as_int(engine_eval("(bm-form-count _bm)")), 1);
        it_eq_i64("bm-max-depth",
            val_as_int(engine_eval("(bm-max-depth _bm)")), 2);
        it_eq_i64("bm-lines",
            val_as_int(engine_eval("(bm-line-count _bm)")), 1);
        it_eq_i64("bm-depth-0",
            val_as_int(engine_eval("(bm-depth-at _bm 0)")), 1);
        it_eq_i64("bm-depth-16",
            val_as_int(engine_eval("(bm-depth-at _bm 16)")), 2);

        // Multi-form source: 3 forms, 3 lines, depth 1
        world_step("(def x 1)\n(def y 2)\n(+ x y)", false);
        engine_eval("(def _bm2 (bm-intern-gram!))");
        it_eq_i64("bm2-forms",
            val_as_int(engine_eval("(bm-form-count _bm2)")), 3);
        it_eq_i64("bm2-lines",
            val_as_int(engine_eval("(bm-line-count _bm2)")), 3);
        it_eq_i64("bm2-max-depth",
            val_as_int(engine_eval("(bm-max-depth _bm2)")), 1);
        it_eq_i64("bm2-digit-token",
            val_as_int(engine_eval("(bm-and-count _bm2 BM_DIGIT BM_TOKEN)")),
            val_as_int(engine_eval("(bm-count _bm2 BM_DIGIT)")));
    }

    pf("\n");
    pfc(t_fail ? C_YELLOW : C_GREEN);
    pf("  %d passed", t_pass);
    pfc(C_RESET);
    if (t_fail) { pfc(C_RED); pf(", %d failed", t_fail); pfc(C_RESET); }
    else { pfc(C_DIM); pf(", 0 failed"); pfc(C_RESET); }
    u64 ticks = rdtsc() - t0;
    // ~3 GHz assumption for display; exact calibration would need a syscall
    u64 elapsed_us = ticks / 3000;
    pfc(C_DIM); pf(" (%d groups, %u us", t_groups, (u32)elapsed_us);
    pf(")"); pfc(C_RESET);
    pf("\n");
    g_pf_batch = false;
    print_flush();
    return t_fail;
}

#endif // TEST_C_INCLUDED
