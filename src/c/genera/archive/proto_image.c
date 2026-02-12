/**
 * proto_image.c — L6: The Program as Indexed Data
 *
 * L6 = no loop. sig() = no search. Same shape.
 *
 *   sig:     on("page:/about", handler)  →  table[intern("page:/about")]
 *   emit:    emit("page:/about")         →  table[intern("page:/about")](data)
 *
 *   image:   define("fib", body)         →  image[intern("fib")] = {body, calls, arity}
 *   query:   "what calls fib?"           →  image[intern("fib")].callers  (O(1))
 *
 * The principle: INTERN AT BOUNDARY. INDEX EVERYWHERE. NEVER SEARCH.
 *
 * Currently in the compiler:
 *   find_fn()     — O(n) linear scan of g_fntab[]
 *   find_global() — O(n) linear scan of g_global_names[]
 *   find_local()  — O(n) linear scan of locals[]
 *
 * After interning: all O(1). StrId IS the index.
 *
 * The program becomes a database:
 *   "what does fib call?"     →  image[S_FIB].calls[]
 *   "what calls fib?"         →  image[S_FIB].callers[]
 *   "what's fib's arity?"     →  image[S_FIB].n_params
 *   "recompile fib"           →  jit(image[S_FIB].body)
 *   "is X defined?"           →  image[X].defined
 *
 * This IS the code image. php.php for C. sig() for programs.
 *
 * Build: gcc -O3 -march=native -nostdlib -static -o proto_image test/proto_image.c
 */

#include "../src/c/sys.c"
#include "../src/c/base.c"
#include "../src/c/read.c"

// ============================================================================
// 1. The Image — program as indexed data
// ============================================================================

#define IMAGE_CAP 4096  // max interned symbols (plenty for a language runtime)

typedef struct {
    // Definition
    bool defined;
    u32  code_offset;   // offset in code buffer (for JIT)
    u32  n_params;      // arity
    Val  params;        // parameter list (for reflection/recompile)
    Val  body;          // body forms (for recompile)

    // Relations (precomputed at define-time)
    StrId calls[32];    // functions this calls
    u32   n_calls;
    StrId callers[32];  // functions that call this
    u32   n_callers;

    // Global value (for def, not defn)
    bool  is_global;
    i64   global_val;
} ImageEntry;

static ImageEntry g_image[IMAGE_CAP];

// ============================================================================
// 2. Call graph extraction — walk AST, collect function references
// ============================================================================

static void extract_calls(Val form, StrId self, StrId *out, u32 *count, u32 cap) {
    if (!val_is_cons(form)) return;
    Val head = car(form);
    Val args = cdr(form);

    // If head is a symbol that's not a special form or builtin, it's a call
    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);
        // Skip special forms and builtins
        bool is_special = (sym == S_IF || sym == S_LET || sym == S_DO ||
                          sym == S_AND || sym == S_OR || sym == S_LOOP ||
                          sym == S_RECUR || sym == S_DEF || sym == S_DEFN);
        bool is_builtin = (sym == S_ADD || sym == S_SUB || sym == S_MUL ||
                          sym == S_DIV || sym == S_MOD || sym == S_EQ ||
                          sym == S_LT || sym == S_GT || sym == S_LTE ||
                          sym == S_GTE || sym == S_NOT || sym == S_INC ||
                          sym == S_DEC || sym == S_PRINTLN ||
                          sym == S_ZEROQ || sym == S_POSQ || sym == S_NEGQ);
        if (!is_special && !is_builtin && sym != self) {
            // Dedup: check if already recorded
            bool dup = false;
            for (u32 i = 0; i < *count; i++)
                if (out[i] == sym) { dup = true; break; }
            if (!dup && *count < cap)
                out[(*count)++] = sym;
        }
    }

    // Recurse into all subforms
    Val f = form;
    while (val_is_cons(f)) {
        Val sub = car(f);
        if (val_is_cons(sub))
            extract_calls(sub, self, out, count, cap);
        f = cdr(f);
    }
}

// ============================================================================
// 3. Image builder — parse + classify + index
// ============================================================================

static void image_define(const char *source) {
    arena_reset(&g_req);
    classify(source);

    // Index all defns
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId name = g_defns[i].name;
        ImageEntry *e = &g_image[name];
        e->defined = true;
        e->n_params = g_defns[i].n_params;
        e->params = g_defns[i].params;
        e->body = g_defns[i].body;
        e->is_global = false;

        // Extract calls
        e->n_calls = 0;
        Val body = g_defns[i].body;
        while (val_is_cons(body)) {
            extract_calls(car(body), name, e->calls, &e->n_calls, 32);
            body = cdr(body);
        }
    }

    // Index all defs
    for (u32 i = 0; i < g_def_count; i++) {
        StrId name = g_defs[i].name;
        ImageEntry *e = &g_image[name];
        e->defined = true;
        e->is_global = true;
        e->n_params = 0;
    }

    // Build reverse index: callers
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId caller = g_defns[i].name;
        ImageEntry *ce = &g_image[caller];
        for (u32 j = 0; j < ce->n_calls; j++) {
            StrId callee = ce->calls[j];
            ImageEntry *ee = &g_image[callee];
            if (ee->n_callers < 32)
                ee->callers[ee->n_callers++] = caller;
        }
    }
}

// ============================================================================
// 4. O(1) queries — the point of all this
// ============================================================================

// Is X defined?
ALWAYS_INLINE bool image_defined(StrId name) {
    return g_image[name].defined;
}

// What's X's arity?
ALWAYS_INLINE u32 image_arity(StrId name) {
    return g_image[name].n_params;
}

// What does X call?
ALWAYS_INLINE StrId *image_calls(StrId name, u32 *n) {
    *n = g_image[name].n_calls;
    return g_image[name].calls;
}

// What calls X?
ALWAYS_INLINE StrId *image_callers(StrId name, u32 *n) {
    *n = g_image[name].n_callers;
    return g_image[name].callers;
}

// Find function code offset — O(1) instead of O(n) find_fn()
ALWAYS_INLINE i32 image_offset(StrId name) {
    return g_image[name].defined ? (i32)g_image[name].code_offset : -1;
}

// ============================================================================
// 5. Comparison: O(n) vs O(1) function lookup
// ============================================================================

// Current approach: linear scan (from emit_x86.c)
typedef struct { StrId name; u32 offset; } FnEntry;
static FnEntry fn_table[256];
static u32 fn_count;

static i32 find_fn_linear(StrId name) {
    for (u32 i = 0; i < fn_count; i++)
        if (fn_table[i].name == name) return (i32)fn_table[i].offset;
    return -1;
}

// Image approach: direct index
static i32 fn_offsets[IMAGE_CAP];  // -1 = not defined

static i32 find_fn_indexed(StrId name) {
    return fn_offsets[name];
}

// ============================================================================
// Tests
// ============================================================================

static int t_pass, t_fail;

static void check_eq(const char *name, i64 got, i64 expected) {
    if (got == expected) { t_pass++; }
    else { pf("  FAIL %s: expected %lld, got %lld\n", name, (long long)expected, (long long)got); t_fail++; }
}

static void check_true(const char *name, bool cond) {
    if (cond) { t_pass++; }
    else { pf("  FAIL %s\n", name); t_fail++; }
}

static void run_tests(void) {
    pf("=== image tests ===\n");
    t_pass = t_fail = 0;

    // Define a multi-function program
    const char *prog =
        "(def pi 3)"
        "(defn add [a b] (+ a b))"
        "(defn double [x] (* x 2))"
        "(defn quad [x] (double (double x)))"
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) (+ s i)))))"
        "(defn main [] (+ (quad 5) (fib 10) (solve 100)))";

    image_define(prog);

    // Intern the names we want to query
    StrId S_add = INTERN("add");
    StrId S_double = INTERN("double");
    StrId S_quad = INTERN("quad");
    StrId S_fib = INTERN("fib");
    StrId S_solve = INTERN("solve");
    StrId S_main = INTERN("main");
    StrId S_pi = INTERN("pi");
    StrId S_xyz = INTERN("xyz");  // not defined

    // O(1) defined?
    check_true("def-add",    image_defined(S_add));
    check_true("def-fib",    image_defined(S_fib));
    check_true("def-pi",     image_defined(S_pi));
    check_true("def-xyz-no", !image_defined(S_xyz));

    // O(1) arity
    check_eq("arity-add",    image_arity(S_add), 2);
    check_eq("arity-double", image_arity(S_double), 1);
    check_eq("arity-fib",    image_arity(S_fib), 1);
    check_eq("arity-main",   image_arity(S_main), 0);

    // O(1) calls
    {
        u32 n;
        StrId *calls = image_calls(S_quad, &n);
        check_eq("calls-quad-n", n, 1);
        check_true("calls-quad-double", n > 0 && calls[0] == S_double);
    }
    {
        u32 n;
        StrId *calls = image_calls(S_fib, &n);
        // fib calls itself — but we skip self-calls in extract_calls
        check_eq("calls-fib-n", n, 0);  // self-recursive, no external calls
    }
    {
        u32 n;
        StrId *calls = image_calls(S_main, &n);
        check_eq("calls-main-n", n, 3);  // quad, fib, solve
    }

    // O(1) callers (reverse index)
    {
        u32 n;
        StrId *callers = image_callers(S_double, &n);
        check_eq("callers-double-n", n, 1);
        check_true("callers-double-quad", n > 0 && callers[0] == S_quad);
    }
    {
        u32 n;
        StrId *callers = image_callers(S_fib, &n);
        check_eq("callers-fib-n", n, 1);  // main calls fib
        check_true("callers-fib-main", n > 0 && callers[0] == S_main);
    }

    // O(1) global check
    check_true("global-pi", g_image[S_pi].is_global);
    check_true("global-add-no", !g_image[S_add].is_global);

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
    buf_s(&g_print_buf, " ns/op\n");                 \
    buf_flush(&g_print_buf, 1);                      \
} while(0)

static void run_bench(void) {
    pf("\n=== benchmarks ===\n");

    u32 N = 10000000;

    // Set up linear table (simulating current compiler)
    fn_count = 0;
    StrId names[7];
    const char *name_strs[] = {"add", "double", "quad", "fib", "solve", "main", "helper"};
    for (u32 i = 0; i < 7; i++) {
        names[i] = INTERN(name_strs[i]);
        fn_table[fn_count++] = (FnEntry){names[i], i * 100};
        fn_offsets[names[i]] = (i32)(i * 100);
    }

    StrId target = names[5];  // "main" — near end of table (worst case for linear)

    pf("\n  -- function lookup (7 functions, find 'main') --\n");

    BENCH("O(n) linear scan:", N, {
        i32 off = find_fn_linear(target);
        SINK(off);
    });

    BENCH("O(1) indexed:", N, {
        i32 off = find_fn_indexed(target);
        SINK(off);
    });

    BENCH("O(1) image query:", N, {
        bool def = image_defined(target);
        SINK(def);
    });

    // Scale: 50 functions (more realistic for a real program)
    pf("\n  -- function lookup (50 functions, find last) --\n");
    fn_count = 0;
    StrId many[50];
    for (u32 i = 0; i < 50; i++) {
        // Generate unique names by interning "fn0", "fn1", etc.
        char buf[8]; buf[0]='f'; buf[1]='n';
        buf[2] = '0' + (char)(i / 10); buf[3] = '0' + (char)(i % 10); buf[4] = 0;
        many[i] = str_intern((Str){(u8*)buf, 4});
        fn_table[fn_count++] = (FnEntry){many[i], i * 50};
        fn_offsets[many[i]] = (i32)(i * 50);
    }
    target = many[49];  // last one

    BENCH("O(n) linear (n=50):", N, {
        i32 off = find_fn_linear(target);
        SINK(off);
    });

    BENCH("O(1) indexed (n=50):", N, {
        i32 off = find_fn_indexed(target);
        SINK(off);
    });

    // Image queries
    pf("\n  -- image queries --\n");

    StrId S_main = INTERN("main");
    StrId S_double = INTERN("double");

    BENCH("arity query:", N, {
        u32 a = image_arity(S_main);
        SINK(a);
    });

    BENCH("calls query:", N, {
        u32 n;
        StrId *c = image_calls(S_main, &n);
        SINK(n + (i64)c);
    });

    BENCH("callers query:", N, {
        u32 n;
        StrId *c = image_callers(S_double, &n);
        SINK(n + (i64)c);
    });

    BENCH("defined? query:", N, {
        bool d = image_defined(S_main);
        SINK(d);
    });

    // Compare with hash map lookup
    pf("\n  -- vs hashmap --\n");

    HashMap fn_map = hashmap_create(&g_perm, 64);
    for (u32 i = 0; i < 50; i++)
        hashmap_put(&fn_map, many[i], val_int(i * 50));

    BENCH("hashmap_get (n=50):", N, {
        Val v;
        hashmap_get(&fn_map, target, &v);
        SINK(v);
    });

    BENCH("O(1) indexed:", N, {
        i32 off = find_fn_indexed(target);
        SINK(off);
    });
}

// ============================================================================
// Analysis
// ============================================================================

static void print_analysis(void) {
    pf("\n=== L6 = sig() = image = O(1) ===\n\n");

    pf("  The same shape:\n\n");
    pf("    sig:     name    → table[intern(name)]   → handler\n");
    pf("    image:   symbol  → image[strid]           → {code, arity, calls}\n");
    pf("    L6:      n       → n*(n-1)/2              → answer\n\n");
    pf("    All three: ELIMINATE THE SEARCH.\n");
    pf("    Intern once (O(n) at boundary), query forever (O(1)).\n\n");

    pf("  What the image gives us:\n\n");
    pf("    image_defined(name)       O(1)  \"is X defined?\"\n");
    pf("    image_arity(name)         O(1)  \"how many args?\"\n");
    pf("    image_calls(name)         O(1)  \"what does X call?\"\n");
    pf("    image_callers(name)       O(1)  \"who calls X?\"\n");
    pf("    image_offset(name)        O(1)  \"where's the code?\"\n");
    pf("    g_image[name].body        O(1)  \"recompile from source\"\n\n");

    pf("  Cost: %u bytes for %u-entry image\n",
       (u32)(IMAGE_CAP * sizeof(ImageEntry)), IMAGE_CAP);
    pf("  Most entries empty (sparse but fast)\n\n");

    pf("  The compiler currently:\n");
    pf("    find_fn():     O(n) scan per call site\n");
    pf("    find_global(): O(n) scan per reference\n");
    pf("    find_local():  O(n) scan per variable\n\n");

    pf("  After image:\n");
    pf("    find_fn():     O(1) — fn_offsets[strid]\n");
    pf("    find_global(): O(1) — image[strid].global_val\n");
    pf("    find_local():  O(1) — locals as stack frame offsets\n\n");

    pf("  This IS php.php for C. sig() for programs.\n");
    pf("  Define → index. Query → O(1). Redefine → reindex.\n");
    pf("  The program is not text. It's a table.\n\n");

    pf("  Hot-reload:\n");
    pf("    1. User edits (defn fib ...)\n");
    pf("    2. image_define(new_source)  — re-intern, O(1) update\n");
    pf("    3. Recompile fib only: jit(image[S_fib].body)\n");
    pf("    4. Patch call sites: all callers in image[S_fib].callers[]\n");
    pf("    5. No restart. No full recompile. O(callers) patching.\n");
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    base_init();
    init_syms();

    // Init image to empty
    for (u32 i = 0; i < IMAGE_CAP; i++) {
        g_image[i].defined = false;
        g_image[i].n_calls = 0;
        g_image[i].n_callers = 0;
    }
    memset(fn_offsets, 0xFF, sizeof(fn_offsets));  // -1 = not defined

    run_tests();
    run_bench();
    print_analysis();

    pf("\n%d passed, %d failed\n", t_pass, t_fail);
    base_cleanup();
    return t_fail ? 1 : 0;
}
