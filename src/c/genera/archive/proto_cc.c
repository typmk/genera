/**
 * proto_cc.c — Self-Reflective C: parse → entities → compile → execute → observe
 *
 * The full loop: grammar entities → Val lowering → JIT → execute → trace.
 * Query IS execution. The bitmask engine finds code, transforms it, re-emits it.
 *
 * Build: gcc -O3 -march=native -nostdlib -static -o proto_cc test/proto_cc.c
 */

#include "../src/c/sys.c"
#include "../src/c/base.c"
#include "../src/c/read.c"
#include "../src/c/emit_x86.c"

// ============================================================================
// 1. Grammar (inlined from proto_grammar.c — the universal parser)
// ============================================================================

#define CL_WS    0x01
#define CL_ID    0x02
#define CL_DIG   0x04
#define CL_OP    0x08
#define CL_DELIM 0x10
#define CL_STR   0x20

enum {
    NK_ROOT=0, NK_LIST, NK_VEC, NK_MAP,
    NK_IDENT, NK_NUM, NK_STR, NK_OP, NK_KW, NK_OTHER,
    NK_COUNT
};
static const char *NK_NAME[] = {
    "root","list","vec","map","ident","num","str","op","kw","other"
};

typedef struct {
    u32 start; u16 len; u8 kind; u8 depth;
    u32 parent; u32 child; u32 next; u32 end;
} GNode;

typedef struct {
    const char *name;
    u8  cc[256];
    u8  open[4], close[4], dkind[4], nd;
    u8  lc1, lc2, bc1, bc2, bc3, bc4, sq, esc;
    bool negnum, kwcolon, opgrp;
} Lang;

typedef struct {
    const char *src; u32 src_len;
    GNode *nodes; u32 n, cap;
    u32 mw;
    u64 *m[NK_COUNT], *m_group, *m_leaf, *m_first;
} Gram;

#define BM_SET(m, i)   ((m)[(i)/64] |= (1ULL << ((i)%64)))
#define BM_GET(m, i)   (((m)[(i)/64] >> ((i)%64)) & 1)

static u32 bm_pop(const u64 *m, u32 nw) {
    u32 c = 0; for (u32 i = 0; i < nw; i++) c += POPCOUNT(m[i]); return c;
}
static u32 bm_next(const u64 *m, u32 nw, u32 pos) {
    u32 w = pos / 64;
    if (w >= nw) return ~0u;
    u64 bits = m[w] & ~((1ULL << (pos % 64)) - 1);
    while (!bits && ++w < nw) bits = m[w];
    return bits ? w * 64 + (u32)CTZ(bits) : ~0u;
}
static bool bm_any_range(const u64 *m, u32 lo, u32 hi) {
    if (lo >= hi) return false;
    u32 wlo = lo / 64, blo = lo % 64;
    u32 whi = (hi - 1) / 64, bhi = (hi - 1) % 64;
    if (wlo == whi) return (m[wlo] & ((2ULL << bhi) - 1) & ~((1ULL << blo) - 1)) != 0;
    if (m[wlo] & ~((1ULL << blo) - 1)) return true;
    for (u32 w = wlo + 1; w < whi; w++) if (m[w]) return true;
    return (m[whi] & ((2ULL << bhi) - 1)) != 0;
}
static u32 bm_pop_range(const u64 *m, u32 lo, u32 hi) {
    if (lo >= hi) return 0;
    u32 c = 0, wlo = lo / 64, blo = lo % 64;
    u32 whi = (hi - 1) / 64, bhi = (hi - 1) % 64;
    if (wlo == whi) return POPCOUNT(m[wlo] & ((2ULL << bhi) - 1) & ~((1ULL << blo) - 1));
    c += POPCOUNT(m[wlo] & ~((1ULL << blo) - 1));
    for (u32 w = wlo + 1; w < whi; w++) c += POPCOUNT(m[w]);
    return c + POPCOUNT(m[whi] & ((2ULL << bhi) - 1));
}

static Gram gram_new(u32 cap) {
    Gram g = {0}; g.cap = cap;
    g.nodes = (GNode *)arena_alloc(&g_perm, cap * sizeof(GNode), 8);
    return g;
}
static StrId TK_NODE, TK_PARSE, TK_INDEX, TK_BRIDGE, TK_COMPILE, TK_EXEC;

static u32 gn_add(Gram *g, u8 kind, u32 start, u16 len, u32 parent, u8 dep) {
    u32 id = g->n++;
    g->nodes[id] = (GNode){start, len, kind, dep, parent, 0, 0, id + 1};
    TAP(TK_NODE, id, kind, start);
    return id;
}
#define LINK(g, last, par, id) do { \
    if (last[par]) (g)->nodes[last[par]].next = id; \
    else (g)->nodes[par].child = id; \
    last[par] = id; \
} while(0)

static void lang_lisp(Lang *l) {
    memset(l, 0, sizeof(*l));
    l->name = "lisp";
    l->cc[' '] = l->cc['\t'] = l->cc['\n'] = l->cc['\r'] = l->cc[','] = CL_WS;
    for (int c = '0'; c <= '9'; c++) l->cc[c] = CL_DIG;
    for (int c = 'a'; c <= 'z'; c++) l->cc[c] = CL_ID;
    for (int c = 'A'; c <= 'Z'; c++) l->cc[c] = CL_ID;
    const char *sc = "!%&*+-./<=>?_";
    for (const char *p = sc; *p; p++) l->cc[(u8)*p] = CL_ID;
    l->cc['('] = l->cc[')'] = CL_DELIM;
    l->cc['['] = l->cc[']'] = CL_DELIM;
    l->cc['{'] = l->cc['}'] = CL_DELIM;
    l->cc['"'] = CL_STR;
    l->nd = 3;
    l->open[0]='('; l->close[0]=')'; l->dkind[0]=NK_LIST;
    l->open[1]='['; l->close[1]=']'; l->dkind[1]=NK_VEC;
    l->open[2]='{'; l->close[2]='}'; l->dkind[2]=NK_MAP;
    l->lc1 = ';'; l->sq = '"'; l->esc = '\\';
    l->negnum = true; l->kwcolon = true;
}

static void gram_parse(Gram *g, const Lang *l, const char *src, u32 len) {
    g->src = src; g->src_len = len; g->n = 0;
    gn_add(g, NK_ROOT, 0, (u16)(len > 65535 ? 65535 : len), 0, 0);
    u32 *last = (u32 *)arena_alloc(&g_temp, g->cap * sizeof(u32), 4);
    memset(last, 0, g->cap * sizeof(u32));
    u32 stk[256], sp = 0, cur = 0; u8 dep = 0;
    u32 pos = 0;
    while (pos < len) {
        u8 c = (u8)src[pos]; u8 cl = l->cc[c];
        if (cl & CL_WS) { pos++; continue; }
        if (l->lc1 && c == l->lc1) {
            if (!l->lc2 || (pos + 1 < len && (u8)src[pos+1] == l->lc2))
                { while (pos < len && src[pos] != '\n') pos++; continue; }
        }
        if (cl & CL_STR) {
            u8 q = c; u32 s = pos++;
            while (pos < len && (u8)src[pos] != q) { if ((u8)src[pos] == l->esc) pos++; pos++; }
            if (pos < len) pos++;
            u32 id = gn_add(g, NK_STR, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        bool hit = false;
        for (u32 d = 0; d < l->nd; d++) {
            if (c == l->open[d]) {
                dep++; u32 id = gn_add(g, l->dkind[d], pos, 0, cur, dep);
                LINK(g, last, cur, id);
                stk[sp++] = cur; cur = id; last[cur] = 0;
                pos++; hit = true; break;
            }
        }
        if (hit) continue;
        for (u32 d = 0; d < l->nd; d++) {
            if (c == l->close[d]) {
                g->nodes[cur].len = (u16)(pos + 1 - g->nodes[cur].start);
                g->nodes[cur].end = g->n;
                if (sp > 0) { cur = stk[--sp]; dep--; }
                pos++; hit = true; break;
            }
        }
        if (hit) continue;
        if (l->negnum && c == '-' && pos + 1 < len && (l->cc[(u8)src[pos+1]] & CL_DIG)
            && (pos == 0 || (l->cc[(u8)src[pos-1]] & (CL_WS | CL_DELIM)))) {
            u32 s = pos++; while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            u32 id = gn_add(g, NK_NUM, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        if (cl & CL_DIG) {
            u32 s = pos; while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            u32 id = gn_add(g, NK_NUM, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        if (l->kwcolon && c == ':' && pos + 1 < len && (l->cc[(u8)src[pos+1]] & CL_ID)) {
            u32 s = pos++; while (pos < len && (l->cc[(u8)src[pos]] & (CL_ID | CL_DIG))) pos++;
            u32 id = gn_add(g, NK_KW, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        if (cl & CL_ID) {
            u32 s = pos; while (pos < len && (l->cc[(u8)src[pos]] & (CL_ID | CL_DIG))) pos++;
            u32 id = gn_add(g, NK_IDENT, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        if (cl & CL_OP) {
            u32 s = pos++;
            if (l->opgrp) while (pos < len && (l->cc[(u8)src[pos]] & CL_OP)) pos++;
            u32 id = gn_add(g, NK_OP, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id); continue;
        }
        pos++;
    }
    while (sp > 0) { g->nodes[cur].end = g->n; cur = stk[--sp]; }
    g->nodes[0].end = g->n;
    arena_reset(&g_temp);
}

static u64 *bm_new(u32 nw) {
    u64 *m = (u64 *)arena_alloc(&g_req, nw * 8, 8);
    memset(m, 0, nw * 8); return m;
}
static void gram_index(Gram *g) {
    g->mw = (g->n + 63) / 64;
    for (u32 k = 0; k < NK_COUNT; k++) g->m[k] = bm_new(g->mw);
    g->m_group = bm_new(g->mw); g->m_leaf = bm_new(g->mw); g->m_first = bm_new(g->mw);
    for (u32 i = 0; i < g->n; i++) {
        u8 k = g->nodes[i].kind;
        BM_SET(g->m[k], i);
        if (k >= NK_LIST && k <= NK_MAP) BM_SET(g->m_group, i);
        if (k >= NK_IDENT && k <= NK_KW) BM_SET(g->m_leaf, i);
        u32 p = g->nodes[i].parent;
        if (i > 0 && g->nodes[p].child == i) BM_SET(g->m_first, i);
    }
}

ALWAYS_INLINE Str gn_text(Gram *g, u32 i) {
    return (Str){(u8 *)(g->src + g->nodes[i].start), g->nodes[i].len};
}

// ============================================================================
// 2. THE BRIDGE: Entity → Val (grammar entities → reader format)
// ============================================================================
//
// This is the key: grammar entities and the JIT emitter speak the same language.
// GNode entity tree → Val cons lists → compile → execute.
// ~40 lines. Connects the universal grammar to both emitters.

static Val entity_to_val(Gram *g, u32 id) {
    GNode *n = &g->nodes[id];
    switch (n->kind) {
        case NK_LIST:
        case NK_VEC:
        case NK_MAP: {
            // Build cons list from children (same as read_delimited)
            Val head = NIL;
            u32 c = n->child;
            while (c) { head = cons_new(entity_to_val(g, c), head); c = g->nodes[c].next; }
            // Reverse in-place
            Val result = NIL;
            while (val_is_cons(head)) {
                Val next = cdr(head);
                ((Cons *)val_as_cons(head))->cdr = result;
                result = head; head = next;
            }
            return result;
        }
        case NK_IDENT: {
            Str text = gn_text(g, id);
            StrId sid = str_intern(text);
            if (sid == S_NIL)   return NIL;
            if (sid == S_TRUE)  return val_true();
            if (sid == S_FALSE) return val_false();
            return val_sym(sid);
        }
        case NK_NUM: {
            Str text = gn_text(g, id);
            i64 v = 0; bool neg = false; u32 i = 0;
            if (text.len && text.data[0] == '-') { neg = true; i++; }
            while (i < text.len) v = v * 10 + (text.data[i++] - '0');
            return val_int(neg ? -v : v);
        }
        case NK_STR: {
            Str text = gn_text(g, id);
            // Skip surrounding quotes
            Str inner = {text.data + 1, text.len >= 2 ? text.len - 2 : 0};
            Str *sp = arena_push(&g_req, Str);
            *sp = str_dup(&g_req, inner);
            return val_str(sp);
        }
        case NK_KW: {
            Str text = gn_text(g, id);
            // Skip leading : for keyword
            Str name = {text.data + 1, text.len > 0 ? text.len - 1 : 0};
            return val_kw(str_intern(name));
        }
        default:
            return NIL;
    }
}

// Convert all top-level forms from grammar entities to Vals.
// Feeds directly into classify() or the emitter.
static void entity_classify(Gram *g) {
    g_defn_count = g_def_count = g_main_count = 0;
    u32 c = g->nodes[0].child;
    while (c) {
        Val form = entity_to_val(g, c);
        if (val_is_cons(form) && val_is_sym(car(form))) {
            StrId sym = val_as_sym(car(form));
            if (sym == S_DEFN) {
                DefnInfo *d = &g_defns[g_defn_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->params = car(cdr(cdr(form)));
                d->body = cdr(cdr(cdr(form)));
                d->n_params = list_len(d->params);
                c = g->nodes[c].next; continue;
            }
            if (sym == S_DEF) {
                DefInfo *d = &g_defs[g_def_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->value = car(cdr(cdr(form)));
                c = g->nodes[c].next; continue;
            }
        }
        g_mains[g_main_count++] = form;
        c = g->nodes[c].next;
    }
}

// The full loop: source → entities → Val → compile → execute → result
static i64 entity_jit_run(const char *source) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;

    // 1. Parse via grammar (entities)
    static Gram g = {0};
    static bool g_init = false;
    if (!g_init) { g = gram_new(4096); g_init = true; }

    Lang l; lang_lisp(&l);
    TAP0(TK_PARSE);
    gram_parse(&g, &l, source, strlen(source));
    // No gram_index needed — entity_classify walks child/next, not bitmasks
    TAP1(TK_PARSE, g.n);

    // 2. Lower entities → Val (bridge)
    TAP0(TK_BRIDGE);
    entity_classify(&g);
    image_build();
    TAP(TK_BRIDGE, g_defn_count, g_def_count, g_main_count);

    // 3. Compile (same as compile_program, after classify)
    for (u32 i = 0; i < g_def_count; i++) {
        g_global_idx[g_defs[i].name] = (i32)g_global_count;
        g_global_gen[g_defs[i].name] = g_gen;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }
    for (u32 i = 0; i < g_defn_count; i++)
        compile_defn(&g_defns[i]);

    u32 entry = g_code.pos;
    Comp cc = {.cb = &g_code, .local_count = 0, .next_slot = 0,
               .in_loop = false, .loop_count = 0};
    x_prologue(&g_code, 256);
    for (u32 i = 0; i < g_def_count; i++) {
        c_expr(&cc, g_defs[i].value);
        x_store_abs(&g_code, &g_globals[i]);
    }
    for (u32 i = 0; i < g_main_count; i++)
        c_expr(&cc, g_mains[i]);
    x_epilogue(&g_code);

    // 4. Patch calls
    for (u32 i = 0; i < g_fix_count; i++) {
        i32 target = find_fn(g_fixes[i].target);
        if (target >= 0) {
            i32 rel = (i32)(target - (g_fixes[i].pos + 4));
            memcpy(g_code.code + g_fixes[i].pos, &rel, 4);
        }
    }

    // 5. Execute
    TAP1(TK_COMPILE, g_code.pos);
    JitFn0 fn = (JitFn0)(g_code.code + entry);
    TAP0(TK_EXEC);
    i64 result = fn();
    TAP1(TK_EXEC, (u32)(result & 0xFFFFFFFF));
    return result;
}

// ============================================================================
// 3. Tests — verify the full loop
// ============================================================================

static int t_pass, t_fail;
static void check(const char *name, bool ok) {
    if (ok) t_pass++; else { pf("  FAIL: %s\n", name); t_fail++; }
}

static void test_bridge(void) {
    pf("\n--- entity → val → jit ---\n");

    // Arithmetic
    check("(+ 1 2) = 3",     entity_jit_run("(+ 1 2)") == 3);
    check("(* 6 7) = 42",    entity_jit_run("(* 6 7)") == 42);
    check("(- 10 3) = 7",    entity_jit_run("(- 10 3)") == 7);
    check("(+ 1 (+ 2 3)) = 6", entity_jit_run("(+ 1 (+ 2 3))") == 6);

    // Comparison
    check("(< 1 2) = 1",     entity_jit_run("(< 1 2)") == 1);
    check("(> 5 3) = 1",     entity_jit_run("(> 5 3)") == 1);
    check("(= 3 3) = 1",     entity_jit_run("(= 3 3)") == 1);

    // If
    check("(if true 1 0) = 1", entity_jit_run("(if true 1 0)") == 1);
    check("(if false 1 0) = 0", entity_jit_run("(if false 1 0)") == 0);
    check("(if (< 1 2) 42 0) = 42", entity_jit_run("(if (< 1 2) 42 0)") == 42);

    // Let
    check("(let [x 5] x) = 5", entity_jit_run("(let [x 5] x)") == 5);
    check("(let [x 3 y 4] (+ x y)) = 7", entity_jit_run("(let [x 3 y 4] (+ x y))") == 7);

    // Defn + call
    check("defn+call fib(10)=55",
        entity_jit_run("(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(fib 10)") == 55);

    // TCO: loop/recur
    check("loop/recur sum",
        entity_jit_run("(loop [i 10 s 0] (if (= i 0) s (recur (- i 1) (+ s i))))") == 55);

    // Def (global)
    check("def+ref",
        entity_jit_run("(def x 42)\nx") == 42);

    pf("  %d/%d bridge tests\n", t_pass, t_pass + t_fail);
}

static void test_perf(void) {
    pf("\n--- performance: entity path vs reader path ---\n");

    const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(fib 35)";

    // Warmup
    i64 result1 = entity_jit_run(src);
    i64 result2 = jit_run(src);
    check("entity path = reader path", result1 == result2);
    pf("  fib(35) = %lld\n", result1);

    // Time: entity path (grammar → entity → Val → compile → execute)
    u32 N = 100;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) entity_jit_run(src);
    u64 t1 = now_ns();
    f64 entity_ms = (f64)(t1 - t0) / (N * 1000000.0);

    // Time: reader path (read_form → Val → compile → execute)
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) jit_run(src);
    t1 = now_ns();
    f64 reader_ms = (f64)(t1 - t0) / (N * 1000000.0);

    pf("  entity path:  "); buf_f1(&g_print_buf, entity_ms); pf(" ms\n");
    pf("  reader path:  "); buf_f1(&g_print_buf, reader_ms); pf(" ms\n");
    pf("  ratio: "); buf_f1(&g_print_buf, entity_ms / reader_ms); pf("x\n");
}

// ============================================================================
// 4. REPL — interactive eval
// ============================================================================

// Accumulated REPL source — each eval recompiles everything
static char g_repl_src[65536];
static u32  g_repl_src_len;

static void cmd_eval(Str args) {
    if (!args.len) { pf("  usage: eval <expression>\n"); return; }
    // Append to accumulated source
    if (g_repl_src_len + args.len + 2 > sizeof(g_repl_src)) {
        pf("  source buffer full\n"); return;
    }
    if (g_repl_src_len > 0) g_repl_src[g_repl_src_len++] = '\n';
    memcpy(g_repl_src + g_repl_src_len, args.data, args.len);
    g_repl_src_len += args.len;
    g_repl_src[g_repl_src_len] = 0;

    i64 result = entity_jit_run(g_repl_src);
    if (g_main_count > 0)
        pf("  => %lld\n", result);
    else
        pf("  defined\n");
}

static void cmd_reader_eval(Str args) {
    if (!args.len) { pf("  usage: reval <expression>\n"); return; }
    char buf[4096];
    u32 len = args.len < sizeof(buf) - 1 ? args.len : (u32)(sizeof(buf) - 1);
    memcpy(buf, args.data, len);
    buf[len] = 0;
    i64 result = jit_run(buf);
    pf("  => %lld\n", result);
}

static void cmd_entities(Str args) {
    if (!args.len) { pf("  usage: entities <source>\n"); return; }
    char buf[4096];
    u32 len = args.len < sizeof(buf) - 1 ? args.len : (u32)(sizeof(buf) - 1);
    memcpy(buf, args.data, len);
    buf[len] = 0;

    static Gram g = {0};
    static bool init = false;
    if (!init) { g = gram_new(4096); init = true; }

    Lang l; lang_lisp(&l);
    gram_parse(&g, &l, buf, len);
    // No gram_index — display walks linearly, doesn't need bitmasks

    for (u32 i = 0; i < g.n; i++) {
        GNode *n = &g.nodes[i];
        for (u8 d = 0; d < n->depth; d++) pf("  ");
        pf("%s", NK_NAME[n->kind]);
        if (n->kind != NK_ROOT && n->len > 0 && n->len < 60) {
            pf(" \"");
            Str t = gn_text(&g, i);
            for (u32 j = 0; j < t.len && j < 60; j++) {
                u8 c = t.data[j];
                if (c >= 32) buf_c(&g_print_buf, c);
            }
            pf("\"");
        }
        pf("\n");
    }
    pf("  %u nodes\n", g.n);
}

static void cmd_image(Str args) {
    (void)args;
    pf("  defns: %u, defs: %u, mains: %u\n", g_defn_count, g_def_count, g_main_count);
    for (u32 i = 0; i < g_defn_count; i++) {
        Str name = str_from_id(g_defns[i].name);
        pf("    ");
        for (u32 j = 0; j < name.len; j++) buf_c(&g_print_buf, name.data[j]);
        pf("/%u", g_defns[i].n_params);
        // Show calls
        ImageMeta *m = &g_image[g_defns[i].name];
        if (m->n_calls) {
            pf(" calls:");
            for (u32 j = 0; j < m->n_calls; j++) {
                Str cn = str_from_id(m->calls[j]);
                pf(" ");
                for (u32 k = 0; k < cn.len; k++) buf_c(&g_print_buf, cn.data[k]);
            }
        }
        pf("\n");
    }
}

static void cmd_query(Str args) {
    if (!args.len) { pf("  usage: query <source>\n"); return; }
    char buf[4096];
    u32 len = args.len < sizeof(buf) - 1 ? args.len : (u32)(sizeof(buf) - 1);
    memcpy(buf, args.data, len);
    buf[len] = 0;

    ArenaMark mark = arena_begin_temp(&g_req);

    static Gram g = {0};
    static bool init = false;
    if (!init) { g = gram_new(4096); init = true; }

    Lang l; lang_lisp(&l);
    gram_parse(&g, &l, buf, len);
    gram_index(&g);  // bitmasks in g_req, freed by arena_end_temp

    pf("  %u nodes:", g.n);
    for (u32 k = 0; k < NK_COUNT; k++) {
        u32 c = bm_pop(g.m[k], g.mw);
        if (c) pf(" %s=%u", NK_NAME[k], c);
    }
    pf("\n  groups=%u leaves=%u", bm_pop(g.m_group, g.mw), bm_pop(g.m_leaf, g.mw));

    // Check for recursion: does any ident in a defn body match the defn name?
    u32 lid = bm_next(g.m[NK_LIST], g.mw, 1);
    while (lid != ~0u) {
        GNode *ln = &g.nodes[lid];
        if (ln->child && g.nodes[ln->child].kind == NK_IDENT) {
            Str fn_name = gn_text(&g, ln->child);
            if (str_eq(fn_name, STR_LIT("defn"))) {
                u32 name_id = g.nodes[ln->child].next;
                Str name = name_id ? gn_text(&g, name_id) : (Str){0,0};
                // Skip past name + params, scan body only
                u32 body_start = name_id ? g.nodes[name_id].next : 0;  // params vec
                if (body_start) body_start = g.nodes[body_start].next; // first body form
                if (name.len && body_start && body_start < ln->end
                    && bm_any_range(g.m[NK_IDENT], body_start, ln->end)) {
                    u32 count = 0;
                    u32 id = bm_next(g.m[NK_IDENT], g.mw, body_start);
                    while (id != ~0u && id < ln->end) {
                        if (str_eq(gn_text(&g, id), name)) count++;
                        id = bm_next(g.m[NK_IDENT], g.mw, id + 1);
                    }
                    if (count) {
                        pf("\n  recursive: ");
                        for (u32 i = 0; i < name.len; i++) buf_c(&g_print_buf, name.data[i]);
                        pf(" (%u self-calls)", count);
                    }
                }
            }
        }
        lid = bm_next(g.m[NK_LIST], g.mw, lid + 1);
    }
    pf("\n");

    arena_end_temp(mark);
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    base_init();
    init_syms();

    // Allocate JIT code buffer
    g_code.code = (u8 *)sys_alloc_exec(CODE_SIZE);
    g_code.cap = CODE_SIZE;
    g_code.pos = 0;

    bool do_repl = false;
    for (int i = 1; i < argc; i++) {
        Str a = {(u8 *)argv[i], (u32)strlen(argv[i])};
        if (str_eq(a, STR_LIT("--repl")) || str_eq(a, STR_LIT("-r")))
            do_repl = true;
    }

    // Init trace kinds
    TK_NODE    = str_intern(STR_LIT("node"));
    TK_PARSE   = str_intern(STR_LIT("parse"));
    TK_INDEX   = str_intern(STR_LIT("index"));
    TK_BRIDGE  = str_intern(STR_LIT("bridge"));
    TK_COMPILE = str_intern(STR_LIT("compile"));
    TK_EXEC    = str_intern(STR_LIT("exec"));

    if (do_repl) {
        cmd_register("eval",     cmd_eval,        "eval <expr> (entity path)");
        cmd_register("reval",    cmd_reader_eval,  "eval <expr> (reader path)");
        cmd_register("entities", cmd_entities,     "show entities for <source>");
        cmd_register("query",    cmd_query,        "bitmask query <source>");
        cmd_register("image",    cmd_image,        "show program image");
        pf("proto_cc repl (eval, entities, query, tap, trace)\n");
        repl();
    } else {
        t_pass = t_fail = 0;
        test_bridge();
        test_perf();
        pf("\n%d passed, %d failed\n", t_pass, t_fail);
    }

    sys_free_exec(g_code.code, CODE_SIZE);
    base_cleanup();
    return t_fail ? 1 : 0;
}
