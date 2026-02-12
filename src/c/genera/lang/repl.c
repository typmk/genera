/**
 * repl.c — Extended REPL Commands
 *
 * Grammar introspection, eval, JIT, C emit — all reachable from REPL.
 * Base REPL (help, trace, tap, arena, intern) registered by cmd.c.
 *
 * Depends on: emit/emit.c (and everything below)
 */
#ifndef GLASS_REPL_C_INCLUDED
#define GLASS_REPL_C_INCLUDED

// Global grammar state for REPL
static Gram *g_gram;
static Lang  g_lang;

// ============================================================================
// Grammar Commands
// ============================================================================

static void cmd_parse(Str args) {
    if (!args.len) { pf("  usage: parse <source>\n"); return; }
    if (!g_gram) {
        static Gram gram = {0};
        gram = gram_new(4096);
        g_gram = &gram;
    }
    gram_parse(g_gram, &g_lang, (const char *)args.data, args.len);
    gram_index(g_gram);
    pf("  %u nodes, %u bytes\n", g_gram->n, g_gram->src_len);
}

static void cmd_tree(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    gram_print(g_gram, 0);
}

static void cmd_nodes(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    for (u32 i = 0; i < g_gram->n; i++) {
        GNode *n = &g_gram->nodes[i];
        pf("  %u: %s", i, NK_NAME[n->kind]);
        if (n->len && n->len < 60) { pf(" \""); pr_text(g_gram, i); pf("\""); }
        pf("  par=%u end=%u\n", n->parent, n->end);
    }
}

static void cmd_stats(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    pf("  nodes: %u  source: %u bytes\n", g_gram->n, g_gram->src_len);
    for (u32 k = 0; k < NK_COUNT; k++) {
        u32 c = bm_pop(g_gram->m[k], g_gram->mw);
        if (c) pf("    %s: %u\n", NK_NAME[k], c);
    }
}

static void cmd_setlang(Str args) {
    if (str_eq(args, STR_LIT("lisp")) || str_eq(args, STR_LIT("clj")))
        lang_lisp(&g_lang);
    else if (str_eq(args, STR_LIT("c")))
        lang_c(&g_lang);
    else if (str_eq(args, STR_LIT("bf")))
        lang_bf(&g_lang);
    else { pf("  usage: lang lisp|c|bf\n"); return; }
    pf("  lang: %s\n", g_lang.name);
}

// ============================================================================
// Query Command
// ============================================================================

static void cmd_query(Str args) {
    if (!args.len) { pf("  usage: query <source>\n"); return; }
    if (!g_gram) {
        static Gram gram = {0};
        gram = gram_new(4096);
        g_gram = &gram;
    }
    gram_parse(g_gram, &g_lang, (const char *)args.data, args.len);
    gram_index(g_gram);

    StrId s_defn  = str_intern(STR_LIT("defn"));
    StrId s_def   = str_intern(STR_LIT("def"));
    StrId s_recur = str_intern(STR_LIT("recur"));

    // Build recur bitmask
    u64 *m_recur = bm_new(g_gram->mw);
    for (u32 i = bm_next(g_gram->m[NK_LIST], g_gram->mw, 0); i < g_gram->n;
             i = bm_next(g_gram->m[NK_LIST], g_gram->mw, i + 1)) {
        u32 fc = gn_child(g_gram, i);
        if (fc && gn_kind(g_gram, fc) == NK_IDENT && gn_intern(g_gram, fc) == s_recur)
            BM_SET(m_recur, i);
    }

    // Classify top-level
    u32 ch = gn_child(g_gram, 0);
    while (ch) {
        if (gn_kind(g_gram, ch) == NK_LIST) {
            u32 fc = gn_child(g_gram, ch);
            if (fc && gn_kind(g_gram, fc) == NK_IDENT) {
                StrId name = gn_intern(g_gram, fc);
                if (name == s_defn) {
                    u32 fn_name = gn_nth(g_gram, ch, 1);
                    pf("  defn "); if (fn_name) pr_text(g_gram, fn_name);
                    bool has = gn_has(g_gram, m_recur, ch);
                    u32 idents = gn_sub_count(g_gram, g_gram->m[NK_IDENT], ch);
                    u32 lists = gn_sub_count(g_gram, g_gram->m[NK_LIST], ch);
                    pf("  recur=%s  idents=%u  lists=%u\n", has ? "yes" : "no", idents, lists);
                } else if (name == s_def) {
                    u32 var_name = gn_nth(g_gram, ch, 1);
                    pf("  def "); if (var_name) pr_text(g_gram, var_name); pf("\n");
                }
            }
        }
        ch = gn_next(g_gram, ch);
    }
}

// ============================================================================
// Eval / JIT / Emit Commands
// ============================================================================

// Null-terminate args on the stack (safe across arena_reset)
#define ARGS_STR(args, buf, max) do { \
    u32 _len = (args).len < (max) - 1 ? (args).len : (max) - 1; \
    memcpy(buf, (args).data, _len); buf[_len] = 0; \
} while(0)

static void cmd_eval(Str args) {
    if (!args.len) { pf("  usage: eval <expr>\n"); return; }
    char buf[4096];
    ARGS_STR(args, buf, 4096);
    Val form = gram_read(buf);
    g_signal = SIGNAL_NONE; g_depth = 0;
    Val result = eval(form, g_global_env);
    if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); return; }
    buf_s(&g_print_buf, "  ");
    pp_val(result);
    buf_c(&g_print_buf, '\n');
    print_flush();
}

static void cmd_jit(Str args) {
    if (!args.len) { pf("  usage: jit <source>\n"); return; }
    char buf[4096];
    ARGS_STR(args, buf, 4096);
    i64 result = jit_run(buf);
    pf("  %lld\n", (long long)result);
}

static void cmd_emit(Str args) {
    if (!args.len) { pf("  usage: emit <source>\n"); return; }
    char buf[4096];
    ARGS_STR(args, buf, 4096);
    cc_emit(buf);
    sys_write(1, g_out.buf, g_out.pos);
}

static void cmd_reval(Str args) {
    if (!args.len) { pf("  usage: reval <source>\n"); return; }
    char buf[4096];
    ARGS_STR(args, buf, 4096);
    int rc = compile_and_capture(buf);
    if (rc == 0) pf("  %s\n", g_captured);
    else pf("  error\n");
}

// ============================================================================
// Analyze Command — uses views instead of image.c
// ============================================================================

static void cmd_analyze(Str args) {
    if (!args.len) { pf("  usage: analyze <source>\n"); return; }
    if (!g_gram) {
        static Gram gram = {0};
        gram = gram_new(4096);
        g_gram = &gram;
    }
    gram_parse(g_gram, &g_lang, (const char *)args.data, args.len);
    gram_analyze(g_gram);

    u32 defs = bm_pop(g_gram->v[V_DEF], g_gram->mw);
    u32 refs = bm_pop(g_gram->v[V_REF], g_gram->mw);
    u32 calls = bm_pop(g_gram->v[V_CALL], g_gram->mw);
    u32 pure = bm_pop(g_gram->v[V_PURE], g_gram->mw);
    u32 consts = bm_pop(g_gram->v[V_CONST], g_gram->mw);
    u32 dead = bm_pop(g_gram->v[V_DEAD], g_gram->mw);
    u32 tails = bm_pop(g_gram->v[V_TAIL], g_gram->mw);
    pf("  %u nodes  defs=%u refs=%u calls=%u\n", g_gram->n, defs, refs, calls);
    pf("  pure=%u const=%u dead=%u tail=%u\n", pure, consts, dead, tails);
}

// ============================================================================
// Register All REPL Commands
// ============================================================================

static void glass_repl_init(void) {
    // Grammar
    cmd_register("parse",  cmd_parse,   "parse <source>");
    cmd_register("tree",   cmd_tree,    "show parse tree");
    cmd_register("nodes",  cmd_nodes,   "list all nodes");
    cmd_register("stats",  cmd_stats,   "node kind counts");
    cmd_register("lang",   cmd_setlang, "set language (lisp|c|bf)");
    cmd_register("query",  cmd_query,   "analyze source (defns, recur)");

    // Eval / JIT / Emit
    cmd_register("eval",   cmd_eval,    "tree-walk eval <expr>");
    cmd_register("jit",    cmd_jit,     "JIT compile + run <source>");
    cmd_register("emit",   cmd_emit,    "emit C source <source>");
    cmd_register("reval",  cmd_reval,   "compile via gcc + run <source>");

    // Analysis
    cmd_register("analyze", cmd_analyze, "analyze source (views)");

    // Default language
    lang_lisp(&g_lang);
}

#endif // GLASS_REPL_C_INCLUDED
