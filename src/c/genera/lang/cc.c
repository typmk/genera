/**
 * cc.c — Clojure → C Source Emitter + GCC Driver (GNode-based)
 *
 * Emits C source directly from GNode parse tree — no entity bridge.
 * GCC driver: parse → emit → write tmp → gcc → execute → capture output.
 *
 * Depends on: lang/grammar.c (GNode, Gram), base/ (OutBuf, sys_run)
 */
#ifndef CC_C_INCLUDED
#define CC_C_INCLUDED

// ============================================================================
// 1. Output Buffer + Name Mangling
// ============================================================================

static char g_out_data[1 << 20];
static OutBuf g_out;

static void out_reset(void) {
    g_out = (OutBuf){g_out_data, 0, sizeof(g_out_data)};
}

static void out(const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(&g_out, fmt, ap);
    va_end(ap);
}

static void emit_cname(StrId id) {
    Str s = str_from_id(id);
    for (u32 i = 0; i < s.len; i++) {
        u8 c = s.data[i];
        switch (c) {
            case '-': out("_"); break;
            case '?': out("_Q"); break;
            case '!': out("_B"); break;
            case '*': out("_S"); break;
            case '<': out("_LT"); break;
            case '>': out("_GT"); break;
            case '=': out("_EQ"); break;
            case '/': out("_SL"); break;
            default:  out("%c", (char)c); break;
        }
    }
}

// ============================================================================
// 2. Expression Emitter — GNode-based
// ============================================================================

static Gram *g_cc;  // set by compile_and_capture

static u32 g_temp_id;

static StrId g_loop_vars[32];
static u32   g_loop_tids[32];
static u32   g_loop_n;
static u32   g_loop_rid;

static void emg_expr(u32 id, int d);
static void emg_tail(u32 id, int d);

static const char *binop_c(StrId op) {
    if (op == S_ADD) return "+"; if (op == S_SUB) return "-";
    if (op == S_MUL) return "*"; if (op == S_DIV) return "/";
    if (op == S_MOD) return "%"; if (op == S_EQ) return "==";
    if (op == S_LT) return "<"; if (op == S_GT) return ">";
    if (op == S_LTE) return "<="; if (op == S_GTE) return ">=";
    return "?";
}

static void emg_binop(StrId op, u32 arg, int d) {
    Gram *g = g_cc;
    u32 n = 0;
    for (u32 a = arg; a; a = g->nodes[a].next) n++;
    if (n == 1 && op == S_SUB) { out("(-("); emg_expr(arg, d); out("))"); return; }
    if (n == 0) { out(op == S_MUL ? "1" : "0"); return; }
    out("("); emg_expr(arg, d);
    u32 a = g->nodes[arg].next;
    while (a) {
        out(" %s ", binop_c(op)); emg_expr(a, d);
        a = g->nodes[a].next;
    }
    out(")");
}

static void emg_println(u32 arg, int d) {
    Gram *g = g_cc;
    if (!arg) { out("({ putchar('\\n'); 0; })"); return; }
    out("({ ");
    u32 n = 0;
    u32 a = arg;
    while (a) {
        if (n > 0) out("putchar(' '); ");
        if (g->nodes[a].kind == NK_STR_NODE) {
            // String literal — skip quotes
            Str t = gn_text(g, a);
            out("fputs(\"");
            for (u32 i = 1; i + 1 < t.len; i++) {
                u8 c = t.data[i];
                if (c == '"') out("\\\""); else if (c == '\\') out("\\\\");
                else if (c == '\n') out("\\n"); else out("%c", (char)c);
            }
            out("\", stdout); ");
        } else {
            out("printf(\"%%lld\", (long long)("); emg_expr(a, d); out(")); ");
        }
        a = g->nodes[a].next; n++;
    }
    out("putchar('\\n'); 0; })");
}

static void emg_and(u32 arg, int d) {
    Gram *g = g_cc;
    if (!arg) { out("1"); return; }
    u32 t = g_temp_id++;
    out("({ i64 _t%u = ", t); emg_expr(arg, d); out(";");
    u32 a = g->nodes[arg].next;
    while (a) {
        out(" if (_t%u) { _t%u = ", t, t); emg_expr(a, d); out("; }");
        a = g->nodes[a].next;
    }
    out(" _t%u; })", t);
}

static void emg_or(u32 arg, int d) {
    Gram *g = g_cc;
    if (!arg) { out("0"); return; }
    u32 t = g_temp_id++;
    out("({ i64 _t%u = ", t); emg_expr(arg, d); out(";");
    u32 a = g->nodes[arg].next;
    while (a) {
        out(" if (!_t%u) { _t%u = ", t, t); emg_expr(a, d); out("; }");
        a = g->nodes[a].next;
    }
    out(" _t%u; })", t);
}

static void emg_if(u32 test, int d) {
    Gram *g = g_cc;
    u32 then = g->nodes[test].next;
    u32 els  = then ? g->nodes[then].next : 0;
    // Dead branch elimination via views
    if (g->analyzed) {
        if (then && BM_GET(g->v[V_DEAD], then)) { if (els) emg_expr(els, d); else out("0"); return; }
        if (els  && BM_GET(g->v[V_DEAD], els))  { emg_expr(then, d); return; }
    }
    out("("); emg_expr(test, d); out(" ? ");
    emg_expr(then, d); out(" : ");
    if (els) emg_expr(els, d); else out("0");
    out(")");
}

static void emg_let(u32 bv, int d) {
    Gram *g = g_cc;
    out("({ ");
    u32 p = g->nodes[bv].child;
    while (p) {
        out("i64 clj_"); emit_cname(gn_intern(g, p));
        u32 vn = g->nodes[p].next;
        if (!vn) break;
        out(" = "); emg_expr(vn, d+1); out("; ");
        p = g->nodes[vn].next;
    }
    u32 body = g->nodes[bv].next;
    while (body) { emg_expr(body, d+1); out("; "); body = g->nodes[body].next; }
    out("})");
}

static void emg_do(u32 body, int d) {
    out("({ ");
    while (body) { emg_expr(body, d); out("; "); body = g_cc->nodes[body].next; }
    out("})");
}

// --- loop/recur ---

static void emg_recur_stmts(u32 arg, int d) {
    Gram *g = g_cc;
    u32 i = 0, a = arg;
    while (a) {
        out("_t%u = ", g_loop_tids[i]); emg_expr(a, d); out("; ");
        a = g->nodes[a].next; i++;
    }
    for (u32 j = 0; j < i; j++) {
        out("clj_"); emit_cname(g_loop_vars[j]); out(" = _t%u; ", g_loop_tids[j]);
    }
    out("continue");
}

static void emg_tail(u32 id, int d) {
    Gram *g = g_cc;
    if (g->nodes[id].kind == NK_LIST && g->nodes[id].child) {
        u32 fc = g->nodes[id].child;
        if (g->nodes[fc].kind == NK_IDENT) {
            StrId sym = gn_intern(g, fc);
            u32 args = g->nodes[fc].next;

            if (sym == S_RECUR) { emg_recur_stmts(args, d); return; }
            if (sym == S_IF) {
                u32 test = args;
                u32 then = g->nodes[test].next;
                u32 els  = then ? g->nodes[then].next : 0;
                out("if ("); emg_expr(test, d); out(") { ");
                emg_tail(then, d);
                out("; } else { ");
                if (els) emg_tail(els, d);
                else { out("_r%u = 0; break", g_loop_rid); }
                out("; }"); return;
            }
            if (sym == S_DO) {
                u32 e = args;
                while (e && g->nodes[e].next) {
                    emg_expr(e, d); out("; "); e = g->nodes[e].next;
                }
                if (e) emg_tail(e, d);
                return;
            }
            if (sym == S_LET) {
                u32 bv = args;
                out("{ ");
                u32 p = g->nodes[bv].child;
                while (p) {
                    out("i64 clj_"); emit_cname(gn_intern(g, p));
                    u32 vn = g->nodes[p].next;
                    if (!vn) break;
                    out(" = "); emg_expr(vn, d); out("; ");
                    p = g->nodes[vn].next;
                }
                u32 body = g->nodes[bv].next;
                while (body && g->nodes[body].next) {
                    emg_expr(body, d); out("; "); body = g->nodes[body].next;
                }
                if (body) emg_tail(body, d);
                out("; }"); return;
            }
        }
    }
    out("_r%u = ", g_loop_rid); emg_expr(id, d); out("; break");
}

static void emg_loop(u32 bv, int d) {
    Gram *g = g_cc;
    StrId sv[32]; u32 st[32]; u32 sc = g_loop_n; u32 sr = g_loop_rid;
    memcpy(sv, g_loop_vars, sc * sizeof(StrId));
    memcpy(st, g_loop_tids, sc * sizeof(u32));

    g_loop_n = 0;
    g_loop_rid = g_temp_id++;
    out("({ ");
    u32 p = g->nodes[bv].child;
    while (p) {
        StrId name = gn_intern(g, p);
        u32 vn = g->nodes[p].next;
        if (!vn) break;
        g_loop_vars[g_loop_n] = name;
        g_loop_tids[g_loop_n] = g_temp_id++;
        g_loop_n++;
        out("i64 clj_"); emit_cname(name); out(" = "); emg_expr(vn, d+1); out("; ");
        p = g->nodes[vn].next;
    }
    out("i64 _r%u; ", g_loop_rid);
    for (u32 i = 0; i < g_loop_n; i++) out("i64 _t%u; ", g_loop_tids[i]);
    out("while(1) { ");
    u32 body = g->nodes[bv].next;
    while (body && g->nodes[body].next) {
        emg_expr(body, d+1); out("; "); body = g->nodes[body].next;
    }
    if (body) emg_tail(body, d+1);
    out("; } _r%u; })", g_loop_rid);

    g_loop_n = sc; g_loop_rid = sr;
    memcpy(g_loop_vars, sv, sc * sizeof(StrId));
    memcpy(g_loop_tids, st, sc * sizeof(u32));
}

// --- Main dispatcher ---

static void emg_expr(u32 id, int d) {
    Gram *g = g_cc;
    GNode *n = &g->nodes[id];

    // View-driven: dead code elimination + constant folding
    if (g->analyzed) {
        if (BM_GET(g->v[V_DEAD], id)) { out("0"); return; }
        if (BM_GET(g->v[V_CONST], id) && BM_GET(g->v[V_INT], id)) {
            out("%lld", (long long)g->const_val[id]); return;
        }
    }

    if (n->kind == NK_NUM) { out("%lld", (long long)gn_parse_int(g, id)); return; }
    if (n->kind == NK_IDENT) {
        StrId name = gn_intern(g, id);
        if (name == S_TRUE)  { out("1"); return; }
        if (name == S_FALSE || name == S_NIL) { out("0"); return; }
        out("clj_"); emit_cname(name); return;
    }
    if (n->kind == NK_STR_NODE) { out("0"); return; }
    if (n->kind != NK_LIST || !n->child) { out("0"); return; }

    u32 fc = n->child;
    if (g->nodes[fc].kind != NK_IDENT) { out("0"); return; }
    StrId sym = gn_intern(g, fc);
    u32 args = g->nodes[fc].next;

    if (sym == S_IF)   { emg_if(args, d); return; }
    if (sym == S_LET)  { emg_let(args, d); return; }
    if (sym == S_DO)   { emg_do(args, d); return; }
    if (sym == S_AND)  { emg_and(args, d); return; }
    if (sym == S_OR)   { emg_or(args, d); return; }
    if (sym == S_LOOP) { emg_loop(args, d); return; }

    if (sym == S_COND) {
        out("(");
        bool first = true;
        u32 a = args;
        while (a && g->nodes[a].next) {
            u32 expr = g->nodes[a].next;
            if (!first) out(" : ");
            first = false;
            if (g->nodes[a].kind == NK_KW) {
                // :else → always true
                emg_expr(expr, d);
                a = g->nodes[expr].next;
            } else {
                emg_expr(a, d); out(" ? "); emg_expr(expr, d);
                a = g->nodes[expr].next;
            }
        }
        if (first) out("0");
        else out(" : 0");
        out(")");
        return;
    }

    if (sym == S_WHEN) {
        out("("); emg_expr(args, d); out(" ? ({ ");
        u32 a = g->nodes[args].next;
        while (a) { emg_expr(a, d); out("; "); a = g->nodes[a].next; }
        out("}) : 0)");
        return;
    }

    if (sym == S_ADD || sym == S_SUB || sym == S_MUL || sym == S_DIV ||
        sym == S_MOD || sym == S_EQ  || sym == S_LT  || sym == S_GT ||
        sym == S_LTE || sym == S_GTE) { emg_binop(sym, args, d); return; }

    if (sym == S_NOT)   { out("(!("); emg_expr(args, d); out("))"); return; }
    if (sym == S_INC)   { out("(("); emg_expr(args, d); out(") + 1)"); return; }
    if (sym == S_DEC)   { out("(("); emg_expr(args, d); out(") - 1)"); return; }
    if (sym == S_ZEROQ) { out("(("); emg_expr(args, d); out(") == 0)"); return; }
    if (sym == S_POSQ)  { out("(("); emg_expr(args, d); out(") > 0)"); return; }
    if (sym == S_NEGQ)  { out("(("); emg_expr(args, d); out(") < 0)"); return; }
    if (sym == S_PRINTLN) { emg_println(args, d); return; }

    // Function call
    out("clj_"); emit_cname(sym); out("(");
    bool first = true;
    u32 a = args;
    while (a) {
        if (!first) out(", ");
        emg_expr(a, d); a = g->nodes[a].next; first = false;
    }
    out(")");
}

// ============================================================================
// 3. Program Emitter — GNode-based
// ============================================================================

static void emg_defn(u32 id) {
    Gram *g = g_cc;
    u32 fc = g->nodes[id].child;                     // "defn"
    u32 nm = g->nodes[fc].next;                       // name
    u32 pv = g->nodes[nm].next;                       // [params]
    StrId name = gn_intern(g, nm);

    u32 n_params = 0;
    for (u32 p = g->nodes[pv].child; p; p = g->nodes[p].next) n_params++;

    out("i64 clj_"); emit_cname(name); out("(");
    u32 p = g->nodes[pv].child; bool first = true;
    while (p) {
        if (!first) out(", "); out("i64 clj_"); emit_cname(gn_intern(g, p));
        p = g->nodes[p].next; first = false;
    }
    out(") {\n");

    bool tco = gn_has_recur(g, id);
    if (tco) {
        g_loop_n = 0;
        g_loop_rid = g_temp_id++;
        p = g->nodes[pv].child;
        while (p) {
            g_loop_vars[g_loop_n] = gn_intern(g, p);
            g_loop_tids[g_loop_n] = g_temp_id++;
            g_loop_n++; p = g->nodes[p].next;
        }
        out("    i64 _r%u; ", g_loop_rid);
        for (u32 i = 0; i < g_loop_n; i++) out("i64 _t%u; ", g_loop_tids[i]);
        out("\n    while(1) { ");
        u32 body = g->nodes[pv].next;
        while (body && g->nodes[body].next) {
            emg_expr(body, 2); out("; "); body = g->nodes[body].next;
        }
        if (body) emg_tail(body, 2);
        out("; }\n    return _r%u;\n", g_loop_rid);
    } else {
        out("    return ");
        u32 body = g->nodes[pv].next;
        // Count body exprs
        u32 n_body = 0;
        for (u32 b = body; b; b = g->nodes[b].next) n_body++;
        if (n_body == 1) {
            emg_expr(body, 1);
        } else {
            out("({ ");
            while (body) { emg_expr(body, 2); out("; "); body = g->nodes[body].next; }
            out("})");
        }
        out(";\n");
    }
    out("}\n\n");
}

static void emg_program(u32 *defn_ids, u32 n_defns,
                         u32 *def_ids, StrId *def_names, u32 n_defs,
                         u32 *main_ids, u32 n_mains) {
    Gram *g = g_cc;
    out_reset();
    g_temp_id = 0;
    out("/* Generated by clj — Clojure-to-C compiler */\n");
    out("#include <stdio.h>\n#include <stdint.h>\ntypedef int64_t i64;\n\n");

    // Forward declarations
    for (u32 i = 0; i < n_defns; i++) {
        u32 fc = g->nodes[defn_ids[i]].child;
        u32 nm = g->nodes[fc].next;
        u32 pv = g->nodes[nm].next;
        u32 np = 0;
        for (u32 p = g->nodes[pv].child; p; p = g->nodes[p].next) np++;
        out("i64 clj_"); emit_cname(gn_intern(g, nm)); out("(");
        for (u32 j = 0; j < np; j++) { if (j) out(", "); out("i64"); }
        out(");\n");
    }
    if (n_defns) out("\n");

    // Global defs
    for (u32 i = 0; i < n_defs; i++) {
        out("static i64 clj_"); emit_cname(def_names[i]); out(";\n");
    }
    if (n_defs) out("\n");

    // Function definitions
    for (u32 i = 0; i < n_defns; i++) emg_defn(defn_ids[i]);

    // Main
    out("int main(void) {\n");
    for (u32 i = 0; i < n_defs; i++) {
        out("    clj_"); emit_cname(def_names[i]); out(" = ");
        emg_expr(def_ids[i], 1); out(";\n");
    }
    for (u32 i = 0; i < n_mains; i++) {
        out("    "); emg_expr(main_ids[i], 1); out(";\n");
    }
    out("    return 0;\n}\n");
}

// ============================================================================
// 4. GCC Driver — compile + capture output
// ============================================================================

// cc_emit — parse source and emit C to g_out (no GCC)
static void cc_emit(const char *source) {
    gram_ensure_scratch();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    Gram *g = &g_gram_scratch;
    gram_parse(g, &lisp, source, (u32)strlen(source));
    gram_index(g);
    gram_analyze(g);
    g_cc = g;

    u32 defn_ids[256]; u32 n_defns = 0;
    u32 def_val[256]; StrId def_names[256]; u32 n_defs = 0;
    u32 main_ids[1024]; u32 n_mains = 0;

    u32 ch = g->nodes[0].child;
    while (ch) {
        if (g->nodes[ch].kind == NK_LIST && g->nodes[ch].child) {
            u32 fc = g->nodes[ch].child;
            if (g->nodes[fc].kind == NK_IDENT) {
                StrId sym = gn_intern(g, fc);
                if (sym == S_DEFN) {
                    defn_ids[n_defns++] = ch;
                    ch = g->nodes[ch].next; continue;
                }
                if (sym == S_DEF) {
                    u32 nm = g->nodes[fc].next;
                    def_names[n_defs] = gn_intern(g, nm);
                    def_val[n_defs] = g->nodes[nm].next;
                    n_defs++;
                    ch = g->nodes[ch].next; continue;
                }
            }
        }
        main_ids[n_mains++] = ch;
        ch = g->nodes[ch].next;
    }

    emg_program(defn_ids, n_defns, def_val, def_names, n_defs, main_ids, n_mains);
}

static char g_captured[65536];
static u32 g_captured_len;

static int compile_and_capture(const char *source) {
    arena_reset(&g_req);
    cc_emit(source);
    sys_write_file("/tmp/_clj_out.c", g_out.buf, g_out.pos);

    char *gcc_argv[] = {"/usr/bin/gcc", "-O2", "-w", "-o", "/tmp/_clj_out", "/tmp/_clj_out.c", NULL};
    int rc = sys_run("/usr/bin/gcc", gcc_argv);
    if (rc != 0) {
        memcpy(g_captured, "COMPILE_ERROR", 14);
        g_captured_len = 13;
        return -1;
    }

    char *run_argv[] = {"/tmp/_clj_out", NULL};
    rc = sys_run_capture("/tmp/_clj_out", run_argv, g_captured, sizeof(g_captured), &g_captured_len);

    while (g_captured_len > 0 && g_captured[g_captured_len - 1] == '\n')
        g_captured[--g_captured_len] = '\0';

    return rc;
}

#endif // CC_C_INCLUDED
