/**
 * cc.c — Clojure → C Source Emitter + GCC Driver
 *
 * Compiles Clojure to C source. Output IS C, so any C function is callable.
 * GCC driver: emit → write tmp → gcc → execute → capture output.
 *
 * Depends on: lang/ (read, classify), base/ (OutBuf, sys_run)
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
// 2. Expression Emitter
// ============================================================================

static u32 g_temp_id;

static StrId g_loop_vars[32];
static u32   g_loop_tids[32];
static u32   g_loop_n;
static u32   g_loop_rid;

static void emit_expr(Val form, int d);
static void emit_tail(Val form, int d);

static const char *binop_c(StrId op) {
    if (op == S_ADD) return "+"; if (op == S_SUB) return "-";
    if (op == S_MUL) return "*"; if (op == S_DIV) return "/";
    if (op == S_MOD) return "%"; if (op == S_EQ) return "==";
    if (op == S_LT) return "<"; if (op == S_GT) return ">";
    if (op == S_LTE) return "<="; if (op == S_GTE) return ">=";
    return "?";
}

static void emit_binop(StrId op, Val args, int d) {
    u32 n = list_len(args);
    if (n == 1 && op == S_SUB) { out("(-("); emit_expr(car(args), d); out("))"); return; }
    if (n == 0) { out(op == S_MUL ? "1" : "0"); return; }
    out("("); emit_expr(car(args), d); args = cdr(args);
    while (val_is_cons(args)) {
        out(" %s ", binop_c(op)); emit_expr(car(args), d); args = cdr(args);
    }
    out(")");
}

static void emit_println(Val args, int d) {
    if (!val_is_cons(args)) { out("({ putchar('\\n'); 0; })"); return; }
    out("({ ");
    u32 n = 0;
    while (val_is_cons(args)) {
        if (n > 0) out("putchar(' '); ");
        Val v = car(args);
        if (val_is_str(v)) {
            Str *s = val_as_str(v);
            out("fputs(\"");
            for (u32 i = 0; i < s->len; i++) {
                u8 c = s->data[i];
                if (c == '"') out("\\\""); else if (c == '\\') out("\\\\");
                else if (c == '\n') out("\\n"); else out("%c", (char)c);
            }
            out("\", stdout); ");
        } else {
            out("printf(\"%%lld\", (long long)("); emit_expr(v, d); out(")); ");
        }
        args = cdr(args); n++;
    }
    out("putchar('\\n'); 0; })");
}

static void emit_and(Val args, int d) {
    if (!val_is_cons(args)) { out("1"); return; }
    u32 t = g_temp_id++;
    out("({ i64 _t%u = ", t); emit_expr(car(args), d); out(";");
    args = cdr(args);
    while (val_is_cons(args)) {
        out(" if (_t%u) { _t%u = ", t, t); emit_expr(car(args), d); out("; }");
        args = cdr(args);
    }
    out(" _t%u; })", t);
}

static void emit_or(Val args, int d) {
    if (!val_is_cons(args)) { out("0"); return; }
    u32 t = g_temp_id++;
    out("({ i64 _t%u = ", t); emit_expr(car(args), d); out(";");
    args = cdr(args);
    while (val_is_cons(args)) {
        out(" if (!_t%u) { _t%u = ", t, t); emit_expr(car(args), d); out("; }");
        args = cdr(args);
    }
    out(" _t%u; })", t);
}

static void emit_if(Val args, int d) {
    out("("); emit_expr(car(args), d); out(" ? ");
    emit_expr(car(cdr(args)), d); out(" : ");
    Val e = cdr(cdr(args));
    if (val_is_cons(e)) emit_expr(car(e), d); else out("0");
    out(")");
}

static void emit_let(Val args, int d) {
    out("({ ");
    Val bindings = pvec_to_list(car(args));
    while (val_is_cons(bindings)) {
        out("i64 clj_"); emit_cname(val_as_sym(car(bindings)));
        bindings = cdr(bindings);
        out(" = "); emit_expr(car(bindings), d+1); out("; ");
        bindings = cdr(bindings);
    }
    Val body = cdr(args);
    while (val_is_cons(body)) { emit_expr(car(body), d+1); out("; "); body = cdr(body); }
    out("})");
}

static void emit_do(Val args, int d) {
    out("({ ");
    while (val_is_cons(args)) { emit_expr(car(args), d); out("; "); args = cdr(args); }
    out("})");
}

// --- loop/recur ---

static void emit_recur_stmts(Val args, int d) {
    u32 i = 0; Val a = args;
    while (val_is_cons(a)) {
        out("_t%u = ", g_loop_tids[i]); emit_expr(car(a), d); out("; ");
        a = cdr(a); i++;
    }
    for (u32 j = 0; j < i; j++) {
        out("clj_"); emit_cname(g_loop_vars[j]); out(" = _t%u; ", g_loop_tids[j]);
    }
    out("continue");
}

static void emit_tail(Val form, int d) {
    if (val_is_cons(form) && val_is_sym(car(form))) {
        StrId sym = val_as_sym(car(form));
        Val args = cdr(form);
        if (sym == S_RECUR) { emit_recur_stmts(args, d); return; }
        if (sym == S_IF) {
            out("if ("); emit_expr(car(args), d); out(") { ");
            emit_tail(car(cdr(args)), d);
            out("; } else { ");
            Val e = cdr(cdr(args));
            if (val_is_cons(e)) emit_tail(car(e), d);
            else { out("_r%u = 0; break", g_loop_rid); }
            out("; }"); return;
        }
        if (sym == S_DO) {
            while (val_is_cons(args) && val_is_cons(cdr(args))) {
                emit_expr(car(args), d); out("; "); args = cdr(args);
            }
            if (val_is_cons(args)) emit_tail(car(args), d);
            return;
        }
        if (sym == S_LET) {
            out("{ ");
            Val bindings = pvec_to_list(car(args));
            while (val_is_cons(bindings)) {
                out("i64 clj_"); emit_cname(val_as_sym(car(bindings)));
                bindings = cdr(bindings);
                out(" = "); emit_expr(car(bindings), d); out("; ");
                bindings = cdr(bindings);
            }
            Val body = cdr(args);
            while (val_is_cons(body) && val_is_cons(cdr(body))) {
                emit_expr(car(body), d); out("; "); body = cdr(body);
            }
            if (val_is_cons(body)) emit_tail(car(body), d);
            out("; }"); return;
        }
    }
    out("_r%u = ", g_loop_rid); emit_expr(form, d); out("; break");
}

static void emit_loop(Val args, int d) {
    StrId sv[32]; u32 st[32]; u32 sc = g_loop_n; u32 sr = g_loop_rid;
    memcpy(sv, g_loop_vars, sc * sizeof(StrId));
    memcpy(st, g_loop_tids, sc * sizeof(u32));

    g_loop_n = 0;
    g_loop_rid = g_temp_id++;
    Val bindings = pvec_to_list(car(args));
    out("({ ");
    while (val_is_cons(bindings)) {
        StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
        g_loop_vars[g_loop_n] = name;
        g_loop_tids[g_loop_n] = g_temp_id++;
        g_loop_n++;
        out("i64 clj_"); emit_cname(name); out(" = "); emit_expr(car(bindings), d+1); out("; ");
        bindings = cdr(bindings);
    }
    out("i64 _r%u; ", g_loop_rid);
    for (u32 i = 0; i < g_loop_n; i++) out("i64 _t%u; ", g_loop_tids[i]);
    out("while(1) { ");
    Val body = cdr(args);
    while (val_is_cons(body) && val_is_cons(cdr(body))) {
        emit_expr(car(body), d+1); out("; "); body = cdr(body);
    }
    if (val_is_cons(body)) emit_tail(car(body), d+1);
    out("; } _r%u; })", g_loop_rid);

    g_loop_n = sc; g_loop_rid = sr;
    memcpy(g_loop_vars, sv, sc * sizeof(StrId));
    memcpy(g_loop_tids, st, sc * sizeof(u32));
}

// --- Main dispatcher ---

static void emit_expr(Val form, int d) {
    if (val_is_int(form))  { out("%lld", (long long)val_as_int(form)); return; }
    if (val_is_bool(form)) { out("%d", val_as_bool(form) ? 1 : 0); return; }
    if (val_is_nil(form))  { out("0"); return; }
    if (val_is_str(form))  { out("0"); return; }
    if (val_is_sym(form))  { out("clj_"); emit_cname(val_as_sym(form)); return; }
    if (!val_is_cons(form)) { out("0"); return; }

    Val head = car(form), args = cdr(form);
    if (!val_is_sym(head)) { out("0"); return; }
    StrId sym = val_as_sym(head);

    if (sym == S_IF)   { emit_if(args, d); return; }
    if (sym == S_LET)  { emit_let(args, d); return; }
    if (sym == S_DO)   { emit_do(args, d); return; }
    if (sym == S_AND)  { emit_and(args, d); return; }
    if (sym == S_OR)   { emit_or(args, d); return; }
    if (sym == S_LOOP) { emit_loop(args, d); return; }

    if (sym == S_COND) {
        out("(");
        bool first = true;
        while (val_is_cons(args) && val_is_cons(cdr(args))) {
            Val test = car(args); args = cdr(args);
            if (!first) out(" : ");
            first = false;
            if (val_is_kw(test)) {
                // :else → always true
                emit_expr(car(args), d); args = cdr(args);
            } else {
                emit_expr(test, d); out(" ? "); emit_expr(car(args), d);
                args = cdr(args);
            }
        }
        if (first) out("0");
        else out(" : 0");
        out(")");
        return;
    }

    if (sym == S_WHEN) {
        out("("); emit_expr(car(args), d); out(" ? ({ ");
        args = cdr(args);
        while (val_is_cons(args)) { emit_expr(car(args), d); out("; "); args = cdr(args); }
        out("}) : 0)");
        return;
    }

    if (sym == S_ADD || sym == S_SUB || sym == S_MUL || sym == S_DIV ||
        sym == S_MOD || sym == S_EQ  || sym == S_LT  || sym == S_GT ||
        sym == S_LTE || sym == S_GTE) { emit_binop(sym, args, d); return; }

    if (sym == S_NOT)   { out("(!("); emit_expr(car(args), d); out("))"); return; }
    if (sym == S_INC)   { out("(("); emit_expr(car(args), d); out(") + 1)"); return; }
    if (sym == S_DEC)   { out("(("); emit_expr(car(args), d); out(") - 1)"); return; }
    if (sym == S_ZEROQ) { out("(("); emit_expr(car(args), d); out(") == 0)"); return; }
    if (sym == S_POSQ)  { out("(("); emit_expr(car(args), d); out(") > 0)"); return; }
    if (sym == S_NEGQ)  { out("(("); emit_expr(car(args), d); out(") < 0)"); return; }
    if (sym == S_PRINTLN) { emit_println(args, d); return; }

    out("clj_"); emit_cname(sym); out("(");
    bool first = true;
    while (val_is_cons(args)) {
        if (!first) out(", ");
        emit_expr(car(args), d); args = cdr(args); first = false;
    }
    out(")");
}

// ============================================================================
// 3. Program Emitter
// ============================================================================

static void emit_defn(DefnInfo *fn) {
    out("i64 clj_"); emit_cname(fn->name); out("(");
    Val p = fn->params; bool first = true;
    while (val_is_cons(p)) {
        if (!first) out(", "); out("i64 clj_"); emit_cname(val_as_sym(car(p)));
        p = cdr(p); first = false;
    }
    out(") {\n");

    bool tco = has_recur(fn->body);
    if (tco) {
        g_loop_n = 0;
        g_loop_rid = g_temp_id++;
        p = fn->params;
        while (val_is_cons(p)) {
            g_loop_vars[g_loop_n] = val_as_sym(car(p));
            g_loop_tids[g_loop_n] = g_temp_id++;
            g_loop_n++; p = cdr(p);
        }
        out("    i64 _r%u; ", g_loop_rid);
        for (u32 i = 0; i < g_loop_n; i++) out("i64 _t%u; ", g_loop_tids[i]);
        out("\n    while(1) { ");
        Val body = fn->body;
        while (val_is_cons(body) && val_is_cons(cdr(body))) {
            emit_expr(car(body), 2); out("; "); body = cdr(body);
        }
        if (val_is_cons(body)) emit_tail(car(body), 2);
        out("; }\n    return _r%u;\n", g_loop_rid);
    } else {
        out("    return ");
        Val body = fn->body;
        if (list_len(body) == 1) {
            emit_expr(car(body), 1);
        } else {
            out("({ ");
            while (val_is_cons(body)) { emit_expr(car(body), 2); out("; "); body = cdr(body); }
            out("})");
        }
        out(";\n");
    }
    out("}\n\n");
}

static void emit_program(void) {
    out_reset();
    g_temp_id = 0;
    out("/* Generated by clj — Clojure-to-C compiler */\n");
    out("#include <stdio.h>\n#include <stdint.h>\ntypedef int64_t i64;\n\n");

    for (u32 i = 0; i < g_defn_count; i++) {
        out("i64 clj_"); emit_cname(g_defns[i].name); out("(");
        for (u32 j = 0; j < g_defns[i].n_params; j++) { if (j) out(", "); out("i64"); }
        out(");\n");
    }
    if (g_defn_count) out("\n");

    for (u32 i = 0; i < g_def_count; i++) {
        out("static i64 clj_"); emit_cname(g_defs[i].name); out(";\n");
    }
    if (g_def_count) out("\n");

    for (u32 i = 0; i < g_defn_count; i++) emit_defn(&g_defns[i]);

    out("int main(void) {\n");
    for (u32 i = 0; i < g_def_count; i++) {
        out("    clj_"); emit_cname(g_defs[i].name); out(" = ");
        emit_expr(g_defs[i].value, 1); out(";\n");
    }
    for (u32 i = 0; i < g_main_count; i++) {
        out("    "); emit_expr(g_mains[i], 1); out(";\n");
    }
    out("    return 0;\n}\n");
}

// ============================================================================
// 4. GCC Driver — compile + capture output
// ============================================================================

static char g_captured[65536];
static u32 g_captured_len;

static int compile_and_capture(const char *source) {
    arena_reset(&g_req);
    classify(source);
    emit_program();

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
