/**
 * proto_clj.c — Clojure-to-C Compiler
 *
 * Compiles Clojure source to C, then to native machine code via gcc.
 * No VM. No interpreter. Source → intern → query → emit → compile → run.
 * Free C FFI (output IS C). Self-hosting path: write compiler in Clojure,
 * compile with this bootstrap → self-hosting.
 *
 * Build:
 *   gcc -O3 -march=native -o proto_clj test/proto_clj.c
 *   ./proto_clj                    # tests + benchmarks
 *   ./proto_clj --emit file.clj    # emit C to stdout
 *   ./proto_clj --run file.clj     # compile and run
 */

#define PROTO_BASE_NO_MAIN
#include "proto_base.c"
#include <stdarg.h>

// ============================================================================
// 1. Cons Cells
// ============================================================================

typedef struct { Val car; Val cdr; } Cons;
#define NIL val_nil()

ALWAYS_INLINE Val cons_new(Val a, Val d) {
    Cons *c = arena_push(&g_req, Cons);
    c->car = a; c->cdr = d;
    return val_cons(c);
}
ALWAYS_INLINE Val car(Val v) { return ((Cons *)val_as_cons(v))->car; }
ALWAYS_INLINE Val cdr(Val v) { return ((Cons *)val_as_cons(v))->cdr; }

static u32 list_len(Val v) {
    u32 n = 0;
    while (val_is_cons(v)) { n++; v = cdr(v); }
    return n;
}

// ============================================================================
// 2. Reader — S-expression parser
// ============================================================================

typedef struct { const char *src; u32 pos; u32 len; } Reader;

static void skip_ws(Reader *r) {
    while (r->pos < r->len) {
        char c = r->src[r->pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',') r->pos++;
        else if (c == ';') { while (r->pos < r->len && r->src[r->pos] != '\n') r->pos++; }
        else break;
    }
}

static bool is_sym_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '+' || c == '-' || c == '*' ||
           c == '/' || c == '=' || c == '<' || c == '>' || c == '!' ||
           c == '?' || c == '_' || c == '&' || c == '.' || c == '%';
}

static Val read_form(Reader *r);

static Val read_delimited(Reader *r, char close) {
    r->pos++;
    skip_ws(r);
    if (r->pos < r->len && r->src[r->pos] == close) { r->pos++; return NIL; }
    Val head = NIL;
    while (r->pos < r->len && r->src[r->pos] != close) {
        head = cons_new(read_form(r), head);
        skip_ws(r);
    }
    if (r->pos < r->len) r->pos++;
    // Reverse
    Val result = NIL;
    while (val_is_cons(head)) {
        Val next = cdr(head);
        ((Cons *)val_as_cons(head))->cdr = result;
        result = head; head = next;
    }
    return result;
}

static Val read_atom(Reader *r) {
    // Negative number
    if (r->src[r->pos] == '-' && r->pos + 1 < r->len &&
        r->src[r->pos + 1] >= '0' && r->src[r->pos + 1] <= '9') {
        r->pos++;
        i64 n = 0;
        while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9')
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(-n);
    }
    // Positive number
    if (r->src[r->pos] >= '0' && r->src[r->pos] <= '9') {
        i64 n = 0;
        while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9')
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(n);
    }
    // String literal
    if (r->src[r->pos] == '"') {
        r->pos++;
        u32 ss = r->pos;
        while (r->pos < r->len && r->src[r->pos] != '"') {
            if (r->src[r->pos] == '\\') r->pos++;
            r->pos++;
        }
        Str s = {(u8 *)(r->src + ss), r->pos - ss};
        Str *sp = arena_push(&g_req, Str);
        *sp = str_dup(&g_req, s);
        if (r->pos < r->len) r->pos++;
        return val_str(sp);
    }
    // Symbol
    u32 ss = r->pos;
    while (r->pos < r->len && is_sym_char(r->src[r->pos])) r->pos++;
    if (r->pos == ss) { r->pos++; return NIL; }
    Str s = {(u8 *)(r->src + ss), r->pos - ss};
    if (str_eq(s, STR_LIT("nil")))   return NIL;
    if (str_eq(s, STR_LIT("true")))  return val_true();
    if (str_eq(s, STR_LIT("false"))) return val_false();
    return val_sym(str_intern(s));
}

static Val read_form(Reader *r) {
    skip_ws(r);
    if (r->pos >= r->len) return NIL;
    char c = r->src[r->pos];
    if (c == '(') return read_delimited(r, ')');
    if (c == '[') return read_delimited(r, ']');
    return read_atom(r);
}

// ============================================================================
// 3. Symbols + Output Buffer + Name Mangling
// ============================================================================

static StrId S_DEF, S_DEFN, S_IF, S_LET, S_DO, S_LOOP, S_RECUR;
static StrId S_AND, S_OR;
static StrId S_ADD, S_SUB, S_MUL, S_DIV, S_MOD;
static StrId S_EQ, S_LT, S_GT, S_LTE, S_GTE;
static StrId S_NOT, S_INC, S_DEC, S_PRINTLN;
static StrId S_ZEROQ, S_POSQ, S_NEGQ;

#define INTERN(s) str_intern(STR_LIT(s))
static void init_syms(void) {
    S_DEF = INTERN("def"); S_DEFN = INTERN("defn");
    S_IF = INTERN("if"); S_LET = INTERN("let"); S_DO = INTERN("do");
    S_LOOP = INTERN("loop"); S_RECUR = INTERN("recur");
    S_AND = INTERN("and"); S_OR = INTERN("or");
    S_ADD = INTERN("+"); S_SUB = INTERN("-"); S_MUL = INTERN("*");
    S_DIV = INTERN("/"); S_MOD = INTERN("mod");
    S_EQ = INTERN("="); S_LT = INTERN("<"); S_GT = INTERN(">");
    S_LTE = INTERN("<="); S_GTE = INTERN(">=");
    S_NOT = INTERN("not"); S_INC = INTERN("inc"); S_DEC = INTERN("dec");
    S_PRINTLN = INTERN("println");
    S_ZEROQ = INTERN("zero?"); S_POSQ = INTERN("pos?"); S_NEGQ = INTERN("neg?");
}

static char g_out[1 << 20];
static u32 g_out_pos;
static void out_reset(void) { g_out_pos = 0; g_out[0] = '\0'; }
static void out(const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    g_out_pos += vsnprintf(g_out + g_out_pos, sizeof(g_out) - g_out_pos, fmt, ap);
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
// 4. Form Classification
// ============================================================================

typedef struct { StrId name; Val params; Val body; u32 n_params; } DefnInfo;
typedef struct { StrId name; Val value; } DefInfo;

static DefnInfo g_defns[256]; static u32 g_defn_count;
static DefInfo  g_defs[256];  static u32 g_def_count;
static Val      g_mains[1024]; static u32 g_main_count;

static void classify(const char *source) {
    Reader r = {source, 0, (u32)strlen(source)};
    g_defn_count = g_def_count = g_main_count = 0;
    while (1) {
        skip_ws(&r);
        if (r.pos >= r.len) break;
        Val form = read_form(&r);
        if (val_is_cons(form) && val_is_sym(car(form))) {
            StrId sym = val_as_sym(car(form));
            if (sym == S_DEFN) {
                DefnInfo *d = &g_defns[g_defn_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->params = car(cdr(cdr(form)));
                d->body = cdr(cdr(cdr(form)));
                d->n_params = list_len(d->params);
                continue;
            }
            if (sym == S_DEF) {
                DefInfo *d = &g_defs[g_def_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->value = car(cdr(cdr(form)));
                continue;
            }
        }
        g_mains[g_main_count++] = form;
    }
}

// ============================================================================
// 5. Expression Emitter
// ============================================================================

static u32 g_temp_id;

// Loop state (saved/restored for nesting)
static StrId g_loop_vars[32];
static u32   g_loop_tids[32];
static u32   g_loop_n;
static u32   g_loop_rid;

static void emit_expr(Val form, int d);
static void emit_tail(Val form, int d);

static bool has_recur_outside_loop(Val form) {
    if (!val_is_cons(form)) return false;
    Val h = car(form);
    if (val_is_sym(h)) {
        if (val_as_sym(h) == S_RECUR) return true;
        if (val_as_sym(h) == S_LOOP) return false;
    }
    Val f = form;
    while (val_is_cons(f)) {
        if (has_recur_outside_loop(car(f))) return true;
        f = cdr(f);
    }
    return false;
}

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
    if (n == 1 && op == S_SUB) {
        out("(-("); emit_expr(car(args), d); out("))"); return;
    }
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
    Val bindings = car(args);
    while (val_is_cons(bindings)) {
        out("i64 clj_"); emit_cname(val_as_sym(car(bindings)));
        bindings = cdr(bindings);
        out(" = "); emit_expr(car(bindings), d+1); out("; ");
        bindings = cdr(bindings);
    }
    Val body = cdr(args);
    while (val_is_cons(body)) {
        emit_expr(car(body), d+1);
        body = cdr(body);
        out("; ");
    }
    out("})");
}

static void emit_do(Val args, int d) {
    out("({ ");
    while (val_is_cons(args)) {
        emit_expr(car(args), d); out("; ");
        args = cdr(args);
    }
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
            Val bindings = car(args);
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
    // Save parent loop state
    StrId sv[32]; u32 st[32]; u32 sc = g_loop_n; u32 sr = g_loop_rid;
    memcpy(sv, g_loop_vars, sc * sizeof(StrId));
    memcpy(st, g_loop_tids, sc * sizeof(u32));

    g_loop_n = 0;
    g_loop_rid = g_temp_id++;
    Val bindings = car(args);
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

    // Restore
    g_loop_n = sc; g_loop_rid = sr;
    memcpy(g_loop_vars, sv, sc * sizeof(StrId));
    memcpy(g_loop_tids, st, sc * sizeof(u32));
}

// --- Main expression dispatcher ---

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

    // User function call
    out("clj_"); emit_cname(sym); out("(");
    bool first = true;
    while (val_is_cons(args)) {
        if (!first) out(", ");
        emit_expr(car(args), d); args = cdr(args); first = false;
    }
    out(")");
}

// ============================================================================
// 6. Program Emitter
// ============================================================================

static void emit_defn(DefnInfo *fn) {
    out("i64 clj_"); emit_cname(fn->name); out("(");
    Val p = fn->params; bool first = true;
    while (val_is_cons(p)) {
        if (!first) out(", "); out("i64 clj_"); emit_cname(val_as_sym(car(p)));
        p = cdr(p); first = false;
    }
    out(") {\n");

    bool tco = has_recur_outside_loop(fn->body);
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
            while (val_is_cons(body)) {
                emit_expr(car(body), 2); out("; "); body = cdr(body);
            }
            out("})");
        }
        out(";\n");
    }
    out("}\n\n");
}

static void emit_program(void) {
    out_reset();
    g_temp_id = 0;

    out("/* Generated by proto_clj — Clojure-to-C compiler */\n");
    out("#include <stdio.h>\n#include <stdint.h>\ntypedef int64_t i64;\n\n");

    // Forward declarations
    for (u32 i = 0; i < g_defn_count; i++) {
        out("i64 clj_"); emit_cname(g_defns[i].name); out("(");
        for (u32 j = 0; j < g_defns[i].n_params; j++) { if (j) out(", "); out("i64"); }
        out(");\n");
    }
    if (g_defn_count) out("\n");

    // Global defs
    for (u32 i = 0; i < g_def_count; i++) {
        out("static i64 clj_"); emit_cname(g_defs[i].name); out(";\n");
    }
    if (g_def_count) out("\n");

    // Function implementations
    for (u32 i = 0; i < g_defn_count; i++) emit_defn(&g_defns[i]);

    // Main
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
// 7. Driver
// ============================================================================

static char g_captured[65536];

static int compile_and_capture(const char *source) {
    arena_reset(&g_req);
    classify(source);
    emit_program();

    FILE *f = fopen("/tmp/_clj_out.c", "w");
    if (!f) return -1;
    fwrite(g_out, 1, g_out_pos, f);
    fclose(f);

    int rc = system("gcc -O2 -w -o /tmp/_clj_out /tmp/_clj_out.c 2>/dev/null");
    if (rc != 0) {
        snprintf(g_captured, sizeof(g_captured), "COMPILE_ERROR");
        return -1;
    }

    FILE *p = popen("/tmp/_clj_out 2>&1", "r");
    if (!p) return -1;
    u32 n = (u32)fread(g_captured, 1, sizeof(g_captured) - 1, p);
    g_captured[n] = '\0';
    while (n > 0 && g_captured[n-1] == '\n') g_captured[--n] = '\0';
    return pclose(p);
}

// ============================================================================
// 8. Tests
// ============================================================================

static void test_e2e(const char *name, const char *source, const char *expected) {
    TEST(name);
    int rc = compile_and_capture(source);
    if (rc != 0 && strcmp(g_captured, "COMPILE_ERROR") == 0) {
        printf("FAIL (compile error)\n");
        printf("    source: %s\n", source);
        // Show first 500 chars of generated C
        printf("    C output (first 500):\n%.500s\n", g_out);
        g_fail++; return;
    }
    if (strcmp(g_captured, expected) == 0) { PASS(); }
    else {
        printf("FAIL\n    expected: '%s'\n    got:      '%s'\n", expected, g_captured);
        g_fail++;
    }
}

static void test_compile(void) {
    printf("\n=== Arithmetic ===\n");
    test_e2e("(+ 1 2)", "(println (+ 1 2))", "3");
    test_e2e("(* 6 7)", "(println (* 6 7))", "42");
    test_e2e("(- 10 3)", "(println (- 10 3))", "7");
    test_e2e("(/ 10 3)", "(println (/ 10 3))", "3");
    test_e2e("(mod 10 3)", "(println (mod 10 3))", "1");
    test_e2e("(- 5) unary", "(println (- 5))", "-5");
    test_e2e("nested arith", "(println (+ (* 2 3) (- 10 4)))", "12");
    test_e2e("variadic +", "(println (+ 1 2 3 4 5))", "15");

    printf("\n=== Comparison ===\n");
    test_e2e("(< 1 2)", "(println (< 1 2))", "1");
    test_e2e("(> 1 2)", "(println (> 1 2))", "0");
    test_e2e("(= 42 42)", "(println (= 42 42))", "1");
    test_e2e("(<= 5 5)", "(println (<= 5 5))", "1");
    test_e2e("(not 0)", "(println (not 0))", "1");
    test_e2e("(not 42)", "(println (not 42))", "0");

    printf("\n=== Control Flow ===\n");
    test_e2e("if true", "(println (if 1 10 20))", "10");
    test_e2e("if false", "(println (if 0 10 20))", "20");
    test_e2e("let binding", "(println (let [a 1 b 2] (+ a b)))", "3");
    test_e2e("nested let", "(println (let [a 1] (let [b (+ a 1)] (* a b))))", "2");
    test_e2e("do form", "(println (do 1 2 42))", "42");
    test_e2e("and truthy", "(println (and 1 2 3))", "3");
    test_e2e("and falsy", "(println (and 1 0 3))", "0");
    test_e2e("or falsy-first", "(println (or 0 0 42))", "42");
    test_e2e("or truthy-first", "(println (or 7 0 42))", "7");

    printf("\n=== Def ===\n");
    test_e2e("def global", "(def x 42) (println x)", "42");
    test_e2e("def computed", "(def x (+ 20 22)) (println x)", "42");

    printf("\n=== Functions ===\n");
    test_e2e("defn + call",
        "(defn double [x] (* x 2)) (println (double 21))", "42");
    test_e2e("multiple defns",
        "(defn double [x] (* x 2))\n"
        "(defn quad [x] (double (double x)))\n"
        "(println (quad 10))", "40");
    test_e2e("recursive factorial",
        "(defn fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))\n"
        "(println (fact 10))", "3628800");
    test_e2e("recursive fibonacci",
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(println (fib 10))", "55");
    test_e2e("defn with let",
        "(defn calc [x y] (let [tmp (+ x y)] (* tmp 2)))\n"
        "(println (calc 10 11))", "42");

    printf("\n=== TCO (defn recur) ===\n");
    test_e2e("tail-recursive fact",
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n"
        "(println (fact 10 1))", "3628800");
    test_e2e("tail-recursive sum",
        "(defn sum-to [n acc] (if (<= n 0) acc (recur (- n 1) (+ acc n))))\n"
        "(println (sum-to 100 0))", "5050");

    printf("\n=== Loop/Recur ===\n");
    test_e2e("loop sum 0..9",
        "(println (loop [i 0 s 0] (if (>= i 10) s (recur (+ i 1) (+ s i)))))", "45");
    test_e2e("loop countdown",
        "(println (loop [n 10] (if (<= n 0) 0 (recur (- n 1)))))", "0");
    test_e2e("loop with let in body",
        "(println (loop [i 0 s 0]\n"
        "  (if (>= i 5) s\n"
        "    (let [sq (* i i)] (recur (+ i 1) (+ s sq))))))", "30");

    printf("\n=== String Literals ===\n");
    test_e2e("println string", "(println \"hello\")", "hello");
    test_e2e("println mixed", "(println \"result:\" (+ 40 2))", "result: 42");

    printf("\n=== Predicates ===\n");
    test_e2e("zero?", "(println (zero? 0))", "1");
    test_e2e("pos?", "(println (pos? 42))", "1");
    test_e2e("neg?", "(println (neg? -1))", "1");
    test_e2e("inc", "(println (inc 41))", "42");
    test_e2e("dec", "(println (dec 43))", "42");
}

// ============================================================================
// 9. Benchmarks
// ============================================================================

static void bench_compile(void) {
    printf("\n--- bench: Compilation ---\n");

    const char *fib_src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(println (fib 35))";

    // Time: source → classify → emit
    int N = 10000;
    u64 t0 = now_ns();
    for (int i = 0; i < N; i++) {
        arena_reset(&g_req);
        classify(fib_src);
        emit_program();
    }
    u64 dt = now_ns() - t0;
    printf("  source→C emit:     %6.1f us/op  (%d iters)\n", (double)dt / N / 1000.0, N);
    printf("  C output size:     %u bytes\n", g_out_pos);

    // Time: full compile (emit + gcc)
    t0 = now_ns();
    arena_reset(&g_req);
    classify(fib_src);
    emit_program();
    FILE *f = fopen("/tmp/_clj_out.c", "w");
    fwrite(g_out, 1, g_out_pos, f);
    fclose(f);
    system("gcc -O2 -w -o /tmp/_clj_out /tmp/_clj_out.c 2>/dev/null");
    dt = now_ns() - t0;
    printf("  source→binary:     %6.1f ms  (includes gcc)\n", (double)dt / 1e6);

    // Time: run compiled fib(35) vs interpreted
    t0 = now_ns();
    system("/tmp/_clj_out >/dev/null");
    dt = now_ns() - t0;
    printf("  fib(35) native:    %6.1f ms\n", (double)dt / 1e6);
}

// ============================================================================
// 10. Main
// ============================================================================

static char *read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) { fprintf(stderr, "Cannot open: %s\n", path); return NULL; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(sz + 1);
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {
    base_init();
    init_syms();

    if (argc > 2 && strcmp(argv[1], "--emit") == 0) {
        char *src = read_file(argv[2]);
        if (!src) return 1;
        classify(src);
        emit_program();
        fwrite(g_out, 1, g_out_pos, stdout);
        free(src);
        base_cleanup();
        return 0;
    }

    if (argc > 2 && strcmp(argv[1], "--run") == 0) {
        char *src = read_file(argv[2]);
        if (!src) return 1;
        int rc = compile_and_capture(src);
        if (rc != 0) { fprintf(stderr, "Error\n"); free(src); return 1; }
        printf("%s\n", g_captured);
        free(src);
        base_cleanup();
        return 0;
    }

    // Default: tests + benchmarks
    printf("proto_clj.c — Clojure-to-C Compiler\n");
    printf("=====================================\n");
    printf("Pipeline: source → intern → query → emit C → gcc → native\n");
    printf("=====================================\n");

    test_compile();

    printf("\n=====================================\n");
    printf("Tests: %d passed, %d failed\n", g_pass, g_fail);
    printf("=====================================\n");

    bench_compile();

    printf("\n=====================================\n");
    printf("Clojure → C → machine code. Done.\n");
    printf("=====================================\n");

    base_cleanup();
    return g_fail ? 1 : 0;
}
