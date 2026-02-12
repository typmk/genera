/**
 * proto_eval.c — Minimal Clojure Evaluator on proto_base
 *
 * McCarthy's eval/apply grounded in the base layer. Proves arena allocators,
 * NaN-boxed values, and interned symbols support a real language runtime.
 *
 * Features:
 *   - Cons cells (16 bytes, arena-allocated)
 *   - S-expression reader (numbers, symbols, keywords, strings, lists, [vectors])
 *   - Printer (Val -> string)
 *   - Lexical environments (HashMap chain)
 *   - 6 special forms: def, defn, fn, if, let, do, quote, and, or
 *   - 25+ builtins: arithmetic, comparison, list ops, higher-order, I/O
 *   - Closures with lexical capture
 *   - Variadic functions (& rest)
 *
 * Build:
 *   gcc -O3 -march=native -mavx2 -o proto_eval test/proto_eval.c
 *   ./proto_eval              # tests + benchmarks
 *   ./proto_eval --repl       # interactive REPL
 */

#define PROTO_BASE_NO_MAIN
#include "proto_base.c"

// ============================================================================
// 1. Cons Cells — the fundamental
// ============================================================================

typedef struct { Val car; Val cdr; } Cons;

#define NIL val_nil()

ALWAYS_INLINE Val cons_new(Val car, Val cdr) {
    Cons *c = arena_push(&g_req, Cons);
    c->car = car;
    c->cdr = cdr;
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
// 2. Environment — lexical scope via HashMap chain
// ============================================================================

typedef struct Env {
    HashMap bindings;
    struct Env *parent;
} Env;

static Env *env_create(Arena *a, Env *parent) {
    Env *e = arena_push(a, Env);
    e->bindings = hashmap_create(a, 16);
    e->parent = parent;
    return e;
}

static void env_set(Env *e, StrId name, Val value) {
    hashmap_put(&e->bindings, name, value);
}

static bool env_get(Env *e, StrId name, Val *out) {
    while (e) {
        if (hashmap_get(&e->bindings, name, out)) return true;
        e = e->parent;
    }
    return false;
}

// ============================================================================
// 3. Functions — builtins + closures
// ============================================================================

typedef Val (*BuiltinFn)(Val args);

typedef struct {
    u8 type;  // 0=builtin, 1=closure
    union {
        struct { BuiltinFn fn; const char *name; } builtin;
        struct { Val params; Val body; Env *env; } closure;
    };
} FnObj;

#define FN_BUILTIN 0
#define FN_CLOSURE 1

static Val make_builtin(const char *name, BuiltinFn fn) {
    FnObj *f = arena_push(&g_perm, FnObj);
    f->type = FN_BUILTIN;
    f->builtin.fn = fn;
    f->builtin.name = name;
    return val_fn(f);
}

static Val make_closure(Val params, Val body, Env *env) {
    FnObj *f = arena_push(&g_req, FnObj);
    f->type = FN_CLOSURE;
    f->closure.params = params;
    f->closure.body = body;
    f->closure.env = env;
    return val_fn(f);
}

// ============================================================================
// 4. Reader — S-expression parser
// ============================================================================

typedef struct { const char *src; u32 pos; u32 len; } Reader;

static void reader_skip_ws(Reader *r) {
    while (r->pos < r->len) {
        char c = r->src[r->pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',') {
            r->pos++;
        } else if (c == ';') {
            while (r->pos < r->len && r->src[r->pos] != '\n') r->pos++;
        } else break;
    }
}

static bool is_sym_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '+' || c == '-' || c == '*' ||
           c == '/' || c == '=' || c == '<' || c == '>' || c == '!' ||
           c == '?' || c == '_' || c == '&' || c == '.' || c == '%';
}

static Val read_form(Reader *r);

// Read a delimited sequence — works for () and []
static Val read_delimited(Reader *r, char close) {
    r->pos++;  // skip opening delimiter
    reader_skip_ws(r);

    if (r->pos < r->len && r->src[r->pos] == close) {
        r->pos++;
        return NIL;
    }

    // Build in reverse, then flip
    Val head = NIL;
    while (r->pos < r->len && r->src[r->pos] != close) {
        head = cons_new(read_form(r), head);
        reader_skip_ws(r);
    }
    if (r->pos < r->len) r->pos++;  // skip closing

    // Reverse in-place
    Val result = NIL;
    while (val_is_cons(head)) {
        Val next = cdr(head);
        ((Cons *)val_as_cons(head))->cdr = result;
        result = head;
        head = next;
    }
    return result;
}

static Val read_atom(Reader *r) {
    u32 start = r->pos;

    // Negative number? '-' followed by digit
    bool neg = false;
    if (r->src[r->pos] == '-' && r->pos + 1 < r->len &&
        r->src[r->pos + 1] >= '0' && r->src[r->pos + 1] <= '9') {
        neg = true;
        r->pos++;
    }

    // Number
    if (r->src[r->pos] >= '0' && r->src[r->pos] <= '9') {
        i64 n = 0;
        while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9') {
            n = n * 10 + (r->src[r->pos] - '0');
            r->pos++;
        }
        if (r->pos < r->len && r->src[r->pos] == '.') {
            r->pos++;
            f64 frac = 0.1, fn = (f64)n;
            while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9') {
                fn += (r->src[r->pos] - '0') * frac;
                frac *= 0.1;
                r->pos++;
            }
            return val_f64(neg ? -fn : fn);
        }
        return val_int(neg ? -n : n);
    }

    r->pos = start;  // reset if '-' was consumed but no number followed

    // Keyword
    if (r->src[r->pos] == ':') {
        r->pos++;
        u32 kw_start = r->pos;
        while (r->pos < r->len && is_sym_char(r->src[r->pos])) r->pos++;
        return val_kw(str_intern((Str){(u8 *)(r->src + kw_start), r->pos - kw_start}));
    }

    // String
    if (r->src[r->pos] == '"') {
        r->pos++;
        u32 str_start = r->pos;
        while (r->pos < r->len && r->src[r->pos] != '"') {
            if (r->src[r->pos] == '\\') r->pos++;
            r->pos++;
        }
        Str s = {(u8 *)(r->src + str_start), r->pos - str_start};
        Str *sp = arena_push(&g_req, Str);
        *sp = str_dup(&g_req, s);
        if (r->pos < r->len) r->pos++;
        return val_str(sp);
    }

    // Symbol or special literal
    u32 sym_start = r->pos;
    while (r->pos < r->len && is_sym_char(r->src[r->pos])) r->pos++;

    if (r->pos == sym_start) { r->pos++; return NIL; }  // unknown char

    Str s = {(u8 *)(r->src + sym_start), r->pos - sym_start};
    if (str_eq(s, STR_LIT("nil")))   return NIL;
    if (str_eq(s, STR_LIT("true")))  return val_true();
    if (str_eq(s, STR_LIT("false"))) return val_false();
    return val_sym(str_intern(s));
}

static Val read_form(Reader *r) {
    reader_skip_ws(r);
    if (r->pos >= r->len) return NIL;

    char c = r->src[r->pos];
    if (c == '(') return read_delimited(r, ')');
    if (c == '[') return read_delimited(r, ']');
    if (c == '\'') {
        r->pos++;
        StrId q = str_intern(STR_LIT("quote"));
        return cons_new(val_sym(q), cons_new(read_form(r), NIL));
    }
    return read_atom(r);
}

static Val read_str(const char *s) {
    Reader r = {s, 0, (u32)strlen(s)};
    return read_form(&r);
}

// ============================================================================
// 5. Printer — Val -> string
// ============================================================================

static char g_pr_buf[8192];
static u32 g_pr_pos;

static void pr_init(void) { g_pr_pos = 0; g_pr_buf[0] = '\0'; }
static void pr_ch(char c) {
    if (g_pr_pos < sizeof(g_pr_buf) - 1) g_pr_buf[g_pr_pos++] = c;
    g_pr_buf[g_pr_pos] = '\0';
}
static void pr_s(const char *s) { while (*s) pr_ch(*s++); }
static void pr_val(Val v);

static void pr_list(Val v) {
    pr_ch('(');
    bool first = true;
    while (val_is_cons(v)) {
        if (!first) pr_ch(' ');
        first = false;
        pr_val(car(v));
        v = cdr(v);
    }
    if (!val_is_nil(v)) { pr_s(" . "); pr_val(v); }
    pr_ch(')');
}

static void pr_val(Val v) {
    if (val_is_nil(v))  { pr_s("nil"); return; }
    if (val_is_bool(v)) { pr_s(val_as_bool(v) ? "true" : "false"); return; }
    if (val_is_int(v))  { char b[32]; snprintf(b, 32, "%lld", (long long)val_as_int(v)); pr_s(b); return; }
    if (val_is_f64(v))  { char b[32]; snprintf(b, 32, "%g", val_as_f64(v)); pr_s(b); return; }
    if (val_is_sym(v))  { Str s = str_from_id(val_as_sym(v)); for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]); return; }
    if (val_is_kw(v))   { pr_ch(':'); Str s = str_from_id(val_as_kw(v)); for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]); return; }
    if (val_is_str(v))  { pr_ch('"'); Str *s = val_as_str(v); for (u32 i = 0; i < s->len; i++) pr_ch(s->data[i]); pr_ch('"'); return; }
    if (val_is_cons(v)) { pr_list(v); return; }
    if (val_is_fn(v))   {
        FnObj *f = (FnObj *)val_as_fn(v);
        if (f->type == FN_BUILTIN) { pr_s("#<"); pr_s(f->builtin.name); pr_ch('>'); }
        else pr_s("#<fn>");
        return;
    }
    pr_s("#<unknown>");
}

static const char *print_val(Val v) { pr_init(); pr_val(v); return g_pr_buf; }

// ============================================================================
// 6. Eval / Apply
// ============================================================================

static Val eval(Val form, Env *env);
static Env *g_global_env;

// Well-known symbols (initialized once)
static StrId SYM_DEF, SYM_FN, SYM_IF, SYM_DO, SYM_LET, SYM_QUOTE;
static StrId SYM_DEFN, SYM_AND, SYM_OR;

static void init_symbols(void) {
    SYM_DEF   = str_intern(STR_LIT("def"));
    SYM_FN    = str_intern(STR_LIT("fn"));
    SYM_IF    = str_intern(STR_LIT("if"));
    SYM_DO    = str_intern(STR_LIT("do"));
    SYM_LET   = str_intern(STR_LIT("let"));
    SYM_QUOTE = str_intern(STR_LIT("quote"));
    SYM_DEFN  = str_intern(STR_LIT("defn"));
    SYM_AND   = str_intern(STR_LIT("and"));
    SYM_OR    = str_intern(STR_LIT("or"));
}

// Eval a sequence of forms, return last result
static Val eval_body(Val forms, Env *env) {
    Val result = NIL;
    while (val_is_cons(forms)) {
        result = eval(car(forms), env);
        forms = cdr(forms);
    }
    return result;
}

// Eval each element, return new cons list
static Val eval_list(Val forms, Env *env) {
    if (!val_is_cons(forms)) return NIL;
    Val head = eval(car(forms), env);
    return cons_new(head, eval_list(cdr(forms), env));
}

static Val apply_fn(Val fn_val, Val args) {
    FnObj *f = (FnObj *)val_as_fn(fn_val);

    if (f->type == FN_BUILTIN)
        return f->builtin.fn(args);

    // Closure: bind params in new env
    Env *call_env = env_create(&g_req, f->closure.env);
    Val params = f->closure.params;

    while (val_is_cons(params) && val_is_cons(args)) {
        StrId name = val_as_sym(car(params));
        // & rest
        Str name_str = str_from_id(name);
        if (name_str.len == 1 && name_str.data[0] == '&') {
            params = cdr(params);
            if (val_is_cons(params))
                env_set(call_env, val_as_sym(car(params)), args);
            break;
        }
        env_set(call_env, name, car(args));
        params = cdr(params);
        args = cdr(args);
    }

    return eval_body(f->closure.body, call_env);
}

static Val eval(Val form, Env *env) {
    // Self-evaluating
    if (val_is_nil(form) || val_is_bool(form) || val_is_int(form) ||
        val_is_f64(form) || val_is_kw(form) || val_is_str(form) || val_is_fn(form))
        return form;

    // Symbol lookup
    if (val_is_sym(form)) {
        Val result;
        if (env_get(env, val_as_sym(form), &result)) return result;
        Str s = str_from_id(val_as_sym(form));
        printf("ERROR: undefined '%.*s'\n", s.len, s.data);
        return NIL;
    }

    // List — special forms or call
    if (!val_is_cons(form)) return form;

    Val head = car(form);
    Val rest = cdr(form);

    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);

        if (sym == SYM_QUOTE) return car(rest);

        if (sym == SYM_DEF) {
            StrId name = val_as_sym(car(rest));
            Val value = eval(car(cdr(rest)), env);
            env_set(g_global_env, name, value);
            return value;
        }

        if (sym == SYM_DEFN) {
            StrId name = val_as_sym(car(rest));
            Val params = car(cdr(rest));
            Val body = cdr(cdr(rest));  // implicit do
            Val fn = make_closure(params, body, env);
            env_set(g_global_env, name, fn);
            return fn;
        }

        if (sym == SYM_FN) {
            Val params = car(rest);
            Val body = cdr(rest);  // implicit do
            return make_closure(params, body, env);
        }

        if (sym == SYM_IF) {
            Val cond = eval(car(rest), env);
            if (val_truthy(cond))
                return eval(car(cdr(rest)), env);
            Val else_branch = cdr(cdr(rest));
            return val_is_cons(else_branch) ? eval(car(else_branch), env) : NIL;
        }

        if (sym == SYM_DO) return eval_body(rest, env);

        if (sym == SYM_LET) {
            Env *let_env = env_create(&g_req, env);
            Val bindings = car(rest);
            while (val_is_cons(bindings)) {
                StrId name = val_as_sym(car(bindings));
                bindings = cdr(bindings);
                env_set(let_env, name, eval(car(bindings), let_env));
                bindings = cdr(bindings);
            }
            return eval_body(cdr(rest), let_env);
        }

        if (sym == SYM_AND) {
            Val result = val_true();
            while (val_is_cons(rest)) {
                result = eval(car(rest), env);
                if (!val_truthy(result)) return result;
                rest = cdr(rest);
            }
            return result;
        }

        if (sym == SYM_OR) {
            while (val_is_cons(rest)) {
                Val result = eval(car(rest), env);
                if (val_truthy(result)) return result;
                rest = cdr(rest);
            }
            return NIL;
        }
    }

    // Function call
    Val fn_val = eval(head, env);
    if (!val_is_fn(fn_val)) {
        printf("ERROR: not callable: %s\n", print_val(head));
        return NIL;
    }
    return apply_fn(fn_val, eval_list(rest, env));
}

// Convenience: read string, eval all forms, return last
static Val eval_string(const char *s, Env *env) {
    Reader r = {s, 0, (u32)strlen(s)};
    Val result = NIL;
    while (1) {
        reader_skip_ws(&r);
        if (r.pos >= r.len) break;
        result = eval(read_form(&r), env);
    }
    return result;
}

// ============================================================================
// 7. Built-in Functions
// ============================================================================

// --- Arithmetic ---
static Val bi_add(Val args) {
    i64 sum = 0;
    bool is_float = false;
    f64 fsum = 0.0;
    while (val_is_cons(args)) {
        Val v = car(args);
        if (val_is_f64(v)) { is_float = true; fsum += val_as_f64(v); }
        else { sum += val_as_int(v); fsum += (f64)val_as_int(v); }
        args = cdr(args);
    }
    return is_float ? val_f64(fsum) : val_int(sum);
}

static Val bi_sub(Val args) {
    if (!val_is_cons(args)) return val_int(0);
    i64 r = val_as_int(car(args));
    args = cdr(args);
    if (!val_is_cons(args)) return val_int(-r);
    while (val_is_cons(args)) { r -= val_as_int(car(args)); args = cdr(args); }
    return val_int(r);
}

static Val bi_mul(Val args) {
    i64 p = 1;
    while (val_is_cons(args)) { p *= val_as_int(car(args)); args = cdr(args); }
    return val_int(p);
}

static Val bi_div(Val args) {
    if (!val_is_cons(args)) return val_int(0);
    i64 r = val_as_int(car(args));
    args = cdr(args);
    while (val_is_cons(args)) {
        i64 d = val_as_int(car(args));
        if (d == 0) { printf("ERROR: /0\n"); return NIL; }
        r /= d; args = cdr(args);
    }
    return val_int(r);
}

static Val bi_mod(Val args) {
    i64 a = val_as_int(car(args)), b = val_as_int(car(cdr(args)));
    return b ? val_int(a % b) : NIL;
}

// --- Comparison ---
static Val bi_eq(Val args) {
    Val a = car(args), b = car(cdr(args));
    if (val_is_int(a) && val_is_int(b)) return val_bool(val_as_int(a) == val_as_int(b));
    if (val_is_nil(a) && val_is_nil(b)) return val_true();
    if (val_is_bool(a) && val_is_bool(b)) return val_bool(val_as_bool(a) == val_as_bool(b));
    if (val_is_sym(a) && val_is_sym(b)) return val_bool(val_as_sym(a) == val_as_sym(b));
    if (val_is_kw(a) && val_is_kw(b)) return val_bool(val_as_kw(a) == val_as_kw(b));
    return val_bool(a == b);
}
static Val bi_lt(Val args)  { return val_bool(val_as_int(car(args)) < val_as_int(car(cdr(args)))); }
static Val bi_gt(Val args)  { return val_bool(val_as_int(car(args)) > val_as_int(car(cdr(args)))); }
static Val bi_lte(Val args) { return val_bool(val_as_int(car(args)) <= val_as_int(car(cdr(args)))); }
static Val bi_gte(Val args) { return val_bool(val_as_int(car(args)) >= val_as_int(car(cdr(args)))); }

// --- List ops ---
static Val bi_cons(Val args)   { return cons_new(car(args), car(cdr(args))); }
static Val bi_first(Val args)  { Val v = car(args); return val_is_cons(v) ? car(v) : NIL; }
static Val bi_rest(Val args)   { Val v = car(args); return val_is_cons(v) ? cdr(v) : NIL; }
static Val bi_list(Val args)   { return args; }
static Val bi_count(Val args)  { return val_int(list_len(car(args))); }
static Val bi_nilq(Val args)   { return val_bool(val_is_nil(car(args))); }
static Val bi_not(Val args)    { return val_bool(!val_truthy(car(args))); }
static Val bi_consq(Val args)  { return val_bool(val_is_cons(car(args))); }

// --- Type predicates ---
static Val bi_intq(Val args)   { return val_bool(val_is_int(car(args))); }
static Val bi_fnq(Val args)    { return val_bool(val_is_fn(car(args))); }
static Val bi_symq(Val args)   { return val_bool(val_is_sym(car(args))); }

// --- I/O ---
static Val bi_println(Val args) {
    bool first = true;
    while (val_is_cons(args)) {
        if (!first) putchar(' ');
        first = false;
        Val v = car(args);
        if (val_is_str(v)) { Str *s = val_as_str(v); fwrite(s->data, 1, s->len, stdout); }
        else printf("%s", print_val(v));
        args = cdr(args);
    }
    putchar('\n');
    return NIL;
}

static Val bi_prstr(Val args) {
    pr_init();
    bool first = true;
    while (val_is_cons(args)) {
        if (!first) pr_ch(' ');
        first = false;
        pr_val(car(args));
        args = cdr(args);
    }
    Str *sp = arena_push(&g_req, Str);
    *sp = str_dup(&g_req, (Str){(u8 *)g_pr_buf, g_pr_pos});
    return val_str(sp);
}

// --- Higher-order ---
static Val bi_map(Val args) {
    Val fn = car(args), lst = car(cdr(args));
    Val result = NIL, *tail = &result;
    while (val_is_cons(lst)) {
        Val cell = cons_new(apply_fn(fn, cons_new(car(lst), NIL)), NIL);
        *tail = cell;
        tail = &((Cons *)val_as_cons(cell))->cdr;
        lst = cdr(lst);
    }
    return result;
}

static Val bi_filter(Val args) {
    Val fn = car(args), lst = car(cdr(args));
    Val result = NIL, *tail = &result;
    while (val_is_cons(lst)) {
        if (val_truthy(apply_fn(fn, cons_new(car(lst), NIL)))) {
            Val cell = cons_new(car(lst), NIL);
            *tail = cell;
            tail = &((Cons *)val_as_cons(cell))->cdr;
        }
        lst = cdr(lst);
    }
    return result;
}

static Val bi_reduce(Val args) {
    Val fn = car(args), init = car(cdr(args)), lst = car(cdr(cdr(args)));
    Val acc = init;
    while (val_is_cons(lst)) {
        acc = apply_fn(fn, cons_new(acc, cons_new(car(lst), NIL)));
        lst = cdr(lst);
    }
    return acc;
}

static Val bi_apply(Val args) {
    return apply_fn(car(args), car(cdr(args)));
}

// --- Register all ---
static void register_builtins(Env *env) {
    #define REG(n, f) env_set(env, str_intern(STR_LIT(n)), make_builtin(n, f))
    REG("+", bi_add);  REG("-", bi_sub);  REG("*", bi_mul);  REG("/", bi_div);
    REG("mod", bi_mod);
    REG("=", bi_eq);  REG("<", bi_lt);  REG(">", bi_gt);
    REG("<=", bi_lte);  REG(">=", bi_gte);
    REG("cons", bi_cons);  REG("first", bi_first);  REG("rest", bi_rest);
    REG("list", bi_list);  REG("count", bi_count);
    REG("nil?", bi_nilq);  REG("not", bi_not);  REG("cons?", bi_consq);
    REG("int?", bi_intq);  REG("fn?", bi_fnq);  REG("symbol?", bi_symq);
    REG("println", bi_println);  REG("pr-str", bi_prstr);
    REG("map", bi_map);  REG("filter", bi_filter);
    REG("reduce", bi_reduce);  REG("apply", bi_apply);
    #undef REG
}

// ============================================================================
// 8. Tests
// ============================================================================

#define EVAL(s) eval_string(s, g_global_env)
#define VAL_EQ_INT(v, n) (val_is_int(v) && val_as_int(v) == (n))
#define VAL_EQ_BOOL(v, b) (val_is_bool(v) && val_as_bool(v) == (b))

static void test_reader(void) {
    printf("\n=== Reader ===\n");

    TEST("integer");
    CHECK(VAL_EQ_INT(read_str("42"), 42));

    TEST("negative integer");
    CHECK(VAL_EQ_INT(read_str("-7"), -7));

    TEST("float");
    Val fv = read_str("3.14");
    CHECK(val_is_f64(fv) && val_as_f64(fv) > 3.13 && val_as_f64(fv) < 3.15);

    TEST("symbol");
    Val sv = read_str("hello");
    CHECK(val_is_sym(sv));

    TEST("keyword");
    Val kv = read_str(":name");
    CHECK(val_is_kw(kv));

    TEST("string");
    Val strv = read_str("\"hello\"");
    CHECK(val_is_str(strv));

    TEST("nil / true / false");
    CHECK(val_is_nil(read_str("nil")) && VAL_EQ_BOOL(read_str("true"), true) &&
          VAL_EQ_BOOL(read_str("false"), false));

    TEST("empty list");
    CHECK(val_is_nil(read_str("()")));

    TEST("simple list (+ 1 2)");
    Val lst = read_str("(+ 1 2)");
    CHECK(val_is_cons(lst) && list_len(lst) == 3);

    TEST("nested list");
    Val nested = read_str("(+ (* 2 3) 4)");
    CHECK(val_is_cons(nested) && val_is_cons(car(cdr(nested))));

    TEST("vector syntax [1 2 3]");
    Val vec = read_str("[1 2 3]");
    CHECK(val_is_cons(vec) && list_len(vec) == 3);

    TEST("quote 'x");
    Val q = read_str("'x");
    CHECK(val_is_cons(q) && val_is_sym(car(q)));
}

static void test_eval_basics(void) {
    printf("\n=== Eval Basics ===\n");

    TEST("integer self-eval");
    CHECK(VAL_EQ_INT(EVAL("42"), 42));

    TEST("(+ 1 2)");
    CHECK(VAL_EQ_INT(EVAL("(+ 1 2)"), 3));

    TEST("(+ 1 2 3 4 5)");
    CHECK(VAL_EQ_INT(EVAL("(+ 1 2 3 4 5)"), 15));

    TEST("(- 10 3)");
    CHECK(VAL_EQ_INT(EVAL("(- 10 3)"), 7));

    TEST("(- 5) unary");
    CHECK(VAL_EQ_INT(EVAL("(- 5)"), -5));

    TEST("(* 6 7)");
    CHECK(VAL_EQ_INT(EVAL("(* 6 7)"), 42));

    TEST("(/ 10 3)");
    CHECK(VAL_EQ_INT(EVAL("(/ 10 3)"), 3));

    TEST("(mod 10 3)");
    CHECK(VAL_EQ_INT(EVAL("(mod 10 3)"), 1));

    TEST("nested arith (+ (* 2 3) (- 10 6))");
    CHECK(VAL_EQ_INT(EVAL("(+ (* 2 3) (- 10 6))"), 10));

    TEST("(= 1 1) / (= 1 2)");
    CHECK(VAL_EQ_BOOL(EVAL("(= 1 1)"), true) && VAL_EQ_BOOL(EVAL("(= 1 2)"), false));

    TEST("(< 1 2) / (> 1 2)");
    CHECK(VAL_EQ_BOOL(EVAL("(< 1 2)"), true) && VAL_EQ_BOOL(EVAL("(> 1 2)"), false));

    TEST("(not true) / (not false) / (not nil)");
    CHECK(VAL_EQ_BOOL(EVAL("(not true)"), false) && VAL_EQ_BOOL(EVAL("(not nil)"), true));
}

static void test_special_forms(void) {
    printf("\n=== Special Forms ===\n");

    TEST("(quote x)");
    CHECK(val_is_sym(EVAL("(quote x)")));

    TEST("'x sugar");
    CHECK(val_is_sym(EVAL("'x")));

    TEST("(if true 1 2)");
    CHECK(VAL_EQ_INT(EVAL("(if true 1 2)"), 1));

    TEST("(if false 1 2)");
    CHECK(VAL_EQ_INT(EVAL("(if false 1 2)"), 2));

    TEST("(if nil 1 2)");
    CHECK(VAL_EQ_INT(EVAL("(if nil 1 2)"), 2));

    TEST("(if 0 1 2) — 0 is truthy");
    CHECK(VAL_EQ_INT(EVAL("(if 0 1 2)"), 1));

    TEST("(do 1 2 3)");
    CHECK(VAL_EQ_INT(EVAL("(do 1 2 3)"), 3));

    TEST("(def x 42) (+ x 8)");
    CHECK(VAL_EQ_INT(EVAL("(def x 42) (+ x 8)"), 50));

    TEST("(let (a 1 b 2) (+ a b))");
    CHECK(VAL_EQ_INT(EVAL("(let (a 1 b 2) (+ a b))"), 3));

    TEST("let scoping — inner shadows, outer unchanged");
    EVAL("(def y 10)");
    Val let_inner = EVAL("(let (y 99) y)");
    Val y_outer = EVAL("y");
    CHECK(VAL_EQ_INT(let_inner, 99) && VAL_EQ_INT(y_outer, 10));

    TEST("(and true true)");
    CHECK(VAL_EQ_BOOL(EVAL("(and true true)"), true));

    TEST("(and true false)");
    CHECK(VAL_EQ_BOOL(EVAL("(and true false)"), false));

    TEST("(or false 42)");
    CHECK(VAL_EQ_INT(EVAL("(or false 42)"), 42));

    TEST("(or false nil)");
    CHECK(val_is_nil(EVAL("(or false nil)")));
}

static void test_functions(void) {
    printf("\n=== Functions ===\n");

    TEST("(fn [x] (+ x 1)) call");
    CHECK(VAL_EQ_INT(EVAL("((fn [x] (+ x 1)) 41)"), 42));

    TEST("defn + call");
    EVAL("(defn double [x] (* x 2))");
    CHECK(VAL_EQ_INT(EVAL("(double 21)"), 42));

    TEST("closure captures env");
    EVAL("(def make-adder (fn [n] (fn [x] (+ n x))))");
    EVAL("(def add5 (make-adder 5))");
    CHECK(VAL_EQ_INT(EVAL("(add5 37)"), 42));

    TEST("implicit do in fn body");
    EVAL("(defn multi [x] (def _tmp 1) (+ x _tmp))");
    CHECK(VAL_EQ_INT(EVAL("(multi 41)"), 42));

    TEST("variadic & rest");
    EVAL("(defn sum-all [& xs] (reduce + 0 xs))");
    CHECK(VAL_EQ_INT(EVAL("(sum-all 1 2 3 4)"), 10));

    TEST("recursive fibonacci");
    EVAL("(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))");
    CHECK(VAL_EQ_INT(EVAL("(fib 10)"), 55));

    TEST("fib(20)");
    CHECK(VAL_EQ_INT(EVAL("(fib 20)"), 6765));
}

static void test_list_ops(void) {
    printf("\n=== List Ops ===\n");

    TEST("(cons 1 (list 2 3))");
    Val r = EVAL("(cons 1 (list 2 3))");
    CHECK(val_is_cons(r) && VAL_EQ_INT(car(r), 1) && list_len(r) == 3);

    TEST("(first (list 1 2 3))");
    CHECK(VAL_EQ_INT(EVAL("(first (list 1 2 3))"), 1));

    TEST("(rest (list 1 2 3))");
    Val rest = EVAL("(rest (list 1 2 3))");
    CHECK(val_is_cons(rest) && VAL_EQ_INT(car(rest), 2) && list_len(rest) == 2);

    TEST("(count (list 1 2 3 4 5))");
    CHECK(VAL_EQ_INT(EVAL("(count (list 1 2 3 4 5))"), 5));

    TEST("(nil? nil) / (nil? 1)");
    CHECK(VAL_EQ_BOOL(EVAL("(nil? nil)"), true) && VAL_EQ_BOOL(EVAL("(nil? 1)"), false));

    TEST("(map (fn [x] (* x x)) (list 1 2 3))");
    Val mapped = EVAL("(map (fn [x] (* x x)) (list 1 2 3))");
    CHECK(VAL_EQ_INT(car(mapped), 1) && VAL_EQ_INT(car(cdr(mapped)), 4));

    TEST("(filter (fn [x] (> x 2)) (list 1 2 3 4))");
    Val filtered = EVAL("(filter (fn [x] (> x 2)) (list 1 2 3 4))");
    CHECK(list_len(filtered) == 2 && VAL_EQ_INT(car(filtered), 3));

    TEST("(reduce + 0 (list 1 2 3 4 5))");
    CHECK(VAL_EQ_INT(EVAL("(reduce + 0 (list 1 2 3 4 5))"), 15));
}

// ============================================================================
// 9. Benchmarks
// ============================================================================

static void bench_eval(void) {
    printf("\n--- bench: Eval ---\n");

    // Pre-read forms for benchmarking eval (not read)
    Val form_add = read_str("(+ 1 2)");
    Val form_nested = read_str("(+ (* 2 3) (- 10 4))");
    Val form_if = read_str("(if true 1 2)");
    Val form_let = read_str("(let (x 42) x)");

    int N;
    u64 t0, dt;

    // (+ 1 2)
    N = 5000000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = eval(form_add, g_global_env);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  (+ 1 2):           %6.1f ns/op\n", (double)dt / N);

    // nested arithmetic
    N = 2000000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = eval(form_nested, g_global_env);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  (+ (* 2 3) (- ..): %6.1f ns/op\n", (double)dt / N);

    // if
    N = 5000000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = eval(form_if, g_global_env);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  (if true 1 2):     %6.1f ns/op\n", (double)dt / N);

    // let
    N = 2000000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = eval(form_let, g_global_env);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  (let (x 42) x):   %6.1f ns/op\n", (double)dt / N);

    // fibonacci(10) — recursive
    Val form_fib = read_str("(fib 10)");
    N = 100000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = eval(form_fib, g_global_env);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  (fib 10):          %6.0f ns/op  (%d calls)\n", (double)dt / N, 177);

    // Reader throughput
    const char *complex_form = "(defn foo [x y] (if (< x y) (+ x (* y 2)) (- x y)))";
    N = 1000000;
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        Val r = read_str(complex_form);
        SINK(r);
    }
    dt = now_ns() - t0;
    printf("  read complex form: %6.1f ns/op\n", (double)dt / N);
}

// ============================================================================
// 10. REPL
// ============================================================================

static void repl(void) {
    char line[4096];
    printf("proto_eval REPL (type 'exit' to quit)\n");
    printf("> ");
    fflush(stdout);
    while (fgets(line, sizeof(line), stdin)) {
        u32 len = (u32)strlen(line);
        if (len > 0 && line[len - 1] == '\n') line[--len] = '\0';
        if (len == 0) { printf("> "); fflush(stdout); continue; }
        if (strcmp(line, "exit") == 0 || strcmp(line, "quit") == 0) break;

        Val result = eval_string(line, g_global_env);
        printf("%s\n> ", print_val(result));
        fflush(stdout);
    }
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char **argv) {
    printf("proto_eval.c — Minimal Clojure Evaluator\n");
    printf("==========================================\n");
    printf("sizeof(Cons): %zu  sizeof(FnObj): %zu  sizeof(Env): %zu\n",
           sizeof(Cons), sizeof(FnObj), sizeof(Env));
    printf("==========================================\n");

    base_init();
    init_symbols();
    g_global_env = env_create(&g_perm, NULL);
    register_builtins(g_global_env);

    // Define fib for tests and benchmarks
    eval_string("(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))",
                g_global_env);

    if (argc > 1 && strcmp(argv[1], "--repl") == 0) {
        repl();
    } else {
        // Tests
        test_reader();
        test_eval_basics();
        test_special_forms();
        test_functions();
        test_list_ops();

        printf("\n==========================================\n");
        printf("Tests: %d passed, %d failed\n", g_pass, g_fail);
        printf("==========================================\n");

        // Benchmarks
        bench_eval();

        printf("\n==========================================\n");
        printf("McCarthy's eval/apply on proto_base — done.\n");
        printf("==========================================\n");
    }

    base_cleanup();
    return g_fail ? 1 : 0;
}
