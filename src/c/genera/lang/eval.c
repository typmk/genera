/**
 * eval.c — Tree-Walking Evaluator
 *
 * McCarthy's eval/apply grounded in the base layer.
 * Freestanding: no libc. Uses arena alloc, interned symbols, NaN-boxed values.
 *
 * Depends on: platform/ (Cons, protocols), lang/read.c (symbols, reader)
 */
#ifndef EVAL_C_INCLUDED
#define EVAL_C_INCLUDED

// ============================================================================
// 1. Environment — lexical scope via HashMap chain
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
// 2. Functions — builtins + closures
// ============================================================================

typedef Val (*BuiltinFn)(Val args);

typedef struct {
    u8 type;  // 0=builtin, 1=closure
    union {
        struct { BuiltinFn fn; StrId name; } builtin;
        struct { Val params; Val body; Env *env; } closure;
    };
} FnObj;

#define FN_BUILTIN 0
#define FN_CLOSURE 1

static Val make_builtin(StrId name, BuiltinFn fn) {
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
// 3. Printer — Val → string (freestanding)
// ============================================================================

static char g_pr_buf[8192];
static u32 g_pr_pos;

static void pr_init(void) { g_pr_pos = 0; g_pr_buf[0] = '\0'; }
static void pr_ch(char c) {
    if (g_pr_pos < sizeof(g_pr_buf) - 1) g_pr_buf[g_pr_pos++] = c;
    g_pr_buf[g_pr_pos] = '\0';
}
static void pr_s(const char *s) { while (*s) pr_ch(*s++); }

static void pr_int(i64 n) {
    if (n < 0) { pr_ch('-'); n = -n; }
    if (n == 0) { pr_ch('0'); return; }
    char buf[20]; u32 len = 0;
    while (n > 0) { buf[len++] = '0' + (char)(n % 10); n /= 10; }
    while (len > 0) pr_ch(buf[--len]);
}

static void pr_f64(f64 v) {
    if (v < 0) { pr_ch('-'); v = -v; }
    i64 whole = (i64)v;
    pr_int(whole);
    f64 frac = v - (f64)whole;
    pr_ch('.');
    if (frac > 0.0000001) {
        u32 start = g_pr_pos;
        for (int i = 0; i < 6; i++) {
            frac *= 10;
            int d = (int)frac;
            pr_ch('0' + (char)d);
            frac -= d;
        }
        // Trim trailing zeros (keep at least one decimal)
        while (g_pr_pos > start + 1 && g_pr_buf[g_pr_pos-1] == '0') g_pr_pos--;
        g_pr_buf[g_pr_pos] = '\0';
    } else {
        pr_ch('0'); // e.g., 4.0
    }
}

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

static void pr_pmap_entry(u32 key, Val value, void *ctx) {
    bool *first = (bool *)ctx;
    if (!*first) pr_ch(' ');
    *first = false;
    pr_ch(':'); Str s = str_from_id((StrId)key);
    for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]);
    pr_ch(' '); pr_val(value);
}

static void pr_val(Val v) {
    if (val_is_nil(v))  { pr_s("nil"); return; }
    if (val_is_bool(v)) { pr_s(val_as_bool(v) ? "true" : "false"); return; }
    if (val_is_int(v))  { pr_int(val_as_int(v)); return; }
    if (val_is_f64(v))  { pr_f64(val_as_f64(v)); return; }
    if (val_is_sym(v))  { Str s = str_from_id(val_as_sym(v)); for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]); return; }
    if (val_is_kw(v))   { pr_ch(':'); Str s = str_from_id(val_as_kw(v)); for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]); return; }
    if (val_is_str(v))  { pr_ch('"'); Str *s = val_as_str(v); for (u32 i = 0; i < s->len; i++) pr_ch(s->data[i]); pr_ch('"'); return; }
    if (val_is_pvec(v)) {
        pr_ch('[');
        CPVec *pv = (CPVec *)val_as_pvec(v);
        for (u32 i = 0; i < pv->count; i++) {
            if (i > 0) pr_ch(' ');
            pr_val(cpvec_get(*pv, i));
        }
        pr_ch(']'); return;
    }
    if (val_is_pmap(v)) {
        pr_ch('{');
        CPMap *pm = (CPMap *)val_as_pmap(v);
        bool first = true;
        cpmap_foreach(*pm, pr_pmap_entry, &first);
        pr_ch('}'); return;
    }
    if (val_is_cons(v)) { pr_list(v); return; }
    if (val_is_fn(v))   {
        FnObj *f = (FnObj *)val_as_fn(v);
        if (f->type == FN_BUILTIN) {
            pr_s("#<");
            Str s = str_from_id(f->builtin.name);
            for (u32 i = 0; i < s.len; i++) pr_ch(s.data[i]);
            pr_ch('>');
        } else pr_s("#<fn>");
        return;
    }
    pr_s("#<unknown>");
}

static const char *print_val(Val v) { pr_init(); pr_val(v); return g_pr_buf; }

// ============================================================================
// 3b. Pretty-Print — colored output to g_print_buf
//
// Structure (brackets, delimiters) → dim
// Keywords → cyan, strings → green, numbers → yellow
// nil/true/false → magenta, symbols → default, fns → dim
// ============================================================================

static void pp_val(Val v);

static void pp_list(Val v) {
    OutBuf *b = &g_print_buf;
    if (g_color) buf_s(b, C_DIM);
    buf_c(b, '(');
    if (g_color) buf_s(b, C_RESET);
    bool first = true;
    while (val_is_cons(v)) {
        if (!first) buf_c(b, ' ');
        first = false;
        pp_val(car(v));
        v = cdr(v);
    }
    if (!val_is_nil(v)) { buf_s(b, " . "); pp_val(v); }
    if (g_color) buf_s(b, C_DIM);
    buf_c(b, ')');
    if (g_color) buf_s(b, C_RESET);
}

static void pp_pmap_entry_color(u32 key, Val value, void *ctx) {
    OutBuf *b = &g_print_buf;
    bool *first = (bool *)ctx;
    if (!*first) buf_c(b, ' ');
    *first = false;
    if (g_color) buf_s(b, C_CYAN);
    buf_c(b, ':');
    Str s = str_from_id((StrId)key);
    buf_n(b, (const char *)s.data, s.len);
    if (g_color) buf_s(b, C_RESET);
    buf_c(b, ' ');
    pp_val(value);
}

static void pp_color(const char *c) { if (g_color) buf_s(&g_print_buf, c); }

static void pp_val(Val v) {
    OutBuf *b = &g_print_buf;
    if (val_is_nil(v))  { pp_color(C_MAGENTA); buf_s(b, "nil"); pp_color(C_RESET); return; }
    if (val_is_bool(v)) { pp_color(C_MAGENTA); buf_s(b, val_as_bool(v) ? "true" : "false"); pp_color(C_RESET); return; }
    if (val_is_int(v))  { pp_color(C_YELLOW); buf_i(b, val_as_int(v)); pp_color(C_RESET); return; }
    if (val_is_f64(v))  { pp_color(C_YELLOW); buf_s(b, print_val(v)); pp_color(C_RESET); return; }
    if (val_is_sym(v))  { Str s = str_from_id(val_as_sym(v)); buf_n(b, (const char *)s.data, s.len); return; }
    if (val_is_kw(v))   { pp_color(C_CYAN); buf_c(b, ':'); Str s = str_from_id(val_as_kw(v)); buf_n(b, (const char *)s.data, s.len); pp_color(C_RESET); return; }
    if (val_is_str(v))  { pp_color(C_GREEN); buf_c(b, '"'); Str *s = val_as_str(v); buf_n(b, (const char *)s->data, s->len); buf_c(b, '"'); pp_color(C_RESET); return; }
    if (val_is_pvec(v)) {
        pp_color(C_DIM); buf_c(b, '['); pp_color(C_RESET);
        CPVec *pv = (CPVec *)val_as_pvec(v);
        for (u32 i = 0; i < pv->count; i++) { if (i) buf_c(b, ' '); pp_val(cpvec_get(*pv, i)); }
        pp_color(C_DIM); buf_c(b, ']'); pp_color(C_RESET);
        return;
    }
    if (val_is_pmap(v)) {
        pp_color(C_DIM); buf_c(b, '{'); pp_color(C_RESET);
        CPMap *pm = (CPMap *)val_as_pmap(v);
        bool first = true;
        cpmap_foreach(*pm, pp_pmap_entry_color, &first);
        pp_color(C_DIM); buf_c(b, '}'); pp_color(C_RESET);
        return;
    }
    if (val_is_cons(v)) { pp_list(v); return; }
    if (val_is_fn(v))   { pp_color(C_DIM); buf_s(b, print_val(v)); pp_color(C_RESET); return; }
    buf_s(b, "#<unknown>");
}

// ============================================================================
// 4. Eval / Apply
// ============================================================================

static Val eval(Val form, Env *env);
static Env *g_global_env;

static Val eval_body(Val forms, Env *env) {
    Val result = NIL;
    while (val_is_cons(forms)) {
        result = eval(car(forms), env);
        forms = cdr(forms);
    }
    return result;
}

static Val eval_list(Val forms, Env *env) {
    if (!val_is_cons(forms)) return NIL;
    Val head = eval(car(forms), env);
    return cons_new(head, eval_list(cdr(forms), env));
}

static Val apply_fn(Val fn_val, Val args) {
    FnObj *f = (FnObj *)val_as_fn(fn_val);

    if (f->type == FN_BUILTIN)
        return f->builtin.fn(args);

    Env *call_env = env_create(&g_req, f->closure.env);
    Val params = f->closure.params;

    while (val_is_cons(params) && val_is_cons(args)) {
        StrId name = val_as_sym(car(params));
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
        val_is_f64(form) || val_is_kw(form) || val_is_str(form) || val_is_fn(form) ||
        val_is_pvec(form) || val_is_pmap(form))
        return form;

    // Symbol lookup
    if (val_is_sym(form)) {
        Val result;
        if (env_get(env, val_as_sym(form), &result)) return result;
        pf("ERROR: undefined '");
        Str s = str_from_id(val_as_sym(form));
        for (u32 i = 0; i < s.len; i++) buf_c(&g_print_buf, s.data[i]);
        pf("'\n");
        return NIL;
    }

    if (!val_is_cons(form)) return form;

    Val head = car(form);
    Val rest = cdr(form);

    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);

        if (sym == S_QUOTE) return car(rest);

        if (sym == S_DEF) {
            StrId name = val_as_sym(car(rest));
            Val value = eval(car(cdr(rest)), env);
            env_set(g_global_env, name, value);
            return value;
        }

        if (sym == S_DEFN) {
            StrId name = val_as_sym(car(rest));
            Val params = pvec_to_list(car(cdr(rest)));
            Val body = cdr(cdr(rest));
            Val fn = make_closure(params, body, env);
            env_set(g_global_env, name, fn);
            return fn;
        }

        if (sym == S_FN) {
            Val params = pvec_to_list(car(rest));
            Val body = cdr(rest);
            return make_closure(params, body, env);
        }

        if (sym == S_IF) {
            Val cond = eval(car(rest), env);
            if (val_truthy(cond))
                return eval(car(cdr(rest)), env);
            Val else_branch = cdr(cdr(rest));
            return val_is_cons(else_branch) ? eval(car(else_branch), env) : NIL;
        }

        if (sym == S_DO) return eval_body(rest, env);

        if (sym == S_LET) {
            Env *let_env = env_create(&g_req, env);
            Val bindings = pvec_to_list(car(rest));
            while (val_is_cons(bindings)) {
                StrId name = val_as_sym(car(bindings));
                bindings = cdr(bindings);
                env_set(let_env, name, eval(car(bindings), let_env));
                bindings = cdr(bindings);
            }
            return eval_body(cdr(rest), let_env);
        }

        if (sym == S_AND) {
            Val result = val_true();
            while (val_is_cons(rest)) {
                result = eval(car(rest), env);
                if (!val_truthy(result)) return result;
                rest = cdr(rest);
            }
            return result;
        }

        if (sym == S_OR) {
            while (val_is_cons(rest)) {
                Val result = eval(car(rest), env);
                if (val_truthy(result)) return result;
                rest = cdr(rest);
            }
            return NIL;
        }

        if (sym == S_COND) {
            while (val_is_cons(rest)) {
                Val test = car(rest);
                rest = cdr(rest);
                if (!val_is_cons(rest)) return NIL;
                // :else is always truthy (it's a keyword)
                if (val_truthy(eval(test, env)))
                    return eval(car(rest), env);
                rest = cdr(rest);
            }
            return NIL;
        }

        if (sym == S_WHEN) {
            Val cond_val = eval(car(rest), env);
            if (val_truthy(cond_val))
                return eval_body(cdr(rest), env);
            return NIL;
        }
    }

    // Function call
    Val fn_val = eval(head, env);
    if (!val_is_fn(fn_val)) {
        pf("ERROR: not callable\n");
        return NIL;
    }
    return apply_fn(fn_val, eval_list(rest, env));
}

static Val eval_string(const char *s, Env *env) {
    gram_ensure_scratch();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    gram_parse(&g_gram_scratch, &lisp, s, (u32)strlen(s));
    Val result = NIL;
    u32 c = g_gram_scratch.nodes[0].child;
    while (c) {
        result = eval(entity_to_val(&g_gram_scratch, c), env);
        c = g_gram_scratch.nodes[c].next;
    }
    return result;
}

// ============================================================================
// 5. Built-in Functions
// ============================================================================

// --- Arithmetic (int/f64 promotion) ---
ALWAYS_INLINE bool args_has_f64(Val args) {
    Val a = args;
    while (val_is_cons(a)) { if (val_is_f64(car(a))) return true; a = cdr(a); }
    return false;
}
ALWAYS_INLINE f64 val_to_f64(Val v) {
    if (val_is_f64(v)) return val_as_f64(v);
    if (val_is_int(v)) return (f64)val_as_int(v);
    return 0.0;
}

static Val bi_add(Val args) {
    if (args_has_f64(args)) {
        f64 sum = 0;
        while (val_is_cons(args)) { sum += val_to_f64(car(args)); args = cdr(args); }
        return val_f64(sum);
    }
    i64 sum = 0;
    while (val_is_cons(args)) { sum += val_as_int(car(args)); args = cdr(args); }
    return val_int(sum);
}

static Val bi_sub(Val args) {
    if (!val_is_cons(args)) return val_int(0);
    if (args_has_f64(args)) {
        f64 r = val_to_f64(car(args)); args = cdr(args);
        if (!val_is_cons(args)) return val_f64(-r);
        while (val_is_cons(args)) { r -= val_to_f64(car(args)); args = cdr(args); }
        return val_f64(r);
    }
    i64 r = val_as_int(car(args)); args = cdr(args);
    if (!val_is_cons(args)) return val_int(-r);
    while (val_is_cons(args)) { r -= val_as_int(car(args)); args = cdr(args); }
    return val_int(r);
}

static Val bi_mul(Val args) {
    if (args_has_f64(args)) {
        f64 p = 1;
        while (val_is_cons(args)) { p *= val_to_f64(car(args)); args = cdr(args); }
        return val_f64(p);
    }
    i64 p = 1;
    while (val_is_cons(args)) { p *= val_as_int(car(args)); args = cdr(args); }
    return val_int(p);
}

static Val bi_div(Val args) {
    if (!val_is_cons(args)) return val_int(0);
    if (args_has_f64(args)) {
        f64 r = val_to_f64(car(args)); args = cdr(args);
        while (val_is_cons(args)) {
            f64 d = val_to_f64(car(args));
            if (d == 0.0) { pf("ERROR: /0\n"); return NIL; }
            r /= d; args = cdr(args);
        }
        return val_f64(r);
    }
    i64 r = val_as_int(car(args)); args = cdr(args);
    while (val_is_cons(args)) {
        i64 d = val_as_int(car(args));
        if (d == 0) { pf("ERROR: /0\n"); return NIL; }
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
static Val bi_lt(Val args)  {
    Val a = car(args), b = car(cdr(args));
    if (val_is_f64(a) || val_is_f64(b)) return val_bool(val_to_f64(a) < val_to_f64(b));
    return val_bool(val_as_int(a) < val_as_int(b));
}
static Val bi_gt(Val args)  {
    Val a = car(args), b = car(cdr(args));
    if (val_is_f64(a) || val_is_f64(b)) return val_bool(val_to_f64(a) > val_to_f64(b));
    return val_bool(val_as_int(a) > val_as_int(b));
}
static Val bi_lte(Val args) {
    Val a = car(args), b = car(cdr(args));
    if (val_is_f64(a) || val_is_f64(b)) return val_bool(val_to_f64(a) <= val_to_f64(b));
    return val_bool(val_as_int(a) <= val_as_int(b));
}
static Val bi_gte(Val args) {
    Val a = car(args), b = car(cdr(args));
    if (val_is_f64(a) || val_is_f64(b)) return val_bool(val_to_f64(a) >= val_to_f64(b));
    return val_bool(val_as_int(a) >= val_as_int(b));
}

// --- List ops ---
static Val bi_cons(Val args)   { return cons_new(car(args), car(cdr(args))); }
static Val bi_first(Val args)  { return p_first(car(args)); }
static Val bi_rest(Val args)   { return p_rest(car(args)); }
static Val bi_list(Val args)   { return args; }
static Val bi_count(Val args)  { return p_count(car(args)); }
static Val bi_nilq(Val args)   { return val_bool(val_is_nil(car(args))); }
static Val bi_not(Val args)    { return val_bool(!val_truthy(car(args))); }
static Val bi_consq(Val args)  { return val_bool(val_is_cons(car(args))); }

// --- Type predicates ---
static Val bi_intq(Val args)   { return val_bool(val_is_int(car(args))); }
static Val bi_fnq(Val args)    { return val_bool(val_is_fn(car(args))); }
static Val bi_symq(Val args)   { return val_bool(val_is_sym(car(args))); }

// --- I/O ---
static Val bi_println(Val args) {
    bool first_arg = true;
    while (val_is_cons(args)) {
        if (!first_arg) pf(" ");
        first_arg = false;
        Val v = car(args);
        if (val_is_str(v)) {
            Str *s = val_as_str(v);
            for (u32 i = 0; i < s->len; i++) buf_c(&g_print_buf, s->data[i]);
        } else {
            pf("%s", print_val(v));
        }
        args = cdr(args);
    }
    pf("\n");
    print_flush();
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

// --- Higher-order (polymorphic: cons list + pvec) ---
static Val bi_map(Val args) {
    Val fn = car(args), coll = car(cdr(args));
    Val result = NIL, *tail = &result;
    if (val_is_pvec(coll)) {
        CPVec *pv = (CPVec *)val_as_pvec(coll);
        for (u32 i = 0; i < pv->count; i++) {
            Val cell = cons_new(apply_fn(fn, cons_new(cpvec_get(*pv, i), NIL)), NIL);
            *tail = cell;
            tail = &((Cons *)val_as_cons(cell))->cdr;
        }
        return result;
    }
    while (val_is_cons(coll)) {
        Val cell = cons_new(apply_fn(fn, cons_new(car(coll), NIL)), NIL);
        *tail = cell;
        tail = &((Cons *)val_as_cons(cell))->cdr;
        coll = cdr(coll);
    }
    return result;
}

static Val bi_filter(Val args) {
    Val fn = car(args), coll = car(cdr(args));
    Val result = NIL, *tail = &result;
    if (val_is_pvec(coll)) {
        CPVec *pv = (CPVec *)val_as_pvec(coll);
        for (u32 i = 0; i < pv->count; i++) {
            Val item = cpvec_get(*pv, i);
            if (val_truthy(apply_fn(fn, cons_new(item, NIL)))) {
                Val cell = cons_new(item, NIL);
                *tail = cell;
                tail = &((Cons *)val_as_cons(cell))->cdr;
            }
        }
        return result;
    }
    while (val_is_cons(coll)) {
        if (val_truthy(apply_fn(fn, cons_new(car(coll), NIL)))) {
            Val cell = cons_new(car(coll), NIL);
            *tail = cell;
            tail = &((Cons *)val_as_cons(cell))->cdr;
        }
        coll = cdr(coll);
    }
    return result;
}

static Val bi_reduce(Val args) {
    Val fn = car(args), init = car(cdr(args)), coll = car(cdr(cdr(args)));
    Val acc = init;
    if (val_is_pvec(coll)) {
        CPVec *pv = (CPVec *)val_as_pvec(coll);
        for (u32 i = 0; i < pv->count; i++)
            acc = apply_fn(fn, cons_new(acc, cons_new(cpvec_get(*pv, i), NIL)));
        return acc;
    }
    while (val_is_cons(coll)) {
        acc = apply_fn(fn, cons_new(acc, cons_new(car(coll), NIL)));
        coll = cdr(coll);
    }
    return acc;
}

static Val bi_apply(Val args) {
    return apply_fn(car(args), car(cdr(args)));
}

// --- Collection ops ---
static Val bi_get(Val args) {
    return p_get(car(args), car(cdr(args)));
}

static Val bi_assoc(Val args) {
    return p_assoc(car(args), car(cdr(args)), car(cdr(cdr(args))));
}

static Val bi_conj(Val args) {
    Val coll = car(args), item = car(cdr(args));
    if (val_is_pvec(coll)) {
        CPVec *pv = (CPVec *)val_as_pvec(coll);
        CPVec *nv = arena_push(&g_req, CPVec);
        *nv = cpvec_append(*pv, item);
        return val_pvec(nv);
    }
    if (val_is_pmap(coll) && val_is_pvec(item)) {
        // (conj map [k v])
        CPVec *kv = (CPVec *)val_as_pvec(item);
        if (kv->count == 2)
            return p_assoc(coll, cpvec_get(*kv, 0), cpvec_get(*kv, 1));
        return coll;
    }
    // Default: cons onto front (list behavior)
    return cons_new(item, coll);
}

static Val bi_nth(Val args) {
    Val coll = car(args);
    i64 idx = val_as_int(car(cdr(args)));
    if (val_is_pvec(coll))
        return cpvec_get(*(CPVec *)val_as_pvec(coll), (u32)idx);
    // List nth
    Val v = coll;
    for (i64 i = 0; i < idx && val_is_cons(v); i++) v = cdr(v);
    return val_is_cons(v) ? car(v) : NIL;
}

static Val bi_vec(Val args) {
    Val lst = car(args);
    CPVec *v = arena_push(&g_req, CPVec);
    *v = cpvec_empty();
    while (val_is_cons(lst)) {
        *v = cpvec_append(*v, car(lst));
        lst = cdr(lst);
    }
    return val_pvec(v);
}

static Val bi_hashmap(Val args) {
    CPMap *m = arena_push(&g_req, CPMap);
    *m = cpmap_empty();
    while (val_is_cons(args) && val_is_cons(cdr(args))) {
        Val k = car(args);
        Val v = car(cdr(args));
        u32 key;
        if (val_is_kw(k))       key = val_as_kw(k);
        else if (val_is_sym(k)) key = val_as_sym(k);
        else { args = cdr(cdr(args)); continue; }
        *m = cpmap_put(*m, key, v);
        args = cdr(cdr(args));
    }
    return val_pmap(m);
}

typedef struct { Val result; } KeysCtx;
static void _keys_cb(u32 key, Val value, void *ctx) {
    (void)value;
    KeysCtx *kc = (KeysCtx *)ctx;
    kc->result = cons_new(val_kw((StrId)key), kc->result);
}
static Val bi_keys(Val args) {
    Val m = car(args);
    if (!val_is_pmap(m)) return NIL;
    KeysCtx ctx = {NIL};
    cpmap_foreach(*(CPMap *)val_as_pmap(m), _keys_cb, &ctx);
    return ctx.result;
}

static void _vals_cb(u32 key, Val value, void *ctx) {
    (void)key;
    KeysCtx *kc = (KeysCtx *)ctx;
    kc->result = cons_new(value, kc->result);
}
static Val bi_vals(Val args) {
    Val m = car(args);
    if (!val_is_pmap(m)) return NIL;
    KeysCtx ctx = {NIL};
    cpmap_foreach(*(CPMap *)val_as_pmap(m), _vals_cb, &ctx);
    return ctx.result;
}

static Val bi_containsq(Val args) {
    Val coll = car(args), key = car(cdr(args));
    if (val_is_pmap(coll)) {
        CPMap *pm = (CPMap *)val_as_pmap(coll);
        u32 k;
        if (val_is_kw(key))       k = val_as_kw(key);
        else if (val_is_sym(key)) k = val_as_sym(key);
        else return val_false();
        Val out;
        return val_bool(cpmap_get(*pm, k, &out));
    }
    return val_false();
}

static Val bi_emptyq(Val args) {
    Val v = car(args);
    if (val_is_nil(v)) return val_true();
    return val_bool(val_as_int(p_count(v)) == 0);
}

static void into_conj(Val *to, Val item) {
    if (val_is_pvec(*to)) {
        CPVec *pv = (CPVec *)val_as_pvec(*to);
        CPVec *nv = arena_push(&g_req, CPVec);
        *nv = cpvec_append(*pv, item);
        *to = val_pvec(nv);
    } else if (val_is_pmap(*to) && val_is_pvec(item)) {
        CPVec *kv = (CPVec *)val_as_pvec(item);
        if (kv->count == 2)
            *to = p_assoc(*to, cpvec_get(*kv, 0), cpvec_get(*kv, 1));
    } else {
        *to = cons_new(item, *to);
    }
}

static Val bi_into(Val args) {
    Val to = car(args), from = car(cdr(args));
    if (val_is_pvec(from)) {
        CPVec *pv = (CPVec *)val_as_pvec(from);
        for (u32 i = 0; i < pv->count; i++)
            into_conj(&to, cpvec_get(*pv, i));
        return to;
    }
    while (val_is_cons(from)) {
        into_conj(&to, car(from));
        from = cdr(from);
    }
    return to;
}

static Val bi_str(Val args) {
    pr_init();
    while (val_is_cons(args)) {
        Val v = car(args);
        if (val_is_str(v)) {
            Str *s = val_as_str(v);
            for (u32 i = 0; i < s->len; i++) pr_ch(s->data[i]);
        } else if (val_is_nil(v)) {
            // nil prints as empty string in str
        } else {
            pr_val(v);
        }
        args = cdr(args);
    }
    Str *sp = arena_push(&g_req, Str);
    *sp = str_dup(&g_req, (Str){(u8 *)g_pr_buf, g_pr_pos});
    return val_str(sp);
}

static Val bi_vecq(Val args)  { return val_bool(val_is_pvec(car(args))); }
static Val bi_mapq(Val args)  { return val_bool(val_is_pmap(car(args))); }
static Val bi_kwq(Val args)   { return val_bool(val_is_kw(car(args))); }
static Val bi_strq(Val args)  { return val_bool(val_is_str(car(args))); }

// --- Range (returns pvec for cache-friendly iteration) ---
static Val bi_range(Val args) {
    i64 start = 0, end;
    if (val_is_cons(cdr(args))) {
        start = val_as_int(car(args));
        end = val_as_int(car(cdr(args)));
    } else {
        end = val_as_int(car(args));
    }
    CPVec *v = arena_push(&g_req, CPVec);
    *v = cpvec_empty();
    for (i64 i = start; i < end; i++)
        *v = cpvec_append(*v, val_int(i));
    return val_pvec(v);
}

// --- Transient operations ---
static Val bi_transient(Val args) {
    Val m = car(args);
    if (!val_is_pmap(m)) return m;
    CPMap *pm = (CPMap *)val_as_pmap(m);
    CPMap *tm = arena_push(&g_req, CPMap);
    *tm = cpmap_transient(*pm);
    return val_pmap(tm);
}

static Val bi_persistent(Val args) {
    Val m = car(args);
    if (!val_is_pmap(m)) return m;
    CPMap *pm = (CPMap *)val_as_pmap(m);
    CPMap *nm = arena_push(&g_req, CPMap);
    *nm = cpmap_persistent(*pm);
    return val_pmap(nm);
}

static Val bi_assocbang(Val args) {
    Val m = car(args), k = car(cdr(args)), v = car(cdr(cdr(args)));
    if (!val_is_pmap(m)) return m;
    // assoc! uses protocol dispatch (which detects transient)
    return p_assoc(m, k, v);
}

// ============================================================================
// 6. Register builtins + init
// ============================================================================

static void register_builtins(Env *env) {
    #define REG(n, f) env_set(env, INTERN(n), make_builtin(INTERN(n), f))
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
    // Collections
    REG("get", bi_get);  REG("assoc", bi_assoc);  REG("conj", bi_conj);
    REG("nth", bi_nth);  REG("vec", bi_vec);  REG("hash-map", bi_hashmap);
    REG("keys", bi_keys);  REG("vals", bi_vals);
    REG("contains?", bi_containsq);  REG("empty?", bi_emptyq);
    REG("into", bi_into);  REG("str", bi_str);
    REG("vector?", bi_vecq);  REG("map?", bi_mapq);
    REG("keyword?", bi_kwq);  REG("string?", bi_strq);
    REG("range", bi_range);
    // Transient operations
    REG("transient", bi_transient);  REG("persistent!", bi_persistent);
    REG("assoc!", bi_assocbang);
    #undef REG
}

static void eval_init(void) {
    g_global_env = env_create(&g_perm, NULL);
    register_builtins(g_global_env);
}

#endif // EVAL_C_INCLUDED
