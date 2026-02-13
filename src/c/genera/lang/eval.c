/**
 * eval.c — Tree-Walking Evaluator
 *
 * McCarthy's eval/apply grounded in the base layer.
 * Freestanding: no libc. Uses arena alloc, interned symbols, NaN-boxed values.
 *
 * Special forms dispatched via sig table: sig_get(sym) → handler(rest, env).
 * Signal protocol (g_signal, g_signal_val, g_depth) in sig.c.
 *
 * Depends on: platform/ (Cons, protocols), lang/grammar.c, lang/coll.c (sig)
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
#define FN_MACRO   2

static Val make_builtin(StrId name, BuiltinFn fn) {
    FnObj *f = arena_push(&g_perm, FnObj);
    f->type = FN_BUILTIN;
    f->builtin.fn = fn;
    f->builtin.name = name;
    return val_fn(f);
}

static Val make_closure_on(Arena *a, Val params, Val body, Env *env) {
    FnObj *f = arena_push(a, FnObj);
    f->type = FN_CLOSURE;
    f->closure.params = params;
    f->closure.body = body;
    f->closure.env = env;
    return val_fn(f);
}
static Val make_closure(Val params, Val body, Env *env) {
    return make_closure_on(&g_req, params, body, env);
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
        } else if (f->type == FN_MACRO) pr_s("#<macro>");
        else pr_s("#<fn>");
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
//
// Signal protocol (g_signal, g_signal_val, g_depth) in sig.c.
// Special forms dispatched via sig: sig_get(sym) → handler(rest, env).
// ============================================================================

static Val eval(Val form, Env *env);

// g_global_env aliases g_world_env (declared in cmd.c as void*).
// This lets undo/redo restore the env root — time travel across eval.
#define g_global_env (*(Env **)&g_world_env)

static Val eval_body(Val forms, Env *env) {
    Val result = NIL;
    while (val_is_cons(forms)) {
        result = eval(car(forms), env);
        if (g_signal) return NIL;
        forms = cdr(forms);
    }
    return result;
}

static Val eval_list(Val forms, Env *env) {
    if (!val_is_cons(forms)) return NIL;
    Val head = eval(car(forms), env);
    if (g_signal) return NIL;
    return cons_new(head, eval_list(cdr(forms), env));
}

static Val apply_fn(Val fn_val, Val args) {
    if (!val_is_fn(fn_val)) {
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        pf("ERROR: not callable\n");
        return NIL;
    }
    FnObj *f = (FnObj *)val_as_fn(fn_val);

    if (f->type == FN_BUILTIN)
        return f->builtin.fn(args);

    if (++g_depth > DEPTH_MAX) {
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        pf("ERROR: stack overflow (depth %u)\n", g_depth);
        g_depth--;
        return NIL;
    }

    Env *call_env = env_create(&g_req, f->closure.env);
    Val params = f->closure.params;

    while (val_is_cons(params)) {
        StrId name = val_as_sym(car(params));
        Str name_str = str_from_id(name);
        if (name_str.len == 1 && name_str.data[0] == '&') {
            params = cdr(params);
            if (val_is_cons(params))
                env_set(call_env, val_as_sym(car(params)), args);
            break;
        }
        env_set(call_env, name, val_is_cons(args) ? car(args) : NIL);
        params = cdr(params);
        if (val_is_cons(args)) args = cdr(args);
    }

    Val result = eval_body(f->closure.body, call_env);
    g_depth--;
    return result;
}

// ============================================================================
// 4b. Special Form Handlers — registered on sig table
// ============================================================================

static Val sf_quote(Val rest, void *ctx) {
    (void)ctx;
    return car(rest);
}

static Val sf_def(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    StrId name = val_as_sym(car(rest));
    Val value = eval(car(cdr(rest)), env);
    if (g_signal) return NIL;
    env_set(g_global_env, name, value);
    return value;
}

static Val sf_defn(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    StrId name = val_as_sym(car(rest));
    Arena *saved = g_val_arena; g_val_arena = &g_perm;
    Val params = pvec_to_list(car(cdr(rest)));
    g_val_arena = saved;
    Val body = cdr(cdr(rest));
    Val fn = make_closure_on(&g_perm, params, body, env);
    env_set(g_global_env, name, fn);
    return fn;
}

static Val sf_fn(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    Val params = pvec_to_list(car(rest));
    Val body = cdr(rest);
    return make_closure(params, body, env);
}

static Val sf_if(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    Val cond = eval(car(rest), env);
    if (g_signal) return NIL;
    if (val_truthy(cond))
        return eval(car(cdr(rest)), env);
    Val else_branch = cdr(cdr(rest));
    return val_is_cons(else_branch) ? eval(car(else_branch), env) : NIL;
}

static Val sf_do(Val rest, void *ctx) {
    return eval_body(rest, (Env *)ctx);
}

static Val sf_let(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    Env *let_env = env_create(&g_req, env);
    Val bindings = pvec_to_list(car(rest));
    while (val_is_cons(bindings)) {
        StrId name = val_as_sym(car(bindings));
        bindings = cdr(bindings);
        Val val = eval(car(bindings), let_env);
        if (g_signal) return NIL;
        env_set(let_env, name, val);
        bindings = cdr(bindings);
    }
    return eval_body(cdr(rest), let_env);
}

// and/or/cond/when → Clojure macros (see rules_init)

static Val sf_loop(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    Val bindings = pvec_to_list(car(rest));
    Val body = cdr(rest);
    Env *loop_env = env_create(&g_req, env);
    // Collect binding names + set initial values
    Val bnames = NIL, *btail = &bnames;
    Val b = bindings;
    while (val_is_cons(b)) {
        StrId name = val_as_sym(car(b)); b = cdr(b);
        Val init = eval(car(b), loop_env); b = cdr(b);
        if (g_signal) return NIL;
        env_set(loop_env, name, init);
        Val cell = cons_new(val_sym(name), NIL);
        *btail = cell; btail = &((Cons *)val_as_cons(cell))->cdr;
    }
    // Execute body, re-bind on recur
    for (;;) {
        Val result = eval_body(body, loop_env);
        if (g_signal != SIGNAL_RECUR) return result;  // normal or error
        g_signal = SIGNAL_NONE;
        Val rargs = g_signal_val;
        Val names = bnames;
        while (val_is_cons(names) && val_is_cons(rargs)) {
            env_set(loop_env, val_as_sym(car(names)), car(rargs));
            names = cdr(names); rargs = cdr(rargs);
        }
    }
}

static Val sf_recur(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    Val args = eval_list(rest, env);
    if (g_signal) return NIL;
    g_signal = SIGNAL_RECUR;
    g_signal_val = args;
    return NIL;
}

static Val sf_defmacro(Val rest, void *ctx) {
    Env *env = (Env *)ctx;
    (void)env;
    StrId name = val_as_sym(car(rest));
    Arena *saved = g_val_arena; g_val_arena = &g_perm;
    Val params = pvec_to_list(car(cdr(rest)));
    g_val_arena = saved;
    Val body = cdr(cdr(rest));
    FnObj *f = arena_push(&g_perm, FnObj);
    f->type = FN_MACRO;
    f->closure.params = params;
    f->closure.body = body;
    f->closure.env = g_global_env;
    env_set(g_global_env, name, val_fn(f));
    return val_fn(f);
}

// ============================================================================
// 4c. Eval — sig dispatch + function call
// ============================================================================

static Val eval(Val form, Env *env) {
    // Self-evaluating
    if (val_is_nil(form) || val_is_bool(form) || val_is_int(form) ||
        val_is_f64(form) || val_is_kw(form) || val_is_str(form) || val_is_fn(form))
        return form;

    // Vector literal: [a b c] → evaluate each element
    if (val_is_pvec(form)) {
        CPVec *src = (CPVec *)val_as_pvec(form);
        CPVec *dst = arena_push(&g_req, CPVec);
        *dst = cpvec_empty();
        for (u32 i = 0; i < src->count; i++) {
            Val v = eval(cpvec_get(*src, i), env);
            if (g_signal) return NIL;
            *dst = cpvec_append(*dst, v);
        }
        return val_pvec(dst);
    }

    // Map literal: {:a x :b y} → evaluate values (keys are self-evaluating)
    if (val_is_pmap(form)) {
        // For now, maps with keyword keys are data — return as-is
        // (full eval of map literals deferred until needed)
        return form;
    }

    // Symbol lookup
    if (val_is_sym(form)) {
        Val result;
        if (env_get(env, val_as_sym(form), &result)) return result;
        g_signal = SIGNAL_ERROR; g_signal_val = form;
        pf("ERROR: undefined '");
        Str s = str_from_id(val_as_sym(form));
        for (u32 i = 0; i < s.len; i++) buf_c(&g_print_buf, s.data[i]);
        pf("'\n");
        return NIL;
    }

    if (!val_is_cons(form)) return form;

    Val head = car(form);
    Val rest = cdr(form);

    // Special form dispatch via sig table
    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);
        SigFn handler = sig_get(sym);
        if (handler) {
            DISPATCH(TK_SIG, sym, g_depth);
            return handler(rest, env);
        }

        // Macro expansion: unevaluated args → expand → eval result
        Val mval;
        if (env_get(env, sym, &mval) && val_is_fn(mval)) {
            FnObj *mf = (FnObj *)val_as_fn(mval);
            if (mf->type == FN_MACRO) {
                DISPATCH(TK_MACRO, sym, g_depth);
                Val expanded = apply_fn(mval, rest);
                if (g_signal) return NIL;
                return eval(expanded, env);
            }
        }
    }

    // Function call
    Val fn_val = eval(head, env);
    if (g_signal) return NIL;
    Val args = eval_list(rest, env);
    if (g_signal) return NIL;
    if (val_is_sym(head)) {
        DISPATCH(TK_CALL, val_as_sym(head), g_depth);
    }
    return apply_fn(fn_val, args);
}

// ============================================================================
// 4d. eval_node — GNode tree walk (no entity_to_val on hot path)
//
// Same tree walk as JIT's cg_expr, different leaf action:
//   JIT:  emit x86 bytes
//   eval: compute Val
// entity_to_val only used for: quote, closure body capture, macro args
// ============================================================================

static Val eval_node(Gram *g, u32 id, Env *env);

// syntax_quote_node — walk GNode tree, return data except ~x (eval) and ~@x (splice)
static Val syntax_quote_node(Gram *g, u32 id, Env *env) {
    if (!id) return NIL;
    GNode *n = &g->nodes[id];
    switch (n->kind) {
    case NK_UNQUOTE:
        return n->child ? eval_node(g, n->child, env) : NIL;
    case NK_SPLICE:
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        pf("ERROR: splice not in list\n");
        return NIL;
    case NK_LIST: {
        Val result = NIL, *tail = &result;
        u32 c = n->child;
        while (c) {
            if (g->nodes[c].kind == NK_SPLICE) {
                Val spliced = g->nodes[c].child ? eval_node(g, g->nodes[c].child, env) : NIL;
                if (g_signal) return NIL;
                while (val_is_cons(spliced)) {
                    Val cell = cons_new(car(spliced), NIL);
                    *tail = cell; tail = &((Cons *)val_as_cons(cell))->cdr;
                    spliced = cdr(spliced);
                }
            } else {
                Val v = syntax_quote_node(g, c, env);
                if (g_signal) return NIL;
                Val cell = cons_new(v, NIL);
                *tail = cell; tail = &((Cons *)val_as_cons(cell))->cdr;
            }
            c = g->nodes[c].next;
        }
        return result;
    }
    case NK_VEC: {
        CPVec *v = arena_push(&g_req, CPVec);
        *v = cpvec_empty();
        u32 c = n->child;
        while (c) {
            if (g->nodes[c].kind == NK_SPLICE) {
                Val spliced = g->nodes[c].child ? eval_node(g, g->nodes[c].child, env) : NIL;
                if (g_signal) return NIL;
                while (val_is_cons(spliced)) {
                    *v = cpvec_append(*v, car(spliced));
                    spliced = cdr(spliced);
                }
            } else {
                *v = cpvec_append(*v, syntax_quote_node(g, c, env));
                if (g_signal) return NIL;
            }
            c = g->nodes[c].next;
        }
        return val_pvec(v);
    }
    case NK_MAP: {
        CPMap *m = arena_push(&g_req, CPMap);
        *m = cpmap_empty();
        u32 c = n->child;
        while (c) {
            Val key = syntax_quote_node(g, c, env);
            if (g_signal) return NIL;
            c = g->nodes[c].next;
            if (!c) break;
            Val val = syntax_quote_node(g, c, env);
            if (g_signal) return NIL;
            if (val_is_kw(key))       *m = cpmap_put(*m, val_as_kw(key), val);
            else if (val_is_sym(key)) *m = cpmap_put(*m, val_as_sym(key), val);
            c = g->nodes[c].next;
        }
        return val_pmap(m);
    }
    default:
        return entity_to_val(g, id);
    }
}

static Val eval_node_body(Gram *g, u32 first, Env *env) {
    Val result = NIL;
    u32 c = first;
    while (c) {
        result = eval_node(g, c, env);
        if (g_signal) return NIL;
        c = g->nodes[c].next;
    }
    return result;
}

static Val eval_node_args(Gram *g, u32 first, Env *env) {
    if (!first) return NIL;
    Val head = eval_node(g, first, env);
    if (g_signal) return NIL;
    return cons_new(head, eval_node_args(g, g->nodes[first].next, env));
}

// Capture GNode siblings as Val cons list (for closure body / macro args)
static Val gn_to_val_list(Gram *g, u32 first) {
    Val result = NIL, *tail = &result;
    u32 c = first;
    while (c) {
        Val cell = cons_new(entity_to_val(g, c), NIL);
        *tail = cell; tail = &((Cons *)val_as_cons(cell))->cdr;
        c = g->nodes[c].next;
    }
    return result;
}

static Val eval_node(Gram *g, u32 id, Env *env) {
    if (!id) return NIL;
    GNode *n = &g->nodes[id];

    // View-driven shortcuts
    if (g->analyzed) {
        if (BM_GET(g->v[V_DEAD], id)) return NIL;
        // Only shortcut integer NK_NUM — booleans/nil need proper Val types,
        // floats need gn_parse_num, comparison results need val_true/val_false
        if (n->kind == NK_NUM && BM_GET(g->v[V_INT], id))
            return val_int(g->val[id]);
    }

    switch (n->kind) {
    case NK_NUM:    return gn_parse_num(g, id);
    case NK_STR_NODE: {
        Str text = gn_text(g, id);
        Str inner = {text.data + 1, text.len >= 2 ? text.len - 2 : 0};
        Str *sp = arena_push(&g_req, Str);
        *sp = str_dup(&g_req, inner);
        return val_str(sp);
    }
    case NK_KW: {
        Str text = gn_text(g, id);
        Str name = {text.data + 1, text.len > 0 ? text.len - 1 : 0};
        return val_kw(str_intern(name));
    }
    case NK_IDENT: {
        StrId sym = gn_intern(g, id);
        if (sym == S_NIL) return NIL;
        if (sym == S_TRUE) return val_true();
        if (sym == S_FALSE) return val_false();
        Val result;
        if (env_get(env, sym, &result)) return result;
        g_signal = SIGNAL_ERROR; g_signal_val = val_sym(sym);
        pf("ERROR: undefined '");
        Str s = str_from_id(sym);
        for (u32 i = 0; i < s.len; i++) buf_c(&g_print_buf, s.data[i]);
        pf("'\n");
        return NIL;
    }
    case NK_QUOTE:
        return n->child ? entity_to_val(g, n->child) : NIL;
    case NK_SYNTAX_QUOTE:
        return n->child ? syntax_quote_node(g, n->child, env) : NIL;
    case NK_UNQUOTE:
        return n->child ? eval_node(g, n->child, env) : NIL;
    case NK_SPLICE:
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        pf("ERROR: splice outside syntax-quote\n");
        return NIL;
    case NK_VEC: {
        CPVec *v = arena_push(&g_req, CPVec);
        *v = cpvec_empty();
        u32 c = n->child;
        while (c) {
            *v = cpvec_append(*v, eval_node(g, c, env));
            if (g_signal) return NIL;
            c = g->nodes[c].next;
        }
        return val_pvec(v);
    }
    case NK_MAP: {
        CPMap *m = arena_push(&g_req, CPMap);
        *m = cpmap_empty();
        u32 c = n->child;
        while (c) {
            Val key = eval_node(g, c, env);
            c = g->nodes[c].next;
            if (!c) break;
            Val val = eval_node(g, c, env);
            if (g_signal) return NIL;
            if (val_is_kw(key))       *m = cpmap_put(*m, val_as_kw(key), val);
            else if (val_is_sym(key)) *m = cpmap_put(*m, val_as_sym(key), val);
            c = g->nodes[c].next;
        }
        return val_pmap(m);
    }
    default: break;
    }

    // List: special forms + function call
    if (n->kind != NK_LIST || !n->child) return NIL;
    u32 fc = n->child;

    if (g->nodes[fc].kind == NK_IDENT) {
        StrId sym = gn_intern(g, fc);
        u32 args = g->nodes[fc].next;

        if (sym == S_QUOTE)
            return args ? entity_to_val(g, args) : NIL;

        if (sym == S_DEF) {
            StrId name = gn_intern(g, args);
            Val value = eval_node(g, g->nodes[args].next, env);
            if (g_signal) return NIL;
            env_set(g_global_env, name, value);
            return value;
        }

        if (sym == S_DEFN) {
            StrId name = gn_intern(g, args);
            u32 pv = g->nodes[args].next;
            // Allocate params/body on g_perm so they survive arena_reset(&g_req)
            Arena *saved = g_val_arena; g_val_arena = &g_perm;
            Val params = pvec_to_list(entity_to_val(g, pv));
            Val body = gn_to_val_list(g, g->nodes[pv].next);
            g_val_arena = saved;
            Val fn = make_closure_on(&g_perm, params, body, env);
            env_set(g_global_env, name, fn);
            return fn;
        }

        if (sym == S_FN) {
            Val params = pvec_to_list(entity_to_val(g, args));
            Val body = gn_to_val_list(g, g->nodes[args].next);
            return make_closure(params, body, env);
        }

        if (sym == S_IF) {
            Val cond = eval_node(g, args, env);
            if (g_signal) return NIL;
            u32 then_n = g->nodes[args].next;
            if (val_truthy(cond))
                return eval_node(g, then_n, env);
            u32 else_n = then_n ? g->nodes[then_n].next : 0;
            return else_n ? eval_node(g, else_n, env) : NIL;
        }

        if (sym == S_DO)
            return eval_node_body(g, args, env);

        if (sym == S_LET) {
            Env *let_env = env_create(&g_req, env);
            u32 p = g->nodes[args].child;
            while (p) {
                StrId name = gn_intern(g, p);
                u32 vn = g->nodes[p].next;
                if (!vn) break;
                Val val = eval_node(g, vn, let_env);
                if (g_signal) return NIL;
                env_set(let_env, name, val);
                p = g->nodes[vn].next;
            }
            return eval_node_body(g, g->nodes[args].next, let_env);
        }

        // and/or/cond/when → macro expansion (fall through to macro check below)

        if (sym == S_LOOP) {
            Env *loop_env = env_create(&g_req, env);
            StrId bnames[64]; u32 bn = 0;
            u32 p = g->nodes[args].child;
            while (p && g->nodes[p].next) {
                StrId name = gn_intern(g, p);
                u32 vn = g->nodes[p].next;
                Val init = eval_node(g, vn, loop_env);
                if (g_signal) return NIL;
                env_set(loop_env, name, init);
                bnames[bn++] = name;
                p = g->nodes[vn].next;
            }
            u32 body_first = g->nodes[args].next;
            for (;;) {
                Val result = eval_node_body(g, body_first, loop_env);
                if (g_signal != SIGNAL_RECUR) return result;
                g_signal = SIGNAL_NONE;
                Val rargs = g_signal_val;
                for (u32 i = 0; i < bn && val_is_cons(rargs); i++) {
                    env_set(loop_env, bnames[i], car(rargs));
                    rargs = cdr(rargs);
                }
            }
        }

        if (sym == S_RECUR) {
            Val rargs = eval_node_args(g, args, env);
            if (g_signal) return NIL;
            g_signal = SIGNAL_RECUR;
            g_signal_val = rargs;
            return NIL;
        }

        if (sym == S_DEFMACRO) {
            StrId name = gn_intern(g, args);
            u32 pv = g->nodes[args].next;
            Arena *saved = g_val_arena; g_val_arena = &g_perm;
            Val params = pvec_to_list(entity_to_val(g, pv));
            Val body = gn_to_val_list(g, g->nodes[pv].next);
            g_val_arena = saved;
            FnObj *f = arena_push(&g_perm, FnObj);
            f->type = FN_MACRO;
            f->closure.params = params;
            f->closure.body = body;
            f->closure.env = g_global_env;
            env_set(g_global_env, name, val_fn(f));
            return val_fn(f);
        }

        // Macro expansion
        Val mval;
        if (env_get(env, sym, &mval) && val_is_fn(mval)) {
            FnObj *mf = (FnObj *)val_as_fn(mval);
            if (mf->type == FN_MACRO) {
                DISPATCH(TK_MACRO, sym, g_depth);
                Val rest = gn_to_val_list(g, args);
                Val expanded = apply_fn(mval, rest);
                if (g_signal) return NIL;
                return eval(expanded, env);
            }
        }
    }

    // Function call
    Val fn_val = eval_node(g, fc, env);
    if (g_signal) return NIL;
    Val fn_args = eval_node_args(g, g->nodes[fc].next, env);
    if (g_signal) return NIL;
    if (g->nodes[fc].kind == NK_IDENT) {
        DISPATCH(TK_CALL, gn_intern(g, fc), g_depth);
    }
    return apply_fn(fn_val, fn_args);
}

static Val eval_string(const char *s, Env *env) {
    Gram *g = world_step(s, true);
    Val result = NIL;
    u32 c = g->nodes[0].child;
    while (c) {
        result = eval_node(g, c, env);
        if (g_signal) break;
        c = g->nodes[c].next;
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

static Val bi_bitxor(Val args) {
    return val_int(val_as_int(car(args)) ^ val_as_int(car(cdr(args))));
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
static Val bi_seq(Val args)    { return p_seq(car(args)); }
// list → boot.clj
static Val bi_count(Val args)  { return p_count(car(args)); }
static Val bi_type(Val args)   { return val_int(val_tag_idx(car(args))); }
static Val bi_nilq(Val args)   { return val_bool(val_is_nil(car(args))); }
// not → boot.clj
static Val bi_consq(Val args)  { return val_bool(val_is_cons(car(args))); }
// int?, fn?, symbol?, vector?, map?, keyword?, string? → boot.clj

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

static Val bi_print(Val args) {
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
// map, filter, reduce → boot.clj

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

// nth → boot.clj

// vec → boot.clj

// hash-map → boot.clj

// keys, vals → boot.clj (via pmap seq returning [k v] entries)

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

// empty? → boot.clj


// into → boot.clj

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

// inc, dec, zero?, pos?, neg? → boot.clj

// vector?, map?, keyword?, string? → boot.clj (via type builtin)
static Val bi_macroq(Val args) {
    Val v = car(args);
    if (!val_is_fn(v)) return val_false();
    return val_bool(((FnObj *)val_as_fn(v))->type == FN_MACRO);
}

// --- Macro support ---
static u32 g_gensym_counter;

static Val bi_gensym(Val args) {
    (void)args;
    char buf[32];
    u32 n = g_gensym_counter++;
    buf[0] = 'G'; buf[1] = '_'; buf[2] = '_';
    u32 pos = 3;
    if (n == 0) { buf[pos++] = '0'; }
    else {
        char digits[10]; u32 dlen = 0;
        while (n > 0) { digits[dlen++] = '0' + (char)(n % 10); n /= 10; }
        while (dlen > 0) buf[pos++] = digits[--dlen];
    }
    return val_sym(str_intern((Str){(u8 *)buf, pos}));
}

static Val bi_macroexpand1(Val args) {
    Val form = car(args);
    if (!val_is_cons(form)) return form;
    Val head = car(form);
    if (!val_is_sym(head)) return form;
    Val fn_val;
    if (!env_get(g_global_env, val_as_sym(head), &fn_val)) return form;
    if (!val_is_fn(fn_val)) return form;
    FnObj *f = (FnObj *)val_as_fn(fn_val);
    if (f->type != FN_MACRO) return form;
    return apply_fn(fn_val, cdr(form));
}

// --- Range (returns pvec for cache-friendly iteration) ---
// range → boot.clj

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
// 5b. Bit operations — foundation for data structures in Clojure
// ============================================================================

static Val bi_bitand(Val a) { return val_int(val_as_int(car(a)) & val_as_int(car(cdr(a)))); }
static Val bi_bitor(Val a)  { return val_int(val_as_int(car(a)) | val_as_int(car(cdr(a)))); }
static Val bi_bitnot(Val a) { return val_int(~val_as_int(car(a))); }
static Val bi_bitshl(Val a) { return val_int(val_as_int(car(a)) << val_as_int(car(cdr(a)))); }
static Val bi_bitshr(Val a) { return val_int((i64)((u64)val_as_int(car(a)) >> (u64)val_as_int(car(cdr(a))))); }
static Val bi_popcount(Val a) { return val_int((i64)__builtin_popcountll((u64)val_as_int(car(a)))); }
static Val bi_hash32_bi(Val a) { return val_int((i64)hash32((u32)val_as_int(car(a)))); }

// ============================================================================
// 5c. Mem — raw memory for self-hosting data structures
//
// Mem = {u8 *base, u32 used, u32 cap}. Three ops: bump, restore, copy.
// This is the "syscall" layer. Everything above (HAMT, pvec) is Clojure.
// ============================================================================

#define MEM_POOL_MAX 16
static struct { u8 *base; u32 used, cap; } g_mem_pool[MEM_POOL_MAX];
static u32 g_mem_pool_count;

static Val bi_mem_new(Val a) {
    u32 cap = (u32)val_as_int(car(a));
    u32 idx = g_mem_pool_count++;
    g_mem_pool[idx].base = (u8 *)sys_alloc(cap);
    g_mem_pool[idx].used = 0;
    g_mem_pool[idx].cap = cap;
    return val_int(idx);
}
static Val bi_mem_bump(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 size = (u32)val_as_int(car(cdr(a)));
    size = (size + 7) & ~7u;  // align to 8
    u32 off = g_mem_pool[idx].used;
    g_mem_pool[idx].used += size;
    return val_int(off);
}
static Val bi_mem_used(Val a) {
    return val_int(g_mem_pool[(u32)val_as_int(car(a))].used);
}
static Val bi_mem_restore(Val a) {
    g_mem_pool[(u32)val_as_int(car(a))].used = (u32)val_as_int(car(cdr(a)));
    return NIL;
}
static Val bi_mem_u32_get(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 off = (u32)val_as_int(car(cdr(a)));
    return val_int(*(u32 *)(g_mem_pool[idx].base + off));
}
static Val bi_mem_u32_set(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 off = (u32)val_as_int(car(cdr(a)));
    *(u32 *)(g_mem_pool[idx].base + off) = (u32)val_as_int(car(cdr(cdr(a))));
    return NIL;
}
static Val bi_mem_val_get(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 off = (u32)val_as_int(car(cdr(a)));
    return *(Val *)(g_mem_pool[idx].base + off);
}
static Val bi_mem_val_set(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 off = (u32)val_as_int(car(cdr(a)));
    *(Val *)(g_mem_pool[idx].base + off) = car(cdr(cdr(a)));
    return NIL;
}
static Val bi_mem_copy(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 dst = (u32)val_as_int(car(cdr(a)));
    u32 src = (u32)val_as_int(car(cdr(cdr(a))));
    u32 n   = (u32)val_as_int(car(cdr(cdr(cdr(a)))));
    memcpy(g_mem_pool[idx].base + dst, g_mem_pool[idx].base + src, n);
    return NIL;
}
static Val bi_mem_fill(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    u32 off = (u32)val_as_int(car(cdr(a)));
    u32 val = (u32)val_as_int(car(cdr(cdr(a))));
    u32 n   = (u32)val_as_int(car(cdr(cdr(cdr(a)))));
    memset(g_mem_pool[idx].base + off, (int)val, n);
    return NIL;
}
static Val bi_mem_base(Val a) {
    u32 idx = (u32)val_as_int(car(a));
    return val_int((i64)(u64)g_mem_pool[idx].base);
}

// Raw memory ops (absolute pointer) — for JIT interop
static Val bi_load64(Val a)  { return val_int(*(i64 *)(u64)val_as_int(car(a))); }
static Val bi_load32(Val a)  { return val_int((i64)*(u32 *)(u64)val_as_int(car(a))); }
static Val bi_load8(Val a)   { return val_int((i64)*(u8 *)(u64)val_as_int(car(a))); }
static Val bi_store64(Val a) { *(i64 *)(u64)val_as_int(car(a)) = val_as_int(car(cdr(a))); return NIL; }
static Val bi_store32(Val a) { *(u32 *)(u64)val_as_int(car(a)) = (u32)val_as_int(car(cdr(a))); return NIL; }
static Val bi_store8(Val a)  { *(u8 *)(u64)val_as_int(car(a)) = (u8)val_as_int(car(cdr(a))); return NIL; }
// Source access — gram source pointer + length
static Val bi_gram_src_ptr(Val a) { (void)a; return val_int((i64)(u64)g_world.gram.src); }
static Val bi_gram_src_len(Val a) { (void)a; return val_int((i64)g_world.gram.src_len); }

// ============================================================================
// 5d. Output Buffer — shared with cc.c
// ============================================================================

static char g_out_data[1 << 20];
static OutBuf g_out;

static void out_reset(void) {
    g_out = (OutBuf){g_out_data, 0, sizeof(g_out_data)};
}

// ============================================================================
// 6. Engine builtins — Clojure ↔ GNode bridge
//
// Give Clojure code read/write access to the GNode world.
// Rules written in Clojure can query node structure, check/set view bits,
// and compute values — same operations as the C analysis passes.
// ============================================================================

// --- GNode access ---

static Val bi_gn_kind(Val args) {
    return val_int(g_world.gram.nodes[(u32)val_as_int(car(args))].kind);
}
static Val bi_gn_child(Val args) {
    return val_int(g_world.gram.nodes[(u32)val_as_int(car(args))].child);
}
static Val bi_gn_next(Val args) {
    return val_int(g_world.gram.nodes[(u32)val_as_int(car(args))].next);
}
static Val bi_gn_parent(Val args) {
    return val_int(g_world.gram.nodes[(u32)val_as_int(car(args))].parent);
}
static Val bi_gn_sym(Val args) {
    return val_int(gn_intern(&g_world.gram, (u32)val_as_int(car(args))));
}
static Val bi_gn_count(Val args) {
    (void)args;
    return val_int(g_world.gram.n);
}
static Val bi_gn_parse_int(Val args) {
    return val_int(gn_parse_int(&g_world.gram, (u32)val_as_int(car(args))));
}
static Val bi_gn_has_dot(Val args) {
    Str t = gn_text(&g_world.gram, (u32)val_as_int(car(args)));
    for (u32 i = 0; i < t.len; i++) if (t.data[i] == '.') return val_true();
    return val_false();
}
static Val bi_gn_text(Val args) {
    Str t = gn_text(&g_world.gram, (u32)val_as_int(car(args)));
    Str *sp = arena_push(&g_req, Str);
    *sp = str_dup(&g_req, t);
    return val_str(sp);
}
static Val bi_sym_name(Val args) {
    Str s = str_from_id((StrId)val_as_int(car(args)));
    Str *sp = arena_push(&g_req, Str);
    *sp = str_dup(&g_req, s);
    return val_str(sp);
}
static Val bi_gram_render(Val args) {
    Gram *g = &g_world.gram;
    i64 which = val_is_nil(args) ? 0 : val_as_int(car(args));
    Lang l;
    if (which == 1) lang_c(&l);
    else lang_lisp(&l);
    gram_render(g, &l);
    return NIL;
}
static Val bi_gram_outline(Val args) {
    (void)args;
    gram_render_outline(&g_world.gram);
    return NIL;
}

// --- View/bitmask ops ---

static Val bi_view_get(Val args) {
    u32 vi = (u32)val_as_int(car(args));
    u32 id = (u32)val_as_int(car(cdr(args)));
    return val_bool(BM_GET(g_world.gram.v[vi], id));
}
static Val bi_view_set(Val args) {
    u32 vi = (u32)val_as_int(car(args));
    u32 id = (u32)val_as_int(car(cdr(args)));
    BM_SET(g_world.gram.v[vi], id);
    return NIL;
}
static Val bi_val_get(Val args) {
    return val_int(g_world.gram.val[(u32)val_as_int(car(args))]);
}
static Val bi_val_set(Val args) {
    u32 id = (u32)val_as_int(car(args));
    g_world.gram.val[id] = val_as_int(car(cdr(args)));
    return NIL;
}

// --- Scope/bind arrays ---

static Val bi_scope_get(Val args) {
    return val_int(g_world.gram.scope[(u32)val_as_int(car(args))]);
}
static Val bi_scope_set(Val args) {
    u32 id = (u32)val_as_int(car(args));
    g_world.gram.scope[id] = (u32)val_as_int(car(cdr(args)));
    return NIL;
}
static Val bi_bind_get(Val args) {
    return val_int(g_world.gram.bind[(u32)val_as_int(car(args))]);
}
static Val bi_bind_set(Val args) {
    u32 id = (u32)val_as_int(car(args));
    g_world.gram.bind[id] = (u32)val_as_int(car(cdr(args)));
    return NIL;
}

// --- GNode end (subtree boundary) ---

static Val bi_gn_end(Val args) {
    return val_int(g_world.gram.nodes[(u32)val_as_int(car(args))].end);
}

// --- Symbol properties ---

static Val bi_pure_biq(Val args) {
    return val_bool(is_pure_bi((StrId)val_as_int(car(args))));
}
static Val bi_int_retq(Val args) {
    return val_bool(is_int_ret((StrId)val_as_int(car(args))));
}
static Val bi_specialq(Val args) {
    return val_bool(is_special((StrId)val_as_int(car(args))));
}

// --- Compound queries ---

static Val bi_all_args_view(Val args) {
    u32 fc = (u32)val_as_int(car(args));
    u32 vi = (u32)val_as_int(car(cdr(args)));
    u32 a = g_world.gram.nodes[fc].next;
    while (a) {
        if (!BM_GET(g_world.gram.v[vi], a)) return val_false();
        a = g_world.gram.nodes[a].next;
    }
    return val_true();
}
static Val bi_cv_eval(Val args) {
    StrId sym = (StrId)val_as_int(car(args));
    u32 fc = (u32)val_as_int(car(cdr(args)));
    return val_int(cv_eval(&g_world.gram, sym, g_world.gram.nodes[fc].next));
}

// --- Engine: eval Clojure against current world ---
// Uses a separate gram for parsing expressions (preserves g_world)

static Val engine_eval(const char *src) {
    static Lang l;
    static bool l_init;
    if (!l_init) { lang_lisp(&l); l_init = true; }
    u32 len = (u32)strlen(src);
    Gram g = gram_new(len < 256 ? 16384 : len * 4);
    gram_parse(&g, &l, src, len);
    gram_index(&g);
    Val result = NIL;
    u32 c = g.nodes[0].child;
    while (c) {
        result = eval_node(&g, c, g_global_env);
        if (g_signal) break;
        c = g.nodes[c].next;
    }
    // gram allocated on g_perm — no free needed (init-time only)
    return result;
}

// engine_eval_file — load and evaluate a .clj file
static bool engine_eval_file(const char *path) {
    FileData f = sys_read_file(path, sys_alloc);
    if (!f.data) return false;
    engine_eval(f.data);
    sys_free(f.data, f.len + 1);
    return true;
}

// Forward-declare: JIT builtins registered from jit.c (included after eval.c)
static void register_jit_builtins(Env *env);

// gn-has-recur? — expose to Clojure
static Val bi_gn_has_recur(Val a) {
    return val_bool(gn_has_recur(&g_world.gram, (u32)val_as_int(car(a))));
}

// ============================================================================
// 6b. File + Output builtins — for Clojure load + emitters
// ============================================================================

static Val bi_load(Val args) {
    Val path_val = car(args);
    if (!val_is_str(path_val)) {
        pf("ERROR: load expects string path\n");
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        return NIL;
    }
    Str *path_str = val_as_str(path_val);
    // Null-terminate path for C API
    char path[512];
    u32 plen = path_str->len < 511 ? path_str->len : 511;
    memcpy(path, path_str->data, plen);
    path[plen] = '\0';
    FileData f = sys_read_file(path, sys_alloc);
    if (!f.data) {
        pf("ERROR: cannot read '%s'\n", path);
        g_signal = SIGNAL_ERROR; g_signal_val = NIL;
        return NIL;
    }
    Val result = engine_eval(f.data);
    sys_free(f.data, f.len + 1);
    return result;
}

static Val bi_out_bang(Val args) {
    Val s = car(args);
    if (val_is_str(s)) {
        Str *sp = val_as_str(s);
        buf_n(&g_out, (const char *)sp->data, sp->len);
    } else {
        const char *p = print_val(s);
        buf_s(&g_out, p);
    }
    return NIL;
}

static Val bi_out_nl(Val args) {
    (void)args;
    buf_c(&g_out, '\n');
    return NIL;
}

static Val bi_out_name(Val args) {
    // Emit PHP-mangled name from StrId
    StrId id = (StrId)val_as_int(car(args));
    Str s = str_from_id(id);
    for (u32 i = 0; i < s.len; i++) {
        u8 c = s.data[i];
        switch (c) {
            case '-': buf_c(&g_out, '_'); break;
            case '?': buf_s(&g_out, "_p"); break;
            case '!': buf_s(&g_out, "_x"); break;
            case '*': buf_s(&g_out, "_s"); break;
            case '<': buf_s(&g_out, "_lt"); break;
            case '>': buf_s(&g_out, "_gt"); break;
            case '=': buf_s(&g_out, "_eq"); break;
            case '/': buf_s(&g_out, "_sl"); break;
            default:  buf_c(&g_out, c); break;
        }
    }
    return NIL;
}

static Val bi_out_reset(Val args) {
    (void)args;
    out_reset();
    return NIL;
}

static Val bi_str_intern(Val args) {
    Val s = car(args);
    if (!val_is_str(s)) return NIL;
    Str *sp = val_as_str(s);
    return val_int(str_intern(*sp));
}

// str-parent: hierarchical name walk for sig dispatch
// "page:/about" → "page:" (strip after last '/')
// "page:"       → "page"  (strip after last ':')
// "page"        → "*"     (no more separators)
// "*"           → "*"     (fixed point)
static Val bi_str_parent(Val args) {
    StrId id = (StrId)val_as_int(car(args));
    Str s = str_from_id(id);
    // Find last separator (/ then :)
    i32 cut = -1;
    for (i32 i = (i32)s.len - 1; i >= 0; i--) {
        if (s.data[i] == '/') { cut = i; break; }
    }
    if (cut < 0) {
        for (i32 i = (i32)s.len - 1; i >= 0; i--) {
            if (s.data[i] == ':') { cut = i; break; }
        }
    }
    if (cut >= 0) {
        return val_int(str_intern((Str){s.data, (u32)cut}));
    }
    // No separator — return wildcard '*'
    return val_int(S_MUL);
}

static Val bi_out_str(Val args) {
    (void)args;
    Str *sp = arena_push(&g_req, Str);
    *sp = str_dup(&g_req, (Str){(u8 *)g_out.buf, g_out.pos});
    return val_str(sp);
}

// ============================================================================
// 8. Register builtins + init
// ============================================================================

static void register_builtins(Env *env) {
    #define REG(n, f) env_set(env, INTERN(n), make_builtin(INTERN(n), f))
    REG("+", bi_add);  REG("-", bi_sub);  REG("*", bi_mul);  REG("/", bi_div);
    REG("mod", bi_mod);  REG("bit-xor", bi_bitxor);
    // inc, dec, zero?, pos?, neg? → boot.clj
    REG("=", bi_eq);  REG("<", bi_lt);  REG(">", bi_gt);
    REG("<=", bi_lte);  REG(">=", bi_gte);
    REG("cons", bi_cons);  REG("first", bi_first);  REG("rest", bi_rest);
    REG("seq", bi_seq);  REG("count", bi_count);
    // list → boot.clj
    REG("type", bi_type);
    REG("nil?", bi_nilq);  REG("cons?", bi_consq);
    // not, empty?, int?, fn?, symbol?, vector?, map?, keyword?, string? → boot.clj
    REG("print", bi_print);  REG("println", bi_println);  REG("pr-str", bi_prstr);
    // map, filter, reduce → boot.clj
    REG("apply", bi_apply);
    // Collections
    REG("get", bi_get);  REG("assoc", bi_assoc);  REG("conj", bi_conj);
    // nth → boot.clj
    // vec, hash-map → boot.clj
    // keys, vals → boot.clj
    REG("contains?", bi_containsq);
    // into → boot.clj
    REG("str", bi_str);
    REG("macro?", bi_macroq);
    // range → boot.clj
    // Transient operations
    REG("transient", bi_transient);  REG("persistent!", bi_persistent);
    REG("assoc!", bi_assocbang);
    // Macros
    REG("gensym", bi_gensym);
    REG("macroexpand-1", bi_macroexpand1);
    // Bit ops
    REG("bit-and", bi_bitand);  REG("bit-or", bi_bitor);
    REG("bit-not", bi_bitnot);  REG("bit-shift-left", bi_bitshl);
    REG("bit-shift-right", bi_bitshr);
    REG("popcount", bi_popcount);  REG("hash32", bi_hash32_bi);
    // Mem — raw memory for self-hosting data structures
    REG("mem-new!", bi_mem_new);    REG("mem-bump!", bi_mem_bump);
    REG("mem-used", bi_mem_used);   REG("mem-restore!", bi_mem_restore);
    REG("mem-u32@", bi_mem_u32_get);  REG("mem-u32!", bi_mem_u32_set);
    REG("mem-val@", bi_mem_val_get);  REG("mem-val!", bi_mem_val_set);
    REG("mem-copy!", bi_mem_copy);    REG("mem-fill!", bi_mem_fill);
    REG("mem-base", bi_mem_base);
    // Raw memory ops (absolute pointer) — JIT-compatible
    REG("load64", bi_load64);  REG("load32", bi_load32);  REG("load8", bi_load8);
    REG("store64", bi_store64);  REG("store32", bi_store32);  REG("store8", bi_store8);
    REG("store64!", bi_store64);  REG("store32!", bi_store32);  REG("store8!", bi_store8);
    REG("gram-src-ptr", bi_gram_src_ptr);  REG("gram-src-len", bi_gram_src_len);
    // Engine: GNode access
    REG("gn-kind", bi_gn_kind);     REG("gn-child", bi_gn_child);
    REG("gn-next", bi_gn_next);     REG("gn-parent", bi_gn_parent);
    REG("gn-sym", bi_gn_sym);       REG("gn-count", bi_gn_count);
    REG("gn-parse-int", bi_gn_parse_int);  REG("gn-has-dot", bi_gn_has_dot);
    REG("gn-end", bi_gn_end);   REG("gn-text", bi_gn_text);
    REG("sym-name", bi_sym_name);
    // Grammar: render through different surface grammars
    REG("gram-render", bi_gram_render);  REG("gram-outline", bi_gram_outline);
    // Engine: View/bitmask ops
    REG("view?", bi_view_get);      REG("view-set!", bi_view_set);
    REG("val-get", bi_val_get);     REG("val-set!", bi_val_set);
    REG("scope-get", bi_scope_get); REG("scope-set!", bi_scope_set);
    REG("bind-get", bi_bind_get);   REG("bind-set!", bi_bind_set);
    // Engine: Symbol properties
    REG("pure-bi?", bi_pure_biq);   REG("int-ret?", bi_int_retq);
    REG("special?", bi_specialq);
    // Engine: Compound queries
    REG("all-args-view?", bi_all_args_view);  REG("cv-eval", bi_cv_eval);
    // Engine: GNode helpers
    REG("gn-has-recur?", bi_gn_has_recur);
    // File + output builtins
    REG("load", bi_load);
    REG("out-emit", bi_out_bang);  REG("out-nl", bi_out_nl);
    REG("out-name", bi_out_name);
    REG("out-reset", bi_out_reset);  REG("out-str", bi_out_str);
    REG("str-intern", bi_str_intern);
    REG("str-parent", bi_str_parent);
    #undef REG

    // Engine constants
    #define CON(n, v) env_set(env, INTERN(n), val_int(v))
    // Type tag indices (for type builtin)
    CON("T_NIL", TI_NIL_T);  CON("T_BOOL", TI_BOOL);  CON("T_INT", TI_INT);
    CON("T_SYM", TI_SYM);    CON("T_KW", TI_KW);      CON("T_STR", TI_STR);
    CON("T_PMAP", TI_PMAP);  CON("T_PVEC", TI_PVEC);  CON("T_FN", TI_FN);
    CON("T_CONS", TI_CONS);  CON("T_F64", TI_F64);
    // Node kinds
    CON("NK_ROOT", NK_ROOT);     CON("NK_LIST", NK_LIST);
    CON("NK_VEC", NK_VEC);       CON("NK_MAP", NK_MAP);
    CON("NK_QUOTE", NK_QUOTE);   CON("NK_SYNTAX_QUOTE", NK_SYNTAX_QUOTE);
    CON("NK_UNQUOTE", NK_UNQUOTE);  CON("NK_SPLICE", NK_SPLICE);
    CON("NK_IDENT", NK_IDENT);   CON("NK_NUM", NK_NUM);
    CON("NK_STR", NK_STR_NODE);  CON("NK_OP", NK_OP);
    CON("NK_KW", NK_KW);
    // Engine constants: view indices
    CON("V_DEF", V_DEF);         CON("V_REF", V_REF);
    CON("V_CALL", V_CALL);       CON("V_TAIL", V_TAIL);
    CON("V_PURE", V_PURE);       CON("V_CONST", V_CONST);
    CON("V_DEAD", V_DEAD);       CON("V_INT", V_INT);
    CON("V_VEC", V_VEC);         CON("V_MAP", V_MAP);
    CON("V_FN", V_FN);
    CON("V_ALLOC", V_ALLOC);     CON("V_SCOPE", V_SCOPE);
    CON("V_DYNAMIC", V_DYNAMIC); CON("V_LIVE", V_LIVE);
    // Engine constants: well-known symbols
    CON("SYM_NIL", S_NIL);       CON("SYM_TRUE", S_TRUE);
    CON("SYM_FALSE", S_FALSE);   CON("SYM_FN", S_FN);
    CON("SYM_DEF", S_DEF);       CON("SYM_DEFN", S_DEFN);
    CON("SYM_IF", S_IF);         CON("SYM_LET", S_LET);
    CON("SYM_DO", S_DO);         CON("SYM_LOOP", S_LOOP);
    CON("SYM_QUOTE", S_QUOTE);
    CON("SYM_AND", S_AND);       CON("SYM_OR", S_OR);
    CON("SYM_COND", S_COND);     CON("SYM_WHEN", S_WHEN);
    CON("SYM_DEFMACRO", S_DEFMACRO);
    CON("SYM_SYNTAX_QUOTE", S_SYNTAX_QUOTE);
    CON("SYM_ADD", S_ADD);       CON("SYM_SUB", S_SUB);
    CON("SYM_MUL", S_MUL);       CON("SYM_DIV", S_DIV);
    CON("SYM_MOD", S_MOD);       CON("SYM_EQ", S_EQ);
    CON("SYM_LT", S_LT);         CON("SYM_GT", S_GT);
    CON("SYM_LTE", S_LTE);       CON("SYM_GTE", S_GTE);
    CON("SYM_NOT", S_NOT);       CON("SYM_INC", S_INC);
    CON("SYM_DEC", S_DEC);
    CON("SYM_ZEROQ", S_ZEROQ);   CON("SYM_POSQ", S_POSQ);
    CON("SYM_NEGQ", S_NEGQ);
    CON("SYM_RECUR", S_RECUR);   CON("SYM_PRINTLN", S_PRINTLN);
    CON("SYM_STAR", S_MUL);
    // Bit ops + memory (for JIT emitter)
    CON("SYM_BAND", S_BAND);     CON("SYM_BOR", S_BOR);
    CON("SYM_BXOR", S_BXOR);     CON("SYM_BNOT", S_BNOT);
    CON("SYM_BSHL", S_BSHL);     CON("SYM_BSHR", S_BSHR);
    CON("SYM_POPCNT", S_POPCNT);
    CON("SYM_LOAD64", S_LOAD64);  CON("SYM_LOAD32", S_LOAD32);  CON("SYM_LOAD8", S_LOAD8);
    CON("SYM_STORE64", S_STORE64); CON("SYM_STORE32", S_STORE32); CON("SYM_STORE8", S_STORE8);
    // x86 registers
    CON("RAX", RAX);  CON("RCX", RCX);  CON("RDX", RDX);  CON("RBX", RBX);
    CON("RSP", RSP);  CON("RBP", RBP);  CON("RSI", RSI);  CON("RDI", RDI);
    CON("R12", 12);   CON("R13", 13);   CON("R14", 14);   CON("R15", 15);
    // x86 condition codes
    CON("CC_E", CC_E);   CON("CC_NE", CC_NE);
    CON("CC_L", CC_L);   CON("CC_GE", CC_GE);
    CON("CC_LE", CC_LE); CON("CC_G", CC_G);
    // MODRM helper constant
    CON("N_CS", 5);  // Callee-saved pool size (defined in jit.c)
    #undef CON

    // JIT builtins registered from jit.c (included after eval.c in unity build)
    register_jit_builtins(env);
}

// syntax_quote_val — Val-world syntax-quote (for macro expansion)
// Walks Val form, returns data except (unquote x) and (unquote-splicing x)
static Val syntax_quote_val(Val form, Env *env) {
    // Vectors: recurse into elements, handle splice
    if (val_is_pvec(form)) {
        CPVec *src = (CPVec *)val_as_pvec(form);
        CPVec *dst = arena_push(&g_req, CPVec);
        *dst = cpvec_empty();
        for (u32 i = 0; i < src->count; i++) {
            Val elem = cpvec_get(*src, i);
            if (val_is_cons(elem) && val_is_sym(car(elem)) &&
                val_as_sym(car(elem)) == S_UNQUOTE_SPLICING) {
                Val spliced = eval(car(cdr(elem)), env);
                if (g_signal) return NIL;
                while (val_is_cons(spliced)) {
                    *dst = cpvec_append(*dst, car(spliced));
                    spliced = cdr(spliced);
                }
            } else {
                *dst = cpvec_append(*dst, syntax_quote_val(elem, env));
                if (g_signal) return NIL;
            }
        }
        return val_pvec(dst);
    }

    // Non-list, non-vec: return as-is (data)
    if (!val_is_cons(form)) return form;

    // Check for (unquote x) and (unquote-splicing x)
    Val head = car(form);
    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);
        if (sym == S_UNQUOTE) return eval(car(cdr(form)), env);
        if (sym == S_UNQUOTE_SPLICING) {
            g_signal = SIGNAL_ERROR; g_signal_val = NIL;
            pf("ERROR: splice not in list\n");
            return NIL;
        }
    }

    // List: process each element, handle splicing
    Val result = NIL, *tail = &result;
    Val cur = form;
    while (val_is_cons(cur)) {
        Val elem = car(cur);
        if (val_is_cons(elem) && val_is_sym(car(elem)) &&
            val_as_sym(car(elem)) == S_UNQUOTE_SPLICING) {
            Val spliced = eval(car(cdr(elem)), env);
            if (g_signal) return NIL;
            while (val_is_cons(spliced)) {
                Val cell = cons_new(car(spliced), NIL);
                *tail = cell; tail = &((Cons *)val_as_cons(cell))->cdr;
                spliced = cdr(spliced);
            }
        } else {
            Val v = syntax_quote_val(elem, env);
            if (g_signal) return NIL;
            Val cell = cons_new(v, NIL);
            *tail = cell; tail = &((Cons *)val_as_cons(cell))->cdr;
        }
        cur = cdr(cur);
    }
    return result;
}

static Val sf_syntax_quote(Val rest, void *ctx) {
    return syntax_quote_val(car(rest), (Env *)ctx);
}

// ============================================================================
// 8. Rules — analysis passes written in Clojure
//
// These replace the C pass_scope/pass_type/pass_flow functions.
// Evaluated once at init, called by gram_analyze via the engine.
// ============================================================================

static bool g_rules_ready;

static void rules_init(void) {
    if (g_rules_ready) return;
    g_rules_ready = true;
    engine_eval_file("src/clj/genera.clj");
}

static void eval_init(void) {
    g_global_env = env_create(&g_perm, NULL);

    // Special forms → sig table
    sig_on(S_QUOTE, sf_quote, SIG_SPECIAL);
    sig_on(S_DEF,   sf_def,   SIG_SPECIAL);
    sig_on(S_DEFN,  sf_defn,  SIG_SPECIAL);
    sig_on(S_FN,    sf_fn,    SIG_SPECIAL);
    sig_on(S_IF,    sf_if,    SIG_SPECIAL);
    sig_on(S_DO,    sf_do,    SIG_SPECIAL);
    sig_on(S_LET,   sf_let,   SIG_SPECIAL);
    // and/or/cond/when → Clojure macros (defined in rules_init)
    sig_on(S_LOOP,  sf_loop,  SIG_SPECIAL);
    sig_on(S_RECUR, sf_recur, SIG_SPECIAL);
    sig_on(S_DEFMACRO, sf_defmacro, SIG_SPECIAL);
    sig_on(S_SYNTAX_QUOTE, sf_syntax_quote, SIG_SPECIAL);

    // Builtins → env (first-class values, can be passed to HOFs)
    register_builtins(g_global_env);

    // Clojure analysis rules (defines pass functions in global env)
    rules_init();
}

#endif // EVAL_C_INCLUDED
