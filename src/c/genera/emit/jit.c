/**
 * jit.c — Clojure → x86-64 JIT Compiler
 *
 * Callee-saved register allocator, expression compiler, program compiler.
 * Source → intern → compile → mmap → execute. No gcc. No assembler.
 *
 * Depends on: emit/x86.c (encoding), lang/ (read, classify, image)
 */
#ifndef JIT_C_INCLUDED
#define JIT_C_INCLUDED

// ============================================================================
// 1. Compiler State + Register Allocator
// ============================================================================

// Callee-saved pool: rbx, r12, r13, r14, r15
enum { R12=12, R13=13, R14=14, R15=15 };
static const u8 CS_POOL[] = {RBX, R12, R13, R14, R15};
#define N_CS 5

typedef struct { StrId name; i32 offset; } Local;

typedef struct {
    CodeBuf *cb;
    Local locals[64];
    u32 local_count;
    i32 next_slot;
    u8  cs_param;
    u8  cs_temp;
    u8  cs_max;
    StrId cs_names[N_CS];
    bool cs_mode;
    StrId loop_vars[16];
    i32   loop_offs[16];
    u32   loop_count;
    u32   loop_start;
    bool  in_loop;
} Comp;

static i8 find_cs(Comp *cc, StrId name) {
    for (u32 i = 0; i < cc->cs_param; i++)
        if (cc->cs_names[i] == name) return (i8)CS_POOL[i];
    return -1;
}

static i8 alloc_cs_temp(Comp *cc) {
    u32 idx = cc->cs_param + cc->cs_temp;
    if (idx >= N_CS) return -1;
    cc->cs_temp++;
    if (idx + 1 > cc->cs_max) cc->cs_max = idx + 1;
    return (i8)CS_POOL[idx];
}
static void free_cs_temp(Comp *cc) { cc->cs_temp--; }

// ============================================================================
// 2. O(1) Function + Global Lookup
// ============================================================================

#define STRID_MAX 4096
static i32 g_fn_offset[STRID_MAX];
static u16 g_fn_gen[STRID_MAX];
static u16 g_gen;

typedef struct { u32 pos; StrId target; } CallFix;
static CallFix g_fixes[4096];
static u32 g_fix_count;

static i64 g_globals[256];
static i32 g_global_idx[STRID_MAX];
static u16 g_global_gen[STRID_MAX];
static u32 g_global_count;

ALWAYS_INLINE i32 find_fn(StrId name) {
    return (name < STRID_MAX && g_fn_gen[name] == g_gen) ? g_fn_offset[name] : -1;
}

static i32 find_local(Comp *cc, StrId name) {
    for (i32 i = (i32)cc->local_count - 1; i >= 0; i--)
        if (cc->locals[i].name == name) return cc->locals[i].offset;
    return 0;
}

ALWAYS_INLINE i32 find_global(StrId name) {
    return (name < STRID_MAX && g_global_gen[name] == g_gen) ? g_global_idx[name] : -1;
}

static i32 alloc_slot(Comp *cc, StrId name) {
    cc->next_slot -= 8;
    cc->locals[cc->local_count] = (Local){name, cc->next_slot};
    cc->local_count++;
    return cc->next_slot;
}

// ============================================================================
// 3. Expression Compiler
// ============================================================================

static void c_expr(Comp *cc, Val form);
static void c_tail(Comp *cc, Val form);

static i32 cmp_cc(StrId op) {
    if (op == S_EQ)  return CC_E;
    if (op == S_LT)  return CC_L;
    if (op == S_GT)  return CC_G;
    if (op == S_LTE) return CC_LE;
    if (op == S_GTE) return CC_GE;
    return -1;
}

static void c_binop(Comp *cc, StrId op, Val args) {
    u32 n = list_len(args);
    if (n == 1 && op == S_SUB) { c_expr(cc, car(args)); x_neg(cc->cb, RAX); return; }
    if (n == 0) { x_imm(cc->cb, RAX, (op == S_MUL) ? 1 : 0); return; }
    c_expr(cc, car(args));
    args = cdr(args);
    while (val_is_cons(args)) {
        x_push(cc->cb, RAX);
        c_expr(cc, car(args));
        x_mov(cc->cb, RCX, RAX);
        x_pop(cc->cb, RAX);
        if (op == S_ADD)      x_add(cc->cb, RAX, RCX);
        else if (op == S_SUB) x_sub(cc->cb, RAX, RCX);
        else if (op == S_MUL) x_imul(cc->cb, RAX, RCX);
        else if (op == S_DIV) x_idiv(cc->cb, RCX);
        else if (op == S_MOD) { x_idiv(cc->cb, RCX); x_mov(cc->cb, RAX, RDX); }
        args = cdr(args);
    }
}

static void c_cmp(Comp *cc, u8 cc_code, Val args) {
    c_expr(cc, car(args));
    x_push(cc->cb, RAX);
    c_expr(cc, car(cdr(args)));
    x_mov(cc->cb, RCX, RAX);
    x_pop(cc->cb, RAX);
    x_cmp(cc->cb, RAX, RCX);
    x_setcc(cc->cb, cc_code);
}

static void c_if(Comp *cc, Val args) {
    Val test = car(args);
    if (val_is_cons(test) && val_is_sym(car(test))) {
        i32 fcc = cmp_cc(val_as_sym(car(test)));
        if (fcc >= 0) {
            Val ca = cdr(test);
            c_expr(cc, car(ca));
            x_push(cc->cb, RAX);
            c_expr(cc, car(cdr(ca)));
            x_mov(cc->cb, RCX, RAX);
            x_pop(cc->cb, RAX);
            x_cmp(cc->cb, RAX, RCX);
            u32 f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));
            c_expr(cc, car(cdr(args)));
            u32 f_end = x_jmp(cc->cb);
            x_patch(cc->cb, f_else);
            Val e = cdr(cdr(args));
            if (val_is_cons(e)) c_expr(cc, car(e));
            else x_imm(cc->cb, RAX, 0);
            x_patch(cc->cb, f_end);
            return;
        }
    }
    c_expr(cc, test);
    x_test(cc->cb, RAX, RAX);
    u32 f_else = x_jcc(cc->cb, CC_E);
    c_expr(cc, car(cdr(args)));
    u32 f_end = x_jmp(cc->cb);
    x_patch(cc->cb, f_else);
    Val e = cdr(cdr(args));
    if (val_is_cons(e)) c_expr(cc, car(e));
    else x_imm(cc->cb, RAX, 0);
    x_patch(cc->cb, f_end);
}

static void c_let(Comp *cc, Val args) {
    u32 saved_count = cc->local_count;
    Val bindings = pvec_to_list(car(args));
    while (val_is_cons(bindings)) {
        StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
        c_expr(cc, car(bindings)); bindings = cdr(bindings);
        i32 off = alloc_slot(cc, name);
        x_store(cc->cb, off, RAX);
    }
    Val body = cdr(args);
    while (val_is_cons(body)) { c_expr(cc, car(body)); body = cdr(body); }
    cc->local_count = saved_count;
}

static void c_do(Comp *cc, Val args) {
    while (val_is_cons(args)) { c_expr(cc, car(args)); args = cdr(args); }
}

static void c_and(Comp *cc, Val args) {
    if (!val_is_cons(args)) { x_imm(cc->cb, RAX, 1); return; }
    c_expr(cc, car(args)); args = cdr(args);
    u32 fixups[16]; u32 fc = 0;
    while (val_is_cons(args)) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_E);
        c_expr(cc, car(args)); args = cdr(args);
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

static void c_or(Comp *cc, Val args) {
    if (!val_is_cons(args)) { x_imm(cc->cb, RAX, 0); return; }
    c_expr(cc, car(args)); args = cdr(args);
    u32 fixups[16]; u32 fc = 0;
    while (val_is_cons(args)) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_NE);
        c_expr(cc, car(args)); args = cdr(args);
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

static void c_recur(Comp *cc, Val args) {
    u32 n = 0;
    Val a = args;
    while (val_is_cons(a)) { c_expr(cc, car(a)); x_push(cc->cb, RAX); a = cdr(a); n++; }
    for (i32 i = n - 1; i >= 0; i--) { x_pop(cc->cb, RAX); x_store(cc->cb, cc->loop_offs[i], RAX); }
    x_jmp_to(cc->cb, cc->loop_start);
}

static void c_loop(Comp *cc, Val args) {
    StrId sv[16]; i32 so[16]; u32 sc = cc->loop_count; u32 ss = cc->loop_start;
    bool si = cc->in_loop;
    memcpy(sv, cc->loop_vars, sc * sizeof(StrId));
    memcpy(so, cc->loop_offs, sc * sizeof(i32));

    u32 saved_count = cc->local_count;
    cc->loop_count = 0;
    Val bindings = pvec_to_list(car(args));
    while (val_is_cons(bindings)) {
        StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
        c_expr(cc, car(bindings)); bindings = cdr(bindings);
        i32 off = alloc_slot(cc, name);
        x_store(cc->cb, off, RAX);
        cc->loop_vars[cc->loop_count] = name;
        cc->loop_offs[cc->loop_count] = off;
        cc->loop_count++;
    }
    cc->loop_start = cc->cb->pos;
    cc->in_loop = true;
    Val body = cdr(args);
    while (val_is_cons(body)) { c_expr(cc, car(body)); body = cdr(body); }

    cc->local_count = saved_count;
    cc->loop_count = sc; cc->loop_start = ss; cc->in_loop = si;
    memcpy(cc->loop_vars, sv, sc * sizeof(StrId));
    memcpy(cc->loop_offs, so, sc * sizeof(i32));
}

static void c_tail(Comp *cc, Val form) {
    if (val_is_cons(form) && val_is_sym(car(form))) {
        StrId sym = val_as_sym(car(form));
        Val args = cdr(form);
        if (sym == S_RECUR) { c_recur(cc, args); return; }
        if (sym == S_IF) {
            Val test = car(args);
            u32 f_else;
            if (val_is_cons(test) && val_is_sym(car(test))) {
                i32 fcc = cmp_cc(val_as_sym(car(test)));
                if (fcc >= 0) {
                    Val ca = cdr(test);
                    c_expr(cc, car(ca)); x_push(cc->cb, RAX);
                    c_expr(cc, car(cdr(ca))); x_mov(cc->cb, RCX, RAX); x_pop(cc->cb, RAX);
                    x_cmp(cc->cb, RAX, RCX);
                    f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));
                    goto tail_if_body;
                }
            }
            c_expr(cc, test); x_test(cc->cb, RAX, RAX);
            f_else = x_jcc(cc->cb, CC_E);
        tail_if_body:;
            c_tail(cc, car(cdr(args)));
            u32 f_end = x_jmp(cc->cb);
            x_patch(cc->cb, f_else);
            Val e = cdr(cdr(args));
            if (val_is_cons(e)) c_tail(cc, car(e));
            else x_imm(cc->cb, RAX, 0);
            x_patch(cc->cb, f_end);
            return;
        }
        if (sym == S_DO) {
            while (val_is_cons(args) && val_is_cons(cdr(args))) { c_expr(cc, car(args)); args = cdr(args); }
            if (val_is_cons(args)) c_tail(cc, car(args));
            return;
        }
        if (sym == S_LET) {
            u32 saved = cc->local_count;
            Val bindings = pvec_to_list(car(args));
            while (val_is_cons(bindings)) {
                StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
                c_expr(cc, car(bindings)); bindings = cdr(bindings);
                i32 off = alloc_slot(cc, name);
                x_store(cc->cb, off, RAX);
            }
            Val body = cdr(args);
            while (val_is_cons(body) && val_is_cons(cdr(body))) { c_expr(cc, car(body)); body = cdr(body); }
            if (val_is_cons(body)) c_tail(cc, car(body));
            cc->local_count = saved;
            return;
        }
    }
    c_expr(cc, form);
}

static void c_expr(Comp *cc, Val form) {
    if (val_is_int(form))  { x_imm(cc->cb, RAX, val_as_int(form)); return; }
    if (val_is_bool(form)) { x_imm(cc->cb, RAX, val_as_bool(form) ? 1 : 0); return; }
    if (val_is_nil(form))  { x_imm(cc->cb, RAX, 0); return; }
    if (val_is_sym(form)) {
        StrId name = val_as_sym(form);
        if (cc->cs_mode) { i8 r = find_cs(cc, name); if (r >= 0) { x_mov_rr(cc->cb, RAX, (u8)r); return; } }
        i32 off = find_local(cc, name);
        if (off) { x_load(cc->cb, RAX, off); return; }
        i32 gi = find_global(name);
        if (gi >= 0) { x_load_abs(cc->cb, &g_globals[gi]); return; }
        x_imm(cc->cb, RAX, 0); return;
    }
    if (!val_is_cons(form)) { x_imm(cc->cb, RAX, 0); return; }

    Val head = car(form), args = cdr(form);
    if (!val_is_sym(head)) { x_imm(cc->cb, RAX, 0); return; }
    StrId sym = val_as_sym(head);

    if (sym == S_IF)   { c_if(cc, args); return; }
    if (sym == S_LET)  { c_let(cc, args); return; }
    if (sym == S_DO)   { c_do(cc, args); return; }
    if (sym == S_AND)  { c_and(cc, args); return; }
    if (sym == S_OR)   { c_or(cc, args); return; }
    if (sym == S_LOOP) { c_loop(cc, args); return; }
    if (sym == S_RECUR && cc->in_loop) { c_recur(cc, args); return; }

    if (sym == S_COND) {
        // Chain of test/expr pairs
        u32 end_fixes[32]; u32 efc = 0;
        while (val_is_cons(args) && val_is_cons(cdr(args))) {
            Val test = car(args); args = cdr(args);
            // :else is always truthy — emit expr directly
            if (val_is_kw(test)) {
                c_expr(cc, car(args));
                goto cond_done;
            }
            c_expr(cc, test);
            x_test(cc->cb, RAX, RAX);
            u32 skip = x_jcc(cc->cb, CC_E);
            c_expr(cc, car(args)); args = cdr(args);
            end_fixes[efc++] = x_jmp(cc->cb);
            x_patch(cc->cb, skip);
        }
        x_imm(cc->cb, RAX, 0); // no match → nil (0)
    cond_done:
        for (u32 i = 0; i < efc; i++) x_patch(cc->cb, end_fixes[i]);
        return;
    }

    if (sym == S_WHEN) {
        c_expr(cc, car(args));
        x_test(cc->cb, RAX, RAX);
        u32 skip = x_jcc(cc->cb, CC_E);
        args = cdr(args);
        while (val_is_cons(args)) { c_expr(cc, car(args)); args = cdr(args); }
        x_patch(cc->cb, skip);
        return;
    }

    if (sym == S_ADD || sym == S_SUB || sym == S_MUL ||
        sym == S_DIV || sym == S_MOD) { c_binop(cc, sym, args); return; }

    i32 cc_code = cmp_cc(sym);
    if (cc_code >= 0) { c_cmp(cc, (u8)cc_code, args); return; }

    if (sym == S_NOT)   { c_expr(cc, car(args)); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_E); return; }
    if (sym == S_INC)   { c_expr(cc, car(args)); x_add_i8(cc->cb, RAX, 1); return; }
    if (sym == S_DEC)   { c_expr(cc, car(args)); x_sub_i8(cc->cb, RAX, 1); return; }
    if (sym == S_ZEROQ) { c_expr(cc, car(args)); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_E); return; }
    if (sym == S_POSQ)  { c_expr(cc, car(args)); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_G); return; }
    if (sym == S_NEGQ)  { c_expr(cc, car(args)); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_L); return; }

    // Function call
    u32 n = list_len(args);
    if (n == 1) {
        c_expr(cc, car(args));
        x_mov(cc->cb, RDI, RAX);
    } else {
        Val a = args;
        while (val_is_cons(a)) { c_expr(cc, car(a)); x_push(cc->cb, RAX); a = cdr(a); }
        for (i32 i = n - 1; i >= 0; i--) x_pop(cc->cb, ARG_REGS[i]);
    }
    u32 fix = x_call(cc->cb);
    g_fixes[g_fix_count++] = (CallFix){fix, sym};
}

// ============================================================================
// 4. Program Compiler + JIT
// ============================================================================

#define CODE_SIZE (1 << 20)
static CodeBuf g_code;

static void compile_defn(DefnInfo *fn) {
    bool tco = has_recur(fn->body);
    bool cs_mode = !tco && fn->n_params <= N_CS;
    u8 n_cs = cs_mode ? (u8)fn->n_params : 0;

    Comp cc = {.cb = &g_code, .local_count = 0,
               .next_slot = -(i32)(n_cs * 8),
               .in_loop = false, .loop_count = 0,
               .cs_mode = cs_mode, .cs_param = n_cs, .cs_temp = 0, .cs_max = n_cs};
    u32 fn_start = g_code.pos;

    if (cs_mode) {
        x_push(&g_code, RBP);
        x_mov(&g_code, RBP, RSP);
        for (u32 i = 0; i < n_cs; i++) x_push_r(&g_code, CS_POOL[i]);
        u32 frame = (n_cs & 1) ? 248 : 256;
        xb(&g_code, 0x48); xb(&g_code, 0x81); xb(&g_code, MODRM(3, 5, RSP)); x32(&g_code, frame);
        Val p = fn->params; u32 pi = 0;
        while (val_is_cons(p)) {
            cc.cs_names[pi] = val_as_sym(car(p));
            x_mov_rr(&g_code, CS_POOL[pi], ARG_REGS[pi]);
            p = cdr(p); pi++;
        }
        Val body = fn->body;
        while (val_is_cons(body)) { c_expr(&cc, car(body)); body = cdr(body); }
        xb(&g_code, 0x48); xb(&g_code, 0x81); xb(&g_code, MODRM(3, 0, RSP)); x32(&g_code, frame);
        for (i32 i = (i32)n_cs - 1; i >= 0; i--) x_pop_r(&g_code, CS_POOL[i]);
        x_pop(&g_code, RBP);
        x_ret(&g_code);
    } else {
        x_prologue(&g_code, 256);
        Val p = fn->params; u32 pi = 0;
        while (val_is_cons(p)) {
            i32 off = alloc_slot(&cc, val_as_sym(car(p)));
            x_store(&g_code, off, ARG_REGS[pi]);
            p = cdr(p); pi++;
        }
        if (tco) {
            cc.in_loop = true;
            cc.loop_count = fn->n_params;
            p = fn->params;
            for (u32 i = 0; i < fn->n_params; i++) {
                cc.loop_vars[i] = val_as_sym(car(p));
                cc.loop_offs[i] = find_local(&cc, cc.loop_vars[i]);
                p = cdr(p);
            }
            cc.loop_start = g_code.pos;
            Val body = fn->body;
            while (val_is_cons(body) && val_is_cons(cdr(body))) { c_expr(&cc, car(body)); body = cdr(body); }
            if (val_is_cons(body)) c_tail(&cc, car(body));
        } else {
            Val body = fn->body;
            while (val_is_cons(body)) { c_expr(&cc, car(body)); body = cdr(body); }
        }
        x_epilogue(&g_code);
    }

    g_fn_offset[fn->name] = (i32)fn_start;
    g_fn_gen[fn->name] = g_gen;
}

static u32 compile_program(const char *source) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;

    classify(source);
    image_build();

    for (u32 i = 0; i < g_def_count; i++) {
        g_global_idx[g_defs[i].name] = (i32)g_global_count;
        g_global_gen[g_defs[i].name] = g_gen;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }
    for (u32 i = 0; i < g_defn_count; i++) compile_defn(&g_defns[i]);

    u32 entry = g_code.pos;
    Comp cc = {.cb = &g_code, .local_count = 0, .next_slot = 0, .in_loop = false, .loop_count = 0};
    x_prologue(&g_code, 256);
    for (u32 i = 0; i < g_def_count; i++) {
        c_expr(&cc, g_defs[i].value);
        x_store_abs(&g_code, &g_globals[i]);
    }
    for (u32 i = 0; i < g_main_count; i++) c_expr(&cc, g_mains[i]);
    x_epilogue(&g_code);

    for (u32 i = 0; i < g_fix_count; i++) {
        i32 target = find_fn(g_fixes[i].target);
        if (target >= 0) {
            i32 rel = (i32)(target - (g_fixes[i].pos + 4));
            memcpy(g_code.code + g_fixes[i].pos, &rel, 4);
        }
    }
    return entry;
}

typedef i64 (*JitFn0)(void);

static i64 jit_run(const char *source) {
    u32 entry = compile_program(source);
    JitFn0 fn = (JitFn0)(g_code.code + entry);
    return fn();
}

#endif // JIT_C_INCLUDED
