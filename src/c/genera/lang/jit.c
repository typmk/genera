/**
 * jit.c — Clojure → x86-64 JIT Compiler (GNode-based)
 *
 * Compiles directly from GNode parse tree — no entity bridge, no Val cons lists.
 * Source → parse → GNode tree → compile → mmap → execute.
 *
 * Depends on: emit/x86.c (encoding), lang/grammar.c (GNode, Gram)
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
    Gram    *g;
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
// 3. Expression Compiler — GNode-based
// ============================================================================

static void cg_expr(Comp *cc, u32 id);
static void cg_tail(Comp *cc, u32 id);

static i32 cmp_cc(StrId op) {
    if (op == S_EQ)  return CC_E;
    if (op == S_LT)  return CC_L;
    if (op == S_GT)  return CC_G;
    if (op == S_LTE) return CC_LE;
    if (op == S_GTE) return CC_GE;
    return -1;
}

static void cg_binop(Comp *cc, StrId op, u32 arg) {
    Gram *g = cc->g;
    u32 n = 0;
    for (u32 a = arg; a; a = g->nodes[a].next) n++;
    if (n == 1 && op == S_SUB) { cg_expr(cc, arg); x_neg(cc->cb, RAX); return; }
    if (n == 0) { x_imm(cc->cb, RAX, (op == S_MUL) ? 1 : 0); return; }
    cg_expr(cc, arg);
    u32 a = g->nodes[arg].next;
    while (a) {
        x_push(cc->cb, RAX);
        cg_expr(cc, a);
        x_mov(cc->cb, RCX, RAX);
        x_pop(cc->cb, RAX);
        if (op == S_ADD)      x_add(cc->cb, RAX, RCX);
        else if (op == S_SUB) x_sub(cc->cb, RAX, RCX);
        else if (op == S_MUL) x_imul(cc->cb, RAX, RCX);
        else if (op == S_DIV) x_idiv(cc->cb, RCX);
        else if (op == S_MOD) { x_idiv(cc->cb, RCX); x_mov(cc->cb, RAX, RDX); }
        a = g->nodes[a].next;
    }
}

static void cg_cmp(Comp *cc, u8 cc_code, u32 arg) {
    Gram *g = cc->g;
    cg_expr(cc, arg);
    x_push(cc->cb, RAX);
    cg_expr(cc, g->nodes[arg].next);
    x_mov(cc->cb, RCX, RAX);
    x_pop(cc->cb, RAX);
    x_cmp(cc->cb, RAX, RCX);
    x_setcc(cc->cb, cc_code);
}

static void cg_if(Comp *cc, u32 test) {
    Gram *g = cc->g;
    u32 then = g->nodes[test].next;
    u32 els  = then ? g->nodes[then].next : 0;

    // Dead branch elimination via views
    if (g->analyzed) {
        if (then && BM_GET(g->v[V_DEAD], then)) { if (els) cg_expr(cc, els); else x_imm(cc->cb, RAX, 0); return; }
        if (els  && BM_GET(g->v[V_DEAD], els))  { cg_expr(cc, then); return; }
    }

    // Fused compare optimization
    if (g->nodes[test].kind == NK_LIST && g->nodes[test].child) {
        u32 tfc = g->nodes[test].child;
        if (g->nodes[tfc].kind == NK_IDENT) {
            i32 fcc = cmp_cc(gn_intern(g, tfc));
            if (fcc >= 0) {
                u32 ca = g->nodes[tfc].next;
                cg_expr(cc, ca);
                x_push(cc->cb, RAX);
                cg_expr(cc, g->nodes[ca].next);
                x_mov(cc->cb, RCX, RAX);
                x_pop(cc->cb, RAX);
                x_cmp(cc->cb, RAX, RCX);
                u32 f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));
                cg_expr(cc, then);
                u32 f_end = x_jmp(cc->cb);
                x_patch(cc->cb, f_else);
                if (els) cg_expr(cc, els);
                else x_imm(cc->cb, RAX, 0);
                x_patch(cc->cb, f_end);
                return;
            }
        }
    }
    cg_expr(cc, test);
    x_test(cc->cb, RAX, RAX);
    u32 f_else = x_jcc(cc->cb, CC_E);
    cg_expr(cc, then);
    u32 f_end = x_jmp(cc->cb);
    x_patch(cc->cb, f_else);
    if (els) cg_expr(cc, els);
    else x_imm(cc->cb, RAX, 0);
    x_patch(cc->cb, f_end);
}

static void cg_let(Comp *cc, u32 bv) {
    Gram *g = cc->g;
    u32 saved_count = cc->local_count;
    u32 p = g->nodes[bv].child;
    while (p) {
        StrId name = gn_intern(g, p);
        u32 vn = g->nodes[p].next;
        if (!vn) break;
        cg_expr(cc, vn);
        i32 off = alloc_slot(cc, name);
        x_store(cc->cb, off, RAX);
        p = g->nodes[vn].next;
    }
    u32 body = g->nodes[bv].next;
    while (body) { cg_expr(cc, body); body = g->nodes[body].next; }
    cc->local_count = saved_count;
}

static void cg_do(Comp *cc, u32 body) {
    while (body) { cg_expr(cc, body); body = cc->g->nodes[body].next; }
}

static void cg_and(Comp *cc, u32 arg) {
    if (!arg) { x_imm(cc->cb, RAX, 1); return; }
    cg_expr(cc, arg);
    u32 fixups[16]; u32 fc = 0;
    arg = cc->g->nodes[arg].next;
    while (arg) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_E);
        cg_expr(cc, arg);
        arg = cc->g->nodes[arg].next;
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

static void cg_or(Comp *cc, u32 arg) {
    if (!arg) { x_imm(cc->cb, RAX, 0); return; }
    cg_expr(cc, arg);
    u32 fixups[16]; u32 fc = 0;
    arg = cc->g->nodes[arg].next;
    while (arg) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_NE);
        cg_expr(cc, arg);
        arg = cc->g->nodes[arg].next;
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

static void cg_recur(Comp *cc, u32 arg) {
    Gram *g = cc->g;
    u32 n = 0;
    u32 a = arg;
    while (a) { cg_expr(cc, a); x_push(cc->cb, RAX); a = g->nodes[a].next; n++; }
    for (i32 i = n - 1; i >= 0; i--) { x_pop(cc->cb, RAX); x_store(cc->cb, cc->loop_offs[i], RAX); }
    x_jmp_to(cc->cb, cc->loop_start);
}

static void cg_loop(Comp *cc, u32 bv) {
    Gram *g = cc->g;
    StrId sv[16]; i32 so[16]; u32 sc = cc->loop_count; u32 ss = cc->loop_start;
    bool si = cc->in_loop;
    memcpy(sv, cc->loop_vars, sc * sizeof(StrId));
    memcpy(so, cc->loop_offs, sc * sizeof(i32));

    u32 saved_count = cc->local_count;
    cc->loop_count = 0;
    u32 p = g->nodes[bv].child;
    while (p) {
        StrId name = gn_intern(g, p);
        u32 vn = g->nodes[p].next;
        if (!vn) break;
        cg_expr(cc, vn);
        i32 off = alloc_slot(cc, name);
        x_store(cc->cb, off, RAX);
        cc->loop_vars[cc->loop_count] = name;
        cc->loop_offs[cc->loop_count] = off;
        cc->loop_count++;
        p = g->nodes[vn].next;
    }
    cc->loop_start = cc->cb->pos;
    cc->in_loop = true;
    u32 body = g->nodes[bv].next;
    while (body) { cg_expr(cc, body); body = g->nodes[body].next; }

    cc->local_count = saved_count;
    cc->loop_count = sc; cc->loop_start = ss; cc->in_loop = si;
    memcpy(cc->loop_vars, sv, sc * sizeof(StrId));
    memcpy(cc->loop_offs, so, sc * sizeof(i32));
}

static void cg_tail(Comp *cc, u32 id) {
    Gram *g = cc->g;
    if (g->nodes[id].kind == NK_LIST && g->nodes[id].child) {
        u32 fc = g->nodes[id].child;
        if (g->nodes[fc].kind == NK_IDENT) {
            StrId sym = gn_intern(g, fc);
            u32 args = g->nodes[fc].next;

            if (sym == S_RECUR) { cg_recur(cc, args); return; }

            if (sym == S_IF) {
                u32 test = args;
                u32 then = g->nodes[test].next;
                u32 els  = then ? g->nodes[then].next : 0;
                u32 f_else;
                // Fused compare
                if (g->nodes[test].kind == NK_LIST && g->nodes[test].child) {
                    u32 tfc = g->nodes[test].child;
                    if (g->nodes[tfc].kind == NK_IDENT) {
                        i32 fcc = cmp_cc(gn_intern(g, tfc));
                        if (fcc >= 0) {
                            u32 ca = g->nodes[tfc].next;
                            cg_expr(cc, ca); x_push(cc->cb, RAX);
                            cg_expr(cc, g->nodes[ca].next); x_mov(cc->cb, RCX, RAX); x_pop(cc->cb, RAX);
                            x_cmp(cc->cb, RAX, RCX);
                            f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));
                            goto tail_if_body;
                        }
                    }
                }
                cg_expr(cc, test); x_test(cc->cb, RAX, RAX);
                f_else = x_jcc(cc->cb, CC_E);
            tail_if_body:;
                cg_tail(cc, then);
                u32 f_end = x_jmp(cc->cb);
                x_patch(cc->cb, f_else);
                if (els) cg_tail(cc, els);
                else x_imm(cc->cb, RAX, 0);
                x_patch(cc->cb, f_end);
                return;
            }

            if (sym == S_DO) {
                u32 e = args;
                while (e && g->nodes[e].next) { cg_expr(cc, e); e = g->nodes[e].next; }
                if (e) cg_tail(cc, e);
                return;
            }

            if (sym == S_LET) {
                u32 bv = args;
                u32 saved = cc->local_count;
                u32 p = g->nodes[bv].child;
                while (p) {
                    StrId name = gn_intern(g, p);
                    u32 vn = g->nodes[p].next;
                    if (!vn) break;
                    cg_expr(cc, vn);
                    i32 off = alloc_slot(cc, name);
                    x_store(cc->cb, off, RAX);
                    p = g->nodes[vn].next;
                }
                u32 body = g->nodes[bv].next;
                while (body && g->nodes[body].next) { cg_expr(cc, body); body = g->nodes[body].next; }
                if (body) cg_tail(cc, body);
                cc->local_count = saved;
                return;
            }
        }
    }
    cg_expr(cc, id);
}

static void cg_expr(Comp *cc, u32 id) {
    Gram *g = cc->g;
    GNode *n = &g->nodes[id];

    // View-driven: dead code elimination + constant folding
    if (g->analyzed) {
        if (BM_GET(g->v[V_DEAD], id)) { x_imm(cc->cb, RAX, 0); return; }
        if (BM_GET(g->v[V_CONST], id) && BM_GET(g->v[V_INT], id)) {
            x_imm(cc->cb, RAX, g->const_val[id]); return;
        }
    }

    if (n->kind == NK_NUM)  { x_imm(cc->cb, RAX, gn_parse_int(g, id)); return; }
    if (n->kind == NK_IDENT) {
        StrId name = gn_intern(g, id);
        if (name == S_TRUE)  { x_imm(cc->cb, RAX, 1); return; }
        if (name == S_FALSE || name == S_NIL) { x_imm(cc->cb, RAX, 0); return; }
        if (cc->cs_mode) { i8 r = find_cs(cc, name); if (r >= 0) { x_mov_rr(cc->cb, RAX, (u8)r); return; } }
        i32 off = find_local(cc, name);
        if (off) { x_load(cc->cb, RAX, off); return; }
        i32 gi = find_global(name);
        if (gi >= 0) { x_load_abs(cc->cb, &g_globals[gi]); return; }
        x_imm(cc->cb, RAX, 0); return;
    }
    if (n->kind != NK_LIST || !n->child) { x_imm(cc->cb, RAX, 0); return; }

    u32 fc = n->child;
    if (g->nodes[fc].kind != NK_IDENT) { x_imm(cc->cb, RAX, 0); return; }
    StrId sym = gn_intern(g, fc);
    u32 args = g->nodes[fc].next;

    if (sym == S_IF)   { cg_if(cc, args); return; }
    if (sym == S_LET)  { cg_let(cc, args); return; }
    if (sym == S_DO)   { cg_do(cc, args); return; }
    if (sym == S_AND)  { cg_and(cc, args); return; }
    if (sym == S_OR)   { cg_or(cc, args); return; }
    if (sym == S_LOOP) { cg_loop(cc, args); return; }
    if (sym == S_RECUR && cc->in_loop) { cg_recur(cc, args); return; }

    if (sym == S_COND) {
        u32 end_fixes[32]; u32 efc = 0;
        u32 a = args;
        bool found_else = false;
        while (a && g->nodes[a].next) {
            u32 expr = g->nodes[a].next;
            if (g->nodes[a].kind == NK_KW) {
                cg_expr(cc, expr);
                found_else = true;
                break;
            }
            cg_expr(cc, a);
            x_test(cc->cb, RAX, RAX);
            u32 skip = x_jcc(cc->cb, CC_E);
            cg_expr(cc, expr);
            end_fixes[efc++] = x_jmp(cc->cb);
            x_patch(cc->cb, skip);
            a = g->nodes[expr].next;
        }
        if (!found_else) x_imm(cc->cb, RAX, 0);
        for (u32 i = 0; i < efc; i++) x_patch(cc->cb, end_fixes[i]);
        return;
    }

    if (sym == S_WHEN) {
        cg_expr(cc, args);
        x_test(cc->cb, RAX, RAX);
        u32 skip = x_jcc(cc->cb, CC_E);
        u32 a = g->nodes[args].next;
        while (a) { cg_expr(cc, a); a = g->nodes[a].next; }
        x_patch(cc->cb, skip);
        return;
    }

    if (sym == S_ADD || sym == S_SUB || sym == S_MUL ||
        sym == S_DIV || sym == S_MOD) { cg_binop(cc, sym, args); return; }

    i32 cc_code = cmp_cc(sym);
    if (cc_code >= 0) { cg_cmp(cc, (u8)cc_code, args); return; }

    if (sym == S_NOT)   { cg_expr(cc, args); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_E); return; }
    if (sym == S_INC)   { cg_expr(cc, args); x_add_i8(cc->cb, RAX, 1); return; }
    if (sym == S_DEC)   { cg_expr(cc, args); x_sub_i8(cc->cb, RAX, 1); return; }
    if (sym == S_ZEROQ) { cg_expr(cc, args); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_E); return; }
    if (sym == S_POSQ)  { cg_expr(cc, args); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_G); return; }
    if (sym == S_NEGQ)  { cg_expr(cc, args); x_test(cc->cb, RAX, RAX); x_setcc(cc->cb, CC_L); return; }

    // Function call
    u32 n_args = 0;
    for (u32 a = args; a; a = g->nodes[a].next) n_args++;
    if (n_args == 1) {
        cg_expr(cc, args);
        x_mov(cc->cb, RDI, RAX);
    } else {
        u32 a = args;
        while (a) { cg_expr(cc, a); x_push(cc->cb, RAX); a = g->nodes[a].next; }
        for (i32 i = n_args - 1; i >= 0; i--) x_pop(cc->cb, ARG_REGS[i]);
    }
    u32 fix = x_call(cc->cb);
    g_fixes[g_fix_count++] = (CallFix){fix, sym};
}

// ============================================================================
// 4. Program Compiler + JIT
// ============================================================================

#define CODE_SIZE (1 << 20)
static CodeBuf g_code;

static void compile_defn_gn(Gram *g, u32 id) {
    u32 fc = g->nodes[id].child;                     // "defn"
    u32 nm = g->nodes[fc].next;                       // name
    u32 pv = g->nodes[nm].next;                       // [params]
    StrId name = gn_intern(g, nm);

    u32 n_params = 0;
    for (u32 p = g->nodes[pv].child; p; p = g->nodes[p].next) n_params++;

    bool tco = gn_has_recur(g, id);
    bool cs_mode = !tco && n_params <= N_CS;
    u8 n_cs = cs_mode ? (u8)n_params : 0;

    Comp cc = {.cb = &g_code, .g = g, .local_count = 0,
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
        u32 p = g->nodes[pv].child, pi = 0;
        while (p) {
            cc.cs_names[pi] = gn_intern(g, p);
            x_mov_rr(&g_code, CS_POOL[pi], ARG_REGS[pi]);
            p = g->nodes[p].next; pi++;
        }
        u32 body = g->nodes[pv].next;
        while (body) { cg_expr(&cc, body); body = g->nodes[body].next; }
        xb(&g_code, 0x48); xb(&g_code, 0x81); xb(&g_code, MODRM(3, 0, RSP)); x32(&g_code, frame);
        for (i32 i = (i32)n_cs - 1; i >= 0; i--) x_pop_r(&g_code, CS_POOL[i]);
        x_pop(&g_code, RBP);
        x_ret(&g_code);
    } else {
        x_prologue(&g_code, 256);
        u32 p = g->nodes[pv].child, pi = 0;
        while (p) {
            i32 off = alloc_slot(&cc, gn_intern(g, p));
            x_store(&g_code, off, ARG_REGS[pi]);
            p = g->nodes[p].next; pi++;
        }
        if (tco) {
            cc.in_loop = true;
            cc.loop_count = n_params;
            p = g->nodes[pv].child;
            for (u32 i = 0; i < n_params; i++) {
                cc.loop_vars[i] = gn_intern(g, p);
                cc.loop_offs[i] = find_local(&cc, cc.loop_vars[i]);
                p = g->nodes[p].next;
            }
            cc.loop_start = g_code.pos;
            u32 body = g->nodes[pv].next;
            while (body && g->nodes[body].next) { cg_expr(&cc, body); body = g->nodes[body].next; }
            if (body) cg_tail(&cc, body);
        } else {
            u32 body = g->nodes[pv].next;
            while (body) { cg_expr(&cc, body); body = g->nodes[body].next; }
        }
        x_epilogue(&g_code);
    }

    g_fn_offset[name] = (i32)fn_start;
    g_fn_gen[name] = g_gen;
}

static u32 compile_program(const char *source) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;

    gram_ensure_scratch();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    Gram *g = &g_gram_scratch;
    gram_parse(g, &lisp, source, (u32)strlen(source));
    gram_index(g);
    gram_analyze(g);

    // Classify top-level forms directly from GNode tree
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

    // Register globals
    for (u32 i = 0; i < n_defs; i++) {
        g_global_idx[def_names[i]] = (i32)g_global_count;
        g_global_gen[def_names[i]] = g_gen;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }

    // Compile defns
    for (u32 i = 0; i < n_defns; i++) compile_defn_gn(g, defn_ids[i]);

    // Entry: init globals + run mains
    u32 entry = g_code.pos;
    Comp cc = {.cb = &g_code, .g = g, .local_count = 0, .next_slot = 0, .in_loop = false, .loop_count = 0};
    x_prologue(&g_code, 256);
    for (u32 i = 0; i < n_defs; i++) {
        cg_expr(&cc, def_val[i]);
        x_store_abs(&g_code, &g_globals[i]);
    }
    for (u32 i = 0; i < n_mains; i++) cg_expr(&cc, main_ids[i]);
    x_epilogue(&g_code);

    // Patch call fixups
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
