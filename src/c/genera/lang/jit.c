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
    DISPATCH(TK_JIT, id, 0);
    Gram *g = cc->g;
    GNode *n = &g->nodes[id];

    // View-driven: dead code elimination + constant folding
    if (g->analyzed) {
        if (BM_GET(g->v[V_DEAD], id)) { x_imm(cc->cb, RAX, 0); return; }
        if (BM_GET(g->v[V_CONST], id) && BM_GET(g->v[V_INT], id)) {
            x_imm(cc->cb, RAX, g->val[id]); return;
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

    Gram *g = world_step(source, true);

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

// Compile from a pre-analyzed Gram (used by image load)
static u32 compile_from_gram(Gram *g) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;

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

    for (u32 i = 0; i < n_defs; i++) {
        g_global_idx[def_names[i]] = (i32)g_global_count;
        g_global_gen[def_names[i]] = g_gen;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }
    for (u32 i = 0; i < n_defns; i++) compile_defn_gn(g, defn_ids[i]);

    u32 entry = g_code.pos;
    Comp cc = {.cb = &g_code, .g = g, .local_count = 0, .next_slot = 0, .in_loop = false, .loop_count = 0};
    x_prologue(&g_code, 256);
    for (u32 i = 0; i < n_defs; i++) {
        cg_expr(&cc, def_val[i]);
        x_store_abs(&g_code, &g_globals[i]);
    }
    for (u32 i = 0; i < n_mains; i++) cg_expr(&cc, main_ids[i]);
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

static i64 jit_run_gram(Gram *g) {
    u32 entry = compile_from_gram(g);
    JitFn0 fn = (JitFn0)(g_code.code + entry);
    return fn();
}

// ============================================================================
// 5. JIT Builtins — expose x86 encoding + compiler state to Clojure
//
// Registered via register_jit_builtins() called from register_builtins().
// Clojure JIT walker calls these to emit x86, manage compiler state, and
// register/execute compiled functions.
// ============================================================================

// Global compiler state for Clojure JIT walker
static Comp g_comp;

// --- x86 encoding builtins ---
static Val bi_x86_imm(Val a)  { x_imm(&g_code, (u8)val_as_int(car(a)), val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_mov(Val a)  { x_mov(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_add(Val a)  { x_add(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_sub(Val a)  { x_sub(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_cmp(Val a)  { x_cmp(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_test(Val a) { x_test(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_imul(Val a) { x_imul(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_idiv(Val a) { x_idiv(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_neg(Val a)  { x_neg(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_push(Val a) { x_push(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_pop(Val a)  { x_pop(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_load(Val a) { x_load(&g_code, (u8)val_as_int(car(a)), (i32)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_store(Val a){ x_store(&g_code, (i32)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_load_abs(Val a) {
    i32 gi = (i32)val_as_int(car(a));
    x_load_abs(&g_code, &g_globals[gi]);
    return NIL;
}
static Val bi_x86_store_abs(Val a) {
    i32 gi = (i32)val_as_int(car(a));
    x_store_abs(&g_code, &g_globals[gi]);
    return NIL;
}
static Val bi_x86_jmp(Val a)    { (void)a; return val_int(x_jmp(&g_code)); }
static Val bi_x86_call(Val a)   { (void)a; return val_int(x_call(&g_code)); }
static Val bi_x86_ret(Val a)    { (void)a; x_ret(&g_code); return NIL; }
static Val bi_x86_jcc(Val a)    { return val_int(x_jcc(&g_code, (u8)val_as_int(car(a)))); }
static Val bi_x86_patch(Val a)  { x_patch(&g_code, (u32)val_as_int(car(a))); return NIL; }
static Val bi_x86_jmp_to(Val a) { x_jmp_to(&g_code, (u32)val_as_int(car(a))); return NIL; }
static Val bi_x86_setcc(Val a)  { x_setcc(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_pro(Val a)    { x_prologue(&g_code, (u32)val_as_int(car(a))); return NIL; }
static Val bi_x86_epi(Val a)    { (void)a; x_epilogue(&g_code); return NIL; }
static Val bi_x86_addi(Val a)   { x_add_i8(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_subi(Val a)   { x_sub_i8(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_push_r(Val a) { x_push_r(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_pop_r(Val a)  { x_pop_r(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_mov_rr(Val a) { x_mov_rr(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
// Bit ops
static Val bi_x86_and(Val a)     { x_and(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_or(Val a)      { x_or(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_xor(Val a)     { x_xor(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_bit_not(Val a) { x_bit_not(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_shl_cl(Val a)  { x_shl_cl(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_shr_cl(Val a)  { x_shr_cl(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x86_and_i(Val a)   { x_and_i(&g_code, (u8)val_as_int(car(a)), (i32)val_as_int(car(cdr(a)))); return NIL; }
static Val bi_x86_popcnt(Val a)  { x_popcnt(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a)))); return NIL; }
// Memory addressing (base register + displacement)
static Val bi_x86_load_base(Val a)  { x_load_base(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a))), (i32)val_as_int(car(cdr(cdr(a))))); return NIL; }
static Val bi_x86_store_base(Val a) { x_store_base(&g_code, (u8)val_as_int(car(a)), (i32)val_as_int(car(cdr(a))), (u8)val_as_int(car(cdr(cdr(a))))); return NIL; }
static Val bi_x86_load32_base(Val a)  { x_load32_base(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a))), (i32)val_as_int(car(cdr(cdr(a))))); return NIL; }
static Val bi_x86_store32_base(Val a) { x_store32_base(&g_code, (u8)val_as_int(car(a)), (i32)val_as_int(car(cdr(a))), (u8)val_as_int(car(cdr(cdr(a))))); return NIL; }
static Val bi_x86_load8_base(Val a)   { x_load8_base(&g_code, (u8)val_as_int(car(a)), (u8)val_as_int(car(cdr(a))), (i32)val_as_int(car(cdr(cdr(a))))); return NIL; }
static Val bi_x86_store8_base(Val a)  { x_store8_base(&g_code, (u8)val_as_int(car(a)), (i32)val_as_int(car(cdr(a))), (u8)val_as_int(car(cdr(cdr(a))))); return NIL; }

static Val bi_xb(Val a)  { xb(&g_code, (u8)val_as_int(car(a))); return NIL; }
static Val bi_x32(Val a) { x32(&g_code, (u32)val_as_int(car(a))); return NIL; }

// --- code buffer ---
static Val bi_code_pos(Val a)   { (void)a; return val_int(g_code.pos); }
static Val bi_code_reset(Val a) { (void)a; g_code.pos = 0; return NIL; }

// --- compiler state (g_comp) ---
static Val bi_comp_reset(Val a) {
    // (comp-reset! gram-ptr cs-mode n-cs next-slot)
    g_comp.cb = &g_code;
    g_comp.g = &g_world.gram;
    g_comp.local_count = 0;
    g_comp.cs_mode = val_truthy(car(a));
    u8 ncs = (u8)val_as_int(car(cdr(a)));
    g_comp.cs_param = ncs;
    g_comp.cs_temp = 0;
    g_comp.cs_max = ncs;
    g_comp.next_slot = (i32)val_as_int(car(cdr(cdr(a))));
    g_comp.in_loop = false;
    g_comp.loop_count = 0;
    return NIL;
}
static Val bi_comp_alloc_slot(Val a) {
    return val_int(alloc_slot(&g_comp, (StrId)val_as_int(car(a))));
}
static Val bi_comp_find_local(Val a) {
    return val_int(find_local(&g_comp, (StrId)val_as_int(car(a))));
}
static Val bi_comp_find_cs(Val a) {
    return val_int(find_cs(&g_comp, (StrId)val_as_int(car(a))));
}
static Val bi_comp_alloc_cs_temp(Val a) {
    (void)a;
    return val_int(alloc_cs_temp(&g_comp));
}
static Val bi_comp_free_cs_temp(Val a) {
    (void)a;
    free_cs_temp(&g_comp);
    return NIL;
}
static Val bi_comp_save_locals(Val a) {
    (void)a;
    return val_int(g_comp.local_count);
}
static Val bi_comp_restore_locals(Val a) {
    g_comp.local_count = (u32)val_as_int(car(a));
    return NIL;
}
static Val bi_comp_set_slot(Val a) {
    g_comp.next_slot = (i32)val_as_int(car(a));
    return NIL;
}
static Val bi_comp_cs_setup(Val a) {
    // (comp-cs-setup! n-params)
    u32 np = (u32)val_as_int(car(a));
    g_comp.cs_param = (u8)np;
    g_comp.cs_max = (u8)np;
    g_comp.cs_temp = 0;
    g_comp.cs_mode = true;
    return NIL;
}
static Val bi_comp_cs_name(Val a) {
    // (comp-cs-name! idx name)
    u32 idx = (u32)val_as_int(car(a));
    g_comp.cs_names[idx] = (StrId)val_as_int(car(cdr(a)));
    return NIL;
}
static Val bi_comp_cs_mode(Val a) {
    (void)a;
    return val_bool(g_comp.cs_mode);
}
static Val bi_comp_loop_start(Val a) {
    (void)a;
    return val_int(g_comp.loop_start);
}
static Val bi_comp_set_loop_start(Val a) {
    g_comp.loop_start = (u32)val_as_int(car(a));
    return NIL;
}
static Val bi_comp_in_loop(Val a) {
    (void)a;
    return val_bool(g_comp.in_loop);
}
static Val bi_comp_set_in_loop(Val a) {
    g_comp.in_loop = val_truthy(car(a));
    return NIL;
}
static Val bi_comp_loop_var(Val a) {
    // (comp-loop-var! idx name off)
    u32 idx = (u32)val_as_int(car(a));
    g_comp.loop_vars[idx] = (StrId)val_as_int(car(cdr(a)));
    g_comp.loop_offs[idx] = (i32)val_as_int(car(cdr(cdr(a))));
    return NIL;
}
static Val bi_comp_loop_count(Val a) {
    (void)a;
    return val_int(g_comp.loop_count);
}
static Val bi_comp_set_loop_count(Val a) {
    g_comp.loop_count = (u32)val_as_int(car(a));
    return NIL;
}
static Val bi_comp_loop_off(Val a) {
    return val_int(g_comp.loop_offs[(u32)val_as_int(car(a))]);
}
static Val bi_comp_loop_save(Val a) {
    // Returns [local_count loop_count loop_start in_loop] as pvec
    (void)a;
    CPVec *v = arena_push(&g_req, CPVec);
    *v = cpvec_empty();
    *v = cpvec_append(*v, val_int(g_comp.local_count));
    *v = cpvec_append(*v, val_int(g_comp.loop_count));
    *v = cpvec_append(*v, val_int(g_comp.loop_start));
    *v = cpvec_append(*v, val_bool(g_comp.in_loop));
    // Save loop vars/offs
    for (u32 i = 0; i < g_comp.loop_count; i++) {
        *v = cpvec_append(*v, val_int(g_comp.loop_vars[i]));
        *v = cpvec_append(*v, val_int(g_comp.loop_offs[i]));
    }
    return val_pvec(v);
}
static Val bi_comp_loop_restore(Val a) {
    // Restores from pvec saved by comp-loop-save!
    CPVec *v = (CPVec *)val_as_pvec(car(a));
    g_comp.local_count = (u32)val_as_int(cpvec_get(*v, 0));
    g_comp.loop_count  = (u32)val_as_int(cpvec_get(*v, 1));
    g_comp.loop_start  = (u32)val_as_int(cpvec_get(*v, 2));
    g_comp.in_loop     = val_truthy(cpvec_get(*v, 3));
    for (u32 i = 0; i < g_comp.loop_count; i++) {
        g_comp.loop_vars[i] = (StrId)val_as_int(cpvec_get(*v, 4 + i * 2));
        g_comp.loop_offs[i] = (i32)val_as_int(cpvec_get(*v, 5 + i * 2));
    }
    return NIL;
}

// --- function/global registry ---
static Val bi_jit_gen_inc(Val a)    { (void)a; g_gen++; return NIL; }
static Val bi_jit_fix_reset(Val a)  { (void)a; g_fix_count = 0; g_global_count = 0; return NIL; }
static Val bi_jit_register_fn(Val a) {
    StrId name = (StrId)val_as_int(car(a));
    i32 offset = (i32)val_as_int(car(cdr(a)));
    g_fn_offset[name] = offset;
    g_fn_gen[name] = g_gen;
    return NIL;
}
static Val bi_jit_find_fn(Val a) {
    return val_int(find_fn((StrId)val_as_int(car(a))));
}
static Val bi_jit_register_global(Val a) {
    StrId name = (StrId)val_as_int(car(a));
    g_global_idx[name] = (i32)g_global_count;
    g_global_gen[name] = g_gen;
    g_globals[g_global_count] = 0;
    i32 idx = (i32)g_global_count;
    g_global_count++;
    return val_int(idx);
}
static Val bi_jit_find_global(Val a) {
    return val_int(find_global((StrId)val_as_int(car(a))));
}
static Val bi_jit_global_addr(Val a) {
    // Return global index (for store-abs/load-abs)
    return val_int((i32)val_as_int(car(a)));
}
static Val bi_jit_add_fix(Val a) {
    u32 pos = (u32)val_as_int(car(a));
    StrId target = (StrId)val_as_int(car(cdr(a)));
    g_fixes[g_fix_count++] = (CallFix){pos, target};
    return NIL;
}
static Val bi_jit_patch_calls(Val a) {
    (void)a;
    for (u32 i = 0; i < g_fix_count; i++) {
        i32 target = find_fn(g_fixes[i].target);
        if (target >= 0) {
            i32 rel = (i32)(target - (g_fixes[i].pos + 4));
            memcpy(g_code.code + g_fixes[i].pos, &rel, 4);
        }
    }
    return NIL;
}
static Val bi_jit_exec(Val a) {
    u32 entry = (u32)val_as_int(car(a));
    JitFn0 fn = (JitFn0)(g_code.code + entry);
    return val_int(fn());
}

// --- register accessors ---
static Val bi_arg_reg(Val a) {
    u32 i = (u32)val_as_int(car(a));
    return val_int(i < 4 ? ARG_REGS[i] : 0);
}
static Val bi_cs_pool(Val a) {
    u32 i = (u32)val_as_int(car(a));
    return val_int(i < N_CS ? CS_POOL[i] : 0);
}

// --- Registration ---
static void register_jit_builtins(Env *env) {
    #define REG(n, f) env_set(env, INTERN(n), make_builtin(INTERN(n), f))
    // x86 encoding
    REG("x86-imm!", bi_x86_imm);     REG("x86-mov!", bi_x86_mov);
    REG("x86-add!", bi_x86_add);     REG("x86-sub!", bi_x86_sub);
    REG("x86-cmp!", bi_x86_cmp);     REG("x86-test!", bi_x86_test);
    REG("x86-imul!", bi_x86_imul);   REG("x86-idiv!", bi_x86_idiv);
    REG("x86-neg!", bi_x86_neg);
    REG("x86-push!", bi_x86_push);   REG("x86-pop!", bi_x86_pop);
    REG("x86-load!", bi_x86_load);   REG("x86-store!", bi_x86_store);
    REG("x86-load-abs!", bi_x86_load_abs);  REG("x86-store-abs!", bi_x86_store_abs);
    REG("x86-jmp!", bi_x86_jmp);     REG("x86-call!", bi_x86_call);
    REG("x86-ret!", bi_x86_ret);     REG("x86-jcc!", bi_x86_jcc);
    REG("x86-patch!", bi_x86_patch); REG("x86-jmp-to!", bi_x86_jmp_to);
    REG("x86-setcc!", bi_x86_setcc);
    REG("x86-prologue!", bi_x86_pro);  REG("x86-epilogue!", bi_x86_epi);
    REG("x86-add-i8!", bi_x86_addi);  REG("x86-sub-i8!", bi_x86_subi);
    REG("x86-push-r!", bi_x86_push_r);  REG("x86-pop-r!", bi_x86_pop_r);
    REG("x86-mov-rr!", bi_x86_mov_rr);
    // Bit ops
    REG("x86-and!", bi_x86_and);     REG("x86-or!", bi_x86_or);
    REG("x86-xor!", bi_x86_xor);    REG("x86-bit-not!", bi_x86_bit_not);
    REG("x86-shl-cl!", bi_x86_shl_cl);  REG("x86-shr-cl!", bi_x86_shr_cl);
    REG("x86-and-i!", bi_x86_and_i);    REG("x86-popcnt!", bi_x86_popcnt);
    // Memory addressing
    REG("x86-load-base!", bi_x86_load_base);    REG("x86-store-base!", bi_x86_store_base);
    REG("x86-load32-base!", bi_x86_load32_base);  REG("x86-store32-base!", bi_x86_store32_base);
    REG("x86-load8-base!", bi_x86_load8_base);  REG("x86-store8-base!", bi_x86_store8_base);
    REG("xb!", bi_xb);  REG("x32!", bi_x32);
    // Code buffer
    REG("code-pos", bi_code_pos);    REG("code-reset!", bi_code_reset);
    // Compiler state
    REG("comp-reset!", bi_comp_reset);
    REG("comp-alloc-slot!", bi_comp_alloc_slot);
    REG("comp-find-local", bi_comp_find_local);
    REG("comp-find-cs", bi_comp_find_cs);
    REG("comp-alloc-cs-temp!", bi_comp_alloc_cs_temp);
    REG("comp-free-cs-temp!", bi_comp_free_cs_temp);
    REG("comp-save-locals!", bi_comp_save_locals);
    REG("comp-restore-locals!", bi_comp_restore_locals);
    REG("comp-set-slot!", bi_comp_set_slot);
    REG("comp-cs-setup!", bi_comp_cs_setup);
    REG("comp-cs-name!", bi_comp_cs_name);
    REG("comp-cs-mode?", bi_comp_cs_mode);
    REG("comp-loop-start", bi_comp_loop_start);
    REG("comp-set-loop-start!", bi_comp_set_loop_start);
    REG("comp-in-loop?", bi_comp_in_loop);
    REG("comp-set-in-loop!", bi_comp_set_in_loop);
    REG("comp-loop-var!", bi_comp_loop_var);
    REG("comp-loop-count", bi_comp_loop_count);
    REG("comp-set-loop-count!", bi_comp_set_loop_count);
    REG("comp-loop-off", bi_comp_loop_off);
    REG("comp-loop-save!", bi_comp_loop_save);
    REG("comp-loop-restore!", bi_comp_loop_restore);
    // Function/global registry
    REG("jit-gen-inc!", bi_jit_gen_inc);
    REG("jit-fix-reset!", bi_jit_fix_reset);
    REG("jit-register-fn!", bi_jit_register_fn);
    REG("jit-find-fn", bi_jit_find_fn);
    REG("jit-register-global!", bi_jit_register_global);
    REG("jit-find-global", bi_jit_find_global);
    REG("jit-global-addr", bi_jit_global_addr);
    REG("jit-add-fix!", bi_jit_add_fix);
    REG("jit-patch-calls!", bi_jit_patch_calls);
    REG("jit-exec!", bi_jit_exec);
    // Register accessors
    REG("arg-reg", bi_arg_reg);
    REG("cs-pool", bi_cs_pool);
    #undef REG
}

// Clojure JIT entry point: parse → analyze → Clojure compile → exec
static i64 clj_jit_run(const char *source) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;

    world_step(source, true);
    Val entry_val = engine_eval("(cg-compile-program)");
    u32 entry = (u32)val_as_int(entry_val);
    JitFn0 fn = (JitFn0)(g_code.code + entry);
    return fn();
}

#endif // JIT_C_INCLUDED
