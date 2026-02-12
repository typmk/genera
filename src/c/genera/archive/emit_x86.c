/**
 * emit_x86.c — Layer 2b: x86-64 JIT Emitter
 *
 * Compiles Clojure forms directly to x86-64 machine code. No gcc. No assembler.
 * Source → intern → query as x86 → mmap → execute.
 * Depends on: base.c, read.c
 */
#ifndef EMIT_X86_INCLUDED
#define EMIT_X86_INCLUDED

// ============================================================================
// 1. x86-64 Encoding
// ============================================================================

typedef struct { u8 *code; u32 pos, cap; } CodeBuf;

enum { RAX=0, RCX=1, RDX=2, RBX=3, RSP=4, RBP=5, RSI=6, RDI=7 };
static const u8 ARG_REGS[] = {RDI, RSI, RDX, RCX};
#define MODRM(mod, reg, rm) ((u8)(((mod)<<6)|((reg)<<3)|(rm)))

static void xb(CodeBuf *c, u8 v) { c->code[c->pos++] = v; }
static void x32(CodeBuf *c, u32 v) { memcpy(c->code+c->pos, &v, 4); c->pos += 4; }
static void x64(CodeBuf *c, u64 v) { memcpy(c->code+c->pos, &v, 8); c->pos += 8; }

// REX.W + opcode + ModRM (reg-reg, mod=3)
static void xrr(CodeBuf *c, u8 op, u8 reg, u8 rm) {
    xb(c, 0x48); xb(c, op); xb(c, MODRM(3, reg, rm));
}

// mov reg, immediate (picks shortest encoding)
static void x_imm(CodeBuf *c, u8 reg, i64 v) {
    if (v >= 0 && v <= 0x7FFFFFFF) {
        xb(c, 0xB8 + reg); x32(c, (u32)v);
    } else if (v >= -0x80000000LL && v < 0) {
        xb(c, 0x48); xb(c, 0xC7); xb(c, MODRM(3, 0, reg)); x32(c, (u32)(i32)v);
    } else {
        xb(c, 0x48); xb(c, 0xB8 + reg); x64(c, (u64)v);
    }
}

// Register-register ops
static void x_mov(CodeBuf *c, u8 d, u8 s) { xrr(c, 0x89, s, d); }
static void x_add(CodeBuf *c, u8 d, u8 s) { xrr(c, 0x01, s, d); }
static void x_sub(CodeBuf *c, u8 d, u8 s) { xrr(c, 0x29, s, d); }
static void x_cmp(CodeBuf *c, u8 a, u8 b) { xrr(c, 0x39, b, a); }
static void x_test(CodeBuf *c, u8 a, u8 b){ xrr(c, 0x85, b, a); }

static void x_imul(CodeBuf *c, u8 d, u8 s) {
    xb(c, 0x48); xb(c, 0x0F); xb(c, 0xAF); xb(c, MODRM(3, d, s));
}
static void x_idiv(CodeBuf *c, u8 s) {
    xb(c, 0x48); xb(c, 0x99);   // cqo
    xb(c, 0x48); xb(c, 0xF7); xb(c, MODRM(3, 7, s));
}
static void x_neg(CodeBuf *c, u8 r) {
    xb(c, 0x48); xb(c, 0xF7); xb(c, MODRM(3, 3, r));
}

// Push / pop
static void x_push(CodeBuf *c, u8 r) { xb(c, 0x50 + r); }
static void x_pop(CodeBuf *c, u8 r)  { xb(c, 0x58 + r); }

// Load/store: [rbp + disp]
static void x_load(CodeBuf *c, u8 reg, i32 off) {
    xb(c, 0x48); xb(c, 0x8B);
    if (off >= -128 && off <= 127) {
        xb(c, MODRM(1, reg, RBP)); xb(c, (u8)(i8)off);
    } else {
        xb(c, MODRM(2, reg, RBP)); x32(c, (u32)off);
    }
}
static void x_store(CodeBuf *c, i32 off, u8 reg) {
    xb(c, 0x48); xb(c, 0x89);
    if (off >= -128 && off <= 127) {
        xb(c, MODRM(1, reg, RBP)); xb(c, (u8)(i8)off);
    } else {
        xb(c, MODRM(2, reg, RBP)); x32(c, (u32)off);
    }
}

// Load from absolute address: mov rax, imm64; mov rax, [rax]
static void x_load_abs(CodeBuf *c, void *addr) {
    x_imm(c, RAX, (i64)addr);
    xb(c, 0x48); xb(c, 0x8B); xb(c, MODRM(0, RAX, RAX));
}
// Store rax to absolute address: mov rcx,rax; mov rax,addr; mov [rax],rcx
static void x_store_abs(CodeBuf *c, void *addr) {
    x_mov(c, RCX, RAX);
    x_imm(c, RAX, (i64)addr);
    xb(c, 0x48); xb(c, 0x89); xb(c, MODRM(0, RCX, RAX));
}

// Jumps (return fixup offset for forward refs)
static u32 x_jmp(CodeBuf *c)      { xb(c, 0xE9); u32 f=c->pos; x32(c,0); return f; }
static u32 x_call(CodeBuf *c)     { xb(c, 0xE8); u32 f=c->pos; x32(c,0); return f; }
static void x_ret(CodeBuf *c)     { (void)c; xb(c, 0xC3); }

// Conditional jumps: 0x0F 0x80+cc rel32
#define CC_E  0x4
#define CC_NE 0x5
#define CC_L  0xC
#define CC_GE 0xD
#define CC_LE 0xE
#define CC_G  0xF
static u32 x_jcc(CodeBuf *c, u8 cc) {
    xb(c, 0x0F); xb(c, 0x80+cc); u32 f=c->pos; x32(c,0); return f;
}

// Patch rel32 fixup to current position
static void x_patch(CodeBuf *c, u32 fixup) {
    i32 rel = (i32)(c->pos - (fixup + 4));
    memcpy(c->code + fixup, &rel, 4);
}
// Jump to known (backward) target
static void x_jmp_to(CodeBuf *c, u32 target) {
    xb(c, 0xE9);
    i32 rel = (i32)(target - (c->pos + 4));
    x32(c, (u32)rel);
}

// setcc al + movzx eax, al → rax = 0 or 1
static void x_setcc(CodeBuf *c, u8 cc) {
    xb(c, 0x0F); xb(c, 0x90+cc); xb(c, 0xC0);  // setcc al
    xb(c, 0x0F); xb(c, 0xB6); xb(c, 0xC0);      // movzx eax, al
}

// Prologue / epilogue
static void x_prologue(CodeBuf *c, u32 frame) {
    x_push(c, RBP);
    x_mov(c, RBP, RSP);
    if (frame) {
        xb(c, 0x48); xb(c, 0x81); xb(c, MODRM(3, 5, RSP)); x32(c, frame);
    }
}
static void x_epilogue(CodeBuf *c) {
    xb(c, 0xC9); xb(c, 0xC3); // leave; ret
}

// add/sub immediate byte
static void x_add_i8(CodeBuf *c, u8 r, i8 v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 0, r)); xb(c, (u8)v);
}
static void x_sub_i8(CodeBuf *c, u8 r, i8 v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 5, r)); xb(c, (u8)v);
}

// Extended register encoding (r8-r15): REX.B for rm, REX.R for reg
static void x_push_r(CodeBuf *c, u8 r) {
    if (r >= 8) xb(c, 0x41);
    xb(c, 0x50 + (r & 7));
}
static void x_pop_r(CodeBuf *c, u8 r) {
    if (r >= 8) xb(c, 0x41);
    xb(c, 0x58 + (r & 7));
}
static void x_mov_rr(CodeBuf *c, u8 dst, u8 src) {
    u8 rex = 0x48;
    if (src >= 8) rex |= 0x04;
    if (dst >= 8) rex |= 0x01;
    xb(c, rex); xb(c, 0x89); xb(c, MODRM(3, src & 7, dst & 7));
}
static void x_add_rr(CodeBuf *c, u8 dst, u8 src) {
    u8 rex = 0x48;
    if (src >= 8) rex |= 0x04;
    if (dst >= 8) rex |= 0x01;
    xb(c, rex); xb(c, 0x01); xb(c, MODRM(3, src & 7, dst & 7));
}
static void x_sub_rr(CodeBuf *c, u8 dst, u8 src) {
    u8 rex = 0x48;
    if (src >= 8) rex |= 0x04;
    if (dst >= 8) rex |= 0x01;
    xb(c, rex); xb(c, 0x29); xb(c, MODRM(3, src & 7, dst & 7));
}

// ============================================================================
// 2. Compiler State + Callee-Saved Register Allocator
// ============================================================================

// Callee-saved register pool: rbx, r12, r13, r14, r15
// Tree depth = register pressure. Counter, not graph.
enum { R12=12, R13=13, R14=14, R15=15 };
static const u8 CS_POOL[] = {RBX, R12, R13, R14, R15};
#define N_CS 5

typedef struct { StrId name; i32 offset; } Local;

typedef struct {
    CodeBuf *cb;
    Local locals[64];
    u32 local_count;
    i32 next_slot;        // next stack offset (grows negative: -8, -16, ...)
    // Callee-saved register allocation
    u8  cs_param;          // params allocated to CS regs
    u8  cs_temp;           // temp CS regs in use (above params)
    u8  cs_max;            // high water mark (for prologue save count)
    StrId cs_names[N_CS];  // what variable is in each CS slot
    bool cs_mode;          // true = register-allocated function
    // Loop/TCO context
    StrId loop_vars[16];
    i32   loop_offs[16];
    u32   loop_count;
    u32   loop_start;     // code position to jump back to
    bool  in_loop;
} Comp;

// Find variable in CS regs: returns register number, or -1
static i8 find_cs(Comp *cc, StrId name) {
    for (u32 i = 0; i < cc->cs_param; i++)
        if (cc->cs_names[i] == name) return (i8)CS_POOL[i];
    return -1;
}

// Allocate a temp CS reg: returns register number, or -1 (use push/pop)
static i8 alloc_cs_temp(Comp *cc) {
    u32 idx = cc->cs_param + cc->cs_temp;
    if (idx >= N_CS) return -1;
    cc->cs_temp++;
    if (idx + 1 > cc->cs_max) cc->cs_max = idx + 1;
    return (i8)CS_POOL[idx];
}
static void free_cs_temp(Comp *cc) { cc->cs_temp--; }

// O(1) function lookup — indexed by StrId (the program IS a table)
// Generation counter: reset = g_gen++. Stale entries have wrong generation.
// No memset. For fib (1 defn): 1 write instead of 4096.
#define STRID_MAX 4096
static i32 g_fn_offset[STRID_MAX];   // StrId → code offset
static u16 g_fn_gen[STRID_MAX];      // per-entry generation stamp
static u16 g_gen;                     // current compilation generation

// Call fixups
typedef struct { u32 pos; StrId target; } CallFix;
static CallFix g_fixes[4096];
static u32 g_fix_count;

// O(1) global lookup — indexed by StrId
static i64 g_globals[256];
static i32 g_global_idx[STRID_MAX];
static u16 g_global_gen[STRID_MAX];  // per-entry generation stamp
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

// Comparison op → condition code
static i32 cmp_cc(StrId op) {
    if (op == S_EQ)  return CC_E;
    if (op == S_LT)  return CC_L;
    if (op == S_GT)  return CC_G;
    if (op == S_LTE) return CC_LE;
    if (op == S_GTE) return CC_GE;
    return -1;
}

// Binary arithmetic: result in rax
static void c_binop(Comp *cc, StrId op, Val args) {
    u32 n = list_len(args);
    if (n == 1 && op == S_SUB) {
        c_expr(cc, car(args)); x_neg(cc->cb, RAX); return;
    }
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
        else if (op == S_MOD) {
            x_idiv(cc->cb, RCX);
            x_mov(cc->cb, RAX, RDX);  // remainder in rdx
        }
        args = cdr(args);
    }
}

// Comparison: result 0 or 1 in rax
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
    // Fused compare-and-branch: (if (< a b) ...) → cmp a,b; jge else
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
            u32 f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));  // invert condition
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
    // Fallback: eval test, test rax, branch
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
    Val bindings = car(args);
    while (val_is_cons(bindings)) {
        StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
        c_expr(cc, car(bindings)); bindings = cdr(bindings);
        i32 off = alloc_slot(cc, name);
        x_store(cc->cb, off, RAX);
    }
    Val body = cdr(args);
    while (val_is_cons(body)) {
        c_expr(cc, car(body)); body = cdr(body);
    }
    cc->local_count = saved_count;
}

static void c_do(Comp *cc, Val args) {
    while (val_is_cons(args)) {
        c_expr(cc, car(args)); args = cdr(args);
    }
}

static void c_and(Comp *cc, Val args) {
    if (!val_is_cons(args)) { x_imm(cc->cb, RAX, 1); return; }
    c_expr(cc, car(args));
    args = cdr(args);
    u32 fixups[16]; u32 fc = 0;
    while (val_is_cons(args)) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_E);
        c_expr(cc, car(args));
        args = cdr(args);
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

static void c_or(Comp *cc, Val args) {
    if (!val_is_cons(args)) { x_imm(cc->cb, RAX, 0); return; }
    c_expr(cc, car(args));
    args = cdr(args);
    u32 fixups[16]; u32 fc = 0;
    while (val_is_cons(args)) {
        x_test(cc->cb, RAX, RAX);
        fixups[fc++] = x_jcc(cc->cb, CC_NE);
        c_expr(cc, car(args));
        args = cdr(args);
    }
    for (u32 i = 0; i < fc; i++) x_patch(cc->cb, fixups[i]);
}

// Recur: evaluate args, store to loop vars, jump back
static void c_recur(Comp *cc, Val args) {
    u32 n = 0;
    Val a = args;
    while (val_is_cons(a)) {
        c_expr(cc, car(a));
        x_push(cc->cb, RAX);
        a = cdr(a); n++;
    }
    for (i32 i = n - 1; i >= 0; i--) {
        x_pop(cc->cb, RAX);
        x_store(cc->cb, cc->loop_offs[i], RAX);
    }
    x_jmp_to(cc->cb, cc->loop_start);
}

static void c_loop(Comp *cc, Val args) {
    // Save outer loop context
    StrId sv[16]; i32 so[16]; u32 sc = cc->loop_count; u32 ss = cc->loop_start;
    bool si = cc->in_loop;
    memcpy(sv, cc->loop_vars, sc * sizeof(StrId));
    memcpy(so, cc->loop_offs, sc * sizeof(i32));

    u32 saved_count = cc->local_count;
    cc->loop_count = 0;

    Val bindings = car(args);
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
    while (val_is_cons(body)) {
        c_expr(cc, car(body)); body = cdr(body);
    }

    // Restore
    cc->local_count = saved_count;
    cc->loop_count = sc; cc->loop_start = ss; cc->in_loop = si;
    memcpy(cc->loop_vars, sv, sc * sizeof(StrId));
    memcpy(cc->loop_offs, so, sc * sizeof(i32));
}

// Tail position: handles if/do/let/recur for TCO
static void c_tail(Comp *cc, Val form) {
    if (val_is_cons(form) && val_is_sym(car(form))) {
        StrId sym = val_as_sym(car(form));
        Val args = cdr(form);
        if (sym == S_RECUR) { c_recur(cc, args); return; }
        if (sym == S_IF) {
            // Fused compare-and-branch in tail position
            Val test = car(args);
            u32 f_else;
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
                    f_else = x_jcc(cc->cb, (u8)(fcc ^ 1));
                    goto tail_if_body;
                }
            }
            c_expr(cc, test);
            x_test(cc->cb, RAX, RAX);
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
            while (val_is_cons(args) && val_is_cons(cdr(args))) {
                c_expr(cc, car(args)); args = cdr(args);
            }
            if (val_is_cons(args)) c_tail(cc, car(args));
            return;
        }
        if (sym == S_LET) {
            u32 saved = cc->local_count;
            Val bindings = car(args);
            while (val_is_cons(bindings)) {
                StrId name = val_as_sym(car(bindings)); bindings = cdr(bindings);
                c_expr(cc, car(bindings)); bindings = cdr(bindings);
                i32 off = alloc_slot(cc, name);
                x_store(cc->cb, off, RAX);
            }
            Val body = cdr(args);
            while (val_is_cons(body) && val_is_cons(cdr(body))) {
                c_expr(cc, car(body)); body = cdr(body);
            }
            if (val_is_cons(body)) c_tail(cc, car(body));
            cc->local_count = saved;
            return;
        }
    }
    c_expr(cc, form);
}

// Main expression dispatcher
static void c_expr(Comp *cc, Val form) {
    if (val_is_int(form))  { x_imm(cc->cb, RAX, val_as_int(form)); return; }
    if (val_is_bool(form)) { x_imm(cc->cb, RAX, val_as_bool(form) ? 1 : 0); return; }
    if (val_is_nil(form))  { x_imm(cc->cb, RAX, 0); return; }
    if (val_is_sym(form)) {
        StrId name = val_as_sym(form);
        if (cc->cs_mode) {
            i8 r = find_cs(cc, name);
            if (r >= 0) { x_mov_rr(cc->cb, RAX, (u8)r); return; }
        }
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
        // 1-arg fast path: eval → mov rdi, rax → call (no push/pop)
        c_expr(cc, car(args));
        x_mov(cc->cb, RDI, RAX);
    } else {
        Val a = args;
        while (val_is_cons(a)) {
            c_expr(cc, car(a));
            x_push(cc->cb, RAX);
            a = cdr(a);
        }
        for (i32 i = n - 1; i >= 0; i--)
            x_pop(cc->cb, ARG_REGS[i]);
    }
    u32 fix = x_call(cc->cb);
    g_fixes[g_fix_count++] = (CallFix){fix, sym};
}

// ============================================================================
// 4. Program Compiler + JIT
// ============================================================================

#define CODE_SIZE (1 << 20)  // 1 MB
static CodeBuf g_code;

static void compile_defn(DefnInfo *fn) {
    bool tco = has_recur(fn->body);
    bool cs_mode = !tco && fn->n_params <= N_CS;
    u8 n_cs = cs_mode ? (u8)fn->n_params : 0;

    Comp cc = {.cb = &g_code, .local_count = 0,
               .next_slot = -(i32)(n_cs * 8),   // skip CS save area
               .in_loop = false, .loop_count = 0,
               .cs_mode = cs_mode, .cs_param = n_cs, .cs_temp = 0, .cs_max = n_cs};
    u32 fn_start = g_code.pos;

    if (cs_mode) {
        // CS prologue: push rbp, save CS regs, sub rsp for alignment + locals
        x_push(&g_code, RBP);
        x_mov(&g_code, RBP, RSP);
        for (u32 i = 0; i < n_cs; i++)
            x_push_r(&g_code, CS_POOL[i]);
        // Alignment: after push rbp + n_cs pushes, need rsp ≡ 0 mod 16
        u32 frame = (n_cs & 1) ? 248 : 256;
        xb(&g_code, 0x48); xb(&g_code, 0x81); xb(&g_code, MODRM(3, 5, RSP)); x32(&g_code, frame);

        // Move params from arg regs to CS regs
        Val p = fn->params; u32 pi = 0;
        while (val_is_cons(p)) {
            cc.cs_names[pi] = val_as_sym(car(p));
            x_mov_rr(&g_code, CS_POOL[pi], ARG_REGS[pi]);
            p = cdr(p); pi++;
        }

        // Compile body
        Val body = fn->body;
        while (val_is_cons(body)) {
            c_expr(&cc, car(body)); body = cdr(body);
        }

        // CS epilogue: restore frame, CS regs, rbp
        xb(&g_code, 0x48); xb(&g_code, 0x81); xb(&g_code, MODRM(3, 0, RSP)); x32(&g_code, frame);
        for (i32 i = (i32)n_cs - 1; i >= 0; i--)
            x_pop_r(&g_code, CS_POOL[i]);
        x_pop(&g_code, RBP);
        x_ret(&g_code);
    } else {
        // Stack-based path (TCO or many params)
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
            while (val_is_cons(body) && val_is_cons(cdr(body))) {
                c_expr(&cc, car(body)); body = cdr(body);
            }
            if (val_is_cons(body)) c_tail(&cc, car(body));
        } else {
            Val body = fn->body;
            while (val_is_cons(body)) {
                c_expr(&cc, car(body)); body = cdr(body);
            }
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
    g_gen++;  // invalidate all fn/global entries — O(1) reset

    classify(source);
    image_build();

    for (u32 i = 0; i < g_def_count; i++) {
        g_global_idx[g_defs[i].name] = (i32)g_global_count;
        g_global_gen[g_defs[i].name] = g_gen;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }

    for (u32 i = 0; i < g_defn_count; i++)
        compile_defn(&g_defns[i]);

    u32 entry = g_code.pos;
    Comp cc = {.cb = &g_code, .local_count = 0, .next_slot = 0, .in_loop = false, .loop_count = 0};
    x_prologue(&g_code, 256);

    for (u32 i = 0; i < g_def_count; i++) {
        c_expr(&cc, g_defs[i].value);
        x_store_abs(&g_code, &g_globals[i]);
    }
    for (u32 i = 0; i < g_main_count; i++)
        c_expr(&cc, g_mains[i]);

    x_epilogue(&g_code);

    // Patch all call fixups
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

#endif // EMIT_X86_INCLUDED
