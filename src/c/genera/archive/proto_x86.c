/**
 * proto_x86.c — Clojure-to-x86-64 JIT Compiler
 *
 * Source → intern → query as x86 → mmap → execute. No gcc. No assembler.
 * Phase 3 of the compilation pipeline: we ARE the compiler.
 *
 * Build:
 *   gcc -O3 -march=native -o proto_x86 test/proto_x86.c
 *   ./proto_x86
 */

#define PROTO_BASE_NO_MAIN
#include "proto_base.c"
#include <sys/mman.h>
#include <stdarg.h>

// ============================================================================
// 1. Cons Cells + Reader (shared with proto_clj.c)
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
    Val result = NIL;
    while (val_is_cons(head)) {
        Val next = cdr(head);
        ((Cons *)val_as_cons(head))->cdr = result;
        result = head; head = next;
    }
    return result;
}

static Val read_atom(Reader *r) {
    if (r->src[r->pos] == '-' && r->pos + 1 < r->len &&
        r->src[r->pos + 1] >= '0' && r->src[r->pos + 1] <= '9') {
        r->pos++;
        i64 n = 0;
        while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9')
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(-n);
    }
    if (r->src[r->pos] >= '0' && r->src[r->pos] <= '9') {
        i64 n = 0;
        while (r->pos < r->len && r->src[r->pos] >= '0' && r->src[r->pos] <= '9')
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(n);
    }
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
// 2. Symbols + Classification
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

static bool has_recur(Val form) {
    if (!val_is_cons(form)) return false;
    Val h = car(form);
    if (val_is_sym(h)) {
        if (val_as_sym(h) == S_RECUR) return true;
        if (val_as_sym(h) == S_LOOP) return false;
    }
    Val f = form;
    while (val_is_cons(f)) {
        if (has_recur(car(f))) return true;
        f = cdr(f);
    }
    return false;
}

// ============================================================================
// 3. x86-64 Encoding
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
        xb(c, MODRM(1, reg, RBP)); xb(c, (u8)(int8_t)off);
    } else {
        xb(c, MODRM(2, reg, RBP)); x32(c, (u32)off);
    }
}
static void x_store(CodeBuf *c, i32 off, u8 reg) {
    xb(c, 0x48); xb(c, 0x89);
    if (off >= -128 && off <= 127) {
        xb(c, MODRM(1, reg, RBP)); xb(c, (u8)(int8_t)off);
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
static void x_ret(CodeBuf *c)     { xb(c, 0xC3); }

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
static void x_add_i8(CodeBuf *c, u8 r, int8_t v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 0, r)); xb(c, (u8)v);
}
static void x_sub_i8(CodeBuf *c, u8 r, int8_t v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 5, r)); xb(c, (u8)v);
}

// ============================================================================
// 4. Compiler State
// ============================================================================

typedef struct { StrId name; i32 offset; } Local;

typedef struct {
    CodeBuf *cb;
    Local locals[64];
    u32 local_count;
    i32 next_slot;        // next stack offset (grows negative: -8, -16, ...)
    // Loop/TCO context
    StrId loop_vars[16];
    i32   loop_offs[16];
    u32   loop_count;
    u32   loop_start;     // code position to jump back to
    bool  in_loop;
} Comp;

// Function table
typedef struct { StrId name; u32 offset; } FnEntry;
static FnEntry g_fntab[256];
static u32 g_fn_count;

// Call fixups
typedef struct { u32 pos; StrId target; } CallFix;
static CallFix g_fixes[4096];
static u32 g_fix_count;

// Global defs
static i64 g_globals[256];
static StrId g_global_names[256];
static u32 g_global_count;

static i32 find_fn(StrId name) {
    for (u32 i = 0; i < g_fn_count; i++)
        if (g_fntab[i].name == name) return (i32)g_fntab[i].offset;
    return -1;
}

static i32 find_local(Comp *cc, StrId name) {
    for (i32 i = (i32)cc->local_count - 1; i >= 0; i--)
        if (cc->locals[i].name == name) return cc->locals[i].offset;
    return 0;
}

static i32 find_global(StrId name) {
    for (u32 i = 0; i < g_global_count; i++)
        if (g_global_names[i] == name) return (i32)i;
    return -1;
}

static i32 alloc_slot(Comp *cc, StrId name) {
    cc->next_slot -= 8;
    cc->locals[cc->local_count] = (Local){name, cc->next_slot};
    cc->local_count++;
    return cc->next_slot;
}

// ============================================================================
// 5. Expression Compiler
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
    // Unary minus
    if (n == 1 && op == S_SUB) {
        c_expr(cc, car(args)); x_neg(cc->cb, RAX); return;
    }
    if (n == 0) { x_imm(cc->cb, RAX, (op == S_MUL) ? 1 : 0); return; }

    c_expr(cc, car(args));  // first arg → rax
    args = cdr(args);
    while (val_is_cons(args)) {
        x_push(cc->cb, RAX);
        c_expr(cc, car(args));
        x_mov(cc->cb, RCX, RAX);
        x_pop(cc->cb, RAX);
        // rax = left, rcx = right
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
    c_expr(cc, car(args));               // condition
    x_test(cc->cb, RAX, RAX);
    u32 f_else = x_jcc(cc->cb, CC_E);   // je else
    c_expr(cc, car(cdr(args)));          // then
    u32 f_end = x_jmp(cc->cb);          // jmp end
    x_patch(cc->cb, f_else);
    Val e = cdr(cdr(args));
    if (val_is_cons(e)) c_expr(cc, car(e)); // else
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
    cc->local_count = saved_count;  // restore scope
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
        fixups[fc++] = x_jcc(cc->cb, CC_E);  // short-circuit if 0
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
        fixups[fc++] = x_jcc(cc->cb, CC_NE);  // short-circuit if non-zero
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
            c_expr(cc, car(args));
            x_test(cc->cb, RAX, RAX);
            u32 f_else = x_jcc(cc->cb, CC_E);
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

    // Function call: evaluate args, push, pop into arg regs, call
    u32 n = 0;
    Val a = args;
    while (val_is_cons(a)) {
        c_expr(cc, car(a));
        x_push(cc->cb, RAX);
        a = cdr(a); n++;
    }
    for (i32 i = n - 1; i >= 0; i--)
        x_pop(cc->cb, ARG_REGS[i]);

    // Emit call with fixup
    u32 fix = x_call(cc->cb);
    g_fixes[g_fix_count++] = (CallFix){fix, sym};
}

// ============================================================================
// 6. Program Compiler + JIT
// ============================================================================

#define CODE_SIZE (1 << 20)  // 1 MB
static CodeBuf g_code;

static void compile_defn(DefnInfo *fn) {
    Comp cc = {.cb = &g_code, .local_count = 0, .next_slot = 0, .in_loop = false, .loop_count = 0};
    u32 fn_start = g_code.pos;

    // Prologue with fixed frame
    x_prologue(&g_code, 256);

    // Spill params to stack
    Val p = fn->params; u32 pi = 0;
    while (val_is_cons(p)) {
        i32 off = alloc_slot(&cc, val_as_sym(car(p)));
        x_store(&g_code, off, ARG_REGS[pi]);
        p = cdr(p); pi++;
    }

    bool tco = has_recur(fn->body);
    if (tco) {
        // Set up TCO loop context
        cc.in_loop = true;
        cc.loop_count = fn->n_params;
        p = fn->params;
        for (u32 i = 0; i < fn->n_params; i++) {
            cc.loop_vars[i] = val_as_sym(car(p));
            cc.loop_offs[i] = find_local(&cc, cc.loop_vars[i]);
            p = cdr(p);
        }
        cc.loop_start = g_code.pos;

        // Compile body in tail position
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
    g_fntab[g_fn_count++] = (FnEntry){fn->name, fn_start};
}

// Compile program, return entry function offset
static u32 compile_program(const char *source) {
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fn_count = 0;
    g_fix_count = 0;
    g_global_count = 0;

    classify(source);

    // Register globals
    for (u32 i = 0; i < g_def_count; i++) {
        g_global_names[g_global_count] = g_defs[i].name;
        g_globals[g_global_count] = 0;
        g_global_count++;
    }

    // Compile all defns
    for (u32 i = 0; i < g_defn_count; i++)
        compile_defn(&g_defns[i]);

    // Compile entry function (defs + mains)
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
        if (target >= 0)
            x_patch(&g_code, g_fixes[i].pos);  // use x_patch trick
    }
    // Re-patch: find_fn returns code offset, need to compute rel32
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

// ============================================================================
// 7. Tests
// ============================================================================

static int t_pass, t_fail;

static void check(const char *name, const char *source, i64 expected) {
    i64 got = jit_run(source);
    if (got == expected) {
        t_pass++;
    } else {
        printf("  FAIL %s: expected %lld, got %lld\n", name, (long long)expected, (long long)got);
        t_fail++;
    }
}

static void run_tests(void) {
    printf("=== proto_x86 tests ===\n");
    t_pass = t_fail = 0;

    // Literals
    check("int", "42", 42);
    check("neg", "-7", -7);
    check("zero", "0", 0);

    // Arithmetic
    check("add", "(+ 1 2)", 3);
    check("sub", "(- 10 3)", 7);
    check("mul", "(* 6 7)", 42);
    check("div", "(/ 20 4)", 5);
    check("mod", "(mod 17 5)", 2);
    check("neg-unary", "(- 5)", -5);
    check("add3", "(+ 1 2 3)", 6);
    check("mul3", "(* 2 3 4)", 24);
    check("nested", "(+ (* 3 4) (- 10 5))", 17);

    // Comparisons
    check("lt-t", "(< 1 2)", 1);
    check("lt-f", "(< 2 1)", 0);
    check("gt-t", "(> 5 3)", 1);
    check("eq-t", "(= 7 7)", 1);
    check("eq-f", "(= 7 8)", 0);
    check("lte", "(<= 3 3)", 1);
    check("gte", "(>= 5 3)", 1);

    // If
    check("if-t", "(if (< 1 2) 42 99)", 42);
    check("if-f", "(if (> 1 2) 42 99)", 99);
    check("if-nested", "(if (< 1 2) (if (< 3 4) 10 20) 30)", 10);

    // Let
    check("let", "(let [x 10] x)", 10);
    check("let2", "(let [x 10 y 20] (+ x y))", 30);
    check("let-nested", "(let [x 10] (let [y 20] (+ x y)))", 30);
    check("let-shadow", "(let [x 10] (let [x 20] x))", 20);

    // Do
    check("do", "(do 1 2 3)", 3);

    // And / Or
    check("and-t", "(and 1 2 3)", 3);
    check("and-f", "(and 1 0 3)", 0);
    check("or-t", "(or 0 0 5)", 5);
    check("or-f", "(or 0 0 0)", 0);

    // Predicates
    check("not-t", "(not 0)", 1);
    check("not-f", "(not 5)", 0);
    check("inc", "(inc 41)", 42);
    check("dec", "(dec 43)", 42);
    check("zero?-t", "(zero? 0)", 1);
    check("zero?-f", "(zero? 5)", 0);
    check("pos?-t", "(pos? 5)", 1);
    check("neg?-t", "(neg? -3)", 1);

    // Defn + call
    check("defn", "(defn add [a b] (+ a b)) (add 3 4)", 7);
    check("defn2", "(defn sq [x] (* x x)) (sq 6)", 36);
    check("multi-fn",
          "(defn double [x] (* x 2)) (defn triple [x] (* x 3)) (+ (double 5) (triple 3))", 19);

    // Recursion
    check("fib10",
          "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)", 55);

    // TCO
    check("tco-fact",
          "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc)))) (fact 20 1)",
          2432902008176640000LL);

    // Loop/recur
    check("loop",
          "(loop [i 0 s 0] (if (>= i 10) s (recur (inc i) (+ s i))))", 45);

    // Def
    check("def", "(def x 42) x", 42);
    check("def-expr", "(def x (+ 10 20)) (+ x 5)", 35);

    // Complex
    check("euler1",
          "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) (if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s))))) (solve 1000)",
          233168);

    printf("  %d passed, %d failed\n", t_pass, t_fail);
}

// ============================================================================
// 8. Benchmarks + Main
// ============================================================================

int main(void) {
    // Init base layer
    g_perm = arena_create(1 << 20);
    g_req  = arena_create(1 << 20);
    g_temp = arena_create(1 << 16);
    intern_init();
    init_syms();

    // Allocate executable code buffer
    g_code.code = mmap(NULL, CODE_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC,
                       MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    g_code.cap = CODE_SIZE;
    g_code.pos = 0;

    if (g_code.code == MAP_FAILED) {
        printf("mmap failed\n");
        return 1;
    }

    run_tests();

    // Benchmark: fib(35)
    printf("\n=== benchmarks ===\n");
    {
        // Compile once
        const char *fib_src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 35)";
        u32 entry = compile_program(fib_src);
        JitFn0 fn = (JitFn0)(g_code.code + entry);

        // Verify
        i64 result = fn();
        printf("  fib(35) = %lld %s\n", (long long)result, result == 9227465 ? "OK" : "WRONG");

        // Benchmark
        u32 N = 5;
        u64 best = UINT64_MAX;
        for (u32 i = 0; i < N; i++) {
            u64 t0 = now_ns();
            i64 r = fn();
            u64 t1 = now_ns();
            SINK(r);
            u64 dt = t1 - t0;
            if (dt < best) best = dt;
        }
        printf("  fib(35) JIT: %.1f ms\n", best / 1e6);
    }

    // Benchmark: compilation speed
    {
        const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 35)";
        u32 N = 10000;
        u64 t0 = now_ns();
        for (u32 i = 0; i < N; i++) {
            compile_program(src);
        }
        u64 t1 = now_ns();
        printf("  compile fib: %.1f us  (source -> x86 bytes)\n", (t1 - t0) / (N * 1e3));
    }

    // Code size
    {
        compile_program("(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 35)");
        printf("  code size: %u bytes (fib + entry)\n", g_code.pos);
    }

    munmap(g_code.code, CODE_SIZE);
    return t_fail ? 1 : 0;
}
