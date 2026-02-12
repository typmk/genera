/**
 * x86.c — x86-64 Encoding Primitives
 *
 * Raw instruction encoding. No IR. No assembler. Direct bytes.
 * Depends on: base/ (types, memcpy)
 */
#ifndef X86_C_INCLUDED
#define X86_C_INCLUDED

// ============================================================================
// 1. Code Buffer + Registers
// ============================================================================

typedef struct { u8 *code; u32 pos, cap; } CodeBuf;

enum { RAX=0, RCX=1, RDX=2, RBX=3, RSP=4, RBP=5, RSI=6, RDI=7 };
static const u8 ARG_REGS[] = {RDI, RSI, RDX, RCX};
#define MODRM(mod, reg, rm) ((u8)(((mod)<<6)|((reg)<<3)|(rm)))

// ============================================================================
// 2. Byte Emitters
// ============================================================================

static void xb(CodeBuf *c, u8 v) { c->code[c->pos++] = v; }
static void x32(CodeBuf *c, u32 v) { memcpy(c->code+c->pos, &v, 4); c->pos += 4; }
static void x64(CodeBuf *c, u64 v) { memcpy(c->code+c->pos, &v, 8); c->pos += 8; }

// REX.W + opcode + ModRM (reg-reg)
static void xrr(CodeBuf *c, u8 op, u8 reg, u8 rm) {
    xb(c, 0x48); xb(c, op); xb(c, MODRM(3, reg, rm));
}

// ============================================================================
// 3. Immediate + Register-Register Ops
// ============================================================================

static void x_imm(CodeBuf *c, u8 reg, i64 v) {
    if (v >= 0 && v <= 0x7FFFFFFF) {
        xb(c, 0xB8 + reg); x32(c, (u32)v);
    } else if (v >= -0x80000000LL && v < 0) {
        xb(c, 0x48); xb(c, 0xC7); xb(c, MODRM(3, 0, reg)); x32(c, (u32)(i32)v);
    } else {
        xb(c, 0x48); xb(c, 0xB8 + reg); x64(c, (u64)v);
    }
}

static void x_mov(CodeBuf *c, u8 d, u8 s)  { xrr(c, 0x89, s, d); }
static void x_add(CodeBuf *c, u8 d, u8 s)  { xrr(c, 0x01, s, d); }
static void x_sub(CodeBuf *c, u8 d, u8 s)  { xrr(c, 0x29, s, d); }
static void x_cmp(CodeBuf *c, u8 a, u8 b)  { xrr(c, 0x39, b, a); }
static void x_test(CodeBuf *c, u8 a, u8 b) { xrr(c, 0x85, b, a); }

static void x_imul(CodeBuf *c, u8 d, u8 s) {
    xb(c, 0x48); xb(c, 0x0F); xb(c, 0xAF); xb(c, MODRM(3, d, s));
}
static void x_idiv(CodeBuf *c, u8 s) {
    xb(c, 0x48); xb(c, 0x99);
    xb(c, 0x48); xb(c, 0xF7); xb(c, MODRM(3, 7, s));
}
static void x_neg(CodeBuf *c, u8 r) {
    xb(c, 0x48); xb(c, 0xF7); xb(c, MODRM(3, 3, r));
}

// ============================================================================
// 4. Push / Pop / Load / Store
// ============================================================================

static void x_push(CodeBuf *c, u8 r) { xb(c, 0x50 + r); }
static void x_pop(CodeBuf *c, u8 r)  { xb(c, 0x58 + r); }

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

static void x_load_abs(CodeBuf *c, void *addr) {
    x_imm(c, RAX, (i64)addr);
    xb(c, 0x48); xb(c, 0x8B); xb(c, MODRM(0, RAX, RAX));
}
static void x_store_abs(CodeBuf *c, void *addr) {
    x_mov(c, RCX, RAX);
    x_imm(c, RAX, (i64)addr);
    xb(c, 0x48); xb(c, 0x89); xb(c, MODRM(0, RCX, RAX));
}

// ============================================================================
// 5. Jumps + Conditionals
// ============================================================================

static u32 x_jmp(CodeBuf *c)  { xb(c, 0xE9); u32 f=c->pos; x32(c,0); return f; }
static u32 x_call(CodeBuf *c) { xb(c, 0xE8); u32 f=c->pos; x32(c,0); return f; }
static void x_ret(CodeBuf *c) { (void)c; xb(c, 0xC3); }

#define CC_E  0x4
#define CC_NE 0x5
#define CC_L  0xC
#define CC_GE 0xD
#define CC_LE 0xE
#define CC_G  0xF

static u32 x_jcc(CodeBuf *c, u8 cc) {
    xb(c, 0x0F); xb(c, 0x80+cc); u32 f=c->pos; x32(c,0); return f;
}
static void x_patch(CodeBuf *c, u32 fixup) {
    i32 rel = (i32)(c->pos - (fixup + 4));
    memcpy(c->code + fixup, &rel, 4);
}
static void x_jmp_to(CodeBuf *c, u32 target) {
    xb(c, 0xE9);
    i32 rel = (i32)(target - (c->pos + 4));
    x32(c, (u32)rel);
}

static void x_setcc(CodeBuf *c, u8 cc) {
    xb(c, 0x0F); xb(c, 0x90+cc); xb(c, 0xC0);
    xb(c, 0x0F); xb(c, 0xB6); xb(c, 0xC0);
}

// ============================================================================
// 6. Prologue / Epilogue / Immediate Arithmetic
// ============================================================================

static void x_prologue(CodeBuf *c, u32 frame) {
    x_push(c, RBP);
    x_mov(c, RBP, RSP);
    if (frame) {
        xb(c, 0x48); xb(c, 0x81); xb(c, MODRM(3, 5, RSP)); x32(c, frame);
    }
}
static void x_epilogue(CodeBuf *c) {
    xb(c, 0xC9); xb(c, 0xC3);
}

static void x_add_i8(CodeBuf *c, u8 r, i8 v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 0, r)); xb(c, (u8)v);
}
static void x_sub_i8(CodeBuf *c, u8 r, i8 v) {
    xb(c, 0x48); xb(c, 0x83); xb(c, MODRM(3, 5, r)); xb(c, (u8)v);
}

// ============================================================================
// 7. Extended Registers (r8-r15)
// ============================================================================

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

#endif // X86_C_INCLUDED
