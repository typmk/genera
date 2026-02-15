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

#define STRID_MAX (1 << 14)  // 16384 — must cover intern table growth
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
// 3. Code Buffer + JIT Entry Points
//
// All compilation goes through the Clojure compiler (05-emit.clj + 06-compile.clj).
// The C side provides: x86 encoding, compiler state, function/global registry.
// ============================================================================

#define CODE_SIZE (1 << 20)
static CodeBuf g_code;
typedef i64 (*JitFn0)(void);

// Clojure JIT: parse → analyze → Clojure compile → exec
static i64 clj_jit_run(const char *source);

static i64 jit_run(const char *source) {
    return clj_jit_run(source);
}

// JIT from a pre-analyzed Gram (used by image load)
static i64 jit_run_gram(Gram *g) {
    // Install gram into world so Clojure compiler sees it
    g_world.gram = *g;
    g_world.version++;
    // Reset JIT state
    arena_reset(&g_req);
    g_code.pos = 0;
    g_fix_count = 0;
    g_global_count = 0;
    g_gen++;
    // Clojure compiler walks g_world.gram
    Val entry_val = engine_eval("(cg-compile-program)");
    u32 entry = (u32)val_as_int(entry_val);
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
