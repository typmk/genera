/**
 * read.c — Layer 1: Reader, Symbols, Classification
 *
 * S-expression parser, symbol interning, form classification.
 * Shared by all backends (C emitter, x86 JIT, etc.).
 * Depends on: base.c (arena, str, NaN box, cons, protocols)
 */
#ifndef READ_C_INCLUDED
#define READ_C_INCLUDED

// ============================================================================
// 1. Reader — S-expression parser
// ============================================================================

// Character classification table: 1 load + AND replaces 5-15 branches.
// Flags: 1=WS 2=SYM_START 4=DIGIT 8=SYM_CONT (2|4 is also SYM_CONT)
#define CH_WS   1
#define CH_SYM  2
#define CH_DIG  4
#define CH_SC   8  // symbol-continue-only (digit, #, :)

static const u8 CHAR_CLASS[256] = {
//  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, // 00-0F: WS at \t \n \r
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 10-1F
    1, 2, 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 1, 2, 2, 2, // 20-2F: WS=space,comma; SYM=!%&*+-./
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 2, 2, 2, 2, // 30-3F: DIGIT=0-9; SYM=<=>?
    0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 40-4F: SYM=A-O
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 2, // 50-5F: SYM=P-Z_
    0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 60-6F: SYM=a-o
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, // 70-7F: SYM=p-z
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

#define IS_WS(c)      (CHAR_CLASS[(u8)(c)] & CH_WS)
#define IS_SYM(c)     (CHAR_CLASS[(u8)(c)] & (CH_SYM | CH_DIG))
#define IS_DIGIT(c)   (CHAR_CLASS[(u8)(c)] & CH_DIG)
#define IS_SYM_START(c) (CHAR_CLASS[(u8)(c)] & CH_SYM)

// Forward-declare literal symbols used by read_atom (defined in init_syms)
static StrId S_NIL, S_TRUE, S_FALSE;

typedef struct { const char *src; u32 pos; u32 len; } Reader;

static void skip_ws(Reader *r) {
    while (r->pos < r->len) {
        char c = r->src[r->pos];
        if (IS_WS(c)) { r->pos++; continue; }
        if (c == ';') { while (r->pos < r->len && r->src[r->pos] != '\n') r->pos++; continue; }
        break;
    }
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
    // Reverse in-place
    Val result = NIL;
    while (val_is_cons(head)) {
        Val next = cdr(head);
        ((Cons *)val_as_cons(head))->cdr = result;
        result = head; head = next;
    }
    return result;
}

static Val read_atom(Reader *r) {
    // Negative number
    if (r->src[r->pos] == '-' && r->pos + 1 < r->len && IS_DIGIT(r->src[r->pos + 1])) {
        r->pos++;
        i64 n = 0;
        while (r->pos < r->len && IS_DIGIT(r->src[r->pos]))
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(-n);
    }
    // Positive number
    if (IS_DIGIT(r->src[r->pos])) {
        i64 n = 0;
        while (r->pos < r->len && IS_DIGIT(r->src[r->pos]))
            n = n * 10 + (r->src[r->pos++] - '0');
        return val_int(n);
    }
    // String literal
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
    // Symbol — intern first, then compare StrId (integer ==) instead of str_eq
    u32 ss = r->pos;
    while (r->pos < r->len && IS_SYM(r->src[r->pos])) r->pos++;
    if (r->pos == ss) { r->pos++; return NIL; }
    Str s = {(u8 *)(r->src + ss), r->pos - ss};
    StrId id = str_intern(s);
    if (id == S_NIL)   return NIL;
    if (id == S_TRUE)  return val_true();
    if (id == S_FALSE) return val_false();
    return val_sym(id);
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
// 2. Symbols
// ============================================================================

static StrId S_DEF, S_DEFN, S_IF, S_LET, S_DO, S_LOOP, S_RECUR;
static StrId S_AND, S_OR;
static StrId S_ADD, S_SUB, S_MUL, S_DIV, S_MOD;
static StrId S_EQ, S_LT, S_GT, S_LTE, S_GTE;
static StrId S_NOT, S_INC, S_DEC, S_PRINTLN;
static StrId S_ZEROQ, S_POSQ, S_NEGQ;

#define INTERN(s) str_intern(STR_LIT(s))

// Bitmap classification: O(1) "is this a special form?" / "is this a builtin?"
// StrIds are small sequential integers, so a u64 bitmask covers all of them.
static u64 g_special_mask;  // bit N set if StrId N is a special form
static u64 g_builtin_mask;  // bit N set if StrId N is a builtin operator

ALWAYS_INLINE bool is_special(StrId s) { return s < 64 && (g_special_mask & (1ULL << s)); }
ALWAYS_INLINE bool is_builtin(StrId s) { return s < 64 && (g_builtin_mask & (1ULL << s)); }

static void init_syms(void) {
    S_NIL = INTERN("nil"); S_TRUE = INTERN("true"); S_FALSE = INTERN("false");
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

    // Build classification bitmasks (one-time, after all syms interned)
    g_special_mask = (1ULL<<S_DEF) | (1ULL<<S_DEFN) | (1ULL<<S_IF) |
        (1ULL<<S_LET) | (1ULL<<S_DO) | (1ULL<<S_LOOP) | (1ULL<<S_RECUR) |
        (1ULL<<S_AND) | (1ULL<<S_OR);
    g_builtin_mask = (1ULL<<S_ADD) | (1ULL<<S_SUB) | (1ULL<<S_MUL) |
        (1ULL<<S_DIV) | (1ULL<<S_MOD) | (1ULL<<S_EQ) | (1ULL<<S_LT) |
        (1ULL<<S_GT) | (1ULL<<S_LTE) | (1ULL<<S_GTE) | (1ULL<<S_NOT) |
        (1ULL<<S_INC) | (1ULL<<S_DEC) | (1ULL<<S_PRINTLN) |
        (1ULL<<S_ZEROQ) | (1ULL<<S_POSQ) | (1ULL<<S_NEGQ);
}

// ============================================================================
// 3. Classification — separate defn/def/main forms
// ============================================================================

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

// ============================================================================
// 4. Recur detection — shared by TCO in both backends
// ============================================================================

// Checks if form contains (recur ...) outside of any (loop ...) — for defn TCO
static bool has_recur(Val form) {
    if (!val_is_cons(form)) return false;
    Val h = car(form);
    if (val_is_sym(h)) {
        if (val_as_sym(h) == S_RECUR) return true;
        if (val_as_sym(h) == S_LOOP) return false;  // loop handles its own recur
    }
    Val f = form;
    while (val_is_cons(f)) {
        if (has_recur(car(f))) return true;
        f = cdr(f);
    }
    return false;
}

// ============================================================================
// 5. Image — program as indexed data (EAV: Entity=StrId, Attribute=field)
// ============================================================================
//
// Intern once at boundary. Index forever. Never search.
// image[strid] → {calls, callers} — O(1) program queries.

#define IMAGE_MAX 4096

typedef struct {
    StrId calls[16];    // functions this calls
    u32   n_calls;
    StrId callers[16];  // functions that call this (reverse index)
    u32   n_callers;
} ImageMeta;

static ImageMeta g_image[IMAGE_MAX];

// Walk AST, extract user function calls (skip specials + builtins)
static void extract_calls(Val form, StrId self, ImageMeta *e) {
    if (!val_is_cons(form)) return;
    Val head = car(form);
    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);
        if (!is_special(sym) && !is_builtin(sym) && sym != self && e->n_calls < 16) {
            bool dup = false;
            for (u32 i = 0; i < e->n_calls; i++)
                if (e->calls[i] == sym) { dup = true; break; }
            if (!dup) e->calls[e->n_calls++] = sym;
        }
    }
    Val f = form;
    while (val_is_cons(f)) {
        if (val_is_cons(car(f))) extract_calls(car(f), self, e);
        f = cdr(f);
    }
}

// Build image from classified program. Call after classify().
static void image_build(void) {
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId name = g_defns[i].name;
        if (name >= IMAGE_MAX) continue;
        ImageMeta *e = &g_image[name];
        e->n_calls = 0; e->n_callers = 0;
        Val body = g_defns[i].body;
        while (val_is_cons(body)) {
            extract_calls(car(body), name, e);
            body = cdr(body);
        }
    }
    // Reverse index: for each caller's calls, register as callee's caller
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId caller = g_defns[i].name;
        if (caller >= IMAGE_MAX) continue;
        ImageMeta *ce = &g_image[caller];
        for (u32 j = 0; j < ce->n_calls; j++) {
            StrId callee = ce->calls[j];
            if (callee >= IMAGE_MAX) continue;
            ImageMeta *ee = &g_image[callee];
            if (ee->n_callers < 16) {
                bool dup = false;
                for (u32 k = 0; k < ee->n_callers; k++)
                    if (ee->callers[k] == caller) { dup = true; break; }
                if (!dup) ee->callers[ee->n_callers++] = caller;
            }
        }
    }
}

// O(1) image queries
ALWAYS_INLINE ImageMeta *image_get(StrId name) {
    return (name < IMAGE_MAX) ? &g_image[name] : NULL;
}

#endif // READ_C_INCLUDED
