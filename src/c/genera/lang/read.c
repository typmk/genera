/**
 * read.c — S-Expression Reader, Symbols, Classification
 *
 * Table-driven reader, interned symbols, form classification.
 * Shared by all backends (eval, C emitter, x86 JIT).
 * Depends on: platform/ (Cons, protocols, collections)
 */
#ifndef READ_C_INCLUDED
#define READ_C_INCLUDED

// ============================================================================
// 1. Reader — S-expression parser
// ============================================================================

// Character classification: 1 load + AND replaces 5-15 branches
#define CH_WS   1
#define CH_SYM  2
#define CH_DIG  4
#define CH_SC   8

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

#define IS_WS(c)        (CHAR_CLASS[(u8)(c)] & CH_WS)
#define IS_SYM(c)       (CHAR_CLASS[(u8)(c)] & (CH_SYM | CH_DIG))
#define IS_DIGIT(c)     (CHAR_CLASS[(u8)(c)] & CH_DIG)
#define IS_SYM_START(c) (CHAR_CLASS[(u8)(c)] & CH_SYM)

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

static Val read_number(Reader *r, bool neg) {
    i64 n = 0;
    while (r->pos < r->len && IS_DIGIT(r->src[r->pos]))
        n = n * 10 + (r->src[r->pos++] - '0');
    // Check for decimal point → f64
    if (r->pos < r->len && r->src[r->pos] == '.') {
        r->pos++;
        f64 d = (f64)n;
        f64 frac = 0.1;
        while (r->pos < r->len && IS_DIGIT(r->src[r->pos])) {
            d += (r->src[r->pos++] - '0') * frac;
            frac *= 0.1;
        }
        return val_f64(neg ? -d : d);
    }
    return val_int(neg ? -n : n);
}

static Val read_atom(Reader *r) {
    // Negative number
    if (r->src[r->pos] == '-' && r->pos + 1 < r->len && IS_DIGIT(r->src[r->pos + 1])) {
        r->pos++;
        return read_number(r, true);
    }
    // Positive number
    if (IS_DIGIT(r->src[r->pos])) {
        return read_number(r, false);
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
    // Symbol — intern, then StrId comparison (O(1))
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

static Val read_vector(Reader *r) {
    r->pos++; // skip '['
    skip_ws(r);
    CPVec *v = arena_push(&g_req, CPVec);
    *v = cpvec_empty();
    while (r->pos < r->len && r->src[r->pos] != ']') {
        *v = cpvec_append(*v, read_form(r));
        skip_ws(r);
    }
    if (r->pos < r->len) r->pos++; // skip ']'
    return val_pvec(v);
}

static Val read_map(Reader *r) {
    r->pos++; // skip '{'
    skip_ws(r);
    CPMap *m = arena_push(&g_req, CPMap);
    *m = cpmap_empty();
    while (r->pos < r->len && r->src[r->pos] != '}') {
        Val key = read_form(r);
        skip_ws(r);
        if (r->pos >= r->len || r->src[r->pos] == '}') break;
        Val val = read_form(r);
        if (val_is_kw(key))       *m = cpmap_put(*m, val_as_kw(key), val);
        else if (val_is_sym(key)) *m = cpmap_put(*m, val_as_sym(key), val);
        skip_ws(r);
    }
    if (r->pos < r->len) r->pos++; // skip '}'
    return val_pmap(m);
}

static Val read_keyword(Reader *r) {
    r->pos++; // skip ':'
    u32 ss = r->pos;
    while (r->pos < r->len && IS_SYM(r->src[r->pos])) r->pos++;
    if (r->pos == ss) return NIL;
    Str s = {(u8 *)(r->src + ss), r->pos - ss};
    return val_kw(str_intern(s));
}

static Val read_form(Reader *r) {
    skip_ws(r);
    if (r->pos >= r->len) return NIL;
    char c = r->src[r->pos];
    if (c == '(') return read_delimited(r, ')');
    if (c == '[') return read_vector(r);
    if (c == '{') return read_map(r);
    if (c == ':') return read_keyword(r);
    if (c == '\'') {
        r->pos++;
        static StrId s_quote_id;
        if (!s_quote_id) s_quote_id = str_intern(STR_LIT("quote"));
        return cons_new(val_sym(s_quote_id), cons_new(read_form(r), NIL));
    }
    return read_atom(r);
}

static Val read_str(const char *s) {
    Reader r = {s, 0, (u32)strlen(s)};
    return read_form(&r);
}

// ============================================================================
// 2. Symbols — interned once, compared as integers
// ============================================================================

static StrId S_DEF, S_DEFN, S_FN, S_IF, S_LET, S_DO, S_LOOP, S_RECUR, S_QUOTE;
static StrId S_AND, S_OR, S_COND, S_WHEN;
static StrId S_ADD, S_SUB, S_MUL, S_DIV, S_MOD;
static StrId S_EQ, S_LT, S_GT, S_LTE, S_GTE;
static StrId S_NOT, S_INC, S_DEC, S_PRINTLN;
static StrId S_ZEROQ, S_POSQ, S_NEGQ;
static StrId S_ELSE;

#define INTERN(s) str_intern(STR_LIT(s))

// Bitmap classification: O(1) "is this a special form?" / "is this a builtin?"
static u64 g_special_mask;
static u64 g_builtin_mask;

ALWAYS_INLINE bool is_special(StrId s) { return s < 64 && (g_special_mask & (1ULL << s)); }
ALWAYS_INLINE bool is_builtin(StrId s) { return s < 64 && (g_builtin_mask & (1ULL << s)); }

static void init_syms(void) {
    S_NIL = INTERN("nil"); S_TRUE = INTERN("true"); S_FALSE = INTERN("false");
    S_DEF = INTERN("def"); S_DEFN = INTERN("defn"); S_FN = INTERN("fn");
    S_IF = INTERN("if"); S_LET = INTERN("let"); S_DO = INTERN("do");
    S_LOOP = INTERN("loop"); S_RECUR = INTERN("recur"); S_QUOTE = INTERN("quote");
    S_AND = INTERN("and"); S_OR = INTERN("or"); S_COND = INTERN("cond"); S_WHEN = INTERN("when");
    S_ELSE = INTERN("else");
    S_ADD = INTERN("+"); S_SUB = INTERN("-"); S_MUL = INTERN("*");
    S_DIV = INTERN("/"); S_MOD = INTERN("mod");
    S_EQ = INTERN("="); S_LT = INTERN("<"); S_GT = INTERN(">");
    S_LTE = INTERN("<="); S_GTE = INTERN(">=");
    S_NOT = INTERN("not"); S_INC = INTERN("inc"); S_DEC = INTERN("dec");
    S_PRINTLN = INTERN("println");
    S_ZEROQ = INTERN("zero?"); S_POSQ = INTERN("pos?"); S_NEGQ = INTERN("neg?");

    g_special_mask = (1ULL<<S_DEF) | (1ULL<<S_DEFN) | (1ULL<<S_FN) |
        (1ULL<<S_IF) | (1ULL<<S_LET) | (1ULL<<S_DO) |
        (1ULL<<S_LOOP) | (1ULL<<S_RECUR) | (1ULL<<S_QUOTE) |
        (1ULL<<S_AND) | (1ULL<<S_OR) | (1ULL<<S_COND) | (1ULL<<S_WHEN);
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
                d->params = pvec_to_list(car(cdr(cdr(form))));
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

#endif // READ_C_INCLUDED
