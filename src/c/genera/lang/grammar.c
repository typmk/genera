/**
 * grammar.c — Universal Grammar, Symbols, Entity Bridge
 *
 * One engine, any language. Source text IS the tree.
 * 256-byte char class table + delimiter pairs = complete language spec.
 * Flat node array + bitmask indexes = O(1) classification queries.
 *
 * Also: interned symbols, form classification, entity bridge.
 * Shared by all backends (eval, JIT, C emitter).
 *
 * Depends on: platform/ (proto, coll)
 */
#ifndef GRAMMAR_C_INCLUDED
#define GRAMMAR_C_INCLUDED

// ============================================================================
// 1. Types
// ============================================================================

#define CL_WS    0x01
#define CL_ID    0x02
#define CL_DIG   0x04
#define CL_OP    0x08
#define CL_DELIM 0x10
#define CL_STR   0x20

enum {
    NK_ROOT=0, NK_LIST, NK_VEC, NK_MAP,
    NK_IDENT, NK_NUM, NK_STR_NODE, NK_OP, NK_KW, NK_OTHER,
    NK_COUNT
};
static const char *NK_NAME[] = {
    "root","list","vec","map","ident","num","str","op","kw","other"
};

typedef struct {
    u32 start;
    u16 len;
    u8  kind;
    u8  depth;
    u32 parent;
    u32 child;
    u32 next;
    u32 end;
} GNode;  // 28 bytes

typedef struct {
    const char *name;
    u8  cc[256];
    u8  open[4], close[4];
    u8  dkind[4];
    u8  nd;
    u8  lc1, lc2;
    u8  bc1, bc2, bc3, bc4;
    u8  sq, esc;
    bool negnum;
    bool kwcolon;
    bool opgrp;
} Lang;

// Semantic views — derived bitmasks over GNode tree
enum {
    V_DEF=0, V_REF, V_CALL,            // Pass 1: scope + binding
    V_TAIL, V_PURE, V_CONST, V_DEAD,   // Pass 2-3: type + flow
    V_INT, V_VEC, V_MAP, V_FN,         // Pass 2: type tags
    V_COUNT
};
static const char *V_NAME[] = {
    "def","ref","call","tail","pure","const","dead","int","vec","map","fn"
};

typedef struct {
    const char *src;
    u32 src_len;
    GNode *nodes;
    u32 n, cap;
    u32 mw;
    u64 *m[NK_COUNT];       // structural (kind-per-node)
    u64 *m_group;
    u64 *m_leaf;
    u64 *m_first;
    u64 *v[V_COUNT];        // semantic views (analysis passes)
    u32 *bind;              // bind[ref_id] = def_id (0 = unbound)
    u32 *scope;             // scope[id] = enclosing fn node (0 = top-level)
    i64 *const_val;         // const_val[id] for V_CONST+V_INT nodes
    bool analyzed;
} Gram;

// ============================================================================
// 2. Bitmask Operations — the query engine
// ============================================================================

#define BM_SET(m, i)   ((m)[(i)/64] |= (1ULL << ((i)%64)))
#define BM_GET(m, i)   (((m)[(i)/64] >> ((i)%64)) & 1)

static u32 bm_pop(const u64 *m, u32 nw) {
    u32 c = 0;
    for (u32 i = 0; i < nw; i++) c += POPCOUNT(m[i]);
    return c;
}

static void bm_and(u64 *dst, const u64 *a, const u64 *b, u32 nw) {
    for (u32 i = 0; i < nw; i++) dst[i] = a[i] & b[i];
}

static void bm_or(u64 *dst, const u64 *a, const u64 *b, u32 nw) {
    for (u32 i = 0; i < nw; i++) dst[i] = a[i] | b[i];
}

static bool bm_any_range(const u64 *m, u32 lo, u32 hi) {
    if (lo >= hi) return false;
    u32 wlo = lo / 64, blo = lo % 64;
    u32 whi = (hi - 1) / 64, bhi = (hi - 1) % 64;
    if (wlo == whi) {
        u64 mask = ((2ULL << bhi) - 1) & ~((1ULL << blo) - 1);
        return (m[wlo] & mask) != 0;
    }
    if (m[wlo] & ~((1ULL << blo) - 1)) return true;
    for (u32 w = wlo + 1; w < whi; w++) if (m[w]) return true;
    return (m[whi] & ((2ULL << bhi) - 1)) != 0;
}

static u32 bm_pop_range(const u64 *m, u32 lo, u32 hi) {
    if (lo >= hi) return 0;
    u32 c = 0;
    u32 wlo = lo / 64, blo = lo % 64;
    u32 whi = (hi - 1) / 64, bhi = (hi - 1) % 64;
    if (wlo == whi) {
        u64 mask = ((2ULL << bhi) - 1) & ~((1ULL << blo) - 1);
        return POPCOUNT(m[wlo] & mask);
    }
    c += POPCOUNT(m[wlo] & ~((1ULL << blo) - 1));
    for (u32 w = wlo + 1; w < whi; w++) c += POPCOUNT(m[w]);
    c += POPCOUNT(m[whi] & ((2ULL << bhi) - 1));
    return c;
}

static u32 bm_next(const u64 *m, u32 nw, u32 pos) {
    u32 w = pos / 64;
    if (w >= nw) return ~0u;
    u64 bits = m[w] & ~((1ULL << (pos % 64)) - 1);
    while (!bits && ++w < nw) bits = m[w];
    return bits ? w * 64 + (u32)CTZ(bits) : ~0u;
}

static u64 *bm_new(u32 nw) {
    u64 *m = (u64 *)arena_alloc(&g_perm, nw * 8, 8);
    memset(m, 0, nw * 8);
    return m;
}

// ============================================================================
// 3. Language Specs
// ============================================================================

static void lang_bf(Lang *l) {
    memset(l, 0, sizeof(*l));
    l->name = "brainfuck";
    for (int i = 0; i < 256; i++) l->cc[i] = CL_WS;
    const char *ops = "+-<>.,";
    for (const char *p = ops; *p; p++) l->cc[(u8)*p] = CL_OP;
    l->cc['['] = CL_DELIM; l->cc[']'] = CL_DELIM;
    l->nd = 1;
    l->open[0] = '['; l->close[0] = ']'; l->dkind[0] = NK_VEC;
}

static void lang_lisp(Lang *l) {
    memset(l, 0, sizeof(*l));
    l->name = "lisp";
    l->cc[' '] = l->cc['\t'] = l->cc['\n'] = l->cc['\r'] = l->cc[','] = CL_WS;
    for (int c = '0'; c <= '9'; c++) l->cc[c] = CL_DIG;
    for (int c = 'a'; c <= 'z'; c++) l->cc[c] = CL_ID;
    for (int c = 'A'; c <= 'Z'; c++) l->cc[c] = CL_ID;
    const char *sc = "!%&*+-./<=>?_";
    for (const char *p = sc; *p; p++) l->cc[(u8)*p] = CL_ID;
    l->cc['('] = l->cc[')'] = CL_DELIM;
    l->cc['['] = l->cc[']'] = CL_DELIM;
    l->cc['{'] = l->cc['}'] = CL_DELIM;
    l->cc['"'] = CL_STR;
    l->nd = 3;
    l->open[0]='('; l->close[0]=')'; l->dkind[0]=NK_LIST;
    l->open[1]='['; l->close[1]=']'; l->dkind[1]=NK_VEC;
    l->open[2]='{'; l->close[2]='}'; l->dkind[2]=NK_MAP;
    l->lc1 = ';'; l->sq = '"'; l->esc = '\\';
    l->negnum = true; l->kwcolon = true;
}

static void lang_c(Lang *l) {
    memset(l, 0, sizeof(*l));
    l->name = "c";
    l->cc[' '] = l->cc['\t'] = l->cc['\n'] = l->cc['\r'] = CL_WS;
    for (int c = '0'; c <= '9'; c++) l->cc[c] = CL_DIG;
    for (int c = 'a'; c <= 'z'; c++) l->cc[c] = CL_ID;
    for (int c = 'A'; c <= 'Z'; c++) l->cc[c] = CL_ID;
    l->cc['_'] = CL_ID;
    const char *ops = "+-*/<>=!&|^%~.;,@";
    for (const char *p = ops; *p; p++) l->cc[(u8)*p] = CL_OP;
    l->cc['('] = l->cc[')'] = CL_DELIM;
    l->cc['['] = l->cc[']'] = CL_DELIM;
    l->cc['{'] = l->cc['}'] = CL_DELIM;
    l->cc['"'] = l->cc['\''] = CL_STR;
    l->nd = 3;
    l->open[0]='('; l->close[0]=')'; l->dkind[0]=NK_LIST;
    l->open[1]='['; l->close[1]=']'; l->dkind[1]=NK_VEC;
    l->open[2]='{'; l->close[2]='}'; l->dkind[2]=NK_MAP;
    l->lc1 = '/'; l->lc2 = '/';
    l->bc1 = '/'; l->bc2 = '*'; l->bc3 = '*'; l->bc4 = '/';
    l->sq = '"'; l->esc = '\\'; l->opgrp = true;
}

// ============================================================================
// 4. Universal Parser — tokenize + structure in one pass
// ============================================================================

static Gram gram_new(u32 cap) {
    Gram g = {0};
    g.cap = cap;
    g.nodes = (GNode *)arena_alloc(&g_perm, cap * sizeof(GNode), 8);
    // Pre-allocate bitmask arrays (reused across gram_index calls)
    g.mw = (cap + 63) / 64;
    for (u32 k = 0; k < NK_COUNT; k++) g.m[k] = bm_new(g.mw);
    g.m_group = bm_new(g.mw);
    g.m_leaf  = bm_new(g.mw);
    g.m_first = bm_new(g.mw);
    g.bind  = (u32 *)arena_alloc(&g_perm, cap * sizeof(u32), 4);
    g.scope = (u32 *)arena_alloc(&g_perm, cap * sizeof(u32), 4);
    return g;
}

// Trace kind StrIds (initialized by grammar_init)
static StrId TK_NODE, TK_PARSE, TK_INDEX;

static u32 gn_add(Gram *g, u8 kind, u32 start, u16 len, u32 parent, u8 dep) {
    u32 id = g->n++;
    g->nodes[id] = (GNode){start, len, kind, dep, parent, 0, 0, id + 1};
    TAP(TK_NODE, id, kind, start);
    return id;
}

#define LINK(g, last, par, id) do { \
    if (last[par]) (g)->nodes[last[par]].next = id; \
    else (g)->nodes[par].child = id; \
    last[par] = id; \
} while(0)

static void gram_parse(Gram *g, const Lang *l, const char *src, u32 len) {
    g->src = src; g->src_len = len; g->n = 0;
    TAP1(TK_PARSE, len);

    gn_add(g, NK_ROOT, 0, (u16)(len > 65535 ? 65535 : len), 0, 0);

    u32 *last = (u32 *)arena_alloc(&g_temp, g->cap * sizeof(u32), 4);
    memset(last, 0, g->cap * sizeof(u32));

    u32 stk[256], sp = 0;
    u32 cur = 0;
    u8  dep = 0;
    u32 pos = 0;

    while (pos < len) {
        u8 c = (u8)src[pos];
        u8 cl = l->cc[c];

        if (cl & CL_WS) { pos++; continue; }

        // Line comment
        if (l->lc1 && c == l->lc1) {
            if (!l->lc2 || (pos + 1 < len && (u8)src[pos+1] == l->lc2)) {
                while (pos < len && src[pos] != '\n') pos++;
                continue;
            }
        }
        // Block comment
        if (l->bc1 && c == l->bc1 && pos + 1 < len && (u8)src[pos+1] == l->bc2) {
            pos += 2;
            while (pos + 1 < len && !((u8)src[pos]==l->bc3 && (u8)src[pos+1]==l->bc4)) pos++;
            if (pos + 1 < len) pos += 2;
            continue;
        }

        // String
        if (cl & CL_STR) {
            u8 q = c; u32 s = pos++;
            while (pos < len && (u8)src[pos] != q) {
                if ((u8)src[pos] == l->esc) pos++;
                pos++;
            }
            if (pos < len) pos++;
            u32 id = gn_add(g, NK_STR_NODE, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Open delimiter
        bool hit = false;
        for (u32 d = 0; d < l->nd; d++) {
            if (c == l->open[d]) {
                dep++;
                u32 id = gn_add(g, l->dkind[d], pos, 0, cur, dep);
                LINK(g, last, cur, id);
                stk[sp++] = cur; cur = id; last[cur] = 0;
                pos++; hit = true; break;
            }
        }
        if (hit) continue;

        // Close delimiter
        for (u32 d = 0; d < l->nd; d++) {
            if (c == l->close[d]) {
                g->nodes[cur].len = (u16)(pos + 1 - g->nodes[cur].start);
                g->nodes[cur].end = g->n;
                if (sp > 0) { cur = stk[--sp]; dep--; }
                pos++; hit = true; break;
            }
        }
        if (hit) continue;

        // Negative number
        if (l->negnum && c == '-' && pos + 1 < len &&
            (l->cc[(u8)src[pos+1]] & CL_DIG) &&
            (pos == 0 || (l->cc[(u8)src[pos-1]] & (CL_WS | CL_DELIM)))) {
            u32 s = pos++;
            while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            if (pos < len && src[pos] == '.' && pos+1 < len && (l->cc[(u8)src[pos+1]] & CL_DIG)) {
                pos++;
                while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            }
            u32 id = gn_add(g, NK_NUM, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Number
        if (cl & CL_DIG) {
            u32 s = pos;
            while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            if (pos < len && src[pos] == '.' && pos+1 < len && (l->cc[(u8)src[pos+1]] & CL_DIG)) {
                pos++;
                while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
            }
            u32 id = gn_add(g, NK_NUM, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Keyword :name
        if (l->kwcolon && c == ':' && pos + 1 < len && (l->cc[(u8)src[pos+1]] & CL_ID)) {
            u32 s = pos++;
            while (pos < len && (l->cc[(u8)src[pos]] & (CL_ID | CL_DIG))) pos++;
            u32 id = gn_add(g, NK_KW, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Identifier
        if (cl & CL_ID) {
            u32 s = pos;
            while (pos < len && (l->cc[(u8)src[pos]] & (CL_ID | CL_DIG))) pos++;
            u32 id = gn_add(g, NK_IDENT, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Operator
        if (cl & CL_OP) {
            u32 s = pos++;
            if (l->opgrp) while (pos < len && (l->cc[(u8)src[pos]] & CL_OP)) pos++;
            u32 id = gn_add(g, NK_OP, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        pos++;
    }

    while (sp > 0) { g->nodes[cur].end = g->n; cur = stk[--sp]; }
    g->nodes[0].end = g->n;
    arena_reset(&g_temp);
}

// ============================================================================
// 5. Index Builder — all queries become bit ops
// ============================================================================

static void gram_index(Gram *g) {
    TAP2(TK_INDEX, g->n, g->src_len);
    u32 mw = (g->n + 63) / 64;
    g->mw = mw;
    // Clear pre-allocated bitmasks
    for (u32 k = 0; k < NK_COUNT; k++) memset(g->m[k], 0, mw * 8);
    memset(g->m_group, 0, mw * 8);
    memset(g->m_leaf,  0, mw * 8);
    memset(g->m_first, 0, mw * 8);
    // Fill
    for (u32 i = 0; i < g->n; i++) {
        u8 k = g->nodes[i].kind;
        BM_SET(g->m[k], i);
        if (k >= NK_LIST && k <= NK_MAP) BM_SET(g->m_group, i);
        if (k >= NK_IDENT && k <= NK_KW) BM_SET(g->m_leaf, i);
        u32 p = g->nodes[i].parent;
        if (i > 0 && g->nodes[p].child == i) BM_SET(g->m_first, i);
    }
}

// ============================================================================
// 6. Semantic Views — analyze() allocates + runs passes
// ============================================================================

static void pass_scope(Gram *g);  // forward decl (defined after symbols)
static void pass_type(Gram *g);   // forward decl
static void pass_flow(Gram *g);   // forward decl

static void gram_analyze(Gram *g) {
    if (!g->mw) gram_index(g);
    u32 mw = g->mw;
    for (u32 i = 0; i < V_COUNT; i++) {
        if (!g->v[i]) g->v[i] = bm_new(mw);
        else memset(g->v[i], 0, mw * 8);
    }
    memset(g->bind,  0, g->n * sizeof(u32));
    memset(g->scope, 0, g->n * sizeof(u32));
    if (!g->const_val) g->const_val = (i64 *)arena_alloc(&g_perm, g->cap * sizeof(i64), 8);
    memset(g->const_val, 0, g->n * sizeof(i64));
    pass_scope(g);      // Pass 1: V_DEF, V_REF, V_CALL, bind[], scope[]
    pass_type(g);       // Pass 2: V_INT, V_VEC, V_MAP, V_FN, V_PURE, V_CONST + const_val
    pass_flow(g);       // Pass 3: V_TAIL, V_DEAD
    g->analyzed = true;
}

// View query: is node i in view v?
ALWAYS_INLINE bool view_is(Gram *g, u32 vid, u32 i) {
    return BM_GET(g->v[vid], i);
}
// View query: does subtree rooted at i contain any node in view v?
ALWAYS_INLINE bool view_has(Gram *g, u32 vid, u32 i) {
    return bm_any_range(g->v[vid], i + 1, g->nodes[i].end);
}
// View query: how many nodes in subtree i are in view v?
ALWAYS_INLINE u32 view_count(Gram *g, u32 vid, u32 i) {
    return bm_pop_range(g->v[vid], i + 1, g->nodes[i].end);
}

// ============================================================================
// 7. Query API
// ============================================================================

ALWAYS_INLINE u32   gn_child(Gram *g, u32 i)  { return g->nodes[i].child; }
ALWAYS_INLINE u32   gn_next(Gram *g, u32 i)   { return g->nodes[i].next; }
ALWAYS_INLINE u32   gn_parent(Gram *g, u32 i) { return g->nodes[i].parent; }
ALWAYS_INLINE u8    gn_kind(Gram *g, u32 i)   { return g->nodes[i].kind; }
ALWAYS_INLINE u32   gn_end(Gram *g, u32 i)    { return g->nodes[i].end; }
ALWAYS_INLINE Str   gn_text(Gram *g, u32 i)   { return (Str){(u8*)(g->src + g->nodes[i].start), g->nodes[i].len}; }
ALWAYS_INLINE StrId gn_intern(Gram *g, u32 i) { return str_intern(gn_text(g, i)); }

static u32 gn_count(Gram *g, u32 i) {
    u32 n = 0, c = g->nodes[i].child;
    while (c) { n++; c = g->nodes[c].next; }
    return n;
}
static u32 gn_nth(Gram *g, u32 i, u32 n) {
    u32 c = g->nodes[i].child;
    while (c && n--) c = g->nodes[c].next;
    return c;
}

ALWAYS_INLINE bool gn_has(Gram *g, const u64 *mask, u32 i) {
    return bm_any_range(mask, i + 1, g->nodes[i].end);
}
ALWAYS_INLINE u32 gn_sub_count(Gram *g, const u64 *mask, u32 i) {
    return bm_pop_range(mask, i + 1, g->nodes[i].end);
}

// Parse i64 from NK_NUM node (integer portion only)
static i64 gn_parse_int(Gram *g, u32 id) {
    Str t = gn_text(g, id);
    bool neg = false; u32 i = 0;
    if (t.len && t.data[0] == '-') { neg = true; i++; }
    i64 v = 0;
    while (i < t.len && t.data[i] != '.') v = v * 10 + (t.data[i++] - '0');
    return neg ? -v : v;
}

static void pr_text(Gram *g, u32 id) {
    Str t = gn_text(g, id);
    for (u32 i = 0; i < t.len && i < 60; i++) {
        u8 c = t.data[i];
        if (c == '\n') { pf("\\n"); } else if (c >= 32) buf_c(&g_print_buf, c);
    }
}

static void gram_print(Gram *g, u32 id) {
    GNode *n = &g->nodes[id];
    for (u8 i = 0; i < n->depth; i++) pf("  ");
    pf("%s", NK_NAME[n->kind]);
    if (n->kind != NK_ROOT && n->len > 0 && n->len < 50) {
        pf(" \""); pr_text(g, id); pf("\"");
    }
    pf("  [%u..%u)\n", id, n->end);
    u32 c = n->child;
    while (c) { gram_print(g, c); c = g->nodes[c].next; }
}

// ============================================================================
// 8. Symbols — interned once, compared as integers
// ============================================================================

static StrId S_NIL, S_TRUE, S_FALSE;
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
static u64 g_pure_bi_mask;   // pure builtins (no I/O)
static u64 g_int_ret_mask;   // builtins that return int when args are int

ALWAYS_INLINE bool is_special(StrId s) { return s < 64 && (g_special_mask & (1ULL << s)); }
ALWAYS_INLINE bool is_builtin(StrId s) { return s < 64 && (g_builtin_mask & (1ULL << s)); }
ALWAYS_INLINE bool is_pure_bi(StrId s) { return s < 64 && (g_pure_bi_mask & (1ULL << s)); }
ALWAYS_INLINE bool is_int_ret(StrId s) { return s < 64 && (g_int_ret_mask & (1ULL << s)); }

// Check if subtree contains recur (not inside nested loop)
static bool gn_has_recur(Gram *g, u32 id) {
    u32 end = g->nodes[id].end;
    for (u32 i = id + 1; i < end; i++) {
        if (g->nodes[i].kind != NK_LIST || !g->nodes[i].child) continue;
        u32 fc = g->nodes[i].child;
        if (g->nodes[fc].kind != NK_IDENT) continue;
        StrId sym = gn_intern(g, fc);
        if (sym == S_RECUR) return true;
        if (sym == S_LOOP) i = g->nodes[i].end - 1;
    }
    return false;
}

// ============================================================================
// 8b. Pass 1: Scope + Binding
//
// Single tree walk fills V_DEF, V_REF, V_CALL, bind[], scope[].
// Scope chain: flat array of {name, def_id} with save/restore for nesting.
// ============================================================================

typedef struct { StrId name; u32 def_id; } ScopeBind;
static ScopeBind g_sb[512];
static u32 g_sn;   // binding count
static u32 g_sf;   // current enclosing fn/defn node (0 = top-level)

static void p1_walk(Gram *g, u32 id);

static void p1_kids(Gram *g, u32 id) {
    u32 c = g->nodes[id].child;
    while (c) { p1_walk(g, c); c = g->nodes[c].next; }
}

static void p1_body(Gram *g, u32 start) {
    while (start) { p1_walk(g, start); start = g->nodes[start].next; }
}

// Mark param idents in [a b c] as V_DEF + push to scope chain
static void p1_params(Gram *g, u32 vec) {
    u32 p = g->nodes[vec].child;
    while (p) {
        g->scope[p] = g_sf;
        if (g->nodes[p].kind == NK_IDENT) {
            BM_SET(g->v[V_DEF], p);
            g_sb[g_sn++] = (ScopeBind){gn_intern(g, p), p};
        }
        p = g->nodes[p].next;
    }
}

// Mark alternating names in [x 1 y 2] as V_DEF, walk value exprs
static void p1_let(Gram *g, u32 vec) {
    u32 p = g->nodes[vec].child;
    bool is_name = true;
    while (p) {
        g->scope[p] = g_sf;
        if (is_name && g->nodes[p].kind == NK_IDENT) {
            BM_SET(g->v[V_DEF], p);
            g_sb[g_sn++] = (ScopeBind){gn_intern(g, p), p};
        } else if (!is_name) {
            p1_walk(g, p);
        }
        is_name = !is_name;
        p = g->nodes[p].next;
    }
}

static void p1_walk(Gram *g, u32 id) {
    g->scope[id] = g_sf;
    GNode *n = &g->nodes[id];

    if (n->kind == NK_LIST) {
        u32 fc = n->child;
        if (!fc) return;
        g->scope[fc] = g_sf;

        if (g->nodes[fc].kind == NK_IDENT) {
            StrId sym = gn_intern(g, fc);

            if (sym == S_DEFN) {
                // (defn name [params] body...) — name in outer scope
                u32 nm = g->nodes[fc].next;  if (!nm) return;
                g->scope[nm] = g_sf;
                BM_SET(g->v[V_DEF], nm);
                g_sb[g_sn++] = (ScopeBind){gn_intern(g, nm), nm};
                u32 pv = g->nodes[nm].next;  if (!pv) return;
                u32 sv = g_sn, of = g_sf;  g_sf = id;
                g->scope[pv] = g_sf;
                if (g->nodes[pv].kind == NK_VEC) p1_params(g, pv);
                p1_body(g, g->nodes[pv].next);
                g_sn = sv; g_sf = of;
                return;
            }
            if (sym == S_DEF) {
                // (def name value)
                u32 nm = g->nodes[fc].next;  if (!nm) return;
                g->scope[nm] = g_sf;
                BM_SET(g->v[V_DEF], nm);
                g_sb[g_sn++] = (ScopeBind){gn_intern(g, nm), nm};
                u32 val = g->nodes[nm].next;
                if (val) p1_walk(g, val);
                return;
            }
            if (sym == S_FN) {
                // (fn [params] body...)
                u32 pv = g->nodes[fc].next;  if (!pv) return;
                u32 sv = g_sn, of = g_sf;  g_sf = id;
                g->scope[pv] = g_sf;
                if (g->nodes[pv].kind == NK_VEC) p1_params(g, pv);
                p1_body(g, g->nodes[pv].next);
                g_sn = sv; g_sf = of;
                return;
            }
            if (sym == S_LET || sym == S_LOOP) {
                // (let [x 1 y 2] body...) / (loop [i 0] body...)
                u32 bv = g->nodes[fc].next;  if (!bv) return;
                u32 sv = g_sn;
                g->scope[bv] = g_sf;
                if (g->nodes[bv].kind == NK_VEC) p1_let(g, bv);
                p1_body(g, g->nodes[bv].next);
                g_sn = sv;
                return;
            }
            if (sym == S_QUOTE) return;  // quoted data — don't walk
            if (is_special(sym)) { p1_kids(g, id); return; }
        }

        // Regular function call
        BM_SET(g->v[V_CALL], id);
        p1_kids(g, id);
        return;
    }

    if (n->kind == NK_IDENT) {
        if (BM_GET(g->v[V_DEF], id)) return;
        StrId sym = gn_intern(g, id);
        if (sym == S_NIL || sym == S_TRUE || sym == S_FALSE) return;
        if (is_special(sym)) return;
        // Head position of call → not a value reference
        u32 par = n->parent;
        if (g->nodes[par].kind == NK_LIST && g->nodes[par].child == id) return;
        BM_SET(g->v[V_REF], id);
        for (i32 i = (i32)g_sn - 1; i >= 0; i--)
            if (g_sb[i].name == sym) { g->bind[id] = g_sb[i].def_id; break; }
        return;
    }

    p1_kids(g, id);
}

static void pass_scope(Gram *g) {
    g_sn = 0; g_sf = 0;
    p1_kids(g, 0);
}

// ============================================================================
// 8c. Pass 2: Type + Purity
//
// Backward scan (children before parents) fills V_INT, V_VEC, V_MAP, V_FN,
// V_PURE, V_CONST. Pure builtin calls with const args → V_CONST.
// Int-returning builtins with int args → V_INT.
// ============================================================================

static i64 cv_eval(Gram *g, StrId op, u32 first_arg) {
    i64 *cv = g->const_val;
    u32 a = first_arg;
    if (!a) return (op == S_MUL) ? 1 : 0;
    i64 r = cv[a];
    u32 n = 1;
    a = g->nodes[a].next;
    while (a) { i64 v = cv[a]; n++; a = g->nodes[a].next;
        if      (op == S_ADD) r += v;
        else if (op == S_SUB) r -= v;
        else if (op == S_MUL) r *= v;
        else if (op == S_DIV) { if (v) r /= v; }
        else if (op == S_MOD) { if (v) r %= v; }
        else if (op == S_EQ)  r = (r == v);
        else if (op == S_LT)  r = (r < v);
        else if (op == S_GT)  r = (r > v);
        else if (op == S_LTE) r = (r <= v);
        else if (op == S_GTE) r = (r >= v);
    }
    if (n == 1 && op == S_SUB) r = -r;
    if (op == S_INC) r += 1;
    if (op == S_DEC) r -= 1;
    if (op == S_NOT) r = !r;
    if (op == S_ZEROQ) r = (r == 0);
    if (op == S_POSQ) r = (r > 0);
    if (op == S_NEGQ) r = (r < 0);
    return r;
}

static void pass_type(Gram *g) {
    i64 *cv = g->const_val;
    for (i32 i = (i32)g->n - 1; i >= 0; i--) {
        GNode *n = &g->nodes[i];

        // Leaf types
        if (n->kind == NK_NUM) {
            BM_SET(g->v[V_INT], i); BM_SET(g->v[V_CONST], i);
            cv[i] = gn_parse_int(g, i);
            continue;
        }
        if (n->kind == NK_STR_NODE) { BM_SET(g->v[V_CONST], i); continue; }
        if (n->kind == NK_VEC)      { BM_SET(g->v[V_VEC], i); continue; }
        if (n->kind == NK_MAP)      { BM_SET(g->v[V_MAP], i); continue; }

        // Constants: nil, true, false — treated as int (0/1) for const folding
        if (n->kind == NK_IDENT && !BM_GET(g->v[V_DEF], i) && !BM_GET(g->v[V_REF], i)) {
            StrId sym = gn_intern(g, i);
            if (sym == S_TRUE)  { BM_SET(g->v[V_INT], i); BM_SET(g->v[V_CONST], i); cv[i] = 1; continue; }
            if (sym == S_FALSE) { BM_SET(g->v[V_INT], i); BM_SET(g->v[V_CONST], i); cv[i] = 0; continue; }
            if (sym == S_NIL)   { BM_SET(g->v[V_INT], i); BM_SET(g->v[V_CONST], i); cv[i] = 0; continue; }
        }

        // fn form → V_FN
        if (n->kind == NK_LIST && n->child &&
            g->nodes[n->child].kind == NK_IDENT && gn_intern(g, n->child) == S_FN)
            { BM_SET(g->v[V_FN], i); continue; }

        // Calls to pure builtins → V_PURE, check const + int propagation
        if (BM_GET(g->v[V_CALL], i) && n->kind == NK_LIST && n->child) {
            u32 fc = n->child;
            if (g->nodes[fc].kind == NK_IDENT) {
                StrId sym = gn_intern(g, fc);
                if (is_pure_bi(sym)) {
                    BM_SET(g->v[V_PURE], i);
                    bool all_const = true;
                    u32 arg = g->nodes[fc].next;
                    while (arg) {
                        if (!BM_GET(g->v[V_CONST], arg)) { all_const = false; break; }
                        arg = g->nodes[arg].next;
                    }
                    if (all_const) BM_SET(g->v[V_CONST], i);
                    if (is_int_ret(sym)) {
                        bool all_int = true;
                        arg = g->nodes[fc].next;
                        while (arg) {
                            if (!BM_GET(g->v[V_INT], arg)) { all_int = false; break; }
                            arg = g->nodes[arg].next;
                        }
                        if (all_int) {
                            BM_SET(g->v[V_INT], i);
                            if (all_const) cv[i] = cv_eval(g, sym, g->nodes[fc].next);
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// 8d. Pass 3: Flow + Dead Code
//
// Marks V_TAIL (tail position) and V_DEAD (unreachable branches).
// pass_flow scans for defn/fn/loop, p3_tail recursively propagates.
// ============================================================================

static void p3_tail(Gram *g, u32 id) {
    BM_SET(g->v[V_TAIL], id);
    GNode *n = &g->nodes[id];
    if (n->kind != NK_LIST || !n->child) return;

    u32 fc = n->child;
    if (g->nodes[fc].kind != NK_IDENT) return;
    StrId sym = gn_intern(g, fc);

    if (sym == S_IF) {
        u32 test = g->nodes[fc].next;  if (!test) return;
        u32 then = g->nodes[test].next; if (!then) return;
        p3_tail(g, then);
        u32 els = g->nodes[then].next;
        if (els) p3_tail(g, els);
        // Dead code: constant test
        if (g->nodes[test].kind == NK_IDENT) {
            StrId ts = gn_intern(g, test);
            if (ts == S_FALSE || ts == S_NIL) BM_SET(g->v[V_DEAD], then);
            else if (ts == S_TRUE)  { if (els) BM_SET(g->v[V_DEAD], els); }
        }
        return;
    }
    if (sym == S_DO) {
        u32 c = g->nodes[fc].next, last = 0;
        while (c) { last = c; c = g->nodes[c].next; }
        if (last) p3_tail(g, last);
        return;
    }
    if (sym == S_LET || sym == S_LOOP) {
        u32 bv = g->nodes[fc].next; if (!bv) return;
        u32 c = g->nodes[bv].next, last = 0;
        while (c) { last = c; c = g->nodes[c].next; }
        if (last) p3_tail(g, last);
        return;
    }
    if (sym == S_COND) {
        u32 c = g->nodes[fc].next;
        bool is_test = true;
        while (c) { if (!is_test) p3_tail(g, c); is_test = !is_test; c = g->nodes[c].next; }
        return;
    }
    if (sym == S_WHEN || sym == S_AND || sym == S_OR) {
        u32 c = g->nodes[fc].next, last = 0;
        while (c) { last = c; c = g->nodes[c].next; }
        if (last) p3_tail(g, last);
        return;
    }
}

static void pass_flow(Gram *g) {
    for (u32 i = 0; i < g->n; i++) {
        GNode *n = &g->nodes[i];
        if (n->kind != NK_LIST || !n->child) continue;
        u32 fc = n->child;
        if (g->nodes[fc].kind != NK_IDENT) continue;
        StrId sym = gn_intern(g, fc);

        u32 body_start = 0;
        if (sym == S_DEFN) {
            u32 nm = g->nodes[fc].next; if (!nm) continue;
            u32 pv = g->nodes[nm].next; if (!pv) continue;
            body_start = g->nodes[pv].next;
        } else if (sym == S_FN) {
            u32 pv = g->nodes[fc].next; if (!pv) continue;
            body_start = g->nodes[pv].next;
        } else if (sym == S_LOOP) {
            u32 bv = g->nodes[fc].next; if (!bv) continue;
            body_start = g->nodes[bv].next;
        } else continue;

        u32 last = 0, e = body_start;
        while (e) { last = e; e = g->nodes[e].next; }
        if (last) p3_tail(g, last);
    }

    // Dead code: scan ALL if/when nodes with constant tests (not just tail)
    for (u32 i = 0; i < g->n; i++) {
        GNode *n = &g->nodes[i];
        if (n->kind != NK_LIST || !n->child) continue;
        u32 fc = n->child;
        if (g->nodes[fc].kind != NK_IDENT) continue;
        StrId sym = gn_intern(g, fc);
        if (sym == S_IF) {
            u32 test = g->nodes[fc].next; if (!test) continue;
            u32 then = g->nodes[test].next; if (!then) continue;
            u32 els  = g->nodes[then].next;
            if (BM_GET(g->v[V_CONST], test) && BM_GET(g->v[V_INT], test)) {
                i64 tv = g->const_val[test];
                if (tv == 0) BM_SET(g->v[V_DEAD], then);
                else if (els) BM_SET(g->v[V_DEAD], els);
            }
        } else if (sym == S_WHEN) {
            u32 test = g->nodes[fc].next; if (!test) continue;
            if (BM_GET(g->v[V_CONST], test) && BM_GET(g->v[V_INT], test) && g->const_val[test] == 0) {
                u32 b = g->nodes[test].next;
                while (b) { BM_SET(g->v[V_DEAD], b); b = g->nodes[b].next; }
            }
        }
    }
}

// ============================================================================
// 9. Classification — separate defn/def/main forms
// ============================================================================

typedef struct { StrId name; Val params; Val body; u32 n_params; } DefnInfo;
typedef struct { StrId name; Val value; } DefInfo;

static DefnInfo g_defns[256]; static u32 g_defn_count;
static DefInfo  g_defs[256];  static u32 g_def_count;
static Val      g_mains[1024]; static u32 g_main_count;

// Recur detection — shared by TCO in both backends
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
// 10. Entity Bridge — GNode entities → Val cons lists
// ============================================================================
//
// Connects the universal grammar to emitters + classify.
// GNode entity tree → Val cons lists → compile → execute.

static Val entity_to_val(Gram *g, u32 id) {
    GNode *n = &g->nodes[id];
    switch (n->kind) {
        case NK_LIST: {
            Val head = NIL;
            u32 c = n->child;
            while (c) { head = cons_new(entity_to_val(g, c), head); c = g->nodes[c].next; }
            Val result = NIL;
            while (val_is_cons(head)) {
                Val next = cdr(head);
                ((Cons *)val_as_cons(head))->cdr = result;
                result = head; head = next;
            }
            return result;
        }
        case NK_VEC: {
            CPVec *v = arena_push(&g_req, CPVec);
            *v = cpvec_empty();
            u32 c = n->child;
            while (c) { *v = cpvec_append(*v, entity_to_val(g, c)); c = g->nodes[c].next; }
            return val_pvec(v);
        }
        case NK_MAP: {
            CPMap *m = arena_push(&g_req, CPMap);
            *m = cpmap_empty();
            u32 c = n->child;
            while (c) {
                Val key = entity_to_val(g, c);
                c = g->nodes[c].next;
                if (!c) break;
                Val val = entity_to_val(g, c);
                if (val_is_kw(key))       *m = cpmap_put(*m, val_as_kw(key), val);
                else if (val_is_sym(key)) *m = cpmap_put(*m, val_as_sym(key), val);
                c = g->nodes[c].next;
            }
            return val_pmap(m);
        }
        case NK_IDENT: {
            Str text = gn_text(g, id);
            StrId sid = str_intern(text);
            if (sid == S_NIL)   return NIL;
            if (sid == S_TRUE)  return val_true();
            if (sid == S_FALSE) return val_false();
            return val_sym(sid);
        }
        case NK_NUM: {
            Str text = gn_text(g, id);
            bool neg = false; u32 i = 0;
            if (text.len && text.data[0] == '-') { neg = true; i++; }
            // Check for decimal point → f64
            bool has_dot = false;
            for (u32 j = i; j < text.len; j++) if (text.data[j] == '.') { has_dot = true; break; }
            if (has_dot) {
                f64 d = 0;
                while (i < text.len && text.data[i] != '.') d = d * 10 + (text.data[i++] - '0');
                if (i < text.len) i++; // skip '.'
                f64 frac = 0.1;
                while (i < text.len) { d += (text.data[i++] - '0') * frac; frac *= 0.1; }
                return val_f64(neg ? -d : d);
            }
            i64 v = 0;
            while (i < text.len) v = v * 10 + (text.data[i++] - '0');
            return val_int(neg ? -v : v);
        }
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
        default:
            return NIL;
    }
}

// Convert all top-level grammar forms to classified Vals.
// Populates g_defns/g_defs/g_mains (same as classify() but from entities).
static void entity_classify(Gram *g) {
    g_defn_count = g_def_count = g_main_count = 0;
    u32 c = g->nodes[0].child;
    while (c) {
        Val form = entity_to_val(g, c);
        if (val_is_cons(form) && val_is_sym(car(form))) {
            StrId sym = val_as_sym(car(form));
            if (sym == S_DEFN) {
                DefnInfo *d = &g_defns[g_defn_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->params = pvec_to_list(car(cdr(cdr(form))));
                d->body = cdr(cdr(cdr(form)));
                d->n_params = list_len(d->params);
                c = g->nodes[c].next; continue;
            }
            if (sym == S_DEF) {
                DefInfo *d = &g_defs[g_def_count++];
                d->name = val_as_sym(car(cdr(form)));
                d->value = car(cdr(cdr(form)));
                c = g->nodes[c].next; continue;
            }
        }
        g_mains[g_main_count++] = form;
        c = g->nodes[c].next;
    }
}

// ============================================================================
// 11. Convenience — gram_read, gram_classify
// ============================================================================

static Gram g_gram_scratch;

static void gram_ensure_scratch(void) {
    if (!g_gram_scratch.cap) g_gram_scratch = gram_new(4096);
}

// Parse single expression → Val (replaces read_str)
static Val gram_read(const char *source) {
    gram_ensure_scratch();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    gram_parse(&g_gram_scratch, &lisp, source, (u32)strlen(source));
    u32 first = g_gram_scratch.nodes[0].child;
    return first ? entity_to_val(&g_gram_scratch, first) : NIL;
}

// Parse + classify all top-level forms (replaces classify)
static void gram_classify(const char *source) {
    gram_ensure_scratch();
    static Lang lisp; static bool inited;
    if (!inited) { lang_lisp(&lisp); inited = true; }
    gram_parse(&g_gram_scratch, &lisp, source, (u32)strlen(source));
    gram_index(&g_gram_scratch);
    entity_classify(&g_gram_scratch);
}

// ============================================================================
// 12. Init
// ============================================================================

static void grammar_init(void) {
    // Trace kind StrIds
    TK_NODE  = str_intern(STR_LIT("node"));
    TK_PARSE = str_intern(STR_LIT("parse"));
    TK_INDEX = str_intern(STR_LIT("index"));

    // Symbols — interned once, compared as integers
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
    g_pure_bi_mask = g_builtin_mask & ~(1ULL << S_PRINTLN);
    g_int_ret_mask = (1ULL<<S_ADD) | (1ULL<<S_SUB) | (1ULL<<S_MUL) |
        (1ULL<<S_DIV) | (1ULL<<S_MOD) | (1ULL<<S_INC) | (1ULL<<S_DEC) |
        (1ULL<<S_EQ) | (1ULL<<S_LT) | (1ULL<<S_GT) | (1ULL<<S_LTE) | (1ULL<<S_GTE) |
        (1ULL<<S_NOT) | (1ULL<<S_ZEROQ) | (1ULL<<S_POSQ) | (1ULL<<S_NEGQ);
}

#endif // GRAMMAR_C_INCLUDED
