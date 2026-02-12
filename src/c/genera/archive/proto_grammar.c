/**
 * proto_grammar.c — Universal Grammar
 *
 * One engine, any language. Source text IS the tree.
 * 256-byte char class table + delimiter pairs = complete language spec.
 * Flat node array + bitmask indexes = O(1) classification queries.
 * Queries are bit operations: AND, OR, POPCOUNT, range scan.
 * No AST allocation. No tree walking for classification. Just bits.
 *
 * Build: gcc -O3 -march=native -nostdlib -static -o proto_grammar test/proto_grammar.c
 */

#include "../src/c/sys.c"
#include "../src/c/base.c"

// ============================================================================
// 1. Types
// ============================================================================

// Character class flags (per-language, 256-byte table)
#define CL_WS    0x01
#define CL_ID    0x02    // identifier start
#define CL_DIG   0x04    // digit
#define CL_OP    0x08    // operator
#define CL_DELIM 0x10    // delimiter
#define CL_STR   0x20    // string quote

// Node kinds
enum {
    NK_ROOT=0, NK_LIST, NK_VEC, NK_MAP,
    NK_IDENT, NK_NUM, NK_STR, NK_OP, NK_KW, NK_OTHER,
    NK_COUNT
};
static const char *NK_NAME[] = {
    "root","list","vec","map","ident","num","str","op","kw","other"
};

// GNode: virtual AST node (flat array, pre-order)
typedef struct {
    u32 start;      // source byte offset
    u16 len;        // source byte length
    u8  kind;       // NK_*
    u8  depth;      // nesting depth
    u32 parent;     // parent node index
    u32 child;      // first child (0 = leaf)
    u32 next;       // next sibling (0 = last)
    u32 end;        // pre-order subtree end (exclusive) — range queries
} GNode;            // 28 bytes

// Language spec — the ONLY per-language config (~280 bytes)
typedef struct {
    const char *name;
    u8  cc[256];            // char → flags
    u8  open[4], close[4];  // delimiter pairs
    u8  dkind[4];           // NK_* for each pair
    u8  nd;                 // num delimiter pairs
    u8  lc1, lc2;           // line comment (lc1=char, lc2=2nd or 0=any)
    u8  bc1, bc2, bc3, bc4; // block comment open[2]+close[2]
    u8  sq, esc;            // string quote, escape char
    bool negnum;            // -N is negative number
    bool kwcolon;           // :name is keyword
    bool opgrp;             // group adjacent op chars
} Lang;

// Grammar: flat nodes + bitmask indexes
typedef struct {
    const char *src;
    u32 src_len;
    GNode *nodes;
    u32 n, cap;
    u32 mw;                 // mask words = (n+63)/64
    u64 *m[NK_COUNT];       // one bitmask per node kind
    u64 *m_group;           // list|vec|map
    u64 *m_leaf;            // ident|num|str|op|kw
    u64 *m_first;           // first child of parent
} Gram;

// ============================================================================
// 2. Bitmask Operations — the query engine
// ============================================================================
//
// Every classification = a bitmask (1 bit per node).
// Every query = bit ops on bitmasks: AND, OR, POPCOUNT, range scan.
// 64 nodes per u64. 256 per AVX2 op. No tree walking.

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

// Any set bit in range [lo, hi)?
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

// Count set bits in range [lo, hi)
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

// Next set bit at or after pos
static u32 bm_next(const u64 *m, u32 nw, u32 pos) {
    u32 w = pos / 64;
    if (w >= nw) return ~0u;
    u64 bits = m[w] & ~((1ULL << (pos % 64)) - 1);
    while (!bits && ++w < nw) bits = m[w];
    return bits ? w * 64 + (u32)CTZ(bits) : ~0u;
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
    const char *sc = "!%&*+-./<=>?_";  // matches read.c
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
    return g;
}

// Trace kind StrIds (interned once, used in TAP)
static StrId TK_NODE, TK_PARSE, TK_INDEX, TK_QUERY;

static u32 gn_add(Gram *g, u8 kind, u32 start, u16 len, u32 parent, u8 dep) {
    u32 id = g->n++;
    g->nodes[id] = (GNode){start, len, kind, dep, parent, 0, 0, id + 1};
    TAP(TK_NODE, id, kind, start);
    return id;
}

// Link child to parent (O(1) via last_child tracking)
#define LINK(g, last, par, id) do { \
    if (last[par]) (g)->nodes[last[par]].next = id; \
    else (g)->nodes[par].child = id; \
    last[par] = id; \
} while(0)

static void gram_parse(Gram *g, const Lang *l, const char *src, u32 len) {
    g->src = src; g->src_len = len; g->n = 0;
    TAP1(TK_PARSE, len);

    gn_add(g, NK_ROOT, 0, (u16)(len > 65535 ? 65535 : len), 0, 0);

    // last_child tracking: O(1) sibling linking
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
            u32 id = gn_add(g, NK_STR, s, (u16)(pos - s), cur, dep + 1);
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
            u32 id = gn_add(g, NK_NUM, s, (u16)(pos - s), cur, dep + 1);
            LINK(g, last, cur, id);
            continue;
        }

        // Number
        if (cl & CL_DIG) {
            u32 s = pos;
            while (pos < len && (l->cc[(u8)src[pos]] & CL_DIG)) pos++;
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

static u64 *bm_new(u32 nw) {
    u64 *m = (u64 *)arena_alloc(&g_perm, nw * 8, 8);
    memset(m, 0, nw * 8);
    return m;
}

static void gram_index(Gram *g) {
    TAP2(TK_INDEX, g->n, g->src_len);
    g->mw = (g->n + 63) / 64;
    for (u32 k = 0; k < NK_COUNT; k++) g->m[k] = bm_new(g->mw);
    g->m_group = bm_new(g->mw);
    g->m_leaf  = bm_new(g->mw);
    g->m_first = bm_new(g->mw);

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
// 6. Query API — structural + bitmask
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

// Subtree queries: does subtree contain matching node? How many?
ALWAYS_INLINE bool gn_has(Gram *g, const u64 *mask, u32 i) {
    return bm_any_range(mask, i + 1, g->nodes[i].end);
}
ALWAYS_INLINE u32 gn_sub_count(Gram *g, const u64 *mask, u32 i) {
    return bm_pop_range(mask, i + 1, g->nodes[i].end);
}

// Print text of node (for debugging)
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
// 7. Tests
// ============================================================================

static int t_pass, t_fail;
static void check(const char *name, bool ok) {
    if (ok) t_pass++; else { pf("  FAIL: %s\n", name); t_fail++; }
}

static void test_bf(void) {
    pf("\n--- brainfuck ---\n");
    Lang l; lang_bf(&l);
    const char *src = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    check("bf: parsed", g.n > 1);
    check("bf: root", gn_kind(&g, 0) == NK_ROOT);

    u32 ops = bm_pop(g.m[NK_OP], g.mw);
    u32 vecs = bm_pop(g.m[NK_VEC], g.mw);
    check("bf: ops > 10", ops > 10);
    check("bf: nested loops", vecs >= 3);
    pf("  %u nodes, %u ops, %u loops\n", g.n, ops, vecs);
}

static void test_lisp(void) {
    pf("\n--- lisp ---\n");
    Lang l; lang_lisp(&l);
    const char *src = "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    check("lisp: parsed", g.n > 1);

    // Root → one child (defn form)
    u32 defn = gn_child(&g, 0);
    check("lisp: defn is list", gn_kind(&g, defn) == NK_LIST);

    // First child = "defn"
    u32 head = gn_child(&g, defn);
    check("lisp: head ident", gn_kind(&g, head) == NK_IDENT);
    check("lisp: head=defn", str_eq(gn_text(&g, head), STR_LIT("defn")));

    // Second child = "fib"
    u32 fname = gn_next(&g, head);
    check("lisp: name=fib", str_eq(gn_text(&g, fname), STR_LIT("fib")));

    // Third child = [n]
    u32 params = gn_next(&g, fname);
    check("lisp: params vec", gn_kind(&g, params) == NK_VEC);
    check("lisp: param n", str_eq(gn_text(&g, gn_child(&g, params)), STR_LIT("n")));

    // Count via bitmasks
    u32 lists = bm_pop(g.m[NK_LIST], g.mw);
    u32 idents = bm_pop(g.m[NK_IDENT], g.mw);
    u32 nums = bm_pop(g.m[NK_NUM], g.mw);
    pf("  %u nodes: %u lists, %u idents, %u nums\n", g.n, lists, idents, nums);

    // Negative number
    const char *s2 = "(+ -7 3)";
    Gram g2 = gram_new(64);
    gram_parse(&g2, &l, s2, strlen(s2));
    u32 neg = gn_nth(&g2, gn_child(&g2, 0), 1);
    check("lisp: -7 is num", gn_kind(&g2, neg) == NK_NUM);
    check("lisp: -7 text", str_eq(gn_text(&g2, neg), STR_LIT("-7")));

    // Comment skipping
    const char *s3 = "; comment\n(+ 1 2)";
    Gram g3 = gram_new(64);
    gram_parse(&g3, &l, s3, strlen(s3));
    u32 add = gn_child(&g3, 0);
    check("lisp: skip comment", gn_kind(&g3, add) == NK_LIST);
    check("lisp: 3 children", gn_count(&g3, add) == 3);

    // Keyword
    const char *s4 = "{:name \"fib\" :arity 1}";
    Gram g4 = gram_new(64);
    gram_parse(&g4, &l, s4, strlen(s4));
    gram_index(&g4);
    u32 kws = bm_pop(g4.m[NK_KW], g4.mw);
    check("lisp: keywords", kws == 2);
}

static void test_c(void) {
    pf("\n--- c ---\n");
    Lang l; lang_c(&l);
    const char *src = "int fib(int n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }";
    Gram g = gram_new(1024);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    check("c: parsed", g.n > 5);
    u32 idents = bm_pop(g.m[NK_IDENT], g.mw);
    u32 nums = bm_pop(g.m[NK_NUM], g.mw);
    u32 groups = bm_pop(g.m_group, g.mw);
    check("c: idents", idents > 0);
    check("c: nums", nums > 0);
    check("c: groups", groups > 0);
    pf("  %u nodes: %u idents, %u nums, %u groups\n", g.n, idents, nums, groups);

    // Block comment
    const char *s2 = "int x = 1; /* comment */ int y = 2;";
    Gram g2 = gram_new(64);
    gram_parse(&g2, &l, s2, strlen(s2));
    gram_index(&g2);
    u32 ids2 = bm_pop(g2.m[NK_IDENT], g2.mw);
    check("c: block comment", ids2 == 4);  // int, x, int, y

    // Line comment
    const char *s3 = "int x; // skip\nint y;";
    Gram g3 = gram_new(64);
    gram_parse(&g3, &l, s3, strlen(s3));
    gram_index(&g3);
    u32 ids3 = bm_pop(g3.m[NK_IDENT], g3.mw);
    check("c: line comment", ids3 == 4);
}

// ============================================================================
// 8. Query Demo — Clojure program analysis via bits
// ============================================================================

static void test_query(void) {
    pf("\n--- query: Clojure program analysis (bits, not trees) ---\n");
    Lang l; lang_lisp(&l);

    const char *src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(defn add [a b] (+ a b))\n"
        "(defn fact [n acc] (if (<= n 1) acc (recur (- n 1) (* n acc))))\n"
        "(def PI 314)\n"
        "(fib 10)";

    Gram g = gram_new(4096);
    gram_parse(&g, &l, src, strlen(src));
    gram_index(&g);

    StrId s_defn  = str_intern(STR_LIT("defn"));
    StrId s_def   = str_intern(STR_LIT("def"));
    StrId s_recur = str_intern(STR_LIT("recur"));

    // Build semantic bitmask: m_recur_form — lists whose head is "recur"
    u64 *m_recur = bm_new(g.mw);
    for (u32 i = bm_next(g.m[NK_LIST], g.mw, 0); i < g.n;
             i = bm_next(g.m[NK_LIST], g.mw, i + 1)) {
        u32 fc = gn_child(&g, i);
        if (fc && gn_kind(&g, fc) == NK_IDENT && gn_intern(&g, fc) == s_recur)
            BM_SET(m_recur, i);
    }

    // Classify top-level forms — walk root children
    u32 n_defns = 0, n_defs = 0, n_mains = 0;
    u32 defn_nodes[16];

    u32 c = gn_child(&g, 0);
    while (c) {
        if (gn_kind(&g, c) == NK_LIST) {
            u32 fc = gn_child(&g, c);
            if (fc && gn_kind(&g, fc) == NK_IDENT) {
                StrId name = gn_intern(&g, fc);
                if (name == s_defn) { defn_nodes[n_defns++] = c; c = gn_next(&g, c); continue; }
                if (name == s_def) { n_defs++; c = gn_next(&g, c); continue; }
            }
        }
        n_mains++;
        c = gn_next(&g, c);
    }

    check("query: 3 defns", n_defns == 3);
    check("query: 1 def", n_defs == 1);
    check("query: 1 main", n_mains == 1);
    pf("  classified: %u defns, %u defs, %u mains\n", n_defns, n_defs, n_mains);

    // For each defn, detect recur via BITMASK RANGE QUERY
    // No tree walking. Just: does subtree of body contain a recur form?
    pf("  recur detection (pure bitmask range scan):\n");
    for (u32 i = 0; i < n_defns; i++) {
        u32 dn = defn_nodes[i];
        u32 fname = gn_nth(&g, dn, 1);
        u32 body = gn_nth(&g, dn, 3);  // 4th child = body (after defn, name, params)
        if (!body) body = gn_nth(&g, dn, 2);  // might be 3rd if body IS the 3rd

        // THE QUERY: one bitmask range check
        bool has = gn_has(&g, m_recur, dn);
        pf("    "); pr_text(&g, fname);
        pf(": recur=%s\n", has ? "yes" : "no");
    }

    // Verify: fib=no (tree recursion), add=no, fact=yes (has recur)
    check("query: fib no recur", !gn_has(&g, m_recur, defn_nodes[0]));
    check("query: add no recur", !gn_has(&g, m_recur, defn_nodes[1]));
    check("query: fact has recur", gn_has(&g, m_recur, defn_nodes[2]));

    // Count all identifiers in fib's body via bitmask
    u32 fib_idents = gn_sub_count(&g, g.m[NK_IDENT], defn_nodes[0]);
    pf("  fib: %u idents in subtree\n", fib_idents);

    // Count all lists (sub-expressions) in fib
    u32 fib_lists = gn_sub_count(&g, g.m[NK_LIST], defn_nodes[0]);
    pf("  fib: %u lists in subtree\n", fib_lists);

    // Total stats
    pf("  total: %u nodes, %u bytes source\n", g.n, g.src_len);
    pf("  masks: %u words × %u bitmasks = %u bytes index\n",
        g.mw, NK_COUNT + 4, g.mw * 8 * (NK_COUNT + 4));
}

// ============================================================================
// 9. Benchmarks
// ============================================================================

static void bench(void) {
    pf("\n=== benchmarks ===\n");
    Lang l; lang_lisp(&l);

    const char *src =
        "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n"
        "(defn solve [n] (loop [i 1 s 0] (if (>= i n) s (recur (inc i) "
        "(if (or (zero? (mod i 3)) (zero? (mod i 5))) (+ s i) s)))))\n"
        "(solve 1000)";
    u32 slen = strlen(src);

    Gram g = gram_new(4096);

    // Warmup
    gram_parse(&g, &l, src, slen);

    // Parse throughput
    u32 N = 200000;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        g.n = 0;
        gram_parse(&g, &l, src, slen);
    }
    u64 t1 = now_ns();
    f64 parse_ns = (f64)(t1 - t0) / N;
    pf("  parse (%u bytes, %u nodes):\n", slen, g.n);
    pf("    "); buf_f1(&g_print_buf, parse_ns); pf(" ns/parse\n");
    pf("    "); buf_f1(&g_print_buf, (f64)slen * N / ((f64)(t1 - t0) / 1e9) / 1e6);
    pf(" MB/s\n");

    // Index build
    N = 500000;
    gram_index(&g);  // warmup
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) gram_index(&g);
    t1 = now_ns();
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns/index\n");

    // Parse + classify (full frontend)
    StrId id_defn = str_intern(STR_LIT("defn"));
    StrId id_def = str_intern(STR_LIT("def"));
    N = 200000;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        g.n = 0;
        gram_parse(&g, &l, src, slen);
        // Classify
        u32 c = gn_child(&g, 0);
        while (c) {
            if (gn_kind(&g, c) == NK_LIST) {
                u32 fc = gn_child(&g, c);
                if (fc && gn_kind(&g, fc) == NK_IDENT) {
                    StrId name = gn_intern(&g, fc);
                    SINK(name == id_defn || name == id_def);
                }
            }
            c = gn_next(&g, c);
        }
    }
    t1 = now_ns();
    pf("  full frontend (parse+classify+intern):\n");
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns\n");

    // Bitmask query: popcount full
    gram_index(&g);
    N = 2000000;
    u32 total = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++)
        total += bm_pop(g.m[NK_LIST], g.mw);
    t1 = now_ns();
    SINK(total);
    pf("  bitmask popcount (%u nodes):\n", g.n);
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns  (whole program)\n");

    // Range query: subtree check
    u32 defn_node = gn_child(&g, 0);
    N = 5000000;
    u32 hits = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++)
        hits += gn_has(&g, g.m[NK_IDENT], defn_node);
    t1 = now_ns();
    SINK(hits);
    pf("  range query (subtree has ident?):\n");
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns\n");

    // Walk speed: iterate all nodes
    N = 2000000;
    total = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++)
        for (u32 j = 0; j < g.n; j++) total += g.nodes[j].kind;
    t1 = now_ns();
    SINK(total);
    pf("  walk %u nodes:\n", g.n);
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns/walk  (");
    buf_f1(&g_print_buf, (f64)(t1 - t0) / (N * (u64)g.n));
    pf(" ns/node)\n");

    // bm_next iteration (iterate set bits)
    N = 2000000;
    total = 0;
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        u32 pos = 0;
        while ((pos = bm_next(g.m[NK_IDENT], g.mw, pos)) < g.n) { total++; pos++; }
    }
    t1 = now_ns();
    SINK(total);
    u32 n_idents = bm_pop(g.m[NK_IDENT], g.mw);
    pf("  iterate %u idents (bm_next):\n", n_idents);
    pf("    "); buf_f1(&g_print_buf, (f64)(t1 - t0) / N); pf(" ns/iter  (");
    buf_f1(&g_print_buf, (f64)(t1 - t0) / (N * (u64)n_idents));
    pf(" ns/ident)\n");

    // Sizes
    pf("\n  sizes:\n");
    pf("    GNode:    %u bytes\n", (u32)sizeof(GNode));
    pf("    Lang:     %u bytes\n", (u32)sizeof(Lang));
    pf("    nodes:    %u × %u = %u bytes\n", g.n, (u32)sizeof(GNode), g.n * (u32)sizeof(GNode));
    pf("    indexes:  %u words × %u masks × 8 = %u bytes\n",
        g.mw, NK_COUNT + 3, g.mw * (NK_COUNT + 3) * 8);
}

// ============================================================================
// Main + REPL Mode
// ============================================================================

// Global grammar for REPL commands to operate on
static Gram *g_gram;
static Lang  g_lang;

static void cmd_parse(Str args) {
    if (!args.len) { pf("  usage: parse <source>\n"); return; }
    if (!g_gram) {
        static Gram gram = {0};
        gram = gram_new(4096);
        g_gram = &gram;
    }
    gram_parse(g_gram, &g_lang, (const char *)args.data, args.len);
    gram_index(g_gram);
    pf("  %u nodes, %u bytes\n", g_gram->n, g_gram->src_len);
}

static void cmd_tree(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    gram_print(g_gram, 0);
}

static void cmd_nodes(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    for (u32 i = 0; i < g_gram->n; i++) {
        GNode *n = &g_gram->nodes[i];
        pf("  %u: %s", i, NK_NAME[n->kind]);
        if (n->len && n->len < 60) { pf(" \""); pr_text(g_gram, i); pf("\""); }
        pf("  par=%u end=%u\n", n->parent, n->end);
    }
}

static void cmd_stats(Str args) {
    (void)args;
    if (!g_gram || !g_gram->n) { pf("  no parse\n"); return; }
    pf("  nodes: %u  source: %u bytes\n", g_gram->n, g_gram->src_len);
    for (u32 k = 0; k < NK_COUNT; k++) {
        u32 c = bm_pop(g_gram->m[k], g_gram->mw);
        if (c) pf("    %s: %u\n", NK_NAME[k], c);
    }
}

static void cmd_lang(Str args) {
    if (str_eq(args, STR_LIT("lisp")) || str_eq(args, STR_LIT("clj")))
        lang_lisp(&g_lang);
    else if (str_eq(args, STR_LIT("c")))
        lang_c(&g_lang);
    else if (str_eq(args, STR_LIT("bf")))
        lang_bf(&g_lang);
    else { pf("  usage: lang lisp|c|bf  (current: %s)\n", g_lang.name ? g_lang.name : "none"); return; }
    pf("  lang: %s\n", g_lang.name);
}

static void grammar_repl_init(void) {
    cmd_register("parse", cmd_parse, "parse <source>");
    cmd_register("tree",  cmd_tree,  "show parse tree");
    cmd_register("nodes", cmd_nodes, "list all nodes");
    cmd_register("stats", cmd_stats, "node kind counts");
    cmd_register("lang",  cmd_lang,  "set language (lisp|c|bf)");
}

int main(int argc, char **argv) {
    base_init();

    // Init trace kind StrIds
    TK_NODE  = str_intern(STR_LIT("node"));
    TK_PARSE = str_intern(STR_LIT("parse"));
    TK_INDEX = str_intern(STR_LIT("index"));
    TK_QUERY = str_intern(STR_LIT("query"));

    // Check for --repl flag
    bool do_repl = false;
    for (int i = 1; i < argc; i++) {
        Str a = {(u8 *)argv[i], (u32)strlen(argv[i])};
        if (str_eq(a, STR_LIT("--repl")) || str_eq(a, STR_LIT("-r")))
            do_repl = true;
    }

    if (do_repl) {
        grammar_repl_init();
        lang_lisp(&g_lang);
        pf("grammar repl (lang: lisp, try: help)\n");
        repl();
    } else {
        t_pass = t_fail = 0;
        test_bf();
        test_lisp();
        test_c();
        test_query();
        bench();
        pf("\n%d passed, %d failed\n", t_pass, t_fail);
    }

    base_cleanup();
    return 0;
}
