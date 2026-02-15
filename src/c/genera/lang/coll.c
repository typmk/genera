/**
 * coll.c — Collections + Sig: the managed heap
 *
 * pmap (HAMT), pvec (32-way trie), atom — persistent data structures.
 * sig — THE dispatch primitive. One table: StrId → handler. ~1ns lookup.
 *
 * Together: all state (pmap) + all behavior (sig) = serializable managed heap.
 * Depends on: base/base.c, platform/proto.c
 */
#ifndef COLL_C_INCLUDED
#define COLL_C_INCLUDED

// ============================================================================
// 1. Constants + Types
// ============================================================================

#define HAMT_BITS  5
#define HAMT_WIDTH 32
#define HAMT_MASK  31

#define SMALL_MAP_MAX 16

#define VEC_WIDTH 32
#define VEC_BITS  5
#define VEC_MASK  31

// Leaf: key-value pair in HAMT
typedef struct { u32 key; Val value; } CLeaf;

// HAMT node: bitmap-indexed compact array
// Children inline after header. high bit = 1 means sub-node, 0 means leaf.
#define COLL_TAG_NODE ((u32)1 << 31)
#define COLL_IS_NODE(x) ((x) & COLL_TAG_NODE)
#define COLL_IDX(x)     ((x) & ~COLL_TAG_NODE)

typedef struct { u32 bitmap; u32 owner; } CNode;  // owner: 0=persistent, >0=txn ID

// Small map: flat array for ≤16 keys
typedef struct {
    u32 count;
    u32 keys[SMALL_MAP_MAX];
    Val values[SMALL_MAP_MAX];
} CSmallMap;

// pmap handle
typedef struct {
    u32  count;
    u32  root;     // arena offset (UINT32_MAX = empty)
    u32  txn;      // 0 = persistent, >0 = transient transaction ID
    bool is_small;
} CPMap;

// pvec interior node: 32-wide array of child indices
typedef struct { u32 children[VEC_WIDTH]; } CVNode;

// pvec leaf node: 32-wide array of Val (full 64-bit)
typedef struct { Val items[VEC_WIDTH]; } CVLeaf;

// pvec handle
typedef struct {
    u32 count;
    u32 shift;
    u32 root;      // CVNode index (UINT32_MAX if empty)
    u32 tail;      // CVLeaf index for tail
    u32 tail_len;
} CPVec;

// atom: mutable reference
typedef struct { u32 version; Val value; } CAtom;

#define COLL_NIL_IDX UINT32_MAX

// ============================================================================
// 2. Arena pools (allocated from g_perm)
// ============================================================================

#define COLL_NODE_CAP   (64 * 1024 * 1024)  // 64 MB node bytes
#define COLL_LEAF_CAP   (1 << 20)
#define COLL_SMALL_CAP  (1 << 16)
#define COLL_VNODE_CAP  (1 << 20)

static struct {
    u8       *node_mem;  u32 node_used, node_cap;
    CLeaf    *leaves;    u32 leaf_count, leaf_cap;
    CSmallMap *smalls;   u32 small_count, small_cap;
    CVNode   *vnodes;    u32 vnode_count, vnode_cap;
    CVLeaf   *vleaves;   u32 vleaf_count, vleaf_cap;
    u32       txn;       // transaction counter for transient maps
    bool      inited;
} g_coll;

static void coll_init(void) {
    if (g_coll.inited) return;
    g_coll.node_cap  = COLL_NODE_CAP;
    g_coll.node_mem  = (u8 *)sys_alloc(g_coll.node_cap);
    g_coll.node_used = 0;
    g_coll.leaf_cap   = COLL_LEAF_CAP;
    g_coll.leaves     = (CLeaf *)sys_alloc(sizeof(CLeaf) * g_coll.leaf_cap);
    g_coll.leaf_count = 0;
    g_coll.small_cap   = COLL_SMALL_CAP;
    g_coll.smalls      = (CSmallMap *)sys_alloc(sizeof(CSmallMap) * g_coll.small_cap);
    g_coll.small_count = 0;
    g_coll.vnode_cap   = COLL_VNODE_CAP;
    g_coll.vnodes      = (CVNode *)sys_alloc(sizeof(CVNode) * g_coll.vnode_cap);
    g_coll.vnode_count = 0;
    g_coll.vleaf_cap   = COLL_VNODE_CAP;
    g_coll.vleaves     = (CVLeaf *)sys_alloc(sizeof(CVLeaf) * g_coll.vleaf_cap);
    g_coll.vleaf_count = 0;
    g_coll.inited = true;
}

static void coll_cleanup(void) {
    if (!g_coll.inited) return;
    sys_free(g_coll.node_mem, g_coll.node_cap);
    sys_free(g_coll.leaves, sizeof(CLeaf) * g_coll.leaf_cap);
    sys_free(g_coll.smalls, sizeof(CSmallMap) * g_coll.small_cap);
    sys_free(g_coll.vnodes, sizeof(CVNode) * g_coll.vnode_cap);
    sys_free(g_coll.vleaves, sizeof(CVLeaf) * g_coll.vleaf_cap);
    g_coll.inited = false;
}

static void coll_reset(void) {
    g_coll.node_used   = 0;
    g_coll.leaf_count  = 0;
    g_coll.small_count = 0;
    g_coll.vnode_count = 0;
    g_coll.vleaf_count = 0;
}

// Internal allocators
ALWAYS_INLINE u32 coll_alloc_node(u32 n_children) {
    u32 size = (u32)(sizeof(CNode) + sizeof(u32) * n_children);
    size = (size + 3) & ~3u;
    u32 off = g_coll.node_used;
    g_coll.node_used += size;
    ((CNode *)(g_coll.node_mem + off))->owner = 0;
    return off;
}

ALWAYS_INLINE CNode *coll_get_node(u32 off) {
    return (CNode *)(g_coll.node_mem + off);
}

ALWAYS_INLINE u32 *coll_get_children(u32 off) {
    return (u32 *)(g_coll.node_mem + off + sizeof(CNode));
}

ALWAYS_INLINE u32 coll_alloc_leaf(u32 key, Val value) {
    u32 idx = g_coll.leaf_count++;
    g_coll.leaves[idx] = (CLeaf){key, value};
    return idx;
}

ALWAYS_INLINE u32 coll_alloc_small(void) {
    u32 idx = g_coll.small_count++;
    g_coll.smalls[idx].count = 0;
    return idx;
}

ALWAYS_INLINE u32 coll_alloc_vnode(void) {
    u32 idx = g_coll.vnode_count++;
    memset(&g_coll.vnodes[idx], 0xFF, sizeof(CVNode));
    return idx;
}

ALWAYS_INLINE u32 coll_alloc_vleaf(void) {
    u32 idx = g_coll.vleaf_count++;
    memset(&g_coll.vleaves[idx], 0, sizeof(CVLeaf));
    return idx;
}

// ============================================================================
// 3. PMAP — Persistent Hash-Array Mapped Trie
// ============================================================================

static CPMap cpmap_empty(void) {
    return (CPMap){0, COLL_NIL_IDX, 0, true};
}

// --- Small map ops ---

static bool csmall_get(u32 sm_idx, u32 key, Val *out) {
    CSmallMap *sm = &g_coll.smalls[sm_idx];
    for (u32 i = 0; i < sm->count; i++) {
        if (sm->keys[i] == key) { *out = sm->values[i]; return true; }
    }
    return false;
}

static u32 csmall_put(u32 sm_idx, u32 key, Val val, bool *added) {
    CSmallMap *old = &g_coll.smalls[sm_idx];
    u32 new_idx = coll_alloc_small();
    CSmallMap *nw = &g_coll.smalls[new_idx];
    memcpy(nw, old, sizeof(CSmallMap));
    for (u32 i = 0; i < nw->count; i++) {
        if (nw->keys[i] == key) { nw->values[i] = val; *added = false; return new_idx; }
    }
    nw->keys[nw->count] = key;
    nw->values[nw->count] = val;
    nw->count++;
    *added = true;
    return new_idx;
}

// --- HAMT ops ---

static u32 chamt_leaf_node(u32 hash, u32 shift, u32 leaf_idx) {
    u32 bit = 1u << ((hash >> shift) & HAMT_MASK);
    u32 off = coll_alloc_node(1);
    CNode *n = coll_get_node(off);
    n->bitmap = bit;
    coll_get_children(off)[0] = leaf_idx;
    return off;
}

static u32 chamt_merge_leaves(u32 shift, u32 h1, u32 l1, u32 h2, u32 l2) {
    if (shift >= 30) {
        u32 off = coll_alloc_node(2);
        CNode *n = coll_get_node(off);
        n->bitmap = 0;
        u32 *ch = coll_get_children(off);
        ch[0] = l1; ch[1] = l2;
        return off;
    }
    u32 b1 = (h1 >> shift) & HAMT_MASK;
    u32 b2 = (h2 >> shift) & HAMT_MASK;
    if (b1 == b2) {
        u32 child = chamt_merge_leaves(shift + HAMT_BITS, h1, l1, h2, l2);
        u32 off = coll_alloc_node(1);
        CNode *n = coll_get_node(off);
        n->bitmap = 1u << b1;
        coll_get_children(off)[0] = child | COLL_TAG_NODE;
        return off;
    }
    u32 off = coll_alloc_node(2);
    CNode *n = coll_get_node(off);
    n->bitmap = (1u << b1) | (1u << b2);
    u32 *ch = coll_get_children(off);
    if (b1 < b2) { ch[0] = l1; ch[1] = l2; }
    else          { ch[0] = l2; ch[1] = l1; }
    return off;
}

static u32 chamt_put(u32 node_off, u32 shift, u32 hash, u32 key, Val val, bool *added) {
    CNode *n = coll_get_node(node_off);

    // Collision node
    if (n->bitmap == 0) {
        u32 *ch = coll_get_children(node_off);
        for (int i = 0; i < 2; i++) {
            CLeaf *lf = &g_coll.leaves[ch[i]];
            if (lf->key == key) {
                u32 new_leaf = coll_alloc_leaf(key, val);
                u32 new_off = coll_alloc_node(2);
                CNode *nn = coll_get_node(new_off);
                nn->bitmap = 0;
                u32 *nc = coll_get_children(new_off);
                nc[0] = (i == 0) ? new_leaf : ch[0];
                nc[1] = (i == 1) ? new_leaf : ch[1];
                *added = false;
                return new_off;
            }
        }
        *added = true;
        u32 new_leaf = coll_alloc_leaf(key, val);
        u32 new_off = coll_alloc_node(3);
        CNode *nn = coll_get_node(new_off);
        nn->bitmap = 0;
        u32 *nc = coll_get_children(new_off);
        nc[0] = ch[0]; nc[1] = ch[1]; nc[2] = new_leaf;
        return new_off;
    }

    u32 bit = 1u << ((hash >> shift) & HAMT_MASK);
    u32 idx = POPCOUNT(n->bitmap & (bit - 1));
    u32 cnt = POPCOUNT(n->bitmap);
    u32 *ch = coll_get_children(node_off);

    if (!(n->bitmap & bit)) {
        *added = true;
        u32 leaf = coll_alloc_leaf(key, val);
        u32 new_off = coll_alloc_node(cnt + 1);
        CNode *nn = coll_get_node(new_off);
        nn->bitmap = n->bitmap | bit;
        u32 *nc = coll_get_children(new_off);
        memcpy(nc, ch, sizeof(u32) * idx);
        nc[idx] = leaf;
        memcpy(nc + idx + 1, ch + idx, sizeof(u32) * (cnt - idx));
        return new_off;
    }

    u32 child = ch[idx];
    if (COLL_IS_NODE(child)) {
        u32 new_child = chamt_put(COLL_IDX(child), shift + HAMT_BITS, hash, key, val, added);
        u32 new_off = coll_alloc_node(cnt);
        CNode *nn = coll_get_node(new_off);
        nn->bitmap = n->bitmap;
        u32 *nc = coll_get_children(new_off);
        memcpy(nc, ch, sizeof(u32) * cnt);
        nc[idx] = new_child | COLL_TAG_NODE;
        return new_off;
    }

    CLeaf *lf = &g_coll.leaves[child];
    if (lf->key == key) {
        if (lf->value == val) { *added = false; return node_off; }
        u32 new_leaf = coll_alloc_leaf(key, val);
        u32 new_off = coll_alloc_node(cnt);
        CNode *nn = coll_get_node(new_off);
        nn->bitmap = n->bitmap;
        u32 *nc = coll_get_children(new_off);
        memcpy(nc, ch, sizeof(u32) * cnt);
        nc[idx] = new_leaf;
        *added = false;
        return new_off;
    }

    *added = true;
    u32 h2 = hash32(lf->key);
    u32 new_leaf = coll_alloc_leaf(key, val);
    u32 sub = chamt_merge_leaves(shift + HAMT_BITS, h2, child, hash, new_leaf);
    u32 new_off = coll_alloc_node(cnt);
    CNode *nn = coll_get_node(new_off);
    nn->bitmap = n->bitmap;
    u32 *nc = coll_get_children(new_off);
    memcpy(nc, ch, sizeof(u32) * cnt);
    nc[idx] = sub | COLL_TAG_NODE;
    return new_off;
}

static bool chamt_get(u32 node_off, u32 shift, u32 hash, u32 key, Val *out) {
    CNode *n = coll_get_node(node_off);
    if (n->bitmap == 0) {
        u32 *ch = coll_get_children(node_off);
        for (int i = 0; i < 3; i++) {
            if (ch[i] == 0 && i > 0) break;
            CLeaf *lf = &g_coll.leaves[ch[i]];
            if (lf->key == key) { *out = lf->value; return true; }
        }
        return false;
    }
    u32 bit = 1u << ((hash >> shift) & HAMT_MASK);
    if (!(n->bitmap & bit)) return false;
    u32 idx = POPCOUNT(n->bitmap & (bit - 1));
    u32 child = coll_get_children(node_off)[idx];
    if (COLL_IS_NODE(child))
        return chamt_get(COLL_IDX(child), shift + HAMT_BITS, hash, key, out);
    CLeaf *lf = &g_coll.leaves[child];
    if (lf->key == key) { *out = lf->value; return true; }
    return false;
}

// --- Public pmap API ---

static CPMap cpmap_put(CPMap m, u32 key, Val val) {
    bool added = false;
    if (m.count == 0) {
        u32 sm = coll_alloc_small();
        g_coll.smalls[sm].keys[0] = key;
        g_coll.smalls[sm].values[0] = val;
        g_coll.smalls[sm].count = 1;
        return (CPMap){1, sm, 0, true};
    }
    if (m.is_small) {
        CSmallMap *old = &g_coll.smalls[m.root];
        for (u32 i = 0; i < old->count; i++) {
            if (old->keys[i] == key) {
                if (old->values[i] == val) return m;
                u32 new_sm = csmall_put(m.root, key, val, &added);
                return (CPMap){m.count, new_sm, 0, true};
            }
        }
        if (old->count < SMALL_MAP_MAX) {
            u32 new_sm = csmall_put(m.root, key, val, &added);
            return (CPMap){m.count + 1, new_sm, 0, true};
        }
        // Promote to HAMT
        u32 first_leaf = coll_alloc_leaf(old->keys[0], old->values[0]);
        u32 root = chamt_leaf_node(hash32(old->keys[0]), 0, first_leaf);
        for (u32 i = 1; i < old->count; i++) {
            root = chamt_put(root, 0, hash32(old->keys[i]), old->keys[i], old->values[i], &added);
        }
        added = false;
        root = chamt_put(root, 0, hash32(key), key, val, &added);
        return (CPMap){m.count + (added ? 1 : 0), root, 0, false};
    }
    u32 new_root = chamt_put(m.root, 0, hash32(key), key, val, &added);
    return (CPMap){m.count + (added ? 1 : 0), new_root, 0, false};
}

static bool cpmap_get(CPMap m, u32 key, Val *out) {
    if (m.count == 0) return false;
    if (m.is_small) return csmall_get(m.root, key, out);
    return chamt_get(m.root, 0, hash32(key), key, out);
}

ALWAYS_INLINE u32 cpmap_count(CPMap m) { return m.count; }

// ============================================================================
// 4. PVEC — Persistent Vector (32-way trie with tail)
// ============================================================================

static CPVec cpvec_empty(void) {
    return (CPVec){0, VEC_BITS, COLL_NIL_IDX, COLL_NIL_IDX, 0};
}

static Val cpvec_get(CPVec v, u32 i) {
    if (i >= v.count) return val_nil();
    u32 tail_off = v.count - v.tail_len;
    if (i >= tail_off) return g_coll.vleaves[v.tail].items[i - tail_off];
    // Walk interior nodes
    CVNode *node = &g_coll.vnodes[v.root];
    for (u32 level = v.shift; level > VEC_BITS; level -= VEC_BITS)
        node = &g_coll.vnodes[node->children[(i >> level) & VEC_MASK]];
    // Bottom level: children are CVLeaf indices
    return g_coll.vleaves[node->children[(i >> VEC_BITS) & VEC_MASK]].items[i & VEC_MASK];
}

static u32 cpvec_new_path(u32 shift, u32 tail) {
    if (shift == 0) return tail;
    u32 nid = coll_alloc_vnode();
    g_coll.vnodes[nid].children[0] = cpvec_new_path(shift - VEC_BITS, tail);
    return nid;
}

static u32 cpvec_push_tail(u32 count, u32 shift, u32 parent, u32 tail) {
    u32 sub_idx = ((count - 1) >> shift) & VEC_MASK;
    u32 nid = coll_alloc_vnode();
    memcpy(&g_coll.vnodes[nid], &g_coll.vnodes[parent], sizeof(CVNode));
    if (shift == VEC_BITS) {
        g_coll.vnodes[nid].children[sub_idx] = tail;
    } else if (g_coll.vnodes[parent].children[sub_idx] != COLL_NIL_IDX) {
        g_coll.vnodes[nid].children[sub_idx] =
            cpvec_push_tail(count, shift - VEC_BITS,
                           g_coll.vnodes[parent].children[sub_idx], tail);
    } else {
        g_coll.vnodes[nid].children[sub_idx] = cpvec_new_path(shift - VEC_BITS, tail);
    }
    return nid;
}

static CPVec cpvec_append(CPVec v, Val val) {
    u32 n = v.count;
    if (v.tail_len < VEC_WIDTH) {
        u32 new_tail;
        if (v.tail == COLL_NIL_IDX) {
            new_tail = coll_alloc_vleaf();
        } else {
            new_tail = coll_alloc_vleaf();
            memcpy(&g_coll.vleaves[new_tail], &g_coll.vleaves[v.tail], sizeof(CVLeaf));
        }
        g_coll.vleaves[new_tail].items[v.tail_len] = val;
        return (CPVec){n + 1, v.shift, v.root, new_tail, v.tail_len + 1};
    }
    // Tail overflow: push old tail into tree, start new tail
    u32 new_tail = coll_alloc_vleaf();
    g_coll.vleaves[new_tail].items[0] = val;
    if (v.root == COLL_NIL_IDX) {
        u32 new_root = coll_alloc_vnode();
        g_coll.vnodes[new_root].children[0] = v.tail;
        return (CPVec){n + 1, VEC_BITS, new_root, new_tail, 1};
    }
    u32 shift = v.shift;
    if ((n >> VEC_BITS) > (1u << shift)) {
        u32 new_root = coll_alloc_vnode();
        g_coll.vnodes[new_root].children[0] = v.root;
        g_coll.vnodes[new_root].children[1] = cpvec_new_path(shift, v.tail);
        return (CPVec){n + 1, shift + VEC_BITS, new_root, new_tail, 1};
    }
    u32 new_root = cpvec_push_tail(n, shift, v.root, v.tail);
    return (CPVec){n + 1, shift, new_root, new_tail, 1};
}

ALWAYS_INLINE u32 cpvec_count(CPVec v) { return v.count; }

// ============================================================================
// 5. ATOM — Mutable reference with version tracking
// ============================================================================

static CAtom catom_new(Val val) { return (CAtom){0, val}; }
ALWAYS_INLINE Val catom_deref(CAtom *a) { return a->value; }
static Val catom_reset(CAtom *a, Val val) { a->value = val; a->version++; return val; }
static bool catom_cas(CAtom *a, Val expected, Val new_val) {
    if (a->value == expected) { a->value = new_val; a->version++; return true; }
    return false;
}

// ============================================================================
// 6. PMAP Iteration — walk small map or HAMT tree
// ============================================================================

typedef void (*CMapIterFn)(u32 key, Val value, void *ctx);

static void chamt_iter(u32 node_off, CMapIterFn fn, void *ctx) {
    CNode *n = coll_get_node(node_off);
    if (n->bitmap == 0) {
        // Collision node: 2-3 leaf children
        u32 *ch = coll_get_children(node_off);
        for (int i = 0; i < 3; i++) {
            if (ch[i] >= g_coll.leaf_count) break;
            fn(g_coll.leaves[ch[i]].key, g_coll.leaves[ch[i]].value, ctx);
        }
        return;
    }
    u32 cnt = POPCOUNT(n->bitmap);
    u32 *ch = coll_get_children(node_off);
    for (u32 i = 0; i < cnt; i++) {
        if (COLL_IS_NODE(ch[i]))
            chamt_iter(COLL_IDX(ch[i]), fn, ctx);
        else
            fn(g_coll.leaves[ch[i]].key, g_coll.leaves[ch[i]].value, ctx);
    }
}

static void cpmap_foreach(CPMap m, CMapIterFn fn, void *ctx) {
    if (m.count == 0) return;
    if (m.is_small) {
        CSmallMap *sm = &g_coll.smalls[m.root];
        for (u32 i = 0; i < sm->count; i++)
            fn(sm->keys[i], sm->values[i], ctx);
        return;
    }
    chamt_iter(m.root, fn, ctx);
}

// ============================================================================
// 7. TRANSIENT HAMT — owner-tagged mutation for batch operations
// ============================================================================

ALWAYS_INLINE u32 coll_next_txn(void) { return ++g_coll.txn; }

// Ensure node is mutable for current transaction.
// If already owned → return same offset (mutate in place).
// Otherwise → clone and return new offset.
ALWAYS_INLINE u32 coll_ensure_mutable(u32 off, u32 txn) {
    CNode *n = coll_get_node(off);
    if (n->owner == txn) return off;
    u32 cnt = POPCOUNT(n->bitmap);
    u32 new_off = coll_alloc_node(cnt);
    CNode *nn = coll_get_node(new_off);
    nn->bitmap = n->bitmap;
    nn->owner = txn;
    memcpy(coll_get_children(new_off), coll_get_children(off), sizeof(u32) * cnt);
    return new_off;
}

// Transient HAMT put: mutate in place if owned, else clone-then-mutate
static u32 chamt_put_transient(u32 node_off, u32 shift, u32 hash,
                               u32 key, Val val, u32 txn, bool *added) {
    node_off = coll_ensure_mutable(node_off, txn);
    CNode *n = coll_get_node(node_off);
    u32 cnt = POPCOUNT(n->bitmap);
    u32 *ch = coll_get_children(node_off);

    u32 bit = 1u << ((hash >> shift) & HAMT_MASK);
    u32 idx = POPCOUNT(n->bitmap & (bit - 1));

    if (!(n->bitmap & bit)) {
        // Empty slot: must alloc new node (can't expand inline)
        *added = true;
        u32 leaf = coll_alloc_leaf(key, val);
        u32 new_off = coll_alloc_node(cnt + 1);
        CNode *nn = coll_get_node(new_off);
        nn->bitmap = n->bitmap | bit;
        nn->owner = txn;
        u32 *nc = coll_get_children(new_off);
        memcpy(nc, ch, sizeof(u32) * idx);
        nc[idx] = leaf;
        memcpy(nc + idx + 1, ch + idx, sizeof(u32) * (cnt - idx));
        return new_off;
    }

    u32 child = ch[idx];
    if (COLL_IS_NODE(child)) {
        u32 new_child = chamt_put_transient(COLL_IDX(child), shift + HAMT_BITS,
                                            hash, key, val, txn, added);
        ch[idx] = new_child | COLL_TAG_NODE;  // mutate in place
        return node_off;
    }

    // Leaf
    CLeaf *lf = &g_coll.leaves[child];
    if (lf->key == key) {
        u32 new_leaf = coll_alloc_leaf(key, val);
        ch[idx] = new_leaf;  // mutate in place
        *added = false;
        return node_off;
    }

    // Split leaf
    *added = true;
    u32 new_leaf = coll_alloc_leaf(key, val);
    u32 sub = chamt_merge_leaves(shift + HAMT_BITS, hash32(lf->key), child, hash, new_leaf);
    ch[idx] = sub | COLL_TAG_NODE;  // mutate in place
    return node_off;
}

// Create transient (mutable) copy of a persistent map
static CPMap cpmap_transient(CPMap m) {
    u32 txn = coll_next_txn();
    if (m.is_small && m.count > 0) {
        // Copy small map so transient owns it
        u32 new_sm = coll_alloc_small();
        memcpy(&g_coll.smalls[new_sm], &g_coll.smalls[m.root], sizeof(CSmallMap));
        return (CPMap){m.count, new_sm, txn, true};
    }
    return (CPMap){m.count, m.root, txn, m.is_small};
}

// Freeze transient → persistent (advance txn so old nodes can't be mutated)
static CPMap cpmap_persistent(CPMap m) {
    return (CPMap){m.count, m.root, 0, m.is_small};
}

// Transient put: mutate in place where possible
static CPMap cpmap_put_t(CPMap m, u32 key, Val val) {
    bool added = false;
    if (m.count == 0) {
        u32 sm;
        if (m.root == COLL_NIL_IDX) sm = coll_alloc_small();
        else sm = m.root;
        g_coll.smalls[sm].keys[0] = key;
        g_coll.smalls[sm].values[0] = val;
        g_coll.smalls[sm].count = 1;
        return (CPMap){1, sm, m.txn, true};
    }
    if (m.is_small) {
        CSmallMap *sm = &g_coll.smalls[m.root];
        for (u32 i = 0; i < sm->count; i++) {
            if (sm->keys[i] == key) { sm->values[i] = val; return m; }
        }
        if (sm->count < SMALL_MAP_MAX) {
            sm->keys[sm->count] = key;
            sm->values[sm->count] = val;
            sm->count++;
            m.count++;
            return m;
        }
        // Promote to HAMT (transient)
        u32 first_leaf = coll_alloc_leaf(sm->keys[0], sm->values[0]);
        u32 root = chamt_leaf_node(hash32(sm->keys[0]), 0, first_leaf);
        coll_get_node(root)->owner = m.txn;
        for (u32 i = 1; i < sm->count; i++) {
            root = chamt_put_transient(root, 0, hash32(sm->keys[i]),
                                       sm->keys[i], sm->values[i], m.txn, &added);
        }
        added = false;
        root = chamt_put_transient(root, 0, hash32(key), key, val, m.txn, &added);
        return (CPMap){m.count + (added ? 1 : 0), root, m.txn, false};
    }
    // HAMT transient put
    u32 new_root = chamt_put_transient(m.root, 0, hash32(key), key, val, m.txn, &added);
    return (CPMap){m.count + (added ? 1 : 0), new_root, m.txn, false};
}

// ============================================================================
// 8. Protocol Implementations for PMAP/PVEC
// ============================================================================

static Val _pmap_get(Val m, Val key) {
    CPMap *pm = (CPMap *)val_as_pmap(m);
    u32 k;
    if (val_is_kw(key))       k = val_as_kw(key);
    else if (val_is_sym(key)) k = val_as_sym(key);
    else return NIL;
    Val out;
    if (cpmap_get(*pm, k, &out)) return out;
    return NIL;
}

static Val _pmap_count(Val m) {
    return val_int(cpmap_count(*(CPMap *)val_as_pmap(m)));
}

// _pmap_seq: return cons list of [k v] entry vectors (proper Clojure behavior)
typedef struct { Val result; } _PMapSeqCtx;
static void _pmap_seq_cb(u32 key, Val value, void *ctx) {
    _PMapSeqCtx *sc = (_PMapSeqCtx *)ctx;
    CPVec *v = arena_push(&g_req, CPVec);
    *v = cpvec_empty();
    *v = cpvec_append(*v, val_kw((StrId)key));
    *v = cpvec_append(*v, value);
    sc->result = cons_new(val_pvec(v), sc->result);
}
static Val _pmap_seq(Val m) {
    CPMap *pm = (CPMap *)val_as_pmap(m);
    if (pm->count == 0) return NIL;
    _PMapSeqCtx ctx = {NIL};
    cpmap_foreach(*pm, _pmap_seq_cb, &ctx);
    return ctx.result;
}

static Val _pmap_assoc(Val m, Val k, Val v) {
    CPMap *pm = (CPMap *)val_as_pmap(m);
    u32 key;
    if (val_is_kw(k))       key = val_as_kw(k);
    else if (val_is_sym(k)) key = val_as_sym(k);
    else return m;
    if (pm->txn > 0) {
        // Transient: mutate in place, return same Val
        *pm = cpmap_put_t(*pm, key, v);
        return m;
    }
    CPMap *nm = arena_push(&g_req, CPMap);
    *nm = cpmap_put(*pm, key, v);
    return val_pmap(nm);
}

static Val _pvec_first(Val v) {
    CPVec *pv = (CPVec *)val_as_pvec(v);
    return pv->count > 0 ? cpvec_get(*pv, 0) : NIL;
}

static Val _pvec_rest(Val v) {
    CPVec *pv = (CPVec *)val_as_pvec(v);
    if (pv->count <= 1) return NIL;
    // Build cons list from index 1..n
    Val result = NIL;
    for (i32 i = (i32)pv->count - 1; i >= 1; i--)
        result = cons_new(cpvec_get(*pv, (u32)i), result);
    return result;
}

static Val _pvec_count(Val v) {
    return val_int(cpvec_count(*(CPVec *)val_as_pvec(v)));
}

static Val _pvec_get(Val v, Val idx) {
    if (!val_is_int(idx)) return NIL;
    return cpvec_get(*(CPVec *)val_as_pvec(v), (u32)val_as_int(idx));
}

static Val _pvec_seq(Val v) {
    return ((CPVec *)val_as_pvec(v))->count > 0 ? v : NIL;
}

static Val _pvec_assoc(Val v, Val k, Val newval) {
    // vec assoc: (assoc [a b c] 1 :x) => [a :x c]
    if (!val_is_int(k)) return v;
    CPVec *pv = (CPVec *)val_as_pvec(v);
    // Rebuild with modified index (full persistent update deferred)
    CPVec *nv = arena_push(&g_req, CPVec);
    *nv = cpvec_empty();
    for (u32 i = 0; i < pv->count; i++)
        *nv = cpvec_append(*nv, i == (u32)val_as_int(k) ? newval : cpvec_get(*pv, i));
    return val_pvec(nv);
}

// Convert pvec to cons list (for special form bindings/params)
static Val pvec_to_list(Val v) {
    if (!val_is_pvec(v)) return v;
    CPVec *pv = (CPVec *)val_as_pvec(v);
    Val result = NIL;
    for (i32 i = (i32)pv->count - 1; i >= 0; i--)
        result = cons_new(cpvec_get(*pv, (u32)i), result);
    return result;
}

static void coll_register_protos(void) {
    extend(P_get,   TI_PMAP, (PFn2)_pmap_get);
    extend(P_count, TI_PMAP, (PFn1)_pmap_count);
    extend(P_seq,   TI_PMAP, (PFn1)_pmap_seq);
    extend(P_assoc, TI_PMAP, (PFn3)_pmap_assoc);

    extend(P_first, TI_PVEC, (PFn1)_pvec_first);
    extend(P_rest,  TI_PVEC, (PFn1)_pvec_rest);
    extend(P_count, TI_PVEC, (PFn1)_pvec_count);
    extend(P_get,   TI_PVEC, (PFn2)_pvec_get);
    extend(P_seq,   TI_PVEC, (PFn1)_pvec_seq);
    extend(P_assoc, TI_PVEC, (PFn3)_pvec_assoc);
}

// ============================================================================
// 8. SIG — THE Dispatch Primitive
//
// One table: StrId → handler. ~1ns lookup.
// Everything is a name: special forms, events, conditions.
//
// Signal protocol: condition/restart for control flow.
// After eval(), check g_signal. If set, propagate immediately.
// loop catches SIGNAL_RECUR. Top-level catches SIGNAL_ERROR.
// ============================================================================

typedef Val (*SigFn)(Val data, void *ctx);

#define SIG_CAP     256
#define SIG_SPECIAL 0x01

static SigFn g_sig_table[SIG_CAP];
static u8    g_sig_flags[SIG_CAP];

ALWAYS_INLINE void sig_on(StrId name, SigFn fn, u8 flags) {
    if (name < SIG_CAP) {
        g_sig_table[name] = fn;
        g_sig_flags[name] = flags;
    }
}

ALWAYS_INLINE SigFn sig_get(StrId name) {
    return (name < SIG_CAP) ? g_sig_table[name] : 0;
}

ALWAYS_INLINE bool sig_is_special(StrId name) {
    return name < SIG_CAP && (g_sig_flags[name] & SIG_SPECIAL);
}

// Condition protocol — control flow side-channel
#define SIGNAL_NONE  0
#define SIGNAL_RECUR 1
#define SIGNAL_ERROR 2
#define DEPTH_MAX    1024

static u8  g_signal;
static Val g_signal_val;
static u32 g_depth;

#endif // COLL_C_INCLUDED
