/**
 * proto_coll.c — C prototype of pmap, pvec, atom from coll.php
 *
 * Explores: how fast can these persistent data structures be in C?
 *
 * Design (borrowed from clojure-fast/):
 *   - Arena allocation: zero malloc on hot path
 *   - Bitmap HAMT: popcount-indexed compact nodes (Clojure's actual design)
 *   - 32-bit indices: half the size of 64-bit pointers, cache-friendly
 *   - String interning: key compare = integer compare
 *   - Small-map optimization: flat array for ≤16 keys (matches coll.php)
 *
 * Build:
 *   gcc -O3 -march=native -o proto_coll test/proto_coll.c -lm
 *   ./proto_coll
 *
 * PHP reference numbers (from bench, 2026-02-09):
 *   pmap get:  381 ns
 *   pmap put:  2.5 µs
 *   array get: 100 ns
 *   array put: 426 ns
 *   100 snapshots (500 entities): pmap 460 KB, array 3580 KB
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#ifdef __GNUC__
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#define POPCOUNT(x) __builtin_popcount(x)
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
static inline int POPCOUNT(uint32_t x) {
    x = x - ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    return (((x + (x >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}
#endif

// ============================================================================
// Constants
// ============================================================================

#define HAMT_BITS  5
#define HAMT_WIDTH 32
#define HAMT_MASK  31
#define HAMT_DEPTH 7          // 32-bit hash / 5 bits = ~7 levels max

#define SMALL_MAP_MAX 16      // matches coll.php threshold

#define ARENA_NODES  (1 << 20)  // 1M nodes
#define ARENA_LEAVES (1 << 20)  // 1M leaves
#define INTERN_CAP   (1 << 16)  // 64K interned strings

#define VEC_WIDTH 32
#define VEC_BITS  5
#define VEC_MASK  31

// Sentinel
#define NIL UINT32_MAX

// ============================================================================
// Types
// ============================================================================

// Leaf: key-value pair in the HAMT
typedef struct {
    uint32_t key;       // interned string ID
    int64_t  value;     // payload
} Leaf;

// HAMT node: bitmap-indexed compact array
// Children stored inline after the struct in arena memory.
// Each child is a uint32_t: high bit = 1 means sub-node index,
//                           high bit = 0 means leaf index.
#define TAG_NODE ((uint32_t)1 << 31)
#define IS_NODE(x) ((x) & TAG_NODE)
#define IDX(x)     ((x) & ~TAG_NODE)

typedef struct {
    uint32_t bitmap;    // which of 32 slots are populated
} Node;
// Followed by POPCOUNT(bitmap) uint32_t child entries

// pmap: persistent map handle
typedef struct {
    uint32_t count;
    uint32_t root;      // NIL = empty, or index into node arena
    // For small maps (count ≤ SMALL_MAP_MAX):
    // root = index into small_maps arena
    bool     is_small;
} PMap;

// Small map: flat array, linear scan
typedef struct {
    uint32_t count;
    uint32_t keys[SMALL_MAP_MAX];
    int64_t  values[SMALL_MAP_MAX];
} SmallMap;

// pvec node: 32-wide array of child indices
typedef struct {
    uint32_t children[VEC_WIDTH];
} VNode;

// pvec: persistent vector handle
typedef struct {
    uint32_t count;
    uint32_t shift;
    uint32_t root;      // VNode arena index (NIL if empty)
    uint32_t tail;      // VNode arena index for tail
    uint32_t tail_len;  // elements in tail (0..32)
} PVec;

// atom: mutable reference
typedef struct {
    uint32_t version;
    int64_t  value;
} Atom;

// ============================================================================
// Arena allocator — bump pointer, zero malloc on hot path
// ============================================================================

typedef struct {
    // HAMT nodes: each node is [Node header][uint32_t children[popcount]]
    // We store them as a flat byte arena for variable-size nodes.
    uint8_t  *node_mem;
    uint32_t  node_used;    // bytes used
    uint32_t  node_cap;     // bytes capacity

    // Leaves (fixed-size)
    Leaf     *leaves;
    uint32_t  leaf_count;
    uint32_t  leaf_cap;

    // Small maps (fixed-size)
    SmallMap *smalls;
    uint32_t  small_count;
    uint32_t  small_cap;

    // VNodes (fixed-size)
    VNode    *vnodes;
    uint32_t  vnode_count;
    uint32_t  vnode_cap;
} Arena;

static Arena g_arena;

static void arena_init(void) {
    // ~40 MB total — fits in RAM, plenty for benchmarks
    g_arena.node_cap  = 64 * 1024 * 1024;  // 64 MB for HAMT nodes
    g_arena.node_mem  = (uint8_t *)malloc(g_arena.node_cap);
    g_arena.node_used = 0;

    g_arena.leaf_cap   = ARENA_LEAVES;
    g_arena.leaves     = (Leaf *)malloc(sizeof(Leaf) * g_arena.leaf_cap);
    g_arena.leaf_count = 0;

    g_arena.small_cap   = 1 << 16;
    g_arena.smalls      = (SmallMap *)malloc(sizeof(SmallMap) * g_arena.small_cap);
    g_arena.small_count = 0;

    g_arena.vnode_cap   = ARENA_NODES;
    g_arena.vnodes      = (VNode *)malloc(sizeof(VNode) * g_arena.vnode_cap);
    g_arena.vnode_count = 0;
}

static void arena_free(void) {
    free(g_arena.node_mem);
    free(g_arena.leaves);
    free(g_arena.smalls);
    free(g_arena.vnodes);
}

static void arena_reset(void) {
    g_arena.node_used   = 0;
    g_arena.leaf_count  = 0;
    g_arena.small_count = 0;
    g_arena.vnode_count = 0;
}

// Allocate a HAMT node with `n` children slots. Returns byte offset in node_mem.
static inline uint32_t alloc_node(uint32_t n_children) {
    uint32_t size = (uint32_t)(sizeof(Node) + sizeof(uint32_t) * n_children);
    // Align to 4 bytes
    size = (size + 3) & ~3u;
    uint32_t off = g_arena.node_used;
    g_arena.node_used += size;
    return off;
}

static inline Node *get_node(uint32_t offset) {
    return (Node *)(g_arena.node_mem + offset);
}

static inline uint32_t *get_children(uint32_t offset) {
    return (uint32_t *)(g_arena.node_mem + offset + sizeof(Node));
}

static inline uint32_t alloc_leaf(uint32_t key, int64_t value) {
    uint32_t idx = g_arena.leaf_count++;
    g_arena.leaves[idx] = (Leaf){key, value};
    return idx;
}

static inline uint32_t alloc_small(void) {
    uint32_t idx = g_arena.small_count++;
    g_arena.smalls[idx].count = 0;
    return idx;
}

static inline uint32_t alloc_vnode(void) {
    uint32_t idx = g_arena.vnode_count++;
    memset(&g_arena.vnodes[idx], 0xFF, sizeof(VNode));  // fill with NIL
    return idx;
}

// ============================================================================
// String interning — string → uint32_t in O(1) amortized
// ============================================================================

typedef struct {
    char     *pool;         // string data pool
    uint32_t  pool_used;
    uint32_t  pool_cap;

    uint32_t *offsets;      // offset into pool for each ID
    uint16_t *lengths;      // length of each string
    uint32_t  count;

    // Hash table for dedup (open addressing)
    uint32_t *table;        // maps hash-slot → intern ID+1 (0 = empty)
    uint32_t  table_mask;
} Intern;

static Intern g_intern;

static void intern_init(void) {
    g_intern.pool_cap  = 1 << 20;  // 1 MB
    g_intern.pool      = (char *)malloc(g_intern.pool_cap);
    g_intern.pool_used = 0;

    g_intern.offsets = (uint32_t *)malloc(sizeof(uint32_t) * INTERN_CAP);
    g_intern.lengths = (uint16_t *)malloc(sizeof(uint16_t) * INTERN_CAP);
    g_intern.count   = 0;

    uint32_t tsize = 1 << 17;  // 128K slots
    g_intern.table      = (uint32_t *)calloc(tsize, sizeof(uint32_t));
    g_intern.table_mask = tsize - 1;
}

static void intern_free(void) {
    free(g_intern.pool);
    free(g_intern.offsets);
    free(g_intern.lengths);
    free(g_intern.table);
}

// FNV-1a hash
static inline uint32_t hash_str(const char *s, uint32_t len) {
    uint32_t h = 2166136261u;
    for (uint32_t i = 0; i < len; i++) {
        h ^= (uint8_t)s[i];
        h *= 16777619u;
    }
    return h;
}

// Hash an integer key (for key lookup in HAMT)
static inline uint32_t hash_int(uint32_t x) {
    // Murmurhash3 finalizer
    x ^= x >> 16;
    x *= 0x85ebca6b;
    x ^= x >> 13;
    x *= 0xc2b2ae35;
    x ^= x >> 16;
    return x;
}

static uint32_t intern(const char *s, uint32_t len) {
    uint32_t h = hash_str(s, len);
    uint32_t slot = h & g_intern.table_mask;

    // Linear probe
    while (true) {
        uint32_t entry = g_intern.table[slot];
        if (entry == 0) break;  // empty slot
        uint32_t id = entry - 1;
        if (g_intern.lengths[id] == len &&
            memcmp(g_intern.pool + g_intern.offsets[id], s, len) == 0) {
            return id;  // already interned
        }
        slot = (slot + 1) & g_intern.table_mask;
    }

    // New entry
    uint32_t id = g_intern.count++;
    g_intern.offsets[id] = g_intern.pool_used;
    g_intern.lengths[id] = (uint16_t)len;
    memcpy(g_intern.pool + g_intern.pool_used, s, len);
    g_intern.pool_used += len;

    g_intern.table[slot] = id + 1;
    return id;
}

// ============================================================================
// PMAP — Persistent Hash-Array Mapped Trie
// ============================================================================
//
// Design follows Clojure's PersistentHashMap:
//   - Bitmap-indexed nodes: compact, cache-friendly
//   - Path-copying for persistence (shared structure = cheap snapshots)
//   - Small-map optimization for ≤16 keys (flat linear scan)
//
// Key difference from PHP coll.php:
//   - PHP: each node = PHP array (HashTable, ~120 byte overhead)
//   - C: each node = 4-byte header + n*4-byte children, arena-allocated
//
// Expected speedup: 10-30x over PHP
// ============================================================================

static PMap pmap_empty(void) {
    return (PMap){0, NIL, true};
}

// --- Small map operations ---

static bool small_get(uint32_t sm_idx, uint32_t key, int64_t *out) {
    SmallMap *sm = &g_arena.smalls[sm_idx];
    for (uint32_t i = 0; i < sm->count; i++) {
        if (sm->keys[i] == key) {
            *out = sm->values[i];
            return true;
        }
    }
    return false;
}

// Clone a small map and insert/update a key. Returns new small map index.
static uint32_t small_put(uint32_t sm_idx, uint32_t key, int64_t val, bool *added) {
    SmallMap *old = &g_arena.smalls[sm_idx];
    uint32_t new_idx = alloc_small();
    SmallMap *nw = &g_arena.smalls[new_idx];
    memcpy(nw, old, sizeof(SmallMap));

    for (uint32_t i = 0; i < nw->count; i++) {
        if (nw->keys[i] == key) {
            nw->values[i] = val;
            *added = false;
            return new_idx;
        }
    }
    nw->keys[nw->count]   = key;
    nw->values[nw->count]  = val;
    nw->count++;
    *added = true;
    return new_idx;
}

// --- HAMT node operations ---

// Create a single-leaf node: bitmap has one bit set, one child (the leaf).
static uint32_t hamt_leaf_node(uint32_t hash, uint32_t shift, uint32_t leaf_idx) {
    uint32_t bit = 1u << ((hash >> shift) & HAMT_MASK);
    uint32_t off = alloc_node(1);
    Node *n = get_node(off);
    n->bitmap = bit;
    get_children(off)[0] = leaf_idx;  // leaf index (no TAG_NODE)
    return off;
}

// Merge two leaves that collide at the current level.
// Creates trie nodes until hashes diverge (or collision node at max depth).
static uint32_t hamt_merge_leaves(uint32_t shift,
                                  uint32_t h1, uint32_t l1,
                                  uint32_t h2, uint32_t l2) {
    if (shift >= 30) {
        // Hash collision: store both leaves in a 2-child node with same bit
        // (We use bitmap=0xFFFFFFFF as collision sentinel, or just store both)
        // Simplification: create node with two children at positions
        uint32_t off = alloc_node(2);
        Node *n = get_node(off);
        n->bitmap = 0;  // collision marker (bitmap=0, but has children)
        uint32_t *ch = get_children(off);
        ch[0] = l1;
        ch[1] = l2;
        return off;
    }

    uint32_t b1 = (h1 >> shift) & HAMT_MASK;
    uint32_t b2 = (h2 >> shift) & HAMT_MASK;

    if (b1 == b2) {
        // Same slot — recurse deeper
        uint32_t child = hamt_merge_leaves(shift + HAMT_BITS, h1, l1, h2, l2);
        uint32_t off = alloc_node(1);
        Node *n = get_node(off);
        n->bitmap = 1u << b1;
        get_children(off)[0] = child | TAG_NODE;
        return off;
    }

    // Different slots — create node with two entries
    uint32_t off = alloc_node(2);
    Node *n = get_node(off);
    n->bitmap = (1u << b1) | (1u << b2);
    uint32_t *ch = get_children(off);
    // Children must be in bitmap order (lower bit first)
    if (b1 < b2) {
        ch[0] = l1;
        ch[1] = l2;
    } else {
        ch[0] = l2;
        ch[1] = l1;
    }
    return off;
}

// Recursive put into HAMT node. Returns new node offset.
static uint32_t hamt_put(uint32_t node_off, uint32_t shift,
                         uint32_t hash, uint32_t key, int64_t val,
                         bool *added) {
    Node *n = get_node(node_off);

    // Collision node (bitmap == 0)
    if (n->bitmap == 0) {
        uint32_t *ch = get_children(node_off);
        // Linear scan for matching key
        // Count children by scanning (we store 2 for collisions typically)
        // For simplicity, handle 2-entry collision
        for (int i = 0; i < 2; i++) {
            Leaf *lf = &g_arena.leaves[ch[i]];
            if (lf->key == key) {
                // Update existing
                uint32_t new_leaf = alloc_leaf(key, val);
                uint32_t new_off = alloc_node(2);
                Node *nn = get_node(new_off);
                nn->bitmap = 0;  // still collision
                uint32_t *nc = get_children(new_off);
                nc[0] = (i == 0) ? new_leaf : ch[0];
                nc[1] = (i == 1) ? new_leaf : ch[1];
                *added = false;
                return new_off;
            }
        }
        // Add third entry — promote to trie if possible
        // For prototype, just add leaf
        *added = true;
        uint32_t new_leaf = alloc_leaf(key, val);
        uint32_t new_off = alloc_node(3);
        Node *nn = get_node(new_off);
        nn->bitmap = 0;
        uint32_t *nc = get_children(new_off);
        nc[0] = ch[0];
        nc[1] = ch[1];
        nc[2] = new_leaf;
        return new_off;
    }

    uint32_t bit = 1u << ((hash >> shift) & HAMT_MASK);
    uint32_t idx = POPCOUNT(n->bitmap & (bit - 1));
    uint32_t cnt = POPCOUNT(n->bitmap);
    uint32_t *ch = get_children(node_off);

    if (!(n->bitmap & bit)) {
        // Empty slot — add leaf
        *added = true;
        uint32_t leaf = alloc_leaf(key, val);
        uint32_t new_off = alloc_node(cnt + 1);
        Node *nn = get_node(new_off);
        nn->bitmap = n->bitmap | bit;
        uint32_t *nc = get_children(new_off);
        // Copy entries before insertion point
        memcpy(nc, ch, sizeof(uint32_t) * idx);
        nc[idx] = leaf;
        // Copy entries after insertion point
        memcpy(nc + idx + 1, ch + idx, sizeof(uint32_t) * (cnt - idx));
        return new_off;
    }

    // Slot occupied
    uint32_t child = ch[idx];

    if (IS_NODE(child)) {
        // Sub-node — recurse
        uint32_t new_child = hamt_put(IDX(child), shift + HAMT_BITS,
                                      hash, key, val, added);
        // Path-copy this node with updated child
        uint32_t new_off = alloc_node(cnt);
        Node *nn = get_node(new_off);
        nn->bitmap = n->bitmap;
        uint32_t *nc = get_children(new_off);
        memcpy(nc, ch, sizeof(uint32_t) * cnt);
        nc[idx] = new_child | TAG_NODE;
        return new_off;
    }

    // Leaf — check if same key
    Leaf *lf = &g_arena.leaves[child];
    if (lf->key == key) {
        // Update value — create new leaf, path-copy node
        if (lf->value == val) {
            *added = false;
            return node_off;  // identity: no change
        }
        uint32_t new_leaf = alloc_leaf(key, val);
        uint32_t new_off = alloc_node(cnt);
        Node *nn = get_node(new_off);
        nn->bitmap = n->bitmap;
        uint32_t *nc = get_children(new_off);
        memcpy(nc, ch, sizeof(uint32_t) * cnt);
        nc[idx] = new_leaf;
        *added = false;
        return new_off;
    }

    // Different key at same slot — split into sub-node
    *added = true;
    uint32_t h2 = hash_int(lf->key);
    uint32_t new_leaf = alloc_leaf(key, val);
    uint32_t sub = hamt_merge_leaves(shift + HAMT_BITS, h2, child, hash, new_leaf);

    uint32_t new_off = alloc_node(cnt);
    Node *nn = get_node(new_off);
    nn->bitmap = n->bitmap;
    uint32_t *nc = get_children(new_off);
    memcpy(nc, ch, sizeof(uint32_t) * cnt);
    nc[idx] = sub | TAG_NODE;
    return new_off;
}

// Recursive get from HAMT node.
static bool hamt_get(uint32_t node_off, uint32_t shift,
                     uint32_t hash, uint32_t key, int64_t *out) {
    Node *n = get_node(node_off);

    // Collision node
    if (n->bitmap == 0) {
        uint32_t *ch = get_children(node_off);
        // Linear scan (collision nodes are rare and small)
        for (int i = 0; i < 3; i++) {
            if (ch[i] == 0 && i > 0) break;
            Leaf *lf = &g_arena.leaves[ch[i]];
            if (lf->key == key) {
                *out = lf->value;
                return true;
            }
        }
        return false;
    }

    uint32_t bit = 1u << ((hash >> shift) & HAMT_MASK);
    if (!(n->bitmap & bit)) return false;

    uint32_t idx = POPCOUNT(n->bitmap & (bit - 1));
    uint32_t child = get_children(node_off)[idx];

    if (IS_NODE(child)) {
        return hamt_get(IDX(child), shift + HAMT_BITS, hash, key, out);
    }

    // Leaf
    Leaf *lf = &g_arena.leaves[child];
    if (lf->key == key) {
        *out = lf->value;
        return true;
    }
    return false;
}

// --- Public pmap API ---

static PMap pmap_put(PMap m, uint32_t key, int64_t val) {
    bool added = false;

    if (m.count == 0) {
        // Empty → first small map
        uint32_t sm = alloc_small();
        g_arena.smalls[sm].keys[0] = key;
        g_arena.smalls[sm].values[0] = val;
        g_arena.smalls[sm].count = 1;
        return (PMap){1, sm, true};
    }

    if (m.is_small) {
        SmallMap *old = &g_arena.smalls[m.root];
        // Check for update
        for (uint32_t i = 0; i < old->count; i++) {
            if (old->keys[i] == key) {
                if (old->values[i] == val) return m;  // identity
                uint32_t new_sm = small_put(m.root, key, val, &added);
                return (PMap){m.count, new_sm, true};
            }
        }
        // New key
        if (old->count < SMALL_MAP_MAX) {
            uint32_t new_sm = small_put(m.root, key, val, &added);
            return (PMap){m.count + 1, new_sm, true};
        }
        // Promote to HAMT
        uint32_t first_leaf = alloc_leaf(old->keys[0], old->values[0]);
        uint32_t root = hamt_leaf_node(hash_int(old->keys[0]), 0, first_leaf);
        for (uint32_t i = 1; i < old->count; i++) {
            root = hamt_put(root, 0, hash_int(old->keys[i]),
                           old->keys[i], old->values[i], &added);
        }
        // Now add the new key
        added = false;
        root = hamt_put(root, 0, hash_int(key), key, val, &added);
        return (PMap){m.count + (added ? 1 : 0), root, false};
    }

    // HAMT put
    uint32_t new_root = hamt_put(m.root, 0, hash_int(key), key, val, &added);
    return (PMap){m.count + (added ? 1 : 0), new_root, false};
}

static bool pmap_get(PMap m, uint32_t key, int64_t *out) {
    if (m.count == 0) return false;
    if (m.is_small) return small_get(m.root, key, out);
    return hamt_get(m.root, 0, hash_int(key), key, out);
}

static uint32_t pmap_count(PMap m) {
    return m.count;
}

// ============================================================================
// PVEC — Persistent Vector (32-way trie with tail)
// ============================================================================
//
// Same design as coll.php and Clojure's PersistentVector:
//   [count, shift, root, tail, tailLen]
//
// Tail optimization: last ≤32 elements in a flat array (no trie walk).
// Structural sharing: only the path from root to modified leaf is copied.
//
// PHP stores each VNode as a PHP array (~120+ bytes overhead).
// C: VNode = 128 bytes (32 × 4-byte indices). Exactly 2 cache lines.
// ============================================================================

static PVec pvec_empty(void) {
    return (PVec){0, VEC_BITS, NIL, NIL, 0};
}

static int64_t pvec_get(PVec v, uint32_t i) {
    if (i >= v.count) return -1;

    // In tail?
    uint32_t tail_off = v.count - v.tail_len;
    if (i >= tail_off) {
        return (int64_t)g_arena.vnodes[v.tail].children[i - tail_off];
    }

    // Walk trie
    VNode *node = &g_arena.vnodes[v.root];
    for (uint32_t level = v.shift; level > 0; level -= VEC_BITS) {
        node = &g_arena.vnodes[node->children[(i >> level) & VEC_MASK]];
    }
    return (int64_t)node->children[i & VEC_MASK];
}

// Create a new path from shift level down to leaf containing `tail`.
static uint32_t pvec_new_path(uint32_t shift, uint32_t tail) {
    if (shift == 0) return tail;
    uint32_t nid = alloc_vnode();
    g_arena.vnodes[nid].children[0] = pvec_new_path(shift - VEC_BITS, tail);
    return nid;
}

// Push tail into trie, return new root.
static uint32_t pvec_push_tail(uint32_t count, uint32_t shift,
                               uint32_t parent, uint32_t tail) {
    uint32_t sub_idx = ((count - 1) >> shift) & VEC_MASK;
    // Clone parent
    uint32_t nid = alloc_vnode();
    memcpy(&g_arena.vnodes[nid], &g_arena.vnodes[parent], sizeof(VNode));

    if (shift == VEC_BITS) {
        g_arena.vnodes[nid].children[sub_idx] = tail;
    } else if (g_arena.vnodes[parent].children[sub_idx] != NIL) {
        g_arena.vnodes[nid].children[sub_idx] =
            pvec_push_tail(count, shift - VEC_BITS,
                          g_arena.vnodes[parent].children[sub_idx], tail);
    } else {
        g_arena.vnodes[nid].children[sub_idx] =
            pvec_new_path(shift - VEC_BITS, tail);
    }
    return nid;
}

static PVec pvec_append(PVec v, int64_t val) {
    uint32_t n = v.count;

    if (v.tail_len < VEC_WIDTH) {
        // Tail has room — clone tail and append
        uint32_t new_tail;
        if (v.tail == NIL) {
            new_tail = alloc_vnode();
        } else {
            new_tail = alloc_vnode();
            memcpy(&g_arena.vnodes[new_tail], &g_arena.vnodes[v.tail], sizeof(VNode));
        }
        g_arena.vnodes[new_tail].children[v.tail_len] = (uint32_t)val;
        return (PVec){n + 1, v.shift, v.root, new_tail, v.tail_len + 1};
    }

    // Tail full — push into trie, start new tail
    uint32_t new_tail = alloc_vnode();
    g_arena.vnodes[new_tail].children[0] = (uint32_t)val;

    if (v.root == NIL) {
        // First push: create root branch with old tail as child[0]
        uint32_t new_root = alloc_vnode();
        g_arena.vnodes[new_root].children[0] = v.tail;
        return (PVec){n + 1, VEC_BITS, new_root, new_tail, 1};
    }

    uint32_t shift = v.shift;
    uint32_t new_root;

    if ((n >> VEC_BITS) > (1u << shift)) {
        // Trie overflow — grow root
        new_root = alloc_vnode();
        g_arena.vnodes[new_root].children[0] = v.root;
        g_arena.vnodes[new_root].children[1] = pvec_new_path(shift, v.tail);
        return (PVec){n + 1, shift + VEC_BITS, new_root, new_tail, 1};
    }

    new_root = pvec_push_tail(n, shift, v.root, v.tail);
    return (PVec){n + 1, shift, new_root, new_tail, 1};
}

static uint32_t pvec_count(PVec v) {
    return v.count;
}

// ============================================================================
// ATOM — mutable reference with version tracking
// ============================================================================

static Atom atom_new(int64_t val) {
    return (Atom){0, val};
}

static int64_t atom_deref(Atom *a) {
    return a->value;
}

static int64_t atom_reset(Atom *a, int64_t val) {
    a->value = val;
    a->version++;
    return val;
}

static bool atom_cas(Atom *a, int64_t expected, int64_t new_val) {
    if (a->value == expected) {
        a->value = new_val;
        a->version++;
        return true;
    }
    return false;
}

// ============================================================================
// Benchmarking
// ============================================================================

static inline uint64_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

// Prevent compiler from optimizing away the result
static volatile int64_t g_sink;
#define SINK(x) (g_sink = (int64_t)(x))

static void bench_pmap_small(void) {
    printf("\n--- pmap (small, ≤16 keys) ---\n");

    // Build a map with 10 keys
    arena_reset();
    PMap m = pmap_empty();
    uint32_t keys[10];
    for (int i = 0; i < 10; i++) {
        char buf[16];
        int len = snprintf(buf, sizeof(buf), "key_%d", i);
        keys[i] = intern(buf, len);
        m = pmap_put(m, keys[i], i * 100);
    }
    printf("  count: %u (is_small: %s)\n", pmap_count(m), m.is_small ? "yes" : "no");

    // Bench get (read-only, no arena growth)
    int64_t val;
    int N = 10000000;
    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        pmap_get(m, keys[i % 10], &val);
        SINK(val);
    }
    uint64_t dt = now_ns() - t0;
    printf("  get:   %6.1f ns/op  (PHP: 381 ns, PHP array: 100 ns)\n",
           (double)dt / N);

    // Bench put (batched: rebuild base map + measure puts per batch)
    int BATCH = 10000;
    int BATCHES = 1000;
    uint64_t total_ns = 0;
    int total_ops = 0;
    for (int b = 0; b < BATCHES; b++) {
        arena_reset();
        m = pmap_empty();
        for (int i = 0; i < 10; i++) m = pmap_put(m, keys[i], i * 100);

        t0 = now_ns();
        for (int i = 0; i < BATCH; i++) {
            PMap m2 = pmap_put(m, keys[i % 10], i);
            SINK(m2.count);
        }
        total_ns += now_ns() - t0;
        total_ops += BATCH;
    }
    printf("  put:   %6.1f ns/op  (PHP: 2500 ns, PHP array: 426 ns)\n",
           (double)total_ns / total_ops);
}

static void bench_pmap_hamt(void) {
    printf("\n--- pmap (HAMT, 100 keys) ---\n");

    arena_reset();
    PMap m = pmap_empty();
    uint32_t keys[100];
    for (int i = 0; i < 100; i++) {
        char buf[16];
        int len = snprintf(buf, sizeof(buf), "key_%03d", i);
        keys[i] = intern(buf, len);
        m = pmap_put(m, keys[i], i * 100);
    }
    printf("  count: %u (is_small: %s)\n", pmap_count(m), m.is_small ? "yes" : "no");

    // Bench get (read-only, no arena growth)
    int64_t val;
    int N = 10000000;
    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        pmap_get(m, keys[i % 100], &val);
        SINK(val);
    }
    uint64_t dt = now_ns() - t0;
    printf("  get:   %6.1f ns/op  (PHP: 381 ns)\n", (double)dt / N);

    // Bench put (batched to avoid arena exhaustion)
    int BATCH = 5000;
    int BATCHES = 1000;
    uint64_t total_ns = 0;
    int total_ops = 0;
    for (int b = 0; b < BATCHES; b++) {
        arena_reset();
        PMap mb = pmap_empty();
        for (int i = 0; i < 100; i++) mb = pmap_put(mb, keys[i], i * 100);

        t0 = now_ns();
        for (int i = 0; i < BATCH; i++) {
            PMap m2 = pmap_put(mb, keys[i % 100], i);
            SINK(m2.count);
        }
        total_ns += now_ns() - t0;
        total_ops += BATCH;
    }
    printf("  put:   %6.1f ns/op  (PHP: 2500 ns)\n",
           (double)total_ns / total_ops);
}

static void bench_pmap_hamt_large(void) {
    printf("\n--- pmap (HAMT, 1000 keys) ---\n");

    arena_reset();
    PMap m = pmap_empty();
    uint32_t keys[1000];
    for (int i = 0; i < 1000; i++) {
        char buf[16];
        int len = snprintf(buf, sizeof(buf), "key_%04d", i);
        keys[i] = intern(buf, len);
        m = pmap_put(m, keys[i], i * 100);
    }
    printf("  count: %u\n", pmap_count(m));

    // Bench get (read-only, no arena growth)
    int64_t val;
    int N = 5000000;
    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        pmap_get(m, keys[i % 1000], &val);
        SINK(val);
    }
    uint64_t dt = now_ns() - t0;
    printf("  get:   %6.1f ns/op  (PHP: 381 ns)\n", (double)dt / N);

    // Bench put (batched)
    int BATCH = 2000;
    int BATCHES = 500;
    uint64_t total_ns = 0;
    int total_ops = 0;
    for (int b = 0; b < BATCHES; b++) {
        arena_reset();
        PMap mb = pmap_empty();
        for (int i = 0; i < 1000; i++) mb = pmap_put(mb, keys[i], i * 100);

        t0 = now_ns();
        for (int i = 0; i < BATCH; i++) {
            PMap m2 = pmap_put(mb, keys[i % 1000], i);
            SINK(m2.count);
        }
        total_ns += now_ns() - t0;
        total_ops += BATCH;
    }
    printf("  put:   %6.1f ns/op  (PHP: 2500 ns)\n",
           (double)total_ns / total_ops);
}

static void bench_pvec(void) {
    printf("\n--- pvec ---\n");

    // Bench append — build a 100K vec (uses ~3200 vnodes)
    arena_reset();
    int N = 100000;
    uint64_t t0 = now_ns();
    PVec v = pvec_empty();
    for (int i = 0; i < N; i++) {
        v = pvec_append(v, i);
    }
    uint64_t dt = now_ns() - t0;
    printf("  append (100K): %6.1f ns/op  count=%u  vnodes=%u\n",
           (double)dt / N, pvec_count(v), g_arena.vnode_count);

    // Bench get (random access)
    int M = 5000000;
    t0 = now_ns();
    for (int i = 0; i < M; i++) {
        int64_t val = pvec_get(v, i % N);
        SINK(val);
    }
    dt = now_ns() - t0;
    printf("  get (random):  %6.1f ns/op\n", (double)dt / M);

    // Bench get (tail-hot — last 32 elements, always in tail)
    t0 = now_ns();
    for (int i = 0; i < M; i++) {
        int64_t val = pvec_get(v, N - 1 - (i % 32));
        SINK(val);
    }
    dt = now_ns() - t0;
    printf("  get (tail):    %6.1f ns/op\n", (double)dt / M);
}

static void bench_pvec_small(void) {
    printf("\n--- pvec (small, 100 elements) ---\n");

    arena_reset();
    PVec v = pvec_empty();
    for (int i = 0; i < 100; i++) v = pvec_append(v, i);

    // Bench get (read-only)
    int N = 10000000;
    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        int64_t val = pvec_get(v, i % 100);
        SINK(val);
    }
    uint64_t dt = now_ns() - t0;
    printf("  get:    %6.1f ns/op\n", (double)dt / N);

    // Bench append (batched — each append allocates a vnode)
    int BATCH = 10000;
    int BATCHES = 1000;
    uint64_t total_ns = 0;
    int total_ops = 0;
    for (int b = 0; b < BATCHES; b++) {
        arena_reset();
        PVec vb = pvec_empty();
        for (int i = 0; i < 100; i++) vb = pvec_append(vb, i);

        t0 = now_ns();
        for (int i = 0; i < BATCH; i++) {
            PVec v2 = pvec_append(vb, i);
            SINK(v2.count);
        }
        total_ns += now_ns() - t0;
        total_ops += BATCH;
    }
    printf("  append: %6.1f ns/op\n", (double)total_ns / total_ops);
}

static void bench_atom(void) {
    printf("\n--- atom ---\n");

    int N = 100000000;
    Atom a = atom_new(0);

    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        atom_reset(&a, i);
    }
    uint64_t dt = now_ns() - t0;
    printf("  reset:  %6.2f ns/op  version=%u\n", (double)dt / N, a.version);

    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        int64_t v = atom_deref(&a);
        SINK(v);
    }
    dt = now_ns() - t0;
    printf("  deref:  %6.2f ns/op\n", (double)dt / N);

    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        atom_cas(&a, a.value, i);
    }
    dt = now_ns() - t0;
    printf("  cas:    %6.2f ns/op\n", (double)dt / N);
}

static void bench_snapshots(void) {
    printf("\n--- snapshot memory (100 snapshots of 500-entity map) ---\n");

    arena_reset();
    uint32_t keys[500];
    for (int i = 0; i < 500; i++) {
        char buf[16];
        int len = snprintf(buf, sizeof(buf), "ent_%04d", i);
        keys[i] = intern(buf, len);
    }

    // Build initial map with 500 entities
    PMap m = pmap_empty();
    for (int i = 0; i < 500; i++) {
        m = pmap_put(m, keys[i], i);
    }

    uint32_t arena_before = g_arena.node_used + g_arena.leaf_count * sizeof(Leaf);

    // Take 100 snapshots, each modifying one key
    PMap snaps[100];
    for (int i = 0; i < 100; i++) {
        m = pmap_put(m, keys[i % 500], 1000 + i);
        snaps[i] = m;  // snapshot = just copy the PMap struct (12 bytes!)
    }

    uint32_t arena_after = g_arena.node_used + g_arena.leaf_count * sizeof(Leaf);
    uint32_t delta = arena_after - arena_before;

    printf("  initial map:   %u bytes arena\n", arena_before);
    printf("  100 snapshots: +%u bytes  (%.1f bytes/snapshot)\n",
           delta, (double)delta / 100);
    printf("  sizeof(PMap):  %zu bytes\n", sizeof(PMap));
    printf("  (PHP: 460 KB for same workload, C: %.1f KB)\n",
           (double)(arena_before + delta) / 1024);
    SINK(snaps[99].count);
}

static void bench_intern(void) {
    printf("\n--- intern ---\n");

    int N = 10000000;
    // Intern a set of keys, then look them up
    char buf[16];
    uint32_t ids[100];
    for (int i = 0; i < 100; i++) {
        int len = snprintf(buf, sizeof(buf), "k%d", i);
        ids[i] = intern(buf, len);
    }

    uint64_t t0 = now_ns();
    for (int i = 0; i < N; i++) {
        int len = snprintf(buf, sizeof(buf), "k%d", i % 100);
        uint32_t id = intern(buf, len);
        SINK(id);
    }
    uint64_t dt = now_ns() - t0;
    printf("  lookup: %6.1f ns/op (includes snprintf)\n", (double)dt / N);

    // Pure lookup without snprintf overhead
    const char *test_keys[] = {"k0","k1","k2","k3","k4","k5","k6","k7","k8","k9"};
    uint32_t test_lens[] = {2,2,2,2,2,2,2,2,2,2};
    t0 = now_ns();
    for (int i = 0; i < N; i++) {
        uint32_t id = intern(test_keys[i % 10], test_lens[i % 10]);
        SINK(id);
    }
    dt = now_ns() - t0;
    printf("  pure:   %6.1f ns/op\n", (double)dt / N);
}

// ============================================================================
// Correctness tests
// ============================================================================

static void test_pmap(void) {
    printf("\n=== correctness: pmap ===\n");
    arena_reset();

    PMap m = pmap_empty();
    uint32_t k1 = intern("hello", 5);
    uint32_t k2 = intern("world", 5);
    uint32_t k3 = intern("foo", 3);

    m = pmap_put(m, k1, 42);
    m = pmap_put(m, k2, 99);
    m = pmap_put(m, k3, 7);

    int64_t val;
    printf("  get(hello)=%s ", pmap_get(m, k1, &val) && val == 42 ? "OK" : "FAIL");
    printf("  get(world)=%s ", pmap_get(m, k2, &val) && val == 99 ? "OK" : "FAIL");
    printf("  get(foo)=%s ",   pmap_get(m, k3, &val) && val == 7  ? "OK" : "FAIL");
    printf("  count=%s\n",     pmap_count(m) == 3 ? "OK" : "FAIL");

    // Persistence: old snapshot still works
    PMap m2 = pmap_put(m, k1, 100);
    printf("  m.hello=%s ",    pmap_get(m, k1, &val) && val == 42  ? "OK" : "FAIL");
    printf("  m2.hello=%s\n",  pmap_get(m2, k1, &val) && val == 100 ? "OK" : "FAIL");

    // HAMT promotion (>16 keys)
    PMap big = pmap_empty();
    uint32_t bkeys[50];
    for (int i = 0; i < 50; i++) {
        char buf[16];
        int len = snprintf(buf, sizeof(buf), "big_%02d", i);
        bkeys[i] = intern(buf, len);
        big = pmap_put(big, bkeys[i], i);
    }
    printf("  big.count=%s ",  pmap_count(big) == 50 ? "OK" : "FAIL");
    bool all_ok = true;
    for (int i = 0; i < 50; i++) {
        if (!pmap_get(big, bkeys[i], &val) || val != i) { all_ok = false; break; }
    }
    printf("  big.all_get=%s\n", all_ok ? "OK" : "FAIL");
}

static void test_pvec(void) {
    printf("\n=== correctness: pvec ===\n");
    arena_reset();

    PVec v = pvec_empty();
    for (int i = 0; i < 100; i++) {
        v = pvec_append(v, i * 10);
    }
    printf("  count=%s ",  pvec_count(v) == 100 ? "OK" : "FAIL");
    printf("  get(0)=%s ", pvec_get(v, 0) == 0 ? "OK" : "FAIL");
    printf("  get(50)=%s ", pvec_get(v, 50) == 500 ? "OK" : "FAIL");
    printf("  get(99)=%s\n", pvec_get(v, 99) == 990 ? "OK" : "FAIL");

    // Large vector (beyond single tail)
    PVec big = pvec_empty();
    for (int i = 0; i < 10000; i++) {
        big = pvec_append(big, i);
    }
    printf("  big.count=%s ", pvec_count(big) == 10000 ? "OK" : "FAIL");
    bool ok = true;
    for (int i = 0; i < 10000; i++) {
        if (pvec_get(big, i) != i) { ok = false; printf("  FAIL at %d (got %ld)\n", i, pvec_get(big, i)); break; }
    }
    printf("  big.all=%s\n", ok ? "OK" : "FAIL");
}

static void test_atom(void) {
    printf("\n=== correctness: atom ===\n");

    Atom a = atom_new(42);
    printf("  deref=%s ", atom_deref(&a) == 42 ? "OK" : "FAIL");
    atom_reset(&a, 99);
    printf("  reset=%s ", atom_deref(&a) == 99 ? "OK" : "FAIL");
    printf("  cas(99→100)=%s ", atom_cas(&a, 99, 100) ? "OK" : "FAIL");
    printf("  cas(99→200)=%s ", !atom_cas(&a, 99, 200) ? "OK" : "FAIL");
    printf("  version=%s\n", a.version == 2 ? "OK" : "FAIL");
}

// ============================================================================
// Main
// ============================================================================

int main(void) {
    printf("proto_coll.c — C prototype of pmap/pvec/atom\n");
    printf("============================================\n");
    printf("sizeof(Node):     %zu bytes\n", sizeof(Node));
    printf("sizeof(Leaf):     %zu bytes\n", sizeof(Leaf));
    printf("sizeof(SmallMap): %zu bytes\n", sizeof(SmallMap));
    printf("sizeof(VNode):    %zu bytes (= %zu cache lines)\n",
           sizeof(VNode), sizeof(VNode) / 64);
    printf("sizeof(PMap):     %zu bytes\n", sizeof(PMap));
    printf("sizeof(PVec):     %zu bytes\n", sizeof(PVec));
    printf("sizeof(Atom):     %zu bytes\n", sizeof(Atom));

    arena_init();
    intern_init();

    // Correctness
    test_pmap();
    test_pvec();
    test_atom();

    // Benchmarks
    bench_intern();
    bench_pmap_small();
    bench_pmap_hamt();
    bench_pmap_hamt_large();
    bench_pvec_small();
    bench_pvec();
    bench_atom();
    bench_snapshots();

    printf("\n============================================\n");
    printf("Summary vs PHP coll.php:\n");
    printf("  PHP pmap get:  381 ns    (crc32 + PHP HashTable walk)\n");
    printf("  PHP pmap put:  2500 ns   (COW HashTable + zval alloc)\n");
    printf("  PHP array get: 100 ns    (native HashTable)\n");
    printf("  PHP array put: 426 ns    (COW copy)\n");
    printf("  C targets:     get ~20-50ns, put ~50-150ns\n");
    printf("  Speedup:       ~10-50x (varies by operation and map size)\n");
    printf("\n");
    printf("Key factors:\n");
    printf("  1. Arena alloc vs PHP zval/HashTable (~120 byte overhead per node)\n");
    printf("  2. Bitmap popcount vs sparse array (cache-dense vs cache-sparse)\n");
    printf("  3. Interned keys: int compare vs string compare\n");
    printf("  4. No refcount/GC overhead\n");
    printf("  5. 32-bit indices vs 64-bit pointers\n");

    intern_free();
    arena_free();
    return 0;
}
