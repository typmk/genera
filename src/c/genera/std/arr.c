/**
 * arr.c — DynArray + HashMap (arena-backed containers)
 *
 * ZERO-INIT: T *arr = NULL = empty. HashMap {0} = empty.
 * Depends on: sys.c, fmt.c, mem.c, val.c
 */
#ifndef ARR_C_INCLUDED
#define ARR_C_INCLUDED

// ============================================================================
// 1. DynArray — Arena-backed stretchy buffer
// ============================================================================

typedef struct { Arena *arena; u32 count, cap; } ArrHdr;

#define ARR_HDR(a) ((ArrHdr *)((u8 *)(a) - sizeof(ArrHdr)))
#define arr_count(a) ((a) ? ARR_HDR(a)->count : 0)

#define arr__grow(a, arena_ptr) do {                                    \
    u32 old_cap = (a) ? ARR_HDR(a)->cap : 0;                           \
    u32 new_cap = old_cap ? old_cap * 2 : 8;                           \
    u32 elem_sz = sizeof(*(a));                                         \
    u8 *mem = (u8 *)arena_alloc((arena_ptr),                            \
        sizeof(ArrHdr) + elem_sz * new_cap, 8);                        \
    ArrHdr *hdr = (ArrHdr *)mem;                                        \
    hdr->arena = (arena_ptr); hdr->cap = new_cap;                      \
    if (a) { hdr->count = ARR_HDR(a)->count;                           \
             memcpy(mem + sizeof(ArrHdr), (a), elem_sz * hdr->count);  \
    } else { hdr->count = 0; }                                         \
    (a) = (void *)(mem + sizeof(ArrHdr));                               \
} while(0)

#define arr_push(a, val, arena_ptr) do {                                \
    if (!(a) || ARR_HDR(a)->count >= ARR_HDR(a)->cap)                   \
        arr__grow(a, arena_ptr);                                        \
    (a)[ARR_HDR(a)->count++] = (val);                                   \
} while(0)

#define arr_pop(a)   ((a)[--ARR_HDR(a)->count])
#define arr_last(a)  ((a)[ARR_HDR(a)->count - 1])
#define arr_clear(a) do { if (a) ARR_HDR(a)->count = 0; } while(0)

// ============================================================================
// 2. HashMap — Open-addressing, SoA, arena-backed
// ============================================================================

#define MAP_EMPTY     UINT32_MAX
#define MAP_TOMBSTONE (UINT32_MAX - 1)

typedef struct {
    u32 *keys; Val *vals;
    u32 cap, count, mask;
    Arena *arena;
} HashMap;

static HashMap hashmap_create(Arena *arena, u32 initial_cap) {
    u32 cap = initial_cap < 16 ? 16 : initial_cap;
    cap--; cap |= cap>>1; cap |= cap>>2; cap |= cap>>4; cap |= cap>>8; cap |= cap>>16; cap++;
    HashMap m = {0}; m.arena = arena; m.cap = cap; m.mask = cap - 1;
    m.keys = arena_push_n(arena, u32, cap);
    m.vals = arena_push_n(arena, Val, cap);
    memset(m.keys, 0xFF, sizeof(u32) * cap);
    return m;
}

ALWAYS_INLINE bool hashmap_get(const HashMap *m, u32 key, Val *out) {
    if (!m->keys) return false;  // zero-init: empty
    u32 slot = hash32(key) & m->mask;
    __builtin_prefetch(&m->vals[slot], 0, 3);
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY) return false;
        if (k == key) { *out = m->vals[slot]; return true; }
        slot = (slot + 1) & m->mask;
    }
}

static void hashmap_grow(HashMap *m);

// Auto-inits on first put if arena is set.
static void hashmap_put(HashMap *m, u32 key, Val val) {
    if (!m->keys) {
        u32 cap = 16;
        m->cap = cap; m->mask = cap - 1; m->count = 0;
        m->keys = arena_push_n(m->arena, u32, cap);
        m->vals = arena_push_n(m->arena, Val, cap);
        memset(m->keys, 0xFF, sizeof(u32) * cap);
    }
    if (m->count * 4 >= m->cap * 3) hashmap_grow(m);
    u32 slot = hash32(key) & m->mask;
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY || k == MAP_TOMBSTONE) {
            m->keys[slot] = key; m->vals[slot] = val; m->count++; return;
        }
        if (k == key) { m->vals[slot] = val; return; }
        slot = (slot + 1) & m->mask;
    }
}

static bool hashmap_del(HashMap *m, u32 key) {
    if (!m->keys) return false;
    u32 slot = hash32(key) & m->mask;
    while (1) {
        u32 k = m->keys[slot];
        if (k == MAP_EMPTY) return false;
        if (k == key) { m->keys[slot] = MAP_TOMBSTONE; m->count--; return true; }
        slot = (slot + 1) & m->mask;
    }
}

static void hashmap_grow(HashMap *m) {
    u32 old_cap = m->cap; u32 *old_keys = m->keys; Val *old_vals = m->vals;
    m->cap *= 2; m->mask = m->cap - 1; m->count = 0;
    m->keys = arena_push_n(m->arena, u32, m->cap);
    m->vals = arena_push_n(m->arena, Val, m->cap);
    memset(m->keys, 0xFF, sizeof(u32) * m->cap);
    for (u32 i = 0; i < old_cap; i++)
        if (old_keys[i] != MAP_EMPTY && old_keys[i] != MAP_TOMBSTONE)
            hashmap_put(m, old_keys[i], old_vals[i]);
}

#endif // ARR_C_INCLUDED
