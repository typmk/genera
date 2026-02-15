/**
 * mem.c — Arena Bump Allocator with Block Chaining
 *
 * ZERO-INIT: Arena {0} = lazy: first alloc creates a 1MB block automatically.
 * Depends on: sys.c, fmt.c (macros)
 */
#ifndef MEM_C_INCLUDED
#define MEM_C_INCLUDED

typedef struct ArenaBlock {
    struct ArenaBlock *prev;
    u32 size, used;
} ArenaBlock;

typedef struct {
    ArenaBlock *current;
    u32 default_size;
    // Always-on tracking (~0.3ns/alloc: one add + one compare)
    u32 alloc_count;     // total allocations (lifetime)
    u64 alloc_total;     // total bytes requested (lifetime)
    u64 live_bytes;      // bytes since last reset (current cycle)
    u64 high_water;      // max live_bytes ever seen (peak)
    u32 reset_count;     // how many resets (lifetime indicator)
    u32 block_count;     // total blocks allocated (lifetime)
    const char *name;    // "perm", "req", "temp" — for reporting
} Arena;
typedef struct { Arena *arena; ArenaBlock *block; u32 used; } ArenaMark;

#define BLOCK_DATA(b) ((u8 *)(b) + sizeof(ArenaBlock))

static ArenaBlock *arena_block_new(u32 size) {
    u32 total = PAGE_ALIGN(sizeof(ArenaBlock) + size);
    ArenaBlock *b = (ArenaBlock *)sys_alloc(total);
    b->prev = NULL;
    b->size = total - (u32)sizeof(ArenaBlock);
    b->used = 0;
    return b;
}

static void arena_block_free(ArenaBlock *b) {
    sys_free(b, sizeof(ArenaBlock) + b->size);
}

static Arena arena_create(u32 default_size) {
    Arena a; a.default_size = default_size;
    a.current = arena_block_new(default_size);
    return a;
}

ALWAYS_INLINE u32 arena_align_offset(ArenaBlock *b, u32 used, u32 align) {
    u64 base = (u64)BLOCK_DATA(b);
    return (u32)(ALIGN_UP(base + used, align) - base);
}

#define ARENA_DEFAULT_SIZE (1 << 20)  // 1 MB — used when Arena is {0}

NOINLINE static void *arena_alloc_slow(Arena *a, u32 size, u32 align) {
    u32 def = a->default_size ? a->default_size : ARENA_DEFAULT_SIZE;
    if (!a->default_size) a->default_size = def;
    u32 block_size = MAX(def, size + align + 64);
    ArenaBlock *b = arena_block_new(block_size);
    b->prev = a->current; a->current = b;
    a->block_count++;
    u32 off = arena_align_offset(b, b->used, align);
    b->used = off + size;
    return BLOCK_DATA(b) + off;
}

// Per-name alloc tracking — set by DISPATCH in tap.c, accumulated here.
// Lives in mem.c because arena_alloc is the hot path.
#define ALLOC_TAG_CAP 65536
static u64  g_obs_alloc[ALLOC_TAG_CAP];  // bytes allocated per name (StrId-indexed)
static u32  g_alloc_tag;                  // current dispatch name

ALWAYS_INLINE void *arena_alloc(Arena *a, u32 size, u32 align) {
    a->alloc_count++;
    a->alloc_total += size;
    a->live_bytes += size;
    if (UNLIKELY(a->live_bytes > a->high_water)) a->high_water = a->live_bytes;
    g_obs_alloc[g_alloc_tag] += size;
    ArenaBlock *b = a->current;
    if (UNLIKELY(!b)) return arena_alloc_slow(a, size, align);
    u32 off = arena_align_offset(b, b->used, align);
    u32 end = off + size;
    if (LIKELY(end <= b->size)) { b->used = end; return BLOCK_DATA(b) + off; }
    return arena_alloc_slow(a, size, align);
}

static void arena_reset(Arena *a) {
    ArenaBlock *b = a->current;
    if (!b) return;  // zero-init Arena: no-op
    while (b->prev) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    b->used = 0; a->current = b;
    a->reset_count++;
    a->live_bytes = 0;  // reset cycle — live drops to zero
}

static ArenaMark arena_begin_temp(Arena *a) {
    return (ArenaMark){a, a->current, a->current ? a->current->used : 0};
}

static void arena_end_temp(ArenaMark mark) {
    ArenaBlock *b = mark.arena->current;
    if (!b) return;  // nothing was allocated since begin_temp
    while (b != mark.block) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    mark.arena->current = mark.block;
    if (mark.block) mark.block->used = mark.used;
}

static void arena_destroy(Arena *a) {
    ArenaBlock *b = a->current;
    while (b) { ArenaBlock *prev = b->prev; arena_block_free(b); b = prev; }
    a->current = NULL;
}

#define arena_push(a, T)       ((T *)arena_alloc((a), sizeof(T), _Alignof(T)))
#define arena_push_n(a, T, n)  ((T *)arena_alloc((a), sizeof(T) * (n), _Alignof(T)))

// Arena snapshot — capture state at a point in time for delta computation
typedef struct { u32 alloc_count; u64 alloc_total; } ArenaSnap;

ALWAYS_INLINE ArenaSnap arena_snap(Arena *a) {
    return (ArenaSnap){a->alloc_count, a->alloc_total};
}

static Arena g_temp, g_req, g_perm;

#endif // MEM_C_INCLUDED
