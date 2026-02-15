/**
 * bench_mem.c — micro-prototypes for the genera allocator model
 *
 * Model: view → execute → reclaim
 *   Before:  view classifies nodes (eliminate, offset, scope, dynamic)
 *   During:  bump for what views can't eliminate
 *   After:   V_ALIVE scan roots + compact
 *
 * Build: gcc -O3 -o bench_mem parity/bench_mem.c -lm
 * Run:   ./bench_mem
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t  i64;

// ============================================================
// The Atom
// ============================================================

typedef struct { u8 *base; u32 used, cap, peak; } Mem;

static inline void *mem_alloc(Mem *m, u32 size, u32 align) {
    u32 off = (m->used + align - 1) & ~(align - 1);
    u32 end = off + size;
    if (__builtin_expect(end > m->cap, 0)) return NULL;
    m->used = end;
    if (__builtin_expect(end > m->peak, 0)) m->peak = end;
    return m->base + off;
}

#define PUSH(m, T)       ((T *)mem_alloc(m, sizeof(T), _Alignof(T)))
#define PUSH_N(m, T, n)  ((T *)mem_alloc(m, (u32)(sizeof(T)*(n)), _Alignof(T)))
#define MARK(m)          ((m)->used)
#define RESTORE(m, v)    ((m)->used = (v))

// ============================================================
// Timing — nanosecond precision
// ============================================================

static inline u64 now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (u64)ts.tv_sec * 1000000000ULL + (u64)ts.tv_nsec;
}

// Prevent dead code elimination
static volatile u64 g_sink;

// ============================================================
// Test types (matching genera's Val/Cons/Leaf)
// ============================================================

typedef u64 Val;
#define NIL 0ULL

typedef struct { Val car, cdr; } Cons;       // 16 bytes
typedef struct { u32 key; Val value; } Leaf;  // 12 bytes (+4 padding = 16)

// ============================================================
// Bench 1: Bump vs malloc — the fundamental operation
// ============================================================

static void bench_bump_vs_malloc(void) {
    printf("--- Bump vs malloc (16B allocations) ---\n");

    const u32 N = 2000000;

    // --- Bump ---
    Mem m = {0};
    m.cap = 64 << 20;
    m.base = (u8 *)malloc(m.cap);
    if (!m.base) { printf("  OOM\n"); return; }

    // Warmup
    for (u32 i = 0; i < N; i++) {
        Cons *c = PUSH(&m, Cons);
        c->car = i; c->cdr = NIL;
    }
    m.used = 0;

    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        Cons *c = PUSH(&m, Cons);
        c->car = i; c->cdr = NIL;
    }
    u64 t1 = now_ns();
    double bump_ns = (double)(t1 - t0) / N;
    g_sink = m.used;
    m.used = 0;

    // --- malloc ---
    Cons **ptrs = (Cons **)malloc(N * sizeof(Cons *));
    if (!ptrs) { free(m.base); printf("  OOM\n"); return; }

    // Warmup
    for (u32 i = 0; i < N; i++) ptrs[i] = (Cons *)malloc(sizeof(Cons));
    for (u32 i = 0; i < N; i++) free(ptrs[i]);

    t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        ptrs[i] = (Cons *)malloc(sizeof(Cons));
        ptrs[i]->car = i;
        ptrs[i]->cdr = NIL;
    }
    t1 = now_ns();
    double malloc_ns = (double)(t1 - t0) / N;
    g_sink = (u64)(uintptr_t)ptrs[N-1];

    for (u32 i = 0; i < N; i++) free(ptrs[i]);
    free(ptrs);
    free(m.base);

    printf("  bump:   %6.1f ns/alloc\n", bump_ns);
    printf("  malloc: %6.1f ns/alloc\n", malloc_ns);
    printf("  ratio:  %6.1fx faster\n\n", malloc_ns / bump_ns);
}

// ============================================================
// Bench 2: MARK/RESTORE vs malloc/free — scoped reclamation
// ============================================================

static void bench_scope(void) {
    printf("--- MARK/RESTORE vs malloc/free (100 allocs x 10K scopes) ---\n");

    const u32 SCOPES = 10000;
    const u32 PER = 100;

    // --- Bump + scope ---
    Mem m = {0};
    m.cap = 8 << 20;
    m.base = (u8 *)malloc(m.cap);
    if (!m.base) { printf("  OOM\n"); return; }

    // Warmup
    for (u32 s = 0; s < 100; s++) {
        u32 mark = MARK(&m);
        for (u32 i = 0; i < PER; i++) {
            Cons *c = PUSH(&m, Cons); c->car = i; c->cdr = NIL;
        }
        RESTORE(&m, mark);
    }

    u64 t0 = now_ns();
    for (u32 s = 0; s < SCOPES; s++) {
        u32 mark = MARK(&m);
        for (u32 i = 0; i < PER; i++) {
            Cons *c = PUSH(&m, Cons); c->car = i; c->cdr = NIL;
        }
        RESTORE(&m, mark);
    }
    u64 t1 = now_ns();
    double scope_ns = (double)(t1 - t0) / (SCOPES * PER);

    // --- malloc/free ---
    Cons *ptrs[100];

    // Warmup
    for (u32 s = 0; s < 100; s++) {
        for (u32 i = 0; i < PER; i++) ptrs[i] = (Cons *)malloc(sizeof(Cons));
        for (u32 i = 0; i < PER; i++) free(ptrs[i]);
    }

    t0 = now_ns();
    for (u32 s = 0; s < SCOPES; s++) {
        for (u32 i = 0; i < PER; i++) {
            ptrs[i] = (Cons *)malloc(sizeof(Cons));
            ptrs[i]->car = i; ptrs[i]->cdr = NIL;
        }
        for (u32 i = 0; i < PER; i++) free(ptrs[i]);
    }
    t1 = now_ns();
    double mf_ns = (double)(t1 - t0) / (SCOPES * PER);

    free(m.base);

    printf("  scope:      %6.1f ns/alloc+reclaim\n", scope_ns);
    printf("  malloc/free: %5.1f ns/alloc+free\n", mf_ns);
    printf("  ratio:      %6.1fx faster\n\n", mf_ns / scope_ns);
}

// ============================================================
// Bench 3: Commit — deep-copy cons list from step to main
// ============================================================

static void bench_commit(void) {
    printf("--- Commit: deep-copy cons list (step → main) ---\n");

    Mem step = {0}, main_p = {0};
    step.cap = 8 << 20;
    step.base = (u8 *)malloc(step.cap);
    main_p.cap = 64 << 20;
    main_p.base = (u8 *)malloc(main_p.cap);
    if (!step.base || !main_p.base) { printf("  OOM\n"); return; }

    const u32 TRIALS = 50000;
    const u32 LENS[] = {10, 50, 200};

    for (int li = 0; li < 3; li++) {
        u32 len = LENS[li];
        u32 main_mark = MARK(&main_p);

        // Build one list in step (reused across trials)
        step.used = 0;
        Cons *list_head = NULL;
        for (u32 i = 0; i < len; i++) {
            Cons *c = PUSH(&step, Cons);
            c->car = (Val)i;
            c->cdr = (Val)(uintptr_t)list_head;
            list_head = c;
        }

        u64 t0 = now_ns();
        for (u32 t = 0; t < TRIALS; t++) {
            RESTORE(&main_p, main_mark);
            // Deep-copy: walk step list, allocate in main
            Cons *src = list_head;
            Cons *dst_head = NULL;
            Cons **tail = &dst_head;
            while (src) {
                Cons *nc = PUSH(&main_p, Cons);
                nc->car = src->car;
                nc->cdr = NIL;
                *tail = nc;
                tail = (Cons **)&nc->cdr;
                src = (Cons *)(uintptr_t)src->cdr;
            }
            g_sink = (u64)(uintptr_t)dst_head;
        }
        u64 t1 = now_ns();

        printf("  len=%3u: %6.0f ns/commit (%4.1f ns/cons)\n",
               len, (double)(t1 - t0) / TRIALS,
               (double)(t1 - t0) / (TRIALS * len));
    }
    printf("\n");

    free(step.base);
    free(main_p.base);
}

// ============================================================
// Bench 4: V_ALIVE — root scanning + bitmask marking
// ============================================================

static void bench_mark_phase(void) {
    printf("--- V_ALIVE: root scan + mark ---\n");

    // Simulate: main pool with N allocations, R roots pointing in
    // Mark phase: for each root, set bit in liveness bitmap
    // Then popcount to find live count

    const u32 POOL_ALLOCS = 1 << 20;  // ~1M allocations
    const u32 BM_WORDS = (POOL_ALLOCS + 63) / 64;
    const u32 ROOT_COUNTS[] = {100, 1000, 10000};
    const u32 TRIALS = 2000;

    u64 *alive = (u64 *)calloc(BM_WORDS, sizeof(u64));
    if (!alive) { printf("  OOM\n"); return; }

    for (int ri = 0; ri < 3; ri++) {
        u32 nroots = ROOT_COUNTS[ri];
        u32 *roots = (u32 *)malloc(nroots * sizeof(u32));
        if (!roots) continue;

        // Scatter roots across the pool
        for (u32 i = 0; i < nroots; i++)
            roots[i] = (i * 997) % POOL_ALLOCS;

        u64 t0 = now_ns();
        for (u32 t = 0; t < TRIALS; t++) {
            memset(alive, 0, BM_WORDS * sizeof(u64));
            for (u32 r = 0; r < nroots; r++) {
                u32 idx = roots[r];
                alive[idx / 64] |= (1ULL << (idx % 64));
            }
            // Count live (simulates "how much to compact")
            u32 live = 0;
            for (u32 w = 0; w < BM_WORDS; w++)
                live += __builtin_popcountll(alive[w]);
            g_sink = live;
        }
        u64 t1 = now_ns();

        printf("  %5u roots, %uM allocs: %6.0f ns/scan (%4.1f ns/root)\n",
               nroots, POOL_ALLOCS >> 20,
               (double)(t1 - t0) / TRIALS,
               (double)(t1 - t0) / ((u64)TRIALS * nroots));

        free(roots);
    }
    printf("\n");
    free(alive);
}

// ============================================================
// Bench 5: Compact — slide live data forward
// ============================================================

static void bench_compact(void) {
    printf("--- Compact: slide live data forward ---\n");

    const u32 POOL_SIZE = 4 << 20;  // 4 MB
    const u32 CHUNK = 64;           // 64-byte objects
    const u32 N_CHUNKS = POOL_SIZE / CHUNK;
    const u32 TRIALS = 500;

    u8 *src = (u8 *)malloc(POOL_SIZE);
    u8 *dst = (u8 *)malloc(POOL_SIZE);
    if (!src || !dst) { printf("  OOM\n"); return; }
    memset(src, 0xAB, POOL_SIZE);

    // Test at different survival rates
    const u32 RATES[] = {25, 50, 75};

    for (int si = 0; si < 3; si++) {
        u32 rate = RATES[si];
        u32 n_live = 0;
        u32 *live = (u32 *)malloc(N_CHUNKS * sizeof(u32));
        if (!live) continue;

        // Select live chunks (scattered)
        for (u32 i = 0; i < N_CHUNKS; i++) {
            if ((i * 7 + 3) % 100 < rate) live[n_live++] = i;
        }

        u64 t0 = now_ns();
        for (u32 t = 0; t < TRIALS; t++) {
            u32 doff = 0;
            for (u32 i = 0; i < n_live; i++) {
                memcpy(dst + doff, src + live[i] * CHUNK, CHUNK);
                doff += CHUNK;
            }
            g_sink = doff;
        }
        u64 t1 = now_ns();

        u32 live_bytes = n_live * CHUNK;
        double us = (double)(t1 - t0) / TRIALS / 1000.0;
        double gbps = (double)live_bytes / ((double)(t1 - t0) / TRIALS);

        printf("  %u%% live (%uKB / %uKB): %6.0f us  (%.1f GB/s)\n",
               rate, live_bytes / 1024, POOL_SIZE / 1024, us, gbps);

        free(live);
    }

    // Baseline: contiguous memcpy
    u64 t0 = now_ns();
    for (u32 t = 0; t < TRIALS; t++) {
        memcpy(dst, src, POOL_SIZE);
        g_sink = dst[0];
    }
    u64 t1 = now_ns();
    printf("  memcpy 4MB baseline:          %6.0f us  (%.1f GB/s)\n",
           (double)(t1 - t0) / TRIALS / 1000.0,
           (double)POOL_SIZE / ((double)(t1 - t0) / TRIALS));
    printf("\n");

    free(src);
    free(dst);
}

// ============================================================
// Bench 6: Full cycle — persistent DS workload
// bump+commit+compact vs malloc/free
// ============================================================

static void bench_full_cycle(void) {
    printf("--- Full cycle: persistent DS workload ---\n");
    printf("  (1000 steps, 100 allocs/step, 10%% committed, compact at end)\n");

    const u32 STEPS = 1000;
    const u32 PER = 100;
    const u32 COMMIT_EVERY = 10;  // 10% survive

    // --- Bump model ---
    Mem step = {0}, main_p = {0};
    step.cap = 8 << 20;
    step.base = (u8 *)malloc(step.cap);
    main_p.cap = 64 << 20;
    main_p.base = (u8 *)malloc(main_p.cap);
    if (!step.base || !main_p.base) { printf("  OOM\n"); return; }

    u64 t0 = now_ns();
    for (u32 s = 0; s < STEPS; s++) {
        step.used = 0;
        for (u32 i = 0; i < PER; i++) {
            Cons *c = PUSH(&step, Cons);
            c->car = (Val)(s * PER + i);
            c->cdr = NIL;
            if (i % COMMIT_EVERY == 0) {
                Cons *mc = PUSH(&main_p, Cons);
                mc->car = c->car;
                mc->cdr = c->cdr;
            }
        }
    }
    u64 t1 = now_ns();
    double bump_ns = (double)(t1 - t0);
    u32 committed = main_p.used;

    // --- malloc/free model ---
    t0 = now_ns();
    for (u32 s = 0; s < STEPS; s++) {
        Cons *ptrs[100];
        for (u32 i = 0; i < PER; i++) {
            ptrs[i] = (Cons *)malloc(sizeof(Cons));
            ptrs[i]->car = (Val)(s * PER + i);
            ptrs[i]->cdr = NIL;
        }
        for (u32 i = 0; i < PER; i++) {
            if (i % COMMIT_EVERY != 0)
                free(ptrs[i]);
            // committed leak intentionally (simulates long-lived)
        }
    }
    u64 t1b = now_ns();
    double mf_ns = (double)(t1b - t0);

    u32 total = STEPS * PER;
    printf("  bump+commit:  %8.0f us  (%4.1f ns/alloc)\n",
           bump_ns / 1000.0, bump_ns / total);
    printf("  malloc/free:  %8.0f us  (%4.1f ns/alloc)\n",
           mf_ns / 1000.0, mf_ns / total);
    printf("  ratio:        %6.1fx faster\n", mf_ns / bump_ns);
    printf("  committed:    %u bytes (%u values)\n\n", committed, committed / 16);

    free(step.base);
    free(main_p.base);
}

// ============================================================
// Bench 7: View elimination simulation
// How much does pre-computing offsets save vs runtime bump?
// ============================================================

static void bench_view_elimination(void) {
    printf("--- View elimination: pre-computed offset vs runtime bump ---\n");

    const u32 N = 2000000;

    Mem m = {0};
    m.cap = 64 << 20;
    m.base = (u8 *)malloc(m.cap);
    if (!m.base) { printf("  OOM\n"); return; }

    // --- Runtime bump (V_DYNAMIC path) ---
    m.used = 0;
    u64 t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        Cons *c = PUSH(&m, Cons);
        c->car = (Val)i;
        c->cdr = NIL;
    }
    u64 t1 = now_ns();
    double dynamic_ns = (double)(t1 - t0) / N;
    g_sink = m.used;

    // --- Pre-computed offset (V_ALLOC_OFF path) ---
    // Simulate: one budget check, then all allocations are base+offset
    m.used = 0;
    u32 budget = N * sizeof(Cons);
    if (budget > m.cap) { printf("  budget exceeds cap\n"); free(m.base); return; }

    t0 = now_ns();
    // One bounds check for entire batch
    u8 *batch_base = m.base;
    m.used = budget;

    // Each "allocation" is just pointer arithmetic — no branch, no check
    for (u32 i = 0; i < N; i++) {
        Cons *c = (Cons *)(batch_base + i * sizeof(Cons));
        c->car = (Val)i;
        c->cdr = NIL;
    }
    t1 = now_ns();
    double static_ns = (double)(t1 - t0) / N;
    g_sink = m.used;

    // --- Pure write (no allocation at all — V_CONST comparison) ---
    // Stack-allocated array, no bump, no offset computation
    // Just the write cost
    Cons local_buf[64];  // small to stay in cache
    t0 = now_ns();
    for (u32 i = 0; i < N; i++) {
        Cons *c = &local_buf[i & 63];
        c->car = (Val)i;
        c->cdr = NIL;
    }
    t1 = now_ns();
    double write_ns = (double)(t1 - t0) / N;
    g_sink = local_buf[0].car;

    free(m.base);

    printf("  V_DYNAMIC (bump):    %5.1f ns/alloc\n", dynamic_ns);
    printf("  V_ALLOC_OFF (offset): %4.1f ns/alloc\n", static_ns);
    printf("  V_CONST (no alloc):  %5.1f ns/write\n", write_ns);
    printf("  bump→offset:         %5.1fx\n", dynamic_ns / static_ns);
    printf("  bump→const:          %5.1fx\n\n", dynamic_ns / write_ns);
}

// ============================================================
// Main
// ============================================================

int main(void) {
    printf("genera allocator — micro benchmarks\n");
    printf("model: view → execute → reclaim\n");
    printf("====================================\n\n");

    bench_bump_vs_malloc();
    bench_scope();
    bench_commit();
    bench_mark_phase();
    bench_compact();
    bench_full_cycle();
    bench_view_elimination();

    printf("====================================\n");
    printf("key: bump = {base, used, cap} pointer advance\n");
    printf("     scope = MARK/RESTORE (u32 save/load)\n");
    printf("     commit = deep-copy step → main\n");
    printf("     V_ALIVE = bitmask scan from roots\n");
    printf("     compact = slide live data forward\n");
    printf("     V_ALLOC_OFF = pre-computed offset (no bump)\n");
    printf("     V_CONST = value in register (no allocation)\n");
    return 0;
}
