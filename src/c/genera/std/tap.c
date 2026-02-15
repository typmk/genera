/**
 * tap.c — Unified Observation: Counts + Sequence + Timing + Allocation
 *
 * Three tiers, all projecting onto the same StrId index:
 *   Tier 1 (always-on, ~2ns): bitmask + counts + ring buffer (no timing)
 *   Tier 2 (toggle, +7ns):    per-name self-time via rdtsc inter-dispatch
 *   Tier 3 (toggle):          pmap environments for time-travel (future)
 *
 * Per-name alloc tracking: always-on, ~0.3ns/alloc on top of arena cost.
 *
 * Depends on: sys.c, fmt.c, str.c
 */
#ifndef TAP_C_INCLUDED
#define TAP_C_INCLUDED

// ============================================================================
// 1. RDTSC — cheapest possible timing
// ============================================================================

ALWAYS_INLINE u64 rdtsc(void) {
    u32 lo, hi;
    __asm__ volatile ("rdtsc" : "=a"(lo), "=d"(hi));
    return ((u64)hi << 32) | lo;
}

// ============================================================================
// 2. Observation State — all indexed by StrId
// ============================================================================

#define OBS_MASK_WORDS 1024   // 64K bits
#define OBS_CAP        65536  // one slot per possible StrId

static u64  g_obs_mask[OBS_MASK_WORDS];  // 8KB — which names hit
static u32  g_obs_count[OBS_CAP];        // 256KB — dispatch count per name
static u64  g_obs_ns[OBS_CAP];           // 512KB — self-time ticks per name (tier 2)
static u32  g_obs_total;                 // total dispatches
// g_obs_alloc[] and g_alloc_tag defined in mem.c (needs to be before tap.c)

// Tier level: 1 = counts+sequence (always), 2 = +timing
static u8   g_obs_level = 1;

// Tier 2 state: inter-dispatch timing
static u64  g_obs_last_tick;
static u32  g_obs_last_name;

static void obs_reset(void) {
    memset(g_obs_mask, 0, sizeof(g_obs_mask));
    memset(g_obs_count, 0, sizeof(g_obs_count));
    memset(g_obs_ns, 0, sizeof(g_obs_ns));
    memset(g_obs_alloc, 0, sizeof(g_obs_alloc));
    g_obs_total = 0;
    g_alloc_tag = 0;
    g_obs_last_tick = 0;
    g_obs_last_name = 0;
}

static bool obs_hit(u32 name) {
    return (g_obs_mask[name >> 6] >> (name & 63)) & 1;
}

// ============================================================================
// 3. Ring Buffer — always-on sequence (no timing in base tier)
// ============================================================================

#define TRACE_CAP  4096
#define TRACE_MASK (TRACE_CAP - 1)

typedef struct {
    u32 name;       // StrId
    u16 depth;      // call stack depth
    u16 kind;       // dispatch kind
    u64 tick;       // rdtsc (0 at tier 1, populated at tier 2)
} TraceEv;          // 16 bytes

// Shared header for cross-process observation
typedef struct {
    u32 pos, total, flags, _pad;
    u64 t0_ns, t0_tick;
} TraceHeader;

static TraceEv   g_trace_local[TRACE_CAP];
static TraceEv  *g_trace = g_trace_local;
static u32       g_trace_pos;
static u32       g_trace_total;
static u64       g_tap_t0_ns;
static u64       g_tap_t0_tick;

// Shared memory state
static TraceHeader *g_trace_hdr;
static void        *g_trace_shm;
static u64          g_trace_shm_size;

// ============================================================================
// 4. DISPATCH — the hot path. Replaces separate OBS + TAP.
//
// Tier 1 (~2ns): counts + ring buffer store (always on)
// Tier 2 (+7ns): rdtsc inter-dispatch → per-name self-time
// ============================================================================

#define DISPATCH(kind, name, depth) do { \
    g_obs_mask[(name) >> 6] |= (1ULL << ((name) & 63)); \
    g_obs_count[name]++; \
    g_obs_total++; \
    g_alloc_tag = (name); \
    u32 _i = g_trace_pos++ & TRACE_MASK; \
    g_trace_total++; \
    g_trace[_i] = (TraceEv){(name), (u16)(depth), (u16)(kind), 0}; \
    if (g_trace_hdr) { g_trace_hdr->pos = g_trace_pos; g_trace_hdr->total = g_trace_total; } \
    if (UNLIKELY(g_obs_level >= 2)) { \
        u64 _now = rdtsc(); \
        g_obs_ns[g_obs_last_name] += _now - g_obs_last_tick; \
        g_obs_last_tick = _now; \
        g_obs_last_name = (name); \
        g_trace[_i].tick = _now; \
    } \
} while(0)


ALWAYS_INLINE void tap_on(void) {
    g_tap_t0_ns = now_ns();
    g_tap_t0_tick = rdtsc();
    if (g_obs_level >= 2) { g_obs_last_tick = g_tap_t0_tick; }
}
ALWAYS_INLINE void tap_off(void) {
    // Flush final timing interval
    if (g_obs_level >= 2 && g_obs_last_name) {
        g_obs_ns[g_obs_last_name] += rdtsc() - g_obs_last_tick;
    }
}
ALWAYS_INLINE void tap_reset(void) { g_trace_pos = 0; g_trace_total = 0; }

// ============================================================================
// 4. Query
// ============================================================================

static u32 trace_count(void) {
    return g_trace_total < TRACE_CAP ? g_trace_total : TRACE_CAP;
}

static TraceEv *trace_at(u32 ago) {
    u32 n = trace_count();
    if (ago >= n) return NULL;
    return &g_trace[(g_trace_pos - 1 - ago) & TRACE_MASK];
}

static u32 trace_count_kind(u32 kind) {
    u32 c = 0, n = trace_count();
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        if (g_trace[idx].kind == kind) c++;
    }
    return c;
}

// ============================================================================
// 5. Dump — call tree with timing and source
// ============================================================================

// Convert ticks to nanoseconds using calibration from tap_on
static u64 ticks_to_ns(u64 tick) {
    if (!g_tap_t0_tick) return 0;
    u64 elapsed_tick = tick - g_tap_t0_tick;
    // Use end calibration for accuracy
    u64 end_ns = now_ns();
    u64 end_tick = rdtsc();
    u64 total_ticks = end_tick - g_tap_t0_tick;
    u64 total_ns = end_ns - g_tap_t0_ns;
    if (!total_ticks) return 0;
    return elapsed_tick * total_ns / total_ticks;
}

// Print trace as indented call tree with timing
static void trace_dump(u32 max) {
    u32 n = trace_count();
    if (max && max < n) n = max;
    if (!n) { pf("(no trace events)\n"); return; }

    // Calibrate: ns per tick
    u64 first_tick = g_trace[(g_trace_pos - n) & TRACE_MASK].tick;
    u64 last_tick = g_trace[(g_trace_pos - 1) & TRACE_MASK].tick;
    u64 total_ticks = last_tick - first_tick;

    // Compute total wall time from start/end
    u64 total_ns = 0;
    if (g_tap_t0_tick && total_ticks) {
        u64 cal_ns = now_ns() - g_tap_t0_ns;
        u64 cal_ticks = rdtsc() - g_tap_t0_tick;
        if (cal_ticks) total_ns = total_ticks * cal_ns / cal_ticks;
    }

    pf("--- trace: %u events", n);
    if (total_ns) {
        buf_s(&g_print_buf, ", ");
        buf_elapsed(&g_print_buf, total_ns);
    }
    pf(" ---\n");

    // Walk trace oldest→newest, print indented call tree
    for (u32 i = n; i > 0; i--) {
        u32 idx = (g_trace_pos - i) & TRACE_MASK;
        TraceEv *e = &g_trace[idx];
        Str kind = str_from_id(e->kind);
        Str name = str_from_id(e->name);

        // Indent by depth
        for (u32 d = 0; d < e->depth && d < 60; d++) buf_c(&g_print_buf, ' ');

        // Kind (dimmed)
        pfc(C_DIM);
        for (u32 j = 0; j < kind.len; j++) buf_c(&g_print_buf, kind.data[j]);
        pfc(C_RESET);
        buf_c(&g_print_buf, ' ');

        // Name
        if (name.len) pfc(C_YELLOW);
        for (u32 j = 0; j < name.len; j++) buf_c(&g_print_buf, name.data[j]);
        pfc(C_RESET);

        // Relative time from start
        u64 rel = e->tick - first_tick;
        buf_s(&g_print_buf, "  +");
        if (total_ns && total_ticks) {
            u64 ns = rel * total_ns / total_ticks;
            buf_elapsed(&g_print_buf, ns);
        } else {
            buf_u(&g_print_buf, rel);
        }

        pf("\n");
    }
}

// Legacy compat
static void trace_print(u32 max) { trace_dump(max); }
static void trace_print_eval(u32 max) { trace_dump(max); }

// Collect obs hits into sorted array (by count descending). Returns count.
static u32 obs_collect(u32 *hits, u32 cap) {
    u32 nh = 0;
    for (u32 w = 0; w < OBS_MASK_WORDS && nh < cap; w++) {
        u64 bits = g_obs_mask[w];
        while (bits && nh < cap) {
            hits[nh++] = w * 64 + __builtin_ctzll(bits);
            bits &= bits - 1;
        }
    }
    for (u32 i = 1; i < nh; i++) {
        u32 k = hits[i], j = i;
        while (j > 0 && g_obs_count[hits[j-1]] < g_obs_count[k]) { hits[j] = hits[j-1]; j--; }
        hits[j] = k;
    }
    return nh;
}

// Convert obs_ns ticks to wall-clock ns using calibration
static u64 obs_ticks_to_ns(u64 ticks) {
    if (!g_tap_t0_tick) return 0;
    u64 cal_ticks = rdtsc() - g_tap_t0_tick;
    u64 cal_ns = now_ns() - g_tap_t0_ns;
    if (!cal_ticks) return 0;
    return ticks * cal_ns / cal_ticks;
}

// ============================================================================
// 6. Always-On Dump — show obs bitmask + counts (zero-cost data)
// ============================================================================

static void obs_dump(u32 max) {
    if (!g_obs_total) { pf("(no observations)\n"); return; }

    // Collect hit names sorted by count (simple insertion sort, small N)
    u32 hits[256]; u32 nh = 0;
    for (u32 w = 0; w < OBS_MASK_WORDS && nh < 256; w++) {
        u64 bits = g_obs_mask[w];
        while (bits && nh < 256) {
            u32 bit = __builtin_ctzll(bits);
            hits[nh++] = w * 64 + bit;
            bits &= bits - 1;
        }
    }

    // Sort by count descending (insertion sort, N < 256)
    for (u32 i = 1; i < nh; i++) {
        u32 key = hits[i];
        u32 j = i;
        while (j > 0 && g_obs_count[hits[j-1]] < g_obs_count[key]) {
            hits[j] = hits[j-1]; j--;
        }
        hits[j] = key;
    }

    if (max && max < nh) nh = max;
    u32 max_count = nh ? g_obs_count[hits[0]] : 0;

    pf("--- obs: %u dispatches, %u names ---\n", g_obs_total, nh);
    for (u32 i = 0; i < nh; i++) {
        u32 id = hits[i];
        Str name = str_from_id(id);
        u32 cnt = g_obs_count[id];

        // Count (right-justified)
        buf_rjust_i(&g_print_buf, cnt, 6);
        buf_s(&g_print_buf, "  ");

        // Bar (proportional)
        buf_bar(&g_print_buf, cnt, max_count, 20, C_CYAN);
        buf_c(&g_print_buf, ' ');

        // Name
        for (u32 j = 0; j < name.len; j++) buf_c(&g_print_buf, name.data[j]);
        pf("\n");
    }
}

// ============================================================================
// 7. Bitmask views — trace events → same query format as grammar views
// ============================================================================

static void tap_to_bitmask(u32 kind, u64 *m, u32 mw) {
    u32 n = trace_count(), maxbit = mw * 64;
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        TraceEv *e = &g_trace[idx];
        if (e->kind == kind && e->name < maxbit)
            m[e->name / 64] |= (1ULL << (e->name % 64));
    }
}

static void tap_hit_counts(u32 kind, u32 *counts, u32 cap) {
    u32 n = trace_count();
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        TraceEv *e = &g_trace[idx];
        if (e->kind == kind && e->name < cap)
            counts[e->name]++;
    }
}

// ============================================================================
// 7. Shared Memory — mmap for cross-process observation
// ============================================================================

static int tap_share(void) {
    u64 size = sizeof(TraceHeader) + TRACE_CAP * sizeof(TraceEv);
    size = PAGE_ALIGN(size);
    void *p = sys_mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANON, -1, 0);
    if ((i64)p < 0) return -1;

    g_trace_shm = p;
    g_trace_shm_size = size;
    g_trace_hdr = (TraceHeader *)p;
    g_trace = (TraceEv *)((u8 *)p + sizeof(TraceHeader));
    g_trace_hdr->pos = 0;
    g_trace_hdr->total = 0;
    g_trace_hdr->flags = 1;
    g_trace_hdr->t0_ns = now_ns();
    g_trace_hdr->t0_tick = rdtsc();
    g_trace_pos = 0;
    g_trace_total = 0;
    return 0;
}

__attribute__((noreturn)) static void tap_observe(void) {
    u32 last_total = 0;
    while (g_trace_hdr->flags) {
        u32 cur_total = g_trace_hdr->total;
        if (cur_total > last_total) {
            u32 new_events = cur_total - last_total;
            if (new_events > TRACE_CAP) new_events = TRACE_CAP;
            u64 first_tick = g_trace[(g_trace_hdr->pos - new_events) & TRACE_MASK].tick;
            for (u32 i = new_events; i > 0; i--) {
                u32 idx = (g_trace_hdr->pos - i) & TRACE_MASK;
                TraceEv *e = &g_trace[idx];
                Str kind = str_from_id(e->kind);
                Str name = str_from_id(e->name);
                for (u32 d = 0; d < e->depth && d < 40; d++) buf_c(&g_print_buf, ' ');
                pfc(C_DIM);
                for (u32 j = 0; j < kind.len; j++) buf_c(&g_print_buf, kind.data[j]);
                pfc(C_RESET);
                buf_c(&g_print_buf, ' ');
                for (u32 j = 0; j < name.len; j++) buf_c(&g_print_buf, name.data[j]);
                // Tick offset
                pfc(C_DIM);
                pf("  +%llu", (unsigned long long)(e->tick - first_tick));
                pfc(C_RESET);
                pf("\n");
            }
            last_total = cur_total;
        }
        sys_nanosleep(100000);  // 100μs poll
    }
    pf("--- observer: %u total events ---\n", g_trace_hdr->total);
    sys_exit(0);
    __builtin_unreachable();
}

static void tap_done(void) {
    if (g_trace_hdr) g_trace_hdr->flags = 0;
}

static void tap_unshare(void) {
    if (g_trace_shm) {
        sys_munmap(g_trace_shm, g_trace_shm_size);
        g_trace_shm = NULL;
        g_trace_hdr = NULL;
        g_trace = g_trace_local;
    }
}

#endif // TAP_C_INCLUDED
