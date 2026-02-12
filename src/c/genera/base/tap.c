/**
 * tap.c — Trace Ring Buffer + TAP Macro
 *
 * Observable execution: sig equivalent for C.
 * Zero cost when off: single branch-not-taken (~0.3 ns).
 * Depends on: sys.c, fmt.c, str.c
 */
#ifndef TAP_C_INCLUDED
#define TAP_C_INCLUDED

#define TRACE_CAP 4096  // ring buffer entries (power of 2)
#define TRACE_MASK (TRACE_CAP - 1)

typedef struct {
    u32 kind;       // StrId of event name
    u32 arg0;
    u32 arg1;
    u32 arg2;
    u64 ts;         // timestamp (ns)
} TraceEv;          // 24 bytes

static TraceEv g_trace[TRACE_CAP];
static u32     g_trace_pos;
static u32     g_trace_total;
static bool    g_tap_on;

NOINLINE static void trace_emit_slow(u32 kind, u32 a0, u32 a1, u32 a2) {
    u32 i = g_trace_pos & TRACE_MASK;
    g_trace[i] = (TraceEv){kind, a0, a1, a2, now_ns()};
    g_trace_pos++;
    g_trace_total++;
}

#define TAP(kind, a0, a1, a2) do { \
    if (UNLIKELY(g_tap_on)) trace_emit_slow(kind, a0, a1, a2); \
} while(0)

#define TAP0(kind)           TAP(kind, 0, 0, 0)
#define TAP1(kind, a0)       TAP(kind, a0, 0, 0)
#define TAP2(kind, a0, a1)   TAP(kind, a0, a1, 0)

ALWAYS_INLINE void tap_on(void)    { g_tap_on = true; }
ALWAYS_INLINE void tap_off(void)   { g_tap_on = false; }
ALWAYS_INLINE void tap_reset(void) { g_trace_pos = 0; g_trace_total = 0; }

static u32 trace_count(void) {
    return g_trace_total < TRACE_CAP ? g_trace_total : TRACE_CAP;
}

static TraceEv *trace_iter(u32 *cursor) {
    u32 n = trace_count();
    if (*cursor >= n) return NULL;
    u32 idx = (g_trace_pos - 1 - *cursor) & TRACE_MASK;
    (*cursor)++;
    return &g_trace[idx];
}

static u32 trace_count_kind(u32 kind) {
    u32 c = 0, n = trace_count();
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        if (g_trace[idx].kind == kind) c++;
    }
    return c;
}

static void trace_print(u32 max) {
    u32 n = trace_count();
    if (max && max < n) n = max;
    pf("  trace (%u events, %u total):\n", n, g_trace_total);
    for (u32 i = 0; i < n; i++) {
        u32 idx = (g_trace_pos - 1 - i) & TRACE_MASK;
        TraceEv *e = &g_trace[idx];
        Str name = str_from_id(e->kind);
        pf("    [%u] ", g_trace_total - 1 - i);
        for (u32 j = 0; j < name.len && j < 20; j++) buf_c(&g_print_buf, name.data[j]);
        pf(" %u %u %u\n", e->arg0, e->arg1, e->arg2);
    }
}

#endif // TAP_C_INCLUDED
