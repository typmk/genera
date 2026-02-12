/**
 * fmt.c — Output Buffer + Formatting (replaces printf)
 *
 * OutBuf, pf(), buf_* helpers, hash32, macros.
 * Depends on: sys.c
 */
#ifndef FMT_C_INCLUDED
#define FMT_C_INCLUDED

// ============================================================================
// 1. Macros
// ============================================================================

#define ALIGN_UP(x, a)    (((x) + ((a)-1)) & ~((a)-1))
#define LIKELY(x)         __builtin_expect(!!(x), 1)
#define UNLIKELY(x)       __builtin_expect(!!(x), 0)
#define POPCOUNT(x)       __builtin_popcountll(x)
#define CTZ(x)            __builtin_ctzll(x)
#define CLZ(x)            __builtin_clzll(x)
#define ALWAYS_INLINE     __attribute__((always_inline)) static inline
#define NOINLINE          __attribute__((noinline))
#define ALIGNED(n)        __attribute__((aligned(n)))
#define MIN(a, b)         ((a) < (b) ? (a) : (b))
#define MAX(a, b)         ((a) > (b) ? (a) : (b))
#define CLAMP(x, lo, hi) (MIN(MAX(x, lo), hi))

#define UINT32_MAX        0xFFFFFFFFU

#define now_ns sys_time_ns

static volatile i64 g_sink;
#define SINK(x) (g_sink = (i64)(x))

ALWAYS_INLINE u32 hash32(u32 x) {
    x ^= x >> 16; x *= 0x85ebca6b;
    x ^= x >> 13; x *= 0xc2b2ae35;
    x ^= x >> 16; return x;
}

// ============================================================================
// 2. Output Buffer + Formatting
// ============================================================================

typedef struct { char *buf; u32 pos; u32 cap; } OutBuf;

ALWAYS_INLINE void buf_c(OutBuf *b, char c) {
    if (LIKELY(b->pos < b->cap)) b->buf[b->pos++] = c;
}

static void buf_s(OutBuf *b, const char *s) {
    while (*s) buf_c(b, *s++);
}

static void buf_n(OutBuf *b, const char *s, u32 n) {
    for (u32 i = 0; i < n; i++) buf_c(b, s[i]);
}

static void buf_u(OutBuf *b, u64 n) {
    char tmp[20]; int i = 0;
    if (n == 0) { buf_c(b, '0'); return; }
    while (n > 0) { tmp[i++] = '0' + (char)(n % 10); n /= 10; }
    while (i > 0) buf_c(b, tmp[--i]);
}

static void buf_i(OutBuf *b, i64 n) {
    if (n < 0) { buf_c(b, '-'); buf_u(b, (u64)(-n)); }
    else buf_u(b, (u64)n);
}

static void buf_x(OutBuf *b, u64 n) {
    static const char hex[] = "0123456789abcdef";
    buf_s(b, "0x");
    if (n == 0) { buf_c(b, '0'); return; }
    char tmp[16]; int i = 0;
    while (n > 0) { tmp[i++] = hex[n & 0xF]; n >>= 4; }
    while (i > 0) buf_c(b, tmp[--i]);
}

// Fixed-point: 1 decimal place (for "12.3 ms" style output)
static void buf_f1(OutBuf *b, f64 d) {
    if (d < 0) { buf_c(b, '-'); d = -d; }
    i64 whole = (i64)d;
    i64 frac = (i64)((d - (f64)whole) * 10.0 + 0.5);
    if (frac >= 10) { whole++; frac = 0; }
    buf_i(b, whole);
    buf_c(b, '.');
    buf_c(b, '0' + (char)frac);
}

static void buf_flush(OutBuf *b, int fd) {
    if (b->pos > 0) {
        sys_write(fd, b->buf, b->pos);
        b->pos = 0;
    }
}

static void buf_reset(OutBuf *b) { b->pos = 0; if (b->buf) b->buf[0] = '\0'; }

static void buf_hex(OutBuf *b, u64 v) {
    const char h[] = "0123456789abcdef";
    char tmp[16]; int n = 0;
    if (v == 0) { buf_c(b, '0'); return; }
    while (v) { tmp[n++] = h[v & 0xF]; v >>= 4; }
    while (n-- > 0) buf_c(b, tmp[n]);
}

// Mini vfmt: handles %s %d %u %x %c %lld %% — no width/padding
static void buf_vfmt(OutBuf *b, const char *fmt, va_list ap) {
    while (*fmt) {
        if (*fmt != '%') { buf_c(b, *fmt++); continue; }
        fmt++;
        if (*fmt == '%') { buf_c(b, '%'); fmt++; continue; }
        if (*fmt == 'c') { buf_c(b, (char)va_arg(ap, int)); fmt++; continue; }
        if (*fmt == 's') { buf_s(b, va_arg(ap, const char *)); fmt++; continue; }
        if (*fmt == 'd') { buf_i(b, va_arg(ap, int)); fmt++; continue; }
        if (*fmt == 'u') { buf_u(b, va_arg(ap, unsigned)); fmt++; continue; }
        if (*fmt == 'x') { buf_hex(b, va_arg(ap, unsigned)); fmt++; continue; }
        if (*fmt == 'l' && *(fmt+1) == 'l' && *(fmt+2) == 'd') {
            buf_i(b, va_arg(ap, i64)); fmt += 3; continue;
        }
        if (*fmt == 'l' && *(fmt+1) == 'l' && *(fmt+2) == 'x') {
            buf_hex(b, va_arg(ap, u64)); fmt += 3; continue;
        }
        // Unknown: pass through
        buf_c(b, '%'); buf_c(b, *fmt++);
    }
}

static void buf_fmt(OutBuf *b, const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(b, fmt, ap);
    va_end(ap);
}

// Global stdout buffer
static char g_print_data[8192];
static OutBuf g_print_buf;

static void print_init(void) {
    g_print_buf = (OutBuf){g_print_data, 0, sizeof(g_print_data)};
}

// pf = printf replacement. Auto-flushes.
static void pf(const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(&g_print_buf, fmt, ap);
    va_end(ap);
    buf_flush(&g_print_buf, 1);
}
static void print_flush(void) { buf_flush(&g_print_buf, 1); }

#endif // FMT_C_INCLUDED
