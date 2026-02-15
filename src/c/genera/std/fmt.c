/**
 * fmt.c — Output Buffer + Formatting + Pretty Printing
 *
 * OutBuf, pf(), buf_* helpers, ANSI colors, column formatting.
 * Depends on: sys/sys.c (for types, macros, sys_write, sys_time_ns)
 *
 * Aesthetic lineage: Genera → SBCL → Clojure → Rust/cargo
 * Core idea: information-dense but visually clear.
 */
#ifndef FMT_C_INCLUDED
#define FMT_C_INCLUDED

#define now_ns sys_time_ns

ALWAYS_INLINE u32 hash32(u32 x) {
    x ^= x >> 16; x *= 0x85ebca6b;
    x ^= x >> 13; x *= 0xc2b2ae35;
    x ^= x >> 16; return x;
}

// ============================================================================
// 1. ANSI Colors
//
// Compose by concatenation: C_BOLD C_RED = bold red.
// pfc() writes color code only when g_color is true (tty detection).
// ============================================================================

// Style
#define C_RESET   "\033[0m"
#define C_BOLD    "\033[1m"
#define C_DIM     "\033[2m"
#define C_UNDER   "\033[4m"
#define C_BLINK   "\033[5m"
#define C_INVERT  "\033[7m"

// Foreground
#define C_RED     "\033[31m"
#define C_GREEN   "\033[32m"
#define C_YELLOW  "\033[33m"
#define C_BLUE    "\033[34m"
#define C_MAGENTA "\033[35m"
#define C_CYAN    "\033[36m"
#define C_WHITE   "\033[37m"
#define C_GRAY    "\033[90m"

// Background
#define C_BG_RED     "\033[41m"
#define C_BG_GREEN   "\033[42m"
#define C_BG_YELLOW  "\033[43m"
#define C_BG_BLUE    "\033[44m"

static bool g_color;  // set true when stdout is a tty

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

// ============================================================================
// 3. Mini Printf
//
// %s %d %u %x %c %lld %llu %llx %%
// ============================================================================

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
        if (*fmt == 'l' && *(fmt+1) == 'l' && *(fmt+2) == 'u') {
            buf_u(b, va_arg(ap, u64)); fmt += 3; continue;
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

// ============================================================================
// 4. Global Output + Color
// ============================================================================

static char g_print_data[65536];
static OutBuf g_print_buf;

static void print_init(void) {
    g_print_buf = (OutBuf){g_print_data, 0, sizeof(g_print_data)};
}

// pf = printf replacement. Auto-flushes unless g_pf_batch is set.
static bool g_pf_batch;
static void pf(const char *fmt, ...) {
    va_list ap; va_start(ap, fmt);
    buf_vfmt(&g_print_buf, fmt, ap);
    va_end(ap);
    if (!g_pf_batch) buf_flush(&g_print_buf, 1);
}
static void print_flush(void) { buf_flush(&g_print_buf, 1); }

// pfc = buffer a color code (no flush). Combine with pf() for one write.
//   pfc(C_RED); pf("error\n");  →  "\033[31merror\n" flushed together
static void pfc(const char *code) {
    if (g_color) buf_s(&g_print_buf, code);
}

// ============================================================================
// 5. Column Formatting — alignment, padding, separators
//
// buf_pad:      repeat char n times
// buf_rjust_i:  right-justify integer in field
// buf_ljust_s:  left-justify string in field
// buf_hr:       horizontal rule (dimmed)
// buf_table:    row of columns with alignment
// ============================================================================

static void buf_pad(OutBuf *b, char ch, u32 n) {
    for (u32 i = 0; i < n; i++) buf_c(b, ch);
}

// Right-justify integer in field of `width` chars
static void buf_rjust_i(OutBuf *b, i64 val, u32 width) {
    char tmp[20]; u32 len = 0;
    u64 v = val < 0 ? (u64)(-val) : (u64)val;
    do { tmp[len++] = '0' + (char)(v % 10); v /= 10; } while (v);
    if (val < 0) tmp[len++] = '-';
    if (len < width) buf_pad(b, ' ', width - len);
    while (len) buf_c(b, tmp[--len]);
}

// Left-justify string in field of `width` chars
static void buf_ljust_s(OutBuf *b, const char *s, u32 width) {
    u32 len = strlen(s);
    buf_s(b, s);
    if (len < width) buf_pad(b, ' ', width - len);
}

// Right-justify string in field of `width` chars
static void buf_rjust_s(OutBuf *b, const char *s, u32 width) {
    u32 len = strlen(s);
    if (len < width) buf_pad(b, ' ', width - len);
    buf_s(b, s);
}

// Horizontal rule — dimmed separator line
static void buf_hr(OutBuf *b, u32 width) {
    if (g_color) buf_s(b, C_DIM);
    buf_pad(b, '-', width);
    if (g_color) buf_s(b, C_RESET);
    buf_c(b, '\n');
}

// Table row: print columns with widths.
//   widths[i] > 0 → left-justify, < 0 → right-justify
//   vals[i] = string content for column
static void buf_row(OutBuf *b, const char **vals, const i32 *widths, u32 ncols) {
    for (u32 i = 0; i < ncols; i++) {
        if (i > 0) buf_s(b, "  ");
        i32 w = widths[i];
        if (w < 0) buf_rjust_s(b, vals[i], (u32)(-w));
        else       buf_ljust_s(b, vals[i], (u32)w);
    }
    buf_c(b, '\n');
}

// ============================================================================
// 6. CSI — Terminal Control (TUI)
//
// CSI (Control Sequence Introducer): \033[ {a} {;b} {code}
// ONE function emits them all. Macros name the common ones.
// read_key() in cmd.c parses the same grammar in reverse.
// ============================================================================

// CSI primitive: \033[ {a} {;b} code — the fundamental
static void csi(u32 a, u32 b, char code) {
    if (!g_color) return;
    OutBuf *o = &g_print_buf;
    buf_s(o, "\033[");
    if (a || b) { buf_u(o, a); if (b) { buf_c(o, ';'); buf_u(o, b); } }
    buf_c(o, code);
}

// Cursor:  A=up B=down C=fwd D=back G=col H=goto
// Erase:   K: 0=eol 1=bol 2=line   J: 0=down 1=up 2=screen
// Scroll:  r=set region
#define cur_up(n)      csi(n, 0, 'A')
#define cur_down(n)    csi(n, 0, 'B')
#define cur_fwd(n)     csi(n, 0, 'C')
#define cur_back(n)    csi(n, 0, 'D')
#define cur_col(c)     csi(c, 0, 'G')
#define cur_goto(r,c)  csi(r, c, 'H')
#define erase_eol()    csi(0, 0, 'K')
#define erase_line()   csi(2, 0, 'K')
#define erase_screen() csi(2, 0, 'J')
#define scroll_set(t,b) csi(t, b, 'r')

// Private modes (DEC): don't follow CSI a;b pattern
#define cur_save()     pfc("\033[s")
#define cur_restore()  pfc("\033[u")
#define cur_hide()     pfc("\033[?25l")
#define cur_show()     pfc("\033[?25h")
#define screen_alt()   pfc("\033[?1049h")
#define screen_main()  pfc("\033[?1049l")
#define scroll_reset() pfc("\033[r")

// Extended color (256 + 24-bit RGB) — writes to any OutBuf
static void buf_fg(OutBuf *b, u8 n) {
    if (g_color) { buf_s(b, "\033[38;5;"); buf_u(b, n); buf_c(b, 'm'); }
}
static void buf_bg(OutBuf *b, u8 n) {
    if (g_color) { buf_s(b, "\033[48;5;"); buf_u(b, n); buf_c(b, 'm'); }
}
static void buf_fg_rgb(OutBuf *b, u8 r, u8 g, u8 bl) {
    if (!g_color) return;
    buf_s(b, "\033[38;2;"); buf_u(b, r); buf_c(b, ';'); buf_u(b, g); buf_c(b, ';'); buf_u(b, bl); buf_c(b, 'm');
}
static void buf_bg_rgb(OutBuf *b, u8 r, u8 g, u8 bl) {
    if (!g_color) return;
    buf_s(b, "\033[48;2;"); buf_u(b, r); buf_c(b, ';'); buf_u(b, g); buf_c(b, ';'); buf_u(b, bl); buf_c(b, 'm');
}

// ============================================================================
// 7. Pretty-Print Helpers
//
// For Genera/SBCL-style output: types in color, structure visible.
// ============================================================================

// Print a labeled value: "label: value" with dim label
static void buf_label(OutBuf *b, const char *label, const char *val) {
    if (g_color) buf_s(b, C_DIM);
    buf_s(b, label);
    buf_s(b, ": ");
    if (g_color) buf_s(b, C_RESET);
    buf_s(b, val);
}

// Print a labeled integer
static void buf_label_i(OutBuf *b, const char *label, i64 val) {
    if (g_color) buf_s(b, C_DIM);
    buf_s(b, label);
    buf_s(b, ": ");
    if (g_color) buf_s(b, C_RESET C_YELLOW);
    buf_i(b, val);
    if (g_color) buf_s(b, C_RESET);
}

// Print elapsed time in human-friendly format
static void buf_elapsed(OutBuf *b, u64 ns) {
    if (g_color) buf_s(b, C_DIM);
    if (ns < 1000)            { buf_u(b, ns); buf_s(b, " ns"); }
    else if (ns < 1000000)    { buf_f1(b, (f64)ns / 1000.0); buf_s(b, " us"); }
    else if (ns < 1000000000) { buf_f1(b, (f64)ns / 1000000.0); buf_s(b, " ms"); }
    else                      { buf_f1(b, (f64)ns / 1000000000.0); buf_s(b, " s"); }
    if (g_color) buf_s(b, C_RESET);
}

// Print a "bar" of width proportional to value/max
static void buf_bar(OutBuf *b, u64 val, u64 max, u32 width, const char *color) {
    u32 filled = max > 0 ? (u32)((val * width) / max) : 0;
    if (filled > width) filled = width;
    if (g_color && color) buf_s(b, color);
    buf_pad(b, '=', filled);
    if (g_color) buf_s(b, C_DIM);
    buf_pad(b, '-', width - filled);
    if (g_color) buf_s(b, C_RESET);
}

// Print byte count in human-friendly format (no color)
static void buf_bytes(OutBuf *b, u64 n) {
    if (n >= 1048576) { buf_f1(b, (f64)n / 1048576.0); buf_s(b, " MB"); }
    else if (n >= 1024) { buf_f1(b, (f64)n / 1024.0); buf_s(b, " KB"); }
    else { buf_u(b, n); buf_s(b, " B"); }
}

#endif // FMT_C_INCLUDED
