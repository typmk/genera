/**
 * types.c — Portable Types, Core Functions, Compiler Macros
 *
 * THE foundation. No OS, no arch, no dependencies.
 * Everything above this can assume these exist.
 */
#ifndef TYPES_C_INCLUDED
#define TYPES_C_INCLUDED

// ============================================================================
// 1. Integer Types — LP64: long = pointer = 8 bytes
// ============================================================================

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;
typedef signed char        i8;
typedef short              i16;
typedef int                i32;
typedef long long          i64;
typedef float              f32;
typedef double             f64;
typedef u64                usize;

#if __STDC_VERSION__ < 202311L
typedef _Bool              bool;
#define true  1
#define false 0
#endif
#define NULL  ((void *)0)

#define UINT32_MAX 0xFFFFFFFFU

_Static_assert(sizeof(u64) == 8, "u64 must be 8 bytes");
_Static_assert(sizeof(i64) == 8, "i64 must be 8 bytes");
_Static_assert(sizeof(f64) == 8, "f64 must be 8 bytes");
_Static_assert(sizeof(void *) == 8, "pointer must be 8 bytes");

// ============================================================================
// 2. Compiler Macros
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

static volatile i64 g_sink;
#define SINK(x) (g_sink = (i64)(x))

// ============================================================================
// 3. Core Functions — linker symbols for -nostdlib
//
// GCC at -O3 may convert struct copies / loops into calls to these symbols.
// With -nostdlib, those symbols don't exist unless we provide them.
// Empty asm volatile prevents GCC from recognizing the loop as memcpy/memset
// and generating an infinite recursion.
// ============================================================================

void *memcpy(void *dst, const void *src, __SIZE_TYPE__ n) {
    u8 *d = (u8 *)dst;
    const u8 *s = (const u8 *)src;
    while (n--) { *d++ = *s++; __asm__ volatile("" ::: "memory"); }
    return dst;
}

void *memset(void *dst, int c, __SIZE_TYPE__ n) {
    u8 *d = (u8 *)dst;
    while (n--) { *d++ = (u8)c; __asm__ volatile("" ::: "memory"); }
    return dst;
}

void *memmove(void *dst, const void *src, __SIZE_TYPE__ n) {
    u8 *d = (u8 *)dst;
    const u8 *s = (const u8 *)src;
    if (d < s) { while (n--) { *d++ = *s++; __asm__ volatile("" ::: "memory"); } }
    else { d += n; s += n; while (n--) { *--d = *--s; __asm__ volatile("" ::: "memory"); } }
    return dst;
}

int memcmp(const void *a, const void *b, __SIZE_TYPE__ n) {
    const u8 *pa = (const u8 *)a, *pb = (const u8 *)b;
    while (n--) {
        if (*pa != *pb) return *pa - *pb;
        pa++; pb++;
    }
    return 0;
}

__SIZE_TYPE__ strlen(const char *s) {
    const char *p = s;
    while (*p) p++;
    return (__SIZE_TYPE__)(p - s);
}

int strcmp(const char *a, const char *b) {
    while (*a && *a == *b) { a++; b++; }
    return *(const u8 *)a - *(const u8 *)b;
}

// Variadic args — pure compiler builtins, no runtime component
typedef __builtin_va_list va_list;
#define va_start __builtin_va_start
#define va_end   __builtin_va_end
#define va_arg   __builtin_va_arg

#endif // TYPES_C_INCLUDED
