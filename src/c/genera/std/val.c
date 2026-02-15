/**
 * val.c — NaN Boxing: 12 types in 64 bits
 *
 * ZERO-INIT: Val 0x0 = f64 +0.0 (NOT nil — nil is TAG_NIL).
 * Depends on: sys.c, fmt.c
 */
#ifndef VAL_C_INCLUDED
#define VAL_C_INCLUDED

typedef u64 Val;

#define QNAN       ((u64)0x7FF8000000000000ULL)
#define SIGN_BIT   ((u64)0x8000000000000000ULL)
#define TAG_MASK   ((u64)0xFFFF000000000000ULL)
#define VAL_MASK   ((u64)0x0000FFFFFFFFFFFFULL)

#define TAG_NIL    ((u64)0x7FF8000000000000ULL)
#define TAG_BOOL   ((u64)0x7FF9000000000000ULL)
#define TAG_INT    ((u64)0x7FFA000000000000ULL)
#define TAG_SYM    ((u64)0x7FFB000000000000ULL)
#define TAG_KW     ((u64)0x7FFC000000000000ULL)
#define TAG_STR    ((u64)0x7FFD000000000000ULL)
#define TAG_PMAP   ((u64)0x7FFE000000000000ULL)
#define TAG_PVEC   ((u64)0x7FFF000000000000ULL)
#define TAG_FN     ((u64)0xFFF8000000000000ULL)
#define TAG_CONS   ((u64)0xFFF9000000000000ULL)

ALWAYS_INLINE Val val_nil(void)       { return TAG_NIL; }
ALWAYS_INLINE Val val_bool(bool b)    { return TAG_BOOL | (u64)b; }
ALWAYS_INLINE Val val_true(void)      { return TAG_BOOL | 1; }
ALWAYS_INLINE Val val_false(void)     { return TAG_BOOL; }
ALWAYS_INLINE Val val_int(i64 n)      { return TAG_INT | ((u64)n & VAL_MASK); }
ALWAYS_INLINE Val val_sym(StrId id)   { return TAG_SYM | (u64)id; }
ALWAYS_INLINE Val val_kw(StrId id)    { return TAG_KW  | (u64)id; }
ALWAYS_INLINE Val val_str(Str *s)     { return TAG_STR | (u64)s; }
ALWAYS_INLINE Val val_pmap(void *p)   { return TAG_PMAP| (u64)p; }
ALWAYS_INLINE Val val_pvec(void *p)   { return TAG_PVEC| (u64)p; }
ALWAYS_INLINE Val val_fn(void *p)     { return TAG_FN  | ((u64)p & VAL_MASK); }
ALWAYS_INLINE Val val_cons(void *p)   { return TAG_CONS | ((u64)p & VAL_MASK); }
ALWAYS_INLINE Val val_f64(f64 d)      { Val v; memcpy(&v, &d, 8); return v; }

ALWAYS_INLINE bool val_is_f64(Val v)  { return (v & QNAN) != QNAN; }
ALWAYS_INLINE bool val_is_nil(Val v)  { return v == TAG_NIL; }
ALWAYS_INLINE bool val_is_bool(Val v) { return (v & TAG_MASK) == TAG_BOOL; }
ALWAYS_INLINE bool val_is_int(Val v)  { return (v & TAG_MASK) == TAG_INT; }
ALWAYS_INLINE bool val_is_sym(Val v)  { return (v & TAG_MASK) == TAG_SYM; }
ALWAYS_INLINE bool val_is_kw(Val v)   { return (v & TAG_MASK) == TAG_KW; }
ALWAYS_INLINE bool val_is_str(Val v)  { return (v & TAG_MASK) == TAG_STR; }
ALWAYS_INLINE bool val_is_pmap(Val v) { return (v & TAG_MASK) == TAG_PMAP; }
ALWAYS_INLINE bool val_is_pvec(Val v) { return (v & TAG_MASK) == TAG_PVEC; }
ALWAYS_INLINE bool val_is_fn(Val v)   { return (v & TAG_MASK) == TAG_FN; }
ALWAYS_INLINE bool val_is_cons(Val v) { return (v & TAG_MASK) == TAG_CONS; }

ALWAYS_INLINE f64   val_as_f64(Val v)  { f64 d; memcpy(&d, &v, 8); return d; }
ALWAYS_INLINE bool  val_as_bool(Val v) { return (bool)(v & 1); }
ALWAYS_INLINE i64   val_as_int(Val v)  { i64 raw = (i64)(v & VAL_MASK); return (raw << 16) >> 16; }
ALWAYS_INLINE StrId val_as_sym(Val v)  { return (StrId)(v & VAL_MASK); }
ALWAYS_INLINE StrId val_as_kw(Val v)   { return (StrId)(v & VAL_MASK); }
ALWAYS_INLINE Str  *val_as_str(Val v)  { return (Str *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_pmap(Val v) { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_pvec(Val v) { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_fn(Val v)   { return (void *)(u64)(v & VAL_MASK); }
ALWAYS_INLINE void *val_as_cons(Val v) { return (void *)(u64)(v & VAL_MASK); }

ALWAYS_INLINE bool val_truthy(Val v) {
    return !val_is_nil(v) && !(val_is_bool(v) && !val_as_bool(v));
}

#endif // VAL_C_INCLUDED
