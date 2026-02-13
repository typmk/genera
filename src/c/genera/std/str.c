/**
 * str.c — Str + Intern + StrBuild
 *
 * Fat pointer strings, interning table, incremental builder.
 * ZERO-INIT: Str {0} = empty string. StrBuild {0} = discards.
 * Depends on: sys.c, fmt.c, mem.c
 */
#ifndef STR_C_INCLUDED
#define STR_C_INCLUDED

typedef struct { u8 *data; u32 len; } Str;
typedef u32 StrId;

#define STR_EMPTY ((Str){NULL, 0})
#define STR_LIT(s) ((Str){(u8 *)(s), sizeof(s) - 1})

ALWAYS_INLINE bool str_eq(Str a, Str b) {
    if (a.len != b.len) return false;
    if (a.data == b.data) return true;
    return memcmp(a.data, b.data, a.len) == 0;
}

ALWAYS_INLINE Str str_slice(Str s, u32 start, u32 len) {
    if (start >= s.len) return STR_EMPTY;
    return (Str){s.data + start, MIN(len, s.len - start)};
}

ALWAYS_INLINE u32 str_hash(Str s) {
    u32 h = 2166136261u;
    for (u32 i = 0; i < s.len; i++) { h ^= s.data[i]; h *= 16777619u; }
    return h;
}

ALWAYS_INLINE Str str_dup(Arena *a, Str s) {
    u8 *copy = arena_push_n(a, u8, s.len);
    memcpy(copy, s.data, s.len);
    return (Str){copy, s.len};
}

// --- Branchless find ---

// First index of byte, or s.len if not found.
ALWAYS_INLINE u32 str_find(Str s, u8 ch) {
    for (u32 i = 0; i < s.len; i++) if (s.data[i] == ch) return i;
    return s.len;
}

// Last index of byte, or s.len if not found.
ALWAYS_INLINE u32 str_rfind(Str s, u8 ch) {
    for (u32 i = s.len; i > 0; i--) if (s.data[i-1] == ch) return i - 1;
    return s.len;
}

// Index past first occurrence, or 0 if not found.
ALWAYS_INLINE u32 str_after(Str s, u8 ch) {
    for (u32 i = 0; i < s.len; i++) if (s.data[i] == ch) return i + 1;
    return 0;
}

// Index past last occurrence, or 0 if not found.
ALWAYS_INLINE u32 str_after_last(Str s, u8 ch) {
    for (u32 i = s.len; i > 0; i--) if (s.data[i-1] == ch) return i;
    return 0;
}

ALWAYS_INLINE bool str_contains(Str s, u8 ch) { return str_find(s, ch) < s.len; }

ALWAYS_INLINE bool str_starts_with(Str s, Str pre) {
    return s.len >= pre.len && memcmp(s.data, pre.data, pre.len) == 0;
}

ALWAYS_INLINE bool str_ends_with(Str s, Str suf) {
    return s.len >= suf.len && memcmp(s.data + s.len - suf.len, suf.data, suf.len) == 0;
}

static Str str_trim(Str s) {
    while (s.len && (s.data[0] == ' ' || s.data[0] == '\t' || s.data[0] == '\n' || s.data[0] == '\r'))
        { s.data++; s.len--; }
    while (s.len && (s.data[s.len-1] == ' ' || s.data[s.len-1] == '\t' || s.data[s.len-1] == '\n' || s.data[s.len-1] == '\r'))
        s.len--;
    return s;
}

static Str str_cat(Arena *a, Str x, Str y) {
    u8 *buf = arena_push_n(a, u8, x.len + y.len);
    memcpy(buf, x.data, x.len);
    memcpy(buf + x.len, y.data, y.len);
    return (Str){buf, x.len + y.len};
}

// --- StringBuilder: incremental construction on arena ---

typedef struct { u8 *data; u32 len, cap; Arena *arena; } StrBuild;

static StrBuild strbuild(Arena *a, u32 cap) {
    if (cap < 64) cap = 64;
    return (StrBuild){arena_push_n(a, u8, cap), 0, cap, a};
}

static void sb_grow(StrBuild *sb) {
    u32 new_cap = sb->cap * 2;
    u8 *new_data = arena_push_n(sb->arena, u8, new_cap);
    memcpy(new_data, sb->data, sb->len);
    sb->data = new_data;
    sb->cap = new_cap;
}

ALWAYS_INLINE void sb_byte(StrBuild *sb, u8 ch) {
    if (UNLIKELY(sb->len >= sb->cap)) {
        if (!sb->arena) return;  // zero-init: discard
        sb_grow(sb);
    }
    sb->data[sb->len++] = ch;
}

static void sb_str(StrBuild *sb, Str s) {
    if (!s.len) return;
    if (!sb->arena && sb->len + s.len > sb->cap) return;  // zero-init: discard
    while (sb->len + s.len > sb->cap) sb_grow(sb);
    memcpy(sb->data + sb->len, s.data, s.len);
    sb->len += s.len;
}

static void sb_cstr(StrBuild *sb, const char *s) {
    while (*s) sb_byte(sb, (u8)*s++);
}

ALWAYS_INLINE Str sb_finish(StrBuild *sb) {
    return (Str){sb->data, sb->len};
}

// --- Intern Table ---

#define INTERN_CAP       (1 << 16)
#define INTERN_TABLE_CAP (1 << 17)

typedef struct {
    Str *strings;
    u32  count;
    u32 *table;
    u32  table_mask;
} InternTable;

static InternTable g_intern;

static void intern_init(void) {
    g_intern.strings = (Str *)sys_alloc(INTERN_CAP * sizeof(Str));
    g_intern.count = 0;
    g_intern.table = (u32 *)sys_alloc(INTERN_TABLE_CAP * sizeof(u32));
    g_intern.table_mask = INTERN_TABLE_CAP - 1;
}

static void intern_free(void) {
    sys_free(g_intern.strings, INTERN_CAP * sizeof(Str));
    sys_free(g_intern.table, INTERN_TABLE_CAP * sizeof(u32));
}

static StrId str_intern(Str s) {
    u32 h = str_hash(s), slot = h & g_intern.table_mask;
    while (1) {
        u32 entry = g_intern.table[slot];
        if (entry == 0) break;
        u32 id = entry - 1;
        if (str_eq(g_intern.strings[id], s)) return id;
        slot = (slot + 1) & g_intern.table_mask;
    }
    StrId id = g_intern.count++;
    g_intern.strings[id] = str_dup(&g_perm, s);
    g_intern.table[slot] = id + 1;
    return id;
}

ALWAYS_INLINE Str str_from_id(StrId id) { return g_intern.strings[id]; }
ALWAYS_INLINE bool strid_eq(StrId a, StrId b) { return a == b; }

#endif // STR_C_INCLUDED
