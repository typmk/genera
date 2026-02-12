/**
 * image.c — Program as Indexed Data
 *
 * Intern at boundary. Index everywhere. Never search.
 * image[strid] → {calls, callers, arity, body} — O(1) program queries.
 *
 * Depends on: lang/read.c (DefnInfo, classify, is_special, is_builtin)
 */
#ifndef IMAGE_C_INCLUDED
#define IMAGE_C_INCLUDED

// ============================================================================
// 1. ImageEntry — the program database
// ============================================================================

#define IMAGE_CAP 4096

typedef struct {
    bool  defined;
    u32   code_offset;
    u32   n_params;
    Val   params;
    Val   body;

    StrId calls[32];
    u32   n_calls;
    StrId callers[32];
    u32   n_callers;

    bool  is_global;
} ImageEntry;

static ImageEntry g_image[IMAGE_CAP];

// ============================================================================
// 2. Call graph extraction
// ============================================================================

static void extract_calls(Val form, StrId self, ImageEntry *e) {
    if (!val_is_cons(form)) return;
    Val head = car(form);
    if (val_is_sym(head)) {
        StrId sym = val_as_sym(head);
        if (!is_special(sym) && !is_builtin(sym) && sym != self && e->n_calls < 32) {
            bool dup = false;
            for (u32 i = 0; i < e->n_calls; i++)
                if (e->calls[i] == sym) { dup = true; break; }
            if (!dup) e->calls[e->n_calls++] = sym;
        }
    }
    Val f = form;
    while (val_is_cons(f)) {
        if (val_is_cons(car(f))) extract_calls(car(f), self, e);
        f = cdr(f);
    }
}

// ============================================================================
// 3. Image builder — call after classify()
// ============================================================================

static void image_build(void) {
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId name = g_defns[i].name;
        if (name >= IMAGE_CAP) continue;
        ImageEntry *e = &g_image[name];
        e->defined = true;
        e->n_params = g_defns[i].n_params;
        e->params = g_defns[i].params;
        e->body = g_defns[i].body;
        e->is_global = false;
        e->n_calls = 0;
        e->n_callers = 0;
        Val body = g_defns[i].body;
        while (val_is_cons(body)) {
            extract_calls(car(body), name, e);
            body = cdr(body);
        }
    }
    for (u32 i = 0; i < g_def_count; i++) {
        StrId name = g_defs[i].name;
        if (name >= IMAGE_CAP) continue;
        ImageEntry *e = &g_image[name];
        e->defined = true;
        e->is_global = true;
        e->n_params = 0;
    }
    // Reverse index: callers
    for (u32 i = 0; i < g_defn_count; i++) {
        StrId caller = g_defns[i].name;
        if (caller >= IMAGE_CAP) continue;
        ImageEntry *ce = &g_image[caller];
        for (u32 j = 0; j < ce->n_calls; j++) {
            StrId callee = ce->calls[j];
            if (callee >= IMAGE_CAP) continue;
            ImageEntry *ee = &g_image[callee];
            if (ee->n_callers < 32) {
                bool dup = false;
                for (u32 k = 0; k < ee->n_callers; k++)
                    if (ee->callers[k] == caller) { dup = true; break; }
                if (!dup) ee->callers[ee->n_callers++] = caller;
            }
        }
    }
}

// ============================================================================
// 4. O(1) queries
// ============================================================================

ALWAYS_INLINE ImageEntry *image_get(StrId name) {
    return (name < IMAGE_CAP) ? &g_image[name] : NULL;
}

ALWAYS_INLINE bool image_defined(StrId name) {
    return name < IMAGE_CAP && g_image[name].defined;
}

ALWAYS_INLINE u32 image_arity(StrId name) {
    return name < IMAGE_CAP ? g_image[name].n_params : 0;
}

ALWAYS_INLINE StrId *image_calls(StrId name, u32 *n) {
    if (name >= IMAGE_CAP) { *n = 0; return NULL; }
    *n = g_image[name].n_calls;
    return g_image[name].calls;
}

ALWAYS_INLINE StrId *image_callers(StrId name, u32 *n) {
    if (name >= IMAGE_CAP) { *n = 0; return NULL; }
    *n = g_image[name].n_callers;
    return g_image[name].callers;
}

ALWAYS_INLINE i32 image_offset(StrId name) {
    return (name < IMAGE_CAP && g_image[name].defined) ? (i32)g_image[name].code_offset : -1;
}

#endif // IMAGE_C_INCLUDED
