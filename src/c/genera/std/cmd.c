/**
 * cmd.c — Command Registry + REPL + Init/Cleanup
 *
 * Commands = StrId -> function pointer. REPL reads stdin, dispatches.
 * Depends on: all base/ components
 */
#ifndef CMD_C_INCLUDED
#define CMD_C_INCLUDED

// ============================================================================
// 1. Command Registry
// ============================================================================

typedef void (*CmdFn)(Str args);

typedef struct {
    StrId name;
    CmdFn fn;
    const char *help;
} Cmd;

#define CMD_CAP 64
static Cmd g_cmds[CMD_CAP];
static u32 g_cmd_count;

static void cmd_register(const char *name, CmdFn fn, const char *help) {
    if (g_cmd_count >= CMD_CAP) return;
    Str s = {(u8 *)name, (u32)strlen(name)};
    g_cmds[g_cmd_count++] = (Cmd){str_intern(s), fn, help};
}

// Built-in commands
static void cmd_help(Str args) {
    (void)args;
    pf("commands:\n");
    for (u32 i = 0; i < g_cmd_count; i++) {
        Str n = str_from_id(g_cmds[i].name);
        pf("  ");
        for (u32 j = 0; j < n.len; j++) buf_c(&g_print_buf, n.data[j]);
        pf(" — %s\n", g_cmds[i].help);
    }
}

static void cmd_trace(Str args) {
    u32 n = 20;
    if (args.len) {
        n = 0;
        for (u32 i = 0; i < args.len; i++)
            if (args.data[i] >= '0' && args.data[i] <= '9')
                n = n * 10 + (args.data[i] - '0');
    }
    trace_print(n);
}

static void cmd_tap(Str args) {
    if (args.len >= 2 && args.data[0] == 'o' && args.data[1] == 'n')
        { g_obs_level = 2; tap_on(); pf("  timing: on\n"); }
    else if (args.len >= 3 && args.data[0] == 'o' && args.data[1] == 'f')
        { tap_off(); g_obs_level = 1; pf("  timing: off\n"); }
    else if (args.len >= 1 && args.data[0] == 'r')
        { tap_reset(); obs_reset(); pf("  tap: reset\n"); }
    else
        pf("  tier %u  (usage: tap on|off|reset)\n", g_obs_level);
}

static void cmd_arena(Str args) {
    (void)args;
    ArenaBlock *bt = g_temp.current, *br = g_req.current, *bp = g_perm.current;
    pf("  temp:  %u / %u bytes\n", bt ? bt->used : 0, bt ? bt->size : 0);
    pf("  req:   %u / %u bytes\n", br ? br->used : 0, br ? br->size : 0);
    pf("  perm:  %u / %u bytes\n", bp ? bp->used : 0, bp ? bp->size : 0);
}

static void cmd_intern(Str args) {
    (void)args;
    pf("  intern table: %u / %u entries\n", g_intern.count, INTERN_CAP);
}

// ============================================================================
// 2. Key Input
//
// read_key() PARSES CSI sequences.  csi() in fmt.c EMITS them.
// Same grammar (\033[ params code), opposite direction.
// ============================================================================

enum {
    KEY_NONE = 0,
    KEY_CTRL_A = 1, KEY_CTRL_B = 2, KEY_CTRL_C = 3, KEY_CTRL_D = 4,
    KEY_CTRL_E = 5, KEY_CTRL_F = 6, KEY_CTRL_K = 11, KEY_CTRL_L = 12,
    KEY_CTRL_N = 14, KEY_CTRL_P = 16, KEY_CTRL_T = 20, KEY_CTRL_U = 21,
    KEY_CTRL_W = 23, KEY_CTRL_Y = 25, KEY_CTRL_Z = 26,
    KEY_TAB = 9, KEY_ENTER = 13, KEY_ESC = 27, KEY_BACKSPACE = 127,
    KEY_UP = 300, KEY_DOWN, KEY_RIGHT, KEY_LEFT,
    KEY_HOME, KEY_END, KEY_DELETE, KEY_PGUP, KEY_PGDN,
};

// Data-driven CSI decode tables (parallel to csi() emit codes)
//   ESC [ letter:  A=up B=down C=right D=left F=end H=home
//   ESC [ digit ~: 1=home 3=del 4=end 5=pgup 6=pgdn 7=home 8=end
static const i16 _csi_letter[8] = {
    KEY_UP, KEY_DOWN, KEY_RIGHT, KEY_LEFT, 0, KEY_END, 0, KEY_HOME  // A..H
};
static const i16 _csi_tilde[9] = {
    0, KEY_HOME, 0, KEY_DELETE, KEY_END, KEY_PGUP, KEY_PGDN, KEY_HOME, KEY_END
};

static int read_key(void) {
    u8 c;
    if (sys_read(0, &c, 1) <= 0) return KEY_NONE;
    if (c != 27) return (int)c;

    u8 s[3];
    if (sys_read(0, &s[0], 1) <= 0) return KEY_ESC;
    if (s[0] == '[') {
        if (sys_read(0, &s[1], 1) <= 0) return KEY_ESC;
        if (s[1] >= '0' && s[1] <= '8') {
            if (sys_read(0, &s[2], 1) > 0 && s[2] == '~')
                return _csi_tilde[s[1] - '0'] ?: KEY_ESC;
            return KEY_ESC;
        }
        if (s[1] >= 'A' && s[1] <= 'H') return _csi_letter[s[1] - 'A'] ?: KEY_ESC;
    }
    if (s[0] == 'O') {
        if (sys_read(0, &s[1], 1) <= 0) return KEY_ESC;
        if (s[1] == 'H') return KEY_HOME;
        if (s[1] == 'F') return KEY_END;
    }
    return KEY_ESC;
}

// ============================================================================
// 3. Line Editor — Persistent Buffer (atom model)
//
// THE fundamental: ed_splice(at, remove_n, insert, insert_n).
// All editing ops are splice with different ranges.
//
// The buffer is a VALUE, like atom(pmap). Each edit creates a new version.
// History is a ring of world snapshots: (buffer_state, image_env).
// Buffer is value-copied (1KB, cheap). Env is root-shared (persistent).
//
// Auto-transaction: consecutive same-type edits are one undo unit.
// Undo/redo navigate the version ring. Both buffer and env rewind together.
//
// When CPMap replaces HashMap for env (Phase 2a), the env snapshot becomes
// a CPMap root pointer — same 8 bytes, full structural sharing.
// The buffer could become a CPVec too, but flat copy wins for ≤1KB.
// ============================================================================

#define ED_CAP    1024
#define HIST_CAP  64
#define UNDO_CAP  32

static struct { char buf[ED_CAP]; u32 len, pos; } g_ed;

// --- World snapshots: buffer + image versioned together ---
//
// The world = (buffer, image_env). Both are values.
// Buffer: 1KB flat copy (cheap — fits in cache).
// Image env: root pointer. Parent chain lives in arena (persistent/shared).
// Saving a pointer IS structural sharing — the pmap principle for free.
//
// eval.c sets g_world_env at init. Each def/eval modifies the env.
// Undo restores BOTH buffer and env — true time travel.

typedef struct {
    char  buf[ED_CAP];
    u16   len, pos;
    void *env;         // Env* — image root (parent chain = shared structure)
} WorldSnap;

static WorldSnap g_undo[UNDO_CAP], g_redo[UNDO_CAP];
static u32 g_undo_n, g_redo_n;
static u8  g_ed_tx;   // auto-tx: 0=none/move, 1=insert, 2=delete

// Image env pointer — set by eval_init(), used by eval().
// Opaque here (void*). eval.c knows the real type (Env*).
static void *g_world_env;

static void _snap_save(WorldSnap *s) {
    memcpy(s->buf, g_ed.buf, g_ed.len);
    s->len = (u16)g_ed.len;
    s->pos = (u16)g_ed.pos;
    s->env = g_world_env;
}

static void _snap_restore(WorldSnap *s) {
    memcpy(g_ed.buf, s->buf, s->len);
    g_ed.len = s->len;
    g_ed.pos = s->pos;
    g_world_env = s->env;
}

static void _undo_push(void) {
    if (g_undo_n >= UNDO_CAP) {
        memmove(g_undo, g_undo + 1, (UNDO_CAP - 1) * sizeof(WorldSnap));
        g_undo_n = UNDO_CAP - 1;
    }
    _snap_save(&g_undo[g_undo_n++]);
}

// Checkpoint: save undo point on edit-type transition
static void ed_checkpoint(u8 type) {
    if (type != g_ed_tx) { _undo_push(); g_redo_n = 0; }
    g_ed_tx = type;
}

static bool ed_undo(void) {
    if (!g_undo_n) return false;
    if (g_redo_n < UNDO_CAP) _snap_save(&g_redo[g_redo_n++]);
    _snap_restore(&g_undo[--g_undo_n]);
    g_ed_tx = 0;
    return true;
}

static bool ed_redo(void) {
    if (!g_redo_n) return false;
    _undo_push();
    _snap_restore(&g_redo[--g_redo_n]);
    g_ed_tx = 0;
    return true;
}

static void ed_undo_reset(void) { g_undo_n = 0; g_redo_n = 0; g_ed_tx = 0; }

// History: circular buffer, dedup last
static char g_hist_buf[HIST_CAP][ED_CAP];
static u32  g_hist_len[HIST_CAP];
static u32  g_hist_count;
static i32  g_hist_idx;         // -1 = current, 0..n = browsing
static char g_hist_saved[ED_CAP];
static u32  g_hist_saved_len;

static void hist_add(const char *s, u32 len) {
    if (!len) return;
    if (g_hist_count > 0) {
        u32 prev = (g_hist_count - 1) % HIST_CAP;
        if (g_hist_len[prev] == len && memcmp(g_hist_buf[prev], s, len) == 0) return;
    }
    u32 slot = g_hist_count++ % HIST_CAP;
    memcpy(g_hist_buf[slot], s, len);
    g_hist_len[slot] = len;
}

static void ed_set(const char *s, u32 len) {
    if (len > ED_CAP - 1) len = ED_CAP - 1;
    memcpy(g_ed.buf, s, len);
    g_ed.len = len; g_ed.pos = len;
}

static void hist_navigate(int dir) {
    u32 avail = g_hist_count < HIST_CAP ? g_hist_count : HIST_CAP;
    if (!avail) return;
    if (dir < 0) {
        if (g_hist_idx < 0) {
            memcpy(g_hist_saved, g_ed.buf, g_ed.len);
            g_hist_saved_len = g_ed.len;
            g_hist_idx = 0;
        } else if (g_hist_idx < (i32)avail - 1) g_hist_idx++;
        else return;
    } else {
        if (g_hist_idx > 0) g_hist_idx--;
        else if (g_hist_idx == 0) { g_hist_idx = -1; ed_set(g_hist_saved, g_hist_saved_len); return; }
        else return;
    }
    u32 slot = (g_hist_count - 1 - (u32)g_hist_idx) % HIST_CAP;
    ed_set(g_hist_buf[slot], g_hist_len[slot]);
}

// THE editing primitive: splice buf at `at`, remove `rm`, insert `ins[0..n]`
// Auto-checkpoints on edit-type transition (insert vs delete vs move).
static void ed_splice(u32 at, u32 rm, const char *ins, u32 n) {
    ed_checkpoint(n ? 1 : (rm ? 2 : 0));
    if (at > g_ed.len) at = g_ed.len;
    if (at + rm > g_ed.len) rm = g_ed.len - at;
    i32 delta = (i32)n - (i32)rm;
    if ((i32)g_ed.len + delta >= ED_CAP) return;
    u32 tail = g_ed.len - at - rm;
    if (tail && delta) memmove(g_ed.buf + at + n, g_ed.buf + at + rm, tail);
    if (n) memcpy(g_ed.buf + at, ins, n);
    g_ed.len = (u32)((i32)g_ed.len + delta);
    g_ed.pos = at + n;
}

// Word boundary for Ctrl-W: skip spaces then non-spaces backwards
static u32 ed_word_start(void) {
    u32 p = g_ed.pos;
    while (p > 0 && g_ed.buf[p - 1] == ' ') p--;
    while (p > 0 && g_ed.buf[p - 1] != ' ') p--;
    return p;
}

static void ed_transpose(void) {
    if (g_ed.pos > 0 && g_ed.pos < g_ed.len) {
        char t = g_ed.buf[g_ed.pos - 1];
        g_ed.buf[g_ed.pos - 1] = g_ed.buf[g_ed.pos];
        g_ed.buf[g_ed.pos] = t;
        g_ed.pos++;
    }
}

static void ed_refresh(const char *prompt, u32 pw) {
    OutBuf *b = &g_print_buf;
    buf_c(b, '\r');
    buf_n(b, prompt, (u32)strlen(prompt));
    buf_n(b, g_ed.buf, g_ed.len);
    erase_eol();
    if (g_ed.pos < g_ed.len) { buf_c(b, '\r'); csi(pw + g_ed.pos, 0, 'C'); }
    buf_flush(b, 1);
}

// ============================================================================
// 4. REPL — Interactive + Piped
// ============================================================================

static bool g_repl_quit;

static bool repl_dispatch(Str line) {
    line = str_trim(line);
    if (!line.len) return true;
    u32 sp = 0;
    while (sp < line.len && line.data[sp] != ' ' && line.data[sp] != '\t') sp++;
    Str name = {line.data, sp};
    Str args = str_trim((Str){line.data + sp, line.len - sp});
    if (str_eq(name, STR_LIT("q")) || str_eq(name, STR_LIT("quit"))
        || str_eq(name, STR_LIT("exit")))
        return false;
    StrId id = str_intern(name);
    for (u32 i = 0; i < g_cmd_count; i++)
        if (g_cmds[i].name == id) { g_cmds[i].fn(args); return true; }
    pf("  unknown: ");
    for (u32 i = 0; i < name.len; i++) buf_c(&g_print_buf, name.data[i]);
    pf(" (try: help)\n");
    return true;
}

// Piped: canonical mode, line-at-a-time (for ./gna < script.txt)
static void repl_piped(void) {
    char buf[4096]; u32 len = 0;
    while (!g_repl_quit) {
        i64 n = sys_read(0, buf + len, sizeof(buf) - 1 - len);
        if (n <= 0) { if (len) { buf[len] = 0; repl_dispatch((Str){(u8*)buf, len}); } break; }
        len += (u32)n; buf[len] = 0;
        u32 start = 0;
        for (u32 i = 0; i < len && !g_repl_quit; i++) {
            if (buf[i] == '\n') {
                buf[i] = 0;
                u32 e = i;
                if (e > start && buf[e-1] == '\r') buf[--e] = 0;
                if (!repl_dispatch((Str){(u8*)buf + start, e - start})) g_repl_quit = true;
                start = i + 1;
            }
        }
        if (start) { len -= start; if (len) memmove(buf, buf + start, len); }
    }
}

// Interactive: raw mode, key-at-a-time, emacs bindings
static void repl_interactive(void) {
    static const char *PROMPT       = "> ";
    static const char *PROMPT_COLOR = "\033[1m\033[32m> \033[0m";
    const char *prompt = g_color ? PROMPT_COLOR : PROMPT;
    u32 pw = 2;  // prompt visible width

    sys_term_raw();
    g_ed.len = 0; g_ed.pos = 0; g_hist_idx = -1;
    ed_refresh(prompt, pw);

    while (!g_repl_quit) {
        int k = read_key();
        if (k == KEY_NONE) continue;

        switch (k) {
        case KEY_ENTER:
            pf("\r\n");
            g_ed.buf[g_ed.len] = 0;
            hist_add(g_ed.buf, g_ed.len);
            if (!repl_dispatch((Str){(u8*)g_ed.buf, g_ed.len})) g_repl_quit = true;
            g_ed.len = 0; g_ed.pos = 0; g_hist_idx = -1;
            ed_undo_reset();
            if (!g_repl_quit) ed_refresh(prompt, pw);
            break;
        case KEY_CTRL_D:
            if (!g_ed.len) { pf("\r\n"); g_repl_quit = true; }
            else ed_splice(g_ed.pos, 1, 0, 0);
            break;
        case KEY_CTRL_C:
            g_ed.len = 0; g_ed.pos = 0; g_ed_tx = 0;
            pf("^C\r\n"); ed_refresh(prompt, pw); break;

        // Undo/redo — navigate world versions (buffer + env together)
        case KEY_CTRL_Z: ed_undo(); break;
        case KEY_CTRL_Y: ed_redo(); break;

        // Movement — tx boundary (typing then moving = two undo units)
        case KEY_CTRL_A: case KEY_HOME:  g_ed.pos = 0; g_ed_tx = 0; break;
        case KEY_CTRL_E: case KEY_END:   g_ed.pos = g_ed.len; g_ed_tx = 0; break;
        case KEY_CTRL_B: case KEY_LEFT:  if (g_ed.pos) g_ed.pos--; g_ed_tx = 0; break;
        case KEY_CTRL_F: case KEY_RIGHT: if (g_ed.pos < g_ed.len) g_ed.pos++; g_ed_tx = 0; break;

        // History — tx boundary
        case KEY_CTRL_P: case KEY_UP:   hist_navigate(-1); g_ed_tx = 0; break;
        case KEY_CTRL_N: case KEY_DOWN: hist_navigate(1); g_ed_tx = 0; break;

        // Editing — all via splice (auto-checkpointed)
        case KEY_BACKSPACE: if (g_ed.pos) ed_splice(g_ed.pos - 1, 1, 0, 0); break;
        case KEY_DELETE:    ed_splice(g_ed.pos, 1, 0, 0); break;
        // Kill ops: force tx boundary (always own undo unit)
        case KEY_CTRL_K:    g_ed_tx = 0; ed_splice(g_ed.pos, g_ed.len - g_ed.pos, 0, 0); break;
        case KEY_CTRL_U:    g_ed_tx = 0; ed_splice(0, g_ed.pos, 0, 0); break;
        case KEY_CTRL_W:    g_ed_tx = 0; { u32 w = ed_word_start(); ed_splice(w, g_ed.pos - w, 0, 0); } break;
        case KEY_CTRL_T:    ed_transpose(); break;
        case KEY_CTRL_L:    erase_screen(); csi(1, 1, 'H'); print_flush(); break;
        case KEY_TAB: case KEY_ESC: break;
        default: if (k >= 32 && k < 127) { char c = (char)k; ed_splice(g_ed.pos, 0, &c, 1); } break;
        }

        if (!g_repl_quit && k != KEY_ENTER && k != KEY_CTRL_C)
            ed_refresh(prompt, pw);
    }
    sys_term_cooked();
}

static void repl(void) {
    g_repl_quit = false;
    if (sys_isatty(0) && sys_isatty(1)) repl_interactive();
    else repl_piped();
    print_flush();
}

// ============================================================================
// 5. Init / Cleanup
// ============================================================================

static void repl_init(void) {
    cmd_register("help",   cmd_help,   "list commands");
    cmd_register("trace",  cmd_trace,  "show trace (trace [N])");
    cmd_register("tap",    cmd_tap,    "tap on|off|reset");
    cmd_register("arena",  cmd_arena,  "show arena usage");
    cmd_register("intern", cmd_intern, "show intern table stats");
}

static void base_init(void) {
    print_init();
    g_color = sys_isatty(1);
    g_temp = arena_create(1 << 20);   g_temp.name = "temp";   // 1 MB
    g_req  = arena_create(1 << 20);   g_req.name  = "req";    // 1 MB
    g_perm = arena_create(8 << 20);   g_perm.name = "perm";   // 8 MB
    intern_init();
    repl_init();
}

static void base_cleanup(void) {
    intern_free();
    arena_destroy(&g_temp);
    arena_destroy(&g_req);
    arena_destroy(&g_perm);
}

#endif // CMD_C_INCLUDED
