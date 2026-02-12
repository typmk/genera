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
        { tap_on(); pf("  tap: on\n"); }
    else if (args.len >= 3 && args.data[0] == 'o' && args.data[1] == 'f')
        { tap_off(); pf("  tap: off\n"); }
    else if (args.len >= 1 && args.data[0] == 'r')
        { tap_reset(); pf("  tap: reset\n"); }
    else
        pf("  tap: %s  (usage: tap on|off|reset)\n", g_tap_on ? "on" : "off");
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
// 2. REPL
// ============================================================================

static bool g_repl_quit;

static bool repl_dispatch(Str line) {
    line = str_trim(line);
    if (!line.len) return true;

    u32 sp = 0;
    while (sp < line.len && line.data[sp] != ' ' && line.data[sp] != '\t') sp++;
    Str cmd_name = {line.data, sp};
    Str cmd_args = {line.data + sp, line.len - sp};
    cmd_args = str_trim(cmd_args);

    if (str_eq(cmd_name, STR_LIT("q")) || str_eq(cmd_name, STR_LIT("quit"))
        || str_eq(cmd_name, STR_LIT("exit")))
        return false;

    StrId id = str_intern(cmd_name);
    for (u32 i = 0; i < g_cmd_count; i++) {
        if (g_cmds[i].name == id) {
            g_cmds[i].fn(cmd_args);
            return true;
        }
    }
    pf("  unknown: ");
    for (u32 i = 0; i < cmd_name.len; i++) buf_c(&g_print_buf, cmd_name.data[i]);
    pf(" (try: help)\n");
    return true;
}

static void repl(void) {
    char buf[4096];
    u32 buf_len = 0;
    g_repl_quit = false;
    pf("> ");
    print_flush();
    while (!g_repl_quit) {
        i64 n = sys_read(0, buf + buf_len, sizeof(buf) - 1 - buf_len);
        if (n <= 0) {
            if (buf_len > 0) {
                buf[buf_len] = 0;
                repl_dispatch((Str){(u8 *)buf, buf_len});
            }
            break;
        }
        buf_len += (u32)n;
        buf[buf_len] = 0;

        u32 start = 0;
        for (u32 i = 0; i < buf_len && !g_repl_quit; i++) {
            if (buf[i] == '\n') {
                buf[i] = 0;
                u32 end = i;
                if (end > start && buf[end-1] == '\r') { buf[end-1] = 0; end--; }
                Str line = {(u8 *)buf + start, end - start};
                if (!repl_dispatch(line)) { g_repl_quit = true; break; }
                pf("> ");
                print_flush();
                start = i + 1;
            }
        }
        if (start > 0) {
            buf_len -= start;
            if (buf_len > 0) memcpy(buf, buf + start, buf_len);
        }
    }
    print_flush();
}

// ============================================================================
// 3. Init / Cleanup
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
    g_temp = arena_create(64 * 1024);
    g_req  = arena_create(1 << 20);
    g_perm = arena_create(1 << 20);
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
