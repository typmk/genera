/**
 * watch.c — File Watcher
 *
 * Watches src/c/ for .c changes, rebuilds + runs tests.
 * Uses Linux inotify for instant notification.
 *
 * ./gna watch
 *
 * Depends on: everything (rebuilds the binary)
 */
#ifndef WATCH_C_INCLUDED
#define WATCH_C_INCLUDED

// ============================================================================
// 1. inotify event layout
// ============================================================================

typedef struct {
    i32 wd;
    u32 mask;
    u32 cookie;
    u32 len;
    // char name[] follows
} InotifyEvent;

#define INOTIFY_EV_SIZE 16   // sizeof(InotifyEvent) header

// ============================================================================
// 2. Watch state
// ============================================================================

#define WATCH_MAX 16

static struct {
    int fd;
    int wd[WATCH_MAX];
    const char *dirs[WATCH_MAX];
    u32 n;
} g_watch;

static bool watch_add(const char *dir) {
    if (g_watch.n >= WATCH_MAX) return false;
    int wd = sys_inotify_add_watch(g_watch.fd, dir, IN_CLOSE_WRITE | IN_CREATE);
    if (wd < 0) return false;
    g_watch.dirs[g_watch.n] = dir;
    g_watch.wd[g_watch.n] = wd;
    g_watch.n++;
    return true;
}

// ============================================================================
// 3. File classification
// ============================================================================

static bool is_c_file(const char *name) {
    u32 len = strlen(name);
    return len > 2 && name[len - 2] == '.' && name[len - 1] == 'c';
}

static const char *watch_dir_for_wd(int wd) {
    for (u32 i = 0; i < g_watch.n; i++)
        if (g_watch.wd[i] == wd) return g_watch.dirs[i];
    return "?";
}

// ============================================================================
// 4. Drain pending events
// ============================================================================

static void watch_drain(void) {
    char buf[4096];
    struct pollfd pfd = {g_watch.fd, POLLIN, 0};
    while (sys_poll(&pfd, 1, 0) > 0 && (pfd.revents & POLLIN)) {
        sys_read(g_watch.fd, buf, sizeof(buf));
        pfd.revents = 0;
    }
}

// ============================================================================
// 5. Rebuild + test
// ============================================================================

static void watch_rebuild(void) {
    u64 t0 = sys_time_ns();

    // Build
    char out[8192];
    u32 out_len = 0;
    char *argv[] = {"/bin/sh", "-c",
        "gcc -O3 -march=native -nostdlib -static -o gna src/c/genera.c 2>&1",
        NULL};
    int rc = sys_run_capture("/bin/sh", argv, out, sizeof(out), &out_len);

    u64 ms = (sys_time_ns() - t0) / 1000000;

    if (rc != 0) {
        pf("    build FAIL (%llu ms)\n", (unsigned long long)ms);
        if (out_len) sys_write(1, out, out_len);
        print_flush();
        return;
    }
    pf("    build ok (%llu ms)\n", (unsigned long long)ms);
    print_flush();

    // Test
    t0 = sys_time_ns();
    char *targ[] = {"./gna", "test", NULL};
    rc = sys_run_capture("./gna", targ, out, sizeof(out), &out_len);
    ms = (sys_time_ns() - t0) / 1000000;

    if (out_len) sys_write(1, out, out_len);
    pf("    (%llu ms)\n", (unsigned long long)ms);
    print_flush();
}

// ============================================================================
// 6. Event loop
// ============================================================================

static int watch_run(void) {
    g_watch.fd = sys_inotify_init();
    if (g_watch.fd < 0) {
        pf("inotify_init failed\n");
        return 1;
    }

    watch_add("src/c/sys");
    watch_add("src/c/sys/linux");
    watch_add("src/c/std");
    watch_add("src/c/lang");

    pf("watching %u dirs for .c changes... (ctrl-c to stop)\n", g_watch.n);
    print_flush();

    char buf[4096];
    while (true) {
        i64 n = sys_read(g_watch.fd, buf, sizeof(buf));
        if (n <= 0) break;

        // Scan for .c file events
        bool changed = false;
        const char *last_file = NULL;
        const char *last_dir = NULL;
        u32 pos = 0;
        while (pos + INOTIFY_EV_SIZE <= (u32)n) {
            InotifyEvent *ev = (InotifyEvent *)(buf + pos);
            u32 ev_size = INOTIFY_EV_SIZE + ev->len;
            if (pos + ev_size > (u32)n) break;
            if (ev->len > 0) {
                char *name = buf + pos + INOTIFY_EV_SIZE;
                if (is_c_file(name)) {
                    changed = true;
                    last_file = name;
                    last_dir = watch_dir_for_wd(ev->wd);
                }
            }
            pos += ev_size;
        }

        if (!changed) continue;

        // Debounce: wait 100ms, drain queued events
        sys_nanosleep(100000000ULL);
        watch_drain();

        pf("\n  %s/%s\n", last_dir, last_file ? last_file : "?");
        watch_rebuild();
    }

    sys_close(g_watch.fd);
    return 0;
}

#endif // WATCH_C_INCLUDED
