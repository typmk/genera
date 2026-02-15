/**
 * api.c — OS-Agnostic System API
 *
 * Convenience wrappers over raw syscalls.
 * Depends on: types.c, <os>/<arch>.c (for _sc* and constants)
 */
#ifndef SYS_API_C_INCLUDED
#define SYS_API_C_INCLUDED

// ============================================================================
// 1. Syscall API
// ============================================================================

static inline i64  sys_read(int fd, void *buf, u64 n)  { return _sc3(SYS_read, fd, (i64)buf, n); }
static inline i64  sys_write(int fd, const void *buf, u64 n) { return _sc3(SYS_write, fd, (i64)buf, n); }
static inline int  sys_open(const char *path, int flags, int mode) { return (int)_sc3(SYS_open, (i64)path, flags, mode); }
static inline int  sys_close(int fd) { return (int)_sc1(SYS_close, fd); }
static inline i64  sys_lseek(int fd, i64 off, int whence) { return _sc3(SYS_lseek, fd, off, whence); }

static inline void *sys_mmap(void *addr, u64 len, int prot, int flags, int fd, i64 off) {
    return (void *)_sc6(SYS_mmap, (i64)addr, len, prot, flags, fd, off);
}
static inline int sys_munmap(void *addr, u64 len) { return (int)_sc2(SYS_munmap, (i64)addr, len); }

static inline int sys_fork(void)   { return (int)_sc1(SYS_fork, 0); }
static inline int sys_pipe(int *fds) { return (int)_sc1(SYS_pipe, (i64)fds); }
static inline int sys_dup2(int old, int new_fd) { return (int)_sc2(SYS_dup2, old, new_fd); }
static inline int sys_execve(const char *path, char *const argv[], char *const envp[]) {
    return (int)_sc3(SYS_execve, (i64)path, (i64)argv, (i64)envp);
}
static inline int sys_waitpid(int pid, int *status, int opts) {
    return (int)_sc4(SYS_wait4, pid, (i64)status, opts, 0);
}

// ============================================================================
// 1b. SysOp Table — unified data for runtime + JIT + introspection
// ============================================================================

typedef struct { u16 nr; u8 nargs; } SysOp;

enum {
    SC_READ, SC_WRITE, SC_OPEN, SC_CLOSE, SC_LSEEK, SC_MMAP, SC_MUNMAP,
    SC_FORK, SC_PIPE, SC_DUP2, SC_EXECVE, SC_WAITPID, SC_EXIT,
    SC_CLOCK, SC_NANOSLEEP, SC_POLL, SC_IOCTL, SC_COUNT
};

static const SysOp SYSOPS[SC_COUNT] = {
    [SC_READ]      = {SYS_read,          3},
    [SC_WRITE]     = {SYS_write,         3},
    [SC_OPEN]      = {SYS_open,          3},
    [SC_CLOSE]     = {SYS_close,         1},
    [SC_LSEEK]     = {SYS_lseek,         3},
    [SC_MMAP]      = {SYS_mmap,          6},
    [SC_MUNMAP]    = {SYS_munmap,        2},
    [SC_FORK]      = {SYS_fork,          1},
    [SC_PIPE]      = {SYS_pipe,          1},
    [SC_DUP2]      = {SYS_dup2,          2},
    [SC_EXECVE]    = {SYS_execve,        3},
    [SC_WAITPID]   = {SYS_wait4,         4},
    [SC_EXIT]      = {SYS_exit_group,    1},
    [SC_CLOCK]     = {SYS_clock_gettime, 2},
    [SC_NANOSLEEP] = {SYS_nanosleep,     2},
    [SC_POLL]      = {SYS_poll,          3},
    [SC_IOCTL]     = {SYS_ioctl,         3},
};

// Generic dispatcher — table-driven syscall (for introspection, not hot path)
static i64 sys_call(u32 sc, i64 a1, i64 a2, i64 a3, i64 a4, i64 a5, i64 a6) {
    if (sc >= SC_COUNT) return -1;
    switch (SYSOPS[sc].nargs) {
    case 1: return _sc1(SYSOPS[sc].nr, a1);
    case 2: return _sc2(SYSOPS[sc].nr, a1, a2);
    case 3: return _sc3(SYSOPS[sc].nr, a1, a2, a3);
    case 4: return _sc4(SYSOPS[sc].nr, a1, a2, a3, a4);
    case 5: return _sc5(SYSOPS[sc].nr, a1, a2, a3, a4, a5);
    case 6: return _sc6(SYSOPS[sc].nr, a1, a2, a3, a4, a5, a6);
    default: return -1;
    }
}

// ============================================================================
// 2. Memory
// ============================================================================

#define PAGE_ALIGN(x) (((x) + (PAGE_SIZE - 1)) & ~(PAGE_SIZE - 1))

static inline void *sys_alloc(u64 size) {
    size = PAGE_ALIGN(size);
    void *p = sys_mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
    return ((i64)p < 0) ? NULL : p;
}

static inline void sys_free(void *ptr, u64 size) {
    sys_munmap(ptr, PAGE_ALIGN(size));
}

static inline void *sys_alloc_exec(u64 size) {
    size = PAGE_ALIGN(size);
    void *p = sys_mmap(NULL, size, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
    return ((i64)p < 0) ? NULL : p;
}

static inline void sys_free_exec(void *ptr, u64 size) {
    sys_munmap(ptr, PAGE_ALIGN(size));
}

// ============================================================================
// 3. Time
// ============================================================================

struct timespec { i64 tv_sec; i64 tv_nsec; };

static inline u64 sys_time_ns(void) {
    struct timespec ts;
    _sc2(SYS_clock_gettime, CLOCK_MONO, (i64)&ts);
    return (u64)ts.tv_sec * 1000000000ULL + (u64)ts.tv_nsec;
}

// ============================================================================
// 4. Process (for C backend → gcc)
// ============================================================================

static int sys_run_capture(const char *path, char *const argv[],
                           char *out, u32 out_cap, u32 *out_len) {
    int pfd[2];
    if (sys_pipe(pfd) < 0) return -1;

    int pid = sys_fork();
    if (pid < 0) { sys_close(pfd[0]); sys_close(pfd[1]); return -1; }

    if (pid == 0) {
        sys_close(pfd[0]);
        sys_dup2(pfd[1], 1);
        sys_dup2(pfd[1], 2);
        sys_close(pfd[1]);
        sys_execve(path, argv, _sys_environ);
        sys_exit(127);
    }

    sys_close(pfd[1]);
    u32 total = 0;
    while (total < out_cap - 1) {
        i64 n = sys_read(pfd[0], out + total, out_cap - 1 - total);
        if (n <= 0) break;
        total += (u32)n;
    }
    out[total] = '\0';
    *out_len = total;
    sys_close(pfd[0]);

    int status = 0;
    sys_waitpid(pid, &status, 0);
    return (status >> 8) & 0xFF;
}

static int sys_run(const char *path, char *const argv[]) {
    int pid = sys_fork();
    if (pid < 0) return -1;
    if (pid == 0) { sys_execve(path, argv, _sys_environ); sys_exit(127); }
    int status = 0;
    sys_waitpid(pid, &status, 0);
    return (status >> 8) & 0xFF;
}

// ============================================================================
// 5. File I/O
// ============================================================================

typedef struct { char *data; u32 len; } FileData;

static FileData sys_read_file(const char *path, void *(*alloc_fn)(u64)) {
    int fd = sys_open(path, O_RDONLY, 0);
    if (fd < 0) return (FileData){NULL, 0};
    i64 size = sys_lseek(fd, 0, SEEK_END);
    sys_lseek(fd, 0, SEEK_SET);
    if (size <= 0) { sys_close(fd); return (FileData){NULL, 0}; }
    char *buf = (char *)alloc_fn((u64)size + 1);
    if (!buf) { sys_close(fd); return (FileData){NULL, 0}; }
    i64 rd = sys_read(fd, buf, (u64)size);
    sys_close(fd);
    buf[rd] = '\0';
    return (FileData){buf, (u32)rd};
}

static bool sys_write_file(const char *path, const void *data, u32 len) {
    int fd = sys_open(path, O_WRONLY|O_CREAT|O_TRUNC, 0644);
    if (fd < 0) return false;
    sys_write(fd, data, len);
    sys_close(fd);
    return true;
}

// ============================================================================
// 6. Terminal
// ============================================================================

static inline bool sys_isatty(int fd) {
    u16 ws[4];
    return _sc3(SYS_ioctl, fd, TIOCGWINSZ, (i64)ws) == 0;
}

static inline u32 sys_term_width(void) {
    u16 ws[4];
    if (_sc3(SYS_ioctl, 1, TIOCGWINSZ, (i64)ws) == 0 && ws[1] > 0) return ws[1];
    return 80;
}

static inline void sys_term_size(u32 *rows, u32 *cols) {
    u16 ws[4];
    if (_sc3(SYS_ioctl, 1, TIOCGWINSZ, (i64)ws) == 0) {
        *rows = ws[0]; *cols = ws[1];
    } else { *rows = 24; *cols = 80; }
}

// Termios — kernel struct (NOT glibc). 4×u32 + u8 + u8[19] = 36 bytes.
typedef struct {
    u32 c_iflag, c_oflag, c_cflag, c_lflag;
    u8  c_line;
    u8  c_cc[19];
} Termios;

static Termios g_orig_term;
static bool    g_raw_mode;

static void sys_term_raw(void) {
    if (g_raw_mode) return;
    _sc3(SYS_ioctl, 0, TCGETS, (i64)&g_orig_term);
    Termios raw = g_orig_term;
    raw.c_iflag &= ~(u32)(TF_BRKINT | TF_ICRNL | TF_INPCK | TF_ISTRIP | TF_IXON);
    // Keep OPOST so \n → \r\n in output
    raw.c_cflag |= TF_CS8;
    raw.c_lflag &= ~(u32)(TF_ECHO | TF_ICANON | TF_IEXTEN | TF_ISIG);
    raw.c_cc[TF_VMIN] = 0;
    raw.c_cc[TF_VTIME] = 1;  // 100ms read timeout
    _sc3(SYS_ioctl, 0, TCSETS, (i64)&raw);
    g_raw_mode = true;
}

static void sys_term_cooked(void) {
    if (!g_raw_mode) return;
    _sc3(SYS_ioctl, 0, TCSETS, (i64)&g_orig_term);
    g_raw_mode = false;
}

// ============================================================================
// 7. inotify + poll + sleep
// ============================================================================

static inline int sys_inotify_init(void) {
    return (int)_sc1(SYS_inotify_init1, IN_CLOEXEC);
}
static inline int sys_inotify_add_watch(int fd, const char *path, u32 mask) {
    return (int)_sc3(SYS_inotify_add_watch, fd, (i64)path, mask);
}

struct pollfd { int fd; i16 events; i16 revents; };
static inline int sys_poll(struct pollfd *fds, u32 nfds, int timeout_ms) {
    return (int)_sc3(SYS_poll, (i64)fds, nfds, timeout_ms);
}

static inline int sys_nanosleep(u64 ns) {
    struct timespec ts = {(i64)(ns / 1000000000ULL), (i64)(ns % 1000000000ULL)};
    return (int)_sc2(SYS_nanosleep, (i64)&ts, 0);
}

// ============================================================================
// 8. Hardware Performance Counters (perf_event_open)
//
// Zero library dependency — just syscall + read.
// Counters run in hardware at zero software cost.
// ============================================================================

// perf_event_attr (kernel struct, simplified — only fields we use)
typedef struct {
    u32 type;           // PERF_TYPE_*
    u32 size;           // sizeof(struct)
    u64 config;         // PERF_COUNT_HW_*
    u64 sample_period;
    u64 sample_type;
    u64 read_format;
    u64 flags;          // disabled, exclude_kernel, exclude_hv
    u32 wakeup_events;
    u32 bp_type;
    u64 bp_config;
    u64 bp_len;
    u64 branch_sample;
    u64 sample_regs;
    u64 sample_stack;
    u64 clockid;
    u64 sample_regs2;
    u64 aux_watermark;
    u32 sample_max_stack;
    u32 __reserved_2;
} PerfAttr;

#define PERF_TYPE_HARDWARE  0
#define PERF_TYPE_HW_CACHE  3

// Hardware event IDs
#define PERF_HW_CYCLES          0
#define PERF_HW_INSTRUCTIONS    1
#define PERF_HW_CACHE_MISSES    3
#define PERF_HW_BRANCH_MISSES   5

// Cache event encoding: (cache_id) | (op << 8) | (result << 16)
#define PERF_L1D_READ_MISS  ((0) | (0 << 8) | (1 << 16))  // L1-dcache read miss
#define PERF_LLC_READ_MISS  ((2) | (0 << 8) | (1 << 16))  // LLC read miss

// perf_event_open flags
#define PERF_FLAG_DISABLED       (1ULL << 0)
#define PERF_FLAG_EXCLUDE_KERNEL (1ULL << 1)
#define PERF_FLAG_EXCLUDE_HV     (1ULL << 2)

static int perf_open(u32 type, u64 config) {
    PerfAttr attr = {0};
    attr.type = type;
    attr.size = sizeof(PerfAttr);
    attr.config = config;
    attr.flags = PERF_FLAG_DISABLED | PERF_FLAG_EXCLUDE_KERNEL | PERF_FLAG_EXCLUDE_HV;
    return (int)_sc5(SYS_perf_event_open, (i64)&attr, 0, -1, -1, 0);
}

// ioctl constants for perf
#define PERF_IOC_ENABLE  0x2400
#define PERF_IOC_DISABLE 0x2401
#define PERF_IOC_RESET   0x2403

static inline void perf_enable(int fd)  { _sc3(SYS_ioctl, fd, PERF_IOC_RESET, 0); _sc3(SYS_ioctl, fd, PERF_IOC_ENABLE, 0); }
static inline void perf_disable(int fd) { _sc3(SYS_ioctl, fd, PERF_IOC_DISABLE, 0); }
static inline u64  perf_read(int fd)    { u64 v = 0; sys_read(fd, &v, 8); return v; }

// Convenience: open a set of counters
#define PERF_MAX 6
typedef struct {
    int  fd[PERF_MAX];
    u64  val[PERF_MAX];
    u32  n;
} PerfCounters;

static PerfCounters perf_init(void) {
    PerfCounters pc = {.n = 0};
    // Try to open each counter — some may fail (WSL, containers, etc.)
    struct { u32 type; u64 config; } wanted[] = {
        {PERF_TYPE_HARDWARE, PERF_HW_CYCLES},
        {PERF_TYPE_HARDWARE, PERF_HW_INSTRUCTIONS},
        {PERF_TYPE_HARDWARE, PERF_HW_BRANCH_MISSES},
        {PERF_TYPE_HARDWARE, PERF_HW_CACHE_MISSES},
        {PERF_TYPE_HW_CACHE, PERF_L1D_READ_MISS},
        {PERF_TYPE_HW_CACHE, PERF_LLC_READ_MISS},
    };
    for (u32 i = 0; i < 6; i++) {
        int fd = perf_open(wanted[i].type, wanted[i].config);
        if (fd >= 0) {
            pc.fd[pc.n] = fd;
            pc.val[pc.n] = 0;
            pc.n++;
        }
    }
    return pc;
}

static void perf_start(PerfCounters *pc) {
    for (u32 i = 0; i < pc->n; i++) perf_enable(pc->fd[i]);
}
static void perf_stop(PerfCounters *pc) {
    for (u32 i = 0; i < pc->n; i++) {
        perf_disable(pc->fd[i]);
        pc->val[i] = perf_read(pc->fd[i]);
    }
}
static void perf_close(PerfCounters *pc) {
    for (u32 i = 0; i < pc->n; i++) sys_close(pc->fd[i]);
    pc->n = 0;
}

#endif // SYS_API_C_INCLUDED
