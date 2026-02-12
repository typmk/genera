/**
 * sys.c — Platform Layer: Types + Linux x86-64 Syscalls
 *
 * THE thin OS abstraction. Nothing above this touches the kernel.
 * Types, core functions, syscall wrappers, convenience alloc/IO.
 *
 * TRULY FREESTANDING: no #include, no libc, no CRT.
 * Build with: gcc -O3 -nostdlib -static -o binary driver.c
 */
#ifndef SYS_C_INCLUDED
#define SYS_C_INCLUDED

// ============================================================================
// 1. Types — LP64: long = pointer = 8 bytes
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

// C23 makes bool a keyword; older standards need this
#if __STDC_VERSION__ < 202311L
typedef _Bool              bool;
#define true  1
#define false 0
#endif
#define NULL  ((void *)0)

_Static_assert(sizeof(u64) == 8, "u64 must be 8 bytes");
_Static_assert(sizeof(i64) == 8, "i64 must be 8 bytes");
_Static_assert(sizeof(f64) == 8, "f64 must be 8 bytes");
_Static_assert(sizeof(void *) == 8, "pointer must be 8 bytes");

// ============================================================================
// 2. Core Functions — real implementations (linker symbols)
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

// ============================================================================
// 3. Linux x86-64 Syscalls
// ============================================================================

// Syscall numbers
#define SYS_read          0
#define SYS_write         1
#define SYS_open          2
#define SYS_close         3
#define SYS_lseek         8
#define SYS_mmap          9
#define SYS_munmap        11
#define SYS_pipe          22
#define SYS_dup2          33
#define SYS_fork          57
#define SYS_execve        59
#define SYS_exit          60
#define SYS_wait4         61
#define SYS_clock_gettime 228
#define SYS_exit_group    231

// Kernel constants
#define PROT_READ    1
#define PROT_WRITE   2
#define PROT_EXEC    4
#define MAP_PRIVATE  0x02
#define MAP_ANON     0x20
#define O_RDONLY     0
#define O_WRONLY     1
#define O_CREAT      0x40
#define O_TRUNC      0x200
#define CLOCK_MONO   1
#define SEEK_END     2
#define SEEK_SET     0
#define PAGE_SIZE    4096

// Raw syscall wrappers
// ABI: rax=NR, args in rdi,rsi,rdx,r10,r8,r9. Returns rax. Clobbers rcx,r11.

static inline i64 _sc1(i64 nr, i64 a1) {
    i64 r;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1) : "rcx","r11","memory");
    return r;
}
static inline i64 _sc2(i64 nr, i64 a1, i64 a2) {
    i64 r;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1), "S"(a2) : "rcx","r11","memory");
    return r;
}
static inline i64 _sc3(i64 nr, i64 a1, i64 a2, i64 a3) {
    i64 r;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1), "S"(a2), "d"(a3) : "rcx","r11","memory");
    return r;
}
static inline i64 _sc4(i64 nr, i64 a1, i64 a2, i64 a3, i64 a4) {
    i64 r;
    register i64 r10 __asm__("r10") = a4;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1), "S"(a2), "d"(a3), "r"(r10) : "rcx","r11","memory");
    return r;
}
static inline i64 _sc6(i64 nr, i64 a1, i64 a2, i64 a3, i64 a4, i64 a5, i64 a6) {
    i64 r;
    register i64 r10 __asm__("r10") = a4;
    register i64 r8  __asm__("r8")  = a5;
    register i64 r9  __asm__("r9")  = a6;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1), "S"(a2), "d"(a3), "r"(r10), "r"(r8), "r"(r9) : "rcx","r11","memory");
    return r;
}

// ============================================================================
// 4. Syscall API
// ============================================================================

static inline i64  sys_read(int fd, void *buf, u64 n)  { return _sc3(SYS_read, fd, (i64)buf, n); }
static inline i64  sys_write(int fd, const void *buf, u64 n) { return _sc3(SYS_write, fd, (i64)buf, n); }
static inline int  sys_open(const char *path, int flags, int mode) { return (int)_sc3(SYS_open, (i64)path, flags, mode); }
static inline int  sys_close(int fd) { return (int)_sc1(SYS_close, fd); }
static inline i64  sys_lseek(int fd, i64 off, int whence) { return _sc3(SYS_lseek, fd, off, whence); }
static inline void sys_exit(int code) { _sc1(SYS_exit_group, code); __builtin_unreachable(); }

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
// 5. Convenience — Memory
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
// 6. Convenience — Time
// ============================================================================

struct timespec { i64 tv_sec; i64 tv_nsec; };

static inline u64 sys_time_ns(void) {
    struct timespec ts;
    _sc2(SYS_clock_gettime, CLOCK_MONO, (i64)&ts);
    return (u64)ts.tv_sec * 1000000000ULL + (u64)ts.tv_nsec;
}

// ============================================================================
// 7. Convenience — Process (for C backend → gcc)
// ============================================================================

static char **_sys_environ;  // set by _start, used by sys_execve

// Run command, capture stdout. Returns exit status.
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

// Run command, no capture. Returns exit status.
static int sys_run(const char *path, char *const argv[]) {
    int pid = sys_fork();
    if (pid < 0) return -1;
    if (pid == 0) { sys_execve(path, argv, _sys_environ); sys_exit(127); }
    int status = 0;
    sys_waitpid(pid, &status, 0);
    return (status >> 8) & 0xFF;
}

// ============================================================================
// 8. File I/O helpers
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
// 9. Entry Point — no CRT
//
// Linux x86-64 initial stack: [argc, argv..., NULL, envp..., NULL]
// We extract argc/argv/envp and call main().
// ============================================================================

void _start_c(long *sp) {
    int argc = (int)sp[0];
    char **argv = (char **)(sp + 1);
    _sys_environ = (char **)(sp + 1 + argc + 1);
    extern int main(int, char **);
    sys_exit(main(argc, argv));
}

__attribute__((naked, noreturn, used)) void _start(void) {
    __asm__ volatile(
        "xor %%rbp, %%rbp\n"       // clear frame pointer (ABI)
        "mov %%rsp, %%rdi\n"        // pass stack pointer to _start_c
        "and $-16, %%rsp\n"         // 16-byte align
        "call _start_c\n"           // _start_c(sp) → calls main → sys_exit
        ::: "memory"
    );
}

#endif // SYS_C_INCLUDED
