/**
 * linux/x86_64.c — Linux x86-64 Syscalls + Entry Point
 *
 * TRULY FREESTANDING: no #include, no libc, no CRT.
 * Depends only on types.c for integer types.
 */
#ifndef LINUX_X86_64_C_INCLUDED
#define LINUX_X86_64_C_INCLUDED

// ============================================================================
// 1. Syscall Numbers
// ============================================================================

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
#define SYS_nanosleep     35
#define SYS_poll          7
#define SYS_inotify_init1 294
#define SYS_ioctl         16
#define SYS_inotify_add_watch 254
#define SYS_perf_event_open   298

// ============================================================================
// 2. Kernel Constants
// ============================================================================

#define PROT_READ    1
#define PROT_WRITE   2
#define PROT_EXEC    4
#define MAP_PRIVATE  0x02
#define MAP_SHARED   0x01
#define MAP_ANON     0x20
#define O_RDONLY     0
#define O_WRONLY     1
#define O_CREAT      0x40
#define O_TRUNC      0x200
#define CLOCK_MONO   1
#define SEEK_END     2
#define SEEK_SET     0
#define PAGE_SIZE    4096

// inotify
#define IN_CLOSE_WRITE 0x00000008
#define IN_CREATE      0x00000100
#define IN_CLOEXEC     0x00080000

// terminal
#define TIOCGWINSZ     0x5413
#define TCGETS         0x5401
#define TCSETS         0x5402

// termios flags
#define TF_BRKINT  0x0002
#define TF_ICRNL   0x0100
#define TF_INPCK   0x0010
#define TF_ISTRIP  0x0020
#define TF_IXON    0x0400
#define TF_CS8     0x0030
#define TF_ECHO    0x0008
#define TF_ICANON  0x0002
#define TF_IEXTEN  0x8000
#define TF_ISIG    0x0001
#define TF_VMIN    6
#define TF_VTIME   5

// poll
#define POLLIN         0x0001

// ============================================================================
// 3. Raw Syscall Wrappers
//
// ABI: rax=NR, args in rdi,rsi,rdx,r10,r8,r9. Returns rax. Clobbers rcx,r11.
// ============================================================================

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
static inline i64 _sc5(i64 nr, i64 a1, i64 a2, i64 a3, i64 a4, i64 a5) {
    i64 r;
    register i64 r10 __asm__("r10") = a4;
    register i64 r8  __asm__("r8")  = a5;
    __asm__ volatile("syscall" : "=a"(r) : "a"(nr), "D"(a1), "S"(a2), "d"(a3), "r"(r10), "r"(r8) : "rcx","r11","memory");
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
// 4. Exit — needed by _start, defined here (raw syscall)
// ============================================================================

static inline void sys_exit(int code) { _sc1(SYS_exit_group, code); __builtin_unreachable(); }

// ============================================================================
// 5. Entry Point — no CRT
//
// Linux x86-64 initial stack: [argc, argv..., NULL, envp..., NULL]
// We extract argc/argv/envp and call main().
// ============================================================================

static char **_sys_environ;

void _start_c(long *sp) {
    int argc = (int)sp[0];
    char **argv = (char **)(sp + 1);
    _sys_environ = (char **)(sp + 1 + argc + 1);
    extern int main(int, char **);
    sys_exit(main(argc, argv));
}

__attribute__((naked, noreturn, used)) void _start(void) {
    __asm__ volatile(
        "xor %%rbp, %%rbp\n"
        "mov %%rsp, %%rdi\n"
        "and $-16, %%rsp\n"
        "call _start_c\n"
        ::: "memory"
    );
}

#endif // LINUX_X86_64_C_INCLUDED
