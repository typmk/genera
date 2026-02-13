# Syscalls as Grammar

Syscalls are GNodes. Not a reference table — grammar nodes parsed, indexed, and queried
with the same bitmask machinery as everything else in genera.

## The Foundation

One CPU instruction. One register selects the operation. Six registers pass arguments.

```
syscall          ; x86-64   — number in rax
svc #0           ; aarch64  — number in x8
ecall            ; riscv64  — number in a7
```

The kernel maintains `sys_call_table[~335]` — a function pointer array. The syscall
number is an index. Same shape as genera's own dispatch:

```
CPU opcodes:     0x90=NOP  0xC3=RET  0x50=PUSH  ...   (~1500 entries, silicon dispatch)
Syscall opcodes: 0=read    1=write   9=mmap     ...   (~335 entries, kernel dispatch)
genera sig:      S_DEF     S_IF      S_FN       ...   (~60 entries, grammar dispatch)
```

Pattern → output. Three dispatch tables at three scales. Same structure.

## Syscalls as GNodes

The EDN (`sys/syscalls.edn`) is surface syntax. When parsed, each operation becomes GNodes:

```
:read                              →  GNode (NK_MAP)
  :cat :io                         →  GNode (NK_KW)       — view membership
  :api :sys_read                   →  GNode (NK_KW)       — binding name
  :sig "(fd, buf, len) -> i64"     →  GNode (NK_STR_NODE) — type signature
  :nr {:linux/x86_64 0 ...}       →  GNode (NK_MAP)       — platform variants
```

Groups are not a taxonomy imposed from outside. They are **bitmask views** over GNodes:

```
V_IO    — bitmask over {read, write, open, openat, close, lseek}
V_MEM   — bitmask over {mmap, munmap}
V_PROC  — bitmask over {fork, clone, execve, wait4, exit_group, pipe, pipe2, dup2, dup3}
V_TIME  — bitmask over {clock_gettime, nanosleep}
V_TERM  — bitmask over {ioctl}
V_WATCH — bitmask over {inotify_init1, inotify_add_watch, poll, ppoll}
V_PERF  — bitmask over {perf_event_open}
```

Dependencies are **bitmask intersections** — which operations reference nodes in other views:

```
bm_and(V_PROC, V_IO)    →  sys_run_capture uses read + close
bm_and(V_PERF, V_IO)    →  perf_read + perf_close parasitic on io
bm_and(V_PERF, V_TERM)  →  perf_enable/disable via ioctl
bm_and(V_WATCH, V_IO)   →  watch reads events via read, outputs via write
bm_and(V_WATCH, V_TIME) →  watch poll loop uses nanosleep
```

No special syscall subsystem. `gram_parse` → `gram_index` → `view_is` / `bm_and`.
The `syscall` instruction is the kernel's eval walking its own dispatch table.

## Three Foundations

genera uses 21 of ~335 opcodes (6%). These 21 decompose into 3 foundations
that no useful program can avoid, plus 4 optional capabilities.

### 1. Pages — the one way to get memory

```
mmap (9)    → sys_alloc()       → Arena → everything
            → sys_alloc_exec()  → JIT code buffer
munmap (11) → sys_free() / sys_free_exec()
```

Two opcodes. ALL memory flows through these. Arena, intern table, coll pools,
JIT buffer — all ultimately mmap. Root of the entire runtime.

### 2. Bytes — the one way to cross the kernel boundary

```
read (0)    → bytes from fd
write (1)   → bytes to fd
open (2)    → path → fd
close (3)   → release fd
lseek (8)   → position within fd
```

Five opcodes. An fd is a capability handle — an opaque integer the kernel gives you.
Complete algebra: acquire (open), release (close), transfer (read/write), seek (lseek).
The other ~45 io opcodes (pread, writev, sendfile, splice...) are optimized special
cases of these 5. genera is not I/O-bound, so it doesn't need them.

### 3. Process — fork/exec/wait/exit

```
fork (57)        → duplicate process
execve (59)      → replace process image
wait4 (61)       → wait for child
exit_group (231) → terminate all threads
pipe (22)        → create fd pair (parent↔child channel)
dup2 (33)        → redirect fd (stdout → pipe)
```

Six opcodes. Compose into exactly two patterns:

```
sys_run:          fork → execve → wait4
sys_run_capture:  pipe → fork → [dup2 + dup2 + execve] → [read + close + wait4]
```

The ~20 other process opcodes (clone, clone3, waitid, unshare, setns...) handle
threads, containers, namespaces. genera is single-process — fork+exec is sufficient.

## Four Capabilities

Each adds one ability. Each is parasitic on the foundations — it borrows
read/close/ioctl from the byte and terminal primitives.

### 4. Time — when

```
clock_gettime (228) → sys_time_ns()   (monotonic nanoseconds)
nanosleep (35)      → sys_nanosleep() (yield for duration)
```

Two opcodes. Enables benchmarking (tap.c), polling delays (watch.c), observer
timing. The other ~8 time opcodes (timer_create, clock_nanosleep, timerfd...) are
for event-driven timers — genera uses a poll loop instead.

### 5. Terminal — ioctl as meta-opcode

```
ioctl (16) → TCGETS/TCSETS:  raw mode      (REPL)
           → TIOCGWINSZ:     window size    (formatting)
           → PERF_IOC_*:     perf control   (counters)
```

One opcode, but a **second-level dispatch**. The request code in the second
argument selects from thousands of sub-operations. genera uses 6 request codes:

| Request | Value | Purpose |
|---------|-------|---------|
| TCGETS | 0x5401 | Read terminal attributes |
| TCSETS | 0x5402 | Set terminal attributes |
| TIOCGWINSZ | 0x5413 | Get window size |
| PERF_IOC_RESET | 0x2403 | Reset counter |
| PERF_IOC_ENABLE | 0x2400 | Start counter |
| PERF_IOC_DISABLE | 0x2401 | Stop counter |

ioctl is the kernel's escape hatch — anything that doesn't fit elsewhere.
That's why term and perf share an opcode.

### 6. Watch — filesystem events

```
inotify_init1 (294)       → create watch fd
inotify_add_watch (254)   → register path + event mask
poll (7)                  → wait for events on fd
```

Three opcodes. init returns an fd. add subscribes a path. poll blocks until events.
Then **read** (from the io foundation) reads the event structs. Watch depends on
bytes for its data path.

### 7. Perf — hardware counters

```
perf_event_open (298) → open counter → fd
```

One unique opcode. After it returns an fd, everything else reuses foundations:
ioctl (enable/disable/reset), read (counter value), close (release).
Fully parasitic on bytes + terminal.

## Dependency Structure

```
               ┌───────────────────────────────────┐
               │    syscall  (1 instruction)        │
               │    rax = opcode, rdi..r9 = args    │
               └──────────────┬────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
         ┌─────────┐   ┌──────────┐   ┌──────────────┐
         │  PAGES  │   │  BYTES   │   │   PROCESS    │
         │ mmap  9 │   │ read   0 │   │  fork    57  │
         │munmap 11│   │ write  1 │   │  execve  59  │
         └────┬────┘   │ open   2 │   │  wait4   61  │
              │        │ close  3 │   │  exit   231  │
              │        │ lseek  8 │   │  pipe    22  │
              │        └────┬─────┘   │  dup2    33  │
              │             │         └──────┬───────┘
              │  FOUNDATION │                │
         ═════╪═════════════╪════════════════╪═════════
              │ CAPABILITIES│                │
              │             │                │
              │        ┌────┴─────┐          │
              │        │   TIME   │          │
              │        │clock 228 │          │
              │        │sleep  35 │          │
              │        └────┬─────┘          │
              │             │                │
              │    ┌────────┴────────┐       │
              │    │   IOCTL (16)    │       │
              │    │  ┌─────┬─────┐  │       │
              │    │ TERM   │ PERF │  │       │
              │    │TCGETS  │IOC_* │  │       │
              │    └────────┴──────┘  │       │
              │             │         │       │
              │    ┌────────┴──┐  ┌───┴──┐    │
              │    │  WATCH    │  │ PERF │    │
              │    │ init 294  │  │open298│   │
              │    │ add  254  │  └──┬───┘    │
              │    │ poll   7  │     │        │
              │    └──┬────────┘     │        │
              │       ▼              ▼        │
              │    read(0) ◄──── read(0)      │
              │    close(3) ◄── close(3)      │
              └───────────────────────────────┘
                    mmap underlies everything
```

Three foundations form a triangle. Four capabilities hang off the bytes edge,
each borrowing read/close/ioctl. mmap is the absolute root.

## Legacy → Modern Pairs

Five opcodes have deprecated (x86-64 only) and modern (portable) forms.
Same semantic operation, different GNode on different targets:

| Legacy | Modern | Shim |
|--------|--------|------|
| open (2) | openat (257/56/56) | `openat(AT_FDCWD, path, flags, mode)` |
| fork (57) | clone (56/220/220) | `clone(SIGCHLD, 0, 0, 0, 0)` |
| pipe (22) | pipe2 (293/59/59) | `pipe2(fds, 0)` |
| dup2 (33) | dup3 (292/24/24) | `dup3(old, new, 0)` |
| poll (7) | ppoll (271/73/73) | `ppoll(fds, n, &ts, NULL)` |

aarch64 and riscv64 only have the modern forms. x86-64 has both.
genera currently uses legacy on x86-64 (smaller immediate encodings, Phase 1).

## Opcode Table

Each row is a GNode. Groups are views. Numbers are platform-variant child nodes.

| Operation | x86-64 | aarch64 | riscv64 | View | genera API |
|-----------|--------|---------|---------|------|------------|
| read | 0 | 63 | 63 | io | `sys_read` |
| write | 1 | 64 | 64 | io | `sys_write` |
| open | 2 | — | — | io | `sys_open` |
| openat | 257 | 56 | 56 | io | `sys_openat` |
| close | 3 | 57 | 57 | io | `sys_close` |
| lseek | 8 | 62 | 62 | io | `sys_lseek` |
| mmap | 9 | 222 | 222 | mem | `sys_mmap` |
| munmap | 11 | 215 | 215 | mem | `sys_munmap` |
| fork | 57 | — | — | proc | `sys_fork` |
| clone | 56 | 220 | 220 | proc | `sys_clone` |
| execve | 59 | 221 | 221 | proc | `sys_execve` |
| wait4 | 61 | 260 | 260 | proc | `sys_waitpid` |
| exit_group | 231 | 94 | 94 | proc | `sys_exit` |
| pipe | 22 | — | — | proc | `sys_pipe` |
| pipe2 | 293 | 59 | 59 | proc | `sys_pipe2` |
| dup2 | 33 | — | — | proc | `sys_dup2` |
| dup3 | 292 | 24 | 24 | proc | `sys_dup3` |
| clock_gettime | 228 | 113 | 113 | time | `sys_time_ns` |
| nanosleep | 35 | 101 | 101 | time | `sys_nanosleep` |
| ioctl | 16 | 29 | 29 | term | `sys_ioctl` |
| poll | 7 | — | — | watch | `sys_poll` |
| ppoll | 271 | 73 | 73 | watch | `sys_ppoll` |
| inotify_init1 | 294 | 26 | 26 | watch | `sys_inotify_init` |
| inotify_add_watch | 254 | 27 | 27 | watch | `sys_inotify_add_watch` |
| perf_event_open | 298 | 241 | 241 | perf | `perf_open` |

Note: aarch64 and riscv64 share syscall numbers for all modern operations.
x86-64 is the outlier with its legacy table.

## Calling Conventions

How each architecture invokes the one instruction:

### x86-64
```
Instruction: syscall
Number:      rax
Arguments:   rdi, rsi, rdx, r10, r8, r9
Return:      rax (negative = -errno)
Clobbered:   rcx, r11
```

### aarch64
```
Instruction: svc #0
Number:      w8 (x8)
Arguments:   x0, x1, x2, x3, x4, x5
Return:      x0 (negative = -errno)
Clobbered:   (per ARM64 calling convention)
```

### riscv64
```
Instruction: ecall
Number:      a7
Arguments:   a0, a1, a2, a3, a4, a5
Return:      a0 (negative = -errno)
Clobbered:   (per RISC-V calling convention)
```

Wrapper pattern (GNode → platform-specific assembly):

```c
static inline long syscall1(long n, long a1) {
#ifdef __x86_64__
    long ret;
    asm volatile("syscall" : "=a"(ret) : "a"(n), "D"(a1) : "rcx", "r11", "memory");
    return ret;
#elif defined(__aarch64__)
    register long x8 asm("x8") = n;
    register long x0 asm("x0") = a1;
    asm volatile("svc #0" : "+r"(x0) : "r"(x8) : "memory");
    return x0;
#elif defined(__riscv)
    register long a7 asm("a7") = n;
    register long a0 asm("a0") = a1;
    asm volatile("ecall" : "+r"(a0) : "r"(a7) : "memory");
    return a0;
#endif
}
```

## Constants

Literal values — leaf GNodes. Only divergent constants listed (same-value omitted).

### mmap

```c
// Protection (common)
PROT_NONE  = 0x0    PROT_READ  = 0x1    PROT_WRITE = 0x2    PROT_EXEC = 0x4

// Flags (common)
MAP_SHARED    = 0x01     MAP_PRIVATE   = 0x02     MAP_FIXED     = 0x10
MAP_ANONYMOUS = 0x20     // WARNING: 0x800 on some platforms
MAP_GROWSDOWN = 0x100    MAP_LOCKED    = 0x2000   MAP_POPULATE  = 0x8000
MAP_STACK     = 0x20000  MAP_HUGETLB   = 0x40000
```

### open/openat

```c
// Access modes (mutually exclusive, low 2 bits)
O_RDONLY = 0x0     O_WRONLY = 0x1     O_RDWR = 0x2

// Flags (OR'd)
O_CREAT     = 0x40      O_EXCL      = 0x80      O_TRUNC    = 0x200
O_APPEND    = 0x400     O_NONBLOCK  = 0x800     O_CLOEXEC  = 0x80000
O_DIRECTORY = 0x10000   O_NOFOLLOW  = 0x20000
```

### ioctl

Request codes are computed macros. genera uses these directly:

```c
TCGETS     = 0x5401     // read terminal attributes
TCSETS     = 0x5402     // set terminal attributes
TIOCGWINSZ = 0x5413     // get window size (rows, cols)
```

### clock_gettime

```c
CLOCK_REALTIME           = 0    CLOCK_MONOTONIC          = 1
CLOCK_PROCESS_CPUTIME_ID = 2    CLOCK_THREAD_CPUTIME_ID  = 3
CLOCK_MONOTONIC_RAW      = 4    CLOCK_BOOTTIME           = 7
```

### File descriptors

```c
STDIN_FILENO = 0    STDOUT_FILENO = 1    STDERR_FILENO = 2
```

### wait4

```c
WNOHANG = 0x01    WUNTRACED = 0x02    WCONTINUED = 0x08
```

### clone

```c
CLONE_VM      = 0x100       CLONE_FS      = 0x200       CLONE_FILES   = 0x400
CLONE_SIGHAND = 0x800       CLONE_THREAD  = 0x10000     CLONE_NEWNS   = 0x20000
CLONE_SETTLS  = 0x80000     CLONE_NEWPID  = 0x20000000  CLONE_NEWNET  = 0x40000000
```

### inotify

```c
// init flags
IN_CLOEXEC  = 0x80000    IN_NONBLOCK = 0x800

// watch mask
IN_ACCESS      = 0x001    IN_MODIFY       = 0x002    IN_ATTRIB        = 0x004
IN_CLOSE_WRITE = 0x008    IN_CLOSE_NOWRITE= 0x010    IN_OPEN          = 0x020
IN_MOVED_FROM  = 0x040    IN_MOVED_TO     = 0x080    IN_CREATE        = 0x100
IN_DELETE      = 0x200    IN_DELETE_SELF   = 0x400    IN_MOVE_SELF     = 0x800
```

## Error Codes

Negative return = -errno. These are the kernel's condition signals.

```c
EPERM  =  1  // not permitted     ENOENT =  2  // no such file      ESRCH  =  3  // no such process
EINTR  =  4  // interrupted       EIO    =  5  // I/O error         EBADF  =  9  // bad fd
ECHILD = 10  // no children       EAGAIN = 11  // try again         ENOMEM = 12  // out of memory
EACCES = 13  // permission        EFAULT = 14  // bad address       EBUSY  = 16  // busy
EEXIST = 17  // exists            EINVAL = 22  // invalid arg       ENOSPC = 28  // no space
EPIPE  = 32  // broken pipe
```

## References

Kernel source:
- x86-64: `arch/x86/entry/syscalls/syscall_64.tbl`
- aarch64/riscv64: `include/uapi/asm-generic/unistd.h`

External:
- [Linux Syscall Tables (mebeim)](https://syscalls.mebeim.net/)
- [Multi-Architecture Table](https://gpages.juszkiewicz.com.pl/syscalls-table/syscalls.html)
- [syscall(2) man page](https://man7.org/linux/man-pages/man2/syscall.2.html)
