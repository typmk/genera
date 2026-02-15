# Windows NT Syscall Equivalents for genera

Research on Windows NT syscalls for genera's freestanding C runtime (currently Linux-only with direct syscalls, no libc).

## Critical Insight: Unstable Syscall Numbers

**Windows syscall numbers are NOT a stable ABI.** They change between Windows versions—not just major releases, but even between service packs and builds. Microsoft actively randomizes syscall numbers as a defense mechanism against attacks using hardcoded syscall numbers.

### Example: NtReadFile Across Windows Versions

| Windows Version | NtReadFile | NtWriteFile | NtAllocateVirtualMemory |
|-----------------|------------|-------------|-------------------------|
| Win7 SP1        | 0x003      | 0x005       | 0x015                   |
| Win10 RS2 15063 | 0x006      | 0x008       | 0x018                   |
| Win10 22H2      | 0x006      | 0x008       | 0x018                   |
| Win11 22000     | Different  | Different   | Different               |

**Syscalls change not just from version to version, but Microsoft reshuffles the entire SSDT (System Service Dispatch Table) regularly.** This is why direct syscalls are fragile on Windows.

## The Stable Approach: Win32 API (kernel32.dll/ntdll.dll)

**The stable ABI is kernel32.dll and ntdll.dll exports, NOT the syscall numbers.** These DLLs provide the documented interface and handle syscall number mapping internally.

For freestanding Windows, genera should use one of two approaches:

1. **Win32 API** (stable, documented, Microsoft-supported)
2. **Dynamic ntdll extraction** (extract syscall numbers at runtime from ntdll.dll stubs)

This document covers both approaches, with Win32 API as the recommended path.

---

## Approach 1: NT Syscalls (ntdll.dll level)

### How Windows Syscalls Work

On x86-64 Windows:

1. User code calls ntdll.dll stub (e.g., `NtReadFile`)
2. Stub loads syscall number into `eax`: `mov eax, 0x006`
3. Stub moves `rcx` to `r10`: `mov r10, rcx` (Windows x64 calling convention adjustment)
4. Stub executes: `syscall` (on modern CPUs) or `int 0x2e` (legacy)
5. Kernel dispatcher uses `eax` to index SSDT and jump to kernel function

### x86-64 Syscall Instruction Evolution

| Method      | Speed      | Usage                                    |
|-------------|------------|------------------------------------------|
| `int 0x2e`  | Slowest    | Legacy (x86), brought back in Win10 TH2+ |
| `syscall`   | **3x faster** | Standard on x64 (AMD instruction)     |
| `sysenter`  | 3x faster  | Intel instruction (not used on x64)      |

**Modern x64 Windows uses `syscall`.** Windows 10 TH2+ re-enabled `int 0x2e` support on x64 (previously dropped since Win8), but `syscall` is still primary.

### Dynamic Syscall Number Extraction

Since syscall numbers are unstable, extract them at runtime from ntdll.dll:

```c
// Load ntdll.dll (already loaded in every process)
HMODULE ntdll = GetModuleHandleA("ntdll.dll");

// Get function address
void* func = GetProcAddress(ntdll, "NtReadFile");

// Extract syscall number from stub (4th byte)
// Typical ntdll stub layout:
//   0: 4C 8B D1          mov r10, rcx
//   3: B8 XX XX XX XX    mov eax, [syscall_number]  <- extract these 4 bytes
//   8: 0F 05             syscall
//  10: C3                ret
unsigned int syscall_num = *(unsigned int*)((char*)func + 4);
```

The syscall number is **2 bytes** starting at offset +4 in the function.

### Core NT Syscalls

#### 1. File I/O

**NtReadFile**
```c
NTSTATUS NtReadFile(
    HANDLE FileHandle,
    HANDLE Event,              // optional
    PIO_APC_ROUTINE ApcRoutine, // optional
    PVOID ApcContext,          // optional
    PIO_STATUS_BLOCK IoStatusBlock,
    PVOID Buffer,
    ULONG Length,
    PLARGE_INTEGER ByteOffset, // optional (NULL = current position)
    PULONG Key                 // optional
);
```

**NtWriteFile**
```c
NTSTATUS NtWriteFile(
    HANDLE FileHandle,
    HANDLE Event,
    PIO_APC_ROUTINE ApcRoutine,
    PVOID ApcContext,
    PIO_STATUS_BLOCK IoStatusBlock,
    PVOID Buffer,
    ULONG Length,
    PLARGE_INTEGER ByteOffset,
    PULONG Key
);
```

**NtCreateFile**
```c
NTSTATUS NtCreateFile(
    PHANDLE FileHandle,
    ACCESS_MASK DesiredAccess,
    POBJECT_ATTRIBUTES ObjectAttributes,
    PIO_STATUS_BLOCK IoStatusBlock,
    PLARGE_INTEGER AllocationSize,  // optional
    ULONG FileAttributes,
    ULONG ShareAccess,
    ULONG CreateDisposition,
    ULONG CreateOptions,
    PVOID EaBuffer,                 // optional
    ULONG EaLength
);
```

**NtClose**
```c
NTSTATUS NtClose(HANDLE Handle);
```
- Syscall number (Win10 22H2): `0x0F`

#### 2. Memory Management

**NtAllocateVirtualMemory**
```c
NTSTATUS NtAllocateVirtualMemory(
    HANDLE ProcessHandle,
    PVOID* BaseAddress,      // in/out
    ULONG_PTR ZeroBits,
    PSIZE_T RegionSize,      // in/out
    ULONG AllocationType,    // MEM_RESERVE | MEM_COMMIT
    ULONG Protect            // PAGE_READWRITE, PAGE_EXECUTE_READWRITE
);
```

**NtFreeVirtualMemory**
```c
NTSTATUS NtFreeVirtualMemory(
    HANDLE ProcessHandle,
    PVOID* BaseAddress,
    PSIZE_T RegionSize,
    ULONG FreeType          // MEM_RELEASE
);
```

**NtProtectVirtualMemory** (for JIT: RW → RX)
```c
NTSTATUS NtProtectVirtualMemory(
    HANDLE ProcessHandle,
    PVOID* BaseAddress,
    PSIZE_T NumberOfBytesToProtect,
    ULONG NewAccessProtection,
    PULONG OldAccessProtection
);
```

#### 3. Process Management

**NtCreateProcess** (legacy, pre-Vista)
```c
NTSTATUS NtCreateProcess(
    PHANDLE ProcessHandle,
    ACCESS_MASK DesiredAccess,
    POBJECT_ATTRIBUTES ObjectAttributes,
    HANDLE ParentProcess,
    BOOLEAN InheritObjectTable,
    HANDLE SectionHandle,
    HANDLE DebugPort,
    HANDLE ExceptionPort
);
```

**NtCreateUserProcess** (Vista+, recommended)
- Single syscall replacing 4 separate calls (NtOpenFile, NtCreateSection, NtCreateProcess, NtCreateThread)
- Complex structure—use Win32 CreateProcessA instead

**NtWaitForSingleObject**
```c
NTSTATUS NtWaitForSingleObject(
    HANDLE Handle,
    BOOLEAN Alertable,
    PLARGE_INTEGER Timeout  // optional, NULL = infinite
);
```

**NtTerminateProcess**
```c
NTSTATUS NtTerminateProcess(
    HANDLE ProcessHandle,   // NULL = current process
    NTSTATUS ExitStatus
);
```
- Syscall number (Win10): `0x2C`

#### 4. Timing

**NtQueryPerformanceCounter**
```c
NTSTATUS NtQueryPerformanceCounter(
    PLARGE_INTEGER PerformanceCounter,
    PLARGE_INTEGER PerformanceFrequency  // optional
);
```

#### 5. Device I/O

**NtDeviceIoControlFile** (equivalent to Linux ioctl)
```c
NTSTATUS NtDeviceIoControlFile(
    HANDLE FileHandle,
    HANDLE Event,
    PIO_APC_ROUTINE ApcRoutine,
    PVOID ApcContext,
    PIO_STATUS_BLOCK IoStatusBlock,
    ULONG IoControlCode,
    PVOID InputBuffer,
    ULONG InputBufferLength,
    PVOID OutputBuffer,
    ULONG OutputBufferLength
);
```

### Direct Syscall Assembly (x86-64)

```asm
; Windows x64 syscall ABI:
; - Parameters in RCX, RDX, R8, R9 (first 4)
; - Additional params on stack (right-to-left)
; - Must copy RCX to R10 before syscall
; - Syscall number in EAX
; - Return value in RAX (NTSTATUS)

global NtReadFile
NtReadFile:
    mov r10, rcx           ; Windows syscall convention
    mov eax, 0x006         ; Syscall number (Win10 RS2+)
    syscall
    ret

global NtWriteFile
NtWriteFile:
    mov r10, rcx
    mov eax, 0x008
    syscall
    ret

global NtClose
NtClose:
    mov r10, rcx
    mov eax, 0x0F
    syscall
    ret
```

**WARNING:** Hardcoded syscall numbers will break across Windows versions. Use dynamic extraction.

---

## Approach 2: Win32 API (kernel32.dll level) — RECOMMENDED

The stable, documented, Microsoft-supported approach. kernel32.dll exports are guaranteed stable across Windows versions.

### 1. File I/O

**CreateFileA**
```c
HANDLE CreateFileA(
    LPCSTR lpFileName,
    DWORD dwDesiredAccess,      // GENERIC_READ | GENERIC_WRITE
    DWORD dwShareMode,          // FILE_SHARE_READ | FILE_SHARE_WRITE
    LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    DWORD dwCreationDisposition, // CREATE_ALWAYS, OPEN_EXISTING, etc.
    DWORD dwFlagsAndAttributes, // FILE_ATTRIBUTE_NORMAL
    HANDLE hTemplateFile
);
```

**ReadFile**
```c
BOOL ReadFile(
    HANDLE hFile,
    LPVOID lpBuffer,
    DWORD nNumberOfBytesToRead,
    LPDWORD lpNumberOfBytesRead,
    LPOVERLAPPED lpOverlapped   // NULL for synchronous
);
```

**WriteFile**
```c
BOOL WriteFile(
    HANDLE hFile,
    LPCVOID lpBuffer,
    DWORD nNumberOfBytesToWrite,
    LPDWORD lpNumberOfBytesWritten,
    LPOVERLAPPED lpOverlapped
);
```

**CloseHandle**
```c
BOOL CloseHandle(HANDLE hObject);
```

### 2. Memory Management

**VirtualAlloc** (equivalent to Linux mmap)
```c
LPVOID VirtualAlloc(
    LPVOID lpAddress,      // NULL = let system choose
    SIZE_T dwSize,
    DWORD flAllocationType, // MEM_RESERVE | MEM_COMMIT
    DWORD flProtect        // PAGE_READWRITE, PAGE_EXECUTE_READWRITE
);
```

**VirtualFree**
```c
BOOL VirtualFree(
    LPVOID lpAddress,
    SIZE_T dwSize,         // 0 if using MEM_RELEASE
    DWORD dwFreeType       // MEM_RELEASE
);
```

**VirtualProtect** (for JIT: change RW → RX)
```c
BOOL VirtualProtect(
    LPVOID lpAddress,
    SIZE_T dwSize,
    DWORD flNewProtect,    // PAGE_EXECUTE_READ
    PDWORD lpflOldProtect
);
```

**Memory Protection Flags**

| Flag                      | Value  | Description                          |
|---------------------------|--------|--------------------------------------|
| PAGE_NOACCESS             | 0x01   | No access                            |
| PAGE_READONLY             | 0x02   | Read-only                            |
| PAGE_READWRITE            | 0x04   | Read/write (DEP blocks execution)    |
| PAGE_WRITECOPY            | 0x08   | Copy-on-write                        |
| PAGE_EXECUTE              | 0x10   | Execute-only                         |
| PAGE_EXECUTE_READ         | 0x20   | Execute + read (JIT final state)     |
| PAGE_EXECUTE_READWRITE    | 0x40   | Execute + read/write (JIT codegen)   |
| PAGE_EXECUTE_WRITECOPY    | 0x80   | Execute + copy-on-write              |

**JIT Memory Workflow:**
```c
// 1. Allocate RW memory for code generation
void* code = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

// 2. Write JIT-compiled code
memcpy(code, compiled_code, code_size);

// 3. Change to RX (W^X policy compliant)
DWORD old_protect;
VirtualProtect(code, code_size, PAGE_EXECUTE_READ, &old_protect);

// 4. Execute
((void(*)(void))code)();

// 5. Free
VirtualFree(code, 0, MEM_RELEASE);
```

**FlushInstructionCache** (required after JIT codegen)
```c
BOOL FlushInstructionCache(
    HANDLE hProcess,       // GetCurrentProcess()
    LPCVOID lpBaseAddress,
    SIZE_T dwSize
);
```

### 3. Process Management

**CreateProcessA**
```c
BOOL CreateProcessA(
    LPCSTR lpApplicationName,
    LPSTR lpCommandLine,
    LPSECURITY_ATTRIBUTES lpProcessAttributes,
    LPSECURITY_ATTRIBUTES lpThreadAttributes,
    BOOL bInheritHandles,
    DWORD dwCreationFlags,
    LPVOID lpEnvironment,
    LPCSTR lpCurrentDirectory,
    LPSTARTUPINFOA lpStartupInfo,
    LPPROCESS_INFORMATION lpProcessInformation
);
```

**WaitForSingleObject**
```c
DWORD WaitForSingleObject(
    HANDLE hHandle,
    DWORD dwMilliseconds   // INFINITE = wait forever
);
```

**ExitProcess**
```c
void ExitProcess(DWORD uExitCode);
```

### 4. Timing

**QueryPerformanceCounter** (high-resolution timer)
```c
BOOL QueryPerformanceCounter(LARGE_INTEGER* lpPerformanceCount);
```

**QueryPerformanceFrequency** (call once at startup)
```c
BOOL QueryPerformanceFrequency(LARGE_INTEGER* lpFrequency);
```

**Usage:**
```c
LARGE_INTEGER freq, start, end;
QueryPerformanceFrequency(&freq);  // Call once
QueryPerformanceCounter(&start);
// ... timed code ...
QueryPerformanceCounter(&end);
double elapsed_seconds = (double)(end.QuadPart - start.QuadPart) / freq.QuadPart;
```

Resolution: < 1 microsecond (typically nanoseconds on modern systems).

### 5. Device I/O

**DeviceIoControl** (equivalent to Linux ioctl)
```c
BOOL DeviceIoControl(
    HANDLE hDevice,
    DWORD dwIoControlCode,
    LPVOID lpInBuffer,
    DWORD nInBufferSize,
    LPVOID lpOutBuffer,
    DWORD nOutBufferSize,
    LPDWORD lpBytesReturned,
    LPOVERLAPPED lpOverlapped
);
```

### 6. File System Monitoring

**ReadDirectoryChangesW** (equivalent to Linux inotify)
```c
BOOL ReadDirectoryChangesW(
    HANDLE hDirectory,
    LPVOID lpBuffer,
    DWORD nBufferLength,
    BOOL bWatchSubtree,
    DWORD dwNotifyFilter,       // FILE_NOTIFY_CHANGE_*
    LPDWORD lpBytesReturned,
    LPOVERLAPPED lpOverlapped,
    LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
);
```

**Notify Filters:**
- `FILE_NOTIFY_CHANGE_FILE_NAME` (create, delete, rename)
- `FILE_NOTIFY_CHANGE_DIR_NAME`
- `FILE_NOTIFY_CHANGE_ATTRIBUTES`
- `FILE_NOTIFY_CHANGE_SIZE`
- `FILE_NOTIFY_CHANGE_LAST_WRITE`
- `FILE_NOTIFY_CHANGE_SECURITY`

**Returns:** `FILE_NOTIFY_INFORMATION` structure (linked list of changes).

### 7. Console/Terminal

**GetConsoleMode**
```c
BOOL GetConsoleMode(
    HANDLE hConsoleHandle,  // GetStdHandle(STD_INPUT_HANDLE)
    LPDWORD lpMode
);
```

**SetConsoleMode** (for raw mode)
```c
BOOL SetConsoleMode(
    HANDLE hConsoleHandle,
    DWORD dwMode
);
```

**Console Input Modes:**
- `ENABLE_ECHO_INPUT` (0x0004) — disable for raw mode
- `ENABLE_LINE_INPUT` (0x0002) — disable for raw mode
- `ENABLE_PROCESSED_INPUT` (0x0001) — disable for raw mode
- `ENABLE_VIRTUAL_TERMINAL_INPUT` (0x0200) — enable for ANSI escape sequences

**Console Output Modes:**
- `ENABLE_PROCESSED_OUTPUT` (0x0001)
- `ENABLE_VIRTUAL_TERMINAL_PROCESSING` (0x0004) — enable for ANSI escape sequences

**Raw Mode Recipe:**
```c
HANDLE hStdin = GetStdHandle(STD_INPUT_HANDLE);
DWORD mode;
GetConsoleMode(hStdin, &mode);
mode &= ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT);
mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
SetConsoleMode(hStdin, mode);
```

**Restore on exit:**
```c
// Save original mode at startup
DWORD original_mode;
GetConsoleMode(hStdin, &original_mode);

// Restore before exit
SetConsoleMode(hStdin, original_mode);
```

### 8. Console Handles

**GetStdHandle**
```c
HANDLE GetStdHandle(DWORD nStdHandle);
```

Constants:
- `STD_INPUT_HANDLE` (-10)
- `STD_OUTPUT_HANDLE` (-11)
- `STD_ERROR_HANDLE` (-12)

---

## Windows x86-64 Calling Convention

**CRITICAL DIFFERENCE FROM LINUX:** Windows x64 uses Microsoft calling convention, NOT System V AMD64 ABI.

| Parameter | Windows x64      | Linux x64 (System V) |
|-----------|------------------|----------------------|
| 1st       | RCX              | RDI                  |
| 2nd       | RDX              | RSI                  |
| 3rd       | R8               | RDX                  |
| 4th       | R9               | RCX                  |
| 5th+      | Stack (RTL)      | R8, R9, stack        |

**Shadow Space:** Caller MUST allocate 32 bytes on stack before call (even for functions with < 4 params). This is space for callee to spill RCX/RDX/R8/R9.

**Register Volatility:**
- **Volatile (caller-saved):** RAX, RCX, RDX, R8, R9, R10, R11
- **Non-volatile (callee-saved):** RBX, RBP, RDI, RSI, RSP, R12-R15

**Stack Alignment:** 16-byte alignment required before `call` or `syscall`.

**Syscall Adjustments:**
- Windows syscalls move RCX → R10 before `syscall` (kernel expects first param in R10)
- RDX, R8, R9 stay the same
- 5th+ params on stack

---

## Freestanding Windows PE

### Linker Flags

```bash
# MSVC linker
link.exe /NODEFAULTLIB /ENTRY:main /SUBSYSTEM:CONSOLE program.obj kernel32.lib

# MinGW/GCC
gcc -nostdlib -e main -lkernel32 program.c
```

Flags:
- `/NODEFAULTLIB` — no CRT linking
- `/ENTRY:main` — custom entry point (default: `mainCRTStartup` for console, `WinMainCRTStartup` for GUI)
- `/SUBSYSTEM:CONSOLE` — console app (or `:WINDOWS` for GUI)

### Entry Point

**Conventional Names:**
- `mainCRTStartup` — console subsystem (calls `main()`)
- `WinMainCRTStartup` — windows subsystem (calls `WinMain()`)

**Custom Entry (freestanding):**
```c
void __stdcall main(void) {
    // Your code here
    ExitProcess(0);
}
```

**MUST use `__stdcall` calling convention** (or linker errors).

### Getting kernel32.dll Without CRT

When Windows loads a PE:
1. Loader places pointer to kernel32.dll in `EAX` at entry (undocumented, may change)
2. Thread Environment Block (TEB) → Process Environment Block (PEB) → Ldr data → module list

**PEB Access:**
```c
// x64 TEB is at gs:[0x30]
asm("mov %%gs:0x30, %0" : "=r"(peb));
// Then walk PEB->Ldr->InLoadOrderModuleList to find kernel32.dll by name
```

**Simpler: Use import table.** Even freestanding PE can import kernel32 functions:

```c
// Declare imported functions
__declspec(dllimport) HANDLE __stdcall GetStdHandle(DWORD);
__declspec(dllimport) BOOL __stdcall WriteFile(HANDLE, LPCVOID, DWORD, LPDWORD, LPOVERLAPPED);
__declspec(dllimport) void __stdcall ExitProcess(DWORD);

void __stdcall main(void) {
    HANDLE stdout = GetStdHandle((DWORD)-11);
    DWORD written;
    WriteFile(stdout, "Hello\n", 6, &written, NULL);
    ExitProcess(0);
}
```

**Linker will create import table for kernel32.dll automatically.**

### Minimal Import Requirement

Windows 2000+ requires at least one import from kernel32.dll in the PE import table. Adding a single ordinal import satisfies this:

```asm
; In .idata section
dd kernel32.dll_name
dd 0  ; timestamp
dd 0  ; forwarder
dd kernel32_name
dd kernel32_iat

kernel32_name: db "kernel32.dll", 0
```

---

## Practical Recommendations for genera

### Option A: Win32 API (Stable, Simple)

**Pros:**
- Stable ABI across all Windows versions
- Documented by Microsoft
- No syscall number extraction required
- Works on all CPUs (x86, x64, ARM64)

**Cons:**
- Slightly slower (one extra call through kernel32 → ntdll)
- Requires linking kernel32.lib (or dynamic import table)

**Implementation:**
```c
// genera/sys_windows.c
#include <windows.h>

void sys_write(int fd, const void* buf, size_t len) {
    HANDLE h = GetStdHandle(fd == 1 ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE);
    DWORD written;
    WriteFile(h, buf, len, &written, NULL);
}

void* sys_alloc(size_t size) {
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void sys_free(void* ptr, size_t size) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}
```

### Option B: Dynamic NT Syscalls (Unstable, Complex)

**Pros:**
- Direct kernel calls (slightly faster)
- No import table required (true freestanding)

**Cons:**
- Syscall numbers change between Windows versions
- Requires runtime extraction from ntdll
- Complex error handling (NTSTATUS vs BOOL)
- Breaks on Windows updates

**Only use if:**
- Absolute minimal binary size required
- Performance-critical (microbenchmarks show ~50ns difference)
- Willing to maintain version detection code

**Implementation:**
```c
// Extract syscall numbers at startup
typedef NTSTATUS (*NtReadFile_t)(HANDLE, HANDLE, PIO_APC_ROUTINE, PVOID,
                                 PIO_STATUS_BLOCK, PVOID, ULONG, PLARGE_INTEGER, PULONG);

HMODULE ntdll = GetModuleHandleA("ntdll.dll");
NtReadFile_t NtReadFile = (NtReadFile_t)GetProcAddress(ntdll, "NtReadFile");

// Now call like normal function (no need to extract syscall number)
NtReadFile(file, NULL, NULL, NULL, &iosb, buffer, size, NULL, NULL);
```

**Wait, this is still function call, not direct syscall!** To get syscall number:

```c
unsigned int get_syscall_number(void* func) {
    // ntdll stub: mov r10, rcx; mov eax, XXXX; syscall; ret
    return *(unsigned int*)((char*)func + 4);
}

unsigned int NtReadFile_syscall = get_syscall_number(NtReadFile);

// Then use inline asm:
__asm__(
    "mov r10, rcx\n"
    "mov eax, %0\n"
    "syscall\n"
    :: "r"(NtReadFile_syscall)
);
```

**Verdict:** Not worth it. Just call the ntdll function directly—it's 3 instructions (mov, mov, syscall). The overhead is ~1-2ns.

### Recommended Approach: Win32 API with Freestanding Entry

```c
// genera_windows.c
#include <windows.h>

void __stdcall mainCRTStartup(void) {
    // genera entry point
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);

    // Allocate memory for genera heap
    void* heap = VirtualAlloc(NULL, 1024*1024, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

    // Initialize genera
    genera_init(heap, 1024*1024);

    // Run REPL
    genera_repl();

    // Cleanup
    VirtualFree(heap, 0, MEM_RELEASE);
    ExitProcess(0);
}
```

**Compile:**
```bash
# MSVC
cl /c genera_windows.c
link /NODEFAULTLIB /ENTRY:mainCRTStartup /SUBSYSTEM:CONSOLE genera_windows.obj kernel32.lib

# MinGW
gcc -nostdlib -e mainCRTStartup genera_windows.c -lkernel32
```

---

## Sources

### Syscall Stability & Mechanism
- [Windows Syscall Tables](https://j00ru.vexillium.org/syscalls/nt/64/)
- [Direct System Calls | Crow's Nest](https://www.crow.rip/nest/mal/dev/inject/syscalls/direct-syscalls)
- [Windows Syscall Mechanism](https://www.evilsocket.net/2014/02/11/On-Windows-syscall-mechanism-and-syscall-numbers-extraction-methods/)
- [Windows 11 Syscalls Repository](https://github.com/ikermit/11Syscalls)
- [j00ru Windows Syscalls Repository](https://github.com/j00ru/windows-syscalls)
- [Alt Syscalls for Windows 11](https://fluxsec.red/alt-syscalls-for-windows-11)

### Calling Conventions
- [x86 Calling Conventions - Wikipedia](https://en.wikipedia.org/wiki/X86_calling_conventions)
- [Windows 64-bit Calling Conventions](https://accu.org/journals/overload/22/120/orr_1897/)
- [x64 Architecture Overview - Microsoft Learn](https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/x64-architecture)
- [Windows x64 Calling Convention | Red Team Notes](https://www.ired.team/miscellaneous-reversing-forensics/windows-kernel-internals/windows-x64-calling-convention-stack-frame)

### Freestanding PE
- [CRT-free in 2023: tips and tricks](https://nullprogram.com/blog/2023/02/15/)
- [HOWTO - Building without Import Libraries](https://hero.handmade.network/forums/code-discussion/t/129-howto_-_building_without_import_libraries)
- [Tiny PE](http://www.phreedom.org/research/tinype/)
- [/ENTRY Linker Option - Microsoft Learn](https://learn.microsoft.com/en-us/cpp/build/reference/entry-entry-point-symbol?view=msvc-170)

### Memory Management
- [VirtualAlloc - Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc)
- [Memory Protection Constants - Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/memory/memory-protection-constants)
- [NtAllocateVirtualMemory - Microsoft Learn](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ntifs/nf-ntifs-ntallocatevirtualmemory)

### I/O & Console
- [ReadDirectoryChangesW - Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw)
- [GetConsoleMode - Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/getconsolemode)
- [SetConsoleMode - Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/setconsolemode)
- [Console Virtual Terminal Sequences - Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences)

### Timing
- [QueryPerformanceCounter - Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/api/profileapi/nf-profileapi-queryperformancecounter)
- [Acquiring high-resolution time stamps - Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps)

### Syscall Instruction Details
- [System Call Instructions - CodeMachine](https://codemachine.com/articles/system_call_instructions.html)
- [Windows 10 TH2 INT 2E mystery](https://www.amossys.fr/insights/blog-technique/windows10-th2-int2e-mystery/)
- [Direct Syscalls vs Indirect Syscalls - RedOps](https://redops.at/en/blog/direct-syscalls-vs-indirect-syscalls)
