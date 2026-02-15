/**
 * sys.c — System Interface
 *
 * Types, OS syscalls, machine encoding, entry point.
 * THE thin boundary between our code and the hardware.
 *
 * Include order: types → <os>/<arch> → api → <cpu> encoding
 * To port: add a new os/arch file, update the #if blocks.
 */
#ifndef SYS_C_INCLUDED
#define SYS_C_INCLUDED

#include "types.c"

// --- OS + Arch ---
#if defined(__linux__) && defined(__x86_64__)
#include "linux/x86_64.c"
#elif defined(__APPLE__) && defined(__x86_64__)
#error "darwin/x86_64 not yet implemented"
#elif defined(__APPLE__) && defined(__aarch64__)
#error "darwin/aarch64 not yet implemented"
#elif defined(__linux__) && defined(__aarch64__)
#error "linux/aarch64 not yet implemented"
#else
#error "Unsupported OS/arch combination"
#endif

#include "api.c"

// --- CPU instruction encoding ---
#if defined(__x86_64__)
#include "x86.c"
#endif

#endif // SYS_C_INCLUDED
