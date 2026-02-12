/**
 * base.c — Layer 0: Foundation
 *
 * Types, memory, strings, values, containers, tracing, REPL.
 * FREESTANDING: no libc. Depends only on sys.c.
 *
 * Include order: sys → fmt → mem → str → val → arr → tap → cmd
 * Each component depends only on previous components.
 */
#ifndef BASE_C_INCLUDED
#define BASE_C_INCLUDED

#include "sys.c"
#include "fmt.c"
#include "mem.c"
#include "str.c"
#include "val.c"
#include "arr.c"
#include "tap.c"
#include "cmd.c"

#endif // BASE_C_INCLUDED
