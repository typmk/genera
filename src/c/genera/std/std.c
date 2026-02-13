/**
 * std.c — Layer 0: Standard Library
 *
 * Formatting, memory, strings, values, containers, tracing, commands.
 * FREESTANDING: no libc. Depends only on sys/.
 *
 * Include order: fmt → mem → str → val → arr → tap → cmd
 * Each component depends only on previous components.
 */
#ifndef STD_C_INCLUDED
#define STD_C_INCLUDED

#include "../sys/sys.c"
#include "fmt.c"
#include "mem.c"
#include "str.c"
#include "val.c"
#include "arr.c"
#include "tap.c"
#include "cmd.c"

#endif // STD_C_INCLUDED
