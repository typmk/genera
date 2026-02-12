/**
 * lang.c — The Language
 *
 * All program code: runtime primitives, language core,
 * code generation, tests, benchmarks, REPL, CLI.
 *
 * Depends on: std/ → sys/
 *
 * Include order follows dependency:
 *   std → proto → coll (+ sig) → grammar → eval → jit → cc
 *   → test → bench → watch → repl → cli
 */
#ifndef LANG_C_INCLUDED
#define LANG_C_INCLUDED

#include "../std/std.c"

// runtime primitives
#include "proto.c"
#include "coll.c"

// language core
#include "grammar.c"
#include "eval.c"

// code generation
#include "jit.c"
#include "cc.c"

// dx
#include "test.c"
#include "bench.c"
#include "watch.c"
#include "repl.c"
#include "cli.c"

#endif // LANG_C_INCLUDED
