/**
 * moss.c — Entry Point
 *
 * Single binary: REPL, test, bench, eval, JIT, emit, parse.
 * Build: gcc -O3 -march=native -nostdlib -static -o moss src/c/moss.c
 *
 * Dependency chain:
 *   moss.c → lang/ → std/ → sys/
 */

#include "lang/lang.c"

int main(int argc, char **argv) {
    // Init all layers
    base_init();
    proto_init();
    grammar_init();
    coll_init();
    coll_register_protos();
    eval_init();

    // Allocate executable code buffer for JIT
    g_code.code = (u8 *)sys_alloc_exec(CODE_SIZE);
    g_code.cap = CODE_SIZE;
    g_code.pos = 0;

    if (!g_code.code) {
        pf("mmap failed\n");
        base_cleanup();
        return 1;
    }

    int rc = cli_run(argc, argv);

    // Cleanup
    sys_free_exec(g_code.code, CODE_SIZE);
    base_cleanup();
    return rc;
}
