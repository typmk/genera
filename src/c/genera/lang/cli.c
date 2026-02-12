/**
 * cli.c — CLI Argument Dispatch
 *
 * Maps command-line arguments to actions:
 *   ./moss              — Interactive REPL
 *   ./moss test         — Run all tests
 *   ./moss bench        — Run all benchmarks
 *   ./moss eval "expr"  — One-shot eval
 *   ./moss jit "source" — JIT compile + execute
 *   ./moss emit "src"   — Emit C source
 *   ./moss run "src"    — Compile via gcc + run
 *   ./moss parse "src"  — Show entity tree
 *
 * Depends on: glass/test.c, glass/bench.c, glass/repl.c
 */
#ifndef CLI_C_INCLUDED
#define CLI_C_INCLUDED

static void *alloc_for_file(u64 size) { return sys_alloc(size); }

static int cli_run(int argc, char **argv) {
    if (argc < 2) {
        // Interactive REPL
        glass_repl_init();
        pf("moss — Clojure runtime (lisp, try: help)\n");
        repl();
        return 0;
    }

    const char *cmd = argv[1];

    if (strcmp(cmd, "test") == 0) {
        return run_all_tests();
    }

    if (strcmp(cmd, "bench") == 0) {
        run_all_bench();
        return 0;
    }

    if (strcmp(cmd, "watch") == 0) {
        return watch_run();
    }

    if (strcmp(cmd, "eval") == 0) {
        if (argc < 3) { pf("usage: moss eval <expr>\n"); return 1; }
        Val form = gram_read(argv[2]);
        g_signal = SIGNAL_NONE; g_depth = 0;
        Val result = eval(form, g_global_env);
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); return 1; }
        pp_val(result);
        buf_c(&g_print_buf, '\n');
        print_flush();
        return 0;
    }

    if (strcmp(cmd, "jit") == 0) {
        if (argc < 3) { pf("usage: moss jit <source>\n"); return 1; }
        i64 result = jit_run(argv[2]);
        pf("%lld\n", (long long)result);
        return 0;
    }

    if (strcmp(cmd, "emit") == 0) {
        if (argc < 3) { pf("usage: moss emit <source|file>\n"); return 1; }
        const char *src = argv[2];
        // Check if it's a file
        FileData f = sys_read_file(src, alloc_for_file);
        if (f.data) {
            cc_emit(f.data);
            sys_write(1, g_out.buf, g_out.pos);
            sys_free(f.data, f.len + 1);
        } else {
            cc_emit(src);
            sys_write(1, g_out.buf, g_out.pos);
        }
        return 0;
    }

    if (strcmp(cmd, "run") == 0) {
        if (argc < 3) { pf("usage: moss run <source|file>\n"); return 1; }
        const char *src = argv[2];
        FileData f = sys_read_file(src, alloc_for_file);
        const char *code = f.data ? f.data : src;
        int rc = compile_and_capture(code);
        if (rc != 0) { pf("error\n"); }
        else { pf("%s\n", g_captured); }
        if (f.data) sys_free(f.data, f.len + 1);
        return rc;
    }

    if (strcmp(cmd, "check") == 0) {
        if (argc < 3) { pf("usage: moss check <source|file>\n"); return 1; }
        t_pass = t_fail = t_groups = 0;
        register_test_builtins();
        const char *src = argv[2];
        FileData f = sys_read_file(src, alloc_for_file);
        if (f.data) {
            g_signal = SIGNAL_NONE; g_depth = 0;
            eval_string(f.data, g_global_env);
            if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
            sys_free(f.data, f.len + 1);
        } else {
            g_signal = SIGNAL_NONE; g_depth = 0;
            eval_string(src, g_global_env);
            if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
        }
        pf("%d passed, %d failed\n", t_pass, t_fail);
        return t_fail ? 1 : 0;
    }

    if (strcmp(cmd, "parse") == 0) {
        if (argc < 3) { pf("usage: moss parse <source>\n"); return 1; }
        Lang l; lang_lisp(&l);
        Gram g = gram_new(4096);
        gram_parse(&g, &l, argv[2], strlen(argv[2]));
        gram_index(&g);
        gram_print(&g, 0);
        return 0;
    }

    pf("unknown command: %s\n", cmd);
    pf("usage: moss [test|bench|watch|eval|jit|emit|run|check|parse] [args]\n");
    return 1;
}

#endif // CLI_C_INCLUDED
