/**
 * cli.c — CLI Argument Dispatch
 *
 * Maps command-line arguments to actions:
 *   ./gna              — Interactive REPL
 *   ./gna test         — Run all tests
 *   ./gna bench        — Run all benchmarks
 *   ./gna eval "expr"  — One-shot eval
 *   ./gna jit "source" — JIT compile + execute
 *   ./gna emit "src"   — Emit C source
 *   ./gna run "src"    — Compile via gcc + run
 *   ./gna parse "src"  — Show entity tree
 *   ./gna render "src" — Render (round-trip or outline)
 *   ./gna trace "src"  — JIT compile + annotated source
 *
 * Depends on: test.c, bench.c, repl.c
 */
#ifndef CLI_C_INCLUDED
#define CLI_C_INCLUDED

static void *alloc_for_file(u64 size) { return sys_alloc(size); }

// Project obs (StrId-indexed) → grammar (node-indexed) for annotated source.
// Join key: intern(node.source_text) → StrId → obs_hit/obs_count.
static void obs_to_gram(Gram *g, u64 *hit_m, u32 *counts) {
    for (u32 i = 1; i < g->n; i++) {
        GNode *n = &g->nodes[i];
        if (n->kind == NK_IDENT && n->len > 0 && n->start + n->len <= g->src_len) {
            Str text = {(u8 *)(g->src + n->start), n->len};
            StrId id = str_intern(text);
            if (obs_hit(id)) {
                hit_m[i >> 6] |= (1ULL << (i & 63));
                counts[i] = g_obs_count[id];
            }
        }
    }
}

// ─── obs_report ──────────────────────────────────────────────────────────────
//
// Grammar (free) + OBS counts + allocs + timing → one unified view.
// All data already collected. This just reads and formats.
//
// obs_collect() and obs_ticks_to_ns() are in tap.c

static void obs_report(Gram *g, Val result, u64 elapsed, u64 phases[3], PerfCounters *pc) {
    OutBuf *b = &g_print_buf;
    bool has_time = (g_obs_level >= 2);

    // Source
    pf("\n");
    gram_render_outline(g);
    pf("\n");

    // Profile
    u32 hits[256];
    u32 nh = obs_collect(hits, 256);
    u32 max_c = nh ? g_obs_count[hits[0]] : 1;
    u32 max_nl = 0;
    for (u32 i = 0; i < nh; i++) { u32 l = str_from_id(hits[i]).len; if (l > max_nl) max_nl = l; }
    if (max_nl < 3) max_nl = 3;

    for (u32 i = 0; i < nh && i < 20; i++) {
        StrId id = hits[i]; Str name = str_from_id(id);
        u32 cnt = g_obs_count[id];
        bool sp = sig_get(id) != NULL;

        buf_s(b, "  ");
        buf_rjust_i(b, cnt, 8);
        buf_s(b, "  ");
        pfc(sp ? C_CYAN : C_YELLOW);
        for (u32 j = 0; j < name.len; j++) buf_c(b, name.data[j]);
        pfc(C_RESET);
        for (u32 j = name.len; j < max_nl; j++) buf_c(b, ' ');
        buf_s(b, "  ");
        buf_bar(b, cnt, max_c, 20, sp ? C_CYAN : C_YELLOW);

        // Annotations: % + self-time + allocs
        pfc(C_DIM);
        buf_s(b, " ");
        u32 p10 = g_obs_total ? (u32)((u64)cnt * 1000 / g_obs_total) : 0;
        buf_u(b, p10 / 10); buf_c(b, '.'); buf_c(b, '0' + p10 % 10); buf_c(b, '%');

        // Self-time (tier 2)
        if (has_time && g_obs_ns[id]) {
            buf_s(b, "  ");
            buf_elapsed(b, obs_ticks_to_ns(g_obs_ns[id]));
        }
        // Per-name allocations
        if (g_obs_alloc[id]) {
            buf_s(b, "  ");
            buf_bytes(b, g_obs_alloc[id]);
        }
        pfc(C_RESET);
        pf("\n");
    }

    // Coverage
    u32 all[256]; u32 na = 0; u64 seen[16] = {0};
    for (u32 i = 1; i < g->n && na < 256; i++) {
        GNode *nd = &g->nodes[i];
        if (nd->kind == NK_IDENT && nd->len > 0 && nd->start + nd->len <= g->src_len) {
            StrId s = gn_intern(g, i); u32 w = s & 1023;
            if (!(seen[w>>6] & (1ULL << (w&63)))) { seen[w>>6] |= (1ULL << (w&63)); all[na++] = s; }
        }
    }
    u32 nhit = 0;
    for (u32 i = 0; i < na; i++) if (obs_hit(all[i])) nhit++;
    pf("\n  ");
    pfc(C_DIM); buf_u(b, nhit); buf_c(b, '/'); buf_u(b, na); pfc(C_RESET);
    buf_s(b, "  ");
    for (u32 i = 0; i < na; i++) {
        Str nm = str_from_id(all[i]);
        pfc(obs_hit(all[i]) ? C_GREEN : C_DIM);
        for (u32 j = 0; j < nm.len; j++) buf_c(b, nm.data[j]);
        pfc(C_RESET);
        buf_c(b, ' ');
    }
    pf("\n");

    // Pipeline
    pf("\n");
    static const char *pn[] = {"parse", "analyze", "eval"};
    u64 total_t = phases[0] + phases[1] + phases[2];
    for (u32 i = 0; i < 3; i++) {
        buf_s(b, "  ");
        buf_ljust_s(b, pn[i], 9);
        buf_elapsed(b, phases[i]);
        if (total_t > 0) { buf_s(b, "  "); buf_bar(b, phases[i], total_t, 12, NULL); }
        pf("\n");
    }

    // Memory (arenas)
    pfc(C_DIM);
    buf_s(b, "  ");
    Arena *ar[] = {&g_perm, &g_req, &g_temp};
    for (u32 i = 0; i < 3; i++) {
        if (!ar[i]->live_bytes) continue;
        buf_s(b, ar[i]->name); buf_c(b, ' ');
        buf_bytes(b, ar[i]->live_bytes);
        buf_s(b, "/"); buf_u(b, ar[i]->alloc_count);
        buf_s(b, "  ");
    }
    pfc(C_RESET);
    pf("\n");

    // Hardware (compact)
    if (pc->n >= 2 && pc->val[0] > 0) {
        pfc(C_DIM);
        buf_s(b, "  ");
        buf_u(b, pc->val[0]); buf_s(b, " cyc  ");
        buf_u(b, pc->val[1]); buf_s(b, " ins  ");
        buf_f1(b, (f64)pc->val[1] / (f64)pc->val[0]); buf_s(b, " IPC");
        if (g_obs_total) {
            buf_s(b, "  "); buf_u(b, (u32)(pc->val[0] / g_obs_total)); buf_s(b, " cyc/op");
        }
        pfc(C_RESET);
        pf("\n");
    }

    // Summary
    pf("\n  ");
    pfc(C_BOLD); pp_val(result); pfc(C_RESET);
    pfc(C_DIM);
    buf_s(b, "  "); buf_u(b, g_obs_total); buf_s(b, " ops  ");
    buf_elapsed(b, elapsed);
    if (g_obs_total) { buf_s(b, "  "); buf_u(b, (u32)(elapsed / g_obs_total)); buf_s(b, " ns/op"); }
    pfc(C_RESET);
    pf("\n");
    print_flush();
}

static int cli_run(int argc, char **argv) {
    if (argc < 2) {
        // Interactive REPL
        glass_repl_init();
        pf("genera — Clojure runtime (lisp, try: help)\n");
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
        if (argc < 3) { pf("usage: gna eval <expr>\n"); return 1; }
        g_signal = SIGNAL_NONE; g_depth = 0;
        Val result = eval_string(argv[2], g_global_env);
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); return 1; }
        pp_val(result);
        buf_c(&g_print_buf, '\n');
        print_flush();
        return 0;
    }

    if (strcmp(cmd, "jit") == 0) {
        if (argc < 3) { pf("usage: gna jit <source>\n"); return 1; }
        i64 result = jit_run(argv[2]);
        pf("%lld\n", (long long)result);
        return 0;
    }

    if (strcmp(cmd, "emit") == 0) {
        if (argc < 3) { pf("usage: gna emit <source|file>\n"); return 1; }
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
        if (argc < 3) { pf("usage: gna run <source|file>\n"); return 1; }
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
        if (argc < 3) { pf("usage: gna check <source|file>\n"); return 1; }
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
        if (argc < 3) { pf("usage: gna parse <source>\n"); return 1; }
        Lang l; lang_lisp(&l);
        Gram g = gram_new(4096);
        gram_parse(&g, &l, argv[2], strlen(argv[2]));
        gram_index(&g);
        gram_print(&g, 0);
        return 0;
    }

    if (strcmp(cmd, "render") == 0) {
        if (argc < 3) { pf("usage: gna render [--outline|--c] <source|file>\n"); return 1; }
        bool outline = false, as_c = false;
        int src_arg = 2;
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "--outline") == 0) { outline = true; src_arg = i + 1; }
            else if (strcmp(argv[i], "--c") == 0) { as_c = true; src_arg = i + 1; }
        }
        if (src_arg >= argc) { pf("usage: gna render [--outline|--c] <source|file>\n"); return 1; }
        const char *src = argv[src_arg];
        FileData f = sys_read_file(src, alloc_for_file);
        const char *code = f.data ? f.data : src;
        u32 len = f.data ? (u32)f.len : (u32)strlen(code);
        Lang l;
        if (as_c) lang_c(&l); else lang_lisp(&l);
        Gram g = gram_new(len < 256 ? 4096 : len * 4);
        gram_parse(&g, &l, code, len);
        gram_index(&g);
        if (outline) {
            gram_render_outline(&g);
        } else {
            Lang rl;
            if (as_c) lang_c(&rl); else lang_lisp(&rl);
            gram_render(&g, &rl);
        }
        if (f.data) sys_free(f.data, f.len + 1);
        return 0;
    }

    if (strcmp(cmd, "trace") == 0) {
        if (argc < 3) { pf("usage: gna trace <source|file>\n"); return 1; }
        const char *src = argv[2];
        FileData f = sys_read_file(src, alloc_for_file);
        const char *code = f.data ? f.data : src;
        // Compile with tracing — uses g_world internally
        tap_reset(); tap_on();
        i64 result = jit_run(code);
        tap_off();
        // Use the world gram that jit_run populated (same GNode IDs)
        Gram *g = &g_world.gram;
        // Build runtime view from trace
        u64 *m_hit = bm_new(g->mw);
        tap_to_bitmask(TK_JIT, m_hit, g->mw);
        u32 *counts = (u32 *)arena_alloc(&g_perm, g->n * sizeof(u32), 4);
        memset(counts, 0, g->n * sizeof(u32));
        tap_hit_counts(TK_JIT, counts, g->n);
        // Show annotated source + result
        gram_annotate(g, m_hit, counts);
        pfc(C_DIM); pf("  result: "); pfc(C_RESET);
        pf("%lld\n", (long long)result);
        if (f.data) sys_free(f.data, f.len + 1);
        return 0;
    }

    if (strcmp(cmd, "save") == 0) {
        if (argc < 4) { pf("usage: gna save <output.gen> <source|file>\n"); return 1; }
        const char *outpath = argv[2];
        const char *src = argv[3];
        FileData f = sys_read_file(src, alloc_for_file);
        const char *code = f.data ? f.data : src;
        u32 len = f.data ? (u32)f.len : (u32)strlen(code);
        Lang l; lang_lisp(&l);
        Gram g = gram_new(len < 256 ? 4096 : len * 4);
        gram_parse(&g, &l, code, len);
        gram_index(&g);
        gram_analyze(&g);
        bool ok = gram_save(&g, outpath);
        if (f.data) sys_free(f.data, f.len + 1);
        if (ok) pf("saved %u nodes to %s\n", g.n, outpath);
        else pf("save failed\n");
        return ok ? 0 : 1;
    }

    if (strcmp(cmd, "load") == 0) {
        if (argc < 3) { pf("usage: gna load <input.gen>\n"); return 1; }
        Gram g = {0};
        if (!gram_load(&g, argv[2])) { pf("load failed\n"); return 1; }
        grammar_init();  // restore S_* globals (same StrIds, just re-assigns)
        i64 result = jit_run_gram(&g);
        pf("%lld\n", (long long)result);
        return 0;
    }

    // observe: fork'd observer reads shared trace while subject evals
    if (strcmp(cmd, "observe") == 0) {
        if (argc < 3) { pf("usage: gna observe <expr>\n"); return 1; }
        if (tap_share() < 0) { pf("mmap failed\n"); return 1; }
        int pid = sys_fork();
        if (pid < 0) { pf("fork failed\n"); return 1; }
        if (pid == 0) {
            // Child = observer
            tap_observe();  // never returns
        }
        // Parent = subject
        tap_on();
        Val form = gram_read(argv[2]);
        g_signal = SIGNAL_NONE; g_depth = 0;
        Val result = eval(form, g_global_env);
        tap_off();
        tap_done();
        int status = 0;
        sys_waitpid(pid, &status, 0);
        // Print result after observer finishes
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
        else { pfc(C_DIM); pf("=> "); pfc(C_RESET); pp_val(result); pf("\n"); print_flush(); }
        tap_unshare();
        return 0;
    }

    // tap: eval with ring buffer trace, dump call tree + obs afterward
    if (strcmp(cmd, "tap") == 0) {
        if (argc < 3) { pf("usage: gna tap <expr>\n"); return 1; }
        obs_reset(); tap_reset();
        g_obs_level = 2;  // enable timing for trace
        tap_on();
        // Eval all forms
        Gram *g = world_step(argv[2], false);
        g_signal = SIGNAL_NONE; g_depth = 0;
        Val result = NIL;
        u32 tc = g->nodes[0].child;
        while (tc) {
            result = eval_node(g, tc, g_global_env);
            if (g_signal) break;
            tc = g->nodes[tc].next;
        }
        tap_off();
        g_obs_level = 1;
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }
        else { pp_val(result); pf("\n"); }
        print_flush();
        pf("\n");
        trace_dump(200);
        pf("\n");
        obs_dump(30);
        print_flush();
        return 0;
    }

    // obs: eval with observation → full debug report
    // obs <expr>      — tier 1 (counts + sequence + allocs)
    // obs -t <expr>   — tier 2 (+ per-name self-time via rdtsc)
    if (strcmp(cmd, "obs") == 0) {
        if (argc < 3) { pf("usage: gna obs [-t] <expr>\n"); return 1; }
        int src_arg = 2;
        if (strcmp(argv[2], "-t") == 0) { g_obs_level = 2; src_arg = 3; }
        else { g_obs_level = 1; }
        if (src_arg >= argc) { pf("usage: gna obs [-t] <expr>\n"); return 1; }
        obs_reset();

        // Parse (timed separately from analyze for obs)
        u64 t0 = now_ns();
        Gram *g = world_step(argv[src_arg], false);
        u64 t_parse = now_ns() - t0;
        t0 = now_ns();
        gram_index(g);
        gram_analyze(g);
        u64 t_analyze = now_ns() - t0;

        // Eval all top-level forms
        PerfCounters pc = perf_init();
        g_signal = SIGNAL_NONE; g_depth = 0;
        Val result = NIL;
        tap_on();  // calibrate timing
        t0 = now_ns();
        perf_start(&pc);
        u32 c = g->nodes[0].child;
        while (c) {
            result = eval_node(g, c, g_global_env);
            if (g_signal) break;
            c = g->nodes[c].next;
        }
        perf_stop(&pc);
        tap_off();  // flush final timing
        u64 t_eval = now_ns() - t0;
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); }

        u64 phases[3] = {t_parse, t_analyze, t_eval};
        obs_report(g, result, t_eval, phases, &pc);
        perf_close(&pc);
        g_obs_level = 1;  // restore default
        return 0;
    }

    if (strcmp(cmd, "php") == 0) {
        if (argc < 3) { pf("usage: gna php <source|file>\n"); return 1; }
        const char *src = argv[2];
        FileData f = sys_read_file(src, alloc_for_file);
        const char *code = f.data ? f.data : src;
        // Parse source into world gram (gn-* builtins read from g_world.gram)
        world_step(code, true);
        // Call Clojure emitter via engine_eval (preserves g_world.gram)
        g_signal = SIGNAL_NONE; g_depth = 0;
        engine_eval("(emit-php-program)");
        if (g_signal) { g_signal = SIGNAL_NONE; print_flush(); if (f.data) sys_free(f.data, f.len + 1); return 1; }
        // Write output buffer to stdout
        if (g_out.buf && g_out.pos > 0) sys_write(1, g_out.buf, g_out.pos);
        if (f.data) sys_free(f.data, f.len + 1);
        return 0;
    }

    pf("unknown command: %s\n", cmd);
    pf("usage: gna [test|bench|watch|eval|jit|emit|run|check|parse|render|trace|tap|observe|obs|save|load|php] [args]\n");
    return 1;
}

#endif // CLI_C_INCLUDED
