;; test/all.clj — Entry point for Clojure test suite
;;
;; Called from C's run_all_tests() after C-only tests complete.
;; Each file loaded separately by C with signal reset between files.
;;
;; Active test files:
;;   core.clj   — test harness (test-compare, run-tests)
;;   eval.clj   — eval/collections/macros/stdlib tests
;;   passes.clj — pass comparison tests (C vs Clojure)
;;
;; Deferred (need arena isolation — jit_run/cc_emit reset g_req):
;;   jit.clj    — JIT tests via jit-run/clj-jit-run builtins
;;   emit.clj   — C emit tests via cc-emit builtin

(load "src/clj/test/core.clj")
(load "src/clj/test/eval.clj")
(load "src/clj/test/passes.clj")
(load "src/clj/test/alloc.clj")
