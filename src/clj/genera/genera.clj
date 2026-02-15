;; genera.clj — Clojure bootstrap entry point
;; Mirrors genera.c: loads std -> sys -> lang -> plt
;;
;; rt/ files (coll, sig, commit, epoch, step) are NOT loaded at boot.
;; They define JIT/arena functions that need initialized memory pointers.
;; Load separately when needed: (load "src/clj/rt/coll.clj") etc.
;; Exception: alloc.clj is loaded at boot (uses only builtins, no memory pointers).

;; std — freestanding library
(load "src/clj/std/core.clj")
(load "src/clj/std/mem.clj")

;; sys — hardware boundary
(load "src/clj/sys/out.clj")
(load "src/clj/sys/x86.clj")

;; lang — passes (Clojure-specific analysis)
(load "src/clj/lang/scope.clj")
(load "src/clj/lang/type.clj")
(load "src/clj/lang/flow.clj")
(load "src/clj/lang/grammar.clj")
(load "src/clj/lang/bitmap.clj")

;; lang — allocation analysis (extends pass system, uses builtins only)
(load "src/clj/rt/alloc.clj")

;; lang — eval + grammar index (Clojure ports)
(load "src/clj/lang/index.clj")
(load "src/clj/lang/eval.clj")

;; plt — platform targets (codegen)
(load "src/clj/plt/emit.clj")
(load "src/clj/plt/compile.clj")
(load "src/clj/plt/platform_php.clj")
(load "src/clj/plt/php.clj")
(load "src/clj/plt/cc.clj")
(load "src/clj/plt/clj.clj")
