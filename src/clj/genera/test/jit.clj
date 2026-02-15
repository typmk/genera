;; test/jit.clj — JIT tests in Clojure
;;
;; Data-driven: each test is (name, src, expected).
;; Uses jit-run and clj-jit-run builtins.

(describe! "clj jit tests")
(it! "jit:int" (= (jit-run "42") 42))
