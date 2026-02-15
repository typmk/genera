;; test/core.clj — Clojure test harness
;;
;; Uses builtins: is, group, describe!, it!
;; These are registered by C (register_test_builtins + test_engine).

(defn run-tests [& test-fns]
  (loop [fns test-fns]
    (when (cons? fns)
      ((first fns))
      (recur (rest fns)))))

(defn test-compare [src]
  (world-parse! src)
  (c-analyze!)
  (analyze-alloc!)
  (views-snapshot!)
  (views-clear!)
  (clj-pass-scope)
  (clj-pass-type (gn-count))
  (clj-pass-flow)
  (analyze-alloc!)
  (it! src (views-match?)))

(defn test-index-compare [src]
  (world-parse! src)
  (index-snapshot!)
  (index-clear!)
  (clj-gram-index)
  (it! (str "idx:" src) (index-match?)))

(defn test-eval-compare [src]
  (world-parse! src)
  (let [root-child (gn-child 0)
        env (global-env)]
    (it! (str "eval:" src) (eval-node-match? root-child env))))
