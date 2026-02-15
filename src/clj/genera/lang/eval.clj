;; lang/eval.clj — Clojure eval_node (port of eval_node from eval.c)
;;
;; Walks GNode tree and computes values. Same tree walk as JIT's cg_expr
;; (already in Clojure as cg-expr), different leaf action: eval computes Val.
;;
;; C builtins: gn-kind, gn-child, gn-next, gn-parent, gn-sym, gn-count,
;;             gn-parse-int, gn-has-dot, gn-end, gn-text, gn-parse-num-val,
;;             entity-to-val, clj-eval, clj-apply,
;;             view?, val-get, val-set!, scope-get, bind-get
;; C constants: NK_*, V_*, SYM_*

;; Evaluate a sequence of sibling nodes, return last
(defn clj-eval-body [first-id env]
  (loop [c first-id result nil]
    (if (= c 0)
      result
      (recur (gn-next c) (clj-eval-node c env)))))

;; Evaluate args into a cons list
(defn clj-eval-args [first-id env]
  (if (= first-id 0)
    nil
    (cons (clj-eval-node first-id env)
          (clj-eval-args (gn-next first-id) env))))

;; Capture GNode siblings as Val cons list (for closure body / macro args)
(defn clj-gn-to-val-list [first-id]
  (if (= first-id 0)
    nil
    (cons (entity-to-val first-id)
          (clj-gn-to-val-list (gn-next first-id)))))

;; Main eval dispatcher
(declare clj-eval-node)

(defn clj-eval-node [id env]
  (if (= id 0)
    nil
    (let [k (gn-kind id)]
      (cond
        ;; Numeric literal
        (= k NK_NUM)
        (gn-parse-num-val id)

        ;; Identifier — variable lookup
        (= k NK_IDENT)
        (let [sym (gn-sym id)]
          (cond
            (= sym SYM_NIL)   nil
            (= sym SYM_TRUE)  true
            (= sym SYM_FALSE) false
            :else (env-get env sym)))

        ;; String literal
        (= k NK_STR)
        (let [text (gn-text id)]
          ;; Return string val (builtin handles it)
          (entity-to-val id))

        ;; Keyword
        (= k NK_KW)
        (entity-to-val id)

        ;; Quote
        (= k NK_QUOTE)
        (if (= (gn-child id) 0) nil (entity-to-val (gn-child id)))

        ;; Vector literal
        (= k NK_VEC)
        (loop [c (gn-child id) acc []]
          (if (= c 0)
            acc
            (recur (gn-next c) (conj acc (clj-eval-node c env)))))

        ;; Map literal
        (= k NK_MAP)
        (loop [c (gn-child id) acc {}]
          (if (= c 0)
            acc
            (let [key (clj-eval-node c env)
                  vn  (gn-next c)]
              (if (= vn 0)
                acc
                (recur (gn-next vn)
                       (assoc acc key (clj-eval-node vn env)))))))

        ;; List — special forms + function call
        (= k NK_LIST)
        (let [fc (gn-child id)]
          (if (= fc 0)
            nil
            (if (= (gn-kind fc) NK_IDENT)
              (let [sym  (gn-sym fc)
                    args (gn-next fc)]
                (cond
                  ;; quote
                  (= sym SYM_QUOTE)
                  (if (= args 0) nil (entity-to-val args))

                  ;; def
                  (= sym SYM_DEF)
                  (let [value (clj-eval-node (gn-next args) env)]
                    (clj-eval `(def ~(entity-to-val args) ~value))
                    value)

                  ;; if
                  (= sym SYM_IF)
                  (let [cond-val (clj-eval-node args env)
                        then-n  (gn-next args)]
                    (if cond-val
                      (clj-eval-node then-n env)
                      (let [else-n (if (= then-n 0) 0 (gn-next then-n))]
                        (if (= else-n 0) nil (clj-eval-node else-n env)))))

                  ;; do
                  (= sym SYM_DO)
                  (clj-eval-body args env)

                  ;; let — self-hosted: create env, bind pairs, eval body
                  (= sym SYM_LET)
                  (let [bv-node args
                        env2 (env-create! env)]
                    ;; Walk binding pairs: name1 val1 name2 val2 ...
                    (loop [c (gn-child bv-node)]
                      (when (not= c 0)
                        (let [val-node (gn-next c)]
                          (env-set! env2 (gn-sym c) (clj-eval-node val-node env2))
                          (recur (gn-next val-node)))))
                    ;; Evaluate body in new env
                    (clj-eval-body (gn-next bv-node) env2))

                  ;; fn
                  (= sym SYM_FN)
                  (clj-eval
                    (cons 'fn
                      (cons (entity-to-val args)
                            (clj-gn-to-val-list (gn-next args)))))

                  ;; defn
                  (= sym SYM_DEFN)
                  (let [name-id args
                        pv      (gn-next name-id)]
                    (clj-eval
                      (cons 'defn
                        (cons (entity-to-val name-id)
                              (cons (entity-to-val pv)
                                    (clj-gn-to-val-list (gn-next pv)))))))

                  ;; loop
                  (= sym SYM_LOOP)
                  (clj-eval
                    (cons 'loop
                      (cons (entity-to-val args)
                            (clj-gn-to-val-list (gn-next args)))))

                  ;; defmacro
                  (= sym SYM_DEFMACRO)
                  (let [name-id args
                        pv      (gn-next name-id)]
                    (clj-eval
                      (cons 'defmacro
                        (cons (entity-to-val name-id)
                              (cons (entity-to-val pv)
                                    (clj-gn-to-val-list (gn-next pv)))))))

                  ;; Default: function call or macro expand
                  :else
                  (let [fn-val (clj-eval-node fc env)]
                    (if (macro? fn-val)
                      ;; Macro: pass unevaluated args, eval expansion
                      (let [raw-args (clj-gn-to-val-list (gn-next fc))
                            expansion (clj-apply fn-val raw-args)]
                        (clj-eval expansion))
                      ;; Function: evaluate args, apply
                      (let [fn-args (clj-eval-args (gn-next fc) env)]
                        (clj-apply fn-val fn-args))))))

              ;; Head is not an ident — eval as function call
              (let [fn-val (clj-eval-node fc env)
                    fn-args (clj-eval-args (gn-next fc) env)]
                (clj-apply fn-val fn-args)))))

        ;; Default
        :else nil))))
