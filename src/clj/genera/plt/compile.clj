;; 07-compile.clj — Compiler entry points
;;
;; Pipeline: source → parse → scope → type → flow → compile → execute
;;
;; cg-compile-defn:    one (defn name [params] body...) → x86
;; cg-compile-program: classify top-level forms, compile all, return entry

;; cg-compile-defn: compile a defn form
(defn cg-compile-defn [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        pv (gn-next nm)
        name (gn-sym nm)
        n-params (loop [p (gn-child pv) c 0]
                   (if (= p 0) c (recur (gn-next p) (inc c))))
        tco (gn-has-recur? id)
        cs-mode (and (not tco) (<= n-params N_CS))
        n-cs (if cs-mode n-params 0)
        fn-start (code-pos)]
    (comp-reset! cs-mode n-cs (* (- 0 n-cs) 8))
    (if cs-mode
      ;; CS mode: callee-saved registers for params
      (do (x86-push! RBP)
          (x86-mov! RBP RSP)
          (loop [i 0]
            (when (< i n-cs)
              (x86-push-r! (cs-pool i))
              (recur (inc i))))
          (let [frame (if (= (mod n-cs 2) 1) 248 256)]
            (xb! 72) (xb! 129) (xb! 236) (x32! frame)
            (loop [p (gn-child pv) pi 0]
              (when (not (= p 0))
                (comp-cs-name! pi (gn-sym p))
                (x86-mov-rr! (cs-pool pi) (arg-reg pi))
                (recur (gn-next p) (inc pi))))
            (loop [body (gn-next pv)]
              (when (not (= body 0))
                (cg-expr body)
                (recur (gn-next body))))
            (xb! 72) (xb! 129) (xb! 196) (x32! frame)
            (loop [i (dec n-cs)]
              (when (>= i 0)
                (x86-pop-r! (cs-pool i))
                (recur (dec i))))
            (x86-pop! RBP)
            (x86-ret!)))
      ;; Standard mode: stack frame
      (do (x86-prologue! 256)
          (loop [p (gn-child pv) pi 0]
            (when (not (= p 0))
              (let [off (comp-alloc-slot! (gn-sym p))]
                (x86-store! off (arg-reg pi)))
              (recur (gn-next p) (inc pi))))
          (if tco
            (do (comp-set-in-loop! true)
                (comp-set-loop-count! n-params)
                (loop [p (gn-child pv) i 0]
                  (when (not (= p 0))
                    (comp-loop-var! i (gn-sym p)
                      (comp-find-local (gn-sym p)))
                    (recur (gn-next p) (inc i))))
                (comp-set-loop-start! (code-pos))
                (let [body-first (gn-next pv)]
                  (loop [body body-first last-b 0]
                    (if (= body 0)
                      (when (not (= last-b 0)) (cg-tail last-b))
                      (do (when (not (= last-b 0)) (cg-expr last-b))
                        (recur (gn-next body) body))))))
            (loop [body (gn-next pv)]
              (when (not (= body 0))
                (cg-expr body)
                (recur (gn-next body)))))
          (x86-epilogue!)))
    (jit-register-fn! name fn-start)))

;; cg-compile-program: compile a full program
(defn cg-compile-program []
  ;; Classify top-level forms
  (let [forms (loop [ch (gn-child 0) defns [] defs [] mains []]
               (if (= ch 0)
                 (list defns defs mains)
                 (if (and (= (gn-kind ch) NK_LIST) (gn-child ch)
                          (= (gn-kind (gn-child ch)) NK_IDENT))
                   (let [sym (gn-sym (gn-child ch))]
                     (cond
                       (= sym SYM_DEFN)
                         (recur (gn-next ch) (conj defns ch) defs mains)
                       (= sym SYM_DEF)
                         (let [nm (gn-next (gn-child ch))]
                           (recur (gn-next ch) defns
                             (conj defs (list (gn-sym nm) (gn-next nm)))
                             mains))
                       1 (recur (gn-next ch) defns defs
                            (conj mains ch))))
                   (recur (gn-next ch) defns defs
                     (conj mains ch)))))
        defns (first forms)
        defs  (first (rest forms))
        mains (first (rest (rest forms)))]
    ;; Register globals
    (loop [i 0]
      (when (< i (count defs))
        (jit-register-global! (first (nth defs i)))
        (recur (inc i))))
    ;; Compile defns
    (loop [i 0]
      (when (< i (count defns))
        (cg-compile-defn (nth defns i))
        (recur (inc i))))
    ;; Entry: init globals + run mains
    (let [entry (code-pos)]
      (comp-reset! false 0 0)
      (x86-prologue! 256)
      (loop [i 0]
        (when (< i (count defs))
          (cg-expr (first (rest (nth defs i))))
          (x86-store-abs! i)
          (recur (inc i))))
      (loop [i 0]
        (when (< i (count mains))
          (cg-expr (nth mains i))
          (recur (inc i))))
      (x86-epilogue!)
      (jit-patch-calls!)
      entry)))