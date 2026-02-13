;; 02-type.clj — Pass 2: Type + Purity (backward scan)
;;
;; Fills V_INT, V_VEC, V_MAP, V_FN, V_PURE, V_CONST, val[].
;; Pure builtin calls with const args → V_CONST.
;; Int-returning builtins with int args → V_INT + const fold.

(defn clj-pass-type [n]
  (loop [i (dec n)]
    (when (>= i 0)
      (let [k (gn-kind i)]
        (when (= k NK_NUM)
          (view-set! V_CONST i)
          (when (not (gn-has-dot i))
            (view-set! V_INT i)
            (val-set! i (gn-parse-int i))))
        (when (= k NK_STR) (view-set! V_CONST i))
        (when (= k NK_VEC) (view-set! V_VEC i))
        (when (= k NK_MAP) (view-set! V_MAP i))
        (when (and (= k NK_IDENT)
                   (not (view? V_DEF i))
                   (not (view? V_REF i)))
          (let [s (gn-sym i)]
            (when (= s SYM_TRUE)
              (view-set! V_INT i) (view-set! V_CONST i) (val-set! i 1))
            (when (= s SYM_FALSE)
              (view-set! V_INT i) (view-set! V_CONST i) (val-set! i 0))
            (when (= s SYM_NIL)
              (view-set! V_INT i) (view-set! V_CONST i) (val-set! i 0))))
        (when (and (= k NK_LIST) (gn-child i)
                   (= (gn-kind (gn-child i)) NK_IDENT)
                   (= (gn-sym (gn-child i)) SYM_FN))
          (view-set! V_FN i))
        (when (and (view? V_CALL i) (= k NK_LIST) (gn-child i))
          (let [fc (gn-child i)]
            (when (= (gn-kind fc) NK_IDENT)
              (let [s (gn-sym fc)]
                (when (pure-bi? s)
                  (view-set! V_PURE i)
                  (when (all-args-view? fc V_CONST)
                    (view-set! V_CONST i))
                  (when (and (int-ret? s) (all-args-view? fc V_INT))
                    (view-set! V_INT i)
                    (when (all-args-view? fc V_CONST)
                      (val-set! i (cv-eval s fc))))))))))
      (recur (dec i)))))