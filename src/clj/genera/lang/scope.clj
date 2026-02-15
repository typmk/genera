;; 01-scope.clj — Pass 1: Scope + Binding (recursive tree walk)
;;
;; Fills V_DEF, V_REF, V_CALL, bind[], scope[].
;; Scope chain threaded as cons list of (name def-id) pairs.

(declare p1-walk)

(defn p1-params [vec sf binds]
  (loop [p (gn-child vec) b binds]
    (if (= p 0) b
      (do (scope-set! p sf)
        (recur (gn-next p)
          (if (= (gn-kind p) NK_IDENT)
            (do (view-set! V_DEF p)
                (cons (list (gn-sym p) p) b))
            b))))))

(defn p1-let [vec sf binds]
  (loop [p (gn-child vec) is-name true b binds]
    (if (= p 0) b
      (do (scope-set! p sf)
        (if is-name
          (recur (gn-next p) false
            (if (= (gn-kind p) NK_IDENT)
              (do (view-set! V_DEF p)
                  (cons (list (gn-sym p) p) b))
              b))
          (do (p1-walk p sf b)
              (recur (gn-next p) true b)))))))

(defn p1-walk [id sf binds]
  (scope-set! id sf)
  (let [k (gn-kind id)]
    ;; NK_LIST — special forms + regular calls
    (when (= k NK_LIST)
      (let [fc (gn-child id)]
        (when (not (= fc 0))
          (scope-set! fc sf)
          (if (= (gn-kind fc) NK_IDENT)
            (let [sym (gn-sym fc)]
              (cond
                (= sym SYM_DEFN)
                  (let [nm (gn-next fc)]
                    (when (not (= nm 0))
                      (scope-set! nm sf)
                      (view-set! V_DEF nm)
                      (let [b2 (cons (list (gn-sym nm) nm) binds)
                            pv (gn-next nm)]
                        (when (not (= pv 0))
                          (scope-set! pv id)
                          (let [b3 (if (= (gn-kind pv) NK_VEC)
                                     (p1-params pv id b2) b2)]
                            (loop [e (gn-next pv)]
                              (when (not (= e 0))
                                (p1-walk e id b3)
                                (recur (gn-next e)))))))))
                (= sym SYM_DEF)
                  (let [nm (gn-next fc)]
                    (when (not (= nm 0))
                      (scope-set! nm sf)
                      (view-set! V_DEF nm)
                      (let [val (gn-next nm)]
                        (when (not (= val 0))
                          (p1-walk val sf
                            (cons (list (gn-sym nm) nm) binds))))))
                (= sym SYM_FN)
                  (let [pv (gn-next fc)]
                    (when (not (= pv 0))
                      (scope-set! pv id)
                      (let [b2 (if (= (gn-kind pv) NK_VEC)
                                 (p1-params pv id binds) binds)]
                        (loop [e (gn-next pv)]
                          (when (not (= e 0))
                            (p1-walk e id b2)
                            (recur (gn-next e)))))))
                (or (= sym SYM_LET) (= sym SYM_LOOP))
                  (let [bv (gn-next fc)]
                    (when (not (= bv 0))
                      (scope-set! bv sf)
                      (let [b2 (if (= (gn-kind bv) NK_VEC)
                                 (p1-let bv sf binds) binds)]
                        (loop [e (gn-next bv)]
                          (when (not (= e 0))
                            (p1-walk e sf b2)
                            (recur (gn-next e)))))))
                (or (= sym SYM_QUOTE) (= sym SYM_SYNTAX_QUOTE))
                  nil
                (= sym SYM_DO)
                  ;; do: thread binds through children (def propagates)
                  (loop [c (gn-next fc) b binds]
                    (when (not (= c 0))
                      (p1-walk c sf b)
                      (recur (gn-next c)
                        (if (and (= (gn-kind c) NK_LIST)
                                 (gn-child c)
                                 (= (gn-kind (gn-child c)) NK_IDENT)
                                 (= (gn-sym (gn-child c)) SYM_DEF))
                          (let [nm (gn-next (gn-child c))]
                            (if (and (not (= nm 0))
                                     (= (gn-kind nm) NK_IDENT))
                              (cons (list (gn-sym nm) nm) b) b))
                          b))))
                (special? sym)
                  (loop [c (gn-child id)]
                    (when (not (= c 0))
                      (p1-walk c sf binds)
                      (recur (gn-next c))))
                1  ;; else: regular call
                  (do (view-set! V_CALL id)
                    (loop [c (gn-child id)]
                      (when (not (= c 0))
                        (p1-walk c sf binds)
                        (recur (gn-next c)))))))
            ;; head is not ident: regular call
            (do (view-set! V_CALL id)
              (loop [c (gn-child id)]
                (when (not (= c 0))
                  (p1-walk c sf binds)
                  (recur (gn-next c)))))))))
    ;; Wrapper nodes
    (when (or (= k NK_QUOTE) (= k NK_SYNTAX_QUOTE)) nil)
    (when (or (= k NK_UNQUOTE) (= k NK_SPLICE))
      (loop [c (gn-child id)]
        (when (not (= c 0))
          (p1-walk c sf binds)
          (recur (gn-next c)))))
    ;; NK_IDENT — resolve references
    (when (= k NK_IDENT)
      (when (not (view? V_DEF id))
        (let [sym (gn-sym id)]
          (when (and (not (= sym SYM_NIL))
                     (not (= sym SYM_TRUE))
                     (not (= sym SYM_FALSE))
                     (not (special? sym)))
            (let [par (gn-parent id)]
              (when (not (and (= (gn-kind par) NK_LIST)
                             (= (gn-child par) id)))
                (view-set! V_REF id)
                (loop [b binds]
                  (when (cons? b)
                    (if (= (first (first b)) sym)
                      (bind-set! id (first (rest (first b))))
                      (recur (rest b)))))))))))
    ;; Other nodes: walk children
    (when (and (not (= k NK_LIST)) (not (= k NK_IDENT))
              (not (= k NK_QUOTE)) (not (= k NK_SYNTAX_QUOTE))
              (not (= k NK_UNQUOTE)) (not (= k NK_SPLICE)))
      (loop [c (gn-child id)]
        (when (not (= c 0))
          (p1-walk c sf binds)
          (recur (gn-next c)))))))

(defn clj-pass-scope []
  (loop [c (gn-child 0)]
    (when (not (= c 0))
      (p1-walk c 0 nil)
      (recur (gn-next c)))))