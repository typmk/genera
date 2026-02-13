;; 06-emit.clj — Expression compiler (syntax GNodes → x86)
;;
;; Recursive walker: cg-expr emits x86 for each syntax node.
;; cg-tail propagates tail position for TCO.
;; Called by 07-compile.clj for each defn/main form.

;; cmp-cc: symbol → condition code (or -1)
(defn cmp-cc [sym]
  (cond (= sym SYM_EQ)  CC_E
        (= sym SYM_LT)  CC_L
        (= sym SYM_GT)  CC_G
        (= sym SYM_LTE) CC_LE
        (= sym SYM_GTE) CC_GE
        1 -1))

;; cg-binop: arithmetic (+, -, *, /, mod)
(defn cg-binop [op arg]
  (let [n (loop [a arg c 0]
           (if (= a 0) c (recur (gn-next a) (inc c))))]
    (cond
      (and (= n 1) (= op SYM_SUB))
        (do (cg-expr arg) (x86-neg! RAX))
      (= n 0)
        (x86-imm! RAX (if (= op SYM_MUL) 1 0))
      1
        (do (cg-expr arg)
          (loop [a (gn-next arg)]
            (when (not (= a 0))
              (x86-push! RAX)
              (cg-expr a)
              (x86-mov! RCX RAX)
              (x86-pop! RAX)
              (cond (= op SYM_ADD) (x86-add! RAX RCX)
                    (= op SYM_SUB) (x86-sub! RAX RCX)
                    (= op SYM_MUL) (x86-imul! RAX RCX)
                    (= op SYM_DIV) (x86-idiv! RCX)
                    (= op SYM_MOD) (do (x86-idiv! RCX)
                                       (x86-mov! RAX RDX)))
              (recur (gn-next a))))))))

;; cg-cmp: comparison operators
(defn cg-cmp [cc arg]
  (cg-expr arg)
  (x86-push! RAX)
  (cg-expr (gn-next arg))
  (x86-mov! RCX RAX)
  (x86-pop! RAX)
  (x86-cmp! RAX RCX)
  (x86-setcc! cc))

;; cg-if: if expression
(defn cg-if [test-id]
  (let [then (gn-next test-id)
        els  (if then (gn-next then) 0)
        fc   (gn-child test-id)
        fcc  (if (and (= (gn-kind test-id) NK_LIST) fc
                      (= (gn-kind fc) NK_IDENT))
                (cmp-cc (gn-sym fc)) -1)]
    (if (>= fcc 0)
      ;; Fused compare
      (let [ca (gn-next fc)]
        (cg-expr ca)
        (x86-push! RAX)
        (cg-expr (gn-next ca))
        (x86-mov! RCX RAX)
        (x86-pop! RAX)
        (x86-cmp! RAX RCX)
        (let [f-else (x86-jcc! (bit-xor fcc 1))]
          (cg-expr then)
          (let [f-end (x86-jmp!)]
            (x86-patch! f-else)
            (if els (cg-expr els) (x86-imm! RAX 0))
            (x86-patch! f-end))))
      ;; General if
      (do (cg-expr test-id)
        (x86-test! RAX RAX)
        (let [f-else (x86-jcc! CC_E)]
          (cg-expr then)
          (let [f-end (x86-jmp!)]
            (x86-patch! f-else)
            (if els (cg-expr els) (x86-imm! RAX 0))
            (x86-patch! f-end)))))))

;; cg-let: let binding
(defn cg-let [bv]
  (let [saved (comp-save-locals!)]
    (loop [p (gn-child bv)]
      (when (not (= p 0))
        (let [name (gn-sym p)
              vn (gn-next p)]
          (when (not (= vn 0))
            (cg-expr vn)
            (let [off (comp-alloc-slot! name)]
              (x86-store! off RAX))
            (recur (gn-next vn))))))
    (loop [body (gn-next bv)]
      (when (not (= body 0))
        (cg-expr body)
        (recur (gn-next body))))
    (comp-restore-locals! saved)))

;; cg-do: do expression
(defn cg-do [body]
  (loop [b body]
    (when (not (= b 0))
      (cg-expr b)
      (recur (gn-next b)))))

;; cg-and, cg-or
(defn cg-and [arg]
  (if (= arg 0)
    (x86-imm! RAX 1)
    (do (cg-expr arg)
      (loop [a (gn-next arg) fixes []]
        (if (= a 0)
          (loop [f fixes]
            (when (cons? f)
              (x86-patch! (first f))
              (recur (rest f))))
          (do (x86-test! RAX RAX)
            (let [fix (x86-jcc! CC_E)]
              (cg-expr a)
              (recur (gn-next a) (cons fix fixes)))))))))

(defn cg-or [arg]
  (if (= arg 0)
    (x86-imm! RAX 0)
    (do (cg-expr arg)
      (loop [a (gn-next arg) fixes []]
        (if (= a 0)
          (loop [f fixes]
            (when (cons? f)
              (x86-patch! (first f))
              (recur (rest f))))
          (do (x86-test! RAX RAX)
            (let [fix (x86-jcc! CC_NE)]
              (cg-expr a)
              (recur (gn-next a) (cons fix fixes)))))))))

;; cg-recur: recur expression (inside loop)
(defn cg-recur [arg]
  (let [n (loop [a arg c 0]
           (if (= a 0) c
             (do (cg-expr a) (x86-push! RAX)
               (recur (gn-next a) (inc c)))))]
    (loop [i (dec n)]
      (when (>= i 0)
        (x86-pop! RAX)
        (x86-store! (comp-loop-off i) RAX)
        (recur (dec i))))
    (x86-jmp-to! (comp-loop-start))))

;; cg-loop: loop expression
(defn cg-loop [bv]
  (let [saved (comp-loop-save!)
        saved-lc (comp-save-locals!)]
    (comp-set-loop-count! 0)
    (loop [p (gn-child bv)]
      (when (not (= p 0))
        (let [name (gn-sym p)
              vn (gn-next p)]
          (when (not (= vn 0))
            (cg-expr vn)
            (let [off (comp-alloc-slot! name)
                  lc (comp-loop-count)]
              (x86-store! off RAX)
              (comp-loop-var! lc name off)
              (comp-set-loop-count! (inc lc)))
            (recur (gn-next vn))))))
    (comp-set-loop-start! (code-pos))
    (comp-set-in-loop! true)
    (loop [body (gn-next bv)]
      (when (not (= body 0))
        (cg-expr body)
        (recur (gn-next body))))
    (comp-restore-locals! saved-lc)
    (comp-loop-restore! saved)))

;; cg-cond: cond expression
(defn cg-cond [arg]
  (loop [a arg end-fixes []]
    (if (or (= a 0) (= (gn-next a) 0))
      (do (when (= a 0) (x86-imm! RAX 0))
        (loop [f end-fixes]
          (when (cons? f)
            (x86-patch! (first f))
            (recur (rest f)))))
      (let [expr (gn-next a)]
        (if (= (gn-kind a) NK_KW)
          (do (cg-expr expr)
            (loop [f end-fixes]
              (when (cons? f)
                (x86-patch! (first f))
                (recur (rest f)))))
          (do (cg-expr a)
            (x86-test! RAX RAX)
            (let [skip (x86-jcc! CC_E)]
              (cg-expr expr)
              (let [end (x86-jmp!)]
                (x86-patch! skip)
                (recur (gn-next expr)
                       (cons end end-fixes))))))))))

;; cg-when: when expression
(defn cg-when [arg]
  (cg-expr arg)
  (x86-test! RAX RAX)
  (let [skip (x86-jcc! CC_E)]
    (loop [a (gn-next arg)]
      (when (not (= a 0))
        (cg-expr a)
        (recur (gn-next a))))
    (x86-patch! skip)))

;; cg-tail: tail position compilation (TCO for recur, if, do, let)
(defn cg-tail [id]
  (if (and (= (gn-kind id) NK_LIST) (gn-child id)
           (= (gn-kind (gn-child id)) NK_IDENT))
    (let [fc (gn-child id)
          sym (gn-sym fc)
          args (gn-next fc)]
      (cond
        (= sym SYM_RECUR) (cg-recur args)
        (= sym SYM_IF)
          (let [test args
                then (gn-next test)
                els (if then (gn-next then) 0)
                tfc (gn-child test)
                fcc (if (and (= (gn-kind test) NK_LIST) tfc
                            (= (gn-kind tfc) NK_IDENT))
                      (cmp-cc (gn-sym tfc)) -1)]
            (if (>= fcc 0)
              (let [ca (gn-next tfc)]
                (cg-expr ca) (x86-push! RAX)
                (cg-expr (gn-next ca)) (x86-mov! RCX RAX) (x86-pop! RAX)
                (x86-cmp! RAX RCX)
                (let [f-else (x86-jcc! (bit-xor fcc 1))]
                  (cg-tail then)
                  (let [f-end (x86-jmp!)]
                    (x86-patch! f-else)
                    (if els (cg-tail els) (x86-imm! RAX 0))
                    (x86-patch! f-end))))
              (do (cg-expr test)
                (x86-test! RAX RAX)
                (let [f-else (x86-jcc! CC_E)]
                  (cg-tail then)
                  (let [f-end (x86-jmp!)]
                    (x86-patch! f-else)
                    (if els (cg-tail els) (x86-imm! RAX 0))
                    (x86-patch! f-end))))))
        (= sym SYM_DO)
          (loop [e args last-e 0]
            (if (= e 0)
              (when (not (= last-e 0)) (cg-tail last-e))
              (do (when (not (= last-e 0)) (cg-expr last-e))
                (recur (gn-next e) e))))
        (= sym SYM_LET)
          (let [bv args
                saved (comp-save-locals!)]
            (loop [p (gn-child bv)]
              (when (not (= p 0))
                (let [name (gn-sym p)
                      vn (gn-next p)]
                  (when (not (= vn 0))
                    (cg-expr vn)
                    (let [off (comp-alloc-slot! name)]
                      (x86-store! off RAX))
                    (recur (gn-next vn))))))
            (loop [body (gn-next bv) last-b 0]
              (if (= body 0)
                (when (not (= last-b 0)) (cg-tail last-b))
                (do (when (not (= last-b 0)) (cg-expr last-b))
                  (recur (gn-next body) body))))
            (comp-restore-locals! saved))
        1 (cg-expr id)))
    (cg-expr id)))

;; cg-expr: main expression compiler
(defn cg-expr [id]
  (let [k (gn-kind id)]
    (cond
      (= k NK_NUM) (x86-imm! RAX (gn-parse-int id))
      (= k NK_IDENT)
        (let [name (gn-sym id)]
          (cond
            (= name SYM_TRUE)  (x86-imm! RAX 1)
            (= name SYM_FALSE) (x86-imm! RAX 0)
            (= name SYM_NIL)   (x86-imm! RAX 0)
            1 (let [cs (if (comp-cs-mode?) (comp-find-cs name) -1)]
                (if (>= cs 0)
                  (x86-mov-rr! RAX cs)
                  (let [off (comp-find-local name)]
                    (if (not (= off 0))
                      (x86-load! RAX off)
                      (let [gi (jit-find-global name)]
                        (if (>= gi 0)
                          (x86-load-abs! gi)
                          (x86-imm! RAX 0)))))))))
      (not (= k NK_LIST)) (x86-imm! RAX 0)
      (= (gn-child id) 0) (x86-imm! RAX 0)
      1
        (let [fc (gn-child id)]
          (if (not (= (gn-kind fc) NK_IDENT))
            (x86-imm! RAX 0)
            (let [sym (gn-sym fc)
                  args (gn-next fc)]
              (cond
                (= sym SYM_IF)   (cg-if args)
                (= sym SYM_LET)  (cg-let args)
                (= sym SYM_DO)   (cg-do args)
                (= sym SYM_AND)  (cg-and args)
                (= sym SYM_OR)   (cg-or args)
                (= sym SYM_LOOP) (cg-loop args)
                (and (= sym SYM_RECUR) (comp-in-loop?))
                  (cg-recur args)
                (= sym SYM_COND) (cg-cond args)
                (= sym SYM_WHEN) (cg-when args)
                (or (= sym SYM_ADD) (= sym SYM_SUB)
                    (= sym SYM_MUL) (= sym SYM_DIV)
                    (= sym SYM_MOD))
                  (cg-binop sym args)
                (>= (cmp-cc sym) 0)
                  (cg-cmp (cmp-cc sym) args)
                (= sym SYM_NOT)
                  (do (cg-expr args)
                      (x86-test! RAX RAX)
                      (x86-setcc! CC_E))
                (= sym SYM_INC)
                  (do (cg-expr args) (x86-add-i8! RAX 1))
                (= sym SYM_DEC)
                  (do (cg-expr args) (x86-sub-i8! RAX 1))
                (= sym SYM_ZEROQ)
                  (do (cg-expr args)
                      (x86-test! RAX RAX)
                      (x86-setcc! CC_E))
                (= sym SYM_POSQ)
                  (do (cg-expr args)
                      (x86-test! RAX RAX)
                      (x86-setcc! CC_G))
                (= sym SYM_NEGQ)
                  (do (cg-expr args)
                      (x86-test! RAX RAX)
                      (x86-setcc! CC_L))
                ;; Bit ops (binary: bit-and, bit-or, bit-xor)
                (or (= sym SYM_BAND) (= sym SYM_BOR) (= sym SYM_BXOR))
                  (do (cg-expr args)
                      (x86-push! RAX)
                      (cg-expr (gn-next args))
                      (x86-mov! RCX RAX)
                      (x86-pop! RAX)
                      (cond (= sym SYM_BAND) (x86-and! RAX RCX)
                            (= sym SYM_BOR)  (x86-or! RAX RCX)
                            (= sym SYM_BXOR) (x86-xor! RAX RCX)))
                ;; Bit shift (value in RAX, count in CL)
                (or (= sym SYM_BSHL) (= sym SYM_BSHR))
                  (do (cg-expr args)
                      (x86-push! RAX)
                      (cg-expr (gn-next args))
                      (x86-mov! RCX RAX)
                      (x86-pop! RAX)
                      (if (= sym SYM_BSHL)
                        (x86-shl-cl! RAX)
                        (x86-shr-cl! RAX)))
                ;; Unary: bit-not, popcount
                (= sym SYM_BNOT)
                  (do (cg-expr args) (x86-bit-not! RAX))
                (= sym SYM_POPCNT)
                  (do (cg-expr args) (x86-popcnt! RAX RAX))
                ;; Memory: load64/load32/load8 (addr) → value
                (= sym SYM_LOAD64)
                  (do (cg-expr args)
                      (x86-load-base! RAX RAX 0))
                (= sym SYM_LOAD32)
                  (do (cg-expr args)
                      (x86-load32-base! RAX RAX 0))
                (= sym SYM_LOAD8)
                  (do (cg-expr args)
                      (x86-load8-base! RAX RAX 0))
                ;; Memory: store64/store32/store8 (addr val) → val
                (or (= sym SYM_STORE64) (= sym SYM_STORE32) (= sym SYM_STORE8))
                  (do (cg-expr args)
                      (x86-push! RAX)
                      (cg-expr (gn-next args))
                      (x86-mov! RCX RAX)
                      (x86-pop! RAX)
                      (cond (= sym SYM_STORE64) (x86-store-base! RAX 0 RCX)
                            (= sym SYM_STORE32) (x86-store32-base! RAX 0 RCX)
                            1                   (x86-store8-base! RAX 0 RCX))
                      (x86-mov! RAX RCX))
                ;; Function call
                1 (let [n-args (loop [a args c 0]
                                (if (= a 0) c
                                  (recur (gn-next a) (inc c))))]
                    (if (= n-args 1)
                      (do (cg-expr args)
                          (x86-mov! RDI RAX))
                      (do (loop [a args]
                            (when (not (= a 0))
                              (cg-expr a) (x86-push! RAX)
                              (recur (gn-next a))))
                        (loop [i (dec n-args)]
                          (when (>= i 0)
                            (x86-pop! (arg-reg i))
                            (recur (dec i))))))
                    (let [fix (x86-call!)]
                      (jit-add-fix! fix sym)))))))))))

