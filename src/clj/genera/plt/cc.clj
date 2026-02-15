;; plt/cc.clj — Clojure → C source emitter
;;
;; Port of cc.c. Recursive descent over GNode world gram, emitting C via out-emit.
;; Mirrors cc.c: walk the same tree, emit C source.
;;
;; C builtins: out-emit, out-nl, out-name, out-reset, out-str, gn-*, str-intern
;; C constants: NK_*, SYM_*, V_*

;; ============================================================================
;; 1. Helpers
;; ============================================================================

(defn cc-name [sym-id]
  (out-emit "clj_")
  (out-name sym-id))

(defn cc-binop-str [op]
  (cond
    (= op SYM_ADD) "+"  (= op SYM_SUB) "-"
    (= op SYM_MUL) "*"  (= op SYM_DIV) "/"
    (= op SYM_MOD) "%"  (= op SYM_EQ)  "=="
    (= op SYM_LT)  "<"  (= op SYM_GT)  ">"
    (= op SYM_LTE) "<=" (= op SYM_GTE) ">="
    :else "?"))

;; ============================================================================
;; 2. Expression emitter
;; ============================================================================

(declare cc-expr)
(declare cc-tail)

(defn cc-binop [op arg]
  ;; Count args
  (let [n (loop [a arg c 0] (if (= a 0) c (recur (gn-next a) (inc c))))]
    (cond
      ;; Unary minus
      (and (= n 1) (= op SYM_SUB))
      (do (out-emit "(-(") (cc-expr arg) (out-emit "))"))
      ;; No args
      (= n 0)
      (if (= op SYM_MUL) (out-emit "1") (out-emit "0"))
      ;; Binary+
      :else
      (do (out-emit "(") (cc-expr arg)
          (loop [a (gn-next arg)]
            (when (not (= a 0))
              (out-emit " ")
              (out-emit (cc-binop-str op))
              (out-emit " ")
              (cc-expr a)
              (recur (gn-next a))))
          (out-emit ")")))))

(defn cc-println [arg]
  (if (= arg 0)
    (out-emit "({ putchar('\\n'); 0; })")
    (do (out-emit "({ ")
        (loop [a arg n 0]
          (when (not (= a 0))
            (when (> n 0) (out-emit "putchar(' '); "))
            (if (= (gn-kind a) NK_STR)
              ;; String literal
              (do (out-emit "fputs(")
                  (out-emit (gn-text a))
                  (out-emit ", stdout); "))
              ;; Integer expression
              (do (out-emit "printf(\"%lld\", (long long)(")
                  (cc-expr a)
                  (out-emit ")); ")))
            (recur (gn-next a) (inc n))))
        (out-emit "putchar('\\n'); 0; })"))))

(defn cc-and [arg]
  (if (= arg 0)
    (out-emit "1")
    (do (out-emit "({ i64 _ta = ") (cc-expr arg) (out-emit ";")
        (loop [a (gn-next arg)]
          (when (not (= a 0))
            (out-emit " if (_ta) { _ta = ") (cc-expr a) (out-emit "; }")
            (recur (gn-next a))))
        (out-emit " _ta; })"))))

(defn cc-or [arg]
  (if (= arg 0)
    (out-emit "0")
    (do (out-emit "({ i64 _to = ") (cc-expr arg) (out-emit ";")
        (loop [a (gn-next arg)]
          (when (not (= a 0))
            (out-emit " if (!_to) { _to = ") (cc-expr a) (out-emit "; }")
            (recur (gn-next a))))
        (out-emit " _to; })"))))

(defn cc-if [args]
  (let [test args
        then (gn-next test)
        els  (if (= then 0) 0 (gn-next then))]
    (out-emit "(") (cc-expr test) (out-emit " ? ")
    (cc-expr then) (out-emit " : ")
    (if (= els 0) (out-emit "0") (cc-expr els))
    (out-emit ")")))

(defn cc-let [bv]
  (out-emit "({ ")
  (loop [p (gn-child bv)]
    (when (not (= p 0))
      (let [vn (gn-next p)]
        (when (not (= vn 0))
          (out-emit "i64 ")
          (cc-name (gn-sym p))
          (out-emit " = ")
          (cc-expr vn)
          (out-emit "; ")
          (recur (gn-next vn))))))
  (loop [b (gn-next bv)]
    (when (not (= b 0))
      (cc-expr b)
      (out-emit "; ")
      (recur (gn-next b))))
  (out-emit "})"))

(defn cc-do [body]
  (out-emit "({ ")
  (loop [b body]
    (when (not (= b 0))
      (cc-expr b) (out-emit "; ")
      (recur (gn-next b))))
  (out-emit "})"))

(defn cc-loop [bv]
  (out-emit "({ ")
  ;; Init bindings
  (loop [p (gn-child bv)]
    (when (not (= p 0))
      (let [vn (gn-next p)]
        (when (not (= vn 0))
          (out-emit "i64 ")
          (cc-name (gn-sym p))
          (out-emit " = ")
          (cc-expr vn)
          (out-emit "; ")
          (recur (gn-next vn))))))
  (out-emit "i64 _rl; ")
  (out-emit "while(1) { ")
  ;; Body — last expr gets tail treatment
  (let [body (gn-next bv)]
    (loop [b body]
      (when (not (= b 0))
        (if (= (gn-next b) 0)
          (cc-tail b)
          (do (cc-expr b) (out-emit "; ")))
        (recur (gn-next b)))))
  (out-emit "; } _rl; })"))

(defn cc-recur-stmts [arg bv]
  ;; Assign to temps, then copy back
  (out-emit "{ ")
  ;; Compute temps
  (loop [a arg p (gn-child bv) i 0]
    (when (and (not (= a 0)) (not (= p 0)))
      (let [vn (gn-next p)]
        (out-emit "i64 _rt")
        (out-emit (str i))
        (out-emit " = ")
        (cc-expr a)
        (out-emit "; ")
        (when (not (= vn 0))
          (recur (gn-next a) (gn-next vn) (inc i))))))
  ;; Copy back
  (loop [p (gn-child bv) i 0]
    (when (not (= p 0))
      (let [vn (gn-next p)]
        (cc-name (gn-sym p))
        (out-emit " = _rt")
        (out-emit (str i))
        (out-emit "; ")
        (when (not (= vn 0))
          (recur (gn-next vn) (inc i))))))
  (out-emit "continue; }"))

(defn cc-tail [id]
  ;; Handle recur in tail position
  (if (and (= (gn-kind id) NK_LIST) (not (= (gn-child id) 0)))
    (let [fc (gn-child id)]
      (if (= (gn-kind fc) NK_IDENT)
        (let [sym (gn-sym fc)
              args (gn-next fc)]
          (cond
            (= sym SYM_RECUR)
            ;; For now, fall back to expr (proper recur needs loop context)
            (do (out-emit "_rl = ") (cc-expr id) (out-emit "; break"))

            (= sym SYM_IF)
            (let [test args
                  then (gn-next test)
                  els  (if (= then 0) 0 (gn-next then))]
              (out-emit "if (") (cc-expr test) (out-emit ") { ")
              (cc-tail then)
              (out-emit "; } else { ")
              (if (= els 0)
                (out-emit "_rl = 0; break")
                (cc-tail els))
              (out-emit "; }"))

            (= sym SYM_DO)
            (do (loop [e args]
                  (when (not (= e 0))
                    (if (= (gn-next e) 0)
                      (cc-tail e)
                      (do (cc-expr e) (out-emit "; ")))
                    (recur (gn-next e)))))

            :else
            (do (out-emit "_rl = ") (cc-expr id) (out-emit "; break"))))
        (do (out-emit "_rl = ") (cc-expr id) (out-emit "; break"))))
    (do (out-emit "_rl = ") (cc-expr id) (out-emit "; break"))))

(defn cc-cond [args]
  (out-emit "(")
  (loop [a args first true]
    (when (and (not (= a 0)) (not (= (gn-next a) 0)))
      (let [expr (gn-next a)]
        (when (not first) (out-emit " : "))
        (if (= (gn-kind a) NK_KW)
          (cc-expr expr)
          (do (cc-expr a) (out-emit " ? ") (cc-expr expr)))
        (recur (gn-next expr) false))))
  (out-emit " : 0)"))

(defn cc-when [args]
  (out-emit "(") (cc-expr args) (out-emit " ? ({ ")
  (loop [a (gn-next args)]
    (when (not (= a 0))
      (cc-expr a) (out-emit "; ")
      (recur (gn-next a))))
  (out-emit "}) : 0)"))

;; Main expression dispatcher
(defn cc-expr [id]
  (let [k (gn-kind id)]
    (cond
      (= k NK_NUM)
      (out-emit (gn-text id))

      (= k NK_IDENT)
      (let [name (gn-sym id)]
        (cond
          (= name SYM_TRUE)  (out-emit "1")
          (= name SYM_FALSE) (out-emit "0")
          (= name SYM_NIL)   (out-emit "0")
          :else (cc-name name)))

      (= k NK_STR)
      (out-emit "0")

      (= k NK_LIST)
      (let [fc (gn-child id)]
        (if (= fc 0)
          (out-emit "0")
          (if (not (= (gn-kind fc) NK_IDENT))
            (out-emit "0")
            (let [sym  (gn-sym fc)
                  args (gn-next fc)]
              (cond
                (= sym SYM_IF)    (cc-if args)
                (= sym SYM_LET)   (cc-let args)
                (= sym SYM_DO)    (cc-do args)
                (= sym SYM_AND)   (cc-and args)
                (= sym SYM_OR)    (cc-or args)
                (= sym SYM_LOOP)  (cc-loop args)
                (= sym SYM_COND)  (cc-cond args)
                (= sym SYM_WHEN)  (cc-when args)
                ;; Arithmetic
                (or (= sym SYM_ADD) (= sym SYM_SUB) (= sym SYM_MUL)
                    (= sym SYM_DIV) (= sym SYM_MOD) (= sym SYM_EQ)
                    (= sym SYM_LT)  (= sym SYM_GT)
                    (= sym SYM_LTE) (= sym SYM_GTE))
                (cc-binop sym args)
                ;; Unary ops
                (= sym SYM_NOT)   (do (out-emit "(!(") (cc-expr args) (out-emit "))"))
                (= sym SYM_INC)   (do (out-emit "((") (cc-expr args) (out-emit ") + 1)"))
                (= sym SYM_DEC)   (do (out-emit "((") (cc-expr args) (out-emit ") - 1)"))
                (= sym SYM_ZEROQ) (do (out-emit "((") (cc-expr args) (out-emit ") == 0)"))
                (= sym SYM_POSQ)  (do (out-emit "((") (cc-expr args) (out-emit ") > 0)"))
                (= sym SYM_NEGQ)  (do (out-emit "((") (cc-expr args) (out-emit ") < 0)"))
                (= sym SYM_PRINTLN) (cc-println args)
                ;; Function call
                :else
                (do (cc-name sym) (out-emit "(")
                    (loop [a args first true]
                      (when (not (= a 0))
                        (when (not first) (out-emit ", "))
                        (cc-expr a)
                        (recur (gn-next a) false)))
                    (out-emit ")")))))))

      :else (out-emit "0"))))

;; ============================================================================
;; 3. Program emitter
;; ============================================================================

(defn cc-defn [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        pv (gn-next nm)
        name (gn-sym nm)]
    ;; Signature
    (out-emit "i64 ") (cc-name name) (out-emit "(")
    (loop [p (gn-child pv) first true]
      (when (not (= p 0))
        (when (not first) (out-emit ", "))
        (out-emit "i64 ") (cc-name (gn-sym p))
        (recur (gn-next p) false)))
    (out-emit ") {") (out-nl)
    ;; Body
    (out-emit "    return ")
    (let [body (gn-next pv)
          n-body (loop [b body c 0] (if (= b 0) c (recur (gn-next b) (inc c))))]
      (if (= n-body 1)
        (cc-expr body)
        (do (out-emit "({ ")
            (loop [b body]
              (when (not (= b 0))
                (cc-expr b) (out-emit "; ")
                (recur (gn-next b))))
            (out-emit "})"))))
    (out-emit ";") (out-nl)
    (out-emit "}") (out-nl) (out-nl)))

(defn cc-program []
  ;; Collect: defn, def, main
  (out-reset)
  (out-emit "/* Generated by clj — Clojure-to-C (Clojure emitter) */") (out-nl)
  (out-emit "#include <stdio.h>") (out-nl)
  (out-emit "#include <stdint.h>") (out-nl)
  (out-emit "typedef int64_t i64;") (out-nl) (out-nl)

  ;; First pass: forward declarations
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (when (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0)))
        (let [fc (gn-child ch)]
          (when (and (= (gn-kind fc) NK_IDENT) (= (gn-sym fc) SYM_DEFN))
            (let [nm (gn-next fc)
                  pv (gn-next nm)]
              (out-emit "i64 ") (cc-name (gn-sym nm)) (out-emit "(")
              (loop [p (gn-child pv) first true]
                (when (not (= p 0))
                  (when (not first) (out-emit ", "))
                  (out-emit "i64")
                  (recur (gn-next p) false)))
              (out-emit ");") (out-nl)))))
      (recur (gn-next ch))))

  ;; Second pass: global defs
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (when (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0)))
        (let [fc (gn-child ch)]
          (when (and (= (gn-kind fc) NK_IDENT) (= (gn-sym fc) SYM_DEF))
            (let [nm (gn-next fc)]
              (out-emit "static i64 ") (cc-name (gn-sym nm)) (out-emit ";") (out-nl)))))
      (recur (gn-next ch))))
  (out-nl)

  ;; Third pass: function definitions
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (when (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0)))
        (let [fc (gn-child ch)]
          (when (and (= (gn-kind fc) NK_IDENT) (= (gn-sym fc) SYM_DEFN))
            (cc-defn ch))))
      (recur (gn-next ch))))

  ;; Main
  (out-emit "int main(void) {") (out-nl)
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (if (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0)))
        (let [fc (gn-child ch)]
          (if (= (gn-kind fc) NK_IDENT)
            (let [sym (gn-sym fc)]
              (cond
                (= sym SYM_DEFN) nil  ;; already emitted
                (= sym SYM_DEF)
                (let [nm (gn-next fc)
                      val-n (gn-next nm)]
                  (out-emit "    ") (cc-name (gn-sym nm)) (out-emit " = ")
                  (cc-expr val-n) (out-emit ";") (out-nl))
                :else
                (do (out-emit "    ") (cc-expr ch) (out-emit ";") (out-nl))))
            (do (out-emit "    ") (cc-expr ch) (out-emit ";") (out-nl))))
        (do (out-emit "    ") (cc-expr ch) (out-emit ";") (out-nl)))
      (recur (gn-next ch))))
  (out-emit "    return 0;") (out-nl)
  (out-emit "}") (out-nl))
