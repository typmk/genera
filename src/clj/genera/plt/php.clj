;; plt/php.clj — Clojure → PHP emitter
;;
;; Recursive descent over GNode world gram, emitting PHP via out-emit.
;; Mirrors cc.c's pattern: walk the same tree, different leaf action.
;;
;; Platform mappings (platform_php.clj) resolve function names:
;;   moss\coll functions: assoc, get, send, atom, pmap, etc.
;;   PHP natives: is_null, strtoupper, json_encode, etc.
;;
;; C builtins used:
;;   out-emit  — write string to output buffer
;;   out-nl    — write newline
;;   out-name  — write PHP-mangled name from StrId
;;   out-reset — clear output buffer
;;   out-str   — return buffer as string
;;   gn-*      — GNode accessors (read g_world.gram)
;;   str-intern — intern string → StrId for platform lookup

;; ============================================================================
;; 1. Helpers
;; ============================================================================

(defn php-var [sym-id]
  (out-emit "$")
  (out-name sym-id))

;; ============================================================================
;; 2. Expression emitter
;; ============================================================================

(defn php-binop [op-str arg]
  (out-emit "(")
  (php-expr arg)
  (loop [a (gn-next arg)]
    (when (not (= a 0))
      (out-emit " ")
      (out-emit op-str)
      (out-emit " ")
      (php-expr a)
      (recur (gn-next a))))
  (out-emit ")"))

(defn php-if [args]
  (let [test args
        then (gn-next test)
        els  (if (= then 0) 0 (gn-next then))]
    (out-emit "(")
    (php-expr test)
    (out-emit " ? ")
    (if (= then 0) (out-emit "null") (php-expr then))
    (out-emit " : ")
    (if (= els 0) (out-emit "null") (php-expr els))
    (out-emit ")")))

(defn php-let [bv]
  (out-emit "(function() {")
  (out-nl)
  (loop [p (gn-child bv)]
    (when (not (= p 0))
      (let [vn (gn-next p)]
        (when (not (= vn 0))
          (out-emit "    ")
          (php-var (gn-sym p))
          (out-emit " = ")
          (php-expr vn)
          (out-emit ";")
          (out-nl)
          (recur (gn-next vn))))))
  ;; body — last expr returned
  (let [body (gn-next bv)]
    (loop [b body]
      (when (not (= b 0))
        (if (= (gn-next b) 0)
          (do (out-emit "    return ")
              (php-expr b)
              (out-emit ";")
              (out-nl))
          (do (out-emit "    ")
              (php-expr b)
              (out-emit ";")
              (out-nl)
              (recur (gn-next b)))))))
  (out-emit "})()")
  nil)

(defn php-do [body]
  (out-emit "(function() {")
  (out-nl)
  (loop [b body]
    (when (not (= b 0))
      (if (= (gn-next b) 0)
        (do (out-emit "    return ")
            (php-expr b)
            (out-emit ";")
            (out-nl))
        (do (out-emit "    ")
            (php-expr b)
            (out-emit ";")
            (out-nl)
            (recur (gn-next b))))))
  (out-emit "})()")
  nil)

(defn php-and [arg]
  (if (= arg 0)
    (out-emit "true")
    (do (out-emit "(")
        (php-expr arg)
        (loop [a (gn-next arg)]
          (when (not (= a 0))
            (out-emit " && ")
            (php-expr a)
            (recur (gn-next a))))
        (out-emit ")"))))

(defn php-or [arg]
  (if (= arg 0)
    (out-emit "null")
    (do (out-emit "(")
        (php-expr arg)
        (loop [a (gn-next arg)]
          (when (not (= a 0))
            (out-emit " || ")
            (php-expr a)
            (recur (gn-next a))))
        (out-emit ")"))))

(defn php-cond [args]
  (out-emit "match(true) {")
  (out-nl)
  (loop [a args]
    (when (and (not (= a 0)) (not (= (gn-next a) 0)))
      (let [expr (gn-next a)]
        (if (= (gn-kind a) NK_KW)
          (do (out-emit "    default => ")
              (php-expr expr))
          (do (out-emit "    ")
              (php-expr a)
              (out-emit " => ")
              (php-expr expr)))
        (out-emit ",")
        (out-nl)
        (recur (gn-next expr)))))
  (out-emit "}")
  nil)

(defn php-when [args]
  (out-emit "(")
  (php-expr args)
  (out-emit " ? ")
  (let [body (gn-next args)]
    (if (= (gn-next body) 0)
      (php-expr body)
      (php-do body)))
  (out-emit " : null)"))

(defn php-loop [bv]
  (out-emit "(function() {")
  (out-nl)
  ;; Init bindings
  (loop [p (gn-child bv)]
    (when (not (= p 0))
      (let [vn (gn-next p)]
        (when (not (= vn 0))
          (out-emit "    ")
          (php-var (gn-sym p))
          (out-emit " = ")
          (php-expr vn)
          (out-emit ";")
          (out-nl)
          (recur (gn-next vn))))))
  (out-emit "    while (true) {")
  (out-nl)
  ;; body
  (let [body (gn-next bv)]
    (loop [b body]
      (when (not (= b 0))
        (if (= (gn-next b) 0)
          (do (out-emit "        return ")
              (php-expr b)
              (out-emit ";")
              (out-nl))
          (do (out-emit "        ")
              (php-expr b)
              (out-emit ";")
              (out-nl)
              (recur (gn-next b)))))))
  (out-emit "    }")
  (out-nl)
  (out-emit "})()")
  nil)

(defn php-recur [args]
  ;; Emit list() = [...] + continue
  ;; (works in PHP: list($x, $y) = [expr1, expr2]; continue;)
  (out-emit "list(")
  ;; TODO: loop var names not accessible here — needs context
  (out-emit ") = [")
  (loop [a args first true]
    (when (not (= a 0))
      (when (not first) (out-emit ", "))
      (php-expr a)
      (recur (gn-next a) false)))
  (out-emit "]; continue"))

(defn php-println [arg]
  (out-emit "echo ")
  (if (= arg 0)
    (out-emit "PHP_EOL")
    (do (loop [a arg first true]
          (when (not (= a 0))
            (when (not first) (out-emit " . ' ' . "))
            (php-expr a)
            (recur (gn-next a) false)))
        (out-emit " . PHP_EOL"))))

(defn php-fn-literal [args]
  (out-emit "fn(")
  (let [pv args]
    (loop [p (gn-child pv) first true]
      (when (not (= p 0))
        (when (not first) (out-emit ", "))
        (php-var (gn-sym p))
        (recur (gn-next p) false)))
    (out-emit ") => ")
    (let [body (gn-next pv)]
      (if (= (gn-next body) 0)
        (php-expr body)
        (php-do body)))))

;; ============================================================================
;; 3. Main dispatcher
;; ============================================================================

(defn php-expr [id]
  (let [kind (gn-kind id)]
    (cond
      (= kind NK_NUM)
        (out-emit (gn-text id))

      (= kind NK_STR)
        (out-emit (gn-text id))

      (= kind NK_KW)
        (let [text (gn-text id)]
          ;; :keyword → 'keyword' (text includes the colon)
          (out-emit "'")
          (out-emit text)
          (out-emit "'"))

      (= kind NK_IDENT)
        (let [sym (gn-sym id)]
          (cond
            (= sym SYM_TRUE) (out-emit "true")
            (= sym SYM_FALSE) (out-emit "false")
            (= sym SYM_NIL) (out-emit "null")
            :else (php-var sym)))

      (= kind NK_VEC)
        (do (out-emit "[")
            (loop [c (gn-child id) first true]
              (when (not (= c 0))
                (when (not first) (out-emit ", "))
                (php-expr c)
                (recur (gn-next c) false)))
            (out-emit "]"))

      (= kind NK_MAP)
        (do (out-emit "[")
            (loop [c (gn-child id) first true]
              (when (not (= c 0))
                (let [vn (gn-next c)]
                  (when (not (= vn 0))
                    (when (not first) (out-emit ", "))
                    (php-expr c)
                    (out-emit " => ")
                    (php-expr vn)
                    (recur (gn-next vn) false)))))
            (out-emit "]"))

      ;; List: special forms + function calls
      (= kind NK_LIST)
        (let [fc (gn-child id)]
          (if (= fc 0)
            (out-emit "null")
            (if (= (gn-kind fc) NK_IDENT)
              (let [sym (gn-sym fc)
                    args (gn-next fc)]
                (cond
                  (= sym SYM_IF) (php-if args)
                  (= sym SYM_LET) (php-let args)
                  (= sym SYM_DO) (php-do args)
                  (= sym SYM_AND) (php-and args)
                  (= sym SYM_OR) (php-or args)
                  (= sym SYM_COND) (php-cond args)
                  (= sym SYM_WHEN) (php-when args)
                  (= sym SYM_LOOP) (php-loop args)
                  (= sym SYM_RECUR) (php-recur args)
                  (= sym SYM_FN) (php-fn-literal args)
                  (= sym SYM_PRINTLN) (php-println args)

                  ;; Arithmetic / comparison → infix
                  (= sym SYM_ADD) (php-binop "+" args)
                  (= sym SYM_SUB) (php-binop "-" args)
                  (= sym SYM_MUL) (php-binop "*" args)
                  (= sym SYM_DIV) (php-binop "/" args)
                  (= sym SYM_MOD) (php-binop "%" args)
                  (= sym SYM_EQ) (php-binop "===" args)
                  (= sym SYM_LT) (php-binop "<" args)
                  (= sym SYM_GT) (php-binop ">" args)
                  (= sym SYM_LTE) (php-binop "<=" args)
                  (= sym SYM_GTE) (php-binop ">=" args)

                  ;; Unary
                  (= sym SYM_NOT)
                    (do (out-emit "(!(") (php-expr args) (out-emit "))"))
                  (= sym SYM_INC)
                    (do (out-emit "((") (php-expr args) (out-emit ") + 1)"))
                  (= sym SYM_DEC)
                    (do (out-emit "((") (php-expr args) (out-emit ") - 1)"))
                  (= sym SYM_ZEROQ)
                    (do (out-emit "((") (php-expr args) (out-emit ") === 0)"))
                  (= sym SYM_POSQ)
                    (do (out-emit "((") (php-expr args) (out-emit ") > 0)"))
                  (= sym SYM_NEGQ)
                    (do (out-emit "((") (php-expr args) (out-emit ") < 0)"))

                  ;; Default: function call (check platform mappings first)
                  :else
                    (let [plt (platform-lookup sym)]
                      (if plt
                        (out-emit (first (rest plt)))
                        (out-name sym))
                      (out-emit "(")
                      (loop [a args first true]
                        (when (not (= a 0))
                          (when (not first) (out-emit ", "))
                          (php-expr a)
                          (recur (gn-next a) false)))
                      (out-emit ")"))))
              ;; Head is not an ident — eval and call
              (do (out-emit "(")
                  (php-expr fc)
                  (out-emit ")(")
                  (loop [a (gn-next fc) first true]
                    (when (not (= a 0))
                      (when (not first) (out-emit ", "))
                      (php-expr a)
                      (recur (gn-next a) false)))
                  (out-emit ")")))))

      :else (out-emit "null /* unknown */"))))

;; ============================================================================
;; 4. Top-level form emitters
;; ============================================================================

(defn php-defn [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        pv (gn-next nm)
        name (gn-sym nm)]
    (out-emit "function ")
    (out-name name)
    (out-emit "(")
    (loop [p (gn-child pv) first true]
      (when (not (= p 0))
        (when (not first) (out-emit ", "))
        (php-var (gn-sym p))
        (recur (gn-next p) false)))
    (out-emit ") {")
    (out-nl)
    ;; body — last expr is returned
    (let [body (gn-next pv)]
      (loop [b body]
        (when (not (= b 0))
          (if (= (gn-next b) 0)
            (do (out-emit "    return ")
                (php-expr b)
                (out-emit ";")
                (out-nl))
            (do (out-emit "    ")
                (php-expr b)
                (out-emit ";")
                (out-nl)
                (recur (gn-next b)))))))
    (out-emit "}")
    (out-nl)
    (out-nl)))

(defn php-def [nm-node val-node]
  (let [name (gn-sym nm-node)]
    (php-var name)
    (out-emit " = ")
    (php-expr val-node)
    (out-emit ";")
    (out-nl)))

;; ============================================================================
;; 5. Program emitter — entry point called from CLI
;; ============================================================================

(defn emit-php-program []
  (out-reset)
  (out-emit "<?php")
  (out-nl)
  (out-emit "// Generated by genera")
  (out-nl)
  (out-nl)
  (emit-php-imports)
  ;; Classify top-level forms (same pattern as cg-compile-program)
  (let [forms (loop [ch (gn-child 0) defns [] defs [] mains []]
               (if (= ch 0)
                 (list defns defs mains)
                 (if (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0))
                          (= (gn-kind (gn-child ch)) NK_IDENT))
                   (let [sym (gn-sym (gn-child ch))]
                     (cond
                       (= sym SYM_DEFN)
                         (recur (gn-next ch) (conj defns ch) defs mains)
                       (= sym SYM_DEF)
                         (let [nm (gn-next (gn-child ch))]
                           (recur (gn-next ch) defns
                             (conj defs (list nm (gn-next nm)))
                             mains))
                       :else (recur (gn-next ch) defns defs
                                (conj mains ch))))
                   (recur (gn-next ch) defns defs
                     (conj mains ch)))))
        defns (first forms)
        defs  (first (rest forms))
        mains (first (rest (rest forms)))]
    ;; Emit function definitions
    (loop [i 0]
      (when (< i (count defns))
        (php-defn (nth defns i))
        (recur (inc i))))
    ;; Emit global defs
    (loop [i 0]
      (when (< i (count defs))
        (let [d (nth defs i)]
          (php-def (first d) (first (rest d))))
        (recur (inc i))))
    ;; Emit main expressions
    (when (> (count mains) 0) (out-nl))
    (loop [i 0]
      (when (< i (count mains))
        (php-expr (nth mains i))
        (out-emit ";")
        (out-nl)
        (recur (inc i))))))
