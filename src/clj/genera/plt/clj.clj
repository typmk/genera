;; plt/clj.clj — Clojure → Clojure emitter (pretty-printer / optimizer)
;;
;; Recursive descent over GNode world gram, emitting Clojure via out-emit.
;; Mirrors php.clj / cc.clj: walk the same tree, emit Clojure source.
;;
;; Unlike other emitters, this is identity on syntax EXCEPT:
;;   - Dead branches in if/when collapse to live branch
;;   - Dead top-level forms are skipped
;;   - Consistent formatting (indentation, spacing)
;;
;; C builtins: out-emit, out-nl, out-name, out-reset, out-str, gn-*, str-intern
;; C constants: NK_*, SYM_*, V_*

;; ============================================================================
;; 1. Helpers
;; ============================================================================

(defn clj-indent [depth]
  (loop [i 0]
    (when (< i depth)
      (out-emit "  ")
      (recur (inc i)))))

(defn clj-dead? [id]
  (and (> id 0)
       (view? V_DEAD id)))

;; ============================================================================
;; 2. Expression emitter
;; ============================================================================

(declare clj-expr)

(defn clj-children [id sep]
  (loop [c (gn-child id) first true]
    (when (not (= c 0))
      (when (not first) (out-emit sep))
      (clj-expr c)
      (recur (gn-next c) false))))

;; Handle (if test then else) with DCE: when a branch is dead, collapse
(defn clj-if [args]
  (let [test args
        then (if (= test 0) 0 (gn-next test))
        els  (if (= then 0) 0 (gn-next then))]
    (cond
      ;; then is dead → emit else directly (or nil if no else)
      (and (not (= then 0)) (clj-dead? then))
      (if (and (not (= els 0)) (not (clj-dead? els)))
        (clj-expr els)
        (out-emit "nil"))

      ;; else is dead → emit (if test then) or just then if test is const true
      (and (not (= els 0)) (clj-dead? els))
      (do (out-emit "(if ")
          (clj-expr test)
          (out-emit " ")
          (clj-expr then)
          (out-emit ")"))

      ;; neither dead → normal if
      :else
      (do (out-emit "(if ")
          (clj-expr test)
          (when (not (= then 0))
            (out-emit " ")
            (clj-expr then))
          (when (and (not (= els 0)) (not (clj-dead? els)))
            (out-emit " ")
            (clj-expr els))
          (out-emit ")")))))

(defn clj-expr [id]
  (let [k (gn-kind id)]
    (cond
      ;; Leaf nodes — emit text directly
      (= k NK_NUM)
      (out-emit (gn-text id))

      (= k NK_STR)
      (out-emit (gn-text id))

      (= k NK_KW)
      (out-emit (gn-text id))

      (= k NK_IDENT)
      (out-emit (gn-text id))

      (= k NK_OP)
      (out-emit (gn-text id))

      ;; Compound — list (check for if special form)
      (= k NK_LIST)
      (let [fc (gn-child id)]
        (if (and (not (= fc 0))
                 (= (gn-kind fc) NK_IDENT)
                 (= (gn-sym fc) SYM_IF))
          (clj-if (gn-next fc))
          (do (out-emit "(")
              (clj-children id " ")
              (out-emit ")"))))

      ;; Compound — vector
      (= k NK_VEC)
      (do (out-emit "[")
          (clj-children id " ")
          (out-emit "]"))

      ;; Compound — map
      (= k NK_MAP)
      (do (out-emit "{")
          (clj-children id " ")
          (out-emit "}"))

      ;; Quote reader macros
      (= k NK_QUOTE)
      (do (out-emit "'")
          (let [c (gn-child id)]
            (when (not (= c 0)) (clj-expr c))))

      (= k NK_SYNTAX_QUOTE)
      (do (out-emit "`")
          (let [c (gn-child id)]
            (when (not (= c 0)) (clj-expr c))))

      (= k NK_UNQUOTE)
      (do (out-emit "~")
          (let [c (gn-child id)]
            (when (not (= c 0)) (clj-expr c))))

      (= k NK_SPLICE)
      (do (out-emit "~@")
          (let [c (gn-child id)]
            (when (not (= c 0)) (clj-expr c))))

      ;; Fallback
      :else (out-emit (gn-text id)))))

;; ============================================================================
;; 3. Top-level form emitters (indented for readability)
;; ============================================================================

(defn clj-defn [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        pv (gn-next nm)]
    (out-emit "(defn ")
    (out-emit (gn-text nm))
    (out-emit " [")
    (loop [p (gn-child pv) first true]
      (when (not (= p 0))
        (when (not first) (out-emit " "))
        (out-emit (gn-text p))
        (recur (gn-next p) false)))
    (out-emit "]")
    ;; body
    (loop [b (gn-next pv)]
      (when (not (= b 0))
        (when (not (clj-dead? b))
          (out-nl)
          (clj-indent 1)
          (clj-expr b))
        (recur (gn-next b))))
    (out-emit ")")
    (out-nl)
    (out-nl)))

(defn clj-def [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        val-n (gn-next nm)]
    (out-emit "(def ")
    (out-emit (gn-text nm))
    (when (not (= val-n 0))
      (out-emit " ")
      (clj-expr val-n))
    (out-emit ")")
    (out-nl)))

(defn clj-defmacro [id]
  (let [fc (gn-child id)
        nm (gn-next fc)
        pv (gn-next nm)]
    (out-emit "(defmacro ")
    (out-emit (gn-text nm))
    (out-emit " [")
    (loop [p (gn-child pv) first true]
      (when (not (= p 0))
        (when (not first) (out-emit " "))
        (out-emit (gn-text p))
        (recur (gn-next p) false)))
    (out-emit "]")
    (loop [b (gn-next pv)]
      (when (not (= b 0))
        (out-nl)
        (clj-indent 1)
        (clj-expr b)
        (recur (gn-next b))))
    (out-emit ")")
    (out-nl)
    (out-nl)))

;; ============================================================================
;; 4. Program emitter — entry point called from CLI
;; ============================================================================

(defn clj-program []
  (out-reset)
  (out-emit ";; Generated by genera") (out-nl)
  (out-nl)
  ;; Walk top-level forms
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (when (not (clj-dead? ch))
        (if (and (= (gn-kind ch) NK_LIST) (not (= (gn-child ch) 0))
                 (= (gn-kind (gn-child ch)) NK_IDENT))
          (let [sym (gn-sym (gn-child ch))]
            (cond
              (= sym SYM_DEFN) (clj-defn ch)
              (= sym SYM_DEFMACRO) (clj-defmacro ch)
              (= sym SYM_DEF) (do (clj-def ch) (out-nl))
              :else (do (clj-expr ch) (out-nl))))
          (do (clj-expr ch) (out-nl))))
      (recur (gn-next ch)))))
