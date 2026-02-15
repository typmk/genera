;; 04-grammar.clj — Grammar views
;;
;; Everything is a grammar: pattern → output.
;; Same GNodes, different grammars, different surfaces.
;;
;;   (gram-render 0)    → Lisp text     (GNodes through lisp grammar)
;;   (gram-render 1)    → C text        (GNodes through C grammar)
;;   (gram-outline)     → indented tree (structural view)
;;   (gram-bc id)       → bytecode text (instruction view)
;;
;; Bytecode is not a separate layer. It's a PRINT GRAMMAR:
;; walk GNodes + views, emit flat instruction mnemonics.
;; The views ARE the analysis. The grammar IS the output.

;; ================================================================
;; Instruction names — for the bytecode print grammar
;; ================================================================

(def op-name
  {SYM_ADD "ADD" SYM_SUB "SUB" SYM_MUL "MUL" SYM_DIV "DIV" SYM_MOD "MOD"
   SYM_EQ "EQ" SYM_LT "LT" SYM_GT "GT" SYM_LTE "LE" SYM_GTE "GE"
   SYM_NOT "NOT" SYM_INC "INC" SYM_DEC "DEC"
   SYM_ZEROQ "ZERO?" SYM_POSQ "POS?" SYM_NEGQ "NEG?"})

;; ================================================================
;; gram-bc: bytecode print grammar
;;
;; Walks syntax GNodes + views, prints as flat instructions.
;; No new nodes. No intermediate representation.
;; Just another way to SEE the same tree.
;; ================================================================

(defn bc-indent [depth]
  (loop [i 0] (when (< i depth) (print "  ") (recur (inc i)))))

(defn gram-bc [id]
  (gram-bc-d id 0))

(defn gram-bc-d [id depth]
  (let [k (gn-kind id)]
    (cond
      ;; Number → CONST
      (= k NK_NUM)
        (do (bc-indent depth)
            (println "CONST" (gn-parse-int id)))

      ;; Identifier → LOAD (or literal for true/false/nil)
      (= k NK_IDENT)
        (let [s (gn-sym id)]
          (bc-indent depth)
          (cond
            (= s SYM_TRUE)  (println "CONST true")
            (= s SYM_FALSE) (println "CONST false")
            (= s SYM_NIL)   (println "CONST nil")
            1 (println "LOAD" (sym-name s))))

      ;; String → CONST
      (= k NK_STR)
        (do (bc-indent depth)
            (println "CONST" (gn-text id)))

      ;; List → dispatch on head
      (= k NK_LIST)
        (when (not (= (gn-child id) 0))
          (let [fc (gn-child id)]
            (if (not (= (gn-kind fc) NK_IDENT))
              (do (bc-indent depth) (println "EXPR <?>" ))
              (let [sym (gn-sym fc)
                    args (gn-next fc)
                    name (get op-name sym)]
                (cond
                  ;; Arithmetic / compare → emit args, then op
                  (not (nil? name))
                    (do (loop [a args]
                          (when (not (= a 0))
                            (gram-bc-d a depth)
                            (recur (gn-next a))))
                        (bc-indent depth)
                        (println name))

                  ;; if → test, JZ, then, JMP, else
                  (= sym SYM_IF)
                    (let [test args
                          then (gn-next test)
                          els  (if then (gn-next then) 0)]
                      (gram-bc-d test depth)
                      (bc-indent depth) (println "JZ else")
                      (when then (gram-bc-d then (inc depth)))
                      (bc-indent depth) (println "JMP end")
                      (bc-indent depth) (println "else:")
                      (when (not (= els 0)) (gram-bc-d els (inc depth)))
                      (bc-indent depth) (println "end:"))

                  ;; let → bindings as STORE, then body
                  (= sym SYM_LET)
                    (let [bv args]
                      (loop [p (gn-child bv)]
                        (when (not (= p 0))
                          (let [vn (gn-next p)]
                            (when (not (= vn 0))
                              (gram-bc-d vn depth)
                              (bc-indent depth)
                              (println "STORE" (sym-name (gn-sym p)))
                              (recur (gn-next vn))))))
                      (loop [body (gn-next bv)]
                        (when (not (= body 0))
                          (gram-bc-d body depth)
                          (recur (gn-next body)))))

                  ;; do → sequence
                  (= sym SYM_DO)
                    (loop [b args]
                      (when (not (= b 0))
                        (gram-bc-d b depth)
                        (recur (gn-next b))))

                  ;; loop → LABEL + bindings + body
                  (= sym SYM_LOOP)
                    (let [bv args]
                      (loop [p (gn-child bv)]
                        (when (not (= p 0))
                          (let [vn (gn-next p)]
                            (when (not (= vn 0))
                              (gram-bc-d vn depth)
                              (bc-indent depth)
                              (println "STORE" (sym-name (gn-sym p)))
                              (recur (gn-next vn))))))
                      (bc-indent depth) (println "LOOP:")
                      (loop [body (gn-next bv)]
                        (when (not (= body 0))
                          (gram-bc-d body (inc depth))
                          (recur (gn-next body)))))

                  ;; recur → emit args, RECUR
                  (= sym SYM_RECUR)
                    (do (loop [a args]
                          (when (not (= a 0))
                            (gram-bc-d a depth)
                            (recur (gn-next a))))
                        (bc-indent depth)
                        (println "RECUR"))

                  ;; defn → FUNC header
                  (= sym SYM_DEFN)
                    (let [nm (gn-next fc)
                          pv (gn-next nm)]
                      (bc-indent depth)
                      (print "FUNC" (sym-name (gn-sym nm)) "(")
                      (loop [p (gn-child pv) first true]
                        (when (not (= p 0))
                          (when (not first) (print " "))
                          (print (sym-name (gn-sym p)))
                          (recur (gn-next p) false)))
                      (println ")")
                      (loop [body (gn-next pv)]
                        (when (not (= body 0))
                          (gram-bc-d body (inc depth))
                          (recur (gn-next body))))
                      (bc-indent depth)
                      (println "RET"))

                  ;; def → emit value, GSTORE
                  (= sym SYM_DEF)
                    (let [nm (gn-next fc)
                          val (gn-next nm)]
                      (when (not (= val 0))
                        (gram-bc-d val depth))
                      (bc-indent depth)
                      (println "GSTORE" (sym-name (gn-sym nm))))

                  ;; when → test, JZ, body
                  (= sym SYM_WHEN)
                    (do (gram-bc-d args depth)
                        (bc-indent depth) (println "JZ end")
                        (loop [a (gn-next args)]
                          (when (not (= a 0))
                            (gram-bc-d a (inc depth))
                            (recur (gn-next a))))
                        (bc-indent depth) (println "end:"))

                  ;; cond → chain of test/JZ pairs
                  (= sym SYM_COND)
                    (loop [a args]
                      (when (not (= a 0))
                        (let [expr (gn-next a)]
                          (when (not (= expr 0))
                            (if (= (gn-kind a) NK_KW)
                              (do (bc-indent depth) (println "else:")
                                  (gram-bc-d expr depth))
                              (do (gram-bc-d a depth)
                                  (bc-indent depth) (println "JZ next")
                                  (gram-bc-d expr (inc depth))
                                  (bc-indent depth) (println "JMP end")
                                  (bc-indent depth) (println "next:")))
                            (recur (gn-next expr))))))

                  ;; Function call → emit args, CALL
                  1 (let [tail (view? V_TAIL id)]
                      (loop [a args]
                        (when (not (= a 0))
                          (gram-bc-d a depth)
                          (recur (gn-next a))))
                      (bc-indent depth)
                      (println (if tail "TCALL" "CALL")
                               (sym-name sym))))))))

      ;; Vec/Map → emit elements
      (= k NK_VEC)
        (do (loop [c (gn-child id)]
              (when (not (= c 0))
                (gram-bc-d c depth)
                (recur (gn-next c))))
            (bc-indent depth)
            (println "VEC" (gn-count id)))

      (= k NK_MAP)
        (do (loop [c (gn-child id)]
              (when (not (= c 0))
                (gram-bc-d c depth)
                (recur (gn-next c))))
            (bc-indent depth)
            (println "MAP" (gn-count id))))))

;; gram-bc-all: bytecode view of entire program
(defn gram-bc-all []
  (loop [ch (gn-child 0)]
    (when (not (= ch 0))
      (gram-bc ch)
      (recur (gn-next ch)))))
