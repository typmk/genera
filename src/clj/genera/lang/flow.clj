;; 03-flow.clj — Pass 3: Flow + Dead Code
;;
;; Marks V_TAIL (tail position) and V_DEAD (unreachable branches).
;; p3-tail propagates recursively through if/do/let/loop/cond/when.
;; Dead code: constant if/when tests with known value.

(defn p3-tail [id]
  (view-set! V_TAIL id)
  (let [k (gn-kind id)]
    (when (and (= k NK_LIST) (gn-child id)
              (= (gn-kind (gn-child id)) NK_IDENT))
      (let [fc (gn-child id)
            sym (gn-sym fc)]
        (when (= sym SYM_IF)
          (let [test (gn-next fc)]
            (when (not (= test 0))
              (let [then (gn-next test)]
                (when (not (= then 0))
                  (p3-tail then)
                  (let [els (gn-next then)]
                    (when (not (= els 0)) (p3-tail els)))
                  (when (= (gn-kind test) NK_IDENT)
                    (let [ts (gn-sym test)]
                      (when (or (= ts SYM_FALSE) (= ts SYM_NIL))
                        (view-set! V_DEAD then))
                      (when (= ts SYM_TRUE)
                        (let [els (gn-next then)]
                          (when (not (= els 0))
                            (view-set! V_DEAD els)))))))))))
        (when (= sym SYM_DO)
          (loop [c (gn-next fc) last 0]
            (if (= c 0)
              (when (not (= last 0)) (p3-tail last))
              (recur (gn-next c) c))))
        (when (or (= sym SYM_LET) (= sym SYM_LOOP))
          (let [bv (gn-next fc)]
            (when (not (= bv 0))
              (loop [c (gn-next bv) last 0]
                (if (= c 0)
                  (when (not (= last 0)) (p3-tail last))
                  (recur (gn-next c) c))))))
        (when (= sym SYM_COND)
          (loop [c (gn-next fc) is-test true]
            (when (not (= c 0))
              (when (not is-test) (p3-tail c))
              (recur (gn-next c) (not is-test)))))
        (when (or (= sym SYM_WHEN)
                  (= sym SYM_AND) (= sym SYM_OR))
          (loop [c (gn-next fc) last 0]
            (if (= c 0)
              (when (not (= last 0)) (p3-tail last))
              (recur (gn-next c) c))))))))

(defn clj-pass-flow []
  ;; Part 1: Scan for defn/fn/loop → p3-tail on last body form
  (loop [i 0]
    (when (< i (gn-count))
      (let [k (gn-kind i)]
        (when (and (= k NK_LIST) (gn-child i)
                   (= (gn-kind (gn-child i)) NK_IDENT))
          (let [fc (gn-child i)
                sym (gn-sym fc)
                body-start
                  (cond
                    (= sym SYM_DEFN)
                      (let [nm (gn-next fc)]
                        (when (not (= nm 0))
                          (let [pv (gn-next nm)]
                            (when (not (= pv 0)) (gn-next pv)))))
                    (= sym SYM_FN)
                      (let [pv (gn-next fc)]
                        (when (not (= pv 0)) (gn-next pv)))
                    (= sym SYM_LOOP)
                      (let [bv (gn-next fc)]
                        (when (not (= bv 0)) (gn-next bv)))
                    1 nil)]
            (when body-start
              (loop [e body-start last 0]
                (if (= e 0)
                  (when (not (= last 0)) (p3-tail last))
                  (recur (gn-next e) e)))))))
      (recur (inc i))))
  ;; Part 2: Dead code — constant if/when tests
  (loop [i 0]
    (when (< i (gn-count))
      (let [k (gn-kind i)]
        (when (and (= k NK_LIST) (gn-child i)
                   (= (gn-kind (gn-child i)) NK_IDENT))
          (let [fc (gn-child i)
                sym (gn-sym fc)]
            (when (= sym SYM_IF)
              (let [test (gn-next fc)]
                (when (not (= test 0))
                  (let [then (gn-next test)]
                    (when (not (= then 0))
                      (when (and (view? V_CONST test) (view? V_INT test))
                        (let [tv (val-get test)]
                          (if (= tv 0)
                            (view-set! V_DEAD then)
                            (let [els (gn-next then)]
                              (when (not (= els 0))
                                (view-set! V_DEAD els)))))))))))
            (when (= sym SYM_WHEN)
              (let [test (gn-next fc)]
                (when (and (not (= test 0))
                           (view? V_CONST test)
                           (view? V_INT test)
                           (= (val-get test) 0))
                  (loop [b (gn-next test)]
                    (when (not (= b 0))
                      (view-set! V_DEAD b)
                      (recur (gn-next b))))))))))
      (recur (inc i)))))