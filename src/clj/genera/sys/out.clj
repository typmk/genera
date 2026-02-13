;; sys/out.clj — Output buffer helpers
;;
;; C builtins: out-emit, out-reset, out-str
;; These add convenience on top.

(defn out-line [s]
  (out-emit s)
  (out-emit "\n"))

(defn out-indent [n]
  (loop [i 0]
    (when (< i n)
      (out-emit "    ")
      (recur (inc i)))))
