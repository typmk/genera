;; lang/index.clj — Grammar indexer (port of gram_index from grammar.c)
;;
;; Sets structural bitmasks: m[kind], m_group, m_leaf, m_first.
;; C builtins: gn-kind, gn-child, gn-parent, gn-count,
;;             kind-set!, group-set!, leaf-set!, first-set!, index-clear!
;; C constants: NK_LIST, NK_MAP, NK_IDENT, NK_KW, NK_COUNT

(defn clj-gram-index []
  (index-clear!)
  (let [n (gn-count)]
    (loop [i 0]
      (when (< i n)
        (let [k (gn-kind i)]
          ;; Set m[kind] bitmask
          (kind-set! k i)
          ;; m_group: lists, vecs, maps (NK_LIST..NK_MAP)
          (when (and (>= k NK_LIST) (<= k NK_MAP))
            (group-set! i))
          ;; m_leaf: atoms (NK_IDENT..NK_KW)
          (when (and (>= k NK_IDENT) (<= k NK_KW))
            (leaf-set! i))
          ;; m_first: first child of parent
          (when (and (> i 0) (= i (gn-child (gn-parent i))))
            (first-set! i)))
        (recur (inc i))))))
