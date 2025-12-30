(ns cljp.lower-php
  "Lowers HIR to PHP-specific MIR.

   This pass converts platform-agnostic HIR nodes to PHP-specific
   MIR nodes that emit.cljc knows how to emit.

   Conversions:
   - :host-call :function → :php-call
   - :host-call :method   → :method
   - :host-call :static   → :static-call
   - :host-call :operator → :php-infix
   - :host-new            → :new
   - :host-const          → :php-const

   All other nodes pass through with their children lowered."
  (:require [cljp.mir :as mir]))

;; ============================================================
;; Tree Walking
;; ============================================================

(declare lower)

(defn- lower-children
  "Lower all child nodes specified in :children."
  [node]
  (if-let [child-keys (:children node)]
    (reduce (fn [n child-key]
              (let [child (get n child-key)]
                (cond
                  (nil? child) n
                  (vector? child) (assoc n child-key (mapv lower child))
                  (map? child) (assoc n child-key (lower child))
                  :else n)))
            node
            child-keys)
    node))

;; ============================================================
;; Lowering Rules
;; ============================================================

(defmulti lower-node :op)

(defmethod lower-node :default [node]
  ;; Pass through, but lower children
  (lower-children node))

(defmethod lower-node :host-call
  [{:keys [call-type host-ns fn-name target class args env form] :as node}]
  (let [lowered-args (mapv lower args)
        lowered-target (when target (lower target))]
    (case call-type
      :function
      (mir/php-call-node env form fn-name lowered-args)

      :method
      (mir/method-node env form fn-name lowered-target lowered-args)

      :static
      (mir/static-call-node env form class fn-name lowered-args)

      :operator
      (if host-ns
        ;; Explicit PHP operator: (php/+ a b) → php-infix
        (mir/php-infix-node env form fn-name lowered-args)
        ;; Clojure operator: (+ a b) → infix (handles unary minus)
        (mir/infix-node env form fn-name fn-name lowered-args)))))

(defmethod lower-node :host-new
  [{:keys [class args env form]}]
  (let [lowered-args (mapv lower args)]
    (mir/new-node env form class lowered-args)))

(defmethod lower-node :host-const
  [{:keys [const-name env form]}]
  (mir/php-const-node env form const-name))

;; ============================================================
;; Public API
;; ============================================================

(defn lower
  "Lower a HIR node tree to PHP MIR.
   Recursively walks the tree, converting :host-* nodes to MIR."
  [node]
  (if (map? node)
    (lower-node node)
    node))

(defn lower-all
  "Lower a sequence of HIR nodes."
  [nodes]
  (mapv lower nodes))
