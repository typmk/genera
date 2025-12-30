(ns cljp.mir
  "Mid-level Intermediate Representation for PHP backend.

   MIR nodes are PHP-specific but still abstract (not text).
   HIR (:host-call, :host-new, :host-const) gets lowered to MIR
   before emission to PHP text.

   MIR node types:
   - :php-call    - PHP function call: strlen($s)
   - :method      - Instance method: $obj->method($x)
   - :static-call - Static method: Class::method($x)
   - :php-infix   - PHP operator: $a + $b
   - :new         - Object instantiation: new Class($x)
   - :php-const   - PHP constant: PHP_EOL

   Other HIR nodes pass through unchanged (they're already
   platform-agnostic: :if, :let, :fn, :invoke, etc.)")

;; ============================================================
;; MIR Node Constructors
;; ============================================================

(defn php-call-node
  "PHP function call: (php/strlen s) → strlen($s)"
  [env form fn-name args]
  {:op :php-call
   :form form
   :env env
   :fn-name fn-name
   :args args
   :children [:args]})

(defn method-node
  "Instance method call: (.method obj x) → $obj->method($x)"
  [env form method target args]
  {:op :method
   :form form
   :env env
   :method method
   :target target
   :args args
   :children [:target :args]})

(defn static-call-node
  "Static method call: (Class/method x) → Class::method($x)"
  [env form class method args]
  {:op :static-call
   :form form
   :env env
   :class class
   :method method
   :args args
   :children [:args]})

(defn php-infix-node
  "PHP infix operator: (php/+ a b) → $a + $b"
  [env form operator clj-operator args]
  {:op :php-infix
   :form form
   :env env
   :operator operator
   :clj-operator clj-operator
   :args args
   :children [:args]})

(defn new-node
  "Object instantiation: (new Class x) → new Class($x)"
  [env form class args]
  {:op :new
   :form form
   :env env
   :class class
   :args args
   :children [:args]})

(defn php-const-node
  "PHP constant: php/PHP_EOL → PHP_EOL"
  [env form const-name]
  {:op :php-const
   :form form
   :env env
   :const-name const-name
   :children []})
