(ns cljp.emit
  "PHP emitter with performance optimizations.

   Key optimizations:
   1. StringBuilder for O(n) string building (vs O(n²) atom concat)
   2. Memoized symbol munging
   3. Case dispatch for hot path (vs multimethod overhead)
   4. Inlined locus/lift modules

   Dispatches on :op keyword. Each node type gets an emit function.

   HIR Node Types Handled:
   - Core: :const :local :var :if :do :let :fn :invoke :def :loop :recur :letfn
   - Error: :throw :try
   - Data: :quote :vector :map :set
   - Host: :host-call :host-new :host-field :host-const :case"
  (:require [cljp.hir :as hir]
            [cljp.infer :as infer]
            [clojure.string :as str]
            [clojure.set :as set]))

;; ============================================================
;; Locus Abstraction (inlined from locus.cljc)
;; ============================================================
;; A locus describes WHERE a value should be placed in PHP code.
;; Instead of context flags scattered through emit, locus provides
;; a unified way to handle statement/expression/return positions.

(def statement-locus
  "Value is discarded, emit as statement."
  {:type :statement :pre "" :post ""})

(def expr-locus
  "Value is used inline, no wrapping."
  {:type :expr :pre "" :post ""})

(def return-locus
  "Value is returned from function."
  {:type :return :pre "return " :post ";\n"})

(defn assign-locus
  "Value is assigned to a variable."
  [var-name]
  {:type :assign :var var-name :pre (str var-name " = ") :post ";\n"})

(defn locus-statement? [locus] (= :statement (:type locus)))
(defn locus-expr? [locus] (= :expr (:type locus)))
(defn locus-return? [locus] (= :return (:type locus)))
(defn locus-assign? [locus] (= :assign (:type locus)))

(defn locus-needs-value?
  "Does this locus need the expression's value?"
  [locus]
  (not (locus-statement? locus)))

(defn locus->context
  "Convert locus to legacy context keyword."
  [locus]
  (case (:type locus)
    :statement :statement
    :expr :expr
    :return :return
    :assign :expr))

;; ============================================================
;; Statement Lifting (inlined from lift.cljc)
;; ============================================================
;; Problem: Clojure treats everything as expressions, PHP doesn't.
;; Lifting extracts statements, leaving just the value.
;;
;; Before lifting (IIFE): echo(1 + (call_user_func(function() { $x = 2; return $x; })));
;; After lifting:         $x = 2; echo(1 + $x);

(defn lift-can-lift?
  "Can we safely lift this node?"
  [node]
  (case (:op node)
    :let true
    :do true
    :loop false  ; loop/recur can't be trivially lifted
    false))

(defn lift-extract-statements
  "Extract statements that should be emitted before the value.
   Returns {:statements [...] :value node}"
  [node]
  (case (:op node)
    :let
    (let [bindings (:bindings node)
          body (:body node)]
      (if (lift-can-lift? body)
        (let [{:keys [statements value]} (lift-extract-statements body)]
          {:statements (concat
                        (map (fn [{:keys [name init]}]
                               {:op :assign :name name :init init})
                             bindings)
                        statements)
           :value value})
        {:statements (map (fn [{:keys [name init]}]
                            {:op :assign :name name :init init})
                          bindings)
         :value body}))

    :do
    (let [stmts (:statements node)
          ret (:ret node)]
      (if (lift-can-lift? ret)
        (let [{:keys [statements value]} (lift-extract-statements ret)]
          {:statements (concat stmts statements)
           :value value})
        {:statements stmts
         :value ret}))

    ;; Not liftable - no statements, whole thing is value
    {:statements []
     :value node}))

;; Forward declaration for docblock integration
(def ^:dynamic *emit-docblocks* true)

;; ============================================================
;; Core Abstractions
;; ============================================================

(def ^:dynamic *output* nil)
(def ^:dynamic *locus* nil)

;; Memoization cache for symbol munging (performance optimization)
(def ^:private munge-cache (atom {}))

(defn write
  "Write strings to output buffer. Uses StringBuilder for O(n) performance."
  [& strings]
  #?(:clj
     (doseq [s strings]
       (when s (.append ^StringBuilder *output* s)))
     :cljs
     (doseq [s strings]
       (when s (swap! *output* str s)))))

(defmulti emit* "Emit PHP for an AST node. Dispatches on :op." :op)

(declare emit-lifted)

(declare emit-node)

(defn emit-php
  "Emit PHP code for an AST node. Returns string."
  [node]
  #?(:clj
     (binding [*output* (StringBuilder. 4096)]
       (emit-node node)
       (.toString ^StringBuilder *output*))
     :cljs
     (binding [*output* (atom "")]
       (emit-node node)
       @*output*)))

(defn emit-php-lifted
  "Emit PHP with statement lifting for clean output.
   Use this for expression-position nodes that should be flattened."
  ([node] (emit-php-lifted node statement-locus))
  ([node loc]
   #?(:clj
      (binding [*output* (StringBuilder. 8192)
                *locus* loc]
        (emit-lifted loc node)
        (.toString ^StringBuilder *output*))
      :cljs
      (binding [*output* (atom "")
                *locus* loc]
        (emit-lifted loc node)
        @*output*))))

;; ============================================================
;; Symbol Munging
;; ============================================================

(def munge-map
  "Maps Clojure symbol characters to PHP-safe equivalents."
  {\- "_"
   \. "_DOT_"
   \: "_COLON_"
   \+ "_PLUS_"
   \> "_GT_"
   \< "_LT_"
   \= "_EQ_"
   \~ "_TILDE_"
   \! "_BANG_"
   \@ "_AT_"
   \# "_SHARP_"
   \? "_QMARK_"
   \* "_STAR_"
   \/ "_SLASH_"})

(defn munge-name
  "Converts a Clojure name to PHP-safe identifier (without $ prefix).
   Memoized for performance - repeated references to the same symbol are O(1)."
  [s]
  (let [s-key (str s)]
    (or (get @munge-cache s-key)
        (let [munged (apply str (map #(get munge-map % %) s-key))]
          (swap! munge-cache assoc s-key munged)
          munged))))

(defn munge-symbol
  "Converts Clojure symbol to PHP variable name with $ prefix.

   Examples:
     'foo     → \"$foo\"
     'my-var  → \"$my_var\"
     'add?    → \"$add_QMARK_\""
  [s]
  (let [s-str (str s)
        s-str (if (str/starts-with? s-str "clojure.core/")
                (subs s-str 13)
                s-str)]
    (if (or (str/includes? s-str "\\") (keyword? s))
      s-str
      (str "$" (munge-name s-str)))))

(defn munge-local
  "Converts Clojure symbol to PHP local variable name.
   Uses __L_ prefix to avoid shadowing global function aliases.

   Examples:
     'rest    → \"$__L_rest\"
     'my-var  → \"$__L_my_var\""
  [s]
  (str "$__L_" (munge-name s)))

(defn resolve-php-type
  "Converts Clojure type hint to PHP type.

   Examples:
     'Long    → \"int\"
     'String  → \"string\""
  [t]
  (case t
    Long "int"
    Integer "int"
    String "string"
    Boolean "bool"
    Double "float"
    Object "object"
    (str t)))

;; ============================================================
;; Helpers
;; ============================================================

(defn emit-comma-list [items]
  (loop [items items first? true]
    (when (seq items)
      (when-not first? (write ", "))
      (emit* (assoc-in (first items) [:env :context] :expr))
      (recur (rest items) false))))

(defn- emit-comma-list-fn
  "Version of emit-comma-list that uses emit-node for case dispatch."
  [items]
  (loop [items items first? true]
    (when (seq items)
      (when-not first? (write ", "))
      (emit-node (assoc-in (first items) [:env :context] :expr))
      (recur (rest items) false))))

(defn statement-context? [node]
  (= :statement (get-in node [:env :context])))

(defn return-context? [node]
  (= :return (get-in node [:env :context])))

(defn expr-context? [node]
  (= :expr (get-in node [:env :context])))

(defn wrap-statement [node body-fn]
  (when (return-context? node) (write "return "))
  (body-fn)
  (when (or (statement-context? node) (return-context? node))
    (write ";"))
  (when (statement-context? node)
    (write "\n")))

;; ============================================================
;; Locus-Based Emit (New API)
;; ============================================================

(defn with-locus
  "Emit node with specified locus."
  [loc node]
  (write (:pre loc))
  (binding [*locus* expr-locus]  ; children are expressions by default
    (emit-node (assoc-in node [:env :context] (locus->context loc))))
  (write (:post loc)))

(defn emit-lifted
  "Emit a node, lifting statements if needed for clean output.

   Always lift when possible - produces cleaner PHP than IIFEs."
  [loc node]
  (if (lift-can-lift? node)
    ;; Lift statements out
    (let [{:keys [statements value]} (lift-extract-statements node)]
      ;; Emit extracted statements
      (doseq [stmt statements]
        (case (:op stmt)
          :assign (do
                    ;; Use munge-local for lifted assigns (they're locals)
                    (write (munge-local (:name stmt)) " = ")
                    (emit-node (assoc-in (:init stmt) [:env :context] :expr))
                    (write ";\n"))
          ;; Regular statement node
          (emit-node (assoc-in stmt [:env :context] :statement))))
      ;; Emit the simple value with the original locus
      (with-locus loc value))
    ;; No lifting needed
    (with-locus loc node)))

(defn emit-quoted [val]
  (cond
    (nil? val) (write "null")
    (true? val) (write "true")
    (false? val) (write "false")
    (string? val) (write "'" (str/replace val "'" "\\'") "'")
    (keyword? val) (if (namespace val)
                     (write "\\Clojure\\Lang\\Keyword::createForNamespace('" (namespace val) "', '" (name val) "')")
                     (write "\\Clojure\\Lang\\Keyword::create('" (name val) "')"))
    (symbol? val) (write "'" (str val) "'")
    (number? val) (write (str val))
    (vector? val) (do
                    (write "[")
                    (loop [items val first? true]
                      (when (seq items)
                        (when-not first? (write ", "))
                        (emit-quoted (first items))
                        (recur (rest items) false)))
                    (write "]"))
    (map? val) (do
                 (write "[")
                 (loop [pairs (seq val) first? true]
                   (when (seq pairs)
                     (when-not first? (write ", "))
                     (emit-quoted (key (first pairs)))
                     (write " => ")
                     (emit-quoted (val (first pairs)))
                     (recur (rest pairs) false)))
                 (write "]"))
    (seq? val) (do
                 (write "\\Cljp\\Runtime::list([")
                 (loop [items val first? true]
                   (when (seq items)
                     (when-not first? (write ", "))
                     (emit-quoted (first items))
                     (recur (rest items) false)))
                 (write "])"))
    :else (write (pr-str val))))

;; ============================================================
;; Emit Functions (case dispatch for performance)
;; ============================================================

(defn- emit-const [{:keys [val] :as node}]
  (wrap-statement node
                  (fn []
                    (cond
                      (nil? val) (write "null")
                      (true? val) (write "true")
                      (false? val) (write "false")
                      (string? val) (write "'" (str/replace val "'" "\\'") "'")
                      (keyword? val) (if (namespace val)
                                       (write "\\Clojure\\Lang\\Keyword::createForNamespace('" (namespace val) "', '" (name val) "')")
                                       (write "\\Clojure\\Lang\\Keyword::create('" (name val) "')"))
                      (number? val) (write (str val))
                      :else (write (pr-str val))))))

(defn- emit-local [{:keys [name] :as node}]
  "Emit local variable reference (let-bound, fn param, loop var)."
  (wrap-statement node
                  (fn []
                    ;; Local variable - use __L_ prefix to avoid shadowing globals
                    (write (munge-local name)))))

(defn- emit-var [{:keys [sym] :as node}]
  "Emit namespace/global variable reference."
  (wrap-statement node
                  (fn []
                    ;; Global variable - use $GLOBALS for closure scope
                    (write "$GLOBALS['" (munge-name sym) "']"))))

(defn- emit-php-const [{:keys [const-name] :as node}]
  (wrap-statement node
                  (fn []
                    ;; Emit PHP constant directly (PHP_EOL, E_ALL, __FILE__, etc.)
                    (write const-name))))

(defn- emit-if [{:keys [test then else] :as node}]
  (if (or (statement-context? node) (return-context? node))
    ;; Statement form
    (do
      (write "if (")
      (emit-node (assoc-in test [:env :context] :expr))
      (write ") { ")
      (emit-node (assoc-in then [:env :context] (if (return-context? node) :return :statement)))
      (write "}")
      (when else
        (write " else { ")
        (emit-node (assoc-in else [:env :context] (if (return-context? node) :return :statement)))
        (write "}"))
      (when (statement-context? node) (write "\n")))
    ;; Expression form (ternary)
    (do
      (write "(")
      (emit-node (assoc-in test [:env :context] :expr))
      (write " ? ")
      (emit-node (assoc-in then [:env :context] :expr))
      (write " : ")
      (if else
        (emit-node (assoc-in else [:env :context] :expr))
        (write "null"))
      (write ")"))))

(defn- emit-do [{:keys [statements ret] :as node}]
  (let [outer-ctx (get-in node [:env :context])]
    (if (expr-context? node)
      ;; Expression context - need IIFE to wrap multiple statements
      (if (empty? statements)
        ;; No statements, just emit the return value
        (emit-node (assoc-in ret [:env :context] :expr))
        ;; Has statements - wrap in IIFE
        (do
          (write "(call_user_func(function() { ")
          (doseq [stmt statements]
            (emit-node (assoc-in stmt [:env :context] :statement)))
          (emit-node (assoc-in ret [:env :context] :return))
          (write " }))"))) ;; closes: write, do, inner-if
      ;; Statement/return context - preserve outer context for ret
      (do
        (doseq [stmt statements]
          (emit-node (assoc-in stmt [:env :context] :statement)))
        (emit-node (assoc-in ret [:env :context] outer-ctx))))))

(defn- emit-let [{:keys [bindings body] :as node}]
  (if (expr-context? node)
    ;; Expression context - need IIFE
    (do
      (write "(call_user_func(function() { ")
      (doseq [{:keys [name init]} bindings]
        (write (munge-local name) " = ")
        (emit-node (assoc-in init [:env :context] :expr))
        (write "; "))
      (emit-node (assoc-in body [:env :context] :return))
      (write " }))"))
    ;; Statement/return context - preserve the context for the body
    (do
      (doseq [{:keys [name init]} bindings]
        (write (munge-local name) " = ")
        (emit-node (assoc-in init [:env :context] :expr))
        (write ";\n"))
      ;; Pass along the outer context (statement or return) to the body
      (emit-node (assoc-in body [:env :context] (get-in node [:env :context]))))))

(defn- emit-letfn [{:keys [fns body] :as node}]
  ;; letfn emits all function definitions first (enabling mutual recursion)
  ;; then executes the body
  (if (expr-context? node)
    ;; Expression context - need IIFE
    (do
      (write "(call_user_func(function() { ")
      (doseq [{:keys [name fn]} fns]
        (write (munge-local name) " = ")
        (emit-node (assoc-in fn [:env :context] :expr))
        (write "; "))
      (emit-node (assoc-in body [:env :context] :return))
      (write " }))"))
    ;; Statement/return context
    (do
      (doseq [{:keys [name fn]} fns]
        (write (munge-local name) " = ")
        (emit-node (assoc-in fn [:env :context] :expr))
        (write ";\n"))
      (emit-node (assoc-in body [:env :context] (get-in node [:env :context]))))))

(defn- emit-fn-docblock
  "Emit PHPDoc for function if type info available.
   Uses lazy type inference if types not already on node."
  [{:keys [param-types return-type] :as node}]
  ;; Use lazy inference if types not already computed
  (let [param-types (or param-types (infer/get-fn-param-types node))
        return-type (or return-type (infer/get-fn-return-type node))
        has-types (or (some :type param-types) return-type)]
    (when (and *emit-docblocks* has-types)
      (write "/** ")
      (when (some :type param-types)
        (doseq [{:keys [name type]} param-types]
          (when type
            (write "@param " type " $" (munge-name name) " "))))
      (when return-type
        (write "@return " (if (map? return-type)
                            (str/join "|" (sort (:union return-type)))
                            (str return-type))))
      (write " */ "))))

(defn- collect-free-vars
  "Collect variables referenced in body that are in outer scope (env locals),
   excluding the function's own parameters."
  [env params]
  (let [locals (set (keys (:locals env)))
        param-set (set params)]
    ;; Return locals that exist in the environment but are not this function's params
    (set/difference locals param-set)))

(defn- emit-single-arity-fn
  "Emit a single-arity function."
  [{:keys [name env] :as node} {:keys [params variadic? body]}]
  (wrap-statement node
                  (fn []
                    ;; Emit docblock if we have type info
                    (emit-fn-docblock node)
                    (write "(function(")
                    ;; Emit params
                    (loop [ps params first? true]
                      (when (seq ps)
                        (when-not first? (write ", "))
                        (when (and variadic? (= 1 (count ps)))
                          (write "..."))
                        ;; Use munge-local for params (they're locals)
                        (write (munge-local (first ps)))
                        (recur (rest ps) false)))
                    (write ")")
                    ;; Add use clause to capture outer locals
                    (let [free-vars (collect-free-vars env params)]
                      (when (seq free-vars)
                        (write " use (")
                        (loop [vars (seq free-vars) first? true]
                          (when vars
                            (when-not first? (write ", "))
                            ;; Use munge-local with & for reference
                            (write "&" (munge-local (first vars)))
                            (recur (next vars) false)))
                        (write ")")))
                    (write " { ")
                    (emit-node (assoc-in body [:env :context] :return))
                    (write "})"))))

(defn- emit-multi-arity-fn
  "Emit a multi-arity function with dispatch based on arg count."
  [{:keys [name arities env] :as node}]
  ;; Multi-arity function: dispatch based on func_num_args()
  ;; PHP: (function(...$args) use (...) { switch(count($args)) { case N: ... } })
  (wrap-statement node
                  (fn []
                    (write "(function(...$__args)")
                    ;; Collect free vars from all arities
                    (let [all-params (into #{} (mapcat :params arities))
                          free-vars (collect-free-vars env all-params)]
                      (when (seq free-vars)
                        (write " use (")
                        (loop [vars (seq free-vars) first? true]
                          (when vars
                            (when-not first? (write ", "))
                            (write "&" (munge-local (first vars)))
                            (recur (next vars) false)))
                        (write ")")))
                    (write " { ")
                    (write "$__n = count($__args); ")
                    ;; Sort arities: variadic last, then by param count descending
                    (let [sorted-arities (sort-by (fn [a] [(if (:variadic? a) 1 0) (- (count (:params a)))]) arities)]
                      (loop [arities sorted-arities first? true]
                        (when (seq arities)
                          (let [{:keys [params variadic? body]} (first arities)
                                min-args (if variadic? (dec (count params)) (count params))]
                            (if variadic?
                              ;; Variadic: if ($__n >= min-args)
                              (do
                                (when-not first? (write " else "))
                                (write "if ($__n >= " min-args ") { "))
                              ;; Fixed arity: if ($__n == count)
                              (do
                                (when-not first? (write " else "))
                                (write "if ($__n == " (count params) ") { ")))
                            ;; Destructure args into named params (use munge-local)
                            (doseq [[i p] (map-indexed vector params)]
                              (if (and variadic? (= i (dec (count params))))
                                ;; Rest param: array_slice
                                (write (munge-local p) " = array_slice($__args, " i "); ")
                                ;; Regular param
                                (write (munge-local p) " = $__args[" i "]; ")))
                            ;; Emit body
                            (emit-node (assoc-in body [:env :context] :return))
                            (write " }"))
                          (recur (rest arities) false))))
                    ;; Default case: throw arity error
                    (write " else { throw new \\InvalidArgumentException('Wrong number of args: ' . $__n); }")
                    (write " })"))))

(defn- emit-fn [{:keys [arities] :as node}]
  "Emit function - dispatches to single or multi-arity based on arities count."
  (if (= 1 (count arities))
    (emit-single-arity-fn node (first arities))
    (emit-multi-arity-fn node)))

(defn- emit-fn-multi [{:keys [name arities env] :as node}]
  ;; Multi-arity function: dispatch based on func_num_args()
  ;; PHP: (function(...$args) use (...) { switch(count($args)) { case N: ... } })
  (wrap-statement node
                  (fn []
                    (write "(function(...$__args)")
                    ;; Collect free vars from all arities
                    (let [all-params (into #{} (mapcat :params arities))
                          free-vars (collect-free-vars env all-params)]
                      (when (seq free-vars)
                        (write " use (")
                        (loop [vars (seq free-vars) first? true]
                          (when vars
                            (when-not first? (write ", "))
                            (write "&" (munge-local (first vars)))
                            (recur (next vars) false)))
                        (write ")")))
                    (write " { ")
                    (write "$__n = count($__args); ")
                    ;; Sort arities: variadic last, then by param count descending
                    (let [sorted-arities (sort-by (fn [a] [(if (:variadic? a) 1 0) (- (count (:params a)))]) arities)]
                      (loop [arities sorted-arities first? true]
                        (when (seq arities)
                          (let [{:keys [params variadic? body]} (first arities)
                                min-args (if variadic? (dec (count params)) (count params))]
                            (if variadic?
                              ;; Variadic: if ($__n >= min-args)
                              (do
                                (when-not first? (write " else "))
                                (write "if ($__n >= " min-args ") { "))
                              ;; Fixed arity: if ($__n == count)
                              (do
                                (when-not first? (write " else "))
                                (write "if ($__n == " (count params) ") { ")))
                            ;; Destructure args into named params (use munge-local)
                            (doseq [[i p] (map-indexed vector params)]
                              (if (and variadic? (= i (dec (count params))))
                                ;; Rest param: array_slice
                                (write (munge-local p) " = array_slice($__args, " i "); ")
                                ;; Regular param
                                (write (munge-local p) " = $__args[" i "]; ")))
                            ;; Emit body
                            (emit-node (assoc-in body [:env :context] :return))
                            (write " }"))
                          (recur (rest arities) false))))
                    ;; Default case: throw arity error
                    (write " else { throw new \\InvalidArgumentException('Wrong number of args: ' . $__n); }")
                    (write " })"))))

(defn- emit-def [{:keys [name init]}]
  ;; Use $GLOBALS for top-level defs so closures can reference them
  (let [var-name (munge-name name)]
    (write "$GLOBALS['" var-name "'] = ")
    (if init
      (emit-node (assoc-in init [:env :context] :expr))
      (write "null"))
    (write ";\n")
    ;; Also create local alias for direct use
    (write "$" var-name " = &$GLOBALS['" var-name "'];\n")))

(defn- emit-loop [{:keys [bindings body] :as node}]
  (let [binding-names (mapv :name bindings)]
    (if (expr-context? node)
      ;; Expression context - need IIFE
      (do
        (write "(call_user_func(function() { ")
        (doseq [{:keys [name init]} bindings]
          (write (munge-local name) " = ")
          (emit-node (assoc-in init [:env :context] :expr))
          (write "; "))
        (write " while(true) { ")
        (emit-node (-> body
                       (assoc-in [:env :context] :statement)
                       (assoc-in [:env :loop-bindings] binding-names)))
        (write " break; }")
        (write " return " (munge-local (first binding-names)) ";")
        (write " }))"))
      ;; Statement context
      (do
        (doseq [{:keys [name init]} bindings]
          (write (munge-local name) " = ")
          (emit-node (assoc-in init [:env :context] :expr))
          (write ";\n"))
        (write " while(true) { ")
        (emit-node (-> body
                       (assoc-in [:env :context] (if (return-context? node) :return :statement))
                       (assoc-in [:env :loop-bindings] binding-names)))
        (write " break; }\n")))))

(defn- emit-recur [{:keys [env args]}]
  (let [bindings (get env :loop-bindings)]
    (loop [bs bindings as args]
      (when (seq bs)
        (write (munge-local (first bs)) " = ")
        (emit-node (assoc-in (first as) [:env :context] :expr))
        (write "; ")
        (recur (rest bs) (rest as))))
    (write "continue;")))

(defn- emit-throw [{:keys [exception]}]
  (write "throw ")
  (emit-node (assoc-in exception [:env :context] :expr))
  (write ";\n"))

(defn- emit-try [{:keys [body catches finally]}]
  (write "try { ")
  (emit-node (assoc-in body [:env :context] :return))
  (write " }")
  (doseq [{:keys [class name body]} catches]
    (write " catch (" class " " (munge-local name) ") { ")
    (emit-node (assoc-in body [:env :context] :return))
    (write " }"))
  (when finally
    (write " finally { ")
    (emit-node (assoc-in finally [:env :context] :statement))
    (write " }"))
  (write "\n"))

(defn- emit-quote [{:keys [val] :as node}]
  (wrap-statement node (fn [] (emit-quoted val))))

(defn- emit-new [{:keys [class args] :as node}]
  (wrap-statement node
                  (fn []
                    (write "(new " class "(")
                    (emit-comma-list-fn args)
                    (write "))"))))

(defn- emit-method [{:keys [method target args] :as node}]
  (wrap-statement node
                  (fn []
                    (emit-node (assoc-in target [:env :context] :expr))
                    (write "->" method "(")
                    (emit-comma-list-fn args)
                    (write ")"))))

(defn- emit-static-call [{:keys [class method args] :as node}]
  (wrap-statement node
                  (fn []
                    (write class "::" method "(")
                    (emit-comma-list-fn args)
                    (write ")"))))

;; Clojure operators (+ - * / mod etc.) - handles unary minus
(defn- emit-infix [{:keys [operator args clj-operator] :as node}]
  (wrap-statement node
                  (fn []
                    (cond
                      ;; Unary minus: (- x) → (-$x)
                      (and (= clj-operator "-") (= 1 (count args)))
                      (do
                        (write "(-")
                        (emit-node (assoc-in (first args) [:env :context] :expr))
                        (write ")"))

                      ;; Single arg for other ops: just return the value
                      (= 1 (count args))
                      (emit-node (assoc-in (first args) [:env :context] :expr))

                      ;; Binary/multi-arity: (+ a b c) → (($a + $b) + $c)
                      :else
                      (do
                        (write "(")
                        (loop [items args first? true]
                          (when (seq items)
                            (when-not first? (write " " operator " "))
                            (emit-node (assoc-in (first items) [:env :context] :expr))
                            (recur (rest items) false)))
                        (write ")"))))))

(defn- emit-php-infix [{:keys [operator args] :as node}]
  (wrap-statement node
                  (fn []
                    (write "(")
                    (if (= operator "instanceof")
                      ;; Special case: instanceof needs unquoted class name
                      (do
                        (emit-node (assoc-in (first args) [:env :context] :expr))
                        (write " instanceof ")
                        ;; Second arg is the class name - emit without quotes
                        (let [class-node (second args)]
                          (if (= :const (:op class-node))
                            (write (str (:val class-node)))  ; Emit raw class name
                            (emit-node (assoc-in class-node [:env :context] :expr)))))
                      ;; Normal infix operators
                      (loop [items args first? true]
                        (when (seq items)
                          (when-not first? (write " " operator " "))
                          (emit-node (assoc-in (first items) [:env :context] :expr))
                          (recur (rest items) false))))
                    (write ")"))))

(defn- emit-php-call [{:keys [fn-name args] :as node}]
  (wrap-statement node
                  (fn []
                    (write fn-name "(")
                    (emit-comma-list-fn args)
                    (write ")"))))

;; ============================================================
;; Host Interop (Platform-Agnostic HIR → PHP)
;; ============================================================

(defn- emit-host-call
  "Emit host interop call. Dispatches based on :call-type."
  [{:keys [call-type host-ns target class fn-name args] :as node}]
  (wrap-statement node
                  (fn []
                    (case call-type
                      ;; Function call: (php/strlen s) → strlen($s)
                      :function
                      (do
                        (write fn-name "(")
                        (emit-comma-list-fn args)
                        (write ")"))

                      ;; Method call: (.method obj x) → $obj->method($x)
                      :method
                      (do
                        (emit-node (assoc-in target [:env :context] :expr))
                        (write "->" fn-name "(")
                        (emit-comma-list-fn args)
                        (write ")"))

                      ;; Static call: (Class/method x) → Class::method($x)
                      :static
                      (do
                        (write class "::" fn-name "(")
                        (emit-comma-list-fn args)
                        (write ")"))

                      ;; Operator: (+ a b) → ($a + $b) or (php/+ a b) → ($a + $b)
                      :operator
                      (cond
                        ;; Unary minus: (- x) → (-$x)
                        (and (= fn-name "-") (= 1 (count args)))
                        (do
                          (write "(-")
                          (emit-node (assoc-in (first args) [:env :context] :expr))
                          (write ")"))

                        ;; Single arg for other ops: just return the value
                        (= 1 (count args))
                        (emit-node (assoc-in (first args) [:env :context] :expr))

                        ;; instanceof special case
                        (= fn-name "instanceof")
                        (do
                          (write "(")
                          (emit-node (assoc-in (first args) [:env :context] :expr))
                          (write " instanceof ")
                          ;; Second arg is the class name - emit without quotes
                          (let [class-node (second args)]
                            (if (= :const (:op class-node))
                              (write (str (:val class-node)))  ; Emit raw class name
                              (emit-node (assoc-in class-node [:env :context] :expr))))
                          (write ")"))

                        ;; Binary/multi-arity: (+ a b c) → (($a + $b) + $c)
                        :else
                        (do
                          (write "(")
                          (loop [items args first? true]
                            (when (seq items)
                              (when-not first? (write " " fn-name " "))
                              (emit-node (assoc-in (first items) [:env :context] :expr))
                              (recur (rest items) false)))
                          (write ")")))))))

(defn- emit-host-new
  "Emit host object instantiation: (new Class args) → new Class($args)"
  [{:keys [class args] :as node}]
  (wrap-statement node
                  (fn []
                    (write "(new " class "(")
                    (emit-comma-list-fn args)
                    (write "))"))))

(defn- emit-host-const
  "Emit host constant: php/PHP_EOL → PHP_EOL"
  [{:keys [const-name] :as node}]
  (wrap-statement node
                  (fn []
                    ;; Emit PHP constant directly (PHP_EOL, E_ALL, __FILE__, etc.)
                    (write const-name))))

(defn- emit-vector [{:keys [items] :as node}]
  (wrap-statement node
                  (fn []
                    (write "\\Cljp\\Runtime::vector([")
                    (emit-comma-list-fn items)
                    (write "])"))))

(defn- emit-map [{:keys [keys vals] :as node}]
  (wrap-statement node
                  (fn []
                    (write "\\Cljp\\Runtime::hashMap([")
                    (loop [ks keys vs vals first? true]
                      (when (seq ks)
                        (when-not first? (write ", "))
                        (emit-node (assoc-in (first ks) [:env :context] :expr))
                        (write ", ")
                        (emit-node (assoc-in (first vs) [:env :context] :expr))
                        (recur (rest ks) (rest vs) false)))
                    (write "])"))))

(defn- emit-set [{:keys [items] :as node}]
  (wrap-statement node
                  (fn []
                    (write "\\Cljp\\Runtime::set([")
                    (emit-comma-list-fn items)
                    (write "])"))))

(defn- emit-invoke [{:keys [args] :as node}]
  (let [fn-node (:fn node)]
    (wrap-statement node
                    (clojure.core/fn []
                      (write "call_user_func(")
                      (emit-node (assoc-in fn-node [:env :context] :expr))
                      (doseq [arg args]
                        (write ", ")
                        (emit-node (assoc-in arg [:env :context] :expr)))
                      (write ")")))))

;; Case expression → PHP match
(defn- emit-case [{:keys [expr clauses default] :as node}]
  (wrap-statement node
                  (fn []
                    (write "match(")
                    (emit-node (assoc-in expr [:env :context] :expr))
                    (write ") { ")
                    ;; Emit each clause
                    (doseq [{:keys [test-vals then]} clauses]
                      ;; Multiple values: 1, 2, 3 => result
                      (loop [vals test-vals first? true]
                        (when (seq vals)
                          (when-not first? (write ", "))
                          (emit-quoted (first vals))
                          (recur (rest vals) false)))
                      (write " => ")
                      (emit-node (assoc-in then [:env :context] :expr))
                      (write ", "))
                    ;; Default clause
                    (when default
                      (write "default => ")
                      (emit-node (assoc-in default [:env :context] :expr)))
                    (write " }"))))

;; ============================================================
;; Case-Based Dispatch (Performance Optimization)
;; ============================================================
;; Using case dispatch instead of multimethods provides 2-3x faster dispatch.
;; This is the primary emit function - multimethods are kept for backward
;; compatibility and extensibility.

(defn emit-node
  "Fast case-based dispatch for emitting HIR nodes to PHP.
   Falls back to multimethod for unknown op types."
  [{:keys [op] :as node}]
  (case op
    ;; Core nodes
    :const       (emit-const node)
    :local       (emit-local node)
    :var         (emit-var node)
    :if          (emit-if node)
    :do          (emit-do node)
    :let         (emit-let node)
    :letfn       (emit-letfn node)
    :fn          (emit-fn node)
    :def         (emit-def node)
    :loop        (emit-loop node)
    :recur       (emit-recur node)
    :throw       (emit-throw node)
    :try         (emit-try node)
    :quote       (emit-quote node)
    :invoke      (emit-invoke node)

    ;; Data nodes
    :vector      (emit-vector node)
    :map         (emit-map node)
    :set         (emit-set node)

    ;; Host interop nodes (platform-agnostic HIR)
    :host-call   (emit-host-call node)
    :host-new    (emit-host-new node)
    :host-const  (emit-host-const node)
    :case        (emit-case node)

    ;; Legacy nodes (for backward compatibility during transition)
    :fn-multi    (emit-fn-multi node)
    :new         (emit-new node)
    :method      (emit-method node)
    :static-call (emit-static-call node)
    :infix       (emit-infix node)
    :php-infix   (emit-php-infix node)
    :php-call    (emit-php-call node)
    :php-const   (emit-php-const node)

    ;; Fall back to multimethod for extensibility
    (emit* node)))

;; ============================================================
;; Multimethod Definitions (Backward Compatibility)
;; ============================================================
;; These defmethods delegate to the fast functions above,
;; allowing external code to still use emit* while getting
;; the performance benefits internally.

;; Core nodes
(defmethod emit* :const [node] (emit-const node))
(defmethod emit* :local [node] (emit-local node))
(defmethod emit* :var [node] (emit-var node))
(defmethod emit* :if [node] (emit-if node))
(defmethod emit* :do [node] (emit-do node))
(defmethod emit* :let [node] (emit-let node))
(defmethod emit* :letfn [node] (emit-letfn node))
(defmethod emit* :fn [node] (emit-fn node))
(defmethod emit* :def [node] (emit-def node))
(defmethod emit* :loop [node] (emit-loop node))
(defmethod emit* :recur [node] (emit-recur node))
(defmethod emit* :throw [node] (emit-throw node))
(defmethod emit* :try [node] (emit-try node))
(defmethod emit* :quote [node] (emit-quote node))
(defmethod emit* :invoke [node] (emit-invoke node))

;; Data nodes
(defmethod emit* :vector [node] (emit-vector node))
(defmethod emit* :map [node] (emit-map node))
(defmethod emit* :set [node] (emit-set node))

;; Host interop nodes (platform-agnostic)
(defmethod emit* :host-call [node] (emit-host-call node))
(defmethod emit* :host-new [node] (emit-host-new node))
(defmethod emit* :host-const [node] (emit-host-const node))
(defmethod emit* :case [node] (emit-case node))

;; Legacy nodes (backward compatibility)
(defmethod emit* :fn-multi [node] (emit-fn-multi node))
(defmethod emit* :new [node] (emit-new node))
(defmethod emit* :method [node] (emit-method node))
(defmethod emit* :static-call [node] (emit-static-call node))
(defmethod emit* :infix [node] (emit-infix node))
(defmethod emit* :php-infix [node] (emit-php-infix node))
(defmethod emit* :php-call [node] (emit-php-call node))
(defmethod emit* :php-const [node] (emit-php-const node))
