#!/usr/bin/env clojure
;; =============================================================================
;; specgen.clj — auto-generate parity specs from live JVM reflection
;;
;; Every Clojure var carries its own spec: arglists, doc, tag.
;; This script reflects on loaded namespaces and generates test expressions.
;; The JVM IS the oracle — run, capture, compare.
;;
;; Usage:
;;   par specgen                                           # core overview
;;   par specgen clojure.string clojure.set                # specific namespaces
;;   par specgen --write spec/gen/                         # write .gen.edn files
;;   par specgen --stats                                   # stats only
;;   par specgen --coverage <dir> --host-data <host.edn>   # coverage analysis
;;   par coverage <ported-dir> <clojure-src>               # (par wires both)
;; =============================================================================

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.edn :as edn]
         '[clojure.pprint])

;; Load all namespaces — some may fail if deps aren't on classpath
(doseq [ns-sym '[;; Shipped with Clojure
                 clojure.set clojure.walk clojure.data clojure.datafy
                 clojure.edn clojure.math clojure.repl clojure.test
                 clojure.zip clojure.xml clojure.stacktrace clojure.template
                 clojure.instant clojure.reflect clojure.inspector clojure.main
                 clojure.java.browse clojure.java.javadoc clojure.java.process
                 clojure.java.shell clojure.core.reducers clojure.core.protocols
                 clojure.core.server clojure.spec.alpha clojure.spec.gen.alpha
                 clojure.spec.test.alpha
                 ;; Contrib — active
                 clojure.core.async
                 clojure.core.cache clojure.core.memoize
                 clojure.data.csv clojure.data.json clojure.data.xml
                 clojure.data.zip clojure.data.priority-map
                 clojure.data.int-map clojure.data.avl clojure.data.codec.base64
                 clojure.java.data clojure.java.classpath
                 clojure.math.combinatorics clojure.math.numeric-tower
                 clojure.test.check clojure.test.check.generators
                 clojure.test.check.properties clojure.test.check.clojure-test
                 clojure.tools.cli clojure.tools.logging
                 clojure.tools.namespace.find clojure.tools.namespace.repl
                 clojure.tools.reader clojure.tools.reader.edn
                 clojure.tools.trace
                 ;; Contrib — stable
                 clojure.algo.generic.arithmetic clojure.algo.generic.collection
                 clojure.algo.generic.functor clojure.algo.generic.math-functions
                 clojure.algo.monads
                 clojure.core.logic clojure.core.match
                 clojure.core.unify]]
  (try (require ns-sym) (catch Exception _)))

;; =============================================================================
;; Value pools — "interesting values" for each domain
;; =============================================================================

(def pools
  {:nil     ["nil"]
   :bool    ["true" "false"]
   :int     ["0" "1" "-1" "42" "100"]
   :float   ["0.0" "3.14" "-2.5"]
   :num     ["0" "1" "-1" "42" "3.14"]
   :string  ["\"\"" "\"hello\"" "\"hello world\"" "\"abc\""]
   :keyword [":a" ":b" ":foo" ":bar/baz"]
   :symbol  ["'foo" "'bar" "'baz/quux"]
   :vec     ["[]" "[1]" "[1 2 3]" "[1 2 3 4 5]"]
   :map     ["{}" "{:a 1}" "{:a 1 :b 2 :c 3}"]
   :set     ["#{}" "#{1 2 3}" "#{:a :b}" "#{{:a 1 :b 2} {:a 3 :b 4}}"]
   :list    ["'()" "'(1 2 3)"]
   :coll    ["[]" "[1 2 3]" "'(1 2 3)" "{:a 1 :b 2}" "#{1 2 3}"]
   :fn      ["inc" "dec" "identity" "str" "keyword"]
   :pred    ["even?" "odd?" "nil?" "pos?" "string?"]
   :regex   ["#\"\\\\d+\"" "#\"[a-z]+\"" "#\"hello\""]
   :any     ["nil" "true" "0" "1" "-1" "42" "3.14"
             "\"hello\"" ":a" "[]" "[1 2]" "{:a 1}" "#{1}"]
   :xf      ["(map inc)" "(filter even?)" "(take 3)"]})

;; =============================================================================
;; Arg name → pool heuristics
;; =============================================================================

(defn arg-pool
  "Pick a value pool for an argument based on its name and namespace context."
  [arg-name ns-name]
  (let [s (str arg-name)]
    (cond
      ;; Exact matches
      (= s "&")               nil  ;; skip rest marker
      (#{"n" "num" "x" "y"
         "a" "b" "start" "end"
         "step" "init" "from"
         "to" "dividend" "divisor"
         "index" "i" "j"} s)            (:num pools)
      (#{"s" "string" "cs"
         "substr" "replacement"} s)     (:string pools)
      (#{"coll" "c" "c1" "c2"
         "c3" "colls"} s)               (:coll pools)
      (#{"f" "fn" "pred" "g"} s)        (:fn pools)
      (#{"xform" "xf"} s)               (:xf pools)
      (#{"k" "key"} s)                  (:keyword pools)
      (#{"ks" "keys"} s)                (:keyword pools)
      (#{"v" "val" "e"} s)              (:any pools)
      (#{"m" "map" "kmap" "smap"} s)    (:map pools)
      (#{"re" "pattern" "match"} s)     (:regex pools)
      (#{"t" "tag"} s)                  (:keyword pools)
      (#{"xrel" "yrel" "xset"
         "s1" "s2" "set1" "set2"} s)    (:set pools)

      ;; Pattern matches
      (str/ends-with? s "map")          (:map pools)
      (str/ends-with? s "set")          (:set pools)
      (str/ends-with? s "?")            (:any pools)

      ;; Namespace context defaults
      (= ns-name "clojure.string")      (:string pools)
      (= ns-name "clojure.math")        (:num pools)
      (= ns-name "clojure.set")         (:set pools)

      ;; Fallback
      :else                              (:any pools))))

(defn limit-pool
  "Limit pool size based on arity to prevent cross-product explosion."
  [pool arity]
  (let [max-per-arg (cond
                      (<= arity 1) 8
                      (= arity 2)  5
                      (= arity 3)  3
                      :else        2)]
    (vec (take max-per-arg pool))))

;; =============================================================================
;; Function classification
;; =============================================================================

(def skip-reasons
  "Vars that need hand-written tests, grouped by reason."
  {:dynamic-vars
   {:reason "Dynamic vars — test via binding forms"
    :vars #{'*ns* '*out* '*err* '*in* '*file* '*command-line-args* '*print-length*
            '*print-level* '*print-meta* '*print-dup* '*print-readably* '*flush-on-newline*
            '*read-eval* '*data-readers* '*default-data-reader-fn* '*assert*
            '*math-context* '*agent* '*1 '*2 '*3 '*e '*warn-on-reflection*
            '*unchecked-math* '*compiler-options* '*compile-path* '*compile-files*
            '*allow-unresolved-vars* '*reader-resolver* '*source-path* '*use-context-classloader*
            '*verbose-defrecords* '*fn-loader* '*suppress-read*}}

   :side-effects
   {:reason "Side effects — need isolation or are destructive"
    :vars #{'shutdown-agents 'alter-var-root 'intern
            'ns-unmap 'ns-unalias 'remove-ns 'in-ns
            'load 'load-file 'load-reader 'load-string
            'require 'use 'import 'refer 'refer-clojure
            'compile 'gen-class 'gen-interface}}

   :agents
   {:reason "Agent ops — need agent context + async"
    :vars #{'send 'send-off 'send-via 'restart-agent
            'set-agent-send-executor! 'set-agent-send-off-executor!
            'await 'await-for 'await1
            'add-watch 'remove-watch 'set-validator!}}

   :refs
   {:reason "Ref/STM ops — need (dosync) + ref context"
    :vars #{'dosync 'commute 'alter 'ref-set 'ensure}}

   :atoms
   {:reason "Atom ops — need (atom) context"
    :vars #{'swap! 'reset! 'swap-vals! 'reset-vals!}}

   :volatiles
   {:reason "Volatile ops — need (volatile!) context"
    :vars #{'vswap! 'vreset!}}

   :promises
   {:reason "Promise ops — need (promise) context"
    :vars #{'deliver}}

   :async
   {:reason "Async/threading — test separately with timeouts"
    :vars #{'future 'future-call 'pmap 'pcalls 'pvalues 'locking 'io!}}

   :io
   {:reason "I/O — need streams, files, or readers"
    :vars #{'slurp 'spit 'print 'println 'pr 'prn 'printf 'newline 'flush
            'read 'read-line 'read-string}}

   :hierarchy
   {:reason "Hierarchy ops — need (make-hierarchy) context"
    :vars #{'class 'type 'supers 'bases 'parents 'ancestors 'descendants
            'make-hierarchy 'isa? 'derive 'underive}}

   :eval
   {:reason "Eval/macroexpand — need controlled expressions"
    :vars #{'eval 'macroexpand 'macroexpand-1 'special-symbol? 'find-keyword}}

   :binding-forms
   {:reason "Binding macros — need var + value setup"
    :vars #{'binding 'with-bindings 'with-bindings*
            'with-redefs 'with-redefs-fn}}})

(def skip-vars
  "Flat set of all skip vars."
  (into #{} (mapcat (comp :vars val)) skip-reasons))

(defn skip-reason
  "Return the skip-reason key for a var, or nil if not skipped."
  [sym ns-name]
  (let [lookup-sym (if (= ns-name "clojure.core") sym
                       (symbol (str ns-name "/" (name sym))))]
    (some (fn [[reason-key {:keys [vars]}]]
            (when (contains? vars lookup-sym) reason-key))
          skip-reasons)))

(defn classify-var [sym var-meta ns-name]
  (let [nm (name sym)
        macro? (:macro var-meta)
        arglists (:arglists var-meta)
        sr (skip-reason sym ns-name)
        min-arity (when arglists
                    (apply min (map #(count (take-while (fn [a] (not= '& a)) %)) arglists)))
        unary-pred? (and (str/ends-with? nm "?") (= 1 min-arity))]
    (cond
      sr                                  [:skip sr]
      (nil? arglists)                     [:skip :not-a-fn]
      (str/starts-with? nm "->")          [:skip :constructor]
      (str/starts-with? nm "map->")       [:skip :constructor]
      (> (or min-arity 0) 4)             [:skip :high-arity]
      macro?                              :macro
      unary-pred?                         :predicate
      :else                               :function)))

;; =============================================================================
;; Spec generation
;; =============================================================================

(defn arg-name
  "Extract a usable name from an arglist entry (handles destructuring)."
  [arg]
  (cond
    (symbol? arg) (name arg)
    (vector? arg) "v"     ;; vector destructuring
    (map? arg)    "m"     ;; map destructuring
    :else         "x"))

(defn gen-parametric
  "Generate a parametric spec entry for a function var."
  [sym var-meta ns-name]
  (let [arglists (:arglists var-meta)
        ;; Pick the most useful arity (shortest non-zero, or zero if that's all there is)
        sorted (sort-by count arglists)
        target-arity (or (first (filter #(pos? (count %)) sorted))
                         (first sorted))
        ;; Remove & and rest args for parametric
        fixed-args (take-while #(not= '& %) target-arity)
        arity (count fixed-args)]
    (when (and (pos? arity) (<= arity 4))
      (let [;; Deduplicate arg names (some fns have [x x] style via destructuring)
            named-args (map-indexed
                         (fn [i arg]
                           (let [base (arg-name arg)]
                             (if (> (count (filter #(= base (arg-name %)) fixed-args)) 1)
                               (str base (inc i))
                               base)))
                         fixed-args)
            params (into {}
                     (map (fn [arg orig]
                            [(keyword arg)
                             (limit-pool (arg-pool orig ns-name) arity)])
                          named-args fixed-args))
            qualified (if (= ns-name "clojure.core")
                        (name sym)
                        (str ns-name "/" (name sym)))
            template (str "(" qualified
                          (str/join "" (map #(str " %" %) named-args))
                          ")")]
        {:describe (name sym)
         :params params
         :template template}))))

(defn gen-nullary
  "Generate test entries for zero-arity functions."
  [sym var-meta ns-name]
  (try
    (let [arglists (:arglists var-meta)
          has-nullary? (some #(zero? (count %)) arglists)
          qualified (if (= ns-name "clojure.core")
                      (name sym)
                      (str ns-name "/" (name sym)))]
      (when has-nullary?
        {:it (name sym) :eval (str "(" qualified ")")}))
    (catch Exception _ nil)))

(defn gen-predicate-spec
  "For predicates, test against ALL value types."
  [sym var-meta ns-name]
  (let [qualified (if (= ns-name "clojure.core")
                    (name sym)
                    (str ns-name "/" (name sym)))]
    {:describe (name sym)
     :params {:val (:any pools)}
     :template (str "(" qualified " %val)")}))

;; =============================================================================
;; Namespace processing
;; =============================================================================

(defn process-namespace
  "Reflect on a namespace, generate spec entries."
  [ns-name]
  (try
    (require (symbol ns-name))
    (let [ns-sym (symbol ns-name)
          publics (sort-by key (ns-publics ns-sym))
          ;; Classify all vars
          classified (for [[sym v] publics
                           :let [m (meta v)
                                 cls (classify-var sym m ns-name)]]
                       {:sym sym :vmeta m :class cls})
          ;; Collect skips with reasons
          skipped-vars (for [{:keys [sym class]} classified
                             :when (and (vector? class) (= :skip (first class)))]
                         {:var (name sym) :reason (second class)})
          ;; Generate specs for non-skipped
          active (remove #(and (vector? (:class %)) (= :skip (first (:class %)))) classified)
          entries (for [{:keys [sym vmeta class]} active]
                    (case class
                      :predicate  {:type :parametric :entry (gen-predicate-spec sym vmeta ns-name)}
                      :function   {:type :parametric :entry (gen-parametric sym vmeta ns-name)}
                      :macro      {:type :parametric :entry (gen-parametric sym vmeta ns-name)}
                      nil))
          parametrics (vec (keep #(when (= (:type %) :parametric) (:entry %)) entries))
          ;; Nullary tests
          nullaries (vec (keep (fn [{:keys [sym vmeta]}]
                                 (gen-nullary sym vmeta ns-name))
                               active))
          generated (count (filter some? parametrics))]
      {:ns ns-name
       :total (count publics)
       :generated generated
       :skipped (count skipped-vars)
       :skipped-vars (vec skipped-vars)
       :parametric (vec (filter some? parametrics))
       :tests nullaries})
    (catch Exception e
      {:ns ns-name :error (str e) :total 0 :generated 0 :skipped 0
       :skipped-vars [] :parametric [] :tests []})))


(defn ->spec-edn
  "Convert processed namespace to parity-compatible spec format."
  [{:keys [ns parametric tests]}]
  (let [cat (keyword (str (str/replace ns "." ".") ".functions"))]
    (cond-> {:category cat}
      (seq tests)      (assoc :tests (vec tests))
      (seq parametric) (assoc :parametric (vec parametric)))))

;; =============================================================================
;; Output
;; =============================================================================

(defn print-stats [results]
  (println "=== Spec Generation Stats ===\n")
  (let [total-gen (reduce + (map :generated results))
        total-all (reduce + (map :total results))
        all-skipped (mapcat :skipped-vars results)]
    (doseq [{:keys [ns total generated skipped error]} results]
      (if error
        (println (format "  %-35s ERROR: %s" ns error))
        (println (format "  %-35s %4d public  %4d generated  %4d skipped"
                         ns total generated skipped))))
    (println (format "\n  Total: %d vars, %d test groups generated" total-all total-gen))

    ;; Skip report — grouped by reason
    (when (seq all-skipped)
      (let [by-reason (group-by :reason all-skipped)]
        (println (format "\n=== NEED HAND-WRITTEN TESTS (%d vars) ===\n" (count all-skipped)))
        (doseq [[reason-key {:keys [reason]}] (sort-by key skip-reasons)
                :let [vars (get by-reason reason-key)]
                :when (seq vars)]
          (println (format "  %s (%d):" reason (count vars)))
          (println (format "    %s" (str/join ", " (sort (map :var vars)))))
          (println))
        ;; Also show structural skips (not-a-fn, constructor, high-arity)
        (doseq [reason-key [:not-a-fn :constructor :high-arity]
                :let [vars (get by-reason reason-key)]
                :when (seq vars)]
          (let [label (case reason-key
                        :not-a-fn    "Not a function (constants, protocols)"
                        :constructor "Record constructors (->Type, map->Type)"
                        :high-arity  "High arity (>4 args) — reduce manually")]
            (println (format "  %s (%d):" label (count vars)))
            (println (format "    %s" (str/join ", " (sort (map :var vars)))))
            (println)))))))

(defn print-spec [results]
  (println "[")
  (doseq [r results :when (or (seq (:tests r)) (seq (:parametric r)))]
    (let [spec (->spec-edn r)]
      (clojure.pprint/pprint spec)
      (println)))
  (println "]"))

(defn write-specs [results dir]
  (doseq [r results :when (or (seq (:tests r)) (seq (:parametric r)))]
    (let [fname (str (str/replace (:ns r) "." ".") ".gen.edn")
          path (str dir "/" fname)
          spec (->spec-edn r)]
      (io/make-parents path)
      (spit path (with-out-str
                   (println "[")
                   (clojure.pprint/pprint spec)
                   (println "]")))
      (println (format "  wrote %s (%d groups)" path (+ (count (:tests r)) (count (:parametric r))))))))

;; =============================================================================
;; Coverage analysis — host.cljc vs JVM host surface
;; Host data comes from depgraph (--host-edn), not regex.
;; =============================================================================

(defn extract-protocol-methods
  "Parse host.cljc and extract all protocol method names."
  [host-file]
  (let [content (slurp host-file)
        ;; Match (-method-name [h ...] "doc") patterns
        methods (re-seq #"\((-[\w?!*+-]+)\s+\[" content)]
    (vec (distinct (map second methods)))))

(defn extract-defns
  "Extract all defn/def names from a .cljc file."
  [file]
  (let [content (slurp file)
        defs (re-seq #"\(defn?-?\s+([\w?!*+-]+)" content)]
    (vec (distinct (map second defs)))))

(defn extract-host-calls
  "Find all Host protocol method calls in a .cljc file.
   Matches patterns like (-method-name (host) ...) and (host/-method ...)."
  [file]
  (let [content (slurp file)
        ;; Direct protocol calls: (-method-name (host) ...)
        direct (re-seq #"\((-[\w?!*+-]+)\s+\(host\)" content)
        ;; Also match convenience wrappers that call protocol
        wrapper (re-seq #"\((-[\w?!*+-]+)\s+\*host\*" content)]
    (vec (distinct (map second (concat direct wrapper))))))

(defn extract-jvm-refs
  "Find JVM host references in a .cljc file (reader conditionals, interop)."
  [file]
  (let [content (slurp file)
        ;; clojure.lang.* refs
        lang-refs (re-seq #"clojure\.lang\.\w+" content)
        ;; java.* refs
        java-refs (re-seq #"java\.\w+\.\w+" content)
        ;; .method calls
        methods (re-seq #"\(\.\w+" content)
        ;; Class/method statics
        statics (re-seq #"[A-Z]\w+/\w+" content)]
    {:clojure-lang (vec (distinct lang-refs))
     :java (vec (distinct java-refs))
     :methods (vec (distinct methods))
     :statics (vec (distinct statics))}))

(defn analyze-ported-file
  "Analyze a single ported .cljc file."
  [file]
  (let [fname (.getName (io/file file))]
    {:file fname
     :path (str file)
     :ns (str/replace (str/replace fname #"\.cljc$" "") "_" "-")
     :defns (extract-defns file)
     :host-calls (extract-host-calls file)
     :jvm-refs (extract-jvm-refs file)}))

(defn do-coverage
  "Cross-reference host.cljc protocol with ported files and JVM host analysis."
  [ported-dir host-analysis]
  (let [host-file (str ported-dir "/host.cljc")
        _ (when-not (.exists (io/file host-file))
            (println (str "ERROR: " host-file " not found"))
            (System/exit 1))
        ;; 1. Extract Host protocol
        protocol-methods (extract-protocol-methods host-file)

        ;; 2. Scan all .cljc files
        cljc-files (->> (file-seq (io/file ported-dir))
                        (filter #(str/ends-with? (.getName %) ".cljc"))
                        (sort-by #(.getName %)))
        ported (mapv #(analyze-ported-file (.getPath %)) cljc-files)

        ;; 3. Aggregate
        all-ported-defns (into #{} (mapcat :defns) ported)
        all-host-calls (into #{} (mapcat :host-calls) ported)
        unused-protocol (remove (set all-host-calls) protocol-methods)
        unknown-calls (remove (set protocol-methods) all-host-calls)

        ;; 4. Cross-reference with JVM analysis
        ;; For each JVM namespace, check if ported covers the host-dependent vars
        jvm-host-vars (for [{:keys [ns vars-host]} host-analysis
                            {:keys [var]} vars-host]
                        {:ns ns :var var})
        jvm-pure-vars (for [{:keys [ns vars-pure]} host-analysis
                            var vars-pure]
                        {:ns ns :var var})

        ;; Map JVM namespace → ported .cljc file(s)
        ;; pcore.cljc + pmcore.cljc are the portable core (core.cljc is upstream JVM reference)
        ns-to-files {"clojure.core" ["pcore.cljc" "pmcore.cljc"]
                      "clojure.string" ["string.cljc"]
                      "clojure.set" ["set.cljc"]
                      "clojure.walk" ["walk.cljc"]
                      "clojure.data" ["data.cljc"]
                      "clojure.zip" ["zip.cljc"]
                      "clojure.math" ["math.cljc"]
                      "clojure.xml" ["xml.cljc"]
                      "clojure.pprint" ["pprint.cljc"]
                      "clojure.edn" ["edn.cljc"]
                      "clojure.test" ["test.cljc"]
                      "clojure.repl" ["repl.cljc"]
                      "clojure.template" ["template.cljc"]
                      "clojure.stacktrace" ["stacktrace.cljc"]
                      "clojure.datafy" ["datafy.cljc"]
                      "clojure.main" ["main.cljc"]
                      "clojure.java.io" ["io.cljc"]
                      "clojure.java.shell" ["shell.cljc"]
                      "clojure.java.process" ["process.cljc"]
                      "clojure.java.browse" ["browse.cljc"]
                      "clojure.core.async" ["async.cljc"]
                      "clojure.core.protocols" ["protocols.cljc"]
                      "clojure.data.csv" ["csv.cljc"]
                      "clojure.data.json" ["json.cljc"]}
        ported-by-file (into {} (map (fn [p] [(:file p) p])) ported)
        ported-file-set (set (map :file ported))
        ;; Unmapped ported files (extras beyond what JVM namespaces map to)
        mapped-files (into #{} (mapcat val) ns-to-files)
        extra-files (remove mapped-files (map :file ported))]

    ;; === Output ===
    (println "=== HOST COVERAGE ANALYSIS ===")
    (println (format "Host protocol: %s (%d methods)" host-file (count protocol-methods)))
    (println (format "Ported files:  %d .cljc files, %d total defns"
                     (count ported) (count all-ported-defns)))
    (println)

    ;; Protocol method usage
    (println (format "--- HOST PROTOCOL METHODS (%d) ---\n" (count protocol-methods)))
    (println (format "  Used:   %d" (count all-host-calls)))
    (println (format "  Unused: %d" (count unused-protocol)))
    (when (seq unused-protocol)
      (println "  Unused methods:")
      (doseq [m (sort unused-protocol)]
        (println (format "    %s" m))))
    (when (seq unknown-calls)
      (println (format "\n  Unknown calls (not in protocol): %d" (count unknown-calls)))
      (doseq [m (sort unknown-calls)]
        (println (format "    %s" m))))

    ;; Per-file host call breakdown
    (println (format "\n--- HOST CALLS PER FILE ---\n"))
    (doseq [{:keys [file defns host-calls jvm-refs]} (sort-by :file ported)
            :when (or (seq host-calls) (seq (:clojure-lang jvm-refs)))]
      (println (format "  %-25s %3d defns  %2d host calls  %2d JVM refs"
                       file (count defns) (count host-calls)
                       (+ (count (:clojure-lang jvm-refs))
                          (count (:java jvm-refs))))))

    ;; Namespace coverage vs JVM
    (println (format "\n--- NAMESPACE COVERAGE (JVM → ported) ---\n"))
    (println (format "  %-40s %5s %5s %5s %5s  %s" "NAMESPACE" "JVM" "PORT" "HOST?" "COVER" "FILE(S)"))
    (println (str "  " (apply str (repeat 100 "-"))))
    (doseq [{:keys [ns total pure host]} (sort-by :ns host-analysis)
            :when (pos? total)]
      (let [files (get ns-to-files ns)
            port-files (when files (keep ported-by-file files))
            has-port? (seq port-files)
            port-defns (reduce + 0 (map #(count (:defns %)) port-files))
            port-jvm-refs (reduce + 0 (map (fn [p]
                                              (let [jr (:jvm-refs p)]
                                                (+ (count (:clojure-lang jr))
                                                   (count (:java jr)))))
                                            port-files))
            pct (if (and has-port? (pos? total))
                  (format "%d%%" (min 100 (int (* 100.0 (/ (double port-defns) total)))))
                  "—")
            file-str (if files (str/join " + " files) "NOT MAPPED")]
        (println (format "  %-40s %5d %5s %5s %5s  %s"
                         ns total
                         (if has-port? (str port-defns) "—")
                         (if has-port? (str port-jvm-refs) "—")
                         pct
                         file-str))))

    ;; Missing namespaces (JVM has them, no ported file)
    (let [missing (for [{:keys [ns total]} host-analysis
                        :when (and (pos? total)
                                   (not (get ns-to-files ns)))]
                    ns)]
      (when (seq missing)
        (println (format "\n--- NOT YET PORTED (%d namespaces) ---\n" (count missing)))
        (doseq [ns-name (sort missing)]
          (let [info (first (filter #(= (:ns %) ns-name) host-analysis))]
            (println (format "  %-40s %3d vars (%d pure, %d host)"
                             ns-name (:total info) (:pure info) (:host info)))))))

    ;; Extra ported files (not mapped to any JVM namespace)
    (when (seq extra-files)
      (println (format "\n--- EXTRA PORTED FILES (no JVM namespace mapping) ---\n"))
      (doseq [f (sort extra-files)]
        (let [p (get ported-by-file f)
              jr (:jvm-refs p)]
          (println (format "  %-25s %3d defns  %2d host-calls  %2d JVM refs"
                           f (count (:defns p)) (count (:host-calls p))
                           (+ (count (:clojure-lang jr)) (count (:java jr))))))))

    ;; Residual JVM refs in portable files (should be zero for true portability)
    (let [portable-with-refs (for [p ported
                                   :let [jr (:jvm-refs p)
                                         n (+ (count (:clojure-lang jr))
                                              (count (:java jr))
                                              (count (:methods jr))
                                              (count (:statics jr)))]
                                   :when (pos? n)
                                   ;; Skip core.cljc (upstream reference, not portable)
                                   :when (not= (:file p) "core.cljc")]
                               (assoc p :total-jvm-refs n))]
      (when (seq portable-with-refs)
        (println (format "\n--- RESIDUAL JVM REFS IN PORTED FILES (%d files) ---\n"
                         (count portable-with-refs)))
        (doseq [p (sort-by :total-jvm-refs > portable-with-refs)]
          (let [jr (:jvm-refs p)]
            (println (format "  %-25s %3d refs" (:file p) (:total-jvm-refs p)))
            (when (seq (:clojure-lang jr))
              (println (format "    clojure.lang: %s" (str/join ", " (:clojure-lang jr)))))
            (when (seq (:java jr))
              (println (format "    java.*:       %s" (str/join ", " (:java jr)))))
            (when (seq (:statics jr))
              (println (format "    statics:      %s" (str/join ", " (take 10 (:statics jr))))))))))

    ;; Summary
    (let [mapped-ns (count (filter #(get ns-to-files (:ns %)) host-analysis))
          total-ns (count (filter #(pos? (:total %)) host-analysis))
          total-jvm-vars (reduce + (map :total host-analysis))
          total-ported (count all-ported-defns)]
      (println "\n--- SUMMARY ---\n")
      (println (format "  JVM namespaces analyzed:  %d" total-ns))
      (println (format "  Mapped to .cljc files:    %d" mapped-ns))
      (println (format "  JVM public vars:          %d" total-jvm-vars))
      (println (format "  Ported definitions:       %d" total-ported))
      (println (format "  Host protocol methods:    %d (%d used)" (count protocol-methods) (count all-host-calls))))))

;; =============================================================================
;; Main
;; =============================================================================

(def default-namespaces
  "All namespaces that ship with Clojure + spec."
  [;; Core
   "clojure.core"
   "clojure.core.protocols"
   "clojure.core.reducers"
   "clojure.core.server"
   ;; Data
   "clojure.data"
   "clojure.datafy"
   "clojure.edn"
   ;; Standard library
   "clojure.instant"
   "clojure.math"
   "clojure.pprint"
   "clojure.repl"
   "clojure.set"
   "clojure.stacktrace"
   "clojure.string"
   "clojure.template"
   "clojure.test"
   "clojure.walk"
   "clojure.xml"
   "clojure.zip"
   ;; Java interop
   "clojure.java.browse"
   "clojure.java.io"
   "clojure.java.javadoc"
   "clojure.java.process"
   "clojure.java.shell"
   ;; Inspection / reflection
   "clojure.inspector"
   "clojure.main"
   "clojure.reflect"
   ;; Spec (ships separately but is Cognitect)
   "clojure.spec.alpha"
   "clojure.spec.gen.alpha"
   "clojure.spec.test.alpha"])

(def contrib-namespaces
  "Contrib libraries (from deps.edn)."
  [;; Active Cognitect
   "clojure.core.async"
   "clojure.core.cache" "clojure.core.memoize"
   "clojure.data.csv" "clojure.data.json" "clojure.data.xml"
   "clojure.data.zip" "clojure.data.priority-map"
   "clojure.data.int-map" "clojure.data.avl"
   "clojure.java.data" "clojure.java.classpath"
   "clojure.math.combinatorics" "clojure.math.numeric-tower"
   "clojure.test.check" "clojure.test.check.generators"
   "clojure.tools.cli" "clojure.tools.logging"
   "clojure.tools.namespace.find"
   "clojure.tools.reader" "clojure.tools.reader.edn"
   "clojure.tools.trace"
   ;; Stable
   "clojure.core.logic" "clojure.core.match"
   "clojure.core.unify"])

(def all-namespaces
  "All namespaces: shipped + contrib."
  (vec (concat default-namespaces contrib-namespaces)))

(let [args *command-line-args*
      pairs (partition 2 1 args)
      write-dir    (some #(when (= "--write" (first %)) (second %)) pairs)
      coverage-dir (some #(when (= "--coverage" (first %)) (second %)) pairs)
      host-data    (some #(when (= "--host-data" (first %)) (second %)) pairs)
      stats-only   (some #{"--stats"} args)
      ns-args (remove #(str/starts-with? % "--") args)
      ns-args (reduce (fn [a dir] (if dir (remove #{dir} a) a))
                       ns-args [write-dir coverage-dir host-data])
      namespaces (if (seq ns-args) (vec ns-args) all-namespaces)]
  (cond
    coverage-dir
    (let [_ (when-not host-data
              (println "ERROR: --coverage requires --host-data <file.edn>")
              (println "  Generate with: par deps <clojure-src> --host-edn > host.edn")
              (System/exit 1))
          host-results (edn/read-string (slurp host-data))
          _ (binding [*out* *err*]
              (println (format "specgen: loaded %d namespaces from %s" (count host-results) host-data)))]
      (do-coverage coverage-dir host-results))

    :else
    (let [_ (binding [*out* *err*]
              (println (format "specgen: reflecting on %d namespaces..." (count namespaces))))
          results (mapv process-namespace namespaces)]
      (cond
        stats-only  (print-stats results)
        write-dir   (do (print-stats results)
                        (println (format "\nWriting to %s:" write-dir))
                        (write-specs results write-dir))
        :else       (do (print-stats results)
                        (println "\n--- Generated Specs ---\n")
                        (print-spec results))))))
