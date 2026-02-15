#!/usr/bin/env clojure
;; =============================================================================
;; langmap.clj — unified Clojure language map
;;
;; Combines JVM reflection + source analysis into one complete picture.
;; Shows everything from compiler specials down to convenience functions.
;;
;; Run:  clojure script/langmap.clj                        # host-only (reflection)
;;       clojure script/langmap.clj src/clj/clojure         # + genera portable core
;;       clojure script/langmap.clj /mnt/c/Proj/genera/ref/clojure/src/clj  # + full JVM
;; =============================================================================

(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(import '[java.lang.reflect Modifier Method]
        '[java.io PushbackReader])

;; =============================================================================
;; 1. JVM Reflection — what the host provides
;; =============================================================================

(def compiler-specials
  (try
    (let [f (.getDeclaredField clojure.lang.Compiler "specials")]
      (.setAccessible f true)
      (into (sorted-set) (map str (keys (.get f nil)))))
    (catch Exception _
      (sorted-set "def" "if" "do" "let*" "fn*" "loop*" "recur" "quote"
                   "var" "try" "catch" "finally" "throw" "monitor-enter"
                   "monitor-exit" "new" "set!" "." "deftype*" "reify*"
                   "case*" "letfn*" "clojure.core/import*" "in-ns" "&"))))

(def seed-classes
  '{:interfaces
    [ISeq IPersistentMap IPersistentVector IPersistentSet IPersistentCollection
     IPersistentList IFn Seqable Sequential Associative Indexed Counted
     Reversible Sorted ILookup IMapEntry IObj IMeta IDeref IBlockingDeref
     IPending IReduce IReduceInit IKVReduce IEditableCollection
     ITransientCollection ITransientMap ITransientVector ITransientSet
     ITransientAssociative Named IHashEq IDrop Fn]
    :abstracts
    [AFn ASeq APersistentMap APersistentVector APersistentSet
     ATransientMap ATransientSet ARef AReference]
    :concretes
    [RT Numbers Util Compiler Symbol Keyword Var Namespace Atom
     Ref Agent Delay LazySeq Cons Reduced PersistentVector
     PersistentHashMap PersistentHashSet PersistentList
     PersistentArrayMap PersistentTreeMap PersistentTreeSet
     PersistentStructMap PersistentQueue MapEntry ArraySeq ChunkedCons
     Range LongRange Repeat Cycle Iterate MultiFn Volatile
     Murmur3 Reflector LockingTransaction BigInt Ratio
     TaggedLiteral ReaderConditional ArityException]})

(defn resolve-class [short-name]
  (try (Class/forName (str "clojure.lang." short-name))
       (catch ClassNotFoundException _ nil)))

(defn public-methods [^Class cls]
  (->> (.getDeclaredMethods cls)
       (filter #(Modifier/isPublic (.getModifiers ^Method %)))
       (map (fn [^Method m]
              {:name (.getName m)
               :arity (count (.getParameterTypes m))
               :static? (Modifier/isStatic (.getModifiers m))
               :return (.getSimpleName (.getReturnType m))}))
       (sort-by (juxt :name :arity))
       distinct vec))

(defn class-entry [^Class cls]
  (let [ifaces (->> (.getInterfaces cls)
                    (map #(.getName %))
                    (filter #(str/starts-with? % "clojure.lang."))
                    (map #(last (str/split % #"\.")))
                    sort vec)
        super (when-let [sc (.getSuperclass cls)]
                (let [n (.getName sc)]
                  (when (str/starts-with? n "clojure.lang.")
                    (last (str/split n #"\.")))))]
    {:name (.getSimpleName cls)
     :fqn (.getName cls)
     :interface? (.isInterface cls)
     :abstract? (Modifier/isAbstract (.getModifiers cls))
     :super super
     :interfaces ifaces
     :methods (public-methods cls)
     :statics (filterv :static? (public-methods cls))
     :instance (filterv (complement :static?) (public-methods cls))}))

(defn collect-host []
  (let [resolve-all (fn [names]
                      (->> names
                           (keep #(when-let [c (resolve-class (str %))]
                                    (class-entry c)))
                           (sort-by :name) vec))
        ifaces (resolve-all (:interfaces seed-classes))
        abstracts (resolve-all (:abstracts seed-classes))
        concretes (resolve-all (:concretes seed-classes))
        bridge (into {}
                 (for [cn ["RT" "Numbers" "Util"]
                       :let [cls (resolve-class cn)
                             statics (when cls (filterv :static? (public-methods cls)))
                             by-name (group-by :name statics)]]
                   [cn {:total (count statics)
                        :unique (count by-name)
                        :ops (into (sorted-map)
                                   (map (fn [[n ms]] [n (count ms)]))
                                   by-name)}]))]
    {:specials compiler-specials
     :interfaces ifaces
     :abstracts abstracts
     :concretes concretes
     :bridge bridge}))

;; =============================================================================
;; 2. Source analysis — what .clj defines
;; =============================================================================

(defn find-clj-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(and (.isFile %)
                     (let [n (.getName %)]
                       (or (str/ends-with? n ".clj")
                           (str/ends-with? n ".cljc")))))
       (sort-by #(.getPath %)) vec))

(defn read-forms [file]
  (try
    (with-open [rdr (PushbackReader. (io/reader file))]
      (binding [*read-eval* false
                *data-readers* (assoc *data-readers*
                                      'inst identity 'uuid identity)]
        (let [eof (Object.)
              opts {:eof eof :read-cond :allow :features #{:clj}}]
          (loop [forms [] errors 0]
            (if (> errors 50) forms  ;; bail on too many errors
                (let [form (try (read opts rdr)
                               (catch Exception _ ::skip))]
                  (cond
                    (identical? form eof) forms
                    (= form ::skip)       (recur forms (inc errors))
                    :else                 (recur (conj forms form) errors))))))))
    (catch Exception _ [])))

(defn def-form? [form]
  (and (seq? form) (symbol? (first form))
       (#{"def" "defn" "defn-" "defmacro" "defonce"
          "defprotocol" "defmulti" "deftype" "defrecord"
          "definterface" "definline"} (name (first form)))
       (>= (count form) 2) (symbol? (second form))))

(defn collect-symbols [form]
  (cond
    (symbol? form) #{(str form)}
    (seq? form)    (into #{} (mapcat collect-symbols) form)
    (vector? form) (into #{} (mapcat collect-symbols) form)
    (map? form)    (into #{} (mapcat collect-symbols) (concat (keys form) (vals form)))
    (set? form)    (into #{} (mapcat collect-symbols) form)
    :else          #{}))

(defn host-ref? [s]
  (or (str/starts-with? s "clojure.lang.")
      (str/starts-with? s "java.")
      (and (str/starts-with? s ".") (> (count s) 1) (not (str/starts-with? s "..")))
      (when-let [prefix (first (str/split s #"/"))]
        (#{"RT" "Numbers" "Util"} prefix))))

(defn body-symbols [form]
  (let [parts (drop 2 form)
        parts (if (string? (first parts)) (rest parts) parts)
        parts (if (map? (first parts)) (rest parts) parts)]
    (collect-symbols (vec parts))))

(defn analyze-file [file]
  (let [forms (read-forms file)
        defs (for [f forms :when (def-form? f)]
               (let [nm (name (second f))
                     syms (body-symbols f)
                     host (into (sorted-set) (filter host-ref?) syms)]
                 {:name nm :kind (keyword (name (first f)))
                  :sym-count (count syms) :host-refs host}))]
    {:file (.getName file) :path (.getPath file)
     :forms (count forms) :defs (vec defs)}))

;; Layer computation — topological sort
(defn compute-layers [all-files]
  (let [all-defs (mapcat :defs all-files)
        def-names (into #{} (map :name) all-defs)
        ;; For each def: which other defs does it reference?
        deps-map (into {}
                   (for [d all-defs]
                     [(:name d)
                      (let [syms (disj (into #{} (map #(str/replace % #"^.*/" ""))
                                             (remove host-ref?
                                                     (map str (keys (zipmap (range) (range))))))
                                       (:name d))]
                        ;; We don't have body symbols stored, so use sym-count as proxy
                        ;; Better: re-extract. But for layer estimation, use host-ref count
                        (if (seq (:host-refs d)) #{:host} #{}))]))
        ;; Simple classification: layer 0 = no host refs and few deps
        ;; layer 1+ = has host refs or many deps
        ]
    ;; For now, classify by host dependency
    (into {} (for [d all-defs]
               [(:name d) (if (seq (:host-refs d)) :host-dependent :pure)]))))

(defn analyze-sources [dir]
  (let [files (find-clj-files dir)]
    (binding [*out* *err*]
      (println (format "langmap: scanning %d files in %s" (count files) dir)))
    (let [results (mapv analyze-file files)
          all-defs (vec (mapcat :defs results))
          host-using (filterv #(seq (:host-refs %)) all-defs)
          pure (filterv #(empty? (:host-refs %)) all-defs)
          all-host (into (sorted-set) (mapcat :host-refs) all-defs)
          ;; Classify host refs
          lang-refs (into (sorted-set) (filter #(str/starts-with? % "clojure.lang.")) all-host)
          java-refs (into (sorted-set) (filter #(str/starts-with? % "java.")) all-host)
          interop (into (sorted-set) (filter #(str/starts-with? % ".")) all-host)
          bridge-refs (into (sorted-set) (filter #(re-matches #"^(RT|Numbers|Util)/.*" %)) all-host)]
      {:files results :total-defs (count all-defs) :total-files (count files)
       :host-using (count host-using) :pure (count pure)
       :lang-refs lang-refs :java-refs java-refs
       :interop interop :bridge-refs bridge-refs})))

;; =============================================================================
;; 3. Unified output
;; =============================================================================

(defn print-langmap [host source]
  (println "=== CLOJURE LANGUAGE MAP ===")
  (println "From compiler primitives to convenience. What genera must implement.\n")

  ;; ─── TIER 1: SPECIALS ─────────────────────────────────────────────
  (let [sp (:specials host)]
    (println (format "TIER 1 — COMPILER SPECIALS (%d)" (count sp)))
    (println "  Hardwired. Every host must implement these.\n")
    (println (str "  " (str/join ", " sp)))
    (println))

  ;; ─── TIER 2: INTERFACES ───────────────────────────────────────────
  (let [ifaces (:interfaces host)
        total-methods (reduce + (map #(count (:methods %)) ifaces))
        ;; Group by concern
        concern-map {"Sequence"    ["ISeq" "Seqable" "Sequential" "IDrop"]
                     "Collection"  ["IPersistentCollection" "Counted" "IEditableCollection"]
                     "Associative" ["Associative" "IPersistentMap" "ILookup" "IMapEntry" "IKVReduce"]
                     "Indexed"     ["IPersistentVector" "Indexed" "Reversible"]
                     "Set"         ["IPersistentSet" "Sorted"]
                     "List"        ["IPersistentList"]
                     "Function"    ["IFn" "Fn"]
                     "Metadata"    ["IObj" "IMeta"]
                     "Reference"   ["IDeref" "IBlockingDeref" "IPending"]
                     "Reduction"   ["IReduce" "IReduceInit"]
                     "Transient"   ["ITransientCollection" "ITransientMap" "ITransientVector"
                                    "ITransientSet" "ITransientAssociative"]
                     "Identity"    ["Named" "IHashEq"]}
        concern-order ["Sequence" "Collection" "Associative" "Indexed" "Set" "List"
                       "Function" "Metadata" "Reference" "Reduction" "Transient" "Identity"]]
    (println (format "TIER 2 — VALUE PROTOCOL (%d interfaces, %d total methods)" (count ifaces) total-methods))
    (println "  What a Clojure value IS. Types implement the relevant subset.\n")
    (let [iface-by-name (into {} (map (fn [i] [(:name i) i])) ifaces)]
      (doseq [concern concern-order]
        (let [names (get concern-map concern)
              matching (keep iface-by-name names)]
          (when (seq matching)
            (println (format "  %s:" concern))
            (doseq [iface matching]
              (let [ms (:instance iface)
                    method-names (str/join ", " (map :name ms))]
                (println (format "    %-28s %2d  %s" (:name iface) (count ms) method-names))))))))
    (println))

  ;; ─── TIER 3: BRIDGE ───────────────────────────────────────────────
  (let [{:keys [bridge]} host
        total (reduce + (map :total (vals bridge)))
        unique (reduce + (map :unique (vals bridge)))]
    (println (format "TIER 3 — RUNTIME BRIDGE (%d statics, %d unique operations)" total unique))
    (println "  .clj calls these. Most are type overloads (long/double/Object).\n")
    (doseq [[cn {:keys [total unique ops]}] (sort-by key bridge)]
      (println (format "  %s — %d statics, %d unique:" cn total unique))
      (let [sorted-ops (sort-by val > ops)
            heavy (take 15 sorted-ops)
            rest-count (- (count sorted-ops) 15)]
        (doseq [[op cnt] heavy]
          (println (format "    %-25s %2d overloads" op cnt)))
        (when (pos? rest-count)
          (println (format "    ... and %d more operations" rest-count))))
      (println)))

  ;; ─── TIER 4: TYPES ────────────────────────────────────────────────
  (let [concretes (:concretes host)
        groups [["Data structures"
                 ["PersistentVector" "PersistentHashMap" "PersistentHashSet"
                  "PersistentList" "PersistentArrayMap" "PersistentTreeMap"
                  "PersistentTreeSet" "PersistentStructMap" "PersistentQueue" "MapEntry"]]
                ["Sequences"
                 ["LazySeq" "Cons" "ArraySeq" "ChunkedCons"
                  "Range" "LongRange" "Repeat" "Cycle" "Iterate"]]
                ["References"
                 ["Var" "Atom" "Ref" "Agent" "Delay" "Volatile" "Reduced"]]
                ["Names"
                 ["Symbol" "Keyword" "Namespace"]]
                ["Numerics"
                 ["BigInt" "Ratio"]]
                ["Infrastructure"
                 ["RT" "Numbers" "Util" "Compiler" "MultiFn"
                  "Murmur3" "Reflector" "LockingTransaction"
                  "TaggedLiteral" "ReaderConditional" "ArityException"]]]
        concrete-by-name (into {} (map (fn [c] [(:name c) c])) concretes)]
    (println (format "TIER 4 — CONCRETE TYPES (%d)" (count concretes)))
    (println "  The value types genera must provide.\n")
    (doseq [[label names] groups]
      (let [matching (keep concrete-by-name names)]
        (when (seq matching)
          (println (format "  %s:" label))
          (doseq [c matching]
            (println (format "    %-28s %3d methods  implements %s"
                             (:name c)
                             (count (:methods c))
                             (str/join ", " (take 6 (:interfaces c))))))
          (println)))))

  ;; ─── TIER 5: CLJ SOURCE ───────────────────────────────────────────
  (when source
    (println (format "TIER 5 — CLJ SOURCE (%d definitions, %d files)"
                     (:total-defs source) (:total-files source)))
    (println (format "  Pure (no host refs):  %d  (portable as-is)" (:pure source)))
    (println (format "  Host-dependent:       %d  (need platform bridge)" (:host-using source)))
    (println)

    ;; Per-file breakdown
    (println "  Per-file:")
    (doseq [{:keys [file defs]} (:files source)
            :when (seq defs)]
      (let [n-pure (count (remove #(seq (:host-refs %)) defs))
            n-host (count (filter #(seq (:host-refs %)) defs))]
        (println (format "    %-40s %4d defs  %4d pure  %4d host"
                         file (count defs) n-pure n-host))))
    (println)

    ;; Host surface from .clj perspective
    (println "  Host surface referenced by .clj code:")
    (println (format "    clojure.lang.* refs:  %d unique" (count (:lang-refs source))))
    (println (format "    java.* refs:          %d unique" (count (:java-refs source))))
    (println (format "    Interop (.method):    %d unique" (count (:interop source))))
    (println (format "    Bridge (RT/Num/Util): %d unique" (count (:bridge-refs source))))
    (println)

    (when (seq (:lang-refs source))
      (println "    clojure.lang references:")
      (doseq [r (take 40 (:lang-refs source))]
        (println (format "      %s" r)))
      (when (> (count (:lang-refs source)) 40)
        (println (format "      ... and %d more" (- (count (:lang-refs source)) 40))))
      (println))

    (when (seq (:bridge-refs source))
      (println "    Bridge references:")
      (doseq [r (take 30 (:bridge-refs source))]
        (println (format "      %s" r)))
      (when (> (count (:bridge-refs source)) 30)
        (println (format "      ... and %d more" (- (count (:bridge-refs source)) 30))))
      (println)))

  ;; ─── SUMMARY ──────────────────────────────────────────────────────
  (println "=== SUMMARY ===\n")
  (let [ifaces (:interfaces host)
        total-iface-methods (reduce + (map #(count (:methods %)) ifaces))
        bridge-unique (reduce + (map :unique (vals (:bridge host))))
        host-total (+ (count (:specials host)) total-iface-methods bridge-unique)]
    (println (format "  %-22s %4d" "Compiler specials:" (count (:specials host))))
    (println (format "  %-22s %4d  (%d interfaces)" "Interface methods:" total-iface-methods (count ifaces)))
    (println (format "  %-22s %4d  (unique operations)" "Bridge ops:" bridge-unique))
    (println (format "  %-22s %4d" "Concrete types:" (count (:concretes host))))
    (when source
      (println (format "  %-22s %4d  (%d pure + %d host)"
                       "CLJ definitions:" (:total-defs source) (:pure source) (:host-using source))))
    (println)
    (println (format "  GENERA HOST = %d specials + %d interface methods + %d bridge ops"
                     (count (:specials host)) total-iface-methods bridge-unique))
    (println (format "             = %d things to implement at the native level" host-total))
    (println "  Everything above that is .clj code running ON the host.")))

;; =============================================================================
;; Main
;; =============================================================================

(let [args *command-line-args*
      src-dir (first (remove #(str/starts-with? % "--") args))
      _ (binding [*out* *err*] (println "langmap: collecting host data via reflection..."))
      host (collect-host)
      _ (binding [*out* *err*] (println "langmap: host data collected."))
      source (when src-dir (analyze-sources src-dir))]
  (print-langmap host source))
