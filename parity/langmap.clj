#!/usr/bin/env clojure
;; =============================================================================
;; langmap.clj — Clojure host contract via JVM reflection
;;
;; What the JVM provides to Clojure: compiler specials, value protocols,
;; runtime bridge (RT/Numbers/Util), concrete types.
;;
;; For source-level analysis (what .clj code uses), see depgraph.clj.
;;
;; Run:  par discover
;; =============================================================================

(require '[clojure.string :as str]
         '[parity.color :refer [bold dim green yellow cyan red white
                                heading *color*]])

(import '[java.lang.reflect Modifier Method])

;; =============================================================================
;; JVM Reflection
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
;; Output
;; =============================================================================

(defn print-langmap [host]
  (let [ifaces (:interfaces host)
        total-iface-methods (reduce + (map #(count (:methods %)) ifaces))
        bridge-unique (reduce + (map :unique (vals (:bridge host))))
        bridge-total (reduce + (map :total (vals (:bridge host))))
        host-total (+ (count (:specials host)) total-iface-methods bridge-unique)]

    ;; ── TIER 1: SPECIALS ───────────────────────────────────────────────
    (let [sp (:specials host)]
      (println (heading (format "TIER 1  Compiler Specials (%d)" (count sp))))
      (println (str "  " (dim "Hardwired in the compiler. Every host must implement these.")))
      (println)
      (println (str "  " (yellow (str/join ", " sp))))
      (println))

    ;; ── TIER 2: INTERFACES ─────────────────────────────────────────────
    (let [concern-map {"Sequence"    ["ISeq" "Seqable" "Sequential" "IDrop"]
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
      (println (heading (format "TIER 2  Value Protocol (%d interfaces, %d methods)" (count ifaces) total-iface-methods)))
      (println (str "  " (dim "What a Clojure value IS. Types implement the relevant subset.")))
      (println)
      (let [iface-by-name (into {} (map (fn [i] [(:name i) i])) ifaces)]
        (doseq [concern concern-order]
          (let [names (get concern-map concern)
                matching (keep iface-by-name names)]
            (when (seq matching)
              (println (str "  " (bold (white (str concern ":")))))
              (doseq [iface matching]
                (let [ms (:instance iface)
                      method-names (str/join ", " (map :name ms))]
                  (println (format "    %-28s %s  %s" (yellow (:name iface))
                                   (bold (format "%2d" (count ms)))
                                   (dim method-names))))))))))
    (println)

    ;; ── TIER 3: BRIDGE ─────────────────────────────────────────────────
    (let [{:keys [bridge]} host]
      (println (heading (format "TIER 3  Runtime Bridge (%d statics, %d unique)" bridge-total bridge-unique)))
      (println (str "  " (dim "Static methods called by .clj code. Most are type overloads (long/double/Object).")))
      (println)
      (doseq [[cn {:keys [total unique ops]}] (sort-by key bridge)]
        (println (format "  %s  %s" (bold (white cn)) (dim (format "(%d statics, %d unique)" total unique))))
        (let [sorted-ops (sort-by val > ops)
              heavy (take 12 sorted-ops)
              rest-count (- (count sorted-ops) 12)]
          (doseq [[op cnt] heavy]
            (println (format "    %-25s %s" (yellow op) (dim (format "%2d overloads" cnt)))))
          (when (pos? rest-count)
            (println (dim (format "    ... +%d more" rest-count)))))
        (println)))

    ;; ── TIER 4: TYPES ──────────────────────────────────────────────────
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
      (println (heading (format "TIER 4  Concrete Types (%d)" (count concretes))))
      (println (str "  " (dim "The value types genera must provide.")))
      (println)
      (doseq [[label names] groups]
        (let [matching (keep concrete-by-name names)]
          (when (seq matching)
            (println (str "  " (bold (white (str label ":")))))
            (doseq [c matching]
              (println (format "    %-24s %s  %s"
                               (yellow (:name c))
                               (bold (format "%3d methods" (count (:methods c))))
                               (dim (str "implements " (str/join ", " (take 6 (:interfaces c))))))))
            (println)))))

    ;; ── SUMMARY ──────────────────────────────────────────────────────────
    (println (bold (cyan "SUMMARY")))
    (println (dim (apply str (repeat 72 "─"))))
    (println)
    (println (format "  %s things genera must implement at the native level:"
                     (bold (str host-total))))
    (println)
    (println (format "    %-24s %s  %s" "Compiler specials"
                     (bold (format "%4d" (count (:specials host))))
                     (dim "(hardwired compiler primitives)")))
    (println (format "    %-24s %s  %s" "Interface methods"
                     (bold (format "%4d" total-iface-methods))
                     (dim (format "(%d interfaces)" (count ifaces)))))
    (println (format "    %-24s %s  %s" "Bridge ops"
                     (bold (format "%4d" bridge-unique))
                     (dim (format "(%d statics with type overloads)" bridge-total))))
    (println (format "    %-24s %s  %s" "Concrete types"
                     (bold (format "%4d" (count (:concretes host))))
                     (dim "(data structures + infrastructure)")))
    (println)
    (println (dim "  Everything above that is .clj code running ON the host."))
    (println (dim "  Use `par discover --edn` for machine-readable output."))))

;; =============================================================================
;; Main
;; =============================================================================

(binding [*out* *err*] (println "langmap: collecting host data via reflection..."))
(let [host (collect-host)
      edn? (some #{"--edn"} *command-line-args*)
      no-color (some #{"--no-color"} *command-line-args*)]
  (binding [*out* *err*] (println "langmap: host data collected."))
  (binding [*color* (not no-color)]
    (if edn?
      (prn host)
      (print-langmap host))))
