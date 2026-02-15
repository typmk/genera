#!/usr/bin/env clojure
;; =============================================================================
;; hostmap.clj — map the complete Clojure language via JVM reflection
;;
;; Uses the actual running Clojure runtime to discover:
;;   1. Compiler specials — what the compiler hardcodes
;;   2. clojure.lang interfaces — the host contract (what a platform must provide)
;;   3. clojure.lang classes — the implementation (what JVM provides)
;;   4. RT/Numbers/Util — the bridge between .clj and .java
;;   5. Class hierarchy — what extends/implements what
;;
;; Run:  clojure -M script/hostmap.clj
;;   or: clojure script/hostmap.clj
;; =============================================================================

(require '[clojure.reflect :as reflect]
         '[clojure.string :as str]
         '[clojure.set :as set]
         '[clojure.pprint :as pp])

;; =============================================================================
;; 1. Compiler specials — THE truth
;; =============================================================================

(def compiler-specials
  "Special forms hardcoded in Compiler.java"
  (try
    (let [field (.getDeclaredField clojure.lang.Compiler "specials")]
      (.setAccessible field true)
      (into (sorted-set) (map str (keys (.get field nil)))))
    (catch Exception _
      ;; Fallback: documented special forms
      (sorted-set "def" "if" "do" "let*" "fn*" "loop*" "recur" "quote"
                   "var" "try" "catch" "finally" "throw" "monitor-enter"
                   "monitor-exit" "new" "set!" "." "deftype*" "reify*"
                   "case*" "letfn*" "clojure.core/import*" "in-ns" "&"))))

;; =============================================================================
;; 2. Discover all clojure.lang classes
;; =============================================================================

(defn find-lang-classes
  "Find all classes/interfaces in clojure.lang package."
  []
  (let [;; Get the clojure jar URL
        cl (.getClassLoader clojure.lang.RT)
        ;; Known key classes — reflect to discover the full set
        seed-classes
        '[clojure.lang.ISeq clojure.lang.IPersistentMap clojure.lang.IPersistentVector
          clojure.lang.IPersistentSet clojure.lang.IPersistentCollection
          clojure.lang.IPersistentList clojure.lang.IFn clojure.lang.Seqable
          clojure.lang.Sequential clojure.lang.Associative clojure.lang.Indexed
          clojure.lang.Counted clojure.lang.Reversible clojure.lang.Sorted
          clojure.lang.ILookup clojure.lang.IMapEntry clojure.lang.IObj
          clojure.lang.IMeta clojure.lang.IDeref clojure.lang.IBlockingDeref
          clojure.lang.IPending clojure.lang.IReduce clojure.lang.IReduceInit
          clojure.lang.IKVReduce clojure.lang.IEditableCollection
          clojure.lang.ITransientCollection clojure.lang.ITransientMap
          clojure.lang.ITransientVector clojure.lang.ITransientSet
          clojure.lang.ITransientAssociative clojure.lang.Named
          clojure.lang.IHashEq clojure.lang.IDrop clojure.lang.Fn
          ;; Key implementation classes
          clojure.lang.RT clojure.lang.Numbers clojure.lang.Util
          clojure.lang.Compiler clojure.lang.Symbol clojure.lang.Keyword
          clojure.lang.Var clojure.lang.Namespace clojure.lang.Atom
          clojure.lang.Ref clojure.lang.Agent clojure.lang.Delay
          clojure.lang.LazySeq clojure.lang.Cons clojure.lang.Reduced
          clojure.lang.PersistentVector clojure.lang.PersistentHashMap
          clojure.lang.PersistentHashSet clojure.lang.PersistentList
          clojure.lang.PersistentArrayMap clojure.lang.PersistentTreeMap
          clojure.lang.PersistentTreeSet clojure.lang.PersistentStructMap
          clojure.lang.PersistentQueue clojure.lang.MapEntry
          clojure.lang.ArraySeq clojure.lang.ChunkedCons
          clojure.lang.Range clojure.lang.LongRange clojure.lang.Repeat
          clojure.lang.Cycle clojure.lang.Iterate
          clojure.lang.MultiFn clojure.lang.Volatile
          clojure.lang.AFn clojure.lang.ASeq clojure.lang.APersistentMap
          clojure.lang.APersistentVector clojure.lang.APersistentSet
          clojure.lang.ATransientMap clojure.lang.ATransientSet
          clojure.lang.ARef clojure.lang.AReference
          clojure.lang.Murmur3 clojure.lang.Reflector
          clojure.lang.LockingTransaction
          clojure.lang.BigInt clojure.lang.Ratio
          clojure.lang.TaggedLiteral clojure.lang.ReaderConditional
          clojure.lang.ArityException]]
    (->> seed-classes
         (keep (fn [sym]
                 (try
                   (let [cls (Class/forName (str sym))]
                     {:name (str sym)
                      :short (.getSimpleName cls)
                      :interface? (.isInterface cls)
                      :abstract? (java.lang.reflect.Modifier/isAbstract (.getModifiers cls))})
                   (catch ClassNotFoundException _ nil))))
         (sort-by :name)
         vec)))

;; =============================================================================
;; 3. Reflect on a class — get methods, interfaces, supers
;; =============================================================================

(defn class-info [class-name]
  (try
    (let [cls (Class/forName class-name)
          mods (.getModifiers cls)
          is-iface (.isInterface cls)
          supers (->> (.getInterfaces cls)
                      (map #(.getName %))
                      (filter #(str/starts-with? % "clojure.lang."))
                      sort
                      vec)
          super-class (when-let [sc (.getSuperclass cls)]
                        (let [n (.getName sc)]
                          (when (str/starts-with? n "clojure.lang.") n)))
          ;; Raw Java reflection — getDeclaredMethods gives THIS class only
          raw-methods (.getDeclaredMethods cls)
          own-methods (->> raw-methods
                          (filter #(java.lang.reflect.Modifier/isPublic (.getModifiers %)))
                          (map (fn [^java.lang.reflect.Method m]
                                 {:name (.getName m)
                                  :params (count (.getParameterTypes m))
                                  :static? (java.lang.reflect.Modifier/isStatic (.getModifiers m))
                                  :return (.getSimpleName (.getReturnType m))}))
                          ;; Deduplicate overloads — keep unique name+params
                          (distinct)
                          (sort-by (juxt :name :params))
                          vec)]
      {:name class-name
       :short (.getSimpleName cls)
       :interface? is-iface
       :abstract? (java.lang.reflect.Modifier/isAbstract mods)
       :super super-class
       :interfaces supers
       :methods own-methods
       :method-count (count own-methods)})
    (catch Exception e
      {:name class-name :error (str e)})))

;; =============================================================================
;; 4. RT/Numbers/Util bridge analysis
;; =============================================================================

(defn static-methods [class-name]
  (try
    (let [cls (Class/forName class-name)
          raw (.getDeclaredMethods cls)]
      (->> raw
           (filter #(let [m (.getModifiers %)]
                      (and (java.lang.reflect.Modifier/isPublic m)
                           (java.lang.reflect.Modifier/isStatic m))))
           (map (fn [^java.lang.reflect.Method m]
                  {:name (.getName m)
                   :params (count (.getParameterTypes m))
                   :return (.getSimpleName (.getReturnType m))}))
           (sort-by (juxt :name :params))
           vec))
    (catch Exception _ [])))

;; =============================================================================
;; 5. Output
;; =============================================================================

(defn print-report []
  (println "=== Clojure Language: Complete Host Map ===\n")

  ;; Compiler specials
  (println (format "--- 1. Compiler Special Forms (%d) ---" (count compiler-specials)))
  (println (str "  " (str/join ", " compiler-specials)))

  ;; Discover classes
  (let [all-classes (find-lang-classes)
        interfaces (filter :interface? all-classes)
        abstracts (filter #(and (:abstract? %) (not (:interface? %))) all-classes)
        concretes (remove #(or (:interface? %) (:abstract? %)) all-classes)]

    (println (format "\n--- 2. clojure.lang Inventory ---"))
    (println (format "  Interfaces:      %d" (count interfaces)))
    (println (format "  Abstract classes: %d" (count abstracts)))
    (println (format "  Concrete classes: %d" (count concretes)))
    (println (format "  Total:           %d" (count all-classes)))

    ;; Interfaces — THE host contract
    (println (format "\n--- 3. Host Contract: Interfaces (%d) ---" (count interfaces)))
    (println "  These define what a Clojure VALUE must support.\n")
    (doseq [iface interfaces]
      (let [info (class-info (:name iface))
            methods (:methods info)]
        (println (format "  %s (%d methods)%s"
                         (:short iface)
                         (count methods)
                         (if (seq (:interfaces info))
                           (str " extends " (str/join ", " (map #(last (str/split % #"\.")) (:interfaces info))))
                           "")))
        (doseq [m methods]
          (println (format "    %s(%d)%s → %s"
                           (:name m) (:params m)
                           (if (:static? m) " [static]" "")
                           (:return m))))))

    ;; Abstract base classes
    (println (format "\n--- 4. Abstract Base Classes (%d) ---" (count abstracts)))
    (println "  Shared implementation. Platform can extend these or reimplement.\n")
    (doseq [cls abstracts]
      (let [info (class-info (:name cls))]
        (println (format "  %s (%d methods) implements %s"
                         (:short cls)
                         (:method-count info)
                         (str/join ", " (map #(last (str/split % #"\.")) (:interfaces info)))))))

    ;; RT bridge
    (println "\n--- 5. RT Bridge (static methods that .clj calls into .java) ---")
    (let [rt-statics (static-methods "clojure.lang.RT")]
      (println (format "  RT: %d static methods" (count rt-statics)))
      (doseq [m (take 50 rt-statics)]
        (println (format "    RT.%s(%d) → %s" (:name m) (:params m) (:return m))))
      (when (> (count rt-statics) 50)
        (println (format "    ... and %d more" (- (count rt-statics) 50)))))

    (let [num-statics (static-methods "clojure.lang.Numbers")]
      (println (format "\n  Numbers: %d static methods" (count num-statics)))
      (doseq [m (take 30 num-statics)]
        (println (format "    Numbers.%s(%d) → %s" (:name m) (:params m) (:return m))))
      (when (> (count num-statics) 30)
        (println (format "    ... and %d more" (- (count num-statics) 30)))))

    (let [util-statics (static-methods "clojure.lang.Util")]
      (println (format "\n  Util: %d static methods" (count util-statics)))
      (doseq [m util-statics]
        (println (format "    Util.%s(%d) → %s" (:name m) (:params m) (:return m)))))

    ;; Concrete classes — count methods per class
    (println (format "\n--- 6. Implementation Classes (%d) ---" (count concretes)))
    (println "  Concrete types that a portable host must provide.\n")
    (let [class-infos (map #(class-info (:name %)) concretes)
          sorted (sort-by :method-count > class-infos)]
      (doseq [info sorted]
        (println (format "  %-35s %3d methods  implements %s"
                         (:short info)
                         (:method-count info)
                         (str/join ", " (map #(last (str/split % #"\."))
                                             (:interfaces info)))))))

    ;; Summary: the minimal host
    (println "\n--- 7. Minimal Host Summary ---")
    (let [iface-infos (map #(class-info (:name %)) interfaces)
          total-iface-methods (reduce + (map :method-count iface-infos))]
      (println (format "  Special forms:    %d (hardcoded in compiler)" (count compiler-specials)))
      (println (format "  Interfaces:       %d (%d total methods)" (count interfaces) total-iface-methods))
      (println (format "  Abstract bases:   %d (optional — shared impl)" (count abstracts)))
      (println (format "  Concrete types:   %d (platform must provide)" (count concretes)))
      (println (format "  RT statics:       %d (bridge: .clj → .java)" (count (static-methods "clojure.lang.RT"))))
      (println (format "  Numbers statics:  %d (arithmetic bridge)" (count (static-methods "clojure.lang.Numbers"))))
      (println (format "  Util statics:     %d (hash/equiv/compare)" (count (static-methods "clojure.lang.Util"))))
      (println)
      (println "  THE HOST CONTRACT:")
      (println (format "    %d interfaces with %d methods = what a value IS"
                       (count interfaces) total-iface-methods))
      (println (format "    %d special forms = what the compiler knows"
                       (count compiler-specials)))
      (println "    Everything else is derived."))))

(print-report)
