#!/usr/bin/env bb
;; =============================================================================
;; depgraph.clj — full dependency graph of the Clojure language
;;
;; Scans ALL .clj files in a source tree. For each file:
;;   - Extracts definitions (defn, defmacro, def, defprotocol, ...)
;;   - Collects symbol references in each body
;;   - Resolves deps against the full cross-file definition set
;;   - Catalogs every clojure.lang.* and java.* reference (the host contract)
;;
;; Output:
;;   - Per-namespace breakdown
;;   - Cross-namespace dependency edges
;;   - Host contract: which Java classes are actually needed
;;   - Layer classification (foundation → core → derived → leaf)
;;
;; Usage:
;;   bb script/depgraph.clj /path/to/clojure/src/clj    # full language
;;   bb script/depgraph.clj src/clj/clojure/core.cljc   # single file
;;   bb script/depgraph.clj /path --edn                  # EDN output
;;   bb script/depgraph.clj /path --dot                  # DOT output
;;   bb script/depgraph.clj /path --host                 # host contract only
;; =============================================================================

(ns depgraph
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure.string :as str]
            [clojure.set :as set]
            [babashka.fs :as fs]))

;; Clojure special forms — compiler primitives
(def special-forms
  '#{def if do let let* fn fn* loop loop* recur quote var
     try catch finally throw monitor-enter monitor-exit
     new set! . .. deftype* reify* case* letfn* clojure.core/import*
     in-ns ns})

;; =============================================================================
;; File discovery
;; =============================================================================

(defn find-clj-files
  "Find all .clj files under a directory."
  [root]
  (->> (fs/glob root "**/*.clj")
       (map str)
       sort
       vec))

;; =============================================================================
;; Extract definitions from a single file
;; =============================================================================

(defn extract-defs
  "Walk top-level forms, extract all def-like names."
  [zloc]
  (loop [loc (z/down zloc)
         defs []]
    (if (nil? loc)
      defs
      (let [form (try (z/sexpr loc) (catch Exception _ nil))
            def-info
            (when (and (list? form) (symbol? (first form)))
              (let [head (first form)
                    head-name (name head)]
                (when (and (#{"def" "defn" "defn-" "defmacro" "defonce"
                              "defprotocol" "defmulti" "deftype" "defrecord"
                              "definterface"} head-name)
                           (symbol? (second form)))
                  {:name (name (second form))
                   :kind (keyword head-name)
                   :form form})))]
        (recur (z/right loc)
               (if def-info (conj defs def-info) defs))))))

;; =============================================================================
;; Symbol collection
;; =============================================================================

(defn collect-symbols
  "Recursively collect all symbols (as strings) referenced in a form."
  [form]
  (cond
    (symbol? form) #{(str form)}  ;; preserve namespace: clojure.lang.RT/conj
    (seq? form)    (into #{} (mapcat collect-symbols) form)
    (vector? form) (into #{} (mapcat collect-symbols) form)
    (map? form)    (into #{} (mapcat collect-symbols) (concat (keys form) (vals form)))
    (set? form)    (into #{} (mapcat collect-symbols) form)
    :else          #{}))

(defn extract-body-symbols [form]
  (let [head-name (name (first form))
        parts (rest form)
        parts (rest parts)  ;; drop name
        parts (if (string? (first parts)) (rest parts) parts)
        parts (if (map? (first parts)) (rest parts) parts)]
    (cond
      (#{"defn" "defn-" "defmacro"} head-name) (collect-symbols (vec parts))
      (#{"def" "defonce"} head-name)            (collect-symbols (vec parts))
      (#{"defmulti"} head-name)                 (collect-symbols (vec parts))
      (#{"defprotocol" "definterface"} head-name) #{}
      :else (collect-symbols (vec parts)))))

;; =============================================================================
;; Host reference analysis
;; =============================================================================

(defn parse-host-ref
  "Parse a symbol string into host reference info, or nil."
  [sym-str]
  (cond
    ;; clojure.lang.RT/conj → {:class "RT" :member "conj" :pkg "clojure.lang"}
    (str/starts-with? sym-str "clojure.lang.")
    (let [rest-str (subs sym-str (count "clojure.lang."))
          [class member] (str/split rest-str #"/" 2)]
      {:pkg "clojure.lang" :class class :member member :raw sym-str})

    ;; java.lang.String → {:class "String" :pkg "java.lang"}
    (str/starts-with? sym-str "java.")
    (let [parts (str/split sym-str #"\.")
          pkg (str/join "." (butlast parts))
          class-and-member (last parts)
          [class member] (if (str/includes? (or (last parts) "") "/")
                           (str/split class-and-member #"/" 2)
                           [class-and-member nil])]
      {:pkg pkg :class class :member member :raw sym-str})

    ;; .methodName → interop method call
    (and (str/starts-with? sym-str ".")
         (not= "." sym-str)
         (not (str/starts-with? sym-str "..")))
    {:pkg :interop :class :method :member (subs sym-str 1) :raw sym-str}

    ;; RT/ Numbers/ Util/ — shorthand
    (or (str/starts-with? sym-str "RT/")
        (str/starts-with? sym-str "Numbers/")
        (str/starts-with? sym-str "Util/"))
    (let [[class member] (str/split sym-str #"/" 2)]
      {:pkg "clojure.lang" :class class :member member :raw sym-str})

    :else nil))

(defn host-ref? [sym-str] (some? (parse-host-ref sym-str)))

;; =============================================================================
;; Process a single file
;; =============================================================================

(defn ns-from-path
  "Derive namespace from file path relative to src root."
  [path root]
  (let [rel (str/replace (subs path (count root)) #"^/" "")
        rel (str/replace rel #"\.clj$" "")]
    (-> rel
        (str/replace "/" ".")
        (str/replace "_" "-"))))

(defn process-file
  "Parse one .clj file. Returns {:ns _ :defs [...] :parse-error? bool}."
  [path root]
  (let [ns-name (ns-from-path path root)
        content (slurp path)]
    (try
      (let [wrapped (str "(\n" content "\n)")
            zloc (z/of-string wrapped)
            defs (extract-defs zloc)]
        {:ns ns-name :path path :defs defs})
      (catch Exception e
        {:ns ns-name :path path :defs [] :parse-error? true
         :error (str (.getMessage e))}))))

;; =============================================================================
;; Build the full graph
;; =============================================================================

(defn build-full-graph
  "Build graph from all files. Each def is keyed as ns/name."
  [file-results]
  (let [;; Collect all definitions with qualified names
        all-defs
        (for [{:keys [ns defs]} file-results
              {:keys [name kind form]} defs]
          {:qn (str ns "/" name)
           :ns ns
           :name name
           :kind kind
           :form form})

        ;; Set of all defined short names and qualified names
        defined-qns (into #{} (map :qn) all-defs)
        defined-names (into #{} (map :name) all-defs)

        ;; Build graph entries
        graph
        (into {}
              (map (fn [{:keys [qn ns name kind form]}]
                     (let [all-syms (extract-body-symbols form)
                           ;; Deps: match by short name against other defs
                           deps-by-name (-> (set/intersection all-syms defined-names)
                                            (disj name))
                           ;; Host refs
                           host-refs (into #{} (filter host-ref?) all-syms)
                           ;; Parsed host refs
                           host-parsed (into #{} (keep parse-host-ref) all-syms)]
                       [qn {:name name
                            :ns ns
                            :kind kind
                            :deps deps-by-name
                            :host-refs host-refs
                            :host-parsed host-parsed}])))
              all-defs)]
    graph))

;; =============================================================================
;; Layering
;; =============================================================================

(defn assign-layers [graph]
  (let [names (set (keys graph))
        layers (atom {})
        compute-layer
        (fn compute [qn visited]
          (if-let [l (@layers qn)]
            l
            (if (visited qn)
              0
              (let [deps (get-in graph [qn :deps])
                    ;; Resolve short names to qualified names in graph
                    dep-qns (filter names
                                    (for [d deps
                                          [k v] graph
                                          :when (= (:name v) d)]
                                      k))
                    dep-layers (map #(compute % (conj visited qn)) dep-qns)
                    layer (if (empty? dep-layers) 0 (inc (apply max dep-layers)))]
                (swap! layers assoc qn layer)
                layer))))]
    (doseq [qn names]
      (compute-layer qn #{}))
    @layers))

;; =============================================================================
;; Host contract extraction
;; =============================================================================

(defn extract-host-contract
  "Aggregate all host references across the entire codebase."
  [graph]
  (let [all-parsed (mapcat (comp :host-parsed val) graph)
        ;; Group by package.class
        by-class (group-by (fn [{:keys [pkg class]}] (str pkg "." class)) all-parsed)
        ;; For each class, collect unique members
        contract
        (into (sorted-map)
              (map (fn [[class-key refs]]
                     [class-key
                      {:count (count refs)
                       :members (sort (into #{} (keep :member) refs))
                       :used-by (sort (into #{} (map (fn [_] "")) refs))}]))
              by-class)
        ;; Which files use each class
        by-class-with-users
        (into (sorted-map)
              (for [[qn info] graph
                    :let [classes (group-by (fn [{:keys [pkg class]}] (str pkg "." class))
                                           (:host-parsed info))]
                    [class-key _] classes]
                [class-key qn]))]
    {:by-class contract
     :users (group-by first (seq by-class-with-users))}))

;; =============================================================================
;; Classification
;; =============================================================================

(defn classify [graph layers]
  (let [dep-count-map
        (into {}
              (map (fn [[qn info]]
                     [qn (count (filter (fn [[_ v]] (contains? (:deps v) (:name info)))
                                        graph))]))
              graph)]
    (into {}
          (map (fn [[qn info]]
                 (let [layer (get layers qn 0)
                       has-host (seq (:host-refs info))
                       dependents (get dep-count-map qn 0)
                       cls (cond
                             has-host              :host
                             (= 0 layer)           :foundation
                             (and (<= layer 2)
                                  (> dependents 5)) :core
                             (zero? dependents)    :leaf
                             :else                 :derived)]
                   [qn (assoc info :layer layer :dependents dependents :class cls)])))
          graph)))

;; =============================================================================
;; Output
;; =============================================================================

(defn print-summary [classified layers file-results]
  (let [by-class (group-by (comp :class val) classified)
        by-ns (group-by (comp :ns val) classified)
        max-layer (if (empty? (vals layers)) 0 (apply max (vals layers)))
        host-contract (extract-host-contract classified)]

    (println "=== Clojure Language Dependency Analysis ===\n")
    (println (format "Files:       %d" (count file-results)))
    (println (format "Definitions: %d" (count classified)))
    (println (format "Max depth:   %d layers" max-layer))
    (println (format "Namespaces:  %d\n" (count by-ns)))

    ;; Parse errors
    (let [errors (filter :parse-error? file-results)]
      (when (seq errors)
        (println (format "⚠ Parse errors: %d" (count errors)))
        (doseq [{:keys [ns error]} errors]
          (println (format "  %s: %s" ns error)))
        (println)))

    ;; Per-namespace summary
    (println "--- Namespaces ---")
    (doseq [[ns-name fns] (sort-by key by-ns)]
      (let [host-count (count (filter #(= :host (:class (val %))) fns))
            pure-count (- (count fns) host-count)]
        (println (format "  %-40s %3d defs (%3d pure, %3d host)"
                         ns-name (count fns) pure-count host-count))))

    ;; Classification summary
    (println "\n--- Classification ---")
    (doseq [cls [:foundation :core :derived :host :leaf]]
      (let [fns (sort-by (comp :dependents val) > (get by-class cls))]
        (println (format "\n%s (%d):" (str/upper-case (name cls)) (count fns)))
        (doseq [[qn info] (take 25 fns)]
          (println (format "  %-45s L:%-2d deps:%-3d used:%-3d %s"
                           qn (:layer info)
                           (count (:deps info)) (:dependents info)
                           (if (seq (:host-refs info))
                             (str "← " (str/join " " (take 3 (:host-refs info))))
                             ""))))
        (when (> (count fns) 25)
          (println (format "  ... and %d more" (- (count fns) 25))))))

    ;; Host contract
    (println "\n--- Host Contract (Java classes used) ---")
    (let [{:keys [by-class]} host-contract
          ;; Split into clojure.lang vs java
          lang-classes (into (sorted-map) (filter #(str/starts-with? (key %) "clojure.lang.") by-class))
          java-classes (into (sorted-map) (filter #(str/starts-with? (key %) "java.") by-class))
          interop (get by-class ":interop.:method")]

      (println (format "\nclojure.lang classes: %d" (count lang-classes)))
      (doseq [[class-name {:keys [count members]}] lang-classes]
        (let [short-name (last (str/split class-name #"\."))]
          (println (format "  %-35s %3dx  members: %s"
                           short-name count
                           (str/join ", " (take 8 members))))))

      (println (format "\njava.* classes: %d" (count java-classes)))
      (doseq [[class-name {:keys [count members]}] java-classes]
        (println (format "  %-35s %3dx  members: %s"
                         class-name count
                         (str/join ", " (take 8 members)))))

      (when interop
        (println (format "\nInterop methods (.method calls): %d unique"
                         (count (:members interop))))))

    ;; Layer distribution
    (println "\n--- Layer Distribution ---")
    (let [by-layer (group-by (comp layers key) classified)]
      (doseq [l (range (inc max-layer))]
        (let [fns (get by-layer l)]
          (when (seq fns)
            (println (format "  Layer %2d: %3d fns%s"
                             l (count fns)
                             (if (<= (count fns) 8)
                               (str "  " (str/join ", " (sort (map (comp :name val) fns))))
                               "")))))))))

(defn print-host-contract [classified]
  (let [host-contract (extract-host-contract classified)
        {:keys [by-class]} host-contract
        lang-classes (into (sorted-map) (filter #(str/starts-with? (key %) "clojure.lang.") by-class))
        java-classes (into (sorted-map) (filter #(str/starts-with? (key %) "java.") by-class))]

    (println "=== Host Contract ===\n")
    (println "What the Clojure LANGUAGE needs from the JVM.\n")
    (println (format "clojure.lang.* classes: %d" (count lang-classes)))
    (println (format "java.* classes:         %d" (count java-classes)))
    (println (format "Total host surface:     %d classes\n" (+ (count lang-classes) (count java-classes))))

    (println "--- clojure.lang (runtime implementation) ---")
    (doseq [[class-name {:keys [count members]}] lang-classes]
      (let [short-name (last (str/split class-name #"\."))]
        (println (format "\n  %s (%dx):" short-name count))
        (doseq [m members]
          (println (format "    .%s" m)))))

    (println "\n--- java.* (platform) ---")
    (doseq [[class-name {:keys [count members]}] java-classes]
      (println (format "\n  %s (%dx):" class-name count))
      (doseq [m members]
        (println (format "    .%s" m))))))

(defn print-edn [classified layers file-results]
  (let [by-class (group-by (comp :class val) classified)
        host-contract (extract-host-contract classified)]
    (prn {:files (count file-results)
          :definitions (count classified)
          :max-layer (if (empty? (vals layers)) 0 (apply max (vals layers)))
          :classification
          (into {} (map (fn [[cls fns]]
                          [cls {:count (count fns)
                                :fns (mapv (fn [[qn info]]
                                             {:qn qn :layer (:layer info)
                                              :deps (count (:deps info))
                                              :dependents (:dependents info)})
                                           (sort-by (comp :dependents val) > fns))}]))
                (group-by (comp :class val) classified))
          :host-classes (count (:by-class host-contract))})))

(defn print-dot [classified]
  (println "digraph clojure {")
  (println "  rankdir=BT; node [shape=box fontsize=7];")
  (doseq [[qn info] classified]
    (let [color (case (:class info)
                  :foundation "#4CAF50" :core "#2196F3"
                  :derived "#FFC107" :host "#F44336" :leaf "#9E9E9E")]
      (println (format "  \"%s\" [style=filled fillcolor=\"%s\" label=\"%s\"];"
                       qn color (:name info)))))
  (doseq [[qn info] classified
          dep (:deps info)]
    ;; Find any qn with matching short name
    (let [targets (filter (fn [[k v]] (= (:name v) dep)) classified)]
      (doseq [[target-qn _] (take 1 targets)]
        (println (format "  \"%s\" -> \"%s\";" qn target-qn)))))
  (println "}"))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [input (or (first (remove #(str/starts-with? % "--") args))
                  "src/clj/clojure/core.cljc")
        mode (cond
               (some #{"--edn"} args)  :edn
               (some #{"--dot"} args)  :dot
               (some #{"--host"} args) :host
               :else                   :summary)
        ;; Is it a directory or a single file?
        is-dir (fs/directory? input)
        root (if is-dir input (str (fs/parent input)))
        files (if is-dir (find-clj-files input) [input])]

    (binding [*out* *err*]
      (println (format "depgraph: %d files in %s" (count files) input)))

    (let [file-results (mapv #(process-file % root) files)
          total-defs (reduce + (map (comp count :defs) file-results))
          _ (binding [*out* *err*]
              (println (format "  Extracted %d definitions from %d files"
                               total-defs (count file-results))))
          graph (build-full-graph file-results)
          layers (assign-layers graph)
          classified (classify graph layers)]

      (case mode
        :summary (print-summary classified layers file-results)
        :host    (print-host-contract classified)
        :edn     (print-edn classified layers file-results)
        :dot     (print-dot classified)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
