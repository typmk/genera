#!/usr/bin/env clojure
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
;;   par deps /path/to/clojure/src/clj          # full language
;;   par deps src/clj/clojure/core.cljc         # single file
;;   par deps /path --edn                       # EDN output
;;   par deps /path --dot                       # DOT output
;;   par deps /path --host                      # host contract only
;; =============================================================================

(ns depgraph
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure.string :as str]
            [clojure.set :as set]
            [parity.color :refer [bold dim green yellow cyan red white
                                  heading *color*]])
  (:import [java.io File]
           [java.nio.file Files Paths]
           [java.util.function BiPredicate]))

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
  "Find all .clj files under a directory (including root level)."
  [root]
  (let [root-path (Paths/get root (into-array String []))
        matcher (reify BiPredicate
                  (test [_ path attrs]
                    (str/ends-with? (str path) ".clj")))]
    (->> (Files/find root-path Integer/MAX_VALUE matcher (into-array java.nio.file.FileVisitOption []))
         .iterator
         iterator-seq
         (map str)
         sort vec)))

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
        host-contract (extract-host-contract classified)
        n-total (count classified)
        n-host (count (get by-class :host))
        n-pure (- n-total n-host)
        {:keys [by-class]} host-contract
        lang-classes (into (sorted-map) (filter #(str/starts-with? (key %) "clojure.lang.") by-class))
        java-classes (into (sorted-map) (filter #(str/starts-with? (key %) "java.") by-class))
        interop-entry (get by-class ":interop.:method")]

    ;; Parse errors (show first if any)
    (let [errors (filter :parse-error? file-results)]
      (when (seq errors)
        (println (red (format "\n  PARSE ERRORS: %d" (count errors))))
        (doseq [{:keys [ns error]} errors]
          (println (format "    %s: %s" ns error)))
        (println)))

    ;; ── NAMESPACES ───────────────────────────────────────────────────────
    (println (heading "NAMESPACES"))
    (println)
    (println (format "  %-36s %5s %5s %5s %5s"
                     (bold "Namespace") (bold "Defs") (bold "Pure") (bold "Host") (bold "Host%")))
    (println (dim (format "  %-36s %5s %5s %5s %5s"
                          (apply str (repeat 36 "─")) "─────" "─────" "─────" "─────")))
    (doseq [[ns-name fns] (sort-by (comp - count val) by-ns)]
      (let [host-count (count (filter #(= :host (:class (val %))) fns))
            pure-count (- (count fns) host-count)
            pct (if (zero? (count fns)) 0 (* 100.0 (/ host-count (count fns))))]
        (println (format "  %-36s %5d %s %s %4.0f%%"
                         ns-name (count fns)
                         (green (format "%5d" pure-count))
                         (yellow (format "%5d" host-count))
                         pct))))
    (println)

    ;; ── MOST DEPENDED ON ─────────────────────────────────────────────────
    (println (heading "TOP 30 DEFINITIONS  (most depended on)"))
    (println)
    (println (format "  %-40s %5s %5s  %s"
                     (bold "Definition") (bold "Used") (bold "Layer") (bold "Kind")))
    (println (dim (format "  %-40s %5s %5s  %s"
                          (apply str (repeat 40 "─")) "─────" "─────" "────")))
    (let [top (take 30 (sort-by (comp - :dependents val) classified))]
      (doseq [[qn info] top]
        (let [host? (seq (:host-refs info))]
          (println (format "  %-40s %5d %5d  %s"
                           qn (:dependents info) (:layer info)
                           (if host?
                             (str (red "HOST") (dim (str " <- " (str/join " " (take 2 (:host-refs info))))))
                             (green "pure")))))))
    (println)

    ;; ── HOST CONTRACT ────────────────────────────────────────────────────
    (println (heading "HOST CONTRACT  (Java classes referenced by .clj source)"))
    (println)
    (println (bold (white (format "  clojure.lang.* (%d classes):" (count lang-classes)))))
    (doseq [[class-name {:keys [count members]}] lang-classes]
      (let [short-name (last (str/split class-name #"\."))]
        (println (format "    %-28s %s  %s" (yellow short-name) (format "%3dx" count)
                         (dim (str/join ", " (take 8 members)))))))

    (println (bold (white (format "\n  java.* (%d classes):" (count java-classes)))))
    (doseq [[class-name {:keys [count members]}] java-classes]
      (println (format "    %-35s %s  %s" (yellow class-name) (format "%3dx" count)
                       (dim (str/join ", " (take 6 members))))))

    (when interop-entry
      (let [members (sort (:members interop-entry))
            shown (take 20 members)
            more (- (count members) (count shown))]
        (println (bold (white (format "\n  Interop (.method calls): %d unique" (count members)))))
        (println (str "    " (dim (str/join ", " shown))
                      (if (pos? more) (dim (format " (+%d more)" more)) "")))))
    (println)

    ;; ── LAYER DISTRIBUTION ───────────────────────────────────────────────
    (println (heading "LAYER DISTRIBUTION"))
    (println)
    (let [by-layer (group-by (comp layers key) classified)
          max-count (apply max 1 (map (comp count val) by-layer))]
      (doseq [l (range (inc max-layer))]
        (let [fns (get by-layer l)]
          (when (seq fns)
            (let [w (max 1 (int (* 30.0 (/ (count fns) max-count))))
                  bar-str (apply str (repeat w "█"))]
              (println (format "  %2d %s%s %3d%s"
                               l (green bar-str) (dim (apply str (repeat (- 30 w) "░")))
                               (count fns)
                               (if (<= (count fns) 6)
                                 (dim (str "  " (str/join ", " (sort (map (comp :name val) fns)))))
                                 ""))))))))
    (println)

    ;; ── SUMMARY ──────────────────────────────────────────────────────────
    (println (bold (cyan "SUMMARY")))
    (println (dim (apply str (repeat 72 "─"))))
    (println)
    (println (format "  %s  |  %s  |  %s  |  %s"
                     (bold (format "%d files" (count file-results)))
                     (bold (format "%d namespaces" (count by-ns)))
                     (bold (format "%d definitions" n-total))
                     (bold (format "%d layers" max-layer))))
    (println (format "  %s  |  %s"
                     (green (format "%d pure (%.0f%%)" n-pure (* 100.0 (/ n-pure n-total))))
                     (yellow (format "%d host-bound (%.0f%%)" n-host (* 100.0 (/ n-host n-total))))))
    (println (format "  Host surface: %s clojure.lang classes, %s java classes%s"
                     (bold (str (count lang-classes))) (bold (str (count java-classes)))
                     (if interop-entry (str ", " (bold (str (count (:members interop-entry)))) " interop") "")))
    (println)
    (println (dim "  Use `par deps <src> --edn` for machine-readable output."))))

(defn print-host-contract [classified]
  (let [host-contract (extract-host-contract classified)
        {:keys [by-class]} host-contract
        lang-classes (into (sorted-map) (filter #(str/starts-with? (key %) "clojure.lang.") by-class))
        java-classes (into (sorted-map) (filter #(str/starts-with? (key %) "java.") by-class))]

    (println (heading "HOST CONTRACT"))
    (println)
    (println "  What the Clojure LANGUAGE needs from the JVM.")
    (println)

    (println (bold (white (format "  clojure.lang (runtime) — %d classes:" (count lang-classes)))))
    (doseq [[class-name {:keys [count members]}] lang-classes]
      (let [short-name (last (str/split class-name #"\."))]
        (println (format "\n    %s %s" (yellow (format "%s (%dx):" short-name count)) ""))
        (doseq [m members]
          (println (format "      %s" (dim (str "." m)))))))

    (println (bold (white (format "\n  java.* (platform) — %d classes:" (count java-classes)))))
    (doseq [[class-name {:keys [count members]}] java-classes]
      (println (format "\n    %s" (yellow (format "%s (%dx):" class-name count))))
      (doseq [m members]
        (println (format "      %s" (dim (str "." m))))))

    ;; Summary at bottom
    (println)
    (println (bold (cyan "SUMMARY")))
    (println (dim (apply str (repeat 72 "─"))))
    (println)
    (println (format "  %s clojure.lang classes  |  %s java classes  |  %s total"
                     (bold (str (count lang-classes)))
                     (bold (str (count java-classes)))
                     (bold (str (+ (count lang-classes) (count java-classes))))))))

(defn print-edn [classified layers file-results]
  (let [host-contract (extract-host-contract classified)
        max-layer (if (empty? (vals layers)) 0 (apply max (vals layers)))
        ;; Full graph: every node with its deps, host refs, classification
        graph
        (into (sorted-map)
              (map (fn [[qn info]]
                     [qn {:ns (:ns info)
                          :name (:name info)
                          :kind (:kind info)
                          :layer (get layers qn 0)
                          :class (:class info)
                          :deps (vec (sort (:deps info)))
                          :dependents (:dependents info)
                          :host-refs (vec (sort (:host-refs info)))
                          :host-parsed (vec (for [p (sort-by :raw (:host-parsed info))]
                                              (select-keys p [:pkg :class :member])))}]))
              classified)]
    (prn {:files (count file-results)
          :definitions (count classified)
          :max-layer max-layer
          :layers (into (sorted-map)
                        (map (fn [[l nodes]] [l (count nodes)]))
                        (group-by (comp layers key) classified))
          :host-contract (:by-class host-contract)
          :graph graph})))

(defn print-host-data
  "Output per-namespace host analysis as EDN, for consumption by specgen --coverage."
  [classified]
  (let [by-ns (group-by (comp :ns val) classified)
        results
        (vec (for [[ns-name entries] (sort-by key by-ns)]
               (let [vars (map val entries)
                     pure (filter #(empty? (:host-refs %)) vars)
                     host (filter #(seq (:host-refs %)) vars)
                     host-pattern-counts
                     (fn [v]
                       (let [parsed (:host-parsed v)]
                         (frequencies
                           (for [p parsed]
                             (cond
                               (= "clojure.lang" (:pkg p))
                               (case (:class p)
                                 "RT" :rt-bridge
                                 "Numbers" :numbers-bridge
                                 "Util" :util-bridge
                                 :clojure-lang)
                               (= :interop (:pkg p)) :method-call
                               (str/starts-with? (str (:pkg p)) "java.") :java
                               :else :other)))))
                     all-refs (mapcat (fn [v]
                                        (for [[pat cnt] (host-pattern-counts v)]
                                          {:pattern pat :count cnt}))
                                      host)
                     by-pattern (group-by :pattern all-refs)]
                 {:ns ns-name
                  :total (count vars)
                  :pure (count pure)
                  :host (count host)
                  :unknown 0
                  :vars-pure (mapv :name pure)
                  :vars-host (vec (for [v host]
                                    {:var (:name v)
                                     :refs (vec (for [[pat cnt] (host-pattern-counts v)]
                                                  {:pattern pat :count cnt}))}))
                  :host-surface (into (sorted-map)
                                      (map (fn [[k vs]] [k (reduce + (map :count vs))]))
                                      by-pattern)})))]
    (prn results)))

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
               (some #{"--edn"} args)       :edn
               (some #{"--dot"} args)       :dot
               (some #{"--host"} args)      :host
               (some #{"--host-edn"} args)  :host-edn
               :else                        :summary)
        no-color (some #{"--no-color"} args)
        is-dir (.isDirectory (File. ^String input))
        root (if is-dir input (.getParent (File. ^String input)))
        files (if is-dir (find-clj-files input) [input])]

    (binding [*color* (not no-color)]
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
          :summary  (print-summary classified layers file-results)
          :host     (print-host-contract classified)
          :host-edn (print-host-data classified)
          :edn      (print-edn classified layers file-results)
          :dot      (print-dot classified))))))

(apply -main *command-line-args*)
