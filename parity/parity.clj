#!/usr/bin/env clj
;; =============================================================================
;; parity.clj — Clojure Parity Test Tool
;; =============================================================================
;;
;; Tests whether a Clojure implementation matches JVM Clojure semantics.
;;
;; Usage:
;;   par expand              → results/expressions.edn
;;   par capture             → results/reference.edn
;;   par test                → run tests, print report
;;   par test <results.edn>  → compare results to reference
;;   par stats               → spec statistics
;;
;; The JVM IS the spec. No hand-written expected values.
;; =============================================================================

(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.set]
         '[clojure.walk]
         '[clojure.data]
         '[clojure.pprint]
         '[clojure.math]
         '[clojure.core.reducers]
         '[clojure.spec.alpha :as s]
         '[clojure.core.async :as async])

(def base-dir (or (System/getProperty "parity.dir") "."))
(def spec-dir (str base-dir "/spec"))
(def results-dir (str base-dir "/results"))
(def expressions-file (str results-dir "/expressions.edn"))
(def reference-file (str results-dir "/reference.edn"))

(defn spec-files
  "Find all .edn files in the spec directory (recursive), sorted by name."
  []
  (->> (file-seq (io/file spec-dir))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn")))
       (sort-by #(.getName %))
       (map #(.getPath %))))


;; =============================================================================
;; 1. EXPAND — spec → flat list of expressions
;; =============================================================================

(defn cross-product
  "Cross-product of param axes. Returns seq of maps.
   {:a [1 2] :b [3 4]} → ({:a 1 :b 3} {:a 1 :b 4} {:a 2 :b 3} {:a 2 :b 4})"
  [params]
  (let [ks (keys params)
        vs (vals params)]
    (reduce
      (fn [combos [k vals]]
        (for [combo combos
              v vals]
          (assoc combo k v)))
      [{}]
      (map vector ks vs))))

(defn substitute
  "Replace %name placeholders in template with values from bindings."
  [template bindings]
  (reduce-kv
    (fn [s k v]
      (str/replace s (str "%" (name k)) v))
    template
    bindings))

(defn expand-parametric
  "Expand a parametric spec into explicit test cases."
  [{:keys [describe params template]}]
  (let [combos (cross-product params)]
    (mapv
      (fn [bindings]
        (let [expr (substitute template bindings)
              label (str describe " " (str/join " " (vals bindings)))]
          {:it label :eval expr :source :parametric}))
      combos)))

(defn expand-category
  "Expand a category into a flat list of {:category :it :eval :ns} maps."
  [ns-prefix {:keys [category tests parametric]}]
  (let [full-cat (if ns-prefix
                   (keyword (str ns-prefix "." (name category)))
                   category)
        explicit (mapv #(assoc % :category full-cat :ns ns-prefix :source :explicit) tests)
        generated (vec (mapcat
                         (fn [p]
                           (map #(assoc % :category full-cat :ns ns-prefix) (expand-parametric p)))
                         parametric))]
    (into explicit generated)))

(defn ns-prefix-from-file
  "Extract namespace prefix from filename: 'core.edn' → 'core', 'clojure.string.edn' → 'clojure.string'"
  [path]
  (-> (io/file path)
      (.getName)
      (str/replace #"\.edn$" "")))

(defn expand-spec-file
  "Read one spec file and expand all categories."
  [spec-path]
  (let [prefix (ns-prefix-from-file spec-path)
        specs (edn/read-string (slurp spec-path))]
    (vec (mapcat (partial expand-category prefix) specs))))

(defn expand-all-specs
  "Read all spec files and expand into flat expression list."
  []
  (let [files (spec-files)]
    (vec (mapcat expand-spec-file files))))

(defn do-expand []
  (let [expressions (expand-all-specs)
        by-ns (group-by :ns expressions)]
    (spit expressions-file (pr-str expressions))
    (println (str "Expanded " (count expressions) " test expressions from "
                  (count (spec-files)) " spec files → " expressions-file))
    (println)
    (doseq [[ns-name ns-tests] (sort-by first by-ns)]
      (let [cats (group-by :category ns-tests)]
        (println (str "  " ns-name " (" (count ns-tests) " tests)"))
        (doseq [[cat tests] (sort-by (comp name first) cats)]
          (println (str "    " (name cat) ": " (count tests))))))
    expressions))


;; =============================================================================
;; 2. CAPTURE — run expressions on JVM, save reference results
;; =============================================================================

(def ^:dynamic *eval-timeout-ms* 1000)

(defn safe-eval
  "Evaluate a string expression with timeout.
   Binds *print-length* to prevent infinite seq realization.
   Uses a dedicated thread (not future pool) to avoid pool exhaustion."
  [expr-str]
  (let [result (promise)
        t (Thread.
            (fn []
              (deliver result
                (try
                  (binding [*print-length* 100
                            *print-level* 10]
                    (let [form (read-string expr-str)
                          val (eval form)
                          s (pr-str val)]
                      {:result s :type (str (type val))}))
                  (catch Throwable t
                    {:error (str (.getSimpleName (class t)) ": " (.getMessage t))
                     :error-class (str (.getSimpleName (class t)))})))))]
    (.setDaemon t true)
    (.start t)
    (let [r (deref result *eval-timeout-ms* ::timeout)]
      (if (= r ::timeout)
        (do (.interrupt t)
            {:error "TimeoutException: eval exceeded timeout"
             :error-class "TimeoutException"})
        r))))

(defn do-capture []
  (let [expressions (do-expand)
        total (count expressions)
        results (atom [])
        errors (atom 0)
        start (System/currentTimeMillis)]
    (println (str "Capturing " total " expressions on JVM..."))
    (doseq [[i {:keys [it eval category] :as expr}] (map-indexed vector expressions)]
      (when (zero? (mod (inc i) 500))
        (let [elapsed (/ (- (System/currentTimeMillis) start) 1000.0)]
          (println (format "  %d/%d (%.1fs)" (inc i) total elapsed))))
      (let [result (safe-eval eval)
            entry (merge {:expr eval :category category :it it} result)]
        (swap! results conj entry)
        (when (:error entry)
          (swap! errors inc))))
    (let [elapsed (/ (- (System/currentTimeMillis) start) 1000.0)]
      (spit reference-file (with-out-str
                             (println "[")
                             (doseq [r @results]
                               (prn r))
                             (println "]")))
      (println (format "Captured %d results → %s (%.1fs)" total reference-file elapsed))
      (println (str "  " (- total @errors) " values, " @errors " errors"))
      @results)))


;; =============================================================================
;; 3. TEST — run expressions on current platform, compare to reference
;; =============================================================================

(defn compare-results
  "Compare target results against reference. Pure data comparison, no eval.
   Returns {:pass :fail :error :missing :failures}."
  [reference target expressions]
  (let [ref-by-expr (into {} (map (fn [r] [(:expr r) r]) reference))
        tgt-by-expr (into {} (map (fn [r] [(:expr r) r]) target))
        pass (atom 0)
        fail (atom 0)
        error (atom 0)
        missing (atom 0)
        failures (atom [])
        ns-pass (atom {})
        ns-total (atom {})
        current-cat (atom nil)]
    (doseq [{:keys [it eval category ns]} expressions]
      (when (not= @current-cat category)
        (reset! current-cat category)
        (println)
        (println (str (name category))))
      (swap! ns-total update ns (fnil inc 0))
      (let [ref (get ref-by-expr eval)
            tgt (get tgt-by-expr eval)]
        (cond
          ;; Missing from target
          (nil? tgt)
          (do (swap! missing inc)
              (println (str "  ? " it " — missing")))

          ;; Both errored — check same error class
          (and (:error ref) (:error tgt))
          (if (= (:error-class ref) (:error-class tgt))
            (do (swap! pass inc)
                (swap! ns-pass update ns (fnil inc 0))
                (println (str "  \u2713 " it)))
            (do (swap! fail inc)
                (swap! failures conj {:it it :expr eval
                                      :expected (:error ref)
                                      :actual (:error tgt)})
                (println (str "  \u2717 " it " — expected error " (:error-class ref)
                              " got " (:error-class tgt)))))

          ;; Ref errored but target didn't
          (:error ref)
          (do (swap! fail inc)
              (swap! failures conj {:it it :expr eval
                                    :expected (str "ERROR: " (:error ref))
                                    :actual (:result tgt)})
              (println (str "  \u2717 " it " — expected error, got " (:result tgt))))

          ;; Target errored but ref didn't
          (:error tgt)
          (do (swap! error inc)
              (swap! failures conj {:it it :expr eval
                                    :expected (:result ref)
                                    :actual (str "ERROR: " (:error tgt))})
              (println (str "  ! " it " — " (:error tgt))))

          ;; Both produced values — compare
          (= (:result ref) (:result tgt))
          (do (swap! pass inc)
              (swap! ns-pass update ns (fnil inc 0))
              (println (str "  \u2713 " it)))

          :else
          (do (swap! fail inc)
              (swap! failures conj {:it it :expr eval
                                    :expected (:result ref)
                                    :actual (:result tgt)})
              (println (str "  \u2717 " it " — expected " (:result ref)
                            " got " (:result tgt)))))))

    ;; Per-namespace summary
    (println)
    (println "=== PER-NAMESPACE ===")
    (doseq [ns-name (sort (keys @ns-total))]
      (let [total (get @ns-total ns-name 0)
            passed (get @ns-pass ns-name 0)
            pct (if (pos? total) (format "%.1f" (* 100.0 (/ (double passed) total))) "0.0")]
        (println (str "  " ns-name ": " passed "/" total " (" pct "%)"))))

    ;; Overall summary
    (println)
    (println "=== PARITY REPORT ===")
    (let [total (+ @pass @fail @error @missing)
          pct (if (pos? total) (format "%.1f" (* 100.0 (/ @pass total))) "0.0")]
      (println (str "  Pass:    " @pass "/" total " (" pct "%)"))
      (println (str "  Fail:    " @fail))
      (println (str "  Error:   " @error))
      (println (str "  Missing: " @missing)))

    (when (seq @failures)
      (println)
      (println "--- Failures ---")
      (doseq [{:keys [it expr expected actual]} (take 20 @failures)]
        (println (str "  " it))
        (println (str "    expr:     " expr))
        (println (str "    expected: " expected))
        (println (str "    actual:   " actual))))

    {:pass @pass :fail @fail :error @error :missing @missing
     :failures @failures}))

(defn do-test
  "Self-check: compare reference against itself (validates capture integrity)."
  []
  (let [reference (edn/read-string (slurp reference-file))
        expressions (edn/read-string (slurp expressions-file))]
    (println "=== SELF-CHECK (reference vs reference) ===")
    (compare-results reference reference expressions)))


;; =============================================================================
;; 4. STATS — show spec statistics without running
;; =============================================================================

(defn do-stats []
  (let [files (spec-files)
        expressions (expand-all-specs)
        by-ns (group-by :ns expressions)
        by-source (group-by :source expressions)]
    (println "=== Parity Spec Statistics ===")
    (println)
    (println (str "Spec files: " (count files)))
    (println (str "Total expressions: " (count expressions)))
    (println (str "  Explicit: " (count (:explicit by-source))))
    (println (str "  Parametric: " (count (:parametric by-source))))
    (println)
    (doseq [[ns-name ns-tests] (sort-by first by-ns)]
      (let [cats (group-by :category ns-tests)
            ns-explicit (count (filter #(= :explicit (:source %)) ns-tests))
            ns-parametric (count (filter #(= :parametric (:source %)) ns-tests))]
        (println (str ns-name " — " (count ns-tests)
                      " (" ns-explicit " explicit + " ns-parametric " parametric)"))
        (doseq [[cat tests] (sort-by (comp name first) cats)]
          (let [explicit (count (filter #(= :explicit (:source %)) tests))
                parametric (count (filter #(= :parametric (:source %)) tests))]
            (println (str "  " (name cat) ": " (count tests)
                          " (" explicit "+" parametric ")"))))))))


;; =============================================================================
;; 5. EXTERNAL RESULTS — compare a platform's results file against reference
;; =============================================================================

(defn do-compare
  "Compare external results file against reference. No eval — pure data comparison."
  [path]
  (let [reference (edn/read-string (slurp reference-file))
        target (edn/read-string (slurp path))
        expressions (edn/read-string (slurp expressions-file))]
    (println (str "=== PARITY: " path " vs " reference-file " ==="))
    (compare-results reference target expressions)))


;; =============================================================================
;; MAIN
;; =============================================================================

(let [[cmd & args] *command-line-args*]
  (case cmd
    "expand"  (do-expand)
    "capture" (do-capture)
    "test"    (if (first args) (do-compare (first args)) (do-test))
    "stats"   (do-stats)
    (do
      (println "Usage: clj -M parity.clj <command>")
      (println)
      (println "Commands:")
      (println "  expand              Expand specs → expressions list")
      (println "  capture             Eval all on JVM, save reference")
      (println "  test                Self-check (reference vs reference)")
      (println "  test <results.edn>  Compare platform results to reference")
      (println "  stats               Show spec statistics")
      (System/exit 1))))
