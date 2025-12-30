(ns cljp.main
  "ClojurePHP compiler CLI.

   Compiles .cljc files to PHP with source maps for debugging.

   Usage: clj -M -m cljp.main [options] <files...>

   Examples:
     clj -M -m cljp.main app.cljc              # → out.php
     clj -M -m cljp.main -o app.php src/*.cljc # → app.php
     clj -M -m cljp.main -o - app.cljc | php   # pipe to PHP
     clj -M -m cljp.main -r                    # start REPL"
  (:require [cljp.analyze :as ana]
            [cljp.ast :as ast]
            [cljp.emit :as emit]
            [cljp.infer :as infer]
            [cljp.lower-php :as lower]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]))

;; ============================================================
;; PHP Header
;; ============================================================

(defn php-header []
  (str "<?php\n\n"
       "require __DIR__ . '/vendor/autoload.php';\n"
       "\\Cljp\\ErrorHandler::register();\n\n"))

;; ============================================================
;; Compilation (pure functions)
;; ============================================================

(defn form->string
  "Convert a form to a readable string, truncated for source maps."
  [form max-len]
  (let [s (pr-str form)]
    (if (> (count s) max-len)
      (str (subs s 0 (- max-len 3)) "...")
      s)))

(defn compile-form
  "Compile a single form to PHP string.
   Returns {:php \"...\" :line n :col n :form \"...\"} where line/col are from source.
   Pipeline: source → analyze (HIR) → lower (MIR) → emit (PHP)"
  [form {:keys [file typed?]}]
  (let [env (ast/make-env {:context :statement :file file})
        hir (ana/analyze env form)
        hir (if typed? (infer/infer-types hir) hir)
        mir (lower/lower hir)
        m (meta form)]
    {:php (binding [emit/*emit-docblocks* typed?]
            (emit/emit-php-lifted mir))
     :line (:line m)
     :col (:column m)
     :file file
     :form (form->string form 60)}))

(defn cljp-file?
  "Check if file is a .cljp file (ClojurePHP only, no reader conditionals)."
  [file]
  (str/ends-with? (str file) ".cljp"))

(defn read-forms
  "Read all forms from a file. Returns vector of forms.
   Supports .cljc (with reader conditionals) and .cljp (plain) files."
  [file]
  (when (.exists (io/file file))
    (let [reader (clojure.lang.LineNumberingPushbackReader.
                   (java.io.StringReader. (slurp file)))
          read-opts (if (cljp-file? file)
                      {:eof ::eof}
                      {:eof ::eof :read-cond :allow :features #{:cljp}})]
      (loop [forms []]
        (let [form (try
                     (read read-opts reader)
                     (catch Exception e
                       (binding [*out* *err*]
                         (println "Read error in" file ":" (.getMessage e)))
                       ::error))]
          (case form
            ::eof forms
            ::error forms
            (recur (conj forms form))))))))

(defn count-lines
  "Count number of newlines in a string."
  [s]
  (count (filter #(= % \newline) s)))

(defn compile-file
  "Compile a single file to {:php \"...\" :source-map [...]}."
  [file options start-line]
  (let [forms (read-forms file)
        opts (assoc options :file (str file))]
    (loop [forms forms
           php-acc ""
           source-map []
           current-line start-line]
      (if (empty? forms)
        {:php php-acc
         :source-map source-map
         :end-line current-line}
        (let [form (first forms)
              result (compile-form form opts)
              php-str (:php result)
              entry {:php-line current-line
                     :clj-line (:line result)
                     :clj-col (:col result)
                     :file (:file result)
                     :form (:form result)}
              new-line (+ current-line (count-lines php-str))]
          (recur (rest forms)
                 (str php-acc php-str)
                 (if (:line result)  ; Only add if we have source location
                   (conj source-map entry)
                   source-map)
                 new-line))))))

(defn compile-files
  "Compile input files to {:php \"...\" :source-map [...]}."
  [files options]
  (let [core-lib (io/file "src/cljp/core.cljc")
        all-files (if (.exists core-lib)
                    (cons (str core-lib) files)
                    files)
        header (php-header)
        start-line (+ 1 (count-lines header))]  ; Line after header
    (loop [files all-files
           php-acc header
           source-map []
           current-line start-line]
      (if (empty? files)
        {:php php-acc
         :source-map source-map
         :files all-files}
        (let [file (first files)
              result (compile-file file options current-line)]
          (recur (rest files)
                 (str php-acc (:php result))
                 (into source-map (:source-map result))
                 (:end-line result)))))))

;; ============================================================
;; Source Map Output
;; ============================================================

(defn source-map-path
  "Get the source map path for a PHP file.
   out.php → .xdebug/out.php.json"
  [php-path]
  (let [f (io/file php-path)
        parent (.getParentFile f)
        xdebug-dir (if parent
                     (io/file parent ".xdebug")
                     (io/file ".xdebug"))]
    (io/file xdebug-dir (str (.getName f) ".json"))))

(defn escape-json-string
  "Escape a string for JSON output."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn write-source-map!
  "Write source map JSON alongside PHP file."
  [php-path source-map]
  (let [map-file (source-map-path php-path)]
    (.mkdirs (.getParentFile map-file))
    (with-open [w (io/writer map-file)]
      (.write w "[")
      (.write w (str/join ","
                  (map (fn [m]
                         (str "{\"php_line\":" (:php-line m)
                              ",\"clj_line\":" (:clj-line m)
                              ",\"clj_col\":" (or (:clj-col m) "null")
                              ",\"file\":\"" (escape-json-string (str (:file m))) "\""
                              ",\"form\":\"" (escape-json-string (or (:form m) "")) "\"}"))
                       source-map)))
      (.write w "]"))))

;; ============================================================
;; Output
;; ============================================================

(defn write-result!
  "Write compilation result to file or stdout.
   When writing to file, also generates source map."
  [{:keys [php source-map]} output verbose?]
  (if (= output "-")
    ;; Stdout mode - no source map
    (print php)
    ;; File mode - write PHP and source map
    (do
      (spit output php)
      (write-source-map! output source-map)
      (when verbose?
        (binding [*out* *err*]
          (println "→" output)
          (println "→" (str (source-map-path output))))))))

;; ============================================================
;; CLI
;; ============================================================

(def cli-options
  [["-o" "--output FILE" "Output file (default: out.php, - for stdout)"
    :default "out.php"]
   ["-t" "--typed" "Enable type inference and PHPDoc"]
   ["-v" "--verbose" "Show compilation progress"]
   ["-r" "--repl" "Start interactive REPL"]
   ["-s" "--serve PORT" "Start TCP eval server on PORT"
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "Show this help"]])

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      ;; Help
      (:help options)
      (do
        (println "ClojurePHP Compiler")
        (println)
        (println "Usage: clj -M -m cljp.main [options] <files...>")
        (println)
        (println "Options:")
        (println summary)
        (println)
        (println "Examples:")
        (println "  clj -M -m cljp.main app.cljc")
        (println "  clj -M -m cljp.main -o app.php src/app.cljc")
        (println "  clj -M -m cljp.main -o - app.cljc | php")
        (println "  clj -M -m cljp.main -r")
        (println "  clj -M -m cljp.main -s 7888"))

      ;; Errors
      errors
      (binding [*out* *err*]
        (doseq [e errors]
          (println "Error:" e))
        (System/exit 1))

      ;; REPL mode
      (:repl options)
      (do
        (require '[cljp.repl :as repl])
        ((resolve 'cljp.repl/start!)))

      ;; TCP Server mode
      (:serve options)
      (do
        (require '[cljp.repl :as repl])
        ((resolve 'cljp.repl/start-tcp-server!) :port (:serve options))
        ;; Keep main thread alive
        @(promise))

      ;; Compile
      :else
      (let [files (if (seq arguments) arguments ["test.cljc"])
            opts {:typed? (:typed options)}
            result (compile-files files opts)]
        (when (:verbose options)
          (binding [*out* *err*]
            (println "Compiling:" (str/join ", " files))))
        (write-result! result (:output options) (:verbose options))))))
