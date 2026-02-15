#!/usr/bin/env bb
;; Clojure utility scripts (consolidated from individual files)
;; Usage: bb clj_utils.clj <command> [args...]
;;   brackets <file...>  - Check bracket balance
;;   meta <expr>         - Check reader metadata
;;   forms <file>        - Debug-print all top-level forms
;;   json <file>         - Inspect interop JSON

(require '[clojure.string :as str])

;; =============================================================================
;; 1. Bracket balance checker
;; =============================================================================

(defn check-balanced [text]
  (let [opens {\( \) \[ \] \{ \}}
        closes (set (vals opens))
        stack (atom [])]
    (loop [chars (seq text)
           line 1
           col 1
           in-string false
           in-regex false
           escape false]
      (if-let [c (first chars)]
        (cond
          escape
          (recur (rest chars) line (inc col) in-string in-regex false)

          (and (= c \") (not in-regex))
          (recur (rest chars) line (inc col) (not in-string) false false)

          (and (= c \#) (= (second chars) \") (not in-string))
          (recur (drop 2 chars) line (+ col 2) false true false)

          (and in-regex (= c \"))
          (recur (rest chars) line (inc col) false false false)

          (and (or in-string in-regex) (= c \\))
          (recur (rest chars) line (inc col) in-string in-regex true)

          (or in-string in-regex)
          (recur (rest chars) (if (= c \newline) (inc line) line)
                 (if (= c \newline) 1 (inc col)) in-string in-regex false)

          (= c \\)
          (let [next-char (second chars)]
            (if (and next-char (Character/isLetter next-char))
              (let [remaining (drop-while #(Character/isLetter %) (rest chars))]
                (recur remaining line (+ col (- (count chars) (count remaining))) false false false))
              (recur (drop 2 chars) line (+ col 2) false false false)))

          (= c \;)
          (let [remaining (drop-while #(not= % \newline) chars)]
            (recur remaining (inc line) 1 false false false))

          (= c \newline)
          (recur (rest chars) (inc line) 1 false false false)

          (contains? opens c)
          (do (swap! stack conj {:char c :line line :col col})
              (recur (rest chars) line (inc col) false false false))

          (contains? closes c)
          (if (empty? @stack)
            {:error :unmatched-close :char c :line line :col col}
            (let [top (peek @stack)
                  expected (opens (:char top))]
              (if (= c expected)
                (do (swap! stack pop)
                    (recur (rest chars) line (inc col) false false false))
                {:error :mismatched :expected expected :got c
                 :line line :col col :opened-at top})))

          :else
          (recur (rest chars) line (inc col) false false false))

        (if (empty? @stack)
          {:ok true}
          {:error :unclosed :unclosed @stack})))))

(defn cmd-brackets [files]
  (doseq [file files]
    (println (str "Checking: " file))
    (let [result (check-balanced (slurp file))]
      (if (:ok result)
        (println "  OK - Brackets balanced")
        (do (println "  ERROR:" (:error result))
            (println "  " (pr-str result)))))))

;; =============================================================================
;; 2. Reader metadata check
;; =============================================================================

(defn cmd-meta [args]
  (let [expr (or (first args) "(foo bar)")
        r (clojure.lang.LineNumberingPushbackReader.
            (java.io.StringReader. expr))
        f (read r)]
    (println "Meta:" (meta f))))

;; =============================================================================
;; 3. Debug-print top-level forms
;; =============================================================================

(defn cmd-forms [files]
  (require '[clojure.java.io :as io])
  (doseq [file files]
    (println (str "--- " file " ---"))
    (let [reader (clojure.lang.LineNumberingPushbackReader.
                   (java.io.StringReader. (slurp file)))]
      (loop []
        (let [f (read {:eof :eof} reader)]
          (when (not= f :eof)
            (println "Form:" (class f) f)
            (recur)))))))

;; =============================================================================
;; 4. Inspect interop JSON
;; =============================================================================

(defn cmd-json [files]
  (require '[clojure.data.json :as json])
  (doseq [file files]
    (let [content (slurp file)
          clean (if (= \uFEFF (first content)) (subs content 1) content)
          specs (clojure.data.json/read-str clean)
          functions (get specs "functions")]
      (println "Functions count:" (count functions))
      (println "First 10:" (take 10 (keys functions))))))

;; =============================================================================
;; Dispatch
;; =============================================================================

(when (= *file* (System/getProperty "babashka.file"))
  (let [[cmd & args] *command-line-args*]
    (case cmd
      "brackets" (cmd-brackets args)
      "meta"     (cmd-meta args)
      "forms"    (cmd-forms args)
      "json"     (cmd-json args)
      (do (println "Usage: bb clj_utils.clj <brackets|meta|forms|json> [args...]")
          (System/exit 1)))))
