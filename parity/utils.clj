#!/usr/bin/env clojure
;; Clojure utility scripts
;; Usage: par check <file...>   - Check bracket balance
;;        par forms <file>      - Debug-print top-level forms

(require '[clojure.string :as str])

;; =============================================================================
;; Bracket balance checker
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
;; Debug-print top-level forms
;; =============================================================================

(defn cmd-forms [files]
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
;; Dispatch
;; =============================================================================

(let [[cmd & args] *command-line-args*]
  (case cmd
    "brackets" (cmd-brackets args)
    "forms"    (cmd-forms args)
    (do (println "Usage: par check <file...> | par forms <file>")
        (System/exit 1))))
