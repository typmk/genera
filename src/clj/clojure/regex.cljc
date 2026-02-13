(ns clojure.regex
  "Portable regular expression operations.

   Thin wrapper over clojure.host - no more RegexEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Pattern compilation
;; =============================================================================

(defn pattern
  "Compiles a pattern string into a regex pattern."
  [s]
  (h/-re-pattern (h/host) s))

(defn re-pattern
  "Returns a compiled regex pattern. If already a pattern, returns it."
  [s]
  (pattern s))

;; =============================================================================
;; Matching
;; =============================================================================

(defn matches?
  "Returns true if pattern matches entire input string."
  [pat s]
  (some? (h/-re-matches (h/host) (pattern pat) s)))

(defn find?
  "Returns true if pattern is found anywhere in input."
  [pat s]
  (some? (h/-re-find (h/host) (pattern pat) s)))

(defn re-matches
  "Returns match groups if entire string matches, nil otherwise.
   If no groups, returns the match string.
   If groups, returns vector of [full-match group1 group2 ...]"
  [pat s]
  (h/-re-matches (h/host) (pattern pat) s))

(defn re-find
  "Returns the first match of pattern in s, or nil.
   If no groups, returns the match string.
   If groups, returns vector of [full-match group1 group2 ...]"
  [pat s]
  (h/-re-find (h/host) (pattern pat) s))

(defn re-seq
  "Returns lazy seq of all matches of pattern in s."
  [pat s]
  (h/-re-find-all (h/host) (pattern pat) s))

;; Aliases
(def find-first re-find)
(def find-all re-seq)
(def groups re-find)

;; =============================================================================
;; Splitting
;; =============================================================================

(defn split
  "Splits input string around matches of pattern."
  ([pat s]
   (h/-re-split (h/host) (pattern pat) s))
  ([pat s limit]
   ;; limit support depends on host - basic split for now
   (h/-re-split (h/host) (pattern pat) s)))

;; =============================================================================
;; Replacement
;; =============================================================================

(defn replace-all
  "Replaces all matches of pattern with replacement."
  [pat s replacement]
  (h/-re-replace (h/host) (pattern pat) s replacement))

(defn replace-first
  "Replaces first match of pattern with replacement."
  [pat s replacement]
  (when-let [match (re-find pat s)]
    (let [match-str (if (string? match) match (first match))]
      (h/-str-replace (h/host) s match-str replacement))))

;; Clojure-compatible aliases
(def re-replace replace-all)
