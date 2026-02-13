(ns clojure.time
  "Portable date and time operations.

   Thin wrapper over clojure.host - no more TimeEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Instant (point in time)
;; =============================================================================

(defn now
  "Returns the current instant."
  []
  (h/-time-now (h/host)))

(defn now-ms
  "Returns current time as milliseconds since epoch."
  []
  (h/-time-now-ms (h/host)))

(defn instant
  "Creates an instant from epoch milliseconds."
  [epoch-ms]
  (h/-time-from-ms (h/host) epoch-ms))

(defn to-epoch-ms
  "Converts instant to milliseconds since epoch."
  [inst]
  (h/-time-instant-ms (h/host) inst))

;; =============================================================================
;; Parsing & Formatting
;; =============================================================================

(defn parse
  "Parses an ISO-8601 string to instant."
  [s]
  (h/-time-parse (h/host) s))

(defn format
  "Formats instant to ISO-8601 string."
  [inst]
  (h/-time-format (h/host) inst))

;; =============================================================================
;; Duration helpers
;; =============================================================================

(defn millis
  "Returns n as milliseconds (identity, for clarity)."
  [n] n)

(defn seconds
  "Returns n seconds as milliseconds."
  [n] (* n 1000))

(defn minutes
  "Returns n minutes as milliseconds."
  [n] (* n 60000))

(defn hours
  "Returns n hours as milliseconds."
  [n] (* n 3600000))

(defn days
  "Returns n days as milliseconds."
  [n] (* n 86400000))

;; =============================================================================
;; Arithmetic
;; =============================================================================

(defn plus
  "Adds milliseconds to an instant."
  [inst ms]
  (h/-time-from-ms (h/host) (+ (to-epoch-ms inst) ms)))

(defn minus
  "Subtracts milliseconds from an instant."
  [inst ms]
  (h/-time-from-ms (h/host) (- (to-epoch-ms inst) ms)))

(defn between
  "Returns milliseconds between two instants."
  [start end]
  (- (to-epoch-ms end) (to-epoch-ms start)))

;; =============================================================================
;; Comparison
;; =============================================================================

(defn before?
  "Returns true if a is before b."
  [a b]
  (< (to-epoch-ms a) (to-epoch-ms b)))

(defn after?
  "Returns true if a is after b."
  [a b]
  (> (to-epoch-ms a) (to-epoch-ms b)))

(defn elapsed
  "Returns milliseconds elapsed since instant."
  [inst]
  (- (now-ms) (to-epoch-ms inst)))
