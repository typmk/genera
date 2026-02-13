(ns clojure.uuid
  "Portable UUID operations.

   Thin wrapper over clojure.host - no more UUIDEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Generation
;; =============================================================================

(defn random-uuid
  "Generates a random (version 4) UUID."
  []
  (h/-uuid-random (h/host)))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn uuid
  "Creates a UUID from a string, or generates random if no arg."
  ([] (random-uuid))
  ([s] (h/-uuid-from-string (h/host) s)))

(defn parse-uuid
  "Parses a UUID from string, returns nil if invalid."
  [s]
  (try
    (uuid s)
    (catch Exception _ nil)))

;; =============================================================================
;; Conversion
;; =============================================================================

(defn uuid-string
  "Returns the UUID as a lowercase string with hyphens."
  [u]
  (h/-uuid-to-string (h/host) u))

(def to-string uuid-string)

;; =============================================================================
;; Predicates
;; =============================================================================

(defn uuid?
  "Returns true if s is a valid UUID string."
  [s]
  (and (string? s)
       (boolean (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" s))))

;; =============================================================================
;; Well-known UUIDs
;; =============================================================================

(def nil-uuid "00000000-0000-0000-0000-000000000000")
(def namespace-dns "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
(def namespace-url "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
(def namespace-oid "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
(def namespace-x500 "6ba7b814-9dad-11d1-80b4-00c04fd430c8")

;; =============================================================================
;; Reader support
;; =============================================================================

(defn default-uuid-reader
  "Default reader for #uuid tagged literals."
  [form]
  (if (string? form)
    (uuid form)
    (throw (ex-info "#uuid requires a string" {:form form}))))
