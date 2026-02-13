(ns clojure.uri
  "Portable URI operations.

   Thin wrapper over clojure.host - no more URIEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn parse
  "Parses URI from string. Returns map with keys:
   :scheme, :host, :port, :path, :query, :fragment
   (or string keys depending on host)"
  [s]
  (h/-uri-parse (h/host) s))

(defn uri
  "Creates a URI from a string."
  [s]
  (parse s))

;; =============================================================================
;; Accessors
;; =============================================================================

(defn- get-part [uri k]
  (or (get uri k) (get uri (name k))))

(defn scheme   [u] (get-part (if (string? u) (parse u) u) :scheme))
(defn host     [u] (get-part (if (string? u) (parse u) u) :host))
(defn port     [u] (get-part (if (string? u) (parse u) u) :port))
(defn path     [u] (get-part (if (string? u) (parse u) u) :path))
(defn query    [u] (get-part (if (string? u) (parse u) u) :query))
(defn fragment [u] (get-part (if (string? u) (parse u) u) :fragment))
(defn user-info [u] (get-part (if (string? u) (parse u) u) :user))

;; =============================================================================
;; Conversion
;; =============================================================================

(defn to-string
  "Formats URI map to string."
  [u]
  (h/-uri-to-string (h/host) u))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn absolute?
  "Returns true if URI has a scheme."
  [u]
  (some? (scheme u)))

(defn relative?
  "Returns true if URI is relative (no scheme)."
  [u]
  (not (absolute? u)))

;; =============================================================================
;; Operations
;; =============================================================================

(defn join
  "Joins base URI with relative path."
  [base relative]
  (let [b (if (string? base) (parse base) base)
        base-path (or (path b) "")]
    (to-string (assoc b :path (str base-path "/" relative)))))

(defn with-query
  "Returns URI with new query string."
  [u query-str]
  (let [parsed (if (string? u) (parse u) u)]
    (to-string (assoc parsed :query query-str))))

(defn with-fragment
  "Returns URI with new fragment."
  [u frag]
  (let [parsed (if (string? u) (parse u) u)]
    (to-string (assoc parsed :fragment frag))))
