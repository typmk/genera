(ns clojure.json
  "JSON encoding and decoding.")

;; Portable JSON - maps to platform JSON libraries

(defn read-str
  "Reads one JSON value from input String. Options:
   :key-fn - function to transform keys (default keyword)
   :value-fn - function to transform values
   :bigdec - if true, use BigDecimal for non-integer numbers
   :eof-error? - if true (default), throw on EOF
   :eof-value - value to return on EOF if eof-error? is false"
  [string & opts]
  (throw (UnsupportedOperationException. "clojure.json/read-str not yet implemented")))

(defn read
  "Reads one JSON value from a reader. See read-str for options."
  [reader & opts]
  (throw (UnsupportedOperationException. "clojure.json/read not yet implemented")))

(defn write-str
  "Converts x to a JSON-formatted string. Options:
   :key-fn - function to transform keys (default name)
   :value-fn - function to transform values
   :escape-unicode - if true, escape non-ASCII characters
   :escape-js-separators - if true, escape U+2028 and U+2029
   :escape-slash - if true, escape /
   :indent - if true, pretty print with 2-space indent"
  [x & opts]
  (throw (UnsupportedOperationException. "clojure.json/write-str not yet implemented")))

(defn write
  "Write JSON-formatted output to a Writer. See write-str for options."
  [x writer & opts]
  (throw (UnsupportedOperationException. "clojure.json/write not yet implemented")))

(defn pprint
  "Pretty-prints JSON to *out*."
  [x & opts]
  (throw (UnsupportedOperationException. "clojure.json/pprint not yet implemented")))

;; Aliases for convenience
(def parse-string read-str)
(def generate-string write-str)
