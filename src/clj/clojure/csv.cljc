(ns clojure.csv
  "CSV reading and writing.")

;; Portable CSV - maps to platform CSV libraries

(defn read-csv
  "Reads CSV-data from input (String or Reader) into a lazy sequence of vectors.
   Options:
   :separator - field separator character (default \\,)
   :quote - quote character (default \\\")
   :escape - escape character for quotes
   :strict - if true, throw on unbalanced quotes"
  [input & opts]
  (throw (UnsupportedOperationException. "clojure.csv/read-csv not yet implemented")))

(defn write-csv
  "Writes data to writer in CSV-format. Data should be a sequence of sequences.
   Options:
   :separator - field separator character (default \\,)
   :quote - quote character (default \\\")
   :quote? - predicate to determine if a string should be quoted
   :newline - :lf (default) or :cr+lf"
  [writer data & opts]
  (throw (UnsupportedOperationException. "clojure.csv/write-csv not yet implemented")))
