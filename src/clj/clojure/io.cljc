(ns clojure.io
  "Portable I/O operations.

   Thin wrapper over clojure.host - no more IOEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Reading & Writing
;; =============================================================================

(defn slurp
  "Reads entire file as string."
  [path]
  (h/-io-slurp (h/host) path))

(defn spit
  "Writes string to file."
  [path content]
  (h/-io-spit (h/host) path content))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn exists?
  "Returns true if path exists."
  [path]
  (h/-io-exists? (h/host) path))

(defn directory?
  "Returns true if path is a directory."
  [path]
  (h/-io-directory? (h/host) path))

(defn file?
  "Returns true if path is a file (exists and not directory)."
  [path]
  (and (exists? path) (not (directory? path))))

;; =============================================================================
;; Directory Operations
;; =============================================================================

(defn mkdir
  "Creates directory (and parents). Returns true on success."
  [path]
  (h/-io-mkdir (h/host) path))

(defn ls
  "Lists directory contents. Returns seq of names."
  [path]
  (h/-io-ls (h/host) path))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn rm
  "Deletes file."
  [path]
  (h/-io-rm (h/host) path))

(defn rmdir
  "Deletes directory recursively."
  [path]
  (h/-io-rmdir (h/host) path))

(defn mv
  "Moves/renames file."
  [from to]
  (h/-io-mv (h/host) from to))

(defn cp
  "Copies file."
  [from to]
  (h/-io-cp (h/host) from to))

(defn delete
  "Deletes file or directory."
  [path]
  (if (directory? path)
    (rmdir path)
    (rm path)))

;; =============================================================================
;; Paths
;; =============================================================================

(defn cwd
  "Returns current working directory."
  []
  (h/-io-cwd (h/host)))

(defn tmp-dir
  "Returns system temp directory."
  []
  (h/-io-tmp (h/host)))

;; =============================================================================
;; Convenience
;; =============================================================================

(defn file-seq
  "Returns lazy seq of all files under dir (recursive)."
  [dir]
  (tree-seq directory? ls dir))

(defn make-parents
  "Creates parent directories for path."
  [path]
  (let [parent (second (re-find #"^(.+)[/\\][^/\\]+$" path))]
    (when parent
      (mkdir parent))))
