(ns clojure.stream
  "Portable stream abstractions.

   This namespace provides platform-independent stream operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.stream namespaces.

   Streams are lazily-evaluated sequences that can only be consumed once.
   These functions are terminal operations that consume the stream.")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol IStream
  "Protocol for stream types that can provide an iterator."
  (-stream-iterator [s] "Returns an iterator for the stream. Terminal operation."))

;;; ====================================================================
;;; Dynamic vars for platform binding
;;; ====================================================================

(def ^:dynamic *stream-engine* nil)

;;; ====================================================================
;;; Portable API
;;; ====================================================================

(defn stream-iterator
  "Returns an iterator for the stream. This is a terminal operation."
  [s]
  (-stream-iterator s))

(defn stream?
  "Returns true if x satisfies IStream."
  [x]
  (satisfies? IStream x))
