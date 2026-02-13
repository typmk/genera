(ns clojure.system
  "System operations: environment, properties, time, exit.

   Portable abstraction over platform system calls.
   Backends implement ISystem protocol.")

(defprotocol ISystem
  "System-level operations. One impl per platform."
  (-getenv [s] [s name] "Get environment variable(s)")
  (-get-property [s name] [s name default] "Get system property")
  (-set-property [s name value] "Set system property")
  (-current-time-ms [s] "Current time in milliseconds since epoch")
  (-nano-time [s] "High-resolution time in nanoseconds")
  (-exit [s status] "Exit with status code")
  (-gc [s] "Run garbage collector")
  (-get-properties [s] "Get all system properties")
  (-line-separator [s] "System line separator")
  (-in [s] "Standard input stream")
  (-out [s] "Standard output stream")
  (-err [s] "Standard error stream"))

(def ^:dynamic *system*
  "The current system implementation. Bound at bootstrap."
  nil)

;; Portable API - delegates to *system*

(defn getenv
  "Returns the value of the environment variable named by name, or nil if not set.
   With no arguments, returns a map of all environment variables."
  ([] (-getenv *system*))
  ([name] (-getenv *system* name)))

(defn get-property
  "Returns the value of the system property named by name, or default if not set."
  ([name] (-get-property *system* name))
  ([name default] (-get-property *system* name default)))

(defn set-property
  "Sets the system property named by name to value."
  [name value]
  (-set-property *system* name value))

(defn current-time-ms
  "Returns the current time in milliseconds since epoch."
  []
  (-current-time-ms *system*))

(defn nano-time
  "Returns the current value of the high-resolution time source, in nanoseconds.
   Only useful for measuring elapsed time."
  []
  (-nano-time *system*))

(defn exit
  "Terminates the currently running process with the given status code."
  [status]
  (-exit *system* status))

(defn gc
  "Runs the garbage collector."
  []
  (-gc *system*))

(defn get-properties
  "Returns a map of all system properties."
  []
  (-get-properties *system*))

(defn line-separator
  "Returns the system-dependent line separator string."
  []
  (-line-separator *system*))

(defn in
  "The standard input stream."
  []
  (-in *system*))

(defn out
  "The standard output stream."
  []
  (-out *system*))

(defn err
  "The standard error stream."
  []
  (-err *system*))
