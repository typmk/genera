(ns clojure.shell
  "Portable shell command execution.

   This namespace provides platform-independent shell operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.shell namespaces.")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol ShellExecutor
  "Execute shell commands.
   Backends must provide an implementation of this protocol."
  (-exec [executor cmd opts]
    "Execute cmd (vector of strings) with opts.
     opts may include:
       :dir     - working directory (string or file)
       :env     - environment map {string string}
       :in      - input to feed to process stdin
       :in-enc  - encoding for :in (default UTF-8)
       :out-enc - encoding for stdout, or :bytes (default UTF-8)
     Returns {:exit int, :out string-or-bytes, :err string}"))

;;; ====================================================================
;;; Dynamic vars
;;; ====================================================================

(def ^:dynamic *sh-dir*
  "When set, provides the default :dir for sh calls."
  nil)

(def ^:dynamic *sh-env*
  "When set, provides the default :env for sh calls."
  nil)

;; Platform implementations register their executor here
(def ^:dynamic *shell-executor* nil)

;;; ====================================================================
;;; Portable API
;;; ====================================================================

(defmacro with-sh-dir
  "Sets the directory for use with sh, see sh for details."
  [dir & forms]
  `(binding [*sh-dir* ~dir]
     ~@forms))

(defmacro with-sh-env
  "Sets the environment for use with sh, see sh for details."
  [env & forms]
  `(binding [*sh-env* ~env]
     ~@forms))

(defn- parse-args
  "Parse sh arguments into [cmd opts]."
  [args]
  (let [default-encoding "UTF-8"
        default-opts {:out-enc default-encoding
                      :in-enc default-encoding
                      :dir *sh-dir*
                      :env *sh-env*}
        [cmd opts] (split-with string? args)]
    [(vec cmd) (merge default-opts (apply hash-map opts))]))

(defn sh
  "Passes the given strings to launch a sub-process.

   Options are:
     :in      may be given followed by any legal input source for
              clojure.io/copy, e.g. InputStream, Reader, File, byte[],
              or String, to be fed to the sub-process's stdin.
     :in-enc  option may be given followed by a String, used as a character
              encoding name (for example \"UTF-8\" or \"ISO-8859-1\") to
              convert the input string specified by the :in option to the
              sub-process's stdin. Defaults to UTF-8.
              If the :in option provides a byte array, then the bytes are
              passed unencoded, and this option is ignored.
     :out-enc option may be given followed by :bytes or a String. If a
              String is given, it will be used as a character encoding
              name (for example \"UTF-8\" or \"ISO-8859-1\") to convert
              the sub-process's stdout to a String which is returned.
              If :bytes is given, the sub-process's stdout will be stored
              in a byte array and returned. Defaults to UTF-8.
     :env     override the process env with a map (or the underlying
              platform's native env format).
     :dir     override the process dir with a String or File.

   You can bind :env or :dir for multiple operations using with-sh-env
   and with-sh-dir.

   sh returns a map of:
     :exit => sub-process's exit code
     :out  => sub-process's stdout (as byte[] or String)
     :err  => sub-process's stderr (String via platform default encoding)"
  [& args]
  (when-not *shell-executor*
    (throw (ex-info "No shell executor registered. Platform must set *shell-executor*." {})))
  (let [[cmd opts] (parse-args args)]
    (-exec *shell-executor* cmd opts)))
