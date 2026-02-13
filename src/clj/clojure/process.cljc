(ns clojure.process
  "Portable process/shell operations.

   Thin wrapper over clojure.host."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Synchronous execution
;; =============================================================================

(defn exec
  "Executes command synchronously. Returns {:exit :out :err}.

   Usage:
     (exec \"ls\" [\"-la\"])
     (exec \"git\" [\"status\"])"
  ([cmd] (exec cmd []))
  ([cmd args] (exec cmd args {}))
  ([cmd args opts]
   (h/-proc-exec (h/host) cmd args opts)))

(defn sh
  "Convenience wrapper for exec. Returns stdout on success, throws on error.

   Usage:
     (sh \"ls\")
     (sh \"git\" \"status\")"
  [& args]
  (let [result (exec (first args) (vec (rest args)))]
    (if (zero? (:exit result))
      (:out result)
      (throw (ex-info (str "Command failed: " (first args))
                      {:exit (:exit result)
                       :err (:err result)
                       :command args})))))

;; =============================================================================
;; Asynchronous execution
;; =============================================================================

(defn spawn
  "Spawns process asynchronously. Returns process handle."
  ([cmd] (spawn cmd []))
  ([cmd args] (spawn cmd args {}))
  ([cmd args opts]
   (h/-proc-spawn (h/host) cmd args opts)))

(defn kill
  "Kills spawned process."
  [proc]
  (h/-proc-kill (h/host) proc))

(defn wait
  "Waits for process to complete. Returns exit code."
  [proc]
  (h/-proc-wait (h/host) proc))

;; =============================================================================
;; Environment
;; =============================================================================

(defn env
  "Gets environment variable(s).
   No args: returns all env vars as map.
   With name: returns value or nil."
  ([] (h/-env-all (h/host)))
  ([name] (h/-env-get (h/host) name)))

(defn getenv
  "Gets environment variable with optional default."
  ([name] (env name))
  ([name default] (or (env name) default)))

;; =============================================================================
;; Convenience
;; =============================================================================

(defn exit-code
  "Returns exit code from exec result."
  [result]
  (:exit result))

(defn stdout
  "Returns stdout from exec result."
  [result]
  (:out result))

(defn stderr
  "Returns stderr from exec result."
  [result]
  (:err result))

(defn success?
  "Returns true if exit code is 0."
  [result]
  (zero? (:exit result)))
