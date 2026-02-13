(ns clojure.concurrent
  "Portable concurrency primitives.

   Thin wrapper over clojure.host - no more ConcurrentEngine protocol.

   Note: Not all platforms support true concurrency (e.g., PHP is single-threaded).
   The host implementation provides appropriate fallbacks."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Thread operations
;; =============================================================================

(defn thread
  "Runs f in a new thread. Returns thread handle (platform-dependent)."
  [f]
  (h/-conc-thread (h/host) f))

(defn sleep
  "Sleeps current thread for ms milliseconds."
  [ms]
  (h/-conc-sleep (h/host) ms))

;; =============================================================================
;; Futures
;; =============================================================================

(defn future-call
  "Executes f asynchronously. Returns a future."
  [f]
  (h/-conc-future (h/host) f))

(defmacro future
  "Executes body asynchronously. Returns a future."
  [& body]
  `(future-call (fn [] ~@body)))

(defn deref-future
  "Dereferences a future, blocking until complete."
  [fut]
  (h/-conc-deref (h/host) fut))

(defn deref-timeout
  "Dereferences future with timeout. Returns timeout-val if timeout exceeded."
  [fut ms timeout-val]
  (h/-conc-deref-timeout (h/host) fut ms timeout-val))

(defn realized?
  "Returns true if future has completed."
  [fut]
  (h/-conc-realized? (h/host) fut))

(def future-done? realized?)

;; =============================================================================
;; Delays
;; =============================================================================

(defn delay-call
  "Creates a delay that computes f when first derefed."
  [f]
  (let [value (volatile! nil)
        done (volatile! false)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (when-not @done
          (vreset! value (f))
          (vreset! done true))
        @value)
      clojure.lang.IPending
      (isRealized [_] @done))))

(defmacro delay
  "Creates a delay that computes body when first derefed."
  [& body]
  `(delay-call (fn [] ~@body)))

;; =============================================================================
;; Promises
;; =============================================================================

(defn promise
  "Creates a promise that can be delivered once."
  []
  (let [value (volatile! nil)
        done (volatile! false)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (while (not @done) (sleep 1))
        @value)
      clojure.lang.IPending
      (isRealized [_] @done)
      clojure.lang.IFn
      (invoke [this v]
        (when-not @done
          (vreset! value v)
          (vreset! done true))
        this))))

(defn deliver
  "Delivers value to promise."
  [p val]
  (p val))

;; =============================================================================
;; Parallel execution
;; =============================================================================

(defn pmap
  "Like map but executes f in parallel."
  [f coll]
  (let [futures (doall (map #(future-call (fn [] (f %))) coll))]
    (map deref-future futures)))

(defn pcalls
  "Executes fns in parallel, returns seq of results."
  [& fns]
  (pmap #(%) fns))

(defmacro pvalues
  "Evaluates exprs in parallel, returns seq of results."
  [& exprs]
  `(pcalls ~@(map (fn [e] `(fn [] ~e)) exprs)))
