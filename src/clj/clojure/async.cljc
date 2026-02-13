(ns clojure.async
  "Portable CSP-style async operations with channels.

   This namespace provides platform-independent async operations
   inspired by core.async. Each platform (JVM, PHP, JS, etc.)
   implements the protocols defined here.

   NOT to be confused with clojure.concurrent which provides
   lower-level primitives (futures, latches, executors) used by core.

   Maps to:
   - JVM: clojure.core.async
   - PHP: ReactPHP, Amp, or Swoole channels
   - JS: Promises, async/await, or channels library")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol IChannel
  "A channel for async communication."
  (-put! [ch val handler] "Async put, calls handler when complete")
  (-take! [ch handler] "Async take, calls handler with value")
  (-close! [ch] "Close the channel")
  (-closed? [ch] "Returns true if channel is closed"))

(defprotocol IBuffer
  "A buffer for channels."
  (-full? [buf] "Returns true if buffer is full")
  (-add! [buf val] "Add value to buffer")
  (-remove! [buf] "Remove and return oldest value"))

(defprotocol IMult
  "A broadcast multiplexer."
  (-tap [m ch close?] "Add ch as a tap")
  (-untap [m ch] "Remove ch as a tap")
  (-untap-all [m] "Remove all taps"))

(defprotocol IPub
  "A topic-based pub/sub."
  (-sub [p topic ch close?] "Subscribe ch to topic")
  (-unsub [p topic ch] "Unsubscribe ch from topic")
  (-unsub-all [p] [p topic] "Unsubscribe all from topic or all topics"))

(defprotocol AsyncEngine
  "Async operations engine.
   Backends must provide an implementation of this protocol."

  ;; Channels
  (-chan [e] [e buf-or-n] [e buf-or-n xform] [e buf-or-n xform ex-handler]
    "Create a channel with optional buffer and transducer")

  ;; Buffers
  (-buffer [e n] "Fixed buffer of size n")
  (-dropping-buffer [e n] "Dropping buffer of size n")
  (-sliding-buffer [e n] "Sliding buffer of size n")

  ;; Blocking operations
  (-put!! [e ch val] "Blocking put")
  (-take!! [e ch] "Blocking take")

  ;; Alts
  (-alts! [e ports opts] "Parking alts for go blocks")
  (-alts!! [e ports opts] "Blocking alts")

  ;; Timeout
  (-timeout [e msecs] "Returns channel that closes after msecs")

  ;; Threading
  (-thread [e f] "Execute f in new thread, return channel with result")

  ;; Go blocks
  (-go [e body-fn] "Execute body in go block, return channel with result")

  ;; Mult
  (-mult [e ch] "Create mult on channel")

  ;; Pub
  (-pub [e ch topic-fn] [e ch topic-fn buf-fn] "Create pub on channel")

  ;; Higher-level
  (-pipe [e from to close?] "Pipe from channel to channel")
  (-merge [e chs buf-or-n] "Merge channels into one"))

;;; ====================================================================
;;; Dynamic vars
;;; ====================================================================

(def ^:dynamic *async-engine* nil)

;;; ====================================================================
;;; Portable API - Channels
;;; ====================================================================

(defn chan
  "Creates a channel with an optional buffer.
   buf-or-n can be:
   - nil or 0: unbuffered (rendezvous)
   - positive integer: fixed buffer of that size
   - a buffer created by buffer, dropping-buffer, or sliding-buffer"
  ([]
   (-chan *async-engine*))
  ([buf-or-n]
   (-chan *async-engine* buf-or-n))
  ([buf-or-n xform]
   (-chan *async-engine* buf-or-n xform))
  ([buf-or-n xform ex-handler]
   (-chan *async-engine* buf-or-n xform ex-handler)))

(defn buffer
  "Returns a fixed buffer of size n."
  [n]
  (-buffer *async-engine* n))

(defn dropping-buffer
  "Returns a buffer of size n. When full, puts complete but val is dropped."
  [n]
  (-dropping-buffer *async-engine* n))

(defn sliding-buffer
  "Returns a buffer of size n. When full, oldest vals are dropped."
  [n]
  (-sliding-buffer *async-engine* n))

(defn close!
  "Closes a channel. Pending takes will return nil."
  [ch]
  (-close! ch))

(defn closed?
  "Returns true if channel is closed."
  [ch]
  (-closed? ch))

;;; ====================================================================
;;; Portable API - Blocking operations (for use outside go blocks)
;;; ====================================================================

(defn >!!
  "Puts val into channel, blocking until complete.
   Returns true unless channel is closed."
  [ch val]
  (-put!! *async-engine* ch val))

(defn <!!
  "Takes a val from channel, blocking until available.
   Returns nil if closed."
  [ch]
  (-take!! *async-engine* ch))

;;; ====================================================================
;;; Portable API - Parking operations (for use in go blocks)
;;; ====================================================================

(defn >!
  "Puts val into channel. Must be called inside a go block.
   Parks until complete."
  [ch val]
  ;; Note: actual implementation requires go macro transformation
  ;; This is a placeholder that will be transformed by the go macro
  (throw (ex-info ">! called outside go block" {:ch ch :val val})))

(defn <!
  "Takes a val from channel. Must be called inside a go block.
   Parks until available."
  [ch]
  ;; Note: actual implementation requires go macro transformation
  (throw (ex-info "<! called outside go block" {:ch ch})))

;;; ====================================================================
;;; Portable API - Go blocks
;;; ====================================================================

(defmacro go
  "Asynchronously executes the body, returning immediately to the calling thread.
   Returns a channel which will receive the result of the body when completed.
   Parking operations (>!, <!, alts!) are allowed inside."
  [& body]
  `(-go *async-engine* (fn [] ~@body)))

(defmacro go-loop
  "Like (go (loop ...))."
  [bindings & body]
  `(go (loop ~bindings ~@body)))

;;; ====================================================================
;;; Portable API - Thread
;;; ====================================================================

(defmacro thread
  "Executes the body in a new thread. Returns a channel with the result."
  [& body]
  `(-thread *async-engine* (fn [] ~@body)))

;;; ====================================================================
;;; Portable API - Alts
;;; ====================================================================

(defn alts!
  "Completes at most one operation from ports.
   ports is a vector of channels or [channel val] for puts.
   Must be called inside a go block.
   Returns [val channel] for takes, [true channel] for puts.

   Options:
   :default val - return [val :default] if no operation ready
   :priority true - try operations in order"
  [ports & {:as opts}]
  (-alts! *async-engine* ports opts))

(defn alts!!
  "Like alts! but blocking. For use outside go blocks."
  [ports & {:as opts}]
  (-alts!! *async-engine* ports opts))

(defmacro alt!
  "Like alts! but as a macro with case-like syntax.

   (alt!
     ch1 ([v] (handle v))
     ch2 ([v] (other v))
     :default :nothing-ready)"
  [& clauses]
  ;; This is a simplified version - full implementation would parse clauses
  `(let [[val# port#] (alts! ~(vec (take-nth 2 clauses)))]
     val#))

(defmacro alt!!
  "Like alt! but blocking."
  [& clauses]
  `(let [[val# port#] (alts!! ~(vec (take-nth 2 clauses)))]
     val#))

;;; ====================================================================
;;; Portable API - Timeout
;;; ====================================================================

(defn timeout
  "Returns a channel that will close after msecs milliseconds."
  [msecs]
  (-timeout *async-engine* msecs))

;;; ====================================================================
;;; Portable API - Async put/take
;;; ====================================================================

(defn put!
  "Asynchronously puts val into channel, calling fn1 (if supplied) when complete.
   Returns true unless channel is already closed."
  ([ch val]
   (-put! ch val (fn [_])))
  ([ch val fn1]
   (-put! ch val fn1))
  ([ch val fn1 on-caller?]
   ;; on-caller? hint is platform-specific
   (-put! ch val fn1)))

(defn take!
  "Asynchronously takes from channel, calling fn1 with value when available."
  ([ch fn1]
   (-take! ch fn1))
  ([ch fn1 on-caller?]
   ;; on-caller? hint is platform-specific
   (-take! ch fn1)))

(defn offer!
  "Puts val into channel if immediately possible.
   Returns true if successful, false if channel full or closed."
  [ch val]
  ;; Platform-specific implementation may optimize this
  (let [result (atom false)]
    (-put! ch val (fn [success] (reset! result success)))
    @result))

(defn poll!
  "Takes value from channel if immediately available.
   Returns value or nil."
  [ch]
  ;; Platform-specific implementation may optimize this
  (let [result (atom nil)]
    (-take! ch (fn [val] (reset! result val)))
    @result))

;;; ====================================================================
;;; Portable API - Higher-level operations
;;; ====================================================================

(defn onto-chan!
  "Puts the contents of coll into chan, then optionally closes chan.
   Returns a channel which closes when complete."
  ([ch coll]
   (onto-chan! ch coll true))
  ([ch coll close?]
   (go
     (doseq [v coll]
       (>! ch v))
     (when close?
       (close! ch)))))

(defn to-chan!
  "Creates and returns a channel which contains the contents of coll."
  [coll]
  (let [ch (chan)]
    (onto-chan! ch coll)
    ch))

(defn pipe
  "Takes elements from from and puts them to to.
   Optionally closes to when from is closed (default true)."
  ([from to]
   (pipe from to true))
  ([from to close?]
   (-pipe *async-engine* from to close?)))

(defn merge
  "Takes a collection of source channels and returns a channel which
   contains all values from all sources. Closes when all sources close."
  ([chs]
   (-merge *async-engine* chs nil))
  ([chs buf-or-n]
   (-merge *async-engine* chs buf-or-n)))

(defn into
  "Returns a channel containing a single collection of all items taken from ch.
   ch must close for the result to be delivered."
  [coll ch]
  (go
    (loop [acc coll]
      (let [v (<! ch)]
        (if (nil? v)
          acc
          (recur (conj acc v)))))))

(defn reduce
  "Reduces over ch with f and init. Returns a channel with the result.
   ch must close for the result to be delivered."
  [f init ch]
  (go
    (loop [acc init]
      (let [v (<! ch)]
        (if (nil? v)
          acc
          (recur (f acc v)))))))

(defn transduce
  "Reduces over ch with xform, f, and init. Returns a channel with the result."
  [xform f init ch]
  (let [rf (xform f)]
    (go
      (loop [acc init]
        (let [v (<! ch)]
          (if (nil? v)
            (rf acc)
            (let [result (rf acc v)]
              (if (reduced? result)
                @result
                (recur result)))))))))

;;; ====================================================================
;;; Portable API - Mult (broadcast)
;;; ====================================================================

(defn mult
  "Creates a mult on the supplied channel. A mult supports multiple taps.
   Each item on the source channel will be put on all taps."
  [ch]
  (-mult *async-engine* ch))

(defn tap
  "Connects ch to the mult. Values from the mult will be put onto ch.
   Optionally close ch when mult source closes (default true)."
  ([m ch]
   (tap m ch true))
  ([m ch close?]
   (-tap m ch close?)))

(defn untap
  "Disconnects ch from the mult."
  [m ch]
  (-untap m ch))

(defn untap-all
  "Disconnects all channels from the mult."
  [m]
  (-untap-all m))

;;; ====================================================================
;;; Portable API - Pub/Sub
;;; ====================================================================

(defn pub
  "Creates a pub on the supplied channel with topic-fn to extract the topic.
   Optionally buf-fn to specify buffer for each topic."
  ([ch topic-fn]
   (-pub *async-engine* ch topic-fn))
  ([ch topic-fn buf-fn]
   (-pub *async-engine* ch topic-fn buf-fn)))

(defn sub
  "Subscribes ch to topic on the pub.
   Optionally close ch when pub closes (default true)."
  ([p topic ch]
   (sub p topic ch true))
  ([p topic ch close?]
   (-sub p topic ch close?)))

(defn unsub
  "Unsubscribes ch from topic on the pub."
  [p topic ch]
  (-unsub p topic ch))

(defn unsub-all
  "Unsubscribes all channels from topic (or all topics if not supplied)."
  ([p]
   (-unsub-all p))
  ([p topic]
   (-unsub-all p topic)))
