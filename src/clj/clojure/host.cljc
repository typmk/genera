;; Copyright (c) Rich Hickey. All rights reserved.
;; Eclipse Public License 1.0

(ns clojure.host
  "The platform boundary. ONE protocol to rule them all.

   Implement this protocol for your platform, get all of Clojure.

   Philosophy: Embrace the host. The host is a first-class citizen.
   These are thin wrappers over native platform capabilities.

   This replaces the scattered *engine* vars with a unified approach:
   - clojure.time/*time-engine*   → clojure.host/*host*
   - clojure.io/*io-engine*       → clojure.host/*host*
   - clojure.regex/*regex-engine* → clojure.host/*host*
   - etc.

   To add a new platform:
   1. Create clojure.<platform>/host.cljc (e.g., clojure.wasm.host)
   2. Implement the Host protocol with native calls
   3. Call (set-host! your-host) at startup
   4. Done. Everything works.")

;; =============================================================================
;; The Host Protocol
;; =============================================================================

(defprotocol Host
  "Everything a platform must provide to run Clojure.

   Grouped by capability. Each method is a thin wrapper
   over native platform functionality."

  ;; ---------------------------------------------------------------------------
  ;; Arrays (for persistent data structures)
  ;; ---------------------------------------------------------------------------
  (-array [h size] "Create array of given size")
  (-array-of [h size val] "Create array filled with val")
  (-aget [h arr i] "Get element at index")
  (-aset [h arr i v] "Set element at index, return arr")
  (-alength [h arr] "Get array length")
  (-acopy [h src src-pos dest dest-pos len] "Copy array region")
  (-aclone [h arr] "Clone array")
  (-asort [h arr] [h arr comparator] "Sort array in place")
  (-shuffle [h coll] "Return shuffled vector from coll")

  ;; ---------------------------------------------------------------------------
  ;; Atoms (for reference types)
  ;; ---------------------------------------------------------------------------
  (-atom [h val] "Create atomic reference")
  (-atom-deref [h a] "Read atomic value")
  (-atom-reset [h a val] "Set atomic value, return new val")
  (-atom-cas [h a old new] "Compare-and-set, return success boolean")

  ;; ---------------------------------------------------------------------------
  ;; Identity & Hashing
  ;; ---------------------------------------------------------------------------
  (-identical? [h a b] "Reference equality")
  (-equiv [h a b] "Value equality")
  (-hash [h x] "Hash code for value")
  (-hash-combine [h seed hash] "Combine two hashes")

  ;; ---------------------------------------------------------------------------
  ;; Time
  ;; ---------------------------------------------------------------------------
  (-time-now-ms [h] "Current time as milliseconds since epoch")
  (-time-now [h] "Current time as instant object")
  (-time-instant-ms [h instant] "Get milliseconds from instant")
  (-time-from-ms [h ms] "Create instant from milliseconds")
  (-time-parse [h s] "Parse ISO-8601 string to instant")
  (-time-format [h instant] "Format instant to ISO-8601 string")

  ;; ---------------------------------------------------------------------------
  ;; IO
  ;; ---------------------------------------------------------------------------
  (-io-slurp [h path] "Read entire file as string")
  (-io-spit [h path content] "Write string to file")
  (-io-exists? [h path] "Check if path exists")
  (-io-directory? [h path] "Check if path is directory")
  (-io-mkdir [h path] "Create directory (and parents)")
  (-io-ls [h path] "List directory contents")
  (-io-rm [h path] "Delete file")
  (-io-rmdir [h path] "Delete directory recursively")
  (-io-mv [h from to] "Move/rename file")
  (-io-cp [h from to] "Copy file")
  (-io-cwd [h] "Current working directory")
  (-io-tmp [h] "System temp directory")

  ;; ---------------------------------------------------------------------------
  ;; Regex
  ;; ---------------------------------------------------------------------------
  (-re-pattern [h s] "Compile regex pattern")
  (-re-matches [h pat s] "Full string match, return groups or nil")
  (-re-find [h pat s] "Find first match, return match or nil")
  (-re-find-all [h pat s] "Find all matches, return seq")
  (-re-split [h pat s] "Split string by pattern")
  (-re-replace [h pat s replacement] "Replace matches")

  ;; ---------------------------------------------------------------------------
  ;; UUID
  ;; ---------------------------------------------------------------------------
  (-uuid-random [h] "Generate random UUID")
  (-uuid-from-string [h s] "Parse UUID from string")
  (-uuid-to-string [h uuid] "Format UUID to string")

  ;; ---------------------------------------------------------------------------
  ;; URI
  ;; ---------------------------------------------------------------------------
  (-uri-parse [h s] "Parse URI from string, return map")
  (-uri-to-string [h uri] "Format URI to string")

  ;; ---------------------------------------------------------------------------
  ;; Process
  ;; ---------------------------------------------------------------------------
  (-proc-exec [h cmd args opts] "Execute command, wait, return {:exit :out :err}")
  (-proc-spawn [h cmd args opts] "Spawn process, return process handle")
  (-proc-kill [h proc] "Kill process")
  (-proc-wait [h proc] "Wait for process, return exit code")
  (-env-get [h name] "Get environment variable")
  (-env-all [h] "Get all environment variables as map")

  ;; ---------------------------------------------------------------------------
  ;; Math
  ;; ---------------------------------------------------------------------------
  (-math-floor [h n] "Floor")
  (-math-ceil [h n] "Ceiling")
  (-math-round [h n] "Round")
  (-math-pow [h base exp] "Exponentiation")
  (-math-sqrt [h n] "Square root")
  (-math-log [h n] "Natural log")
  (-math-sin [h n] "Sine")
  (-math-cos [h n] "Cosine")
  (-math-random [h] "Random double 0.0-1.0")

  ;; ---------------------------------------------------------------------------
  ;; Concurrent
  ;; ---------------------------------------------------------------------------
  (-conc-thread [h f] "Run f in new thread, return handle")
  (-conc-sleep [h ms] "Sleep current thread")
  (-conc-future [h f] "Run f async, return future")
  (-conc-deref [h fut] "Block and get future value")
  (-conc-deref-timeout [h fut ms timeout-val] "Deref with timeout")
  (-conc-realized? [h fut] "Check if future completed")

  ;; ---------------------------------------------------------------------------
  ;; String (platform-optimized)
  ;; ---------------------------------------------------------------------------
  (-str-length [h s] "String length")
  (-str-subs [h s start] [h s start end] "Substring")
  (-str-index-of [h s sub] "Find substring index or -1")
  (-str-upper [h s] "Uppercase")
  (-str-lower [h s] "Lowercase")
  (-str-trim [h s] "Trim whitespace")
  (-str-split [h s re] "Split by regex")
  (-str-join [h sep coll] "Join with separator")
  (-str-replace [h s match replacement] "Replace all")

  ;; ---------------------------------------------------------------------------
  ;; Interop (for host type system)
  ;; ---------------------------------------------------------------------------
  (-type-name [h x] "Get type/class name")
  (-instance? [h type x] "Check if x is instance of type")
  (-invoke [h obj method args] "Invoke method")
  (-get-field [h obj field] "Get field value")
  (-set-field [h obj field val] "Set field value")
  (-construct [h class args] "Construct new instance")

  ;; ---------------------------------------------------------------------------
  ;; Mutable Buffer (for transducers)
  ;; ---------------------------------------------------------------------------
  (-make-buffer [h] [h capacity] "Create mutable buffer")
  (-buf-add [h buf val] "Add element to buffer, return buf")
  (-buf-empty? [h buf] "Check if buffer is empty")
  (-buf-size [h buf] "Get buffer size")
  (-buf-to-array [h buf] "Convert buffer to array")
  (-buf-clear [h buf] "Clear buffer, return buf")

  ;; ---------------------------------------------------------------------------
  ;; Type predicates (for core.cljc)
  ;; ---------------------------------------------------------------------------
  (-string? [h x] "Check if x is a string")
  (-char? [h x] "Check if x is a character")
  (-number? [h x] "Check if x is a number")
  (-integer? [h x] "Check if x is an integer")
  (-float? [h x] "Check if x is a float")

  ;; ---------------------------------------------------------------------------
  ;; String builders (for efficient str concatenation)
  ;; ---------------------------------------------------------------------------
  (-to-string [h x] "Convert x to string")
  (-string-builder [h] [h s] "Create string builder")
  (-sb-append [h sb s] "Append to string builder")
  (-sb-to-string [h sb] "Convert string builder to string")

  ;; ---------------------------------------------------------------------------
  ;; Function application
  ;; ---------------------------------------------------------------------------
  (-apply-to [h f args] "Apply function to argument seq")
  (-to-array [h coll] "Convert collection to array")

  ;; ---------------------------------------------------------------------------
  ;; Comparison
  ;; ---------------------------------------------------------------------------
  (-compare [h x y] "Compare two values, return -1, 0, or 1")

  ;; ---------------------------------------------------------------------------
  ;; Arithmetic (platform math primitives)
  ;; ---------------------------------------------------------------------------
  (-add [h x y] "Add two numbers")
  (-subtract [h x y] "Subtract y from x")
  (-multiply [h x y] "Multiply two numbers")
  (-divide [h x y] "Divide x by y")
  (-negate [h x] "Negate a number")
  (-inc [h x] "Increment by 1")
  (-dec [h x] "Decrement by 1")
  (-lt [h x y] "Less than")
  (-lte [h x y] "Less than or equal")
  (-gt [h x y] "Greater than")
  (-gte [h x y] "Greater than or equal")
  (-num-equiv [h x y] "Numeric equality (type-independent)")
  (-zero? [h x] "Check if zero")
  (-pos? [h x] "Check if positive")
  (-neg? [h x] "Check if negative")
  (-quot [h x y] "Integer quotient")
  (-rem [h x y] "Remainder")
  (-mod [h x y] "Modulus (always positive)")
  (-bit-and [h x y] "Bitwise AND")
  (-bit-or [h x y] "Bitwise OR")
  (-bit-xor [h x y] "Bitwise XOR")
  (-bit-not [h x] "Bitwise NOT")
  (-bit-shift-left [h x n] "Left shift")
  (-bit-shift-right [h x n] "Right shift (signed)")
  (-unsigned-bit-shift-right [h x n] "Right shift (unsigned)"))

;; =============================================================================
;; Host Registry
;; =============================================================================

(def ^:dynamic *host*
  "The current host implementation. Set at startup."
  nil)

(defn set-host!
  "Set the global host implementation."
  [h]
  (alter-var-root #'*host* (constantly h)))

(defn host
  "Get current host. Throws if not set."
  []
  (or *host*
      (throw (ex-info "No host set. Call (clojure.host/set-host! h) first." {}))))

;; =============================================================================
;; Convenience functions (used by types/ and abstractions)
;; =============================================================================

;; Arrays
(defn array [size] (-array (host) size))
(defn array-of [size val] (-array-of (host) size val))
(defn aget [arr i] (-aget (host) arr i))
(defn aset [arr i v] (-aset (host) arr i v))
(defn alength [arr] (-alength (host) arr))
(defn acopy [src sp dest dp len] (-acopy (host) src sp dest dp len))
(defn aclone [arr] (-aclone (host) arr))
(defn asort
  ([arr] (-asort (host) arr))
  ([arr comparator] (-asort (host) arr comparator)))
(defn shuffle [coll] (-shuffle (host) coll))

;; Atoms
(defn atom* [val] (-atom (host) val))
(defn atom-deref [a] (-atom-deref (host) a))
(defn atom-reset [a val] (-atom-reset (host) a val))
(defn atom-cas [a old new] (-atom-cas (host) a old new))

;; Identity
(defn identical? [a b] (-identical? (host) a b))
(defn hash* [x] (-hash (host) x))
(defn hash-combine [seed h] (-hash-combine (host) seed h))

;; Buffers
(defn make-buffer
  ([] (-make-buffer (host)))
  ([capacity] (-make-buffer (host) capacity)))
(defn buf-add [buf val] (-buf-add (host) buf val))
(defn buf-empty? [buf] (-buf-empty? (host) buf))
(defn buf-size [buf] (-buf-size (host) buf))
(defn buf-to-array [buf] (-buf-to-array (host) buf))
(defn buf-clear [buf] (-buf-clear (host) buf))
