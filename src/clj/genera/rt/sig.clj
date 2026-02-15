;; sig.clj — Dispatch as Facts About Names
;;
;; Pure Clojure. JIT-compiled to native x86.
;;
;; sig dissolves into the same machinery as everything else:
;;   StrId and GNode are both u32 indices.
;;   Both have bitmask facts (BM_GET/POPCOUNT/scan).
;;   Dispatch = v_handler[name] + parent walk.
;;   Tap = v_dispatched bitmask.
;;   No separate dispatch table — just views on names.
;;
;; The C version is ~30 lines (g_sig_table[256] flat array).
;; The Clojure version uses the SAME structure but expresses
;; it as the general pattern: scan → predicate → action.
;;
;; Handler storage: a flat array of Val indexed by StrId.
;; (In practice: a Mem arena with Val[SIG_CAP] at known offset.)
;;
;; Parent walk for hierarchical dispatch:
;;   page:/about → page: → page → *
;;   The ':' and '/' boundaries define parent names.
;;   Parent of StrId can be computed from the interned string.

;; ================================================================
;; Constants
;; ================================================================

(def SIG_CAP 256)          ;; max handler slots
(def SIG_HANDLER_OFF 0)    ;; Val[] at offset 0 in sig arena
(def SIG_FLAGS_OFF 2048)   ;; u8[] at offset 2048 (256 × 8 = 2048)

(def FLAG_SPECIAL 0x01)

;; ================================================================
;; Core operations
;; ================================================================
;;
;; sig_base: pointer to the sig arena (set at init)
;; All operations are pointer-relative loads/stores.

;; Register handler for name
(defn sig-on! [sig-base name handler flags]
  (when (< name SIG_CAP)
    (store64! sig-base (* name 8) handler)
    (store32! sig-base (+ SIG_FLAGS_OFF name) flags)))

;; Get handler for name (direct lookup, no parent walk)
(defn sig-get [sig-base name]
  (if (< name SIG_CAP)
    (load64 sig-base (* name 8))
    nil))

;; Remove handler
(defn sig-off! [sig-base name]
  (when (< name SIG_CAP)
    (store64! sig-base (* name 8) 0)))

;; Check if name is special form
(defn sig-special? [sig-base name]
  (and (< name SIG_CAP)
       (not (zero? (bit-and (load32 sig-base (+ SIG_FLAGS_OFF name))
                             FLAG_SPECIAL)))))

;; ================================================================
;; Hierarchical dispatch
;; ================================================================
;;
;; send(name, data):
;;   1. Try exact handler: sig-get(name)
;;   2. If nil, try parent: sig-get(parent(name))
;;   3. If nil, try parent of parent: ...
;;   4. If nil, try * (wildcard): sig-get(SYM_STAR)
;;
;; Parent computation:
;;   "page:/about" → strip after last '/' → "page:"
;;   "page:"       → strip after last ':' → "page"
;;   "page"        → no more separators → try "*"
;;
;; This needs str-parent as a builtin or Clojure fn that
;; operates on the intern table. For bootstrap, it's a C builtin.
;; Once self-hosting, it reads the intern table directly.

(defn sig-send [sig-base name data]
  (loop [n name]
    (let [handler (sig-get sig-base n)]
      (if (not (zero? handler))
        handler  ;; found — caller invokes it
        ;; Try parent
        (let [parent (str-parent n)]
          (if (= parent n)
            ;; No more parents — try wildcard
            (sig-get sig-base SYM_STAR)
            (recur parent)))))))

;; ================================================================
;; Observation (tap)
;; ================================================================
;;
;; Tap is a bitmask: v_dispatched[name] = 1 after dispatch.
;; Same as any other view bit. Queried the same way.
;;
;; obs-reset: clear the bitmask
;; obs-hit?: check if name was dispatched
;;
;; In the Mem model, the bitmask is bump-allocated (SIG_CAP / 8 bytes).
;; Or just a u64[4] for 256 bits.

(def OBS_OFF 2304)         ;; After flags: 2048 + 256 = 2304
(def OBS_WORDS 4)          ;; 4 × u64 = 256 bits

(defn obs-mark! [sig-base name]
  (when (< name SIG_CAP)
    (let [word (bit-shift-right name 6)
          bit  (bit-shift-left 1 (bit-and name 63))
          off  (+ OBS_OFF (* word 8))
          old  (load64 sig-base off)]
      (store64! sig-base off (bit-or old bit)))))

(defn obs-hit? [sig-base name]
  (if (>= name SIG_CAP) false
    (let [word (bit-shift-right name 6)
          bit  (bit-shift-left 1 (bit-and name 63))
          val  (load64 sig-base (+ OBS_OFF (* word 8)))]
      (not (zero? (bit-and val bit))))))

(defn obs-reset! [sig-base]
  (loop [i 0]
    (when (< i OBS_WORDS)
      (store64! sig-base (+ OBS_OFF (* i 8)) 0)
      (recur (inc i)))))

;; ================================================================
;; Dispatch with observation
;; ================================================================
;;
;; sig-dispatch = sig-send + obs-mark.
;; This is the complete dispatch path:
;;   1. Find handler (with parent walk)
;;   2. Mark name as dispatched (for observation)
;;   3. Return handler (caller invokes with data)

(defn sig-dispatch [sig-base name data]
  (obs-mark! sig-base name)
  (sig-send sig-base name data))
