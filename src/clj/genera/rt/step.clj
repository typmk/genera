;; step.clj — The Step Loop
;;
;; Pure Clojure. JIT-compiled to native x86.
;; Uses every memory primitive from the other .clj files.
;;
;; This is the REPL/eval/step boundary — the heartbeat of the system.
;; world → step → world. One operation. No modes.
;;
;; The step loop:
;;   1. MARK g_step (save current position)
;;   2. Parse source → GNodes
;;   3. Analyze (passes from boot.clj + alloc.clj)
;;   4. Eval (interpreted) or JIT-compile + run (compiled)
;;   5. If def: commit result to g_main (commit.clj)
;;   6. RESTORE g_step (free all step temporaries)
;;   7. If g_coll pressure: epoch-rotate (epoch.clj)
;;   8. Advance g_world.version
;;
;; 90%+ of allocations are step temps (cons cells from eval,
;; intermediate lists, env frames). They die at step 6.
;; Only def'd values survive (via commit at step 5).

;; ================================================================
;; Mem globals (set by runtime at init)
;; ================================================================
;;
;; These are JIT globals — the JIT accesses them via x86-load-abs!.
;; In interpreted mode, they're passed as arguments.
;;
;; G_STEP: Mem* for step-local allocations (bump, RESTORE per step)
;; G_MAIN: Mem* for committed values (survives steps)
;; G_COLL: Mem*[2] for collection nodes (epoch pair)
;; G_EPOCH: u32, current epoch index (0 or 1)
;;
;; (def G_STEP  (jit-register-global! :g-step))
;; (def G_MAIN  (jit-register-global! :g-main))
;; (def G_COLL0 (jit-register-global! :g-coll-0))
;; (def G_COLL1 (jit-register-global! :g-coll-1))
;; (def G_EPOCH (jit-register-global! :g-epoch))

;; ================================================================
;; Epoch pressure heuristic
;; ================================================================
;;
;; Trigger epoch rotation when coll arena is >75% full.
;; After rotation, the old epoch is RESTORE'd to 0.
;; Live nodes (reachable from global env) have been copied
;; to the other epoch by epoch-rotate (epoch.clj).

(def COLL_THRESHOLD_PCT 75)

(defn coll-pressure? [coll-ptr]
  (let [used (load32 coll-ptr MEM_USED)
        cap  (load32 coll-ptr MEM_CAP)]
    (> (* used 100) (* cap COLL_THRESHOLD_PCT))))

;; ================================================================
;; world-step: the fundamental operation
;; ================================================================
;;
;; Input:  source string, g_step Mem*, g_main Mem*, g_coll Mem*
;; Output: result Val (displayed or discarded)
;; Effect: g_world.version incremented, maybe epoch rotated
;;
;; This function IS the step boundary. Everything below it
;; (parse, analyze, eval, commit) are subordinate.
;;
;; For the interpreted path (bootstrap):
;;   Uses eval_node from C, commit from commit.clj
;;
;; For the JIT path (steady state):
;;   JIT compiles the source, executes native x86
;;   commit compiled by JIT from commit.clj
;;
;; Both paths share: MARK/RESTORE on g_step, epoch check

(defn world-step [step-ptr main-ptr coll-ptr epoch
                  source eval-fn commit-fn]
  ;; 1. MARK
  (let [saved (mark step-ptr)]

    ;; 2-4. Parse + analyze + eval (delegated to eval-fn)
    ;; eval-fn: source → Val (handles parse, analyze, eval internally)
    (let [result (eval-fn source)]

      ;; 5. Check if this is a def (result needs commitment)
      ;; The eval-fn returns a tagged value. If it's a def binding,
      ;; commit-fn promotes it from step to main arena.
      ;; For non-def expressions, result is just displayed.
      (when commit-fn
        (commit-fn result main-ptr))

      ;; 6. RESTORE
      (restore! step-ptr saved)

      ;; 7. Epoch check
      (when (coll-pressure? coll-ptr)
        ;; epoch-rotate is defined in epoch.clj
        ;; It copies live coll nodes to the other epoch,
        ;; RESTORE's the current one, and returns the new epoch.
        ;; (epoch-rotate env coll-pair epoch)
        nil)  ;; TODO: wire epoch-rotate once env walking is available

      ;; 8. Return result
      result)))

;; ================================================================
;; REPL loop
;; ================================================================
;;
;; The REPL is world-step in a loop with I/O.
;;
;; loop:
;;   print prompt
;;   read line
;;   world-step(line)
;;   print result
;;
;; In the JIT path, the REPL itself can be JIT-compiled.
;; But it starts interpreted (C reads stdin, calls world-step).
;;
;; The REPL also handles:
;;   - :commands (special REPL commands, dispatched via sig)
;;   - Multi-form input (eval each top-level form)
;;   - Error recovery (catch signals, print, continue)

(defn repl-step [step-ptr main-ptr coll-ptr epoch
                 source eval-fn commit-fn print-fn]
  (let [result (world-step step-ptr main-ptr coll-ptr epoch
                           source eval-fn commit-fn)]
    (when print-fn
      (print-fn result))
    result))

;; ================================================================
;; Batch evaluation (load file)
;; ================================================================
;;
;; Load a .clj file: parse all forms, eval each as a step.
;; Each top-level form gets its own MARK/RESTORE cycle.
;; def/defn forms commit. Other forms eval and discard.

(defn eval-file [step-ptr main-ptr coll-ptr epoch
                 forms eval-fn commit-fn]
  (loop [f forms result nil]
    (if (nil? f) result
      (let [r (world-step step-ptr main-ptr coll-ptr epoch
                          (first f) eval-fn commit-fn)]
        (recur (rest f) r)))))

;; ================================================================
;; Step budget (from alloc.clj)
;; ================================================================
;;
;; Before eval, alloc.clj's passes compute the static allocation
;; budget for the step. If the budget exceeds g_step remaining
;; capacity, we can:
;;   a) Grow g_step (re-mmap)
;;   b) Reject the step (error: too large)
;;   c) Stream-process (eval + commit incrementally)
;;
;; For now: simple check, reject if too large.

(defn step-fits? [step-ptr budget]
  (let [used (load32 step-ptr MEM_USED)
        cap  (load32 step-ptr MEM_CAP)]
    (<= (+ used budget) cap)))
