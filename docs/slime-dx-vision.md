# SLIME-like Developer Experience for ClojurePHP

## Philosophy

**Adding ClojurePHP should make development EASIER than plain PHP, not harder.**

This isn't "Clojure with PHP overhead" - it's "PHP made joyful through Clojure."

The bar: **Using ClojurePHP should be simpler, more helpful, and more productive than writing PHP directly.** If it adds friction, we've failed.

### The Value Proposition

| Plain PHP | With ClojurePHP |
|-----------|-----------------|
| Verbose, imperative | Concise, functional |
| Mutable by default | Immutable by default |
| No REPL | Interactive development |
| Stack traces in PHP | Errors point to your Clojure |
| Manual debugging | Inspector, macroexpand |
| Edit-save-refresh | Hot reload, instant feedback |
| Framework lock-in | Drop-in anywhere PHP runs |

**You never leave Clojure.** PHP is invisible - just the fast, ubiquitous runtime underneath.

---

## The Symbiosis: Clojure + PHP = More Than Either Alone

ClojurePHP isn't just "Clojure on PHP" - it's a **mutual enrichment** of both ecosystems.

```
┌─────────────────────────────────────────────────────────────┐
│                    The Union                                │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  PHP gains:                 Clojure gains:                  │
│  ──────────                 ─────────────                   │
│  • REPL                     • Runs everywhere PHP runs      │
│  • Live image               • $5/month shared hosting       │
│  • Immutable data           • WordPress/Laravel ecosystem   │
│  • Macros                   • Composer packages             │
│  • Functional idioms        • PHP extensions (imagick, etc) │
│  • Interactive dev          • No JVM baggage                │
│  • Hot reload               • Instant deployment            │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### ClojurePHP as a PHP Development Tool

The self-hosted runtime isn't just for ClojurePHP apps - it's a **power tool for any PHP project**:

```php
<?php
// Any Laravel/Symfony/WordPress project
require 'vendor/clojurephp/runtime/autoload.php';

$users = User::where('active', true)->get();

// Drop into Clojure for data transformation
$report = cljp('
  (->> $users
       (group-by :region)
       (map-vals (fn [users]
                   {:count (count users)
                    :total (reduce + (map :revenue users))})))
', ['users' => $users]);

return view('report', ['data' => $report]);
```

### Bidirectional Integration

**From PHP → ClojurePHP:**
```php
<?php
require 'cljp/runtime.php';

// 1. Inline Clojure expressions
$total = cljp('(reduce + (range 100))');

// 2. Call compiled cljp namespaces
use function Cljp\myapp\core\{calculate_total, process_order};
$result = process_order($order);

// 3. Use cljp data structures in PHP
$vec = cljp('(vec (range 10))');
$vec = $vec->conj(10)->conj(11);  // immutable ops

// 4. Variable binding - PHP vars available with $ prefix
$items = [['price' => 10, 'qty' => 2]];
$total = cljp('(->> $items (map #(* (:price %) (:qty %))) (reduce +))');
```

**From ClojurePHP → PHP:**
```clojure
;; Inline PHP evaluation
cljp> (php "return array_map(fn($x) => $x * 2, [1,2,3]);")
=> [2 4 6]

;; Call PHP classes and methods
cljp> (php/require "vendor/autoload.php")
cljp> (def user (php/new App\Models\User))
cljp> (-> (php/:: User find 42) (php/-> posts) (php/-> count))
=> 17

;; PHP objects as Clojure data
cljp> (php/props user)
=> {:id 42 :name "Alice" :email "alice@example.com"}
```

### The Adoption Spectrum

```
Pure PHP ◄────────────────────────────────────────────► Pure cljp

  │           │              │              │              │
  │      cljp('...')    use Cljp\*     .cljc files     full
  │      inline         functions      + PHP interop    cljp app
  │
  ▼                                                       ▼
Laravel app          Mixed codebase               Swoole live image
sprinkles cljp       growing toward cljp          pure Clojure DX
```

Each step is natural. No big rewrites. PHP devs can stop anywhere on that spectrum.

### REPL Tools for PHP

The same REPL that powers ClojurePHP development becomes a **PHP power tool**:

```clojure
;; Explore any PHP codebase interactively
cljp> (php/require "vendor/autoload.php")
cljp> (php/require "bootstrap/app.php")

;; Inspect PHP objects
cljp> (inspect (php/new App\Services\PaymentGateway))

;; List available methods
cljp> (php/methods 'App\Models\User)
=> [find create update delete where with ...]

;; Quick benchmarking
cljp> (phpbench
        (php "$sum = 0; for ($i = 0; $i < 1000; $i++) $sum += $i;"))
=> {:mean 0.023ms :iterations 1000}

;; Compare implementations
cljp> (phpbench-compare
        {:php-loop   (php "for ($i=0; $i<1000; $i++) $sum += $i;")
         :php-reduce (php "array_reduce(range(0,999), fn($a,$b) => $a+$b, 0);")
         :cljp       (reduce + (range 1000))})
```

### Testing PHP from ClojurePHP

Use `clojure.test` to test PHP code:

```clojure
(ns myapp.php-test
  (:require [clojure.test :refer [deftest is testing]]))

(deftest user-model-test
  (php/require "vendor/autoload.php")

  (testing "find returns user"
    (let [user (php "return User::find(1);")]
      (is (= 1 (:id user)))))

  (testing "validation rejects bad email"
    (is (thrown? Exception
          (php "User::create(['email' => 'not-an-email']);")))))
```

### The PHP Toolbox

ClojurePHP becomes a **power platform** for PHP development - not just a language, but a suite of REPL-driven tools:

```
┌─────────────────────────────────────────────────────────────┐
│              ClojurePHP: The PHP Power Platform             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  EXPLORE            ANALYZE             TEST                │
│  ───────            ───────             ────                │
│  • REPL             • Complexity        • clojure.test      │
│  • Inspect          • Dependencies      • Property-based    │
│  • php/methods      • Code smells       • phpunit bridge    │
│  • php/props        • N+1 detection     • Coverage          │
│                                                             │
│  BENCHMARK          REFACTOR            MIGRATE             │
│  ─────────          ────────            ───────             │
│  • phpbench         • Rename            • php->cljp         │
│  • Compare impls    • Extract           • Hybrid wrappers   │
│  • Profile          • Apply patterns    • Equivalence tests │
│                                                             │
│  All REPL-driven • All produce data • All composable        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Analysis tools:**
```clojure
;; Understand a codebase
cljp> (php/analyze-ns 'App\Services\OrderService)
=> {:methods 12
    :complexity {:cyclomatic 47 :cognitive 62}
    :dependencies [App\Models\Order App\Repositories\OrderRepo]
    :used-by [App\Http\Controllers\OrderController]}

;; Find problems
cljp> (php/smells 'App\Services\*)
=> [{:type :god-class :class "UserService" :methods 47}
    {:type :n-plus-one :file "ReportService.php" :line 45}]
```

**Migration assistance:**
```clojure
;; Translate PHP to idiomatic ClojurePHP
cljp> (php->cljp "
        function sum($items) {
          $total = 0;
          foreach ($items as $item) {
            $total += $item['price'];
          }
          return $total;
        }")
=> (defn sum [items]
     (reduce + (map :price items)))

;; Verify equivalence
cljp> (php/verify-equivalence
        {:php 'App\Services\Tax::calculate
         :cljp 'app.services.tax/calculate
         :inputs [[100 "US"] [100 "EU"] [0 "UK"]]})
=> {:status :pass :all-match true}
```

### Why This Matters

1. **Lower barrier** - PHP devs start with familiar code, get REPL superpowers immediately
2. **Trust through visibility** - See what ClojurePHP does, compare outputs, verify
3. **Incremental adoption** - No big bang rewrite, migrate function by function
4. **The tools sell the language** - Once you've used REPL-driven dev, you won't go back

### The Runtime Package

The self-hosted ClojurePHP becomes a composer package:

```bash
composer require clojurephp/runtime
```

```
vendor/clojurephp/runtime/
├── src/
│   ├── Runtime.php      # bootstrap, cljp() function
│   ├── Compiler.php     # JIT compile (dev) or load cached
│   ├── Core.php         # clojure.core as PHP
│   └── Lang/
│       ├── Vector.php
│       ├── HashMap.php
│       ├── Keyword.php
│       └── ...
└── compiled/            # pre-compiled cljp → PHP
```

This IS the self-hosted ClojurePHP - delivered as a drop-in package for any PHP project.

---

## The Endgame: Self-Hosting

ClojurePHP will eventually compile itself. The JVM is just the bootstrap.

```
┌─────────────────────────────────────────┐
│           PHP Image                      │
│                                          │
│  ┌─────────────┐  ┌─────────────┐       │
│  │  Compiler   │  │  Your App   │       │
│  │  (in cljp)  │  │  (in cljp)  │       │
│  └─────────────┘  └─────────────┘       │
│         │                                │
│         ▼                                │
│  ┌─────────────┐                        │
│  │  Runtime    │                        │
│  │  - eval     │                        │
│  │  - macros   │                        │
│  │  - reader   │                        │
│  └─────────────┘                        │
│                                          │
│  ┌─────────────┐                        │
│  │  REPL/nREPL │ ◄── Editor connection  │
│  │  Server     │                        │
│  └─────────────┘                        │
│                                          │
│  ┌─────────────┐                        │
│  │  Xdebug     │  (invisible)           │
│  └─────────────┘                        │
└─────────────────────────────────────────┘
```

**Like SBCL:**
- SBCL is written in Common Lisp, bootstrapped from an existing Lisp
- ClojurePHP will be written in ClojurePHP, bootstrapped from JVM Clojure
- Result: single PHP image with compiler + runtime + REPL

**Why this matters:**
- Macroexpand happens in-process (full access to compiler)
- REPL is the running program, not a separate tool
- Errors are inspectable with full context
- No JVM dependency in production

---

## Learning from Other Clojure Implementations

### ClojureDart's Minimal Bootstrap

ClojureDart takes only **2 Java files** from Clojure:

| File | Size | Purpose |
|------|------|---------|
| `LispReader.java` | ~56KB | Full Clojure reader with line/col metadata |
| `LineNumberingWriter.java` | ~55 lines | Track output positions for source maps |

Everything else (analyzer, emitter, core library) is written in Clojure/cljd itself.

**For ClojurePHP self-hosting:**

| ClojureDart (Java) | ClojurePHP (PHP) | Purpose |
|-------------------|------------------|---------|
| `LispReader.java` | `LispReader.php` | Parse `.cljc` → forms with `:line`/`:col` |
| `LineNumberingWriter.java` | `LineNumberingWriter.php` | Track emitted PHP positions |
| (rest in cljd) | (rest in cljp) | Analyzer, emitter, macros |

**Key insight**: The reader is the bootstrap barrier. Once we have `LispReader.php`, the rest can be written in ClojurePHP and compiled by the JVM-hosted compiler.

### ClojureCLR's Entry Point Pattern

ClojureCLR's `Main.cs` is just 30 lines - a thin shim that bootstraps the runtime:

```csharp
static void Main(string[] args) {
    RT.Init();
    REQUIRE.invoke(CLOJURE_MAIN);
    MAIN.applyTo(RT.seq(args));
}
```

**For ClojurePHP**: A minimal `main.php` that bootstraps runtime, then calls `cljp.main/main` written in ClojurePHP itself.

### Clojure JVM's Compiler Architecture

`Compiler.java` (~8000 lines) defines all special forms with a uniform structure:

```java
static final public IPersistentMap specials = PersistentHashMap.create(
    DEF, new DefExpr.Parser(),
    LOOP, new LetExpr.Parser(),
    RECUR, new RecurExpr.Parser(),
    IF, new IfExpr.Parser(),
    LET, new LetExpr.Parser(),
    DO, new BodyExpr.Parser(),
    FN, null,
    QUOTE, new ConstantExpr.Parser(),
    TRY, new TryExpr.Parser(),
    THROW, new ThrowExpr.Parser(),
    NEW, new NewExpr.Parser(),
    DOT, new HostExpr.Parser(),
    ...
);
```

Each special form has:
- A `Symbol` constant
- An `Expr` class (AST node)
- A `Parser` inner class (syntax → AST)
- An `emit` method (AST → bytecode/PHP)

---

## Two Modes of Operation

### Development: Live Image (Swoole)

```
┌─────────────────────────────────────────┐
│  Swoole Event Loop                       │
│                                          │
│  ┌───────────┐  ┌───────────┐           │
│  │ HTTP      │  │ WebSocket │           │
│  │ Server    │  │ Server    │           │
│  └───────────┘  └───────────┘           │
│                                          │
│  ┌───────────┐  ┌───────────┐           │
│  │ nREPL     │  │ Compiler  │           │
│  │ Server    │  │           │           │
│  └───────────┘  └───────────┘           │
│                                          │
│  Coroutines, channels, async I/O         │
└─────────────────────────────────────────┘
```

Eval a form → compiles in-process → immediately available. No restart.

### Production: Static Compilation

```bash
cljp build app.cljc -o app.php      # single file, no REPL
cljp build app.cljc -o app.phar     # distributable archive
cljp build app.cljc --tree-shake    # dead code elimination
```

**Brownfield deployment:**
```php
<?php
// Legacy Laravel/Symfony/WordPress app
require 'vendor/autoload.php';
require 'compiled/clojurephp-runtime.php';
require 'compiled/my-module.php';  // ← static cljp output

// Use ClojurePHP functions from PHP
$result = MyModule\process_data(['foo' => 'bar']);
```

**The spectrum:**
```
Static PHP ◄─────────────────────────────► Live Image
(brownfield)                               (greenfield)

- compiled .php files                      - Swoole runtime
- no REPL                                  - REPL connected
- legacy integration                       - hot reload
- deploy like normal PHP                   - SLIME-like DX
- tree-shaking                             - full introspection
```

---

## What Makes SLIME/CIDER Magical

1. **Interactive Errors** - Click on stack frames, jump to source, inspect locals
2. **REPL IS the program** - Not a separate tool, the actual running process
3. **Inspector** - Zoom into any value, navigate its structure
4. **Macroexpand** - See what macros expand to (in-process!)
5. **Hot Reload** - Change a function, it's immediately available
6. **Seamless** - Everything just works, no context switching

**Key insight:** In SLIME, the REPL is not talking to the program - it IS the program. The compiler, debugger, and your code all live in the same image.

---

## Lessons from SLIME/SBCL Architecture

Deep analysis of the SLIME (`swank/`) and SBCL (`src/code/debug*.lisp`) codebases reveals key design patterns we should adopt.

### Backend Abstraction Pattern (DEFINTERFACE/DEFIMPLEMENTATION)

SLIME separates portable debugger logic from implementation-specific code:

```lisp
;; swank/backend.lisp - Interface contract
(definterface frame-locals (frame-number)
  "Return list of ((&key NAME ID VALUE) ...)")

(definterface eval-in-frame (form frame-number)
  "Evaluate FORM in frame's lexical context")

(definterface frame-source-location (frame-number)
  "Return location for frame (file, position, snippet)")

;; swank/sbcl.lisp - SBCL-specific implementation
(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
         (loc (sb-di:frame-code-location frame)))
    ...))
```

**For ClojurePHP:** This maps directly to our shared/platform split:

```clojure
;; clojure/debugger.cljc (shared)
(defprotocol IDebugBackend
  (frame-locals [this frame-number])
  (eval-in-frame [this form frame-number])
  (frame-source-location [this frame-number])
  (compute-backtrace [this start end]))

;; backends/php/debug.cljc (platform-specific)
(extend-type PhpDebugger
  IDebugBackend
  (frame-locals [this frame-number]
    ;; PHP-specific frame introspection
    ...))
```

### Debugger Hook Architecture

SLIME uses a **debugger hook injection pattern**:

```lisp
(defun swank-debugger-hook (condition hook)
  ;; Installed via (install-debugger-globally #'swank-debugger-hook)
  (handler-case
      (call-with-debugger-hook #'swank-debugger-hook
                               (lambda () (invoke-slime-debugger condition)))
    (invoke-default-debugger ()
      (invoke-default-debugger condition))))
```

**Flow:**
1. Condition signaled → `*DEBUGGER-HOOK*` called
2. `swank-debugger-hook` packages condition, calls `invoke-slime-debugger`
3. `debug-in-emacs` captures state, enters `sldb-loop`
4. Loop: send state to Emacs, wait for commands, dispatch

**For ClojurePHP:**
```clojure
;; Register hook at startup
(set-exception-handler! cljp-debugger-hook)

(defn cljp-debugger-hook [exception]
  (let [restarts (compute-restarts exception)
        frames (compute-backtrace 0 20)]
    (sldb-loop {:exception exception
                :restarts restarts
                :frames frames
                :level (inc *sldb-level*)})))
```

### The SLDB Loop (Debugger Message Protocol)

SLIME's debugger implements a client-server message loop:

```lisp
(defun sldb-loop (level)
  (unwind-protect
       (loop
        (send-to-emacs (list* :debug thread-id level
                              (debugger-info-for-emacs 0 *sldb-initial-frames*)))
        (send-to-emacs (list :debug-activate thread-id level nil))
        (loop
         (handler-case
             (dcase (wait-for-event `(or (:emacs-rex . _)
                                         (:sldb-return ,(1+ level))))
               ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
               ((:sldb-return _) (return nil)))
           (sldb-condition (c) (handle-sldb-condition c)))))
    (send-to-emacs `(:debug-return ,thread-id ,level ,*sldb-stepping-p*))))
```

**Message Protocol:**
| Message | Direction | Purpose |
|---------|-----------|---------|
| `:debug` | → Editor | Enter debugger (condition, restarts, frames) |
| `:debug-activate` | → Editor | Activate debugger UI |
| `:emacs-rex` | ← Editor | RPC call (inspect, eval, etc.) |
| `:sldb-return` | ← Editor | Return from debugger |
| `:debug-return` | → Editor | Exit debugger with status |

### Condition/Restart System

SBCL's restarts are **closures with captured state**, dynamically bound:

```lisp
(defstruct restart
  (name (missing-arg) :type symbol :read-only t)
  (function (missing-arg) :type function :read-only t)
  (report-function nil :type (or null function) :read-only t)
  (interactive-function nil :type (or null function) :read-only t)
  (test-function (lambda (cond) t) :type function :read-only t))
```

**Binding via dynamic scope:**
```lisp
(restart-case (do-something-risky)
  (use-value (v)
    :report "Use a different value"
    v)
  (skip ()
    :report "Skip this item"
    nil))
```

**For ClojurePHP:**
```clojure
;; Shared (clojure/condition.cljc)
(defmacro with-restarts [restart-specs & body]
  `(binding [*restart-clusters* (cons ~(compile-restarts restart-specs)
                                       *restart-clusters*)]
     ~@body))

(defn invoke-restart [name & args]
  (if-let [restart (find-restart name)]
    (apply (:function restart) args)
    (throw (ex-info "No such restart" {:name name}))))

;; Usage
(with-restarts [(use-value [v] v)
                (skip [] nil)]
  (process-risky-data))

;; On error, REPL shows:
;;   0: [USE-VALUE] Use a different value
;;   1: [SKIP] Skip this item
;;   2: [ABORT] Return to REPL
```

### Inspector Architecture

SLIME's inspector uses **stateful navigation with history**:

```lisp
(defstruct istate
  object              ; Current inspected object
  (parts ...)         ; Referenced sub-objects (vector, indexed)
  (actions ...)       ; Interactive actions (vector, indexed)
  content             ; Formatted content for display
  next previous)      ; History navigation
```

**Content format (Emacs-compatible):**
- Strings → literal text
- `(:newline)` → line breaks
- `(:value OBJ [STR])` → inspectable object reference
- `(:action LABEL LAMBDA)` → interactive actions
- `(:line LABEL VALUE)` → key-value pairs

**Key pattern:** Objects are indexed into vectors; editor refers by index to avoid serialization overhead.

### Lazy Frame Fetching

SLIME only sends initial 20 frames, fetches more on demand:

```lisp
*sldb-initial-frames* = 20
;; Editor can request: (backtrace start end)
```

**For ClojurePHP:** Don't eagerly compute the entire stack. Send 20 frames, let editor request more.

### Variable Validity States

Variables have three states at any code location:

```lisp
(ecase (debug-var-validity var location)
  (:valid     (debug-var-value var frame))  ; Safe to read
  (:invalid   ':<not-available>)            ; May not exist
  (:unknown   ':<not-available>))           ; Corrupted/dead
```

**For ClojurePHP:** PHP closures capture by-value or by-reference. Track which locals are accessible at each point.

### Debug Info Storage (SBCL)

SBCL embeds debug info in **packed binary format** in compiled code:

```
[flags] [name-length] [name-bytes] [package] [id] [sc+offset] [live-mask]
```

Each code location has:
- **Kind** (3 bits): call-site, block-start, non-local-exit, etc.
- **PC Delta** - Offset from previous location
- **Form Info** - Top-level form number + form number within
- **Live Set** - Bitmap of which variables are live

**For ClojurePHP:** We can embed source locations directly in PHP output (inline comments or data structures), not just separate files.

### Source Location Resolution (Three Tiers)

SLIME uses layered fallback for robustness:

```
Tier 1: Debug Block Information (most precise)
        └─ Compiler embedded block positions

Tier 2: Source Path Parser
        └─ Read source, record positions, resolve form-numbers

Tier 3: Function Definition Fallback
        └─ Return function's location if nothing else works
```

**Key insight:** Always show something useful. Never fail to locate.

---

## Shared vs Platform Concerns

Based on SLIME/SBCL analysis and our discussions, here's the clean separation:

### Source Maps

```
┌─────────────────────────────────────────────────────────────┐
│  CLOJURE CONCERN (shared)                                   │
│                                                             │
│  • Reader attaches {:line :column :file} to forms           │
│  • Analyzer preserves location in HIR node's :env           │
│  • CONTRACT: every HIR node carries source location         │
│                                                             │
│  {:op :invoke                                               │
│   :env {:file "app.cljc" :line 12 :column 5}                │
│   :fn ...}                                                  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  PLATFORM CONCERN (per backend)                             │
│                                                             │
│  • Format: How to encode the mapping                        │
│  • Storage: Inline, separate file, embedded                 │
│  • Consumption: How runtime/debugger reads it               │
│                                                             │
│  PHP:  .xdebug/*.map (Xdebug 3.5 format)                    │
│        .xdebug/*.php (OPcache-friendly array)               │
│  JS:   .map (V3 source map, VLQ-encoded)                    │
│  JVM:  LineNumberTable in .class bytecode                   │
│  WASM: DWARF debug info or custom section                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Debugger

```
┌─────────────────────────────────────────────────────────────┐
│  CLOJURE CONCERN (shared)                                   │
│                                                             │
│  clojure/debugger.cljc:                                     │
│    • IDebugBackend protocol                                 │
│    • SLDB loop (wait for commands, dispatch)                │
│    • Message protocol (:debug, :debug-activate, etc.)       │
│    • Restart binding (*restart-clusters*)                   │
│    • Inspector (stateful navigation, history)               │
│                                                             │
│  clojure/condition.cljc:                                    │
│    • Condition hierarchy                                    │
│    • with-restarts, invoke-restart                          │
│    • signal, error, warn, cerror                            │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  PLATFORM CONCERN (per backend)                             │
│                                                             │
│  backends/php/debug.cljc:                                   │
│    • Emit breakpoint instrumentation                        │
│    • Capture locals via closure introspection               │
│    • Frame representation                                   │
│                                                             │
│  Runtime (PHP):                                             │
│    \Cljp\Debug::brk($locals, $file, $line)                  │
│    \Cljp\Debug::pushRestart($name, $fn)                     │
│    \Cljp\Debug::invokeRestart($name, ...$args)              │
│                                                             │
│  Optional Xdebug integration for PHP-level debugging        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Clojure Debugger vs Platform Debugger

| Aspect | Clojure Debugger | Platform Debugger (Xdebug) |
|--------|------------------|----------------------------|
| **Sees** | Clojure bindings, forms | PHP variables, PHP lines |
| **Steps by** | Clojure form | PHP statement |
| **Macros** | Can step through expansion | Already expanded |
| **Interop** | Opaque unless instrumented | Full visibility |
| **Condition/restart** | Yes (like Common Lisp) | No |
| **Works when** | REPL connected | Always (production too) |
| **Backend-specific** | No (shared protocol) | Yes |

**Recommendation:** Build the Clojure debugger first (shared, 95% of use cases). Xdebug integration is nice-to-have for PHP interop debugging.

---

## Xdebug 3.5 Integration

Xdebug 3.5.0 (December 2025) adds native source map support with line range mappings:

### Source Map Format

```
# out.php.map
remote_prefix: /var/www/app
local_prefix: /home/dev/project

# Line range mappings: PHP lines → Clojure source
out.php:6-8 = src/app.cljc:12
out.php:10-15 = src/app.cljc:15
out.php:18 = src/app.cljc:20
```

### Registration

```php
// In bootstrap
if (extension_loaded('xdebug')) {
    foreach (glob(__DIR__ . '/.xdebug/*.map') as $map) {
        xdebug_set_source_map($map);
    }
}
```

### What This Enables

With Xdebug's mapping:
1. User sets breakpoint at `app.cljc:15` in IDE
2. Xdebug maps to `out.php:10`
3. Execution stops
4. Stack trace shows `app.cljc:15` (not PHP location)

### Limitations

From Xdebug docs: "Extending line range maps to also include column information, although IDEs can't do anything with this yet, and neither does the DBGp protocol support this yet either."

Line-level is the best we can do with Xdebug. Column info stays in our rich JSON format for error messages.

### Source Map Outputs

| Format | Purpose | Consumer |
|--------|---------|----------|
| `.xdebug/*.map` | Xdebug line mapping | Xdebug 3.5+ debugger |
| `.xdebug/*.php` | Fast error lookup | ErrorHandler (OPcache'd) |
| `.xdebug/*.json` | Rich metadata | Error display, tooling |

---

## Unified Error System

### The One Exception Type: ExceptionInfo

Following Clojure's design, we use ONE exception type with structured data:

```clojure
(ex-info "Division by zero"
  {:cljp.error/phase :runtime
   :cljp.error/type :arithmetic
   :cljp.error/file "app.cljc"
   :cljp.error/line 12
   :cljp.error/column 5
   :cljp.error/symbol 'user/divide
   :cljp.error/form '(/ total count)
   :cljp.error/locals {:total 100 :count 0}
   :cljp.error/hint "Check for zero before dividing"})
```

No class hierarchy needed. The `:cljp.error/type` key in the data map distinguishes error kinds. Tools filter/dispatch on that.

### Error Phases (from clojure.main/ex-triage)

Clojure JVM's `ex-triage` function categorizes errors into phases. We adopt the same:

| Phase | When | Example |
|-------|------|---------|
| `:read-source` | Reader can't parse | `EOF while reading`, `Unmatched )` |
| `:macro-syntax-check` | Macro args invalid | Bad `defn` syntax |
| `:macroexpansion` | Macro throws unexpectedly | Exception in macro body |
| `:compile-syntax-check` | Compiler rejects form | Invalid special form usage |
| `:compilation` | Emit fails | Type mismatch |
| `:execution` | Runtime error | `IndexOutOfBounds`, user `throw` |
| `:read-eval-result` | Can't read return value | Broken tagged literal |
| `:print-eval-result` | Can't print return value | Circular reference |
| `:interop` | PHP call fails | Class not found, method missing |

### Error Types

```
:cljp.error/type values:
├── :syntax          - Reader/parser errors
├── :arity           - Wrong number of arguments
├── :bounds          - Index out of bounds
├── :type            - Type mismatch
├── :unresolved      - Symbol not found
├── :arithmetic      - Division by zero, overflow
├── :assertion       - (assert ...) failed
├── :interop         - PHP interop failure
│   ├── :class-not-found
│   ├── :method-not-found
│   └── :property-error
└── :user            - User's (throw ...) calls
```

### Error Triage Pipeline (from clojure.main)

Clojure JVM uses a clean pipeline for error processing:

```clojure
(-> throwable Throwable->map ex-triage ex-str)
```

| Function | Purpose |
|----------|---------|
| `Throwable->map` | Convert exception to data (`:via`, `:trace`, `:cause`) |
| `ex-triage` | Analyze and extract key info (`:clojure.error/*` keys) |
| `ex-str` | Format for human-readable display |

**For ClojurePHP:**
```clojure
(-> php-exception exception->map ex-triage ex-str)
```

### Integration Points

**REPL:**
```clojure
cljp> (process-data bad-input)
;; Error occurs...

cljp> *e                        ; last exception
cljp> (ex-data *e)              ; structured data
{:cljp.error/phase :execution
 :cljp.error/type :bounds
 :cljp.error/symbol 'user/process-data
 :cljp.error/message "Index 5 out of bounds"
 ...}

cljp> (ex-cause *e)             ; chain to previous
cljp> (clojure.stacktrace/e)    ; brief trace
```

**Test framework:**
```clojure
(deftest process-data-test
  (is (= {:result 42} (process-data input)))
  (is (thrown? ExceptionInfo (process-data nil))))

;; Output:
;; FAIL in (process-data-test) (app_test.cljc:12)
;; expected: (= {:result 42} (process-data input))
;;   actual: (not (= {:result 42} {:error "missing field"}))
```

**Clojure Debugger (Primary):**
- Breakpoints via instrumented emit: `\Cljp\Debug::brk($locals, ...)`
- SLDB loop: send state to editor, wait for commands
- Step by Clojure form, not PHP line
- Inspect Clojure bindings, not PHP variables
- Condition/restart: choose recovery strategies interactively

**Xdebug (Secondary, for PHP interop):**
- Breakpoint in `.cljc` → Xdebug 3.5 source map → PHP line
- Step → PHP steps → source map → show Clojure line
- Useful for debugging PHP library calls and interop issues
- All invisible. User thinks they're debugging Clojure.

### Rich Error Display (from clojure.main/ex-str)

Clojure formats errors by phase:

```
;; :read-source
Syntax error reading source at (app.cljc:12).
EOF while reading

;; :macro-syntax-check
Syntax error macroexpanding defn at (app.cljc:15).
Call to clojure.core/defn did not conform to spec.

;; :execution
Execution error (ArityException) at user/process-data (app.cljc:42).
Wrong number of args (3) passed to user/process-data
```

**ClojurePHP enhanced display:**
```
ArityError: Wrong number of arguments (3) passed to user/process-data
  Expected: 1 or 2 arguments
  Got: 3

  (process-data a b c)
                    ^-- extra argument

  Defined at: src/app.cljc:15
  Called from: src/app.cljc:42

  Hint: Remove the extra argument, or update the function to accept 3 args
```

```
BoundsError: Index 5 out of bounds
  Vector has 3 elements (valid indices: 0, 1, 2)

     10│ (defn get-item [items idx]
  >> 11│   (nth items idx))
     12│

  Form: (nth items idx)

  Locals:
    items = ["a" "b" "c"]
    idx   = 5

  Hint: Use (get items 5 :default) for safe access with fallback
```

---

## Current State (What We Have)

### Source Maps
```
.xdebug/
├── out.php.map    # Simple format for debugging
└── out.php.json   # Rich map with line/col/form
```

### Error Handler
Rewrites PHP errors to show Clojure locations with source context:
```
Error: Something went wrong!
in test_error.cljc:3:1

      1| ;; Test error handling
      2|
 >>   3| (defn process-data [x]
      4|   (throw (new Exception "...")))

Form: (defn process-data [x] (throw ...))

Stack:
  0. test_error.cljc:6 (defn main [] ...)
  1. test_error.cljc:10 (main)
```

### Compiler CLI
```bash
cljp app.cljc                  # → out.php + source map
cljp -o app.php src/*.cljc     # → app.php + source map
cljp -o - app.cljc | php       # pipe mode
```

---

## CLI Architecture (from clojure.main)

### Command Line Dispatch Pattern

Both Clojure JVM and ClojureScript use a two-phase dispatch:

1. **Init options** (`-i`, `-e`) - accumulate, run in order
2. **Main options** (`-r`, `-m`, path) - terminal, pick one

```clojure
;; Clojure JVM pattern (clojure.main:651-668)
(loop [[opt arg & more] args, inits []]
  (cond
    (init-dispatch opt)  (recur more (conj inits [opt arg]))
    :main-opt            ((main-dispatch opt) args inits)))
```

**For ClojurePHP CLI:**

```
Usage: cljp [init-opt*] [main-opt] [arg*]

init options (repeatable, run in order):
  -i, --init path      Load a file or resource
  -e, --eval string    Evaluate expressions; print non-nil values
  -v, --verbose        Enable verbose logging

main options (pick one):
  -r, --repl           Run a REPL
  -m, --main ns        Call -main function from namespace
  -c, --compile ns     Compile namespace to PHP
  -w, --watch path     Watch and recompile on changes
  -s, --serve [port]   Start dev server with REPL
  path                 Run script from file
  -                    Run script from stdin
  -h, --help           Print help and exit

operation:
  - Enters the user namespace
  - Binds *command-line-args* to args after main option
  - Runs all init options in order
  - Runs main option (repl/script/compile)
```

### ClojureScript's Extensible Commands Map

ClojureScript uses a **commands map** pattern that's extensible:

```clojure
(def default-commands
  {:groups {::main {:desc "init options only for --main and --repl"}
            ::compile {:desc "init options only for --compile"}}
   :init {["-i" "--init"] {:fn init-opt :arg "path" :doc "Load a file"}
          ["-e" "--eval"] {:fn eval-opt :arg "string" :doc "Evaluate expressions"}
          ...}
   :main {["-r" "--repl"] {:fn repl-opt :doc "Run a repl"}
          ["-m" "--main"] {:fn main-opt :arg "ns" :doc "Call -main from namespace"}
          ...}})
```

Different REPL environments can add their own commands. **ClojurePHP** can use this pattern for Swoole vs traditional PHP modes.

---

## REPL Architecture (from clojure.main)

### Hook-Based REPL

Clojure's `repl` function is fully customizable via hooks:

```clojure
(repl :init    #(...)         ; initialization
      :read    repl-read      ; how to read input
      :eval    eval           ; how to evaluate
      :print   prn            ; how to print results
      :caught  repl-caught    ; error handling
      :prompt  repl-prompt    ; prompt display
      :need-prompt #(...))    ; when to show prompt
```

**Key insight**: Everything is a replaceable function. You can swap any part.

### REPL State Bindings

The `with-bindings` macro sets up all dynamic vars for a fresh REPL context:

```clojure
(binding [*ns* *ns*
          *warn-on-reflection* *warn-on-reflection*
          *print-meta* *print-meta*
          *print-length* *print-length*
          *print-level* *print-level*
          *compile-path* (System/getProperty "clojure.compile.path" "classes")
          *command-line-args* *command-line-args*
          *1 nil    ; last result
          *2 nil    ; second-to-last result
          *3 nil    ; third-to-last result
          *e nil]   ; last exception
  ...)
```

### Result History

```clojure
;; After each successful eval:
(set! *3 *2)
(set! *2 *1)
(set! *1 value)

;; On error:
(set! *e exception)
```

### REPL Read-Eval-Print Loop

```clojure
(fn read-eval-print []
  (try
    (let [input (read request-prompt request-exit)]
      (or (#{request-prompt request-exit} input)
          (let [value (eval input)]
            (set! *3 *2) (set! *2 *1) (set! *1 value)
            (try (print value)
              (catch Throwable e
                (throw (ex-info nil {:clojure.error/phase :print-eval-result} e)))))))
    (catch Throwable e
      (caught e)
      (set! *e e))))
```

---

## Vision: The Complete Experience

### 1. Seamless REPL

```
┌─────────────────────────────────────────────────────────────┐
│  Emacs / VS Code / Terminal                                 │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  user.cljc                     │  cljp>                     │
│  ──────────                    │                            │
│  (ns user)                     │  (+ 1 2)                   │
│                                │  => 3                      │
│  (defn greet [name]            │                            │
│    (str "Hello, " name)) ──────┼──▶ (greet "World")         │
│                         C-x C-e│  => "Hello, World"         │
│                                │                            │
│  (defn process [data]          │  (macroexpand-1            │
│    (-> data                    │    '(-> x (a) (b)))        │
│        (transform)             │  => (b (a x))              │
│        (validate)))            │                            │
│                                │                            │
└─────────────────────────────────────────────────────────────┘
```

In self-hosted mode, macroexpand happens in the same image. Full access to the compiler.

### 2. Inspector

```
cljp> (inspect user-data)

Inspecting: user-data
───────────────────────────────────────
Type: PersistentHashMap (3 entries)

  :name     "Alice"              [i]nspect
  :scores   [95 87 92]           [i]nspect  [e]xpand
  :metadata {:created "2024"...} [i]nspect  [e]xpand

[b]ack  [p]rint  [c]opy EDN
```

### 3. Macroexpand

```
cljp> (macroexpand-1 '(defn foo [x] (inc x)))
=> (def foo (fn [x] (inc x)))

cljp> (macroexpand-all '(-> x (a 1) (b 2)))
=> (b (a x 1) 2)
```

### 4. Watch Mode & Hot Reload

```bash
$ cljp watch src/ --repl

Watching src/ for changes...
REPL ready on port 7888

[12:34:56] src/app.cljc changed
           Recompiled (23ms)
           Reloaded: greet, process-data, main

cljp> (greet "World")   ; uses new definition immediately
=> "Hello, World!"
```

---

## Architecture

### Bootstrap Phase (Now)

```
┌─────────────────────────────────────────────────────────────────┐
│                     ClojurePHP Development Server               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐ │
│  │   Compiler  │    │  REPL/Eval  │    │   PHP Runtime       │ │
│  │   (JVM)     │◄──►│   Server    │◄──►│   (Subprocess)      │ │
│  └─────────────┘    └─────────────┘    └─────────────────────┘ │
│         │                  │                     │              │
│         ▼                  ▼                     ▼              │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐ │
│  │ Source Maps │    │   nREPL     │    │  State (atoms,      │ │
│  │ .xdebug/    │    │  Protocol   │    │  defined fns)       │ │
│  └─────────────┘    └─────────────┘    └─────────────────────┘ │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Self-Hosted Phase (Goal)

```
┌─────────────────────────────────────────────────────────────────┐
│                     ClojurePHP Image (Swoole)                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Single PHP Process                    │   │
│  │                                                          │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │   │
│  │  │  Compiler   │  │   Reader    │  │   Your App      │  │   │
│  │  │  (cljp)     │  │   (cljp)    │  │   (cljp)        │  │   │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘  │   │
│  │         │                │                  │            │   │
│  │         └────────────────┼──────────────────┘            │   │
│  │                          ▼                               │   │
│  │                  ┌─────────────┐                         │   │
│  │                  │   Runtime   │                         │   │
│  │                  │   - eval    │                         │   │
│  │                  │   - macros  │                         │   │
│  │                  └─────────────┘                         │   │
│  │                                                          │   │
│  └──────────────────────────────────────────────────────────┘   │
│                          │                                       │
│                          ▼                                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │   nREPL     │  │   HTTP      │  │   Xdebug (invisible)    │ │
│  │   :7888     │  │   :8080     │  │                         │ │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘ │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  Editor Integration: CIDER / Calva / Conjure / Cursive         │
└─────────────────────────────────────────────────────────────────┘
```

**One protocol (nREPL), all editors:**
- CIDER (Emacs) - native support
- Calva (VS Code) - native support
- Conjure (Neovim) - native support
- Cursive (JetBrains) - native support

### Bootstrap Sequence (like ClojureDart)

```
1. JVM Clojure compiles clojure.core → PHP
2. JVM Clojure compiles the reader → PHP
3. JVM Clojure compiles the analyzer/emitter → PHP
4. Now PHP can read + compile .cljc files itself
5. No more JVM dependency
```

---

## Core Library Modules

The standard library that makes ClojurePHP feel like Clojure.

### clojure.main - Entry Point & REPL

Port of Clojure's `clojure.main` namespace:

```clojure
;; Entry point
(main & args)              ; CLI dispatcher

;; REPL
(repl & options)           ; customizable REPL loop
(repl-read request-prompt request-exit)
(repl-prompt)              ; "user=> "
(repl-caught e)            ; error handler

;; Error handling
(ex-triage datafied-throwable)  ; analyze error
(ex-str triage-data)            ; format for display
(err->msg e)                    ; full pipeline

;; Utilities
(with-bindings & body)     ; set up REPL context
(load-script path)         ; load file or resource
(demunge fn-name)          ; PHP name → Clojure symbol
(root-cause throwable)     ; unwrap to original
(stack-element-str el)     ; format stack frame
```

### clojure.repl - Interactive Utilities

```clojure
(doc defn)              ; show documentation
(source my-fn)          ; show source (via source maps)
(find-doc "reduce")     ; search all docs
(apropos "map")         ; find symbols containing "map"
(dir user)              ; list public vars in namespace
(pst)                   ; print stack trace of *e
(pst 5)                 ; ... limited to 5 frames
```

Key functions:
- `doc` - metadata-based documentation
- `source` / `source-fn` - retrieve source via `:file` and `:line` metadata
- `pst` - stack trace with phase detection (compile vs runtime)
- `demunge` - convert PHP function names back to Clojure symbols

### clojure.walk - Tree Traversal

```clojure
(postwalk f form)       ; bottom-up traversal
(prewalk f form)        ; top-down traversal
(macroexpand-all form)  ; recursively expand all macros

(keywordize-keys m)     ; {"a" 1} → {:a 1}
(stringify-keys m)      ; {:a 1} → {"a" 1}
(postwalk-replace smap form)
(prewalk-replace smap form)
```

Essential for:
- `macroexpand-all` - just 3 lines using prewalk
- AST transformations in the compiler
- Code analysis and instrumentation

### clojure.set - Relational Algebra

```clojure
(union s1 s2)           ; set union
(intersection s1 s2)    ; set intersection
(difference s1 s2)      ; set difference
(subset? s1 s2)         ; is s1 ⊆ s2?
(superset? s1 s2)       ; is s1 ⊇ s2?

;; Relational operations (sets of maps)
(select pred xrel)      ; filter relation
(project xrel ks)       ; select columns
(join xrel yrel)        ; natural join
(rename xrel kmap)      ; rename keys
(index xrel ks)         ; group by keys
```

### clojure.string - String Operations

```clojure
(str/join ", " coll)
(str/split s #",")
(str/replace s #"foo" "bar")
(str/trim s)
(str/upper-case s)
(str/blank? s)
```

### clojure.test - Test Framework

```clojure
(deftest my-test
  (testing "arithmetic"
    (is (= 4 (+ 2 2)))
    (is (thrown? ExceptionInfo (bad-fn)))))

(run-tests 'my.namespace)
```

Integrated with error system:
- `{:type :pass}`, `{:type :fail}`, `{:type :error}`
- Shows `:expected` vs `:actual`
- Stack traces with source mapping

### clojure.stacktrace - Error Utilities

```clojure
(root-cause e)          ; unwrap to original exception
(print-stack-trace e)   ; Clojure-centric trace
(print-cause-trace e)   ; include cause chain
(e)                     ; REPL shortcut for *e
```

### clojure.edn - Data Reader/Writer

```clojure
(edn/read-string "{:a 1}")
(edn/read opts reader)
```

Safe data interchange format. Extensible via `:readers` and `:default`.

### clojure.pprint - Pretty Printing

```clojure
(pprint data)                    ; structured output
(cl-format true "~{~a~^, ~}" items)  ; Common Lisp format
(print-table [:name :age] users) ; tabular display
```

Multiple dispatch functions for different output formats.

### clojure.data - Diff & Comparison

```clojure
(diff a b)
;; => [things-only-in-a things-only-in-b things-in-both]

(diff {:a 1 :b 2} {:a 1 :b 3})
;; => [{:b 2} {:b 3} {:a 1}]
```

Essential for test assertions - show what's different, not just "not equal".

### clojure.math - Math Functions

```clojure
(math/sin x)  (math/cos x)  (math/tan x)
(math/sqrt x) (math/pow x y) (math/log x)
(math/floor x) (math/ceil x) (math/round x)
math/PI math/E
```

Thin wrappers over PHP's math functions.

### clojure.inspector - Data Navigation

```clojure
(inspect data)       ; GUI tree view (Swing in JVM)
(inspect-tree data)  ; hierarchical view
(inspect-table data) ; tabular view
```

For ClojurePHP: terminal-based or web-based inspector.

---

## Type System Constructs

### deftype / defrecord

```clojure
(defrecord User [name email])

(deftype Counter [^:mutable count]
  IDeref
  (deref [_] count)

  IReset
  (reset [_ v] (set! count v)))
```

Maps to PHP classes with defined fields and protocol implementations.

### reify

```clojure
(reify
  IFn
  (invoke [_ x] (* x 2))

  Object
  (toString [_] "doubler"))
```

Anonymous implementation of protocols/interfaces.

### defprotocol / extend-protocol

```clojure
(defprotocol IStringable
  (to-string [x]))

(extend-protocol IStringable
  String (to-string [s] s)
  Number (to-string [n] (str n))
  nil    (to-string [_] ""))
```

PHP: interface + trait implementations.

### gen-class (PHP equivalent)

For ClojurePHP, we need interop constructs:
- Generate PHP classes callable from PHP code
- Extend existing PHP classes
- Implement PHP interfaces

```clojure
(gen-class
  :name "App\\Services\\UserService"
  :extends "App\\Services\\BaseService"
  :implements ["App\\Contracts\\UserServiceInterface"]
  :methods [[findById [int] "App\\Models\\User"]])
```

---

## The Reader: Foundation of Everything

The reader is where source location enters the system. It's foundational to REPL, tests, and errors.

### How It Works

```
Source Text → Reader → Forms (with metadata) → Compiler → PHP
                ↓
         {:line 12 :column 5 :file "app.cljc"}
         attached to every form
```

### Two Readers

| Feature | EdnReader | LispReader |
|---------|-----------|------------|
| Purpose | Safe data | Full code |
| Syntax-quote `` ` `` | ✗ | ✓ |
| Unquote `~` | ✗ | ✓ |
| Fn literals `#()` | ✗ | ✓ |
| Deref `@` | ✗ | ✓ |
| Eval `#=` | ✗ | ✓ |

**EdnReader** - for config files, data interchange (safe)
**LispReader** - for code, macros, REPL (full power)

### Reader Macros (Dispatch Table)

```
macros[256]           - character → reader function
dispatchMacros[256]   - # + character → reader function

'"' → StringReader      '(' → ListReader
';' → CommentReader     '[' → VectorReader
'^' → MetaReader        '{' → MapReader
'#' → DispatchReader

Dispatch macros (#...):
'#' → SymbolicValueReader (##Inf, ##NaN)
'{' → SetReader           (#{...})
'"' → RegexReader         (#"...")
'(' → FnReader            (#(...))
'_' → DiscardReader       (#_...)
':' → NamespaceMapReader  (#:ns{...})
```

### Why Reader Matters for DX

**REPL:**
```
User types: (defn foo [x] (inc x))
            ↓
Reader produces form with line/col metadata
            ↓
Compile → PHP
            ↓
Execute
            ↓
Error? → metadata traces back to source
```

**Tests:**
```clojure
(is (= expected actual))
```
The `is` macro captures the *original form* via the reader.
On failure: `expected: (= expected actual)` - that's the form!

**Errors:**
```
Reader metadata → Compiler preserves → Source maps → ErrorHandler displays
{:line 12}         AST nodes           PHP→Clojure      "in app.cljc:12"
```

### For Self-Hosting

The reader is the first thing to port:
1. **EdnReader in PHP** - safe config/data loading
2. **LispReader in PHP** - full compiler support
3. **LineNumberingPushbackReader** - track line/column

Once we have the reader in PHP, we can build the compiler in PHP, and then we have a self-hosted Lisp.

---

## Printing System

Based on `clojure.core/print-method` multimethod:

```clojure
(defmethod print-method User [u w]
  (.write w (str "#user/User{:name " (:name u) "}")))
```

Key features:
- `*print-length*` - limit collection items printed
- `*print-level*` - limit nesting depth
- `*print-readably*` - escape strings for reader
- `*print-meta*` - include metadata
- `Throwable->map` - structured exception data

---

## Implementation Roadmap

Each phase marks items as **[S]** Shared (Clojure concern) or **[P]** Platform (PHP-specific).

### Phase 1: Rich Errors ✓
- [x] **[S]** Source location in HIR `:env`
- [x] **[P]** Source maps with file/line/col (JSON format)
- [x] **[P]** Error handler rewrites stack traces
- [x] **[P]** Form capture in source maps
- [x] **[P]** Source context in errors (surrounding lines)
- [ ] **[P]** ExceptionInfo implementation in PHP
- [ ] **[S]** Structured error data (`:cljp.error/*` keys)
- [ ] **[S]** `ex-triage` / `ex-str` pipeline
- [ ] **[S]** Helpful hints in error messages

### Phase 2: Basic REPL
- [ ] **[S]** `with-bindings` macro (set up REPL context)
- [ ] **[S]** Hook-based REPL (`:read`, `:eval`, `:print`, `:caught`)
- [ ] **[S]** `*1`, `*2`, `*3`, `*e` bindings
- [ ] **[P]** TCP eval server (compile form → execute in PHP → return result)
- [ ] **[P]** Long-running PHP subprocess (maintains state)
- [ ] **[S]** Simple readline REPL (`cljp repl`)
- [ ] **[P]** Hot reload (redefine functions without restart)

### Phase 3: CLI Architecture
- [ ] **[S]** Two-phase dispatch (init options → main option)
- [ ] **[S]** Extensible commands map (like ClojureScript)
- [ ] **[S]** `cljp -i init.cljc -e '(+ 1 2)' script.cljc`
- [ ] **[S]** `cljp -r` / `cljp --repl`
- [ ] **[S]** `cljp -m app.core` (call `-main`)
- [ ] **[P]** `cljp -c app.core` (compile to PHP)
- [ ] **[S]** `cljp -w src/` (watch mode)

### Phase 4: nREPL Compatibility
- [ ] **[S]** Bencode transport
- [ ] **[S]** Core ops: eval, load-file, complete, info
- [ ] **[S]** CIDER middleware compatibility
- [ ] **[S]** Works with existing Clojure editor plugins

### Phase 5: Test Framework
- [ ] **[S]** `clojure.test` port (deftest, is, testing)
- [ ] **[S]** Integration with error system
- [ ] **[S]** `(thrown? ...)` assertions
- [ ] **[S]** Test runner CLI

### Phase 6: Self-Hosting
- [ ] **[P]** `LispReader.php` - the bootstrap barrier
- [ ] **[P]** `LineNumberingPushbackReader.php`
- [ ] **[S]** Reader in ClojurePHP (compiled by JVM)
- [ ] **[S]** Analyzer in ClojurePHP
- [ ] **[P]** PHP emitter in ClojurePHP
- [ ] **[P]** Bootstrap: JVM compiles cljp compiler → PHP
- [ ] **[P]** Native: PHP image runs compiler, no JVM needed

### Phase 7: Debugger & Advanced Features
- [ ] **[S]** `IDebugBackend` protocol
- [ ] **[S]** SLDB loop (message protocol)
- [ ] **[S]** Condition/restart system (`with-restarts`, `invoke-restart`)
- [ ] **[S]** Inspector (stateful navigation, history)
- [ ] **[S]** Macroexpand (expand-1, expand-all) - in-process!
- [ ] **[P]** Breakpoint instrumentation in emit
- [ ] **[P]** Local capture via closure introspection
- [ ] **[P]** Xdebug 3.5 source map generation (`.map` format)
- [ ] **[P]** Watch mode (file watcher + auto-reload)

### Phase 8: PHP Symbiosis

**Core Integration:**
- [ ] `cljp()` function - inline Clojure eval from PHP
- [ ] Variable binding - `$php_var` available in cljp scope
- [ ] `(php "...")` form - inline PHP eval from ClojurePHP
- [ ] `php/require` - load PHP files into runtime
- [ ] `php/new`, `php/::`, `php/->` - PHP interop forms
- [ ] Composer package - `clojurephp/runtime`
- [ ] Static precompilation - `cljp precompile *.php`

**REPL Tools for PHP:**
- [ ] `php/methods`, `php/props` - reflection helpers
- [ ] `inspect` for PHP objects - navigate structure
- [ ] `phpbench` - benchmark PHP code from REPL
- [ ] `phpbench-compare` - compare PHP vs cljp implementations

**Analysis & Migration:**
- [ ] `php/analyze-ns` - complexity, dependencies, usage
- [ ] `php/smells` - detect code smells, N+1 queries
- [ ] `php->cljp` - assisted translation to idiomatic ClojurePHP
- [ ] `php/verify-equivalence` - ensure PHP and cljp produce same results
- [ ] Hybrid wrappers - generate cljp wrappers around PHP classes

### Phase 9: MCP Integration (LLM-Powered Development)

Expose the live image to LLMs via Model Context Protocol (inspired by [clojure-mcp](https://github.com/bhauman/clojure-mcp)):

```
┌─────────────────────────────────────────────────────────────┐
│  LLM (Claude, etc)                                          │
├─────────────────────────────────────────────────────────────┤
│                         │                                   │
│                         ▼                                   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  MCP Server (in live image)                          │   │
│  │                                                      │   │
│  │  Tools:                                              │   │
│  │  • eval        - evaluate cljp/php expressions       │   │
│  │  • inspect     - examine any value                   │   │
│  │  • php-methods - reflection on PHP classes           │   │
│  │  • phpbench    - benchmark code                      │   │
│  │  • analyze     - complexity, deps, smells            │   │
│  │  • test        - run tests, get results              │   │
│  │  • source      - retrieve function source            │   │
│  │  • doc         - get documentation                   │   │
│  │                                                      │   │
│  │  Resources:                                          │   │
│  │  • namespaces  - list all loaded namespaces          │   │
│  │  • vars        - list vars in namespace              │   │
│  │  • php-classes - list available PHP classes          │   │
│  │  • errors      - recent errors with context          │   │
│  └─────────────────────────────────────────────────────┘   │
│                         │                                   │
│                         ▼                                   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  ClojurePHP Live Image (Swoole)                      │   │
│  │  Your app + Runtime + Compiler                       │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**What this enables:**

```
You: "Why is the OrderService slow?"

LLM: [calls eval: (php/analyze-ns 'App\Services\OrderService)]
     [calls phpbench: (-> (php/new OrderService) (.process sample-order))]
     [calls eval: (php/smells 'App\Services\OrderService)]

     "The OrderService has N+1 query issues on line 45.
      It's loading related products inside a loop.
      Here's a fix using eager loading..."
```

```
You: "Convert the TaxCalculator to ClojurePHP"

LLM: [calls php-methods: 'App\Services\TaxCalculator]
     [calls source: 'App\Services\TaxCalculator]
     [calls eval: (php->cljp (slurp "src/Services/TaxCalculator.php"))]
     [calls eval: (php/verify-equivalence ...)]

     "Here's the idiomatic ClojurePHP version.
      I've verified it produces identical results
      for all test cases."
```

**Implementation:**
- [ ] MCP server embedded in Swoole live image
- [ ] `eval` tool - execute cljp or php, return result
- [ ] `inspect` tool - deep dive into values
- [ ] `php-reflect` tool - class/method/property info
- [ ] `phpbench` tool - performance measurement
- [ ] `analyze` tool - code quality metrics
- [ ] `test` tool - run tests, structured results
- [ ] `source` / `doc` tools - code navigation
- [ ] Resource providers for namespace/class discovery
- [ ] Error context automatically available

The LLM gets the same REPL superpowers a human developer has - but can use them programmatically, in conversation.

---

## Phase 10: Cross-Platform Orchestration

The ultimate vision: **one REPL, all targets**.

### Architecture

```
┌──────────────────────────────────────┐
│           REPL Frontend              │
│  (Clojure, runs on JVM or any host)  │
├──────────────────────────────────────┤
│           Compiler                   │
│  source → HIR → target emitter       │
├──────────┬──────────┬────────────────┤
│ PHP      │ JS       │ Rust           │
│ Backend  │ Backend  │ Backend        │
└────┬─────┴────┬─────┴────────┬───────┘
     │          │              │
┌────▼────┐ ┌───▼───┐    ┌─────▼─────┐
│ PHP     │ │ Node  │    │ Genera    │
│ Process │ │Process│    │ Process   │
│ (eval)  │ │(eval) │    │ (eval)    │
└─────────┘ └───────┘    └───────────┘
```

### Unified REPL Interface

```clojure
$ cljp repl

user=> :target php
Switched to PHP runtime

user=> (+ 1 2)
3  ; executed on PHP

user=> :target js
Switched to JS runtime

user=> (+ 1 2)
3  ; executed on Node

user=> :target all
Broadcast mode

user=> (+ 1 2)
{:php 3, :js 3, :rust 3}  ; executed on all, results compared
```

### Orchestration Protocol

Runtimes communicate via a simple eval protocol:

```clojure
;; Request
{:op :eval :code "(+ 1 2)" :id 1}

;; Response
{:op :result :value 3 :id 1}

;; Lookup
{:op :lookup :sym 'x :id 2}
{:op :result :value 10 :id 2}

;; Error
{:op :error :message "Division by zero" :data {...} :id 3}
```

### FFI Orchestration (Cross-Runtime Calls)

```clojure
;; Call PHP from JS runtime
(php/call "file_get_contents" "/etc/hosts")

;; Under the hood:
;; 1. JS runtime sends request to orchestrator
;; 2. Orchestrator routes to PHP runtime
;; 3. PHP executes, returns result
;; 4. Result serialized back to JS
```

For performance-critical paths, compile-time FFI generation:

```clojure
;; Each platform implements natively
(defprotocol IFileSystem
  (read-file [this path]))

;; Calls stay within platform, no cross-runtime overhead
```

### Polyglot Extension (Future)

Same pattern extends to any language with a runtime:

```
┌─────────────────────────────────────────────────────┐
│                 Polyglot REPL                       │
│           (Clojure as lingua franca)                │
├─────────────────────────────────────────────────────┤
│                  Orchestrator                       │
├────────┬────────┬────────┬────────┬─────────────────┤
│ PHP    │ JS     │ Python │ Ruby   │ Rust/Native     │
│Runtime │Runtime │Runtime │Runtime │Runtime          │
└────────┴────────┴────────┴────────┴─────────────────┘

;; Same Clojure code, any target
(map inc [1 2 3])  ; works everywhere

;; Platform-specific when needed
(php/pdo-connect ...)
(py/numpy.array ...)
(js/fetch ...)
```

### Implementation Phases

```
Phase 10a: Single-platform REPLs (have this)
  └─ PHP REPL, Rust VM REPL

Phase 10b: Cross-platform REPL
  └─ Unified frontend, runtime backends via IPC
  └─ :target switching

Phase 10c: Broadcast/compare mode
  └─ Run on all, compare results
  └─ Property-based cross-platform testing

Phase 10d: FFI orchestration
  └─ Cross-runtime calls
  └─ Shared state protocols

Phase 10e: Polyglot (add more targets)
  └─ Python, Ruby, Go, etc.
  └─ Same pattern: emitter + runtime + protocol
```

### Why This Matters

- **Development**: Test on all platforms simultaneously
- **Migration**: Gradually move code between platforms
- **Debugging**: Compare behavior across runtimes
- **CI/CD**: Property-based cross-platform verification

**Clojure becomes the universal interface. Each runtime is a subprocess speaking a simple eval protocol.**

---

## Reference: Key Files in Other Implementations

### Clojure JVM
| File | Lines | Purpose |
|------|-------|---------|
| `Compiler.java` | ~8000 | Analyzer + emitter, special forms |
| `Compile.java` | ~85 | CLI entry point for AOT compilation |
| `LispReader.java` | ~3500 | Full reader with dispatch tables |
| `RT.java` | ~2500 | Runtime utilities |
| `clojure/main.clj` | ~677 | REPL, CLI dispatch, error triage |

### ClojureScript
| File | Lines | Purpose |
|------|-------|---------|
| `cljs/main.clj` | ~68 | Entry point (thin, delegates to cli) |
| `cljs/cli.clj` | ~749 | Full CLI with extensible commands |
| `cljs/repl.clj` | ~1500 | REPL implementation |
| `cljs/analyzer.cljc` | ~4000 | Analyzer (in cljc!) |
| `cljs/compiler.cljc` | ~2500 | Emitter (in cljc!) |

### ClojureDart
| File | Lines | Purpose |
|------|-------|---------|
| `LispReader.java` | ~1400 | Forked from Clojure, package `cljd.lang` |
| `LineNumberingWriter.java` | ~55 | Output position tracking |
| (everything else) | | Written in Clojure/cljd |

### ClojureCLR
| File | Lines | Purpose |
|------|-------|---------|
| `Main.cs` | ~58 | Thin shim, calls `clojure.main/main` |
| (rest mirrors JVM) | | C# ports of Clojure Java classes |

### SLIME (Debugger Architecture)
| File | Lines | Purpose | Type |
|------|-------|---------|------|
| `swank.lisp` | ~18000 | Main server, RPC, debugger loop | **[S]** Shared |
| `swank/backend.lisp` | ~1650 | Interface contracts (DEFINTERFACE) | **[S]** Shared |
| `swank/sbcl.lisp` | ~2050 | SBCL-specific implementations | **[P]** Platform |
| `swank/source-path-parser.lisp` | ~200 | Source location tracking | **[S]** Shared |

### SBCL (Debug Internals)
| File | Lines | Purpose |
|------|-------|---------|
| `src/code/debug.lisp` | ~2750 | Frame iteration, backtrace |
| `src/code/debug-int.lisp` | ~4150 | Debug API, frame/var access |
| `src/code/debug-info.lisp` | ~435 | Debug info structures (packed binary) |
| `src/code/restart.lisp` | ~300 | Restart binding and invocation |

---

## ClojurePHP File Structure (Shared vs Platform)

Based on the patterns from SLIME/SBCL and other Clojure implementations.

**Key insight:** Everything compiles from Clojure. No native runtime code needed for VM targets.

```
src/
├── clj/
│   └── clojure/                      # ALL CLOJURE (compiles to any target)
│       ├── core.cljc                 # Standard library
│       ├── main.cljc                 # CLI, REPL loop, error triage
│       ├── test.cljc                 # Test framework
│       ├── repl.cljc                 # REPL utilities (doc, source, etc.)
│       ├── walk.cljc                 # Tree traversal
│       ├── set.cljc                  # Set operations
│       ├── string.cljc               # String operations
│       ├── debugger.cljc             # IDebugBackend, SLDB loop, inspector
│       ├── condition.cljc            # with-restarts, invoke-restart
│       │
│       ├── lang/                     # Data structures (compile to target)
│       │   ├── kernel.cljc           # Cons, Sym, Kw, Atom (deftype)
│       │   ├── PersistentVector.cljc # HAMT vector
│       │   ├── PersistentHashMap.cljc# HAMT map
│       │   └── ...
│       │
│       ├── nrepl/
│       │   ├── server.cljc           # nREPL server
│       │   ├── bencode.cljc          # Bencode transport
│       │   └── middleware.cljc       # CIDER compatibility
│       │
│       ├── php/                      # PHP EMITTER (Clojure code)
│       │   ├── emit.cljc             # HIR → PHP syntax
│       │   ├── source-map.cljc       # Generate .map/.json
│       │   └── debug.cljc            # IDebugBackend impl
│       │
│       ├── js/                       # JS EMITTER (Clojure code)
│       │   └── emit.cljc             # HIR → JS syntax
│       │
│       └── rust/                     # RUST EMITTER (Clojure code)
│           └── emit.cljc             # HIR → Rust/bytecode
│
└── platforms/                        # PLATFORM MAPPINGS
    ├── php.cljc                      # Abstract ops → PHP calls
    ├── js.cljc                       # Abstract ops → JS calls
    └── dart.cljc                     # Abstract ops → Dart calls
```

**What's NOT needed:**
- No `runtime/php/*.php` - data structures compile from .cljc
- No native code per platform - just emitters
- No separate kernel implementations - deftype compiles everywhere

**The only platform-specific code is the emitter**, which outputs target syntax.

---

## Summary: The Architecture Principle

Following SBCL's philosophy but taken further: **100% Clojure, 0% native code**.

```
┌────────────────────────────────────────────────────────────────────┐
│                                                                    │
│   COMPILES TO ALL TARGETS (~95% of code)                           │
│   ──────────────────────────────────────                           │
│                                                                    │
│   • Kernel (Cons, Sym, Kw, Atom - deftype)                         │
│   • Data structures (PersistentVector, HashMap, etc.)              │
│   • Reader (syntax → forms with metadata)                          │
│   • Analyzer (forms → HIR with :env locations)                     │
│   • Standard library (clojure.core, clojure.string, etc.)          │
│   • REPL loop (hook-based, customizable)                           │
│   • nREPL server (bencode, CIDER middleware)                       │
│   • Error system (ex-triage, ex-str, phases)                       │
│   • Debugger protocol (IDebugBackend, SLDB loop)                   │
│   • Condition/restart system (with-restarts, invoke-restart)       │
│   • Inspector (stateful navigation, history)                       │
│   • Test framework (deftest, is, testing)                          │
│   • CLI dispatcher (two-phase, extensible)                         │
│                                                                    │
├────────────────────────────────────────────────────────────────────┤
│                                                                    │
│   EMITTER PER TARGET (~5% of code, still Clojure)                  │
│   ───────────────────────────────────────────────                  │
│                                                                    │
│   • emit.cljc (HIR → target syntax)                                │
│   • Platform mappings (abstract ops → platform calls)              │
│   • Source map format (target-specific)                            │
│                                                                    │
│   VM targets (PHP, JS, Dart): ~500 LOC each                        │
│   Native targets (Rust, WASM): ~1000 LOC each                      │
│                                                                    │
└────────────────────────────────────────────────────────────────────┘
```

**No native runtime code.** The host provides GC, closures, strings, arrays.
We just output syntax.

### Why This Matters

| Approach | Lines of Code | Semantic Drift | Maintenance |
|----------|---------------|----------------|-------------|
| Reimplement per platform | ~25K × N backends | Diverges over time | N teams |
| Shared core + thin backends | ~20K + 2K × N | Identical everywhere | 1 team |

**For 6 backends:**
- Reimplementation: ~150,000 lines, 6 communities, 6 bug trackers
- Shared + backends: ~32,000 lines, 1 community, 1 bug tracker

### The Contract

**HIR guarantees:**
```clojure
;; Every HIR node has source location
{:op :invoke
 :env {:file "app.cljc" :line 12 :column 5 :context :expr}
 :fn {...}
 :args [...]}
```

**Emitter implements:**
```clojure
;; clojure/php/emit.cljc (or js, rust, etc.)
(defmulti emit :op)

(defmethod emit :invoke [{:keys [fn args]}]
  (str (emit fn) "(" (str/join ", " (map emit args)) ")"))

(defmethod emit :const [{:keys [val]}]
  (pr-str val))

;; ... ~20 methods for each HIR node type
```

**Platform mappings provide:**
```clojure
;; platforms/php.cljc
(def mappings
  {:string/upper-case  'strtoupper
   :io/read-file       'file_get_contents
   :type/string?       'is_string})
```

**That's it.** No native code. No runtime to port. Just syntax emission + mappings.

---

## The Dream

Write Clojure. Evaluate instantly. See results. Navigate errors back to source.
PHP is invisible - just the fast, ubiquitous runtime underneath.

```
cljp> (defn fib [n]
        (if (<= n 1)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
=> #'user/fib

cljp> (map fib (range 10))
=> [0 1 1 2 3 5 8 13 21 34]

cljp> (time (fib 30))
"Elapsed time: 45ms"
=> 832040

cljp> (macroexpand-1 '(defn greet [x] x))
=> (def greet (fn [x] x))              ; ← in-process, instant

cljp> (ex-data *e)                      ; ← inspect last error
{:cljp.error/phase :execution
 :cljp.error/type :arity
 :cljp.error/symbol 'user/greet
 ...}
```

**And from PHP:**

```php
<?php
require 'vendor/clojurephp/runtime/autoload.php';

// The missing PHP REPL - now exists
$orders = Order::where('status', 'pending')->get();

// Clojure's power, PHP's ecosystem
$summary = cljp('
  (->> $orders
       (group-by :region)
       (map-vals #(reduce + (map :total %))))
');

// Best of both worlds
echo json_encode($summary);
```

That's the experience. Pure Clojure joy, PHP performance, everywhere PHP runs.

**For Clojure devs**: Deploy to $5/month hosting. Use any PHP library. No JVM.

**For PHP devs**: Get a REPL. Get immutable data. Get macros. Keep your codebase.

Eventually: no JVM required. Just `cljp` - a single PHP image that IS the Lisp. Delivered as `composer require clojurephp/runtime`.
