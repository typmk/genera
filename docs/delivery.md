# ClojurePHP Delivery: How Users Get and Run It

## Philosophy

**Zero friction at every entry point.**

A curious developer should be running Clojure on PHP within 60 seconds. An enterprise team should have clear paths for CI/CD, deployment, and dependency management.

**Composable, orthogonal design.**

Output shape, runtime mode, and cache strategy are independent axes. Test each axis once; composition handles the rest. That's Clojure.

**Follow `clj` conventions.**

One command (`cljp`), not multiple binaries. Same flag patterns as `clj` where applicable. Clojure developers should feel at home.

---

## It's Just Clojure

The ClojurePHP compiler is Clojure code that reads `.cljc` files and emits PHP. There's no magic — if you have `clj`, you have everything.

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  .cljc source                                                   │
│       │                                                         │
│       ▼                                                         │
│  clojure.php.main          Clojure namespace                    │
│  (reader, analyzer,        (runs on JVM or PHP)                 │
│   emitter)                                                      │
│       │                                                         │
│       ▼                                                         │
│  .php output                                                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Two Hosts, Same Compiler

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  JVM-HOSTED                                                     │
│                                                                 │
│  clj -M:cljp -c app.cljc -o app.php                             │
│                                                                 │
│  • Standard clj command                                         │
│  • Compiler is a Clojure library (clojure.php.main)             │
│  • Uses deps.edn for dependencies                               │
│  • Same as how ClojureScript/ClojureDart work                   │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  PHP-HOSTED (self-hosted)                                       │
│                                                                 │
│  cljp -c app.cljc -o app.php                                    │
│                                                                 │
│  • Standalone binary (no JVM required)                          │
│  • Same compiler, compiled to PHP                               │
│  • Same CLI, same output                                        │
│  • For environments without JVM                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### What `cljp` Actually Is

The `cljp` binary is just the self-hosted compiler:

```php
#!/usr/bin/env php
<?php
require __DIR__ . '/runtime/bootstrap.php';
require __DIR__ . '/compiler/main.php';  // clojure.php.main compiled to PHP
clojure\php\main($argv);
```

If you have `clj`, you don't need `cljp`. Just add the alias:

```clojure
;; deps.edn
{:aliases
 {:cljp {:extra-deps {io.github.clojure-php/clojure-php {:git/tag "v0.1"}}
         :main-opts ["-m" "clojure.php.main"]}}}
```

```bash
clj -M:cljp -r                        # REPL
clj -M:cljp app.cljc                  # run script
clj -M:cljp -c src/ -o out/           # compile project
clj -M:cljp -e '(println "hello")'    # eval
```

The `cljp` binary exists for:
- PHP developers who don't have JVM
- Production servers without JVM
- Simpler CI/CD pipelines
- The eventual self-hosted future (no JVM dependency at all)

---

## The Three Axes

```
┌─────────────────────────────────────────────────────────────────────┐
│                                                                     │
│  AXIS 1: OUTPUT SHAPE      AXIS 2: RUNTIME       AXIS 3: CACHE     │
│  ────────────────────      ───────────────       ─────────────     │
│                                                                     │
│  • file (.php)             • embed               • project          │
│  • executable (.phar)      • composer            • output           │
│  • directory               • path:X              • system           │
│                            • system              • none             │
│                            • none                • path:X           │
│                                                                     │
│  4 shapes × 5 runtimes × 5 caches = 100 combinations               │
│  Test: 4 + 5 + 5 = 14 cases. Composition handles the rest.         │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Entry Points

### 1. The Curious Explorer

**Goal:** Try it in 60 seconds, no install

```bash
# Web REPL (nothing to install)
open https://cljp.run

# One-liner eval
curl -s https://cljp.run/eval --data '(+ 1 2 3)'
# => 6

# Download and run
curl -sL https://cljp.run/hello.cljc | cljp -
```

### 2. The Clojure Developer

**Goal:** Use standard `clj` — it's just a Clojure library

```clojure
;; deps.edn (add this alias)
{:aliases
 {:cljp {:extra-deps {io.github.clojure-php/clojure-php
                      {:git/tag "v0.1.0" :git/sha "abc123"}}
         :main-opts ["-m" "clojure.php.main"]}}}
```

```bash
clj -M:cljp                           # REPL
clj -M:cljp app.cljc                  # run script
clj -M:cljp -c app.cljc -o app.php    # compile
clj -M:cljp -c src/ -o out/           # compile project
clj -M:cljp -c src/ -o out/ --watch   # watch mode
clj -M:cljp -m myapp.core             # run -main
clj -M:cljp -e '(+ 1 2)'              # eval
```

That's it. No special tooling. Same `clj` you already use. The compiler (`clojure.php.main`) is just Clojure code that emits PHP instead of JVM bytecode.

### 3. The PHP Developer

**Goal:** No JVM required — self-hosted compiler via Composer

```bash
# Install via Composer (includes self-hosted compiler)
composer require clojure-php/clojure-php

# REPL
./vendor/bin/cljp

# Run script
./vendor/bin/cljp app.cljc

# Compile
./vendor/bin/cljp -c app.cljc -o app.php

# Global install
composer global require clojure-php/clojure-php
cljp                                  # REPL
cljp app.cljc                         # run
cljp -c app.cljc -o app.php           # compile
```

The `cljp` binary is the same compiler as `clj -M:cljp`, but compiled to PHP. Same CLI, same output — just doesn't need JVM.

### 4. The Script Writer

**Goal:** Executable .cljc files

```clojure
#!/usr/bin/env cljp
;; hello.cljc

(println "Hello from Clojure!")
(println "Args:" (vec (rest php/$argv)))
(php/exit 0)
```

```bash
chmod +x hello.cljc
./hello.cljc --name world
```

**How it works:**

```
#!/usr/bin/env cljp
        │
        ▼
┌─────────────────────────────────────┐
│ cljp                                │
│                                     │
│ 1. Hash source path + mtime         │
│ 2. Check cache (system by default)  │
│ 3. If stale, compile → cache        │
│ 4. Execute cached PHP               │
│ 5. Pass through exit code           │
└─────────────────────────────────────┘
```

### 5. The Embedder

**Goal:** Clojure inside existing PHP apps

```php
<?php
require 'vendor/autoload.php';

use Clojure\Php\Clj;

// Eval a string
$sum = Clj::eval('(+ 1 2 3)');  // => 6

// Compile once, call many (faster)
$square = Clj::compile('(fn [x] (* x x))');
echo $square(5);  // => 25

// With PHP variables in scope
$users = User::where('active', true)->get();
$names = Clj::eval('(map :name $users)', ['users' => $users]);

// Heredoc for complex code
$transform = Clj::compile(<<<'CLJ'
    (fn [orders]
      (->> orders
           (filter #(> (:total %) 100))
           (group-by :region)
           (map-vals #(reduce + (map :total %)))))
CLJ);
```

### 6. The Seamless Includer

**Goal:** Include .cljc like .php

```php
<?php
require 'vendor/autoload.php';

// Register stream wrapper (once, in bootstrap)
Clojure\Php\StreamWrapper::register();

// Include .cljc directly - compiles on first use, caches thereafter
include 'cljc://src/myapp/core.cljc';
include 'cljc://src/myapp/utils.cljc';

// Functions available immediately
myapp\core\main();
```

**How it works:**

```
include 'cljc://src/app.cljc'
              │
              ▼
┌─────────────────────────────────────┐
│ StreamWrapper::stream_open()        │
│                                     │
│ 1. Resolve path                     │
│ 2. Check cache (project by default) │
│ 3. Compare mtimes                   │
│ 4. If stale, compile on-the-fly    │
│ 5. Return compiled PHP to include   │
└─────────────────────────────────────┘
```

### 7. The Framework User

**Goal:** Laravel/Symfony integration

```php
// Laravel: config/app.php
'providers' => [
    Clojure\Php\Laravel\ClojureServiceProvider::class,
],

// Now available everywhere
$result = clj('(+ 1 2)');

// Blade templates
{{ clj('(format-currency total)') }}

// Routes to Clojure handlers
Route::get('/api/users', 'cljc://app.handlers::list-users');
```

---

## Output Shapes

### Single File → Single File

```bash
cljp -c app.cljc -o app.php
cljp -c app.cljc -o build/app.php
```

### Single File → Executable

```bash
cljp -c app.cljc -x -o app.phar
cljp -c app.cljc -x -o myapp.phar
```

### Project → Directory

```bash
cljp -c src/ -o out/
```

```
out/
├── myapp/
│   ├── core.php
│   └── utils.php
└── bootstrap.php
```

### Project → Executable

```bash
cljp -c src/ -x -o myapp.phar
```

```
myapp.phar (single distributable file)
├── stub.php
├── myapp/
│   ├── core.php
│   └── utils.php
├── runtime/ (if embedded)
└── bootstrap.php
```

---

## Runtime Modes

### embed

Bundle the entire runtime in the output. Zero external dependencies.

```bash
cljp -c app.cljc -o app.php --runtime=embed
```

```php
<?php
// === ClojurePHP Runtime v0.1.0 ===
namespace Clojure\Php;
class Vec implements \ArrayAccess, \Countable { /* ... */ }
function vec(...$items) { return new Vec($items); }
// ... ~2000 lines ...

// === Compiled Code ===
namespace app;
\Clojure\Php\println("Hello!");
```

**Use when:** Distributing standalone scripts, maximum portability.

### composer

Require runtime via Composer autoload. The idiomatic PHP approach.

```bash
cljp -c app.cljc -o app.php --runtime=composer
```

```php
<?php
require __DIR__ . '/vendor/autoload.php';

namespace app;
\Clojure\Php\println("Hello!");
```

**Use when:** Normal project development, library authoring.

### path:X

Require runtime from an explicit path.

```bash
cljp -c app.cljc -o app.php --runtime=path:/opt/clojure-php
```

```php
<?php
require '/opt/clojure-php/bootstrap.php';

namespace app;
\Clojure\Php\println("Hello!");
```

**Use when:** Custom deployment environments, shared hosting.

### system

Require runtime from system-wide installation.

```bash
cljp -c app.cljc -o app.php --runtime=system
```

```php
<?php
require '/usr/local/share/cljp/runtime/bootstrap.php';

namespace app;
\Clojure\Php\println("Hello!");
```

**Resolution order:**

```
1. $CLJP_RUNTIME environment variable
2. /usr/local/share/cljp/runtime/
3. /usr/share/cljp/runtime/
4. ~/.local/share/cljp/runtime/
5. C:\Program Files\ClojurePHP\runtime\ (Windows)
```

**Use when:** System-wide scripts, shebang execution.

### none

No runtime. Compile Clojure syntax to PHP without Clojure data structures or core functions. Only special forms and PHP interop work.

```bash
cljp -c app.cljc -o app.php --runtime=none
```

```php
<?php
// No runtime required - pure PHP output

namespace app;

function find_user($id) {
    return \User::find($id);
}
```

**What works:**

```clojure
(def x 42)                        ; def
(defn greet [name] ...)           ; defn
(let [a 1] ...)                   ; let
(if test then else)               ; if
(fn [x] ...)                      ; fn
(do ...)                          ; do
(php/new User name)               ; interop
(php/-> obj method)               ; interop
(php/:: Class method)             ; interop
```

**What doesn't work:**

```clojure
(vec [1 2 3])                     ; no Vec class
(map inc coll)                    ; no map function
(reduce + coll)                   ; no reduce function
(atom 0)                          ; no Atom class
```

**Use when:** Writing PHP libraries in Clojure syntax, thin glue code, minimal footprint requirements.

---

## Cache Strategies

### project

Cache in project directory. Good for development.

```bash
cljp -c src/ -o out/ --cache=project
```

```
my-project/
├── src/
├── out/
└── .cljp-cache/           ← cache here
    ├── hashes.edn
    └── compiled/
```

### output

Cache alongside output. Keeps cache with build artifacts.

```bash
cljp -c src/ -o out/ --cache=output
```

```
my-project/
├── src/
└── out/
    ├── myapp/
    │   └── core.php
    └── .cache/            ← cache here
```

### system

Cache in user's home directory. Good for global tools and scripts.

```bash
cljp -c app.cljc -o app.php --cache=system
```

```
~/.cache/cljp/             (Linux/Mac)
%LOCALAPPDATA%\cljp\cache\ (Windows)
├── compiled/
│   ├── a1b2c3d4/          ← hash of source path + content
│   │   └── app.php
│   └── ...
└── metadata.edn
```

### none

No caching. Recompile every time.

```bash
cljp -c app.cljc -o app.php --cache=none
```

**Use when:** CI/CD builds, debugging cache issues.

### path:X

Cache at explicit path.

```bash
cljp -c app.cljc -o app.php --cache=path:/tmp/cljp-cache
```

**Use when:** Shared build caches, custom CI setups.

---

## CLI Reference

Following `clj` conventions: one command, init options run in order, one main option.

```
Usage: cljp [init-opt*] [main-opt] [arg*]

init-opts (repeatable, run in order):
  -i, --init path       Load file
  -e, --eval string     Eval expression (print non-nil)

main-opts (pick one):
  -r, --repl            Run REPL (default if no args)
  -m, --main ns         Call -main from namespace
  -c, --compile path    Compile file or directory
  path                  Run script (compile + execute)
  -                     Run from stdin
  -h, --help            Print help
  --version             Show version

compile-opts (with -c):
  -o, --output path     Output path (required with -c)
  -x, --executable      Output as .phar

runtime-opts:
  --runtime=embed       Bundle runtime in output
  --runtime=composer    Use Composer autoload (default)
  --runtime=system      Use system-installed runtime
  --runtime=path:X      Use runtime at path X
  --runtime=none        No runtime (syntax-only)

cache-opts:
  --cache=project       Cache in .cljp-cache/ (default)
  --cache=output        Cache in output/.cache/
  --cache=system        Cache in ~/.cache/cljp/
  --cache=none          No caching
  --cache=path:X        Cache at path X

build-opts:
  --dev                 Development build (default)
  --release             Production build (optimized, no source maps)
  --watch               Watch for changes and recompile

Environment Variables:
  CLJP_RUNTIME          Default runtime path
  CLJP_CACHE            Default cache path
  CLJP_SOURCEMAPS       Source map output directory
```

### Examples

```bash
# REPL
cljp
cljp -r

# Run script
cljp app.cljc
cljp -m myapp.core

# Eval
cljp -e '(+ 1 2)'
cljp -e '(println "hello")'

# Compile single file
cljp -c app.cljc -o app.php
cljp -c app.cljc -o app.php --runtime=embed

# Compile project
cljp -c src/ -o out/
cljp -c src/ -o out/ --release

# Compile to executable
cljp -c app.cljc -x -o app.phar
cljp -c src/ -x -o myapp.phar

# Watch mode
cljp -c src/ -o out/ --watch

# Combined
cljp -i setup.cljc -e '(start-server)' -m myapp.core
```

---

## Smart Defaults

```
┌──────────────────────────────────┬───────────┬───────────┬───────────┐
│ Command                          │ Output    │ Runtime   │ Cache     │
├──────────────────────────────────┼───────────┼───────────┼───────────┤
│ cljp app.cljc                    │ (run)     │ system    │ system    │
│ cljp -c app.cljc -o app.php      │ file      │ embed     │ project   │
│ cljp -c app.cljc -x -o app.phar  │ phar      │ embed     │ none      │
│ cljp -c src/ -o out/             │ directory │ composer  │ output    │
│ cljp -c src/ -x -o app.phar      │ phar      │ embed     │ none      │
│ ./script.cljc (shebang)          │ (cached)  │ system    │ system    │
│ Clj::eval('...')                 │ (memory)  │ composer  │ system    │
│ include 'cljc://...'             │ (cached)  │ composer  │ project   │
└──────────────────────────────────┴───────────┴───────────┴───────────┘
```

---

## Installation

### Via Composer (Recommended)

```bash
# Project-local
composer require clojure-php/clojure-php

# Global
composer global require clojure-php/clojure-php
```

### Via Installer Script

```bash
curl -sL https://cljp.run/install | sh
```

Installs:
- `/usr/local/bin/cljp` - the command (REPL, runner, compiler)
- `/usr/local/share/cljp/` - runtime

### Via Package Managers (Future)

```bash
brew install clojure-php       # macOS
apt install clojure-php        # Debian/Ubuntu
scoop install clojure-php      # Windows
```

### Composer Packages

```
clojure-php/runtime     Runtime only (data structures, core functions)
clojure-php/compiler    PHP-native compiler
clojure-php/clojure-php All-in-one (runtime + compiler)
```

```bash
# Full development
composer require clojure-php/clojure-php

# Production (pre-compiled code)
composer require clojure-php/runtime

# CI/CD (build only)
composer require --dev clojure-php/compiler
```

---

## Deployment Patterns

### Pattern 1: Pre-compiled (Recommended)

```bash
# CI/CD builds everything
cljp -c src/ -o dist/ --release

# Deploy compiled PHP + runtime
rsync -av dist/ vendor/clojure-php/ server:/var/www/
```

### Pattern 2: Standalone Executable

```bash
# Single portable file
cljp -c src/ -x -o myapp.phar --release

# Deploy anywhere
scp myapp.phar server:/usr/local/bin/
```

### Pattern 3: Docker

```dockerfile
# Build stage
FROM php:8.2-cli AS builder
COPY --from=composer:latest /usr/bin/composer /usr/bin/composer
WORKDIR /app
COPY . .
RUN composer install
RUN ./vendor/bin/cljp -c src/ -o dist/ --release

# Runtime stage
FROM php:8.2-fpm
COPY --from=builder /app/dist/ /var/www/
COPY --from=builder /app/vendor/clojure-php/runtime/ /var/www/vendor/clojure-php/runtime/
```

### Pattern 4: Serverless

```bash
# Minimal standalone for Lambda/Cloud Functions
cljp -c handler.cljc -x -o handler.phar --runtime=embed --release
```

---

## REPL Connectivity

### Local REPL

```bash
cljp

user=> (+ 1 2)
3
user=> (def x 42)
#'user/x
```

### Connect to Running App

```php
<?php
// Enable in development only
if (getenv('APP_ENV') === 'development') {
    Clojure\Php\Nrepl::start(['port' => 7888]);
}
```

```bash
cljp --connect localhost:7888
```

### Editor Integration

Works with any nREPL client:

- **Emacs**: `M-x cider-connect`
- **VS Code**: Calva → Connect to Running REPL
- **Neovim**: `:ConjureConnect 7888`
- **IntelliJ**: Cursive → Remote REPL

---

## The Flow

```
┌──────────────────────────────────────────────────────────────────────┐
│                         USER JOURNEYS                                │
├──────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  Curiosity      →  cljp.run              →  "This is nice"           │
│       │                                                              │
│       ▼                                                              │
│  Experiment     →  composer require      →  "Let me try locally"     │
│       │                                                              │
│       ▼                                                              │
│  Integration    →  Clj::eval / include   →  "Using in my app"        │
│       │                                                              │
│       ▼                                                              │
│  Commitment     →  Full .cljc project    →  "This is how I PHP now"  │
│       │                                                              │
│       ▼                                                              │
│  Evangelism     →  Publishing libraries  →  "Everyone should try"    │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘

Each step natural. No cliffs. No big rewrites. Just progressively more joy.
```

---

## Design Principles

### Orthogonal Composition

Output shape, runtime mode, and cache strategy are independent transforms:

```clojure
(defn compile [input opts]
  (-> input
      parse
      analyze
      emit-php
      (wrap-runtime (:runtime opts))   ; independent
      (package-output (:output opts))  ; independent
      (cache-result (:cache opts))))   ; independent
```

Test each axis. Composition handles the rest.

### Invisible Until Needed

Defaults work. Configuration exists for when you need it:

```bash
# Just works
cljp app.cljc

# Full control when needed
cljp -c app.cljc -x -o app.phar --runtime=system --cache=none --release
```

### PHP-Idiomatic

- Composer for dependencies
- PSR-4 autoloading
- Standard PHP deployment
- Works with existing tools (PHPUnit, PHPStan, etc.)

### Clojure-Idiomatic

- REPL-driven development
- nREPL protocol for editor integration
- Source maps for debugging
- Hot reload in development
