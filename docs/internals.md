# ClojurePHP Internals

Technical deep-dives into implementation details.

---

## 1. Concurrency Model

### PHP's Reality

PHP has a **share-nothing** architecture. Each request is isolated:

```
Request 1 ──→ [PHP Process] ──→ Response
Request 2 ──→ [PHP Process] ──→ Response  (no shared state)
```

This is fundamentally different from Clojure's JVM model with shared memory.

### Current Implementation

| Clojure | ClojurePHP | Notes |
|---------|------------|-------|
| `atom` | `Clojure\Lang\Variable` | Per-request only |
| `deref` / `@` | `->deref()` | Works |
| `reset!` | `->set()` | Works |
| `swap!` | `swap!` function | Works |
| `ref` | - | No STM |
| `agent` | - | No async agents |

### Swoole for Concurrency

ClojurePHP requires Swoole for async operations. No fallback.

**Why:**
- Widely available (PECL, most hosting)
- Hooks make existing PHP code async transparently
- Channels/coroutines map 1:1 to Swoole primitives
- One codepath to maintain

```clojure
;; Runtime check
(when-not (php/extension_loaded "swoole")
  (throw (php/Exception. "ClojurePHP async requires Swoole extension")))
```

### core.async Mapping

| core.async | Swoole | Notes |
|------------|--------|-------|
| `chan` | `new Channel($capacity)` | capacity=1 for unbuffered |
| `chan n` | `new Channel($n)` | buffered |
| `>!` | `$channel->push($val)` | parks coroutine |
| `<!` | `$channel->pop()` | parks coroutine |
| `close!` | `$channel->close()` | |
| `go` | `go(function() { ... })` | Swoole coroutine |
| `timeout` | Channel + timer | close after delay |
| `alts!` | `Channel::select()` | multi-channel wait |

### Example: Channel Operations

```clojure
(def c (chan))

(go
  (>! c 42))

(go
  (println (<! c)))
```

Compiles to:

```php
$c = new \Swoole\Coroutine\Channel(1);

go(function() use ($c) {
    $c->push(42);
});

go(function() use ($c) {
    echo $c->pop();
});
```

---

## 2. FP Patterns in Generated PHP

### Current FP Constructs

| Pattern | PHP Mechanism | Example |
|---------|---------------|---------|
| First-class functions | `Closure` | `$f = (function($x) { ... });` |
| Function application | `call_user_func` | `call_user_func($f, $x)` |
| Variadic apply | `call_user_func_array` | `call_user_func_array($f, $args)` |
| Currying/Partial | Closure capture | `function(...$more) use ($f, $args)` |
| Immutable data | Phel collections | `PersistentVector`, `PersistentHashMap` |
| Higher-order fns | Closures as args | `$reduce($f, $init, $coll)` |

### Generated Code Examples

```php
// Composition (comp)
$comp = (function(...$fs) {
    $fs = array_reverse($fs);
    return (function(...$args) use ($fs) {
        $ret = call_user_func_array($fs[0], $args);
        foreach (array_slice($fs, 1) as $f) {
            $ret = call_user_func($f, $ret);
        }
        return $ret;
    });
});

// Partial application
$partial = (function($f, ...$args) {
    return (function(...$more) use ($f, $args) {
        return call_user_func_array($f, array_merge($args, $more));
    });
});

// Reduce via loop/recur (no stack overflow)
$reduce = (function($f, $val, $coll) {
    while ($coll) {
        $val = $f($val, $coll->first());
        $coll = $coll->rest();
    }
    return $val;
});
```

### PHP 8.x FP Improvements

| Feature | Current | PHP 8.x |
|---------|---------|---------|
| Function reference | `'strlen'` (string) | `strlen(...)` (Closure) |
| Piping | Nested calls | `$x \|> f(...) \|> g(...)` |
| Arrow functions | `function($x) { return $x + 1; }` | `fn($x) => $x + 1` |
| Null handling | `if ($x !== null)` | `$x?->method()` |

---

## 3. Self-Hosting Strategy

### The Vision

```
┌──────────────────────────────────────────────────────────────┐
│  Phase 1: Bootstrap                                           │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐     │
│  │  Clojure    │ ──→ │  Compiler   │ ──→ │    PHP      │     │
│  │  (JVM)      │     │  (JVM)      │     │  Output     │     │
│  └─────────────┘     └─────────────┘     └─────────────┘     │
├──────────────────────────────────────────────────────────────┤
│  Phase 2: Partial Self-Hosting                                │
│  JVM calls PHP for some modules (emit, lift)                  │
├──────────────────────────────────────────────────────────────┤
│  Phase 3: Full Self-Hosting                                   │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐     │
│  │  Clojure    │ ──→ │  Compiler   │ ──→ │    PHP      │     │
│  │  (.cljc)    │     │  (PHP)      │     │  Output     │     │
│  └─────────────┘     └─────────────┘     └─────────────┘     │
│                 No JVM required!                              │
└──────────────────────────────────────────────────────────────┘
```

### Module Layering

| Module | JVM Version | PHP Version | Swap Strategy |
|--------|-------------|-------------|---------------|
| **Reader** | `clojure.edn/read` | PHP EDN parser | Implement or use lib |
| **Analyzer** | `analyze.cljc` | `analyze.php` | Compile with itself |
| **AST** | `ast.cljc` | `ast.php` | Pure data, easy to port |
| **Emit** | `emit.cljc` | `emit.php` | String concatenation |
| **Lift** | `lift.cljc` | `lift.php` | Tree walking |
| **Infer** | `infer.cljc` | `infer.php` | AST transformation |

### Testing Strategy

Each module has a **conformance test suite**:

```clojure
(deftest emit-if-test
  (let [ast {:op :if :test {...} :then {...} :else {...}}]
    ;; Test JVM emit
    (is (= (jvm-emit ast) "if ($x) { ... } else { ... }"))
    ;; Test PHP emit (once compiled)
    (is (= (php-emit ast) "if ($x) { ... } else { ... }"))
    ;; They must match!
    (is (= (jvm-emit ast) (php-emit ast)))))
```

---

## 4. Runtime Architecture

### Directory Structure

```
src/php/Clojure/Lang/
├── Collections/
│   ├── Vector/          (PersistentVector, Transient, SubVector)
│   ├── Map/             (PersistentHashMap, HAMT nodes)
│   ├── HashSet/
│   ├── LinkedList/
│   └── Struct/
├── Keyword.php, Symbol.php, Variable.php
├── Hasher.php, Equalizer.php, TypeFactory.php
└── [Interfaces: ISeq, IFn, IMeta, etc.]
```

### Performance Optimizations Applied

| Optimization | Impact | Location |
|--------------|--------|----------|
| Keyword interning | O(1) equality via `===` | `Keyword.php` |
| Symbol interning | O(1) equality, cached hash | `Symbol.php` |
| Singleton Hasher | Removed instance storage | `Hasher.php` |
| Singleton Equalizer | Removed instance storage | `Equalizer.php` |
| String hash cache | 10K string cache | `Hasher.php` |
| Reusable Box | Reduced allocations | `Box.php` |
| Identity check first | Fast path in equals | `Equalizer.php` |

### Benchmark Results

| Operation | PHP (optimized) | Notes |
|-----------|-----------------|-------|
| Vector Append 1000 | 1.1ms | Good |
| Vector Get 1000 | 0.9ms | Good |
| Map Put 1000 | 4.4ms | 3.4x improvement |
| Map Get 1000 (cached) | 1.1ms | 8.5x with key caching |
| Keyword Creation 100 | 16.5μs | Interned |
| Keyword Equality 100 | 113.6μs | O(1) |

---

## 5. Rust Extension (Optional)

A Rust PHP extension (`ext-cljp/`) provides faster persistent data structures.

### When to Use

| Operation | PHP vs Rust | Recommendation |
|-----------|-------------|----------------|
| Vector ops | PHP wins | Use PHP |
| Map Put | Rust 3.4x faster | Use Rust for large maps |
| Map Get | Rust 4.2x faster | Use Rust for cold lookups |
| Keywords | PHP wins | Use PHP |

### Architecture

```
┌─────────────────────────────────────────────┐
│            ClojurePHP Code                   │
│         (clojure.core functions)             │
└─────────────────────┬───────────────────────┘
                      │
          ┌───────────┴───────────┐
          │                       │
          ▼                       ▼
┌─────────────────┐   ┌─────────────────────┐
│  PHP Collections │   │   Rust Extension    │
│  (small maps,    │   │   (large maps,      │
│   vectors,       │   │    bulk ops,        │
│   keywords)      │   │    hot paths)       │
└─────────────────┘   └─────────────────────┘
```

### Building the Extension

```bash
cd ext-cljp
cargo build --release

# Copy to PHP extensions directory
cp target/release/libcljp.so $(php-config --extension-dir)/cljp.so
echo "extension=cljp.so" >> php.ini
```

---

## 6. Compiler Pipeline

### Overview

```
Source → [analyze] → AST → [passes...] → Enriched AST → [emit] → PHP
              ↑           ↓                    ↓
         Multimethod   Composable          Multimethod
         per symbol    transforms          per :op
```

### AST Node Structure

```clojure
{:op :if
 :form '(if test then else)
 :env {:context :statement, :locals {...}}
 :test {:op :var :name 'x ...}
 :then {:op :const :val 1 ...}
 :else {:op :const :val 2 ...}
 :children [:test :then :else]
 :php/type "int"     ; From type inference
 :php/pure false}    ; From stub analysis
```

### Key Passes

1. **Macro expansion** - `defmacro` forms evaluated on JVM
2. **Destructuring** - Patterns expanded to flat bindings
3. **Analysis** - Forms converted to AST nodes
4. **Type inference** - Propagate types through AST
5. **Statement lifting** - Hoist statements from expressions
6. **Emission** - Generate PHP from AST

### Adding New Features

**New special form:**
1. Add `defmethod analyze-form` in `analyze.cljc`
2. Add `defmethod emit*` in `emit.cljc`
3. Add test cases

**New optimization:**
1. Add pass function
2. Insert into pass pipeline
3. Passes are composable transforms

---

## 7. Source Maps

### Structure

```
.xdebug/
├── out.php.map    # Xdebug-compatible source map
└── out.php.json   # Rich map with column info
```

### JSON Format

```json
{
  "version": 3,
  "file": "out.php",
  "sources": ["input.cljc"],
  "mappings": [
    {"php_line": 5, "source_line": 3, "source_col": 1, "form": "(defn foo ...)"}
  ]
}
```

### Error Handler Integration

`ErrorHandler.php` rewrites PHP stack traces to show Clojure source locations:

```
Before: Fatal error in out.php:48
After:  Fatal error in input.cljc:12:5 (defn process-data ...)
```

---

## 8. Development Workflow

### Quick Test Cycle

```bash
# Compile and run
clj -M -m cljp.main test.cljc > out.php && php out.php

# With debug output
clj -M -m cljp.main --debug test.cljc > out.php && php out.php
```

### Test Expression Helper (TODO)

```clojure
;; Dream API:
(test-expr '(let [[a b] [1 2]] (+ a b)) 3)  ; => ✓ pass
(test-expr '(let [{:keys [x]} {:x 1}] x) 1) ; => ✓ pass
```

### Watch Mode (TODO)

```bash
cljp watch src/ --out dist/ --run-tests
```
