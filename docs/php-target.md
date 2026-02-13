# PHP Target Version Strategy

## Version Requirements

| Version | Status | Notes |
|---------|--------|-------|
| **PHP 8.5** | Target | Full feature utilization |
| **PHP 8.4** | Minimum | Fallback for hosting constraints |
| **PHP 8.3** | Legacy | Phel compatibility baseline |

## PHP 8.5 Features to Leverage

### Pipe Operator `|>` — High Priority

**Perfect mapping for threading macros:**

```clojure
;; Clojure source
(-> input trim lower-case slugify)

;; Current output (nested calls)
slugify(strtolower(trim($input)))

;; PHP 8.5 output (native pipes)
$input |> trim(...) |> strtolower(...) |> slugify(...)
```

**Benefits:**
- Readable left-to-right flow matches Clojure mental model
- Debuggable: each step is a separate expression
- First-class callable syntax `fn(...)` integrates cleanly

**Implementation:** Modify `emit*` for `:thread-first` and `:thread-last` ops.

### `array_first()` / `array_last()` — High Priority

**Direct mapping eliminates runtime overhead:**

```clojure
(first coll)  ;; → array_first($coll)
(last coll)   ;; → array_last($coll)
```

**Current:** Uses Phel runtime `Seq::first()`.
**PHP 8.5:** Native C-level functions, zero overhead.

### `clone with` — Medium Priority

**Immutable object updates:**

```php
// PHP 8.5
$updated = clone($obj, ['name' => 'new-value']);
```

**Use case:** When targeting PHP classes with readonly properties.

```clojure
;; Could enable
(assoc php-obj :name "new-value")
;; → clone($php_obj, ['name' => 'new-value'])
```

### `#[NoDiscard]` Attribute — Low Priority

Mark pure functions to warn on unused return values:

```php
#[NoDiscard]
function strlen(string $s): int { ... }

strlen($x); // Warning: return value discarded
```

**Use case:** Emit on generated functions marked `:php/pure`.

---

## PHP 8.4 Fallback Strategy

When targeting 8.4, emit compatible alternatives:

| PHP 8.5 Feature | PHP 8.4 Fallback |
|-----------------|------------------|
| `\|>` pipe | Nested function calls |
| `array_first()` | `$arr[array_key_first($arr)]` |
| `array_last()` | `$arr[array_key_last($arr)]` |
| `clone with` | Manual clone + property assignment |

**Implementation:** Add `--php-version` CLI flag:
```bash
clj -M -m cljp.main --php-version 8.5 src.cljc  # Use pipes
clj -M -m cljp.main --php-version 8.4 src.cljc  # Nested calls
```

---

## PHP 8.4 Features

Released: November 2024

### Array Functions — High Priority

New array utilities that map directly to functional patterns:

```php
// array_find - find first matching element
array_find($arr, fn($v) => $v > 10);

// array_find_key - find key of first match
array_find_key($arr, fn($v) => $v > 10);

// array_any - true if any element matches
array_any($arr, fn($v) => $v > 10);

// array_all - true if all elements match
array_all($arr, fn($v) => $v > 10);
```

**Clojure mapping:**
```clojure
(some pred coll)   ;; → array_find($coll, $pred) or array_any($coll, $pred)
(every? pred coll) ;; → array_all($coll, $pred)
```

### Multibyte String Functions — Medium Priority

```php
mb_trim($str);      // Unicode-aware trim
mb_ltrim($str);     // Unicode-aware left trim
mb_rtrim($str);     // Unicode-aware right trim
mb_ucfirst($str);   // Unicode-aware capitalize first
mb_lcfirst($str);   // Unicode-aware lowercase first
```

### Property Hooks — Low Priority (Future)

PHP 8.4 adds get/set hooks for properties:

```php
class User {
    public string $name {
        get => strtoupper($this->name);
        set => strtolower($value);
    }
}
```

Could enable computed properties in `defrecord` if we add class generation.

### `#[Deprecated]` Attribute — Low Priority

Official way to mark deprecations:

```php
#[Deprecated("Use newFunction instead", since: "2.0")]
function oldFunction() { }
```

---

## PHP 8.3 Features

Released: November 2023

### Typed Class Constants — Low Priority

```php
class Foo {
    public const string VERSION = '1.0.0';
}
```

### `json_validate()` — Medium Priority

Validate JSON without parsing:

```clojure
;; Could add
(json-valid? s) ;; → json_validate($s)
```

### Dynamic Class Constant Fetch — Low Priority

```php
$class::$constantName  // Dynamic constant access
```

### `#[Override]` Attribute — Low Priority

Explicit override marker for methods.

---

## PHP 8.2 Features

Released: December 2022

### `readonly` Classes — Medium Priority

Make entire class immutable:

```php
readonly class Point {
    public function __construct(
        public int $x,
        public int $y,
    ) {}
}
```

**Clojure mapping:** Natural fit for `defrecord` - all generated record classes could be `readonly`.

### DNF Types (Disjunctive Normal Form) — Low Priority

Combine union and intersection types:

```php
function foo((Countable&Traversable)|null $input) { }
```

### `null`, `true`, `false` as Standalone Types — Medium Priority

```php
function alwaysFalse(): false { return false; }
function alwaysNull(): null { return null; }
```

**Use case:** Emit `false` return type for functions that return `false` on failure.

### `#[SensitiveParameter]` — Low Priority

Hide parameters in stack traces:

```php
function login(#[\SensitiveParameter] string $password) { }
```

### Random Extension — Low Priority

New `Random\Randomizer` class with pluggable engines.

### Constants in Traits — Low Priority

Traits can now have constants.

---

## PHP 8.1 Features

Released: November 2021

### First-Class Callable Syntax — High Priority

Create closures from callables:

```php
$fn = strlen(...);  // Creates Closure from strlen
$method = $obj->method(...);
$static = Foo::bar(...);
```

**Clojure mapping:** Essential for pipe operator `|>` in 8.5.

```clojure
;; Enables clean pipe emission
(-> x trim lower-case)
;; → $x |> trim(...) |> strtolower(...)
```

### Enums — Medium Priority

Native enumeration support:

```php
enum Status: string {
    case Draft = 'draft';
    case Published = 'published';
}
```

**Clojure mapping:** Could map keywords to enums when interop requires it.

### `readonly` Properties — Medium Priority

Immutable properties:

```php
class User {
    public readonly string $name;
}
```

**Use case:** Emit for `defrecord` fields.

### Intersection Types — Low Priority

Require all types:

```php
function foo(Countable&Traversable $input) { }
```

### `never` Return Type — Medium Priority

For functions that never return:

```php
function fail(): never {
    throw new Exception();
}
```

**Use case:** Emit for functions that always throw.

### `array_is_list()` — Medium Priority

Check if array is a list (sequential numeric keys):

```clojure
(vector? coll) ;; → array_is_list($coll)  ; partial match
```

### Fibers — Low Priority

Low-level async primitives. Framework-level concern.

---

## PHP 8.0 Features

Released: November 2020

### `match` Expression — High Priority

```clojure
;; Clojure
(cond
  (= x 1) "one"
  (= x 2) "two"
  :else "other")

;; Current PHP output
($x === 1) ? "one" : (($x === 2) ? "two" : "other")

;; With match (cleaner)
match($x) {
    1 => "one",
    2 => "two",
    default => "other"
}
```

**Caveat:** `match` uses strict equality and is exhaustive. Only emit for suitable `cond` patterns.

### Native Union Types — High Priority

```clojure
;; Clojure with type hints
(defn parse ^{:tag "int|false"} [^string s] ...)

;; Current output (docblock only)
/** @param string $s @return int|false */
function($s) { ... }

;; PHP 8.0+ (native)
function(string $s): int|false { ... }
```

### Null-Safe Operator `?->` — High Priority

Short-circuit on null:

```php
$result = $obj?->method()?->property;  // Returns null if any step is null
```

**Clojure mapping:**
```clojure
(some-> obj .method .property)
;; → $obj?->method()?->property
```

### Named Arguments — Medium Priority

```php
htmlspecialchars($string, double_encode: false);
```

**Clojure mapping:** Could support via keyword args:
```clojure
(php/htmlspecialchars s :double_encode false)
```

### Constructor Property Promotion — High Priority

Reduces boilerplate:

```php
// Before
class Point {
    public int $x;
    public int $y;
    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }
}

// After (PHP 8.0+)
class Point {
    public function __construct(
        public int $x,
        public int $y,
    ) {}
}
```

**Use case:** Emit for `defrecord` - much cleaner output.

### `mixed` Type — Low Priority

Explicit "any type" declaration:

```php
function foo(mixed $input): mixed { }
```

### Attributes — Medium Priority

Native metadata annotations:

```php
#[Route('/api/users')]
class UserController { }
```

**Use case:** Emit metadata from Clojure `^{:route "/api/users"}`.

### `str_contains()`, `str_starts_with()`, `str_ends_with()` — Medium Priority

```clojure
(clojure.string/includes? s sub) ;; → str_contains($s, $sub)
(clojure.string/starts-with? s prefix) ;; → str_starts_with($s, $prefix)
(clojure.string/ends-with? s suffix) ;; → str_ends_with($s, $suffix)
```

### Throw as Expression — Medium Priority

```php
$value = $input ?? throw new InvalidArgumentException();
```

Enables cleaner error handling patterns.

### Trailing Comma in Parameter Lists — Low Priority

Already using for cleaner diffs.

---

## Feature Priority Summary

| Priority | Feature | PHP Ver | Clojure Mapping |
|----------|---------|---------|-----------------|
| **Critical** | Pipe `\|>` | 8.5 | `->`, `->>` |
| **Critical** | First-class callables | 8.1 | HOF, pipe support |
| **High** | `array_first/last` | 8.5 | `first`, `last` |
| **High** | `match` expression | 8.0 | Equality `cond` |
| **High** | Union types (native) | 8.0 | Type hints |
| **High** | Null-safe `?->` | 8.0 | `some->` |
| **High** | Constructor promotion | 8.0 | `defrecord` |
| **Medium** | `array_find/any/all` | 8.4 | `some`, `every?` |
| **Medium** | `readonly` classes | 8.2 | `defrecord` |
| **Medium** | `never` return | 8.1 | Throwing fns |
| **Medium** | `array_is_list` | 8.1 | `vector?` |
| **Medium** | Named arguments | 8.0 | Keyword args |
| **Medium** | Attributes | 8.0 | Metadata |
| **Low** | Enums | 8.1 | Keywords? |
| **Low** | Fibers | 8.1 | Async |
| **Low** | Property hooks | 8.4 | Computed props |

---

## Parity Measurement with PHP-Parser

Use `nikic/php-parser` to measure what PHP constructs ClojurePHP generates:

```php
<?php
// script/measure_parity.php
require 'vendor/autoload.php';

use PhpParser\ParserFactory;
use PhpParser\NodeFinder;

$parser = (new ParserFactory)->createForNewestSupportedVersion();
$finder = new NodeFinder;

$code = file_get_contents($argv[1] ?? 'out.php');
$ast = $parser->parse($code);

$types = [];
foreach ($finder->find($ast, fn($n) => true) as $node) {
    $types[get_class($node)] = ($types[get_class($node)] ?? 0) + 1;
}

ksort($types);
foreach ($types as $type => $count) {
    $short = str_replace('PhpParser\\Node\\', '', $type);
    echo sprintf("%-50s %d\n", $short, $count);
}

echo "\nTotal node types: " . count($types) . "/192\n";
```

**Run:**
```bash
php script/measure_parity.php out.php
```

**Sample output:**
```
Expr\Assign                                        12
Expr\BinaryOp\Plus                                 8
Expr\Closure                                       5
Expr\FuncCall                                      23
Stmt\Expression                                    15
...
Total node types: 24/192
```

---

## Implementation Phases

### Phase A: Version Detection
- [ ] Add `--php-version` CLI flag (default: 8.5)
- [ ] Store version in emit context
- [ ] Version-gated emission helpers

### Phase B: PHP 8.5 Pipes
- [ ] Detect `->` / `->>` threading macros
- [ ] Emit `|>` with first-class callables
- [ ] Fallback to nested calls for 8.4

### Phase C: Native Array Functions
- [ ] `first` → `array_first()` (8.5) or runtime (8.4)
- [ ] `last` → `array_last()` (8.5) or runtime (8.4)

### Phase D: Match Expressions
- [ ] Detect equality-based `cond` patterns
- [ ] Emit `match` for PHP 8.0+
- [ ] Keep ternary chain for complex predicates

### Phase E: Native Union Types
- [ ] Parse union type metadata
- [ ] Emit native `int|string` for PHP 8.0+
- [ ] Keep docblock for PHP 7.x compatibility (if ever needed)

---

## Core Library Parity

ClojurePHP implements a subset of Clojure core functions, mapped to PHP semantics.

### Implementation Status

| Stage | Status | Functions |
|-------|--------|-----------|
| **Stage 1** | ✅ Complete | `map`, `filter`, `=`, `not=`, `and`, `or`, `if-let`, `when-let`, `last`, `second` |
| **Stage 2** | ✅ Complete | `some->`, `some->>`, `doseq`, `dotimes`, type predicates, `keys`, `vals`, `take`, `drop`, `range`, `concat`, `reverse` |
| **Stage 3** | ✅ Complete | `for` macro, `case`→`match`, `mapcat`, `map-indexed`, `keep`, `flatten`, `interleave`, `interpose`, arithmetic functions |

### Function → PHP Mapping

| Clojure | PHP Implementation | Notes |
|---------|-------------------|-------|
| `first` | `->first()` / `reset()` | Phel collection or PHP array |
| `rest` | `->rest()` / `array_slice` | Returns remaining elements |
| `next` | `rest` + nil check | Returns nil for empty |
| `cons` | `Runtime::cons()` | Prepends to collection |
| `conj` | `Runtime::conj()` | Appends (vector) or prepends (list) |
| `count` | `php/count` | Native PHP |
| `seq` | `count > 0 ? coll : nil` | Nil-punning |
| `map` | Loop + `conj` | Eager, returns vector |
| `filter` | Loop + predicate + `conj` | Eager, returns vector |
| `reduce` | Loop + accumulator | Tail-recursive |
| `=` | `Runtime::equals()` | Phel Equalizer |
| `keys` | `Runtime::keys()` | Returns vector of keys |
| `vals` | `Runtime::vals()` | Returns vector of values |
| `get` | `Runtime::get()` | Map/vector access |
| `assoc` | `Runtime::assoc()` | Immutable put |
| `dissoc` | `Runtime::dissoc()` | Immutable remove |
| `merge` | Reduce + assoc | Merges maps |
| `get-in` | Reduce + get | Nested access |
| `assoc-in` | Recursive assoc | Nested update |
| `update-in` | Recursive update | Nested update with fn |

### Type Predicates

| Clojure | PHP |
|---------|-----|
| `string?` | `is_string()` |
| `int?` | `is_int()` |
| `float?` | `is_float()` |
| `number?` | `is_numeric()` |
| `boolean?` | `is_bool()` |
| `nil?` | `is_null()` |
| `fn?` | `is_callable()` |
| `vector?` | `instanceof PersistentVectorInterface` |
| `map?` | `instanceof PersistentMapInterface` |
| `set?` | `instanceof PersistentHashSetInterface` |
| `list?` | `instanceof PersistentListInterface` |

### Sequence Predicates

| Clojure | PHP Implementation |
|---------|-------------------|
| `every?` | Loop with early false return |
| `some` | Loop with early truthy return |
| `not-every?` | `not (every? ...)` |
| `not-any?` | `not (some ...)` |

### Higher-Order Utilities

| Clojure | PHP Implementation |
|---------|-------------------|
| `constantly` | Returns fn that ignores args |
| `complement` | Returns fn with negated result |
| `partial` | Captures args, merges with later args |
| `comp` | Composes functions right-to-left |
| `juxt` | Returns vector of fn applications |
| `fnil` | Replaces nil first arg with default |

### Macros

| Clojure | Expansion |
|---------|-----------|
| `->` | Threads through first position |
| `->>` | Threads through last position |
| `and` | Nested if (short-circuit) |
| `or` | Nested if (short-circuit) |
| `when` | `(if test (do ...))` |
| `when-not` | `(if test nil (do ...))` |
| `if-let` | `let` + `if` with temp binding |
| `when-let` | `let` + `when` with temp binding |
| `cond` | Nested `if` expressions |
| `doseq` | Loop for side effects |
| `dotimes` | Counted loop |
| `some->` | Nil-safe threading |
| `some->>` | Nil-safe threading (last position) |

### PHP Version-Specific Mappings

| Function | PHP 8.5 | PHP 8.4 | PHP 8.0+ |
|----------|---------|---------|----------|
| `first` | `array_first()` | `$c[array_key_first($c)]` | Runtime |
| `last` | `array_last()` | `$c[array_key_last($c)]` | Runtime |
| `some` | — | `array_find()` | Loop |
| `every?` | — | `array_all()` | Loop |
| `->` | `\|>` pipe | Nested calls | Nested calls |
| `some->` | — | `?->` chain | Null checks |

### Closure Scope Handling

PHP closures don't automatically capture outer scope. ClojurePHP handles this by:

1. **Global definitions** (`def`): Stored in `$GLOBALS` and aliased locally
2. **Local captures** (`let`, fn params): Added to `use (&$var)` clause
3. **Function parameters**: Not captured (already in scope)

Example:
```clojure
(let [x 1]
  (fn [y] (+ x y)))
```

Emits:
```php
$x = 1;
(function($y) use (&$x) { return ($x + $y); })
```

---

## References

### PHP Version Documentation
- [PHP 8.5 Features - PHP.Watch](https://php.watch/versions/8.5)
- [PHP 8.4 Features - PHP.Watch](https://php.watch/versions/8.4)
- [PHP 8.3 Features - PHP.Watch](https://php.watch/versions/8.3)
- [PHP 8.2 Features - PHP.Watch](https://php.watch/versions/8.2)
- [PHP 8.1 Features - Stitcher.io](https://stitcher.io/blog/new-in-php-81)
- [PHP 8.2 Features - Stitcher.io](https://stitcher.io/blog/new-in-php-82)
- [PHP Changelog](https://www.php.net/ChangeLog-8.php)

### Tools
- [nikic/PHP-Parser](https://github.com/nikic/PHP-Parser) - AST parsing for parity measurement
- [JetBrains/phpstorm-stubs](https://github.com/JetBrains/phpstorm-stubs) - Type information for PHP functions
