# ClojurePHP - Agent Guide

> ClojurePHP compiles Clojure (.cljc) to PHP via direct emission.
> Compiler runs on JVM, output runs on PHP.

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  .cljc file │ ──▶ │  Compiler   │ ──▶ │  .php file  │
│  (Clojure)  │     │  (JVM)      │     │  (PHP)      │
└─────────────┘     └─────────────┘     └─────────────┘
```

**No intermediate AST.** Forms are emitted directly to PHP strings.
This is the SLIME/ClojureDart approach, not the ClojureScript approach.

## Key Files

| File | Purpose |
|------|---------|
| `src/clj_php/emit/core.cljc` | Main dispatch, context handling, writer |
| `src/clj_php/emit/specials.cljc` | Special forms: if, let, fn, loop, do, try |
| `src/clj_php/emit/literals.cljc` | Literals: strings, numbers, vectors, maps |
| `src/clj_php/emit/interop.cljc` | PHP interop: php/, .method, new |
| `src/clj_php/core.cljc` | Core macros: defn, when, ->, cond |
| `src/clj_php/stubs/registry.cljc` | phpstorm-stubs lookup and validation |
| `src/php/Runtime.php` | PHP runtime: collections, keywords |

## Context Map

The `ctx` map controls how forms are emitted:

```clojure
{:statement true   ; Emit as PHP statement (no return needed)
 :return true      ; Wrap result in `return`
 :top-level true   ; At file/namespace scope
 :in-loop true     ; Inside loop (enables recur)
 :loop-syms [...]  ; Symbols bound by enclosing loop
 :options {...}}   ; Compiler options (:debug, :file, etc.)
```

### Context Rules

| Context | `if` emits | `let` emits |
|---------|-----------|-------------|
| `:statement` | `if (...) { } else { }` | `$x = 1; ...` |
| `:return` | `if (...) { return ...; }` | `$x = 1; return ...;` |
| neither | `(... ? ... : ...)` | `(call_user_func(...))` |

## Emission Pattern

All emit functions follow this pattern:

```clojure
(defn emit-X
  "Docstring explaining behavior and context handling.

   Examples:
     (emit-X ctx '(X ...)) => PHP output"
  [ctx form]
  ;; 1. Destructure form
  ;; 2. Check context flags
  ;; 3. Write PHP via (write ...)
  )
```

## Adding a New Special Form

1. Add `emit-X` function to `emit/specials.cljc`
2. Add case to `emit-special` multimethod in `emit/core.cljc`
3. Add test cases to `tests/specs.cljc`

## Adding PHP Interop

1. Function already works via `php/name` syntax
2. For validation, add to `stubs/registry.cljc`
3. For custom emission, add case to `emit/interop.cljc`

## Testing

```bash
# Unit tests (compiler output strings)
clj -M:test

# Integration tests (compile + run PHP)
clj -M:run test.cljc && php out.php
```

## Common Patterns

### IIFE Wrapping

When `let`/`do`/`loop` appear in expression position, wrap in IIFE:

```clojure
;; Expression position
(+ 1 (let [x 2] x))
;; => (1 + (call_user_func(function() { $x = 2; return $x; })))
```

This is correct but has overhead. See `docs/decisions.md#001`.

### Macro Expansion

Macros are evaluated on JVM, not PHP:

```clojure
(defmacro when [test & body]
  `(if ~test (do ~@body)))
```

The macro function lives in `*macros*` atom, called during `expand-form`.

## Debugging

```clojure
;; Enable debug output
(compile-forms forms {:debug true})
```

Debug mode adds newlines between statements for readability.

## PHP Runtime

`src/php/Runtime.php` provides:

- `Runtime::vector([...])` - Immutable vector
- `Runtime::map([k, v, ...])` - Immutable map
- `Runtime::keyword('name')` - Keyword interning
- `Runtime::conj(coll, x)` - Collection operations
- `Runtime::toArray(coll)` - Convert to PHP array

## Related Docs

- `docs/README.md` - Documentation index
- `docs/session.md` - Development log (current state)
- `docs/parity.md` - Clojure feature coverage
- `docs/internals.md` - Technical deep-dives
- `docs/php-target.md` - PHP version features
