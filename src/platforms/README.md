# Platform Mappings

This directory contains platform-specific mappings that translate abstract Clojure operations to platform-native calls.

## Architecture

```
Clojure code (portable)
       ↓
   (string/upper-case s)
       ↓
Emitter looks up in platforms/php.cljc
       ↓
   {:string/upper-case 'strtoupper}
       ↓
PHP output: strtoupper($s)
```

## Files

| File | Target | Description |
|------|--------|-------------|
| `php.cljc` | PHP | Maps to PHP functions |
| `js.cljc` | JavaScript | Maps to JS functions/methods |
| `dart.cljc` | Dart | (future) |
| `rust.cljc` | Rust | (future) |

## Usage

The emitter loads its platform mappings at require time:

```clojure
;; clojure/php/emit.cljc
(ns clojure.php.emit
  (:require [platforms.php :as platform]))

(defn emit-abstract-call [op args]
  (if-let [php-fn (platform/get-mapping op)]
    (emit-platform-call php-fn args)
    (throw (ex-info "No mapping for operation" {:op op}))))
```

## Mapping Categories

Each platform file organizes mappings by category:

- `:type/*` - Type predicates (string?, number?, etc.)
- `:io/*` - File system operations
- `:string/*` - String manipulation
- `:math/*` - Mathematical functions
- `:array/*` - Array operations (for HAMT internals)
- `:time/*` - Time/date operations
- `:system/*` - OS/process operations
- `:hash/*` - Hashing functions
- `:json/*` - JSON encoding/decoding
- `:regex/*` - Regular expression operations
- `:output/*` - Print/output operations

## Cross-Platform Remapping

Each platform file includes reverse mappings for cross-platform code translation:

```clojure
;; If you have PHP-specific code:
(php/strtoupper s)

;; The compiler can remap to abstract:
(string/upper-case s)

;; Then emit for any target:
s.toUpperCase()  ; JS
s.toUpperCase()  ; Dart
```

## Adding a New Platform

1. Create `platforms/newplatform.cljc`
2. Define mappings for each category
3. Implement `get-mapping` function
4. (Optional) Add reverse mappings for remapping support

Example template:

```clojure
(ns platforms.newplatform)

(def type-mappings
  {:string?     'native_is_string
   :number?     'native_is_number
   ...})

(def string-mappings
  {:upper-case  'native_upper
   :lower-case  'native_lower
   ...})

;; ... other categories ...

(def mappings
  (merge
   (into {} (map (fn [[k v]] [(keyword "type" (name k)) v]) type-mappings))
   (into {} (map (fn [[k v]] [(keyword "string" (name k)) v]) string-mappings))
   ...))

(defn get-mapping [op]
  (get mappings op))
```

## Method vs Function

Some platforms (JS, Dart) use method syntax vs function syntax:

```clojure
;; platforms/js.cljc
(def string-mappings
  {:upper-case  '.toUpperCase   ; method - emits: s.toUpperCase()
   :length      '.-length})     ; property - emits: s.length

;; platforms/php.cljc
(def string-mappings
  {:upper-case  'strtoupper     ; function - emits: strtoupper($s)
   :length      'strlen})       ; function - emits: strlen($s)
```

The emitter handles this distinction based on symbol prefix:
- `.method` → method call
- `.-prop` → property access
- `function` → function call
