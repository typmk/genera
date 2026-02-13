# Changelog

All notable changes to ClojurePHP are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.21.0] - 2026-01-01

### Added
- `lazy-seq` macro for lazy sequence creation
- `LazySeq`, `Cons`, `ArraySeq`, `EmptySeq` PHP classes
- `mapv` and `filterv` for eager evaluation
- Infinite sequence support

### Changed
- `map`, `filter`, `take`, `drop` are now lazy
- `cons` returns lazy `Cons` instead of forcing to array
- `seq`/`next` no longer force full realization

## [0.20.0] - 2025-12-31

### Changed
- Reduced HIR from 26 to 21 node types
- `set!` transforms to `:host-call` with `:call-type :assign`
- `var/#'` transforms to `:host-new` with `Variable` class
- `deftype`/`defrecord` transform to `Runtime::` calls

### Added
- Runtime methods: `defProtocol`, `extendType`, `protocolDispatch`, `defMulti`, `addMethod`, `multimethodDispatch`, `reify`, `defType`, `defRecord`

## [0.19.0] - 2025-12-26

### Added
- Full destructuring: `[a b & rest :as all]`, `{:keys [x] :or {x 0} :as m}`
- Nested destructuring patterns
- Multi-arity functions: `(fn ([x] ...) ([x y] ...))`

### Fixed
- Local variable shadowing (`$__L_` prefix)

## [0.18.0] - 2025-12-26

### Changed
- Inlined hot paths in IndexedNode
- Direct property access optimization
- Identity checks before equals
- Integer hash fast path

## [0.17.0] - 2025-12-26

### Added
- `ext-cljp` Rust extension using ext-php-rs + rpds

### Changed
- Rust 4-6x faster for maps; PHP faster for vectors/keywords
- PHP map optimization: 3.4x improvement

## [0.14.0] - 2025-12-26

### Added
- Migrated Phel runtime to `Clojure\Lang` namespace (70 PHP files)

### Changed
- Keyword/Symbol interning
- Singleton Hasher/Equalizer
- String hash caching (10K limit)

## [0.12.0] - 2025-12-26

### Added
- CLI options: `--typed`, `--ast`, `--debug`, `--out-dir`
- Class method stubs lookup
- Lazy loading of stubs
- Reader conditionals (`:cljp`)
- Multi-file compilation

## [0.8.0] - 2025-12-25

### Added
- phpstorm-stubs integration (5000+ functions)
- Type inference pass (`:php/type` metadata)
- PHPDoc generation
- Compile-time arity validation

## [0.4.0] - 2025-12-25

### Added
- AST foundation with uniform node structure
- Emit multimethods dispatching on `:op`
- Locus abstraction for statement/expression context
- Statement lifting to eliminate IIFEs
