# ClojurePHP Documentation

## Quick Links

| Document | Purpose |
|----------|---------|
| [session.md](session.md) | Handover doc (current state, next steps) |
| [parity.md](parity.md) | Clojure feature coverage & priorities |
| [internals.md](internals.md) | Technical deep-dives |
| [php-target.md](php-target.md) | PHP version features |
| [changelog.md](changelog.md) | Development history by phase |

## Project Overview

**ClojurePHP** compiles Clojure to PHP. The compiler runs on the JVM, output runs anywhere PHP runs.

```
.cljc source → [Compiler (JVM)] → .php output → [PHP Runtime]
```

### Value Proposition

PHP developers get Clojure's benefits without leaving PHP's ecosystem:
- Immutable data structures (persistent vectors, maps, sets)
- REPL-driven development
- Macros
- Functional programming patterns

While still:
- Using existing PHP libraries
- Deploying to standard PHP hosting
- Working with existing teams

### Architecture

```
src/cljp/           # Compiler (Clojure, runs on JVM)
├── main.clj        # CLI entry point
├── analyze.cljc    # Form → AST
├── emit.cljc       # AST → PHP
├── destructure.cljc # Destructuring expansion
├── infer.cljc      # Type inference
├── lift.cljc       # Statement lifting
└── stubs/          # phpstorm-stubs integration

src/php/            # Runtime (PHP)
├── Runtime.php     # Bootstrap, core functions
├── ErrorHandler.php # Source-mapped errors
└── Clojure/Lang/   # Persistent data structures
```

### Running the Compiler

```bash
# Compile to stdout
clj -M -m cljp.main input.cljc > out.php

# Compile with debug output
clj -M -m cljp.main --debug input.cljc > out.php

# Multi-file compilation
clj -M -m cljp.main --out-dir dist/ src/**/*.cljc
```

### Current Status

- **Compiler**: Production-ready for core features
- **Coverage**: ~30% of clojure.core (see [parity.md](parity.md))
- **PHP Target**: 8.4+ (8.5 for pipe operator)
- **Runtime**: Phel-based persistent collections

See [session.md](session.md) for development history and current phase.
