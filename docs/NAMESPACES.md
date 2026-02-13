# ClojurePHP Namespace Plan

## Architecture

ClojurePHP uses a platform-agnostic approach:

```
source.cljc (portable Clojure)
     │
     ▼
 ┌────────┐
 │ Reader │
 └────────┘
     │
     ▼
 ┌──────────┐
 │ Analyzer │
 └──────────┘
     │
     ▼
 ┌─────────────────────────────────┐
 │              HIR                │
 │    (platform-agnostic IR)       │
 └─────────────────────────────────┘
     │
     ├──────────────┬──────────────┐
     ▼              ▼              ▼
 ┌────────┐    ┌────────┐    ┌────────┐
 │emit/php│    │emit/jvm│    │emit/js │
 └────────┘    └────────┘    └────────┘
     │              │              │
     ▼              ▼              ▼
 ┌────────┐    ┌────────┐    ┌────────┐
 │PHP     │    │JVM     │    │JS      │
 │Runtime │    │Runtime │    │Runtime │
 └────────┘    └────────┘    └────────┘
```

## Design Principles

1. **JVM syntax as canonical form** - Write standard Clojure, runs everywhere
2. **No reader conditionals in user code** - Compiler handles platform mapping
3. **Platform interop when needed** - `php/`, `java/`, `js/` prefixes for escape hatches
4. **Upstream compatibility** - Real Clojure code runs unchanged

## Standard Namespaces

### Core

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.core` | Partial | Core language functions and macros |
| `clojure.string` | Stub | String manipulation |
| `clojure.set` | Stub | Set operations |
| `clojure.walk` | Implemented | Tree walking/transformation |
| `clojure.data` | Stub | Data diff/comparison |
| `clojure.edn` | Stub | EDN reader/writer |
| `clojure.pprint` | Stub | Pretty printing |
| `clojure.zip` | Stub | Zipper navigation |
| `clojure.template` | Stub | Code templating |
| `clojure.math` | Implemented | Math functions (maps to Math/*) |

### System & IO

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.system` | Implemented | System properties, env vars, exit (maps to System/*) |
| `clojure.io` | Stub | File/stream IO |
| `clojure.shell` | Stub | Shell command execution |

### Data Formats

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.data.json` | Stub | JSON encode/decode |
| `clojure.data.csv` | Stub | CSV parsing |
| `clojure.data.xml` | Stub | XML parsing |

### Time

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.time` | Stub | Date/time operations |

### Network

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.http` | Stub | HTTP client |
| `clojure.uri` | Stub | URI manipulation |

### Concurrency

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.core.async` | Stub | Async operations, channels |

### Testing & Tools

| Namespace | Status | Description |
|-----------|--------|-------------|
| `clojure.test` | Implemented | Unit testing framework |
| `clojure.bench` | Implemented | Benchmarking |
| `clojure.parity` | Implemented | Cross-platform parity testing |
| `clojure.repl` | Stub | REPL utilities |
| `clojure.stacktrace` | Stub | Exception formatting |

## Platform Interop

For platform-specific code, use prefixed namespaces:

```clojure
;; PHP interop
(php/file_get_contents path)
(php/array_map f arr)

;; JVM interop (when targeting JVM)
(java/System.currentTimeMillis)
(java/Thread.sleep 1000)

;; JS interop (when targeting JS)
(js/console.log msg)
(js/fetch url)
```

## JVM → Platform Mapping

The compiler maps common JVM idioms to platform equivalents:

| JVM | PHP | JS |
|-----|-----|-----|
| `System/nanoTime` | `hrtime(true)` | `performance.now() * 1e6` |
| `System/currentTimeMillis` | `(int)(microtime(true) * 1000)` | `Date.now()` |
| `System/getenv` | `getenv()` | `process.env` |
| `System/exit` | `exit()` | `process.exit()` |
| `Thread/sleep` | `usleep()` | `await sleep()` |
| `Math/abs` | `abs()` | `Math.abs()` |
| `Math/floor` | `floor()` | `Math.floor()` |
| `Math/ceil` | `ceil()` | `Math.ceil()` |
| `Math/sqrt` | `sqrt()` | `Math.sqrt()` |
| `Math/pow` | `pow()` | `Math.pow()` |
| `Math/random` | `mt_rand()/mt_getrandmax()` | `Math.random()` |
| `.getMessage` (ex) | `->getMessage()` | `.message` |
| `.length` (string) | `strlen()` | `.length` |

## Directory Structure

```
src/clojure/
├── core.cljc              # Core language
├── string.cljc            # String manipulation
├── set.cljc               # Set operations
├── walk.cljc              # Tree walking
├── data.cljc              # Data utilities
├── edn.cljc               # EDN format
├── pprint.cljc            # Pretty printing
├── zip.cljc               # Zippers
├── template.cljc          # Code templates
├── math.cljc              # Math functions
├── system.cljc            # System operations
├── io.cljc                # IO operations
├── shell.cljc             # Shell commands
├── time.cljc              # Date/time
├── http.cljc              # HTTP client
├── uri.cljc               # URI handling
├── test.cljc              # Testing framework
├── bench.cljc             # Benchmarking
├── parity.cljc            # Parity testing
├── repl.cljc              # REPL utilities
├── stacktrace.cljc        # Exception formatting
├── core/
│   └── async.cljc         # Async/channels
├── data/
│   ├── json.cljc          # JSON
│   ├── csv.cljc           # CSV
│   └── xml.cljc           # XML
└── php/
    ├── emit.cljc          # HIR → PHP emitter
    ├── analyzer.cljc      # Clojure → HIR analyzer
    └── runtime/           # PHP runtime implementations
        ├── Vec.php
        ├── Map.php
        ├── Set.php
        └── ...
```

## Implementation Priority

### Phase 1: Core
- [x] clojure.core (partial)
- [x] clojure.test
- [ ] clojure.string
- [ ] clojure.math
- [ ] clojure.system

### Phase 2: Data
- [ ] clojure.set
- [ ] clojure.walk
- [ ] clojure.data
- [ ] clojure.data.json
- [ ] clojure.edn

### Phase 3: IO
- [ ] clojure.io
- [ ] clojure.shell
- [ ] clojure.http

### Phase 4: Tools
- [ ] clojure.bench
- [ ] clojure.parity
- [ ] clojure.pprint
- [ ] clojure.repl
- [ ] clojure.stacktrace

### Phase 5: Advanced
- [ ] clojure.time
- [ ] clojure.zip
- [ ] clojure.template
- [ ] clojure.core.async
- [ ] clojure.data.csv
- [ ] clojure.data.xml
- [ ] clojure.uri
