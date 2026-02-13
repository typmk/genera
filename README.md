# Genera

A live programming environment inspired by Symbolics Genera, Smalltalk, and SLIME/SBCL.

## Vision

Genera combines:
- **Multi-target Clojure compiler** - Compile to PHP, Python, Rust, JavaScript, Julia, CLR, JVM
- **Live system introspection** - SLIME-like tools for the physical machine
- **Protocol bridges** - MCP, LSP, DAP integration for editor/tool connectivity

## Structure

```
genera/
├── src/                    # Compiler source
│   ├── php/                # PHP target (primary)
│   ├── py/                 # Python target
│   ├── rs/                 # Rust target
│   ├── js/                 # JavaScript target
│   ├── jl/                 # Julia target
│   ├── clr/                # CLR/.NET target
│   ├── jvm/                # JVM target
│   └── platforms/          # Platform-specific code
├── tools/
│   ├── defnet/             # Analysis/networking layer (submodule)
│   ├── defport/            # Protocol bridge - MCP, LSP, DAP (submodule)
│   ├── emacs/              # Emacs tooling (parinfer, bracket detection, MCP)
│   └── observatory/        # System introspection tools
│       ├── system-observatory/
│       ├── wsl-monitor/
│       └── wsl-taskman*/
├── ref/                    # Reference implementations (32 submodules)
│   ├── clojure/            # Clojure source
│   ├── clojurescript/      # ClojureScript
│   ├── sbcl/               # Steel Bank Common Lisp
│   ├── slime/              # SLIME (Superior Lisp Interaction Mode)
│   ├── otp/                # Erlang/OTP
│   └── ...                 # See ref/ for full list
├── tests/
├── docs/
├── config/
└── script/
```

## Quick Start

### Compile Clojure to PHP

```bash
clojure -M:cljp input.cljc > output.php
```

### Run the compiler

```bash
clojure -M:cljp --help
```

## Requirements

- Clojure 1.12+
- Java 22+ (for FFM)
- PHP 8.4+ (for runtime)

## Development

```bash
# Install dependencies
clojure -P

# Run tests
clojure -M:test

# Start REPL
clojure -M:dev
```

## Submodules

Clone with all reference implementations:

```bash
git clone --recurse-submodules https://github.com/typmk/ClojurePHP.git genera
```

Or initialize after clone:

```bash
git submodule update --init --recursive
```

## Related Projects

- [defnet](https://github.com/typmk/defnet) - Analysis and networking layer
- [defport](https://github.com/typmk/defport) - Multi-protocol support (MCP, LSP, DAP)

## License

MIT License - see [LICENSE](LICENSE)
