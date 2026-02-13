# ClojurePHP Session Handover

Quick context for continuing development.

---

## Current State

**Phase 19 complete.** Compiler is functional with:
- Full destructuring (vector, map, nested)
- Multi-arity functions
- Type inference + PHPDoc generation
- phpstorm-stubs integration (5000+ functions)
- Optimized PHP runtime (Phel-based)

## Architecture

```
src/cljp/           # Compiler (Clojure, runs on JVM)
├── main.clj        # CLI entry
├── analyze.cljc    # Form → AST
├── emit.cljc       # AST → PHP
├── destructure.cljc # Destructuring expansion
├── infer.cljc      # Type inference
└── stubs/          # phpstorm-stubs

src/php/            # Runtime (PHP)
├── Runtime.php     # Bootstrap
└── Clojure/Lang/   # Persistent collections
```

## Run Commands

```bash
# Compile
clj -M -m cljp.main input.cljc > out.php

# Compile with types
clj -M -m cljp.main --typed input.cljc > out.php

# Run
php out.php
```

## What Works

- Special forms: `def`, `if`, `let`, `fn`, `loop/recur`, `try/catch`, `do`, `quote`
- Macros: `defn`, `when`, `cond`, `->`, `->>`, `and`, `or`, `doseq`, `dotimes`
- Destructuring in `let`, `loop`, `fn` params
- Multi-arity functions
- PHP interop: `php/fn`, `.method`, `Class/static`, `new`
- ~80 core functions (see `parity.md`)

## Known Issues

1. **`:do` in expression context** - Semicolons break ternary. Use `let` instead.
2. **Eager sequences** - No lazy evaluation yet.
3. **No TCO** - Use `loop/recur` for recursion.

## Next Steps (Priority Order)

1. **`case` macro** - Emit PHP `match` (quick win)
2. **`for` macro** - List comprehension
3. **`letfn`** - Mutually recursive local fns
4. **LazySeq** - Foundation for lazy evaluation
5. **Protocols/Multimethods** - Polymorphism

## Developer Experience TODOs

From user feedback:
- Source map → error line mapping (infrastructure exists in `.xdebug/`)
- Quick eval: `(compile-and-eval '(+ 1 2))` → `3`
- Test helper: `(test-expr '(+ 1 2) 3)` → pass/fail
- Watch mode: `cljp watch src/ --out dist/`

## Key Files to Know

| File | Purpose |
|------|---------|
| `src/cljp/emit.cljc` | All PHP emission logic |
| `src/cljp/analyze.cljc` | Form → AST, macro expansion |
| `src/cljp/destructure.cljc` | Destructuring patterns |
| `src/cljp/core.cljc` | Core macros (`defn`, `when`, etc.) |
| `src/php/Runtime.php` | PHP bootstrap |

## Related Docs

- `parity.md` - Feature coverage & priorities
- `internals.md` - Technical deep-dives
- `changelog.md` - Phase history
