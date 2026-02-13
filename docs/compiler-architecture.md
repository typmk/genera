# ClojurePHP Compiler Architecture

## Learning from Other Clojure Implementations

This document captures insights from studying Clojure JVM, ClojureScript, ClojureCLR, and ClojureDart compilers, and how they inform ClojurePHP's design.

---

## The Compilation Pipeline

### How Clojure Implementations Map Forms to Target Platforms

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    Clojure Compilation Pipeline                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Source Text                                                             │
│       │                                                                  │
│       ▼                                                                  │
│  ┌─────────────────┐                                                    │
│  │     Reader      │  LispReader.java (JVM) / tools.reader (cljs/cljd) │
│  │                 │  → Forms with :line/:column metadata               │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │  Macroexpand    │  let → let*, fn → fn*, loop → loop*               │
│  │                 │  Destructuring happens HERE (macro layer)          │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │    Analyzer     │  Primitive forms → AST nodes                       │
│  │                 │  Only sees: let*, fn*, if, do, def, etc.          │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │    Emitter      │  AST → Target code (bytecode/JS/Dart/PHP)         │
│  └─────────────────┘                                                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Implementation Comparison

### Dispatch Mechanisms

| Implementation | Dispatch | AST Type | Self-Hosting |
|----------------|----------|----------|--------------|
| **Clojure JVM** | HashMap + Interfaces (`IParser`, `Expr`) | Java classes (`DefExpr`, `IfExpr`) | No (Java) |
| **ClojureCLR** | Dictionary + Interfaces | C# classes | No (C#) |
| **ClojureScript** | `defmulti` on `:op` keyword | Clojure maps | Yes (.cljc) |
| **ClojureDart** | `case` + functions | "dartsexp" lists | Yes (.cljc) |

### Clojure JVM's Elegant Pattern (Compiler.java)

Each special form has three components:

```java
// 1. Symbol constant
static final Symbol DEF = Symbol.intern("def");

// 2. Dispatch table mapping symbols to parsers
static final public IPersistentMap specials = PersistentHashMap.create(
    DEF, new DefExpr.Parser(),
    IF,  new IfExpr.Parser(),
    LET, new LetExpr.Parser(),
    // ...
);

// 3. Expr class with Parser, eval(), and emit()
static class DefExpr implements Expr {
    public Object eval() { ... }           // interpret directly
    public void emit(C ctx, ObjExpr objx, GeneratorAdapter gen) { ... }  // → bytecode

    static class Parser implements IParser {
        public Expr parse(C context, Object form) { ... }  // form → AST
    }
}
```

### ClojureScript's Separated Phases

```clojure
;; analyzer.cljc - forms → AST maps
(defmethod parse 'def [_ env form _] ...)
(defmethod parse 'if  [_ env form _] ...)
;; Returns: {:op :def :name ... :init ...}

;; compiler.cljc - AST maps → JavaScript
(defmulti emit* :op)
(defmethod emit* :def [ast] (emits ...))
(defmethod emit* :if  [ast] (emits "if(" test ")..."))
```

### ClojureDart's Minimal Approach

Single `emit` function with case dispatch, no separate AST:

```clojure
(defn emit [x env]
  (let [x (macroexpand-and-inline env x)]
    (cond
      (symbol? x) (emit-symbol x env)
      (number? x) x
      (seq? x)
      (case (first x)
        if      (emit-if x env)
        fn*     (emit-fn* x env)
        def     (emit-def x env)
        let*    (emit-let* x env)
        ;; ... more special forms
        (emit-fn-call x env))
      :else ...)))
```

---

## Key Insight: Destructuring is a MACRO

**Clojure's special forms are MINIMAL.** The `*` suffix indicates primitives:

| User-Facing | Primitive | Difference |
|-------------|-----------|------------|
| `let` | `let*` | `let` macro handles destructuring |
| `fn` | `fn*` | `fn` macro handles multi-arity, pre/post |
| `loop` | `loop*` | `loop` macro handles destructuring |

### Clojure's `let` macro (core.clj:4523)

```clojure
(defmacro let [bindings & body]
  `(let* ~(destructure bindings) ~@body))
```

The `destructure` function (~80 lines) transforms:
```clojure
(let [{:keys [x y]} m] body)
```
Into:
```clojure
(let* [map__1 m
       x (get map__1 :x)
       y (get map__1 :y)]
  body)
```

**The analyzer never sees destructuring patterns** — only simple `[sym val]` pairs.

---

## ClojurePHP Current vs Ideal Architecture

### Current Structure (11 files)

```
src/cljp/
├── ast.cljc           # Node constructors
├── analyze.cljc       # Forms → AST (handles destructuring!)
├── emit.cljc          # AST → PHP
├── locus.cljc         # Statement/expr context
├── lift.cljc          # Statement lifting pass
├── infer.cljc         # Type inference pass
├── docblock.cljc      # PHPDoc generation
├── destructure.cljc   # Destructuring expansion
├── stubs/registry.cljc # PHP function metadata
├── core.cljc          # Top-level compile
└── main.clj           # CLI entry
```

### Issues with Current Approach

1. **Analyzer does too much** — handles destructuring, multi-arity, which should be macros
2. **Separate passes** — `lift.cljc`, `infer.cljc` could be inline
3. **More files = harder to self-host**

### Ideal Structure (Clojure-style)

```
src/cljp/
├── macros.cljc        # let→let*, fn→fn*, destructuring
├── analyze.cljc       # ONLY primitive forms → AST
├── emit.cljc          # AST → PHP (with inline lift/infer)
├── stubs/registry.cljc
├── core.cljc
└── main.clj
```

### The Macro Layer

```clojure
;; macros.cljc
(defmacro let [bindings & body]
  `(let* ~(destructure bindings) ~@body))

(defmacro fn [& sigs]
  `(fn* ~@(normalize-fn-sigs sigs)))

(defmacro loop [bindings & body]
  `(loop* ~(destructure bindings) ~@body))
```

### Simplified Analyzer

```clojure
;; analyze.cljc - ONLY primitive forms
(defmethod analyze-form 'let* [env [_ bindings & body]]
  ;; bindings are ALWAYS simple: [x 1, y 2]
  ;; NO destructuring logic!
  )

(defmethod analyze-form 'fn* [env [_ params & body]]
  ;; params are ALWAYS simple symbols
  ;; NO multi-arity handling!
  )
```

---

## Lifting and Type Inference: ClojureDart's Approach

ClojureDart doesn't have separate `lift.cljc` or `infer.cljc`. Instead:

### Lifting is Inline

```clojure
;; ClojureDart: with-lifted macro used during emit
(defmacro with-lifted [[name expr] env wrapped-expr]
  `(let [~name ~expr]
     (if-some [[bindings# ~name] (liftable ~name ~env)]
       (list 'dart/let bindings# ~wrapped-expr)
       ~wrapped-expr)))

;; Used inline:
(with-lifted [dart-test (emit test env)] env
  (list 'dart/if dart-test then-expr else-expr))
```

### Type Inference is Lazy/On-Demand

```clojure
;; ClojureDart: infer-type reads metadata from dartsexp
(defn infer-type [x]
  (let [m (meta x)]
    (if (:dart/inferred m)
      m  ; already cached
      (cond
        (nil? x) {:dart/type dc-Null :dart/const true}
        (boolean? x) {:dart/type dc-bool :dart/const true}
        (seq? x)
        (case (first x)
          dart/if (merge-types (infer-type then) (infer-type else))
          dart/let (infer-type (last x))
          ...)))))
```

**No separate pass** — computed when needed during emit.

---

## Recommended Refactoring for ClojurePHP

### Phase 1: Macro Layer

Move destructuring from analyzer to macros:

```clojure
;; Before: analyze.cljc handles destructuring
(defmethod analyze-form :let [env [_ bindings & body :as form]]
  (let [expanded-pairs (destruct/expand-bindings bindings)]
    ...))

;; After: macro expands BEFORE analysis
(defmacro let [bindings & body]
  `(let* ~(destructure bindings) ~@body))

(defmethod analyze-form 'let* [env [_ bindings & body]]
  ;; bindings already flat!
  )
```

### Phase 2: Inline Emit Concerns

Merge `locus.cljc`, `lift.cljc`, `infer.cljc`, `docblock.cljc` into `emit.cljc`:

```clojure
;; emit.cljc

;; Locus (was 168 lines, now ~20)
(def return-locus {:pre "return " :post ";\n"})
(def expr-locus {:pre "" :post ""})

;; Lift (was 199 lines, now ~30 with macro)
(defmacro with-lifted [[name expr] & body]
  ...)

;; Infer (was 300 lines, now ~50 - lazy)
(defn infer-type [node]
  (or (:inferred-type node)
      (case (:op node)
        :const (literal-type (:val node))
        :if (union-type (infer-type (:then node))
                        (infer-type (:else node)))
        nil)))
```

### Phase 3: File Consolidation

```
Before: 11 files
After:  6 files

Keep separate:
  - destructure.cljc (used by macros)
  - stubs/registry.cljc (PHP metadata)

Merge into emit.cljc:
  - locus.cljc
  - lift.cljc
  - infer.cljc
  - docblock.cljc

Simplify:
  - analyze.cljc (only primitive forms)
```

---

## PHP as Target Platform

### Option 1: Compile to PHP Source (Current, Recommended)

```
.cljc → ClojurePHP Compiler → .php source → PHP Compiler → Opcodes
```

Benefits:
- Simple, works everywhere
- OPcache handles bytecode caching
- Source maps work (PHP line → Clojure line)
- Brownfield friendly (integrates with existing PHP)

This is exactly what ClojureScript (→ JS) and ClojureDart (→ Dart) do.

### PHP VM Opcodes (Reference)

PHP has 211 opcodes. Key categories:

| Category | Opcodes | Clojure Forms |
|----------|---------|---------------|
| Arithmetic | ADD, SUB, MUL, DIV | `+`, `-`, `*`, `/` |
| Comparison | IS_IDENTICAL, IS_EQUAL | `=`, `==` |
| Control Flow | JMP, JMPZ, JMPNZ | `if`, `cond` |
| Functions | DO_FCALL, RETURN | `fn`, function calls |
| Objects | NEW, CLONE | `new`, interop |

### OPcache Integration

```php
// opcache.preload for production
<?php
opcache_compile_file('clojurephp-runtime.php');
opcache_compile_file('my-app.php');
```

---

## Self-Hosting Path

Following ClojureDart's minimal bootstrap (only 2 Java files):

| ClojureDart | ClojurePHP |
|-------------|------------|
| `LispReader.java` | `LispReader.php` |
| `LineNumberingWriter.java` | `LineNumberingWriter.php` |
| (rest in cljd) | (rest in cljp) |

**The reader is the bootstrap barrier.** Once `LispReader.php` exists:
1. JVM Clojure compiles `clojure.core` → PHP
2. JVM Clojure compiles the analyzer/emitter → PHP
3. PHP can now read + compile `.cljc` files itself
4. No more JVM dependency

---

## Summary

### Key Insights

1. **Destructuring belongs in macros**, not the analyzer
2. **Special forms should be minimal** (`let*`, `fn*`, not `let`, `fn`)
3. **Lifting and type inference can be inline** in emit
4. **ClojureDart's approach is simpler** for self-hosting
5. **PHP source as target** is the right choice (like CLJS → JS)

### Trade-offs

| Approach | Pros | Cons |
|----------|------|------|
| Current (structured AST) | Debugging, tooling, passes | More files, more to port |
| ClojureDart (inline) | Simpler, fewer primitives | Less modular |

### Recommendation

Keep the 2-phase structure (analyze → emit) but:
1. Move destructuring to macro layer ✅ **DONE** (macros.cljc)
2. Inline emit-time concerns (lift, infer, locus)
3. Simplify analyzer to only handle primitive forms ✅ **DONE** (let*, fn*, loop*)
4. Target 6-7 files instead of 11

---

## Performance Optimization

### Current Performance Bottlenecks

Analysis of the compilation pipeline identified these issues, ranked by impact:

| Priority | Issue | Impact | Location |
|----------|-------|--------|----------|
| **CRITICAL** | Atom-based string concat | O(n²) strings | `emit.cljc:20-22` |
| **HIGH** | Symbol munging repeated | No caching | `emit.cljc:66-95` |
| **MEDIUM** | Multiple AST passes | 3-4 walks | analyze→infer→emit |
| **MEDIUM** | Multimethod overhead | ~100 defmethods | All passes |
| **LOW** | Env cloning at bindings | Map copies | `analyze.cljc` |

### Current Pipeline Cost

```
compile-form (per form)
├─ ana/analyze         O(AST) × multimethod dispatch
├─ infer/infer-types   O(AST) × multimethod dispatch [optional]
└─ emit/emit-php-lifted
   ├─ lift/extract     O(statements)
   └─ emit*            O(AST) × multimethod × string concat
```

**Total passes**: 3-4 per form (without --typed) to 4-5 (with --typed)

---

### Optimization 1: StringBuilder Pattern (CRITICAL)

**Problem**: Current emit uses atomic string concatenation

```clojure
;; CURRENT: O(n²) - creates new string each time
(def ^:dynamic *output* (atom ""))
(defn write [& strings]
  (doseq [s strings]
    (swap! *output* str s)))  ; ← Atomic update + string copy
```

**Solution**: Use Java StringBuilder

```clojure
;; OPTIMAL: O(n) - mutates in place
(def ^:dynamic *sb* nil)

(defn write [& strings]
  (doseq [s strings]
    (when s (.append ^StringBuilder *sb* s))))

(defn emit-php [node]
  (binding [*sb* (StringBuilder. 8192)]  ; preallocate
    (emit* node)
    (.toString *sb*)))
```

**Impact**: 10-100x faster for large files.

---

### Optimization 2: Case Dispatch (HIGH)

**Problem**: Multimethod dispatch has overhead (~100 defmethods across passes)

```clojure
;; CURRENT: Multimethod overhead per node
(defmulti emit* :op)
(defmethod emit* :if [node] ...)
(defmethod emit* :do [node] ...)
;; 40+ methods
```

**Solution**: Direct case dispatch

```clojure
;; OPTIMAL: O(1) case dispatch
(defn emit* [{:keys [op] :as node}]
  (case op
    :const      (emit-const node)
    :var        (emit-var node)
    :if         (emit-if node)
    :do         (emit-do node)
    :let        (emit-let node)
    :fn         (emit-fn node)
    :fn-multi   (emit-fn-multi node)
    :invoke     (emit-invoke node)
    :loop       (emit-loop node)
    :recur      (emit-recur node)
    :def        (emit-def node)
    :throw      (emit-throw node)
    :try        (emit-try node)
    :quote      (emit-quote node)
    :new        (emit-new node)
    :method     (emit-method node)
    :static-call (emit-static-call node)
    :php-call   (emit-php-call node)
    :php-infix  (emit-php-infix node)
    :infix      (emit-infix node)
    :php-const  (emit-php-const node)
    :vector     (emit-vector node)
    :map        (emit-map node)
    :set        (emit-set node)
    (throw (ex-info "Unknown op" {:op op}))))
```

**Impact**: 2-3x faster dispatch.

---

### Optimization 3: Memoized Symbol Munging (HIGH)

**Problem**: Symbol munging happens per reference, not per binding

```clojure
;; CURRENT: Called every time variable is referenced
(defn munge-name [s]
  (apply str (map #(get munge-map % %) (str s))))

(defn munge-local [s]
  (str "$__L_" (munge-name s)))
```

**Solution A**: Memoize at runtime

```clojure
(def munge-cache (atom {}))

(defn munge-name [s]
  (or (get @munge-cache s)
      (let [munged (apply str (map #(get munge-map % %) (str s)))]
        (swap! munge-cache assoc s munged)
        munged)))
```

**Solution B**: Store munged name in AST at binding time

```clojure
;; In analyze-form :let*
(let [munged (munge-local name)
      binding {:name name :munged munged :init init-node}]
  ...)

;; In emit, use pre-computed name
(defn emit-var [{:keys [munged]}]
  (write munged))
```

**Impact**: 2-3x faster for variable-heavy code.

---

### Optimization 4: Lazy Type Inference (MEDIUM)

**Problem**: Full AST walk even when types aren't used

```clojure
;; CURRENT: Separate pass over entire AST
(defn compile-form [form {:keys [typed?]}]
  (let [ast (ana/analyze env form)
        ast (if typed? (infer/infer-types ast) ast)]  ; ← Full walk
    ...))
```

**Solution**: Infer on-demand during emit

```clojure
;; OPTIMAL: Lazy inference, cached in node
(defn infer-type [node]
  (or (:inferred-type node)  ; Check cache first
      (let [t (case (:op node)
                :const (literal-type (:val node))
                :if (union-type (infer-type (:then node))
                                (infer-type (:else node)))
                :invoke (return-type-of (:fn node))
                nil)]
        ;; Cache would require mutable node or separate map
        t)))

;; Only called when emitting PHPDoc
(when (and typed? (needs-docblock? node))
  (emit-docblock (infer-type node)))
```

**Impact**: Eliminates pass when types unused; faster when partially used.

---

### Optimization 5: Single-Pass Direct Emit (ClojureDart Style)

For maximum performance and minimal self-hosting complexity:

```clojure
(defn emit [x env ^StringBuilder sb]
  (let [x (macroexpand env x)]
    (cond
      (nil? x)     (.append sb "null")
      (number? x)  (.append sb (str x))
      (string? x)  (doto sb (.append "'") (.append (escape x)) (.append "'"))
      (symbol? x)  (.append sb (munge-local x))
      (keyword? x) (emit-keyword x sb)

      (seq? x)
      (case (first x)
        if      (emit-if x env sb)
        do      (emit-do x env sb)
        let*    (emit-let x env sb)
        fn*     (emit-fn x env sb)
        loop*   (emit-loop x env sb)
        recur   (emit-recur x env sb)
        def     (emit-def x env sb)
        quote   (emit-quote x env sb)
        new     (emit-new x env sb)
        try     (emit-try x env sb)
        throw   (emit-throw x env sb)
        ;; default: function call
        (emit-invoke x env sb))

      (vector? x) (emit-vector x env sb)
      (map? x)    (emit-map x env sb)
      (set? x)    (emit-set x env sb))))
```

**Trade-offs**:

| Aspect | AST-based (current) | Direct emit |
|--------|---------------------|-------------|
| Speed | Slower (3-4 passes) | Faster (1 pass) |
| Tooling | Full AST available | Limited |
| Error messages | Rich context | Form only |
| Self-host LOC | ~1500 lines | ~600 lines |
| Debugging | Easy (inspect AST) | Harder |

---

### Performance Optimization Phases

#### Phase 1: Quick Wins (No Architecture Change)

| Change | Effort | Impact |
|--------|--------|--------|
| StringBuilder in emit | 2 hours | 10-100x large files |
| Memoize symbol munging | 1 hour | 2-3x variable-heavy |
| Case dispatch in emit | 3 hours | 2-3x overall |

#### Phase 2: Inline Concerns

| Change | Effort | Impact |
|--------|--------|--------|
| Inline locus.cljc | 1 hour | Cleaner code |
| Inline lift.cljc | 2 hours | Remove pass |
| Lazy type inference | 2 hours | Skip when unused |
| Inline docblock.cljc | 1 hour | Cleaner code |

#### Phase 3: Architecture Options

**Option A: Optimized AST-based**
- Keep analyze → emit separation
- Apply all Phase 1 + Phase 2 optimizations
- ~1300 LOC to port for self-hosting

**Option B: Direct emit (ClojureDart style)**
- Single pass, no intermediate AST
- Maximum performance
- ~600 LOC to port for self-hosting
- Lose AST-based tooling

**Option C: Hybrid**
- AST for development/debugging
- Direct emit for production builds
- Flag to switch: `--fast` uses direct emit

---

### Benchmarking Targets

After optimizations, target these metrics:

| Metric | Current | After Phase 1 | After Phase 2 |
|--------|---------|---------------|---------------|
| 100-form file | ~50ms | ~10ms | ~5ms |
| 1000-form file | ~500ms | ~50ms | ~25ms |
| core.cljc (872 lines) | ~200ms | ~30ms | ~15ms |
| Memory per form | ~2KB AST | ~2KB AST | ~1KB (lazy) |

---

## Implementation Status

### Completed ✅

1. **Macro layer** (macros.cljc)
   - `let` → `let*` with destructuring
   - `fn` → `fn*` with destructuring
   - `loop` → `loop*` with destructuring
   - `defn` → `def` + `fn*`

2. **Primitive forms in analyzer**
   - `:let*` - simple bindings only
   - `:fn*` - simple params only
   - `:loop*` - simple bindings only

3. **LispReader.php** for self-hosting bootstrap

4. **Phase 1 Performance Optimizations** (emit.cljc)
   - ✅ StringBuilder for O(n) string building (was O(n²))
   - ✅ Memoized symbol munging (cache hit for repeated symbols)
   - ✅ Case dispatch for hot path (2-3x faster than multimethod)

5. **Phase 2 Module Consolidation** (emit.cljc)
   - ✅ Inlined locus.cljc (~60 lines) - standalone file deleted
   - ✅ Inlined lift.cljc (~50 lines) - standalone file deleted
   - ✅ Lazy type inference (infer.cljc now has `get-type` API)
   - ✅ Deleted docblock.cljc (was unused, emit.cljc has own impl)

### Pending 🔲

1. **PHP core functions** (predicates, seq ops)
2. **PHP analyzer** (port analyze.cljc)
3. **PHP emitter** (port emit.cljc)
