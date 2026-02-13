# Clojure Parity

Tracking ClojurePHP's implementation of Clojure features.

---

## Quick Status

| Category | Coverage | Notes |
|----------|----------|-------|
| Special Forms | 20/22 (91%) | Missing: monitor-enter/exit (Java-specific) |
| Core Macros | 52/70 (74%) | +defn-, defonce, declare, binding, with-bindings, with-open |
| Core Functions | ~280/493 (57%) | +regex, tree-seq, transducers, format, bit ops |
| Data Structures | 12/14 (86%) | +Queue, SortedMap, SortedSet |
| Concurrency | 30/40 (75%) | +refs, dosync, pmap, core.async channels (Swoole) |
| Transducers | Full | transduce, eduction, all xforms |

---

## Special Forms

| Form | Status | Notes |
|------|--------|-------|
| `def` | [x] | Global definition |
| `if` | [x] | Statement and expression contexts |
| `do` | [x] | Block execution |
| `let` | [x] | With full destructuring |
| `fn` | [x] | Multi-arity support |
| `loop` | [x] | With destructuring |
| `recur` | [x] | Tail recursion |
| `quote` | [x] | Literal data |
| `try/catch/finally` | [x] | Exception handling |
| `throw` | [x] | Exception throwing |
| `new` | [x] | Object instantiation |
| `.` (dot) | [x] | Method/field access |
| `defmacro` | [x] | Macro definition (JVM-side) |
| `var` | [x] | Var references (#'foo) |
| `set!` | [x] | Mutable assignment |
| `letfn` | [x] | Mutual recursion |
| `case*` | [x] | Emits PHP `match` expression |
| `binding` | [x] | Dynamic vars via Var system |
| `monitor-enter/exit` | [ ] | Locking (Java-specific, use Swoole locks) |

---

## Core Macros

### Control Flow
| Macro | Status | Notes |
|-------|--------|-------|
| `when` | [x] | |
| `when-not` | [x] | |
| `if-not` | [x] | |
| `if-let` | [x] | |
| `when-let` | [x] | |
| `cond` | [x] | |
| `and` | [x] | Short-circuit |
| `or` | [x] | Short-circuit |
| `if-some` | [x] | Non-nil test |
| `when-some` | [x] | Non-nil when |
| `condp` | [x] | Predicate dispatch |
| `case` | [x] | Emits PHP `match` expression |

### Threading
| Macro | Status | Notes |
|-------|--------|-------|
| `->` | [x] | Thread-first |
| `->>` | [x] | Thread-last |
| `some->` | [x] | Nil-safe thread-first |
| `some->>` | [x] | Nil-safe thread-last |
| `as->` | [x] | Named threading |
| `cond->` | [x] | Conditional threading |
| `cond->>` | [x] | Conditional threading (last) |

### Iteration
| Macro | Status | Notes |
|-------|--------|-------|
| `doseq` | [x] | With destructuring |
| `dotimes` | [x] | Counted iteration |
| `for` | [x] | List comprehension with :let/:when/:while |
| `while` | [x] | Loop while true |
| `comment` | [x] | Evaluates to nil |
| `doto` | [x] | Side-effecting threading |

### Definition
| Macro | Status | Notes |
|-------|--------|-------|
| `defn` | [x] | Multi-arity |
| `defmacro` | [x] | |
| `defn-` | [x] | Private (metadata) |
| `defonce` | [x] | Define if not already defined |
| `declare` | [x] | Forward declaration |
| `defmulti` | [x] | Multimethod definition |
| `defmethod` | [x] | Method implementation |

### Types
| Macro | Status | Notes |
|-------|--------|-------|
| `defprotocol` | [x] | Protocol definition |
| `deftype` | [x] | Custom types |
| `defrecord` | [x] | Record types (map-like) |
| `reify` | [x] | Anonymous protocol impl |
| `extend-type` | [x] | Extend protocol to type |
| `extend-protocol` | [x] | Extend protocol to multiple types |
| `satisfies?` | [x] | Check protocol satisfaction |

---

## Core Functions

### Sequences - Creation
| Function | Status | Notes |
|----------|--------|-------|
| `list` | [x] | |
| `vector` | [x] | |
| `hash-map` | [x] | |
| `hash-set` | [x] | |
| `seq` | [x] | |
| `range` | [x] | |
| `repeat` | [x] | |
| `set` | [x] | |
| `iterate` | [x] | In Core.php as iterate_() |
| `cycle` | [x] | In Core.php as cycle_() |
| `lazy-seq` | [x] | ✅ Thunk-based |

### Sequences - Access
| Function | Status | Notes |
|----------|--------|-------|
| `first` | [x] | |
| `rest` | [x] | |
| `next` | [x] | |
| `second` | [x] | |
| `last` | [x] | |
| `nth` | [x] | |
| `count` | [x] | |
| `get` | [x] | |
| `get-in` | [x] | |
| `keys` | [x] | |
| `vals` | [x] | |
| `contains?` | [x] | |

### Sequences - Modification
| Function | Status | Notes |
|----------|--------|-------|
| `cons` | [x] | |
| `conj` | [x] | |
| `concat` | [x] | |
| `assoc` | [x] | |
| `assoc-in` | [x] | |
| `dissoc` | [x] | |
| `update` | [x] | |
| `update-in` | [x] | |
| `merge` | [x] | |
| `select-keys` | [x] | |
| `into` | [x] | |
| `zipmap` | [x] | |
| `reverse` | [x] | |

### Sequences - Transformation
| Function | Status | Notes |
|----------|--------|-------|
| `map` | [x] | Eager |
| `filter` | [x] | Eager |
| `remove` | [x] | |
| `take` | [x] | |
| `take-while` | [x] | |
| `drop` | [x] | |
| `drop-while` | [x] | |
| `partition` | [x] | |
| `group-by` | [x] | |
| `frequencies` | [x] | |
| `distinct` | [x] | |
| `sort` | [x] | |
| `sort-by` | [x] | |
| `mapcat` | [x] | |
| `map-indexed` | [x] | |
| `keep` | [x] | |
| `keep-indexed` | [x] | |
| `flatten` | [x] | |
| `interleave` | [x] | Multi-arity |
| `interpose` | [x] | |

### Reduction
| Function | Status | Notes |
|----------|--------|-------|
| `reduce` | [x] | |
| `apply` | [x] | |
| `reduced` | [x] | Early termination |
| `reduced?` | [x] | Check if reduced |
| `unreduced` | [x] | Unwrap reduced value |
| `reduce-kv` | [x] | Key-value reduce |
| `transduce` | [x] | Full transducer support in Transducers.php |

### Predicates
| Function | Status | Notes |
|----------|--------|-------|
| `every?` | [x] | |
| `some` | [x] | |
| `not-every?` | [x] | |
| `not-any?` | [x] | |
| `empty?` | [x] | |
| `nil?` | [x] | |
| `some?` | [x] | |
| `true?` | [x] | |
| `false?` | [x] | |
| `boolean?` | [x] | |
| `string?` | [x] | |
| `number?` | [x] | |
| `integer?` | [x] | |
| `float?` | [x] | |
| `fn?` | [x] | |
| `keyword?` | [x] | |
| `symbol?` | [x] | |
| `vector?` | [x] | |
| `map?` | [x] | |
| `set?` | [x] | |
| `list?` | [x] | |
| `coll?` | [x] | |
| `seq?` | [x] | |

### Higher-Order Functions
| Function | Status | Notes |
|----------|--------|-------|
| `identity` | [x] | |
| `constantly` | [x] | |
| `comp` | [x] | |
| `complement` | [x] | |
| `partial` | [x] | |
| `juxt` | [x] | |
| `fnil` | [x] | |
| `memoize` | [x] | Uses vector keys for hashing |
| `trampoline` | [x] | |
| `every-pred` | [x] | |
| `some-fn` | [x] | |
| `doto` | [x] | |

### Arithmetic
| Function | Status | Notes |
|----------|--------|-------|
| `+` | [x] | Variadic, infix |
| `-` | [x] | Variadic, infix |
| `*` | [x] | Variadic, infix |
| `/` | [x] | Variadic, infix |
| `inc` | [x] | |
| `dec` | [x] | |
| `mod` | [x] | Clojure semantics (floor division) |
| `rem` | [x] | PHP % operator |
| `quot` | [x] | Integer division |
| `max` | [x] | Variadic |
| `min` | [x] | Variadic |
| `abs` | [x] | |
| `zero?` | [x] | |
| `pos?` | [x] | |
| `neg?` | [x] | |
| `even?` | [x] | |
| `odd?` | [x] | |

### Comparison
| Function | Status | Notes |
|----------|--------|-------|
| `=` | [x] | Via Runtime::equals |
| `not=` | [x] | |
| `<` | [x] | Infix |
| `>` | [x] | Infix |
| `<=` | [x] | Infix |
| `>=` | [x] | Infix |
| `compare` | [x] | Comparator function |
| `identical?` | [x] | PHP === operator |

### Bitwise
| Function | Status | Notes |
|----------|--------|-------|
| `bit-and` | [x] | Infix |
| `bit-or` | [x] | Infix |
| `bit-xor` | [x] | Infix |
| `bit-shift-left` | [x] | Infix |
| `bit-shift-right` | [x] | Infix |
| `bit-not` | [x] | PHP ~ operator |
| `bit-test` | [x] | Test bit at index |
| `bit-set` | [x] | Set bit at index |
| `bit-clear` | [x] | Clear bit at index |
| `bit-flip` | [x] | Flip bit at index |
| `unsigned-bit-shift-right` | [x] | >>> equivalent |

### Atoms & Concurrency
| Function | Status | Notes |
|----------|--------|-------|
| `atom` | [x] | Swoole-safe with spinlock |
| `deref` / `@` | [x] | Works with Atom, Agent, Promise, Future, Delay |
| `reset!` | [x] | |
| `swap!` | [x] | CAS with retry |
| `compare-and-set!` | [x] | |
| `add-watch` | [x] | |
| `remove-watch` | [x] | |
| `set-validator!` | [x] | |
| `agent` | [x] | Swoole coroutine-based |
| `send` | [x] | Async state update |
| `await` | [x] | Wait for agent queue |
| `promise` | [x] | Single-assignment with Swoole Channel |
| `deliver` | [x] | |
| `realized?` | [x] | |
| `future` | [x] | Swoole coroutine |
| `delay` | [x] | Lazy computation |
| `force` | [x] | |
| `volatile!` | [x] | Fast mutable box (no CAS) |

### STM (Refs)
| Function | Status | Notes |
|----------|--------|-------|
| `ref` | [x] | Create STM ref |
| `deref` | [x] | Read ref value |
| `dosync` | [x] | Transaction block with retry |
| `alter` | [x] | Change ref in transaction |
| `ref-set` | [x] | Set ref in transaction |
| `commute` | [x] | Commutative change (same as alter for now) |
| `ensure` | [x] | Read lock in transaction |

### Parallelism (Swoole)
| Function | Status | Notes |
|----------|--------|-------|
| `pmap` | [x] | Parallel map (Swoole coroutines) |
| `pcalls` | [x] | Parallel function calls |
| `pvalues` | [x] | Parallel value evaluation |
| `pvec` | [x] | Parallel map to vector |

### core.async (Swoole)
| Function | Status | Notes |
|----------|--------|-------|
| `chan` | [x] | Buffered channel |
| `go` | [x] | Go block (Swoole coroutine) |
| `<!` (take) | [x] | Take from channel |
| `>!` (put) | [x] | Put to channel |
| `close!` | [x] | Close channel |
| `alts!` | [x] | Select on channels |
| `timeout` | [x] | Timeout channel |
| `onto-chan` | [x] | Put coll onto channel |
| `to-chan` | [x] | Create channel from coll |
| `pipeline` | [x] | Parallel transducer pipeline |
| `pub` | [x] | Publish/subscribe source |
| `sub` | [x] | Subscribe to topic |

### Strings
| Function | Status | Notes |
|----------|--------|-------|
| `str` | [x] | |
| `print` | [x] | |
| `println` | [x] | |
| `keyword` | [x] | |
| `pr` | [x] | EDN format |
| `prn` | [x] | EDN format + newline |
| `pr-str` | [x] | Returns EDN string |
| `prn-str` | [x] | Returns EDN string + newline |
| `subs` | [x] | Substring |
| `format` | [x] | Via sprintf |
| `name` | [x] | Get name of keyword/symbol |
| `namespace` | [x] | Get namespace of keyword/symbol |
| `symbol` | [x] | Create symbol |

### clojure.string
| Function | Status | Notes |
|----------|--------|-------|
| `split` | [x] | Regex split |
| `join` | [x] | |
| `replace` | [x] | String or regex |
| `replace-first` | [x] | |
| `upper-case` | [x] | MB-safe |
| `lower-case` | [x] | MB-safe |
| `capitalize` | [x] | |
| `trim` | [x] | |
| `triml` | [x] | |
| `trimr` | [x] | |
| `blank?` | [x] | |
| `starts-with?` | [x] | |
| `ends-with?` | [x] | |
| `includes?` | [x] | |
| `reverse` | [x] | MB-safe |

### clojure.set
| Function | Status | Notes |
|----------|--------|-------|
| `union` | [x] | |
| `intersection` | [x] | |
| `difference` | [x] | |
| `subset?` | [x] | |
| `superset?` | [x] | |
| `select` | [x] | |
| `project` | [x] | |
| `rename-keys` | [x] | |
| `index` | [x] | |
| `map-invert` | [x] | |
| `join` | [x] | Relational join |

### clojure.walk
| Function | Status | Notes |
|----------|--------|-------|
| `walk` | [x] | |
| `postwalk` | [x] | |
| `prewalk` | [x] | |
| `postwalk-replace` | [x] | |
| `prewalk-replace` | [x] | |
| `keywordize-keys` | [x] | |
| `stringify-keys` | [x] | |

### Regex
| Function | Status | Notes |
|----------|--------|-------|
| `re-pattern` | [x] | Compile regex |
| `re-matches` | [x] | Full string match |
| `re-find` | [x] | Find first match |
| `re-seq` | [x] | Lazy seq of matches |
| `re-groups` | [x] | Get match groups |

### I/O
| Function | Status | Notes |
|----------|--------|-------|
| `slurp` | [x] | Read file |
| `spit` | [x] | Write file |
| `line-seq` | [x] | Lazy seq of lines |

### Sorted Collections
| Function | Status | Notes |
|----------|--------|-------|
| `sorted-map` | [x] | Red-black tree |
| `sorted-map-by` | [x] | With custom comparator |
| `sorted-set` | [x] | Backed by sorted-map |
| `sorted-set-by` | [x] | With custom comparator |
| `subseq` | [x] | Subsequence from key |
| `rsubseq` | [x] | Reverse subsequence |
| `compare` | [x] | Comparator function |

### Queue
| Function | Status | Notes |
|----------|--------|-------|
| `queue` | [x] | Create PersistentQueue |
| `conj` (queue) | [x] | Add to rear |
| `peek` (queue) | [x] | View front |
| `pop` (queue) | [x] | Remove front |

### Transducers
| Function | Status | Notes |
|----------|--------|-------|
| `transduce` | [x] | Apply transducer with reducing fn |
| `into` (xf) | [x] | Transduce into collection |
| `sequence` (xf) | [x] | Lazy transduced sequence |
| `eduction` | [x] | Reducible/iterable of transformed values |
| `map` (xf) | [x] | `mapping` transducer |
| `filter` (xf) | [x] | `filtering` transducer |
| `remove` (xf) | [x] | `removing` transducer |
| `take` (xf) | [x] | `taking` transducer |
| `drop` (xf) | [x] | `dropping` transducer |
| `take-while` (xf) | [x] | `takingWhile` transducer |
| `drop-while` (xf) | [x] | `droppingWhile` transducer |
| `mapcat` (xf) | [x] | `mapcatting` transducer |
| `distinct` (xf) | [x] | `distincting` transducer |
| `dedupe` (xf) | [x] | `deduping` transducer |
| `partition-all` (xf) | [x] | `partitioningAll` transducer |
| `keep` (xf) | [x] | `keeping` transducer |
| `keep-indexed` (xf) | [x] | `keepingIndexed` transducer |
| `map-indexed` (xf) | [x] | `mappingIndexed` transducer |
| `interpose` (xf) | [x] | `interposing` transducer |
| `replace` (xf) | [x] | `replacing` transducer |
| `cat` | [x] | Concatenating transducer |
| `halt-when` | [x] | Halt on predicate |
| `completing` | [x] | Wrap reducing function |

### Tree/Graph Traversal
| Function | Status | Notes |
|----------|--------|-------|
| `tree-seq` | [x] | Lazy tree traversal |
| `file-seq` | [x] | Lazy file tree (directory listing) |
| `xml-seq` | [x] | XML tree traversal |

### Math (additional)
| Function | Status | Notes |
|----------|--------|-------|
| `gcd` | [x] | Greatest common divisor |
| `lcm` | [x] | Least common multiple |
| `pow` | [x] | |
| `sqrt` | [x] | |
| `floor` | [x] | |
| `ceil` | [x] | |
| `round` | [x] | |
| `rand` | [x] | |
| `rand-int` | [x] | |

---

## Data Structures

| Structure | Status | Notes |
|-----------|--------|-------|
| PersistentVector | [x] | With TransientVector |
| PersistentHashMap | [x] | With TransientMap |
| PersistentHashSet | [x] | |
| PersistentList | [x] | |
| Keyword | [x] | Interned |
| Symbol | [x] | Interned |
| Atom | [x] | Swoole-safe with CAS |
| Agent | [x] | Swoole coroutine-based |
| LazySeq | [x] | ✅ Thunk-based |
| Promise | [x] | Swoole Channel-based |
| Future | [x] | Swoole coroutine |
| Delay | [x] | Lazy computation |
| Volatile | [x] | Fast mutable box |
| PersistentQueue | [x] | FIFO with two-list implementation |
| PersistentTreeMap | [x] | Red-black tree (SortedMap.php) |
| PersistentTreeSet | [x] | Backed by SortedMap |
| Var | [x] | Dynamic binding with push/pop |
| Reduced | [x] | Early termination wrapper |
| Ratio | [ ] | |

---

## PHP Interop

| Feature | Status | Notes |
|---------|--------|-------|
| `php/function` | [x] | Call PHP functions |
| `.method` | [x] | Instance methods |
| `Class/staticMethod` | [x] | Static methods |
| `Class.` / `new` | [x] | Constructors |
| Infix operators | [x] | `php/+`, `php/*`, etc. |
| Named arguments | [ ] | PHP 8.0+ |
| Null-safe `?->` | [ ] | PHP 8.0+ |
| Attributes | [ ] | PHP 8.0+ |

---

## Known Issues

### `:do` in Expression Context

**Severity:** Medium

When `:do` appears in expression position, semicolons break PHP syntax.

```clojure
;; This breaks:
(if condition (do (side-effect) result) other)
```

**Workaround:** Use `let` instead of `do`:
```clojure
(if condition (let [_ (side-effect)] result) other)
```

### ~~Eager Evaluation~~ (FIXED)

~~All sequences are eager. This is a design limitation, not a bug.~~

**UPDATE:** LazySeq is now implemented! Core functions like `map`, `filter`, `take`, `drop` are lazy:

```clojure
;; This now works with infinite sequences:
(defn naturals [n]
  (lazy-seq (cons n (naturals (inc n)))))

(take 5 (map inc (naturals 0)))  ;; => (1 2 3 4 5)
(take 5 (filter even? (naturals 0)))  ;; => (0 2 4 6 8)
```

Use `mapv` and `filterv` for eager versions when needed.

### No Tail Call Optimization

PHP doesn't support TCO. Deep recursion will overflow.

```clojure
;; Works (uses loop/recur):
(loop [n 1000 acc 0]
  (if (zero? n) acc (recur (dec n) (+ acc n))))

;; Fails (no recur):
(defn sum [n acc]
  (if (zero? n) acc (sum (dec n) (+ acc n))))
(sum 10000 0)  ;; Stack overflow
```

**Workaround:** Always use `loop`/`recur` for recursion.

---

## Implementation Priority

### P0 - Critical (Core Language)
1. **LazySeq** - Foundation for lazy evaluation
2. ~~**Multimethods** - `defmulti`/`defmethod`~~ ✅ Done
3. ~~**Protocols** - `defprotocol`/`extend-type`~~ ✅ Done
4. ~~**deftype/defrecord** - User-defined types~~ ✅ Done
5. **Vars & Namespaces** - `ns`, `require`, dynamic binding

### P1 - Important (Completeness)
6. ~~**for** - List comprehension macro~~ ✅ Done
7. ~~**case** - Emit PHP `match`~~ ✅ Done
8. ~~**letfn** - Mutually recursive local fns~~ ✅ Done
9. **Reader macros** - `#()`, `@`, `#'`
10. ~~**Sorted collections** - TreeMap/TreeSet~~ ✅ Done (SortedMap.php, SortedSet.php)

### P2 - Concurrency (Swoole Required) ✅ DONE
11. ~~**Futures**~~ ✅ Done - `future`, `promise`, `deliver`, `realized?`
12. ~~**Agents**~~ ✅ Done - `agent`, `send`, `await`
13. ~~**core.async**~~ ✅ Done - Channels, go blocks (Async.php)
14. ~~**STM Refs**~~ ✅ Done - `ref`, `dosync`, `alter`, `commute` (Ref.php)
15. ~~**Parallel**~~ ✅ Done - `pmap`, `pcalls`, `pvalues` (Ref.php)

### P3 - Standard Library ✅ MOSTLY DONE
16. ~~**clojure.string**~~ ✅ Done - split, join, replace, trim, case functions
17. ~~**clojure.set**~~ ✅ Done - union, intersection, difference, subset?, etc.
18. ~~**clojure.walk**~~ ✅ Done - postwalk, prewalk, keywordize-keys, etc.
19. ~~**Transducers**~~ ✅ Done - transduce, eduction, all xforms (Transducers.php)
20. **clojure.edn** - EDN reader exists in Reader.php

---

## Quick Wins (Effort: Low)

These can be implemented in 1 day or less:

| Feature | Approach |
|---------|----------|
| ~~`case`~~ | ✅ Done - Emits PHP `match` expression |
| ~~`cond->` / `cond->>`~~ | ✅ Done - Conditional threading macros |
| ~~`some->` / `some->>`~~ | ✅ Already done |
| ~~`letfn`~~ | ✅ Done - Emits fns before use |
| ~~`as->`~~ | ✅ Done - Named threading macro |
| ~~`comment`~~ | ✅ Done - Expands to `nil` |
| ~~`doto`~~ | ✅ Done - Side-effecting threading |
| ~~`while`~~ | ✅ Done - Loop while condition true |
| ~~`if-some` / `when-some`~~ | ✅ Done - Non-nil tests |
| ~~`condp`~~ | ✅ Done - Predicate dispatch |

## Medium Effort (1-3 days)

| Feature | Approach |
|---------|----------|
| ~~`for`~~ | ✅ Done - Expands to nested `mapcat` |
| ~~`mapcat`~~ | ✅ Done - `(apply concat (map f coll))` |
| ~~`reduce-kv`~~ | ✅ Done - Runtime function |
| ~~`reduced`~~ | ✅ Done - Wrapper object + check in reduce |
| ~~`memoize`~~ | ✅ Done - Uses vector keys for hashing |
| ~~`compare`~~ | ✅ Done - Comparator function |
| ~~`identical?`~~ | ✅ Done - PHP === operator |
| ~~`name` / `namespace`~~ | ✅ Done - Keyword/symbol utilities |
| ~~`subs`~~ | ✅ Done - Substring function |

## Major Effort (1+ weeks)

| Feature | Notes |
|---------|-------|
| ~~LazySeq~~ | ✅ Done - Thunk-based lazy sequences |
| ~~Protocols~~ | ✅ Done - Runtime dispatch |
| ~~Multimethods~~ | ✅ Done - Runtime dispatch |
| Namespaces | Var registry + require/use |
