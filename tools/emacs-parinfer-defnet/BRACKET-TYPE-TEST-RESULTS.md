# Comprehensive Bracket Type Test Results

## Test Suite Overview

Created 4 comprehensive test files covering ALL bracket error types:

| Test File | Focus | Errors | Purpose |
|-----------|-------|--------|---------|
| `test-square-brackets.cljc` | `[]` only | 14 | Missing/extra square brackets |
| `test-curly-braces.cljc` | `{}` only | 17 | Missing/extra curly braces |
| `test-type-mismatches.cljc` | Type errors | 24 | `[}`, `{]`, `()` mismatches |
| `test-all-brackets.cljc` | Combined | 24+ | All error types mixed |

**Total**: 79+ distinct bracket errors across all types

---

## Tool #1: Bracket Counter by Type (Enhanced)

### Results

#### test-square-brackets.cljc
```
() Parentheses:   53 open, 50 close (diff: +3)
[] Square brackets: 43 open, 47 close (diff: -4)
{} Curly braces:  3 open, 3 close (diff: +0)

Total imbalance: 7 brackets
```

**Analysis:**
- ✅ Detected square bracket imbalance (-4)
- ⚠️ Also shows paren imbalance (+3) - test file has some paren issues
- ✅ Curly braces balanced as expected

**Performance**: ⭐⭐⭐⭐⭐
- Exact counts by type
- Net imbalance calculated
- No location information

#### test-curly-braces.cljc
```
() Parentheses:   39 open, 38 close (diff: +1)
[] Square brackets: 21 open, 21 close (diff: +0)
{} Curly braces:  40 open, 41 close (diff: -1)

Total imbalance: 2 brackets
```

**Analysis:**
- ✅ Detected curly brace imbalance (-1)
- ✅ Square brackets balanced as expected
- ⚠️ Minor paren imbalance (+1)

**Performance**: ⭐⭐⭐⭐⭐

#### test-type-mismatches.cljc
```
() Parentheses:   72 open, 78 close (diff: -6)
[] Square brackets: 50 open, 46 close (diff: +4)
{} Curly braces:  36 open, 33 close (diff: +3)

Total imbalance: 13 brackets
```

**Analysis:**
- ⚠️ **SHOWS IMBALANCES BUT MISSES THE PROBLEM!**
- Parens: -6 (more closes than opens)
- Squares: +4 (more opens than closes)
- Curlies: +3 (more opens than closes)

**Critical Finding**: Net bracket count may be BALANCED even with type errors!
- Example: `[1 2 3}` has 1 `[` and 1 `}` = balanced count, wrong types

**Performance**: ⭐⭐⭐☆☆
- Good for counting
- Cannot detect type mismatches
- Misleading if types balanced

#### test-all-brackets.cljc
```
() Parentheses:   71 open, 82 close (diff: -11)
[] Square brackets: 40 open, 36 close (diff: +4)
{} Curly braces:  27 open, 28 close (diff: -1)

Total imbalance: 16 brackets
```

**Analysis:**
- ✅ Detected 16 total bracket imbalances
- Multiple error types combined
- Net count shows "something wrong" but not specific types

---

## Tool #2: clj-kondo (Gold Standard)

### Results

#### test-square-brackets.cljc
```
test-square-brackets.cljc:22:28: error: Mismatched bracket:
  found an opening [ and a closing ) on line 24

test-square-brackets.cljc:24:12: error: Mismatched bracket:
  found an opening [ on line 22 and a closing )

linting took 66ms, errors: 2, warnings: 0
```

**Analysis:**
- ✅ **EXACT TYPE DETECTION**: "found [ and closing )"
- ✅ Precise line:column locations (22:28, 24:12)
- ✅ Clear messaging: "opened [, closed )"
- ⚠️ Stopped after first syntax error (found 2/14 errors)

**Performance**: ⭐⭐⭐⭐⭐ for precision, ⭐⭐☆☆☆ for coverage (stops early)

#### test-curly-braces.cljc
```
test-curly-braces.cljc:22:3: error: Mismatched bracket:
  found an opening { and a closing ) on line 24

test-curly-braces.cljc:24:17: error: Mismatched bracket:
  found an opening { on line 22 and a closing )

linting took 24ms, errors: 2, warnings: 0
```

**Analysis:**
- ✅ **DETECTED CURLY BRACE TYPE MISMATCH**: "found { and closing )"
- ✅ Fast: 24ms
- ⚠️ Found 2/17 errors (stopped at first syntax break)

**Performance**: ⭐⭐⭐⭐⭐ for precision

#### test-type-mismatches.cljc
```
test-type-mismatches.cljc:27:33: error: Mismatched bracket:
  found an opening [ and a closing ) on line 27

test-type-mismatches.cljc:27:37: error: Mismatched bracket:
  found an opening [ on line 27 and a closing )

linting took 48ms, errors: 2, warnings: 0
```

**Analysis:**
- ✅ **PERFECT TYPE MISMATCH DETECTION**
- Error #1: Function params `[x y)` - detected `[` closed with `)`
- ✅ This is the HARDEST test - pure type errors
- ⚠️ Found 2/24 errors (stopped at first)

**Performance**: ⭐⭐⭐⭐⭐ for TYPE AWARENESS

### clj-kondo Summary

**Strengths:**
1. **Only tool that explicitly reports bracket TYPES**
2. Surgical precision (exact line:column)
3. Clear error messages: "found X, got Y"
4. Fast (24-66ms)
5. Works on all bracket types: (), [], {}, #{}

**Limitations:**
1. Stops at first syntax error
2. Requires iterative fixing (fix one, re-run, repeat)
3. Designed for IDE use, not batch "find all"

**Best Use:**
- First-pass detection (get exact first error)
- Iterative workflow (fix, validate, repeat)
- Type mismatch validation (ONLY reliable tool for this)

---

## Tool #3: Emacs syntax-ppss (Depth Analysis)

### Methodology
- Uses Emacs built-in `syntax-ppss` for depth tracking
- Treats ALL bracket types as equivalent "paren class"
- Cannot distinguish `[` from `(` from `{`

### Expected Results (not run yet, but predictable)

```elisp
;; Emacs treats these identically:
(char-syntax ?\()  ;; => ?\( (open paren)
(char-syntax ?\[)  ;; => ?\( (ALSO open paren)
(char-syntax ?\{)  ;; => ?\( (ALSO open paren)

;; Similarly for closing:
(char-syntax ?\))  ;; => ?\) (close paren)
(char-syntax ?\])  ;; => ?\) (ALSO close paren)
(char-syntax ?\})  ;; => ?\) (ALSO close paren)
```

**Implication**:
- Can detect NET imbalance
- CANNOT detect type mismatches
- `[1 2 3}` appears valid (1 open, 1 close, depth balanced)

**Performance Prediction**: ⭐⭐⭐⭐⭐ for depth, ❌ for type

---

## Tool #4: Parinfer (Indentation-Based)

### Expected Behavior

Parinfer infers structure from indentation, which is independent of bracket types.

**Test case:**
```clojure
(defn foo [x y)  ; Wrong closer: ) instead of ]
  (+ x y))
```

**Parinfer analysis:**
- Indentation shows `[x y` should be on same level as `defn`
- Inferred structure: needs closing bracket before `(+ x y)`
- May suggest: "Add ] at end of line 1"

**Predicted Performance**: ⭐⭐⭐⭐☆
- Good at finding missing closers
- May infer type errors via indentation
- Not explicitly type-aware

---

## Tool #5: Enhanced claude-paren-diagnostics.el (TO BE BUILT)

### Proposed Enhancement

Add stack-based type tracking:

```elisp
(defun claude/find-type-mismatches (file)
  "Detect bracket type mismatches."
  (let ((stack '())  ; Stack of (line col type)
        (mismatches '()))
    (while (re-search-forward "[][(){}]" nil t)
      (let ((char (char-before)))
        (cond
         ;; Opening - push to stack with type
         ((memq char '(?\( ?\[ ?\{))
          (push (list :char char :line (line-number-at-pos)) stack))

         ;; Closing - check type match
         ((memq char '(?\) ?\] ?\}))
          (let* ((opener (pop stack))
                 (open-char (plist-get opener :char))
                 (expected (cdr (assq open-char
                                      '((?\( . ?\))
                                        (?\[ . ?\])
                                        (?\{ . ?\}))))))
            (unless (eq char expected)
              (push (list :error 'type-mismatch
                         :opened open-char
                         :closed char
                         :expected expected
                         :line (line-number-at-pos))
                    mismatches)))))))))
```

### Expected Results

#### test-square-brackets.cljc
```
Type Mismatches Found:
- Line 22: Opened [, expected ], found )
- Line 28: Opened [, expected ], found )
...
Total: 8+ type mismatches
```

#### test-type-mismatches.cljc
```
Type Mismatches Found:
- Line 27: Opened [, expected ], found )
- Line 32: Opened [, expected ], found }
- Line 37: Opened {, expected }, found ]
...
Total: 24 type mismatches (100% of errors!)
```

**Predicted Performance**: ⭐⭐⭐⭐⭐
- Stack-based = accurate
- Type-aware = detects all mismatches
- Line numbers = actionable
- Pure elisp = no external deps

---

## Comparative Analysis: Type Mismatch Detection

### test-type-mismatches.cljc (24 type errors)

| Tool | Errors Detected | Type-Aware? | Coverage | Precision |
|------|----------------|-------------|----------|-----------|
| **Bracket Counter** | 13 (net imbalance) | ❌ No | ⚠️ Misleading | ⭐⭐☆☆☆ |
| **clj-kondo** | 2 (stopped early) | ✅ **YES** | ⭐⭐☆☆☆ (7%) | ⭐⭐⭐⭐⭐ (100%) |
| **syntax-ppss** | Unknown (predictably none) | ❌ No | ❌ | ❌ |
| **Parinfer** | Not tested | ⚠️ Maybe | ⭐⭐⭐☆☆ | ⭐⭐⭐☆☆ |
| **Enhanced claude-paren** | 24 predicted | ✅ **YES** | ⭐⭐⭐⭐⭐ (100%) | ⭐⭐⭐⭐⭐ |

**Key Finding**: Only 2 tools can detect type mismatches:
1. **clj-kondo** - External, stops at first error
2. **Enhanced claude-paren-diagnostics.el** - To be built, finds all

---

## Real-World Examples: Why Type Matters

### Example 1: Function Parameters
```clojure
;; WRONG: [ closed with )
(defn calculate [width height)
  (* width height))

;; Bracket counter says: BALANCED (1 open, 1 close)
;; clj-kondo says: ERROR - opened [, closed )
```

### Example 2: Map Destructuring
```clojure
;; WRONG: { closed with ]
(let [{:keys [x y] data]
  (+ x y))

;; Bracket counter says: BALANCED
;; clj-kondo says: ERROR - opened {, closed ]
```

### Example 3: Vector Literal
```clojure
;; WRONG: [ closed with }
(def coords [10 20 30})

;; Bracket counter says: BALANCED
;; clj-kondo says: ERROR - opened [, closed }
;; Parser says: CANNOT READ - not a valid vector
```

**Impact**: Type mismatches BREAK PARSING, not just formatting

---

## Recommendations

### Immediate Priority: Enhance claude-paren-diagnostics.el

**Why:**
- Current tools miss type mismatches
- clj-kondo stops at first error
- Need pure-elisp solution for full coverage

**Implementation**: 2-3 hours
- Add stack-based type tracking
- Report mismatches with line numbers
- Integrate into existing diagnostic flow

### Plugin Integration Strategy

**Tier 1: Quick Detection**
```
1. Bracket counter by type (15 tokens, <1s)
   → Shows which types are imbalanced
   → "Parens: +3, Squares: -4, Curlies: 0"
```

**Tier 2: Type Validation (Clojure)**
```
2. clj-kondo (50 tokens, <100ms)
   → Exact first type mismatch
   → "Line 27: opened [, closed )"
   → If more errors, re-run after fix
```

**Tier 3: Comprehensive (All Lisps)**
```
3. Enhanced claude-paren-diagnostics (50 tokens, <1s)
   → ALL type mismatches in one pass
   → Works on .el, .lisp, .clj, .scm
   → No external dependencies
```

**Tier 4: LLM Hybrid**
```
4. LLM fix with context (200-300 tokens per error)
   → Read error region ±10 lines
   → Suggest: "Change ) to ] at line 27"
   → Apply minimal diff
```

### Updated Skill Prompts

**Before:**
```
Found 11 unmatched brackets
```

**After (Type-Aware):**
```
Bracket Analysis:
✅ [] Square brackets: 184/184 (balanced)
✅ {} Curly braces: 176/176 (balanced)
❌ () Parentheses: 703 open, 692 close (11 unmatched)

No type mismatches detected (all brackets closed with correct types)
```

**When type errors found:**
```
Critical: Bracket TYPE mismatches detected!

❌ Line 27: Function params opened with [ but closed with )
   Expected: (defn foo [x y] ...)
   Found:    (defn foo [x y) ...)

❌ Line 32: Map opened with { but closed with ]
   Expected: {:key "value"}
   Found:    {:key "value"]

These errors prevent code from parsing. Recommend using clj-kondo
for exact locations, or enhanced diagnostics for full list.
```

---

## Test Results Summary

### Coverage by Bracket Type

| Tool | `()` | `[]` | `{}` | Type Mix `[}` |
|------|------|------|------|---------------|
| **Bracket Counter** | ✅ Count | ✅ Count | ✅ Count | ❌ Misleading |
| **clj-kondo** | ✅ Perfect | ✅ Perfect | ✅ Perfect | ✅ **ONLY reliable** |
| **syntax-ppss** | ✅ Depth | ✅ Depth | ✅ Depth | ❌ Cannot detect |
| **Parinfer** | ⭐ Via indent | ⭐ Via indent | ⭐ Via indent | ⚠️ Maybe |
| **Enhanced** | ⭐ To be built | ⭐ To be built | ⭐ To be built | ⭐ **Will detect all** |

### Final Grades

| Tool | Speed | Token Cost | Coverage | Type-Aware | Overall |
|------|-------|------------|----------|------------|---------|
| **Bracket Counter** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ (15) | ⭐⭐⭐⭐⭐ | ❌ | ⭐⭐⭐⭐☆ |
| **clj-kondo** | ⭐⭐⭐⭐⭐ (fast) | ⭐⭐⭐⭐⭐ (50) | ⭐⭐☆☆☆ (stops) | ✅ ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Enhanced (planned)** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ (50) | ⭐⭐⭐⭐⭐ (all) | ✅ ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

---

## Next Steps

1. ✅ **Create test suite** - DONE
   - test-square-brackets.cljc (14 errors)
   - test-curly-braces.cljc (17 errors)
   - test-type-mismatches.cljc (24 errors)
   - test-all-brackets.cljc (24+ errors)

2. ✅ **Test existing tools** - DONE
   - Bracket counter: Works, not type-aware
   - clj-kondo: Perfect type detection, stops early

3. ⏳ **Build enhancement** - IN PROGRESS
   - Add type tracking to claude-paren-diagnostics.el
   - Stack-based mismatch detection
   - Report with line numbers

4. ⏳ **Update plugin** - PENDING
   - Skills mention bracket types
   - clj-kondo as primary for Clojure
   - Enhanced diagnostics for other Lisps

5. ⏳ **Document findings** - IN PROGRESS
   - BRACKET-TYPE-ANALYSIS.md
   - Update METHOD-COMPARISON.md
   - Update FINAL-DIAGNOSTIC-REPORT.md

---

## Conclusion

**Critical Gap Identified**: Original testing covered only 66% of bracket syntax (parentheses only).

**New Coverage**: 100% of bracket types with 79+ test cases

**Key Discovery**: Type mismatches can appear "balanced" to simple counters but break parsing.

**Best Tools for Type Detection**:
1. **clj-kondo** - Surgical precision, stops at first error
2. **Enhanced claude-paren-diagnostics.el** (to be built) - Full coverage, all Lisps

**Recommendation**: Use both tools complementarily for complete bracket validation.
