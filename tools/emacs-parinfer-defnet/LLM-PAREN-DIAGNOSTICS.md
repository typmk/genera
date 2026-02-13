# Parenthesis Diagnostics Optimized for LLM Token Efficiency

## The Real Use Case: Quick Diagnostics for LLMs

**Goal:** Help LLMs (like me) quickly identify and fix paren issues with minimal token usage.

**Key Requirements:**
1. ✅ Precise error location (line + column)
2. ✅ Minimal context (not entire file)
3. ✅ Structured output (easy to parse)
4. ✅ Multiple diagnostic methods (cross-validation)
5. ✅ Fast execution (no interactive tools needed)

---

## The Problem

### When I Generate Bad Code:

```elisp
;; I might generate:
(defun example (x)
  (if (> x 0)
      (message "positive")
  ;; Oops! Missing closing paren
```

### Without Good Diagnostics:

❌ **User shows me entire file** (wastes tokens)
❌ **I have to read everything** to find the issue
❌ **Slow back-and-forth** "Is it line 5? Line 10?"

### With Good Diagnostics:

✅ **Precise location:** "Line 4, column 0"
✅ **Minimal context:** Just the problematic line
✅ **Clear error:** "Unmatched bracket or quote"
✅ **Quick fix:** I immediately know what to fix

---

## Solution: Diagnostic Helper Functions

I've created **`claude-paren-diagnostics.el`** with functions optimized for LLM consumption.

### Installation

```elisp
;; Add to your init.el
(load-file (expand-file-name "~/em/claude-paren-diagnostics.el"))
```

Now these functions are available for me to call via emacsclient!

---

## Available Diagnostic Functions

### 1. `claude/minimal-diagnostic` ⭐ (BEST for tokens)

**Purpose:** Ultra-compact error report

**Returns:**
```elisp
;; If OK:
(:ok t)

;; If error:
(:ok nil
 :line 4
 :col 0
 :msg "Unmatched bracket or quote"
 :text "(defun test-missing-close (x)")
```

**Token cost:** ~20-30 tokens for error report

**When to use:** Always try this first!

**Example usage by me:**
```bash
emacsclient -e '(claude/minimal-diagnostic "problem-file.el")'
# => (:ok nil :line 42 :col 5 :msg "..." :text "...")
# I immediately know: Line 42, column 5, specific problem!
```

---

### 2. `claude/diagnose-parens` (Detailed)

**Purpose:** Comprehensive error info with context

**Returns:**
```elisp
(:status "error"
 :error-type scan-error
 :message "Unmatched bracket or quote"
 :buffer "test.el"
 :position 123
 :line 4
 :column 0
 :line-text "(defun test-missing-close (x)"
 :context "...100 chars before and after..."
 :char-at-error "(")
```

**Token cost:** ~50-100 tokens

**When to use:** When minimal diagnostic isn't enough

---

### 3. `claude/quick-paren-summary` (Overview)

**Purpose:** Quick sanity check - are parens balanced overall?

**Returns:**
```elisp
(:open-count 15
 :close-count 13
 :balance 2              ; 2 more opens than closes
 :depth-at-end 2         ; Ended 2 levels deep
 :balanced nil
 :likely-issue "missing-closers")
```

**Token cost:** ~15-20 tokens

**When to use:** First pass - "Is there even a problem?"

---

### 4. `claude/find-unmatched-opener` (Specific)

**Purpose:** Find ALL unmatched opening parens/brackets/braces

**Returns:**
```elisp
(:status "error"
 :count 2
 :unmatched ((:position 123
              :line 4
              :column 15
              :char "("
              :context "  (if (> x 0)")
             (:position 456
              :line 12
              :column 8
              :char "["
              :context "  (let [[result")))
```

**Token cost:** ~30-40 tokens per unmatched item

**When to use:** Multiple errors suspected

---

### 5. `claude/validate-by-indentation` (Heuristic)

**Purpose:** Use indentation to guess where parens are wrong

**Returns:**
```elisp
(:status "warning"
 :count 2
 :suspicious ((:line 8
               :indent 12
               :jump 8
               :text "        (very-indented-code)"
               :warning "Large indentation jump - possible missing paren above")
              (:line 15
               :indent 0
               :prev-indent 8
               :text "(defun another"
               :warning "Sudden return to column 0 - possible unclosed form above")))
```

**Token cost:** ~40-60 tokens

**When to use:** When syntax check isn't finding the issue, but something "looks wrong"

---

### 6. `claude/show-paren-depth` (Visual)

**Purpose:** Show nesting depth at each line

**Returns:**
```elisp
(:status "ok"
 :lines 50
 :depth-changes ((:line 1 :depth 0 :text "(defun foo ()")
                 (:line 2 :depth 1 :text "  (let ((x 1))")
                 (:line 3 :depth 2 :text "    (if x")
                 (:line 5 :depth 1 :text "  (message")))
```

**Token cost:** ~10-15 tokens per depth change

**When to use:** Visualizing structure to understand nesting

---

### 7. `claude/all-diagnostics` (Complete)

**Purpose:** Run ALL diagnostics at once

**Returns:** Combined results from all methods

**Token cost:** ~200-300 tokens

**When to use:** Complex issues, need full picture

---

## Optimal Workflow for LLM Diagnostics

### Scenario 1: User Reports "Paren error"

**Step 1:** Quick check
```bash
emacsclient -e '(claude/quick-paren-summary "file.el")'
# => (:balance 2 :likely-issue "missing-closers")
# Aha! Missing 2 closing parens
```

**Step 2:** Find location
```bash
emacsclient -e '(claude/minimal-diagnostic "file.el")'
# => (:ok nil :line 42 :col 5 :msg "Unmatched" :text "...")
# Found it! Line 42, column 5
```

**Step 3:** Fix and verify
```bash
# I suggest fix
# User applies
emacsclient -e '(claude/minimal-diagnostic "file.el")'
# => (:ok t)
# Success!
```

**Total tokens:** ~50-60 tokens
**Time:** Seconds

---

### Scenario 2: I Generated Bad Code

**What happens:**
1. I generate code with paren error
2. User pastes into Emacs
3. They get error message

**Old workflow:**
```
User: "Your code has a paren error"
Me: "Can you show me the file?"
User: *pastes 500 lines*
Me: *reads entire file, uses 5000 tokens*
Me: "Oh, line 42 is missing a paren"
```

**New workflow:**
```
User: "Your code has a paren error"
Me: "Let me check..."
# I run: emacsclient -e '(claude/minimal-diagnostic buffer)'
# Result: (:ok nil :line 42 :col 5 :msg "..." :text "...")
Me: "Found it! Line 42, column 5. Here's the fix:"
```

**Token savings:** 90%+ reduction!

---

### Scenario 3: Complex Multi-Error File

**Step 1:** Overview
```bash
emacsclient -e '(claude/quick-paren-summary "complex.el")'
# => (:balance 3 :likely-issue "missing-closers")
# OK, missing 3 closers
```

**Step 2:** Find all unmatched
```bash
emacsclient -e '(claude/find-unmatched-opener "complex.el")'
# => (:count 3 :unmatched ((:line 10 ...) (:line 25 ...) (:line 40 ...)))
# All 3 locations identified!
```

**Step 3:** Cross-validate with indentation
```bash
emacsclient -e '(claude/validate-by-indentation "complex.el")'
# => (:suspicious ((:line 11 :warning "Large jump") ...))
# Confirms line 10 issue
```

**Total tokens:** ~150-200 tokens
**Confidence:** High (multiple methods agree)

---

## Integration with Current Helpers

### Add to `claude-helpers.el`:

```elisp
;; Load paren diagnostics
(require 'claude-paren-diagnostics)

;; Convenience wrapper
(defun claude/check-file (filename)
  "Quick health check for FILENAME.
Returns minimal diagnostic + summary."
  (list :minimal (claude/minimal-diagnostic filename)
        :summary (claude/quick-paren-summary filename)))
```

### Now I can call:

```bash
emacsclient -e '(claude/check-file "problem.el")'
# => (:minimal (:ok nil :line 42 ...) :summary (:balance 2 ...))
```

**Result:** Complete picture in one call, ~60 tokens

---

## Comparison: Token Efficiency

### Traditional Approach:

**User sends entire file:**
```
(defun foo ()
  (let ((x 1)
        (y 2))
    (if (> x y)
        (message "x is bigger")
      (message "y is bigger"))))

(defun bar ()
  (let ((a 1)
    (message "done"))  ; <-- Missing paren here

(defun baz ()
  (some-code))
```

**Token cost:** ~500 tokens for file
**I have to:** Read entire thing, find issue myself

### Diagnostic Approach:

**User runs:**
```bash
emacsclient -e '(claude/minimal-diagnostic buffer)'
```

**I receive:**
```elisp
(:ok nil :line 12 :col 4 :msg "Unmatched bracket" :text "    (message \"done\"))")
```

**Token cost:** ~30 tokens
**I immediately know:** Line 12, column 4, missing paren

**Savings:** 94% fewer tokens! ⚡

---

## Why This Matters for LLM Workflows

### Problem: Context Window Limits

- LLMs have token limits (100k-200k)
- Large files consume context quickly
- Less room for actual conversation

### Solution: Precise Diagnostics

- Only send error info, not whole file
- I can fix issues with minimal context
- More tokens available for complex tasks

### Example:

**File size:** 1000 lines = ~5000 tokens
**Diagnostic:** ~30 tokens

**Result:** 166x more efficient! 🚀

---

## Comparison with Other Tools

### Flycheck/Flymake

**What they do:**
- Real-time linting in Emacs GUI
- Visual indicators in buffer
- Great for interactive editing

**For LLM use:**
- ❌ Designed for human interaction
- ❌ Visual output (I can't see it)
- ❌ Not programmatic
- ⚠️ Can query results, but verbose

**Verdict:** Good for you, not optimal for me

### check-parens (Built-in)

**What it does:**
```elisp
(check-parens)
# => Error: "Unmatched bracket or quote"
# Leaves point at error location
```

**For LLM use:**
- ✅ Fast
- ✅ Accurate
- ❌ Minimal info (just message + position)
- ❌ Requires interpretation

**Verdict:** Good foundation, but my wrappers add crucial context

### My Diagnostic Functions

**What they do:**
- Structured output (plists)
- Multiple analysis methods
- Optimized for parsing
- Minimal tokens

**For LLM use:**
- ✅ Perfect for programmatic access
- ✅ Easy to parse
- ✅ Minimal token usage
- ✅ Comprehensive coverage

**Verdict:** Purpose-built for LLM workflows ⭐

---

## Setup Instructions

### 1. Install Diagnostic Functions

```elisp
;; Add to your init.el
(let ((diagnostics (expand-file-name "~/em/claude-paren-diagnostics.el")))
  (when (file-exists-p diagnostics)
    (load-file diagnostics)
    (message "Claude paren diagnostics loaded")))
```

### 2. Restart Emacs (or reload)

```bash
# Or without restart:
emacsclient -e '(load-file "~/em/claude-paren-diagnostics.el")'
```

### 3. Test It

```bash
# Test on known-bad file:
emacsclient -e '(claude/minimal-diagnostic "~/em/test-paren-diagnostics.el")'

# Should return error info
```

### 4. Use in Workflow

**When you ask me to debug parens:**
1. I request diagnostic: `(claude/minimal-diagnostic buffer-name)`
2. You run it (or it runs automatically)
3. I get structured error info
4. I provide precise fix
5. Verify: `(claude/minimal-diagnostic buffer-name)` => `(:ok t)`

---

## Real-World Examples

### Example 1: Missing Closer

**Code:**
```elisp
(defun example (x)
  (if (> x 0)
      (message "positive")
  (message "done"))
```

**Diagnostic:**
```bash
emacsclient -e '(claude/minimal-diagnostic buffer)'
# => (:ok nil :line 4 :col 2 :msg "Unmatched" :text "  (message \"done\"))")
```

**Fix:**
```elisp
(defun example (x)
  (if (> x 0)
      (message "positive"))  ; <-- Add paren here
  (message "done"))
```

**Tokens used:** ~30 (vs ~200 for full file)

---

### Example 2: Mismatched Brackets

**Code:**
```elisp
(defun test ()
  (let ((data [1 2 3}))  ; <-- [ closed with }
    data))
```

**Diagnostic:**
```bash
emacsclient -e '(claude/minimal-diagnostic buffer)'
# => (:ok nil :line 2 :col 17 :msg "Unmatched" :text "  (let ((data [1 2 3})")
```

**Fix:** Change `}` to `]` at line 2, column 17

**Tokens used:** ~35

---

### Example 3: Multiple Errors

**Code:**
```elisp
(defun multi-error (x y)
  (let ((a (+ x y))   ; <-- Missing closing paren
    (if (> a 0)
        (message "positive")
  (message "done"))
```

**Diagnostic:**
```bash
# First error:
emacsclient -e '(claude/minimal-diagnostic buffer)'
# => (:ok nil :line 2 :col 2 ...)

# All errors:
emacsclient -e '(claude/find-unmatched-opener buffer)'
# => (:count 2 :unmatched ((:line 2 ...) (:line 3 ...)))
```

**Fix:** Add `)` at end of line 2, add `)` at end of line 4

**Tokens used:** ~80 (vs ~300 for full file)

---

## Advanced: Combining with Other Diagnostics

### With Flycheck (if installed):

```elisp
(defun claude/flycheck-summary ()
  "Get Flycheck errors in LLM-friendly format."
  (when (bound-and-true-p flycheck-mode)
    (let ((errors (flycheck-overlay-errors-in (point-min) (point-max))))
      (mapcar (lambda (err)
                (list :line (flycheck-error-line err)
                      :column (flycheck-error-column err)
                      :message (flycheck-error-message err)
                      :level (flycheck-error-level err)))
              errors))))
```

**Result:** Get both paren errors AND other issues (unused variables, etc.)

---

## Summary

### Key Functions (Priority Order)

1. **`claude/minimal-diagnostic`** - Always try first (20-30 tokens)
2. **`claude/quick-paren-summary`** - Quick health check (15-20 tokens)
3. **`claude/find-unmatched-opener`** - Multiple errors (30-40 tokens each)
4. **`claude/validate-by-indentation`** - Heuristic hints (40-60 tokens)
5. **`claude/diagnose-parens`** - Full details when needed (50-100 tokens)

### Token Efficiency

| Method | Tokens | Use Case |
|--------|--------|----------|
| Send entire file | 5000+ | ❌ Wasteful |
| Minimal diagnostic | 30 | ✅ Perfect |
| Full diagnostic | 100 | ✅ When needed |
| All diagnostics | 300 | ✅ Complex cases |

**Average savings:** 95%+ reduction in tokens! 🎉

### For You

- Run diagnostics when asking me to debug
- Paste just the diagnostic output (not whole file)
- I can fix issues precisely and quickly

### For Me

- Request specific diagnostic functions
- Parse structured output
- Provide targeted fixes
- Verify with follow-up diagnostic

---

## Next Steps

**Want me to integrate this into your workflow?**

I can:
1. Add these to your init.el
2. Create keyboard shortcuts for quick diagnostics
3. Set up automatic checking on save
4. Integrate with your existing helpers

Just say the word! 🚀
