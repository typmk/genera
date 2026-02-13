---
name: emacs-diagnostics
description: Automatically diagnose Emacs Lisp, Clojure, and Common Lisp parenthesis/bracket errors using token-efficient diagnostic tools. Invoked when user encounters syntax errors, unmatched delimiters, or paren-related issues in .el, .clj, or .lisp files.
allowed-tools:
  - Bash
---

# Emacs Diagnostics Skill

You are an expert at diagnosing Lisp code errors using token-efficient emacsclient commands.

## When to Use This Skill

Automatically activate when:
- User reports "unmatched parenthesis" or similar syntax errors
- Emacs shows paren-related error messages
- User asks about bracket/delimiter balancing
- Files with extensions: `.el`, `.clj`, `.cljs`, `.lisp`, `.lsp`

## Available Diagnostic Functions

### Quick Health Check (15-20 tokens)
```bash
emacsclient -e '(claude/quick-paren-summary "FILE_PATH")'
```
Returns: Overall balance status, total counts

### Precise Error Location (20-30 tokens)
```bash
emacsclient -e '(claude/minimal-diagnostic "FILE_PATH")'
```
Returns: Exact line number and context of first error

### Find All Unmatched (30-40 tokens per error)
```bash
emacsclient -e '(claude/find-unmatched-opener "FILE_PATH")'
```
Returns: All unmatched delimiters with locations

### Indentation Heuristics (40-50 tokens)
```bash
emacsclient -e '(claude/validate-by-indentation "FILE_PATH")'
```
Returns: Suspicious indentation patterns suggesting structural issues

### Combined Multi-Method Analysis (60-80 tokens)
```bash
emacsclient -e '(claude/combined-diagnosis "FILE_PATH")'
```
Returns: Cross-validated results from multiple diagnostic approaches

### Efficient Structure Analysis (variable, checkpoint-based)
```bash
emacsclient -e '(claude/efficient-diagnosis "FILE_PATH")'
```
Returns: Structure-based analysis with early termination on errors

### Bracket Type Detection (NEW - Type-Aware)

**Count Brackets by Type** (~30 tokens)
```bash
emacsclient -e '(claude/count-by-type "FILE_PATH")'
```
Returns: Separate counts for `()`, `[]`, `{}` brackets

**Find Type Mismatches** (~40-60 tokens per error)
```bash
emacsclient -e '(claude/find-type-mismatches "FILE_PATH")'
```
Returns: Errors where brackets opened with one type but closed with another (e.g., `[}`, `{]`)

**Comprehensive Bracket Summary** (~50-80 tokens)
```bash
emacsclient -e '(claude/bracket-summary "FILE_PATH")'
```
Returns: Complete analysis combining counts, type checking, and recommendations

## Diagnostic Strategy

**Always follow this progressive approach:**

1. **Quick Summary First** - Use `claude/quick-paren-summary` to confirm there's an issue (15-20 tokens)
2. **Check Bracket Types** - Use `claude/count-by-type` to see which bracket types are imbalanced (30 tokens)
3. **Detect Type Mismatches** - Use `claude/find-type-mismatches` to find critical type errors like `[}` (40-60 tokens)
4. **Locate Precisely** - Use `claude/minimal-diagnostic` to find exact error location (20-30 tokens)
5. **Multi-Error Search** - If multiple errors suspected, use `claude/find-unmatched-opener` (30-40 tokens each)
6. **Cross-Validate** - For complex cases, use `claude/combined-diagnosis` or `claude/bracket-summary` (60-80 tokens)

**IMPORTANT**: Type mismatches (e.g., `[1 2 3}`) can appear "balanced" to simple counters because they have equal open/close counts but use wrong types. Always check for type mismatches in Clojure code.

**Never read entire file contents when debugging parens** - this wastes 95%+ tokens. Always use diagnostics.

## Path Handling

- **Bash**: Use backslashes or quoted paths: `"C:\\Users\\Apollo\\file.el"`
- **Emacsclient**: Use forward slashes: `"C:/Users/Apollo/file.el"`

## Example Usage

### Example 1: Emacs Lisp Paren Error

```bash
# User reports: "I'm getting unmatched paren error in my config"

# Step 1: Quick check
emacsclient -e '(claude/quick-paren-summary "C:/Users/Apollo/.emacs.d/init.el")'
# Output: (:status "error" :message "Unmatched opener at line 42")

# Step 2: Get details
emacsclient -e '(claude/minimal-diagnostic "C:/Users/Apollo/.emacs.d/init.el")'
# Output: (:line 42 :column 15 :char "(" :context "(defun my-function...")

# Step 3: Report to user
"Found unmatched opening parenthesis at line 42, column 15 in the defun my-function definition."
```

### Example 2: Clojure Bracket Type Mismatch

```bash
# User reports: "My Clojure code won't compile but I don't see any obvious errors"

# Step 1: Check bracket types
emacsclient -e '(claude/count-by-type "C:/Users/Apollo/project/core.clj")'
# Output: (:paren-open 50 :paren-close 50 :square-open 10 :square-close 9 :curly-open 5 :curly-close 5)
# Notice: Counts look almost balanced, but square brackets off by 1

# Step 2: Check for type mismatches (critical!)
emacsclient -e '(claude/find-type-mismatches "C:/Users/Apollo/project/core.clj")'
# Output: (:status "error" :count 2
#          :type-mismatches ((:opened 91 :closed 41 :expected 93
#                             :open-line 27 :close-line 27
#                             :open-context "(defn my-func [x y)")))
# Found: Function parameter vector `[x y)` - opened with [ but closed with )

# Step 3: Report to user
"Found critical type mismatch on line 27: function parameters opened with [ but closed with ).
Should be: (defn my-func [x y] ...) not (defn my-func [x y) ...)"
```

## Output Format

All diagnostic functions return structured plists:

### Standard Diagnostic Output
```elisp
(:status "ok"|"error"
 :line NUMBER
 :column NUMBER
 :char "("|")"|"["|"]"|"{"|"}"
 :context "surrounding code..."
 :message "human-readable description")
```

### Bracket Count by Type Output
```elisp
(:paren-open N :paren-close M :paren-diff D
 :square-open N :square-close M :square-diff D
 :curly-open N :curly-close M :curly-diff D
 :total-diff TOTAL)
```

### Type Mismatch Detection Output
```elisp
(:status "error"|"ok"
 :count N
 :type-mismatches ((:error type-mismatch
                    :opened CHAR :closed CHAR :expected CHAR
                    :open-line N :open-col N
                    :close-line N :close-col N
                    :open-context "..." :close-context "...")
                   ...)
 :unmatched-opens N)
```

### Bracket Summary Output
```elisp
(:counts <count-by-type-plist>
 :type-check <type-mismatch-plist>
 :recommendation "✓ All balanced" | "✗ N type mismatch(es) detected" | ...)
```

Parse these structured outputs rather than raw file contents.

## Error Handling

If emacsclient fails:
1. Check if Emacs server is running: `emacsclient -e '(claude/ping)'`
2. Verify helper functions loaded: Check that `claude-paren-diagnostics.el` is loaded
3. For bracket type functions, ensure `bracket-type-detection-clean.el` is loaded:
   ```bash
   emacsclient -e '(load-file "C:/Users/Apollo/em/bracket-type-detection-clean.el")'
   ```
4. Fall back to reading file only if diagnostics completely unavailable

## Performance Notes

- Diagnostics are 95%+ more token-efficient than reading files
- Most issues resolved in 50-100 tokens vs 5000+ for file reading
- Multiple validation methods provide high confidence results
- Early termination on first error for efficiency
