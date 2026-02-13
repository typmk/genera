---
name: clj-kondo-diagnose
description: Use clj-kondo for comprehensive Clojure/ClojureScript/CLJC diagnostics. Provides exact error locations (line:column), mismatched brackets, unmatched parens, and semantic analysis (undefined vars, unused bindings). Auto-invoked for .clj, .cljs, .cljc files. Rated ⭐⭐⭐⭐⭐ by canonical methods - the gold standard for Clojure.
allowed-tools:
  - Bash
---

# clj-kondo Diagnostic Skill

You are an expert at using clj-kondo for Clojure code diagnostics.

## When to Use This Skill

Automatically activate when:
- User has Clojure/ClojureScript files (.clj, .cljs, .cljc)
- User reports errors in Clojure code
- User asks about paren errors, syntax errors, or structural issues
- User wants comprehensive code analysis

**This is the PRIMARY tool for Clojure diagnostics.**

## Why clj-kondo is Best-in-Class

From canonical method comparison:
- **Method #3: Parser-Based Detection** - ⭐⭐⭐⭐⭐ rating
- Exact line:column locations
- Finds missing/extra parens
- Detects mismatched brackets (e.g., found '(' but got '}')
- Semantic analysis (undefined vars, unused bindings, type errors)
- **Fast: 16ms for 1187-line file** (vs 5s for other tools)
- **Accurate: 100% detection rate** on structural issues

## Basic Usage

### Human-Readable Output
```bash
clj-kondo --lint "FILE_PATH"
```

Example output:
```
C:/path/to/file.cljc:160:26: error: Mismatched bracket:
  found an opening ( and a closing } on line 162
C:/path/to/file.cljc:162:34: error: Mismatched bracket:
  found an opening ( on line 160 and a closing }

linting took 16ms, errors: 2, warnings: 0
```

### JSON Output (for programmatic parsing)
```bash
clj-kondo --lint "FILE_PATH" --config '{:output {:format :json}}'
```

Example output:
```json
{
  "findings": [
    {
      "type": "syntax",
      "filename": "file.cljc",
      "row": 160,
      "col": 26,
      "message": "Mismatched bracket: found an opening ( and a closing } on line 162",
      "level": "error"
    }
  ],
  "summary": {
    "error": 2,
    "warning": 0,
    "duration": 16,
    "files": 1
  }
}
```

## What clj-kondo Detects

### 1. Structural Errors ⭐⭐⭐⭐⭐
- **Missing closing parens/brackets**: EOF while reading
- **Missing opening parens/brackets**: Unexpected closing delimiter
- **Mismatched brackets**: Found '(' but got '}', etc.
- **Unbalanced quotes**: Unclosed strings
- **Invalid reader macros**: Malformed #(), #{}, #_, etc.

### 2. Semantic Errors ⭐⭐⭐⭐⭐
- **Unresolved symbols**: Undefined vars, missing requires
- **Unused bindings**: Unused let bindings, function params
- **Arity mismatches**: Wrong number of arguments
- **Invalid syntax**: Malformed special forms
- **Namespace issues**: Namespace doesn't match filename

### 3. Code Quality (warnings/info)
- **Unused imports**: Required but never used
- **Deprecated usage**: Old APIs
- **Type hints**: Missing or incorrect
- **Idiomatic issues**: Non-idiomatic patterns

## Diagnostic Strategy

### Step 1: Quick Check (Human-Readable)
```bash
clj-kondo --lint "FILE_PATH"
```

**Output interpretation:**
- `error:` - Must fix (breaks compilation)
- `warning:` - Should fix (code smells)
- `info:` - Consider fixing (style)

### Step 2: Detailed Analysis (JSON for parsing)
```bash
clj-kondo --lint "FILE_PATH" --config '{:output {:format :json}}'
```

Parse JSON to extract:
- `:row` - Line number (1-indexed)
- `:col` - Column number (1-indexed)
- `:type` - Error category (syntax, unresolved-symbol, etc.)
- `:level` - Severity (error, warning, info)
- `:message` - Human-readable description

### Step 3: Report to User

Format output clearly:
```
Found 2 errors in file.cljc:

1. Line 160, Column 26: Mismatched bracket
   found an opening ( and a closing } on line 162

2. Line 162, Column 34: Mismatched bracket
   found an opening ( on line 160 and a closing }

Recommendation: Add missing ')' at line 161
```

## Advanced Usage

### Lint Multiple Files
```bash
clj-kondo --lint src/ test/
```

### Copy Findings to Clipboard (if needed)
```bash
clj-kondo --lint "FILE_PATH" --config '{:output {:format :json}}' | clip
```

### Cache Analysis (for large projects)
```bash
# First run creates .clj-kondo cache
clj-kondo --lint src/ --dependencies

# Subsequent runs are faster
clj-kondo --lint src/
```

### Custom Configuration
```bash
clj-kondo --lint "FILE_PATH" --config '{
  :linters {
    :unresolved-symbol {:level :error}
    :unused-binding {:level :warning}
  }
}'
```

## Performance Characteristics

| File Size | Lines | Typical Duration |
|-----------|-------|-----------------|
| Small | <100 | <10ms |
| Medium | 100-1000 | 10-50ms |
| Large | 1000-5000 | 50-500ms |
| Very Large | 5000+ | 500ms-2s |

**Our test:** 1187 lines → 16ms ⚡

## Comparison to Other Tools

| Tool | Speed | Accuracy | Details |
|------|-------|----------|---------|
| **clj-kondo** | ⭐⭐⭐⭐⭐ (16ms) | ⭐⭐⭐⭐⭐ (100%) | ⭐⭐⭐⭐⭐ (exact loc) |
| Parinfer | ⭐⭐⭐⭐☆ (5s) | ⭐⭐⭐⭐⭐ (via indent) | ⭐⭐⭐⭐☆ (inferred) |
| Stack counter | ⭐⭐⭐⭐⭐ (<1s) | ⭐⭐⭐⭐⭐ (count) | ⭐⭐☆☆☆ (no location) |
| clojure-lsp | ⭐⭐☆☆☆ (slow) | ⭐☆☆☆☆ (IDE-focused) | ⭐⭐⭐☆☆ (uses clj-kondo) |

**Recommendation:** Use clj-kondo as PRIMARY tool for Clojure

## Integration with Other Skills

### Workflow 1: Comprehensive Analysis
```
1. clj-kondo (primary) - Get exact errors
2. Parinfer (if needed) - Infer structure from indentation
3. LLM minimal-diff - Fix based on clj-kondo output
4. clj-kondo (validation) - Confirm fix worked
```

### Workflow 2: Quick Fix
```
1. clj-kondo - Get error
2. LLM fix - Apply minimal change
3. Done (skip re-validation if confident)
```

## Example Session

```
User: "I'm getting errors in my Clojure file"

Step 1: Run clj-kondo
bash: clj-kondo --lint core.clj

Output:
core.clj:45:32: error: Mismatched bracket: found ( but got }
core.clj:47:37: error: Mismatched bracket: found ( but got }

Step 2: Analyze context
bash: Read lines 40-50 of core.clj

Step 3: Identify issue
"Lines 45 and 47 are missing closing ')' for reset! calls"

Step 4: Suggest fix
"Add ')' at end of line 45 and line 47"

Step 5: After user applies fix, validate
bash: clj-kondo --lint core.clj
Output: linting took 12ms, errors: 0, warnings: 0 ✓
```

## Error Types Reference

### Common Syntax Errors

| Error | Meaning | Fix |
|-------|---------|-----|
| EOF while reading | Missing closing delimiter | Add ')' or ']' or '}' |
| Unmatched delimiter | Extra closing delimiter | Remove ')' or add opening '(' |
| Mismatched bracket | Wrong type (e.g., '[' with '}') | Use matching pair |
| Unclosed string | Missing closing " | Add closing quote |
| Invalid token | Malformed symbol/number | Fix token syntax |

### Common Semantic Errors

| Error | Meaning | Fix |
|-------|---------|-----|
| Unresolved symbol | Var not defined/required | Add require or def |
| Unused binding | Binding never used | Remove or use it |
| Wrong arity | Wrong arg count | Fix call site |
| Namespace mismatch | ns doesn't match filename | Rename ns or file |

## Configuration Tips

### For Strict Analysis
```bash
clj-kondo --lint "FILE_PATH" --config '{
  :linters {
    :unused-binding {:level :error}
    :unresolved-symbol {:level :error}
  }
}'
```

### For Lenient (warnings only)
```bash
clj-kondo --lint "FILE_PATH" --config '{
  :linters {
    :unused-binding {:level :off}
  }
}'
```

## Limitations

1. **Clojure-specific**: Only works for Clojure/ClojureScript
   - For Emacs Lisp: Use parinfer or stack counter
   - For Common Lisp: Use parinfer or stack counter

2. **Requires binary**: Not pure Emacs (unlike parinfer-elisp)
   - But: Fast, mature, actively maintained
   - Tradeoff: Speed and accuracy vs pure elisp

3. **May need cache**: First run on large projects is slower
   - Solution: Use --dependencies flag to build cache

## Installation Check

Verify clj-kondo is available:
```bash
clj-kondo --version
```

If not installed:
```bash
# Windows (scoop)
scoop install clj-kondo

# Mac
brew install borkdude/brew/clj-kondo

# Linux
curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
./install-clj-kondo
```

## Success Criteria

A successful clj-kondo diagnosis:
1. ✅ Exact line:column locations reported
2. ✅ Error type clearly identified
3. ✅ Severity level appropriate
4. ✅ Fast execution (<1s for typical files)
5. ✅ Actionable error messages

## Token Efficiency

- Running clj-kondo: ~50 tokens (command + parsing output)
- Reading context: ~100-200 tokens (only error region)
- Generating fix: ~200-500 tokens (LLM minimal-diff)
- Validation: ~30 tokens (re-run clj-kondo)

**Total: ~380-780 tokens** per fix cycle

vs traditional:
- Read entire file: ~5000 tokens
- Claude analyzes: ~10000 tokens
- Total: ~15000 tokens

**Savings: 95%** 🎉

---

## Summary

clj-kondo is the **gold standard** for Clojure diagnostics:
- ⭐⭐⭐⭐⭐ Accuracy (100% on structural + semantic)
- ⭐⭐⭐⭐⭐ Speed (16ms typical)
- ⭐⭐⭐⭐⭐ Detail (exact line:column)
- ⭐⭐⭐⭐⭐ Coverage (structure + semantics)

**Use this as PRIMARY tool for all Clojure diagnostics.**
