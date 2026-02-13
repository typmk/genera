---
name: emacs-paren-fixer
description: Automatically fix parenthesis errors using minimal-diff repair after diagnosis. Invoked when user asks to fix paren errors or when diagnostics reveal fixable structural issues.
allowed-tools:
  - Bash
  - Read
  - Edit
---

# Emacs Paren Fixer Skill

You are an expert at fixing parenthesis errors using a hybrid diagnostic + minimal-diff repair strategy.

## When to Use This Skill

Automatically activate when:
- User says "fix the paren error"
- User says "balance the parentheses"
- Diagnosis reveals fixable structural issues
- User confirms they want automated repair after seeing diagnostic

## Fixing Strategy: Minimal-Diff Repair

Based on structural editing research, use this proven workflow:

### Phase 1: Precise Diagnosis (20-30 tokens)
```bash
emacsclient -e '(claude/minimal-diagnostic "FILE_PATH")'
```

Returns exact error location:
```elisp
(:line 423 :column 15 :char "(" :context "defun my-func...")
```

### Phase 2: Extract Minimal Context (~100-200 tokens)

Read **only** 10-20 lines around the error:

```bash
# Read lines START to END
emacsclient -e '(with-temp-buffer
  (insert-file-contents "FILE_PATH")
  (goto-line START)
  (buffer-substring (point) (progn (forward-line 20) (point))))'
```

**Never read entire file** - wastes tokens and increases hallucination risk.

### Phase 3: Generate Minimal Diff (~50-100 tokens)

Prompt yourself internally:

```
Task: Fix unmatched CHAR at line LINE, column COL
Context: [10-20 lines around error]

Requirements:
1. SMALLEST possible change
2. Do NOT reformat/reindent (preserves user style)
3. Do NOT rename variables
4. Do NOT move code
5. ONLY add/remove/fix the specific delimiter

Produce: The exact Edit tool call with minimal old_string/new_string
```

### Phase 4: Apply Fix

Use Edit tool with **smallest possible old_string**:

```
Edit:
  file_path: FILE_PATH
  old_string: "(defn my-func [x]\n  (bar x)"  # 2-3 lines max
  new_string: "(defn my-func [x]\n  (bar x))" # Fixed
```

### Phase 5: Validate (~20-30 tokens)

```bash
emacsclient -e '(claude/quick-paren-summary "FILE_PATH")'
```

If returns `(:status "ok")` → Success!
If returns `(:status "error")` → Retry with more context (max 3 attempts)

## Total Token Budget

| Phase | Cost | Running Total |
|-------|------|---------------|
| Diagnose | 30 | 30 |
| Extract context | 150 | 180 |
| Generate fix | 80 | 260 |
| Validate | 30 | 290 |
| **Total** | **~290 tokens** | **(vs 15000+ blind fixing)** |

## Error Recovery

### If Fix Fails Validation

1. **Retry 1**: Expand context window (20 → 40 lines)
2. **Retry 2**: Use combined diagnosis for cross-validation
   ```bash
   emacsclient -e '(claude/combined-diagnosis "FILE_PATH")'
   ```
3. **Retry 3**: Ask user for guidance

### If Multiple Errors

Run diagnostics first to count errors:
```bash
emacsclient -e '(claude/find-unmatched-opener "FILE_PATH")'
```

If >3 errors, suggest:
- Fix one at a time (start from earliest)
- Or use `/emacs-debugger` subagent for complex cases

## Advanced: AST-Aware Repair (Future Enhancement)

For highest correctness, could integrate:

```elisp
;; Convert to tree representation
(read-tree "file.clj")
;; Returns: (list 'defn 'my-func (vec 'x) (list 'bar 'x))

;; LLM edits tree (impossible to create unbalanced parens)

;; Generate back to source
(write-tree edited-tree)
```

This guarantees structural correctness (used by Parinfer, rewrite-clj).

## Comparison to Pure LLM Methods

### Token-Constraint Streaming (their method #1)
```
Cost: Stream entire file with state machine
Tokens: 5000+
Accuracy: High (if implemented correctly)
Complexity: High (need state tracking)
```

### Our Emacs-Hybrid Method
```
Cost: Diagnose → context → fix → validate
Tokens: ~290
Accuracy: Very high (Emacs validates)
Complexity: Low (Emacs does parsing)
```

**Winner: Our method** - 95% token reduction, simpler, validated by Emacs.

### Reader + Diff (their method #2)
```
Similar to our approach, but we use Emacs reader instead of tools.reader.
Advantage: Emacs reader is already running, no separate process.
```

### Minimal-Diff Repair (their method #3)
```
This is EXACTLY what we implement.
We use Emacs to find the error, then apply their repair strategy.
Perfect combination.
```

## Preventing Hallucinations

**Their insight:** LLMs hallucinate global structure, not local patterns.

**Our solution:**
- Emacs provides ground truth (global structure)
- LLM only sees 10-20 lines (local pattern)
- LLM generates tiny diff (minimal surface for errors)
- Emacs validates result (catches any mistakes)

**Result:** Near-zero hallucination rate.

## Integration with Other Skills

Works seamlessly with:
- **emacs-diagnostics**: Always diagnose first
- **emacs-debugger**: Escalate complex multi-error cases
- **emacs-buffer-ops**: Check file state before/after

## Success Metrics

Track these to measure effectiveness:
- Fix accuracy: Target >95%
- Token usage: Target <500 per fix
- Iterations needed: Target 1 (rarely 2)
- User intervention: Target <5%

## Example Session

```
User: "Fix the paren error in my core.clj"

Step 1: Diagnose
  → emacsclient: Line 423, col 15, unmatched '('

Step 2: Extract context (lines 413-433)
  → 20 lines around error

Step 3: Generate fix
  → Analyze: Missing ')' after (bar x) on line 425
  → Edit: Add ')' at end of line 425

Step 4: Validate
  → emacsclient: ✓ File now balances correctly

Report: "Fixed unmatched '(' at line 423 by adding ')' at line 425.
         File now parses correctly."

Total: 285 tokens
```

## Communication Style

When fixing:
1. Show diagnostic result
2. Explain what you'll change
3. Apply fix
4. Confirm validation passed
5. Summarize what was fixed and why

Be transparent about token usage and validation results.
