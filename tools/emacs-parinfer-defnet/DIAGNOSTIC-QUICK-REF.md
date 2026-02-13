# Paren Diagnostics Quick Reference

## Installation Status: ✅ INSTALLED

Location: Lines 101-105 in `~/.emacs.d/init.el`

## Quick Commands (Copy-Paste Ready)

### 1. Minimal Diagnostic (BEST - Start Here)
```bash
emacsclient -e '(claude/minimal-diagnostic buffer-file-name)'
```
**Returns:** `(:ok t)` or `(:ok nil :line N :col N :msg "..." :text "...")`
**Tokens:** ~20-30

### 2. Quick Summary (Health Check)
```bash
emacsclient -e '(claude/quick-paren-summary buffer-file-name)'
```
**Returns:** `(:balance N :likely-issue "...")`
**Tokens:** ~15-20

### 3. Find All Unmatched
```bash
emacsclient -e '(claude/find-unmatched-opener buffer-file-name)'
```
**Returns:** List of all unmatched openers
**Tokens:** ~30-40 per error

### 4. Indentation Pattern Scan (NEW - Your Visual Method!)
```bash
emacsclient -e '(claude/scan-indentation-pattern buffer-file-name)'
```
**Returns:** Anomalies based on indentation flow
**Tokens:** ~60-80
**Note:** This mimics how you scan with rainbow delimiters!

### 4b. Efficient Structure Map (NEW - UPDATED!)
```bash
emacsclient -e '(claude/indentation-map buffer-file-name)'
# OR directly:
emacsclient -e '(claude/efficient-diagnosis buffer-file-name)'
```
**Returns:** Structural data with direct depth numbers (NO counting!)
**Tokens:** ~300-500 (50-85% reduction vs old visual bars)
**Note:** Now uses syntax-ppss parser - returns `:depth 3` not "│││"

### 4c. Combined Diagnosis (NEW - BEST!)
```bash
emacsclient -e '(claude/combined-diagnosis buffer-file-name)'
```
**Returns:** Both indentation heuristics AND syntax check with recommendation
**Tokens:** ~90-120
**Note:** Best of both worlds - fast identification + accurate confirmation

### 5. Full Diagnostic
```bash
emacsclient -e '(claude/diagnose-parens buffer-file-name)'
```
**Returns:** Complete error details with context
**Tokens:** ~50-100

### 6. All Diagnostics at Once
```bash
emacsclient -e '(claude/all-diagnostics buffer-file-name)'
```
**Returns:** Combined results from all methods
**Tokens:** ~200-300

## Typical Workflow

### When You Have a Paren Error:

**Step 1:** Run minimal diagnostic
```bash
emacsclient -e '(claude/minimal-diagnostic buffer-file-name)'
```

**Step 2:** Send me just the output (not the whole file!)
```
(:ok nil :line 42 :col 5 :msg "Unmatched bracket or quote" :text "(defun example (x)")
```

**Step 3:** I provide fix immediately

**Step 4:** Verify fix
```bash
emacsclient -e '(claude/minimal-diagnostic buffer-file-name)'
# => (:ok t)
```

## Token Savings

| Method | Tokens | Example |
|--------|--------|---------|
| Send entire file | ~5000 | ❌ Wasteful |
| Minimal diagnostic | ~30 | ✅ Perfect |
| Full diagnostic | ~100 | ✅ When needed |

**Average savings: 95%+**

## Available Functions

### Core Diagnostics
1. `claude/minimal-diagnostic` - Minimal error info (30 tokens)
2. `claude/diagnose-parens` - Full error details (100 tokens)
3. `claude/quick-paren-summary` - Quick health check (20 tokens)

### Indentation-Based (Enhanced!)
4. `claude/scan-indentation-pattern` - Visual scanning technique (60 tokens)
5. `claude/validate-by-indentation` - Enhanced indentation heuristics (60 tokens)
6. `claude/combined-diagnosis` - Indentation + syntax (best!) (90 tokens)

### Structure-Based (NEW - Most Efficient!) ⭐
7. `claude/structure-checkpoints` - Depth checkpoints using syntax-ppss (50-100 tokens)
8. `claude/structure-deltas` - Depth changes only (30-60 tokens)
9. `claude/stack-at-error` - Exact paren stack positions (40-80 tokens)
10. `claude/efficient-diagnosis` - Complete structural analysis (300-500 tokens)
11. `claude/indentation-map` - (Now redirects to efficient-diagnosis)

### Advanced Analysis
12. `claude/find-unmatched-opener` - Find all unmatched openers (40 tokens per error)
13. `claude/show-paren-depth` - Nesting depth visualization (varies)
14. `claude/all-diagnostics` - Run all diagnostics (200-300 tokens)

## Examples

### Example 1: Simple Error
```bash
emacsclient -e '(claude/minimal-diagnostic "test.el")'
# => (:ok nil :line 4 :col 0 :msg "Unmatched bracket or quote" :text "(defun test-missing-close (x)")
```
**Fix:** Add closing paren

### Example 2: Multiple Errors
```bash
emacsclient -e '(claude/find-unmatched-opener "test.el")'
# => (:status "error" :count 2 :unmatched ((:line 4 ...) (:line 12 ...)))
```
**Fix:** Add closers at lines 4 and 12

### Example 3: All Good
```bash
emacsclient -e '(claude/minimal-diagnostic "good.el")'
# => (:ok t)
```
**Status:** ✅ No issues

## Next Steps After Install

1. **Restart Emacs** (or reload: `M-x eval-buffer` in init.el)
2. **Test it:**
   ```bash
   emacsclient -e '(claude/minimal-diagnostic "~/em/test-paren-diagnostics.el")'
   ```
3. **Use it:** Next time you ask me to debug parens, just run the command and send me the output!

## Pro Tips

- Always try `minimal-diagnostic` first (fastest, most efficient)
- Use `quick-summary` to confirm if there's even a problem
- Use `find-unmatched-opener` for multiple errors
- Use `validate-by-indentation` when syntax check doesn't help
- Send me ONLY the diagnostic output, not the whole file!

## Integration with Your Workflow

**Before:** You show me entire file (5000 tokens)
**After:** You run diagnostic and show me 30 tokens

**Result:**
- ✅ 95%+ token savings
- ✅ Faster diagnosis
- ✅ More precise fixes
- ✅ More context available for other tasks

---

Enjoy token-efficient debugging! 🚀
