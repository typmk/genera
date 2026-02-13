# Emacs Integration Plugin - Quick Reference

## 🚀 Most Common Operations

### Check for Paren Errors

**Clojure files (.clj, .cljs, .cljc):**
```bash
# Auto-invokes clj-kondo-diagnose skill
"Check my-file.clj for errors"

# Or manually:
/emacs-diag my-file.clj
```

**Other Lisp files (.el, .lisp, .scm):**
```bash
# Auto-invokes emacs-diagnostics skill
"Check my-file.el for errors"

# Or manually:
/emacs-diag my-file.el
```

### Fix Paren Errors

```bash
"Fix the paren error in my-file.clj"
# Uses: emacs-paren-fixer skill
# Strategy: Diagnosis → LLM minimal-diff → Validation
```

### Test Emacs Connection

```bash
/emacs-ping
# Expected: "Emacs is responsive"
```

### Query Emacs Configuration

```bash
/emacs-config "variable-name"
# Examples:
/emacs-config "user-emacs-directory"
/emacs-config "load-path"
```

### List Open Buffers

```bash
"List my open buffers"
# Or:
/emacs-repl  # Shows REPL-specific buffers
```

### Evaluate Emacs Lisp

```bash
/emacs-eval "(+ 1 2 3)"
# Returns: 6
```

---

## 🎯 Common Scenarios

### Scenario 1: "I have syntax errors in my Clojure file"

```
You: "Check src/core.clj for errors"

Claude: [Auto-invokes clj-kondo-diagnose]
Running: clj-kondo --lint src/core.clj

Found 1 error:
Line 45, Column 32: Mismatched bracket
  found an opening ( and a closing } on line 47

Would you like me to fix it?
```

### Scenario 2: "Is my Emacs configured correctly for Clojure?"

```
You: "Check my Clojure setup"

Claude: [Auto-invokes emacs-config-query]
Checking packages:
✓ clojure-mode: installed
✓ cider: installed
✓ smartparens: installed
✓ eglot: installed

Clojure LSP: c:/Users/Apollo/scoop/shims/clojure-lsp.exe ✓

Your setup looks good!
```

### Scenario 3: "My CIDER REPL won't connect"

```
You: "Debug my CIDER connection"

Claude: I'll use the emacs-debugger agent for this.

[Launches specialized agent]
Agent checks:
1. CIDER installed? ✓
2. nREPL running? Let me check...
   /emacs-repl
   Found: *cider-repl localhost* - Connected ✓

The REPL is actually connected. Are you seeing a specific error?
```

### Scenario 4: "I want to add a new function but worried about breaking syntax"

```
You: "Add a new function to calculate fibonacci"

Claude: [PreToolUse hook automatically runs]
Checking current file balance... ✓ Balanced

[Adds function]

[Hook runs again after edit]
Checking new balance... ✓ Still balanced

Function added successfully!
```

---

## 📊 Token Usage Comparison

| Task | Traditional LLM | This Plugin | Savings |
|------|----------------|-------------|---------|
| Find paren error | ~5,000 tokens | ~30 tokens | **99.4%** |
| Fix single error | ~17,000 tokens | ~380 tokens | **97.8%** |
| Check config | ~500 tokens | ~20 tokens | **96.0%** |
| List buffers | ~1,000 tokens | ~40 tokens | **96.0%** |

---

## 🔧 Skills Reference

### Auto-Invoked Skills

| Skill | Triggers On | Purpose |
|-------|-------------|---------|
| **clj-kondo-diagnose** | .clj, .cljs, .cljc files | Exact line:column errors for Clojure |
| **emacs-diagnostics** | .el, .lisp, .scm files | Stack-based counting + type-aware (), [], {} detection |
| **emacs-config-query** | Config questions | Package/variable status |
| **emacs-buffer-ops** | Buffer questions | Buffer lists, REPL state |
| **emacs-paren-fixer** | Fix requests | Minimal-diff repairs |

### Manual Skills

Use by mentioning them explicitly:
```
"Use emacs-debugger agent to investigate..."
```

---

## 💻 Command Reference

| Command | Args | Example |
|---------|------|---------|
| `/emacs-ping` | None | `/emacs-ping` |
| `/emacs-eval` | `<elisp-expr>` | `/emacs-eval "(+ 1 2)"` |
| `/emacs-diag` | `<file-path>` | `/emacs-diag src/core.clj` |
| `/emacs-config` | `<var-name>` | `/emacs-config "load-path"` |
| `/emacs-repl` | None | `/emacs-repl` |

---

## 🪝 Hooks Reference

### PreToolUse Hook

**Triggers:** Before Edit/Write on Lisp files

**Pattern:** `\\.el$|\\.clj$|\\.cljs$|\\.cljc$|\\.lisp$|\\.scm$`

**Action:** Runs quick diagnostic to check current balance

**Benefit:** Prevents introducing new paren errors

### SessionStart Hook

**Triggers:** When Claude Code session starts

**Action:** Runs `(claude/ping)` to verify Emacs connectivity

**Benefit:** Early detection of server issues

---

## 🎓 Diagnostic Strategy

### Progressive Diagnostic Levels

**Level 1: Quick Check (15 tokens)**
```elisp
(claude/quick-paren-summary "file.el")
;; Returns: (:open 315 :close 315 :balanced t)
```

**Level 2: Precise Error (20 tokens)**
```elisp
(claude/minimal-diagnostic "file.el")
;; Returns: (:unmatched 3 :locations (45 67 89))
```

**Level 3: Multiple Errors (30 tokens/error)**
```elisp
(claude/find-unmatched-opener "file.el")
;; Returns: List of all unmatched with context
```

**Level 4: Type-Aware Analysis (NEW - 30-80 tokens)**
```elisp
;; Count each bracket type separately
(claude/count-by-type "file.clj")
;; Returns: (:paren-open N :paren-close M :square-open N :square-close M :curly-open N :curly-close M)

;; Find type mismatches (e.g., [} or {])
(claude/find-type-mismatches "file.clj")
;; Returns: List of brackets opened with one type but closed with another

;; Comprehensive analysis
(claude/bracket-summary "file.clj")
;; Returns: Counts + type checking + recommendation
```

**Level 5: Full Analysis (40-50 tokens)**
```elisp
(claude/combined-diagnosis "file.el")
;; Returns: Multi-method validation
```

---

## 🎯 Best Practices

### DO ✅

- Let skills auto-invoke naturally
- Use `/emacs-ping` if connection issues
- Use progressive diagnostics (quick → detailed)
- Trust the hooks to prevent errors
- Use clj-kondo for Clojure (it's the best)

### DON'T ❌

- Don't ask Claude to read entire files
- Don't bypass diagnostics for speed
- Don't ignore hook warnings
- Don't manually calculate paren counts
- Don't fix Clojure files without clj-kondo check

---

## 🔍 Troubleshooting Quick Fixes

### "Emacs server not running"

```elisp
;; In Emacs, run:
(server-start)
```

### "claude/ping not found"

```elisp
;; Load the helpers:
(load "C:/Users/Apollo/em/claude-helpers.el")
```

### "clj-kondo not found"

```bash
# Install it:
scoop install clj-kondo  # Windows
brew install borkdude/brew/clj-kondo  # Mac
```

### Skills not auto-invoking

```bash
# Check plugin location:
ls ~/.claude-plugins/emacs-integration/.claude-plugin/plugin.json
```

---

## 📈 Performance Expectations

| Tool | Typical Time | File Size |
|------|-------------|-----------|
| **clj-kondo** | 16ms | 1187 lines |
| **Stack counter** | <1s | Any size |
| **Depth maps** | <1s | <5000 lines |
| **Parinfer** | ~5s | <2000 lines |
| **emacsclient** | <100ms | Query only |

---

## 🏆 Tool Ratings

| Tool | Missing ")" | Missing "(" | Location | Speed | Overall |
|------|-----------|-----------|----------|-------|---------|
| **clj-kondo** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | **⭐⭐⭐⭐⭐** |
| **Stack** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐☆☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Depth** | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐☆ | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ |
| **Parinfer** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐⭐ |

---

## 📚 Documentation Links

- **README.md** - Complete feature overview
- **INSTALL.md** - Installation instructions
- **PLUGIN-SUMMARY.md** - Deployment guide
- **DEPLOYMENT-CHECKLIST.md** - Pre-deployment validation
- **FINAL-DIAGNOSTIC-REPORT.md** - Comprehensive tool evaluation

---

## 💡 Pro Tips

### Tip 1: Trust the Auto-Invocation

Don't manually specify skills. Just describe the problem naturally:

```
✅ "Check my Clojure file for errors"
❌ "Use clj-kondo-diagnose skill to check my file"
```

### Tip 2: Use Progressive Diagnostics

Start with quick checks, drill down only if needed:

```
1. "Is the file balanced?" (15 tokens)
2. If not: "Where are the errors?" (20 tokens)
3. If multiple: "Show all errors" (30 tokens)
```

### Tip 3: Let Hooks Protect You

The PreToolUse hook automatically checks before edits. Trust it:

```
You: "Add a function"
Hook: [Runs quick check] ✓ Balanced
Claude: [Adds function]
Hook: [Checks again] ✓ Still balanced
```

### Tip 4: Use clj-kondo for Clojure

It's the best tool by far:
- 16ms execution
- Exact line:column
- Structural + semantic
- 100% accuracy

### Tip 5: Agent for Complex Issues

For multi-step debugging, use the agent:

```
"Use emacs-debugger agent to investigate why my REPL won't start"
```

---

## 🎯 One-Liner Cheat Sheet

```bash
# Connection
/emacs-ping

# Diagnose Clojure
"Check file.clj"  # or /emacs-diag file.clj

# Diagnose other Lisp
"Check file.el"  # or /emacs-diag file.el

# Fix errors
"Fix the errors in file.clj"

# Config check
/emacs-config "variable-name"

# REPL status
/emacs-repl

# Eval expression
/emacs-eval "(+ 1 2 3)"

# Complex debugging
"Use emacs-debugger agent to investigate..."
```

---

**Quick Reference Version: 1.0.0**

**For detailed docs, see README.md**

**For installation, see INSTALL.md**

**For methodology, see FINAL-DIAGNOSTIC-REPORT.md**
