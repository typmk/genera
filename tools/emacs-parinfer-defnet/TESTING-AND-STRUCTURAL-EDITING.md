# Testing and Structural Editing in Emacs

## Two Separate Questions, Both with Great Answers!

### 1. Testing Large Programs - YES! ✅
### 2. Fixing Parentheses/Structural Issues - YES! ✅

---

## Part 1: Testing Tools

### Built-in: ERT (Emacs Lisp Regression Testing)

**What it does:**
- Define and run test suites
- Report test results
- Interactive debugging of test failures
- Built into Emacs (no installation needed)

**Example:**
```elisp
(require 'ert)

(ert-deftest test-addition ()
  "Test that addition works correctly."
  (should (= (+ 1 2) 3))
  (should (= (+ 5 5) 10)))

(ert-deftest test-string-concat ()
  "Test string concatenation."
  (should (string= (concat "hello" " " "world")
                   "hello world")))

;; Run tests
(ert-run-tests-interactively t)
```

**Pros:**
- ✅ Built-in (always available)
- ✅ Simple to use
- ✅ Interactive debugging
- ✅ Well documented

**Cons:**
- ❌ Verbose for complex setups
- ❌ Limited mocking/stubbing
- ❌ Manual test organization

---

### Enhanced: Buttercup (BDD Framework)

**What it does:**
- Behavior-driven development style
- Powerful mocking ("spies")
- Shared setup/teardown (before-each, after-each)
- Better test organization
- Compatible with ERT

**Installation:**
```elisp
(use-package buttercup
  :ensure t)
```

**Example:**
```elisp
(require 'buttercup)

(describe "A calculator"
  (describe "addition"
    (it "adds two numbers"
      (expect (+ 2 3) :to-equal 5))

    (it "handles negative numbers"
      (expect (+ -5 3) :to-equal -2)))

  (describe "with mocking"
    (before-each
      (spy-on 'some-expensive-function :and-return-value 42))

    (it "calls the function"
      (my-code-that-calls-expensive-function)
      (expect 'some-expensive-function :to-have-been-called))))

;; Run with: M-x buttercup-run
```

**Pros:**
- ✅ Readable BDD syntax (describe/it)
- ✅ Powerful mocking/spies
- ✅ Shared fixtures (before-each/after-each)
- ✅ Better organization
- ✅ Used by major projects (CIDER)

**Cons:**
- ❌ Requires installation
- ❌ More complex than ERT

---

### Language-Specific Test Runners

**For Clojure (CIDER):**
```elisp
;; Run tests in current namespace
M-x cider-test-run-ns-tests

;; Run all tests in project
M-x cider-test-run-project-tests

;; Re-run failed tests
M-x cider-test-rerun-failed-tests
```

**For Common Lisp (SLIME):**
```elisp
;; Works with libraries like FiveAM, Prove, etc.
;; Example with FiveAM:
(in-package :my-package-tests)

(test simple-addition
  (is (= (+ 1 2) 3)))

;; Run from SLIME REPL:
(run! 'simple-addition)
```

**For Julia:**
```elisp
;; julia-snail provides test running
M-x julia-snail-send-line  ; Send test to REPL
```

**For C++:**
```elisp
;; Compile and run tests
M-x compile RET ./run_tests RET

;; Or with projectile
M-x projectile-test-project
```

---

## Part 2: AI-Assisted Test Generation

### With Claude Code (Me!) ✅

**How I can help:**
1. **Generate test skeletons** based on your code
2. **Suggest test cases** (edge cases, error conditions)
3. **Write complete tests** in ERT or Buttercup format
4. **Create mocks/stubs**
5. **Organize test suites**

**Example workflow:**
```
You: "Write tests for this function"
Me: Analyzes function, generates comprehensive test suite
```

**Integration with claude-code.el:**
- You can ask me to write tests while in Emacs
- I can read your code and generate tests directly
- Tests appear in proper format (ERT, Buttercup, etc.)

---

### With GitHub Copilot

**Installation:**
```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode))
```

**Usage:**
- Type test function signature
- Copilot suggests test implementation
- Accept with TAB

**Pros:**
- ✅ Real-time suggestions
- ✅ Context-aware
- ✅ Learns from your style

**Cons:**
- ❌ Subscription required
- ❌ May need guidance for complex tests

---

### With gptel (ChatGPT/Claude in Emacs)

**Installation:**
```elisp
(use-package gptel
  :ensure t
  :config
  (setq gptel-model "claude-3-5-sonnet-20241022"
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))))
```

**Usage:**
1. Select function code
2. `M-x gptel-send` with prompt: "Write comprehensive tests for this"
3. Tests appear in buffer

**Pros:**
- ✅ Full conversation context
- ✅ Can refine tests interactively
- ✅ Explains test rationale

---

## Part 3: Parentheses/Structural Editing

### Problem: Mismatched Parentheses

**Common issues:**
- Unmatched opening/closing parens
- Lost track of nesting depth
- Accidentally deleted a paren
- Need to restructure code

### Solution 1: Paredit (The Classic)

**What it does:**
- **Prevents** unmatched parens
- Keeps expressions balanced
- Structural editing commands
- Works at the expression level

**Installation:**
```elisp
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (scheme-mode . paredit-mode)))
```

**Key Features:**
- **Auto-balancing**: Type `(` and `)` appears automatically
- **Safe deletion**: Can't delete unmatched parens
- **Structural movement**: Move by s-expressions
- **Slurping/barfing**: Add/remove expressions from forms

**Common Commands:**
```elisp
;; Slurp: pull next expression into current one
C-<right>  ; (foo) bar  =>  (foo bar)

;; Barf: push last expression out
C-<left>   ; (foo bar)  =>  (foo) bar

;; Wrap in parens
M-(        ; foo  =>  (foo)

;; Split expression
M-S        ; (foo bar)  =>  (foo) (bar)

;; Join expressions
M-J        ; (foo) (bar)  =>  (foo bar)

;; Kill expression
C-M-k      ; Delete forward s-expression

;; Safe delete
C-d        ; Only deletes if balanced
```

**Pros:**
- ✅ Never get unmatched parens
- ✅ Powerful structural editing
- ✅ Works at semantic level
- ✅ Battle-tested (decades old)

**Cons:**
- ❌ Learning curve
- ❌ Different from regular editing
- ❌ Lisp-focused

---

### Solution 2: Smartparens (Modern, Multi-Language)

**What it does:**
- Everything Paredit does
- Works with ALL languages
- Handles pairs: (), [], {}, "", '', etc.
- More flexible than Paredit

**Installation:**
```elisp
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))
```

**Key Features:**
- **Universal**: Works with any language
- **Smart pairing**: Context-aware
- **Hybrid modes**: Can use Paredit bindings
- **Visual feedback**: Highlights matching pairs

**Modes:**
- `smartparens-mode` - Basic pairing
- `smartparens-strict-mode` - Paredit-like (prevents unbalanced)

**Common Commands:**
```elisp
;; Slurp/barf (same as Paredit)
C-<right>, C-<left>

;; Navigate
C-M-f     ; Forward s-expression
C-M-b     ; Backward s-expression

;; Kill
C-M-k     ; Kill forward
C-M-w     ; Copy expression

;; Wrap
M-(       ; Wrap in parens
```

**Pros:**
- ✅ Works everywhere (Python, C++, JavaScript, etc.)
- ✅ More flexible than Paredit
- ✅ Good documentation
- ✅ Active development

**Cons:**
- ❌ More complex configuration
- ❌ Can be "too smart" sometimes

---

### Solution 3: Parinfer (Inference-based)

**What it does:**
- **Infers** structure from indentation
- OR infers indentation from structure
- Minimal intervention
- Lisp-focused

**Two modes:**
- **Indent Mode**: Indentation controls structure
- **Paren Mode**: Parentheses control indentation

**Installation:**
```elisp
(use-package parinfer-rust-mode
  :ensure t
  :hook ((emacs-lisp-mode . parinfer-rust-mode)
         (clojure-mode . parinfer-rust-mode)
         (lisp-mode . parinfer-rust-mode))
  :config
  (setq parinfer-rust-auto-download t))
```

**Pros:**
- ✅ Minimal learning curve
- ✅ "Just works" feeling
- ✅ Fast (Rust implementation)

**Cons:**
- ❌ Can cause issues with version control (reformats)
- ❌ Less explicit control
- ❌ Lisp-only

---

### Solution 4: Built-in Tools

**Check Parentheses:**
```elisp
M-x check-parens
```
Checks if all parentheses are balanced in buffer.

**Show Paren Mode (Built-in):**
```elisp
(show-paren-mode 1)
```
Highlights matching parentheses.

**Electric Pair Mode (Built-in):**
```elisp
(electric-pair-mode 1)
```
Auto-inserts closing parens (basic, not structural).

---

## Comparison: Paredit vs Smartparens vs Parinfer

| Feature | Paredit | Smartparens | Parinfer |
|---------|---------|-------------|----------|
| **Balanced enforcement** | ✅ Strong | ✅ Optional | ✅ Automatic |
| **Multi-language** | ❌ Lisp-focused | ✅ All languages | ❌ Lisp-only |
| **Learning curve** | Steep | Moderate | Gentle |
| **Structural editing** | ✅ Excellent | ✅ Excellent | ⚠️ Limited |
| **Indentation handling** | Manual | Manual | ✅ Automatic |
| **Git-friendly** | ✅ Yes | ✅ Yes | ⚠️ No (reformats) |
| **Maturity** | Very mature | Mature | Newer |

---

## Recommendations

### For Testing

**Use Buttercup for:**
- New Elisp projects
- Projects needing mocks/spies
- BDD-style tests
- Team projects (readable tests)

**Use ERT for:**
- Quick tests
- Simple assertions
- Built-in availability

**Use Language-Specific:**
- CIDER for Clojure
- SLIME + test library for Common Lisp
- pytest.el for Python
- Compile + test framework for C++

**Use AI (Me!):**
- Generating test skeletons
- Suggesting edge cases
- Writing boilerplate
- Explaining test strategies

---

### For Parentheses/Structural Editing

**Use Smartparens if:**
- ✅ You work with multiple languages
- ✅ You want flexibility
- ✅ You want Paredit features everywhere

**Use Paredit if:**
- ✅ You primarily write Lisp
- ✅ You want strict enforcement
- ✅ You prefer the classic tool

**Use Parinfer if:**
- ✅ You're new to structural editing
- ✅ You work solo (Git issues less important)
- ✅ You want minimal intervention

**My Recommendation:**
**Start with Smartparens in strict mode** - best of both worlds.

---

## Setup Example: Complete Testing + Structural Editing

```elisp
;; Structural editing - Smartparens (works everywhere)
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (smartparens-strict-mode t)  ; Paredit-like strictness
  (show-smartparens-global-mode t))

;; Testing - Buttercup (better than ERT)
(use-package buttercup
  :ensure t)

;; Show matching parens (built-in)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Check parens on save
(add-hook 'before-save-hook
          (lambda ()
            (when (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode))
              (check-parens))))

;; Language-specific test runners
(use-package cider
  :ensure t  ; Clojure testing
  :config
  (setq cider-test-show-report-on-success t))

;; Optional: Visual feedback for errors
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
```

---

## Helper Functions for Programmatic Access

I can add these to `claude-helpers.el`:

```elisp
;;; Testing helpers

(defun claude/run-tests ()
  "Run tests and return results."
  (condition-case err
      (let ((results (ert-run-tests-batch-and-exit)))
        (list :status "ok"
              :results results))
    (error
     (list :status "error"
           :error (error-message-string err)))))

(defun claude/check-parens-all-buffers ()
  "Check parentheses in all Lisp buffers."
  (let ((results '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode))
          (condition-case err
              (progn
                (check-parens)
                (push (list :buffer (buffer-name) :status "ok") results))
            (error
             (push (list :buffer (buffer-name)
                        :status "error"
                        :error (error-message-string err))
                   results))))))
    (list :status "ok"
          :checked (length results)
          :results results)))

(defun claude/paredit-check ()
  "Check if paredit/smartparens is active."
  (list :paredit-mode (bound-and-true-p paredit-mode)
        :smartparens-mode (bound-and-true-p smartparens-mode)
        :smartparens-strict (bound-and-true-p smartparens-strict-mode)
        :show-paren-mode show-paren-mode))
```

---

## Quick Start Guide

### Set Up Testing (5 minutes)

1. **Install Buttercup:**
```elisp
(use-package buttercup
  :ensure t)
```

2. **Write a test file** (e.g., `test-my-code.el`):
```elisp
(require 'buttercup)

(describe "My code"
  (it "does something"
    (expect (my-function 1 2) :to-equal 3)))
```

3. **Run tests:**
```
M-x buttercup-run-at-point
```

### Set Up Structural Editing (2 minutes)

1. **Install Smartparens:**
```elisp
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (smartparens-strict-mode t))
```

2. **Restart Emacs**

3. **Try it:** Type `(` and see `)` appear automatically!

---

## Summary

### Testing: YES! ✅

**Options:**
- ERT (built-in, simple)
- Buttercup (powerful, BDD-style)
- Language-specific (CIDER, SLIME, etc.)
- AI-assisted (me, Copilot, gptel)

**I can help:**
- Generate test skeletons
- Suggest test cases
- Write complete test suites
- Explain testing strategies

### Parentheses: YES! ✅

**Options:**
- Smartparens (recommended - works everywhere)
- Paredit (classic, Lisp-focused)
- Parinfer (inference-based)
- Built-in tools (show-paren-mode, check-parens)

**Benefits:**
- Never have unmatched parens
- Structural editing (slurp, barf, etc.)
- Safe deletion
- Visual feedback

---

## Want Me To Set These Up?

I can:
1. **Add testing + structural editing** to your init.el
2. **Create test helper functions** for programmatic access
3. **Generate tests** for your existing code
4. **Show you** how to use structural editing effectively

Just let me know what you'd like to prioritize!
