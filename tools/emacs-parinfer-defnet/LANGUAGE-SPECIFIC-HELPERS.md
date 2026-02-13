# Language-Specific Programming in Emacs

## Short Answer

**Current helpers are sufficient for basic language-agnostic tasks, but NOT optimal for language-specific REPL workflows.**

You'll need **additional packages and helpers** for each language's REPL interaction.

---

## What Current Helpers Provide

### ✅ Good For (Language Agnostic)

- File operations (read/write/edit)
- Buffer management
- Configuration checking
- Package management
- General Emacs state inspection
- Elisp evaluation

### ❌ NOT Sufficient For (Language Specific)

- REPL interaction (sending code to SBCL, Clojure REPL, etc.)
- Language server protocol (LSP) interaction
- Debugger control
- Compilation results
- Test running
- Code evaluation in language REPL
- Inspecting language-specific state

---

## Language-Specific Requirements

### Common Lisp (SBCL) - SLIME

**What SLIME provides:**
- Interactive REPL connection to SBCL
- Compile and load Lisp files
- Inspect objects, debug, trace functions
- REPL history and shortcuts
- Integration with Common Lisp debugger

**What you need:**
```elisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))
```

**Additional helpers needed:**
- Send code to SLIME REPL
- Get REPL output
- Query compilation results
- Inspect Lisp values
- Control debugger

### Clojure - CIDER

**What CIDER provides:**
- nREPL connection to Clojure process
- Interactive evaluation
- Test runner integration
- Debugger (cider-debug)
- ClojureDocs lookup
- Code navigation

**What you need:**
```elisp
(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil))
```

**Additional helpers needed:**
- Send forms to CIDER REPL
- Get evaluation results
- Run tests programmatically
- Query namespace info
- Get compilation errors

### C++ - LSP Mode

**What lsp-mode provides:**
- Language server integration (clangd)
- Code completion
- Error checking
- Go to definition
- Find references

**What you need:**
```elisp
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp))
  :config
  (setq lsp-clients-clangd-executable "clangd"))
```

**Additional helpers needed:**
- Query LSP diagnostics
- Get compilation errors
- Run/control debugger (GDB)
- Execute builds
- Parse compiler output

### Julia - julia-snail or julia-repl

**What julia-snail provides:**
- Julia REPL interaction (inspired by SLIME/CIDER)
- Code evaluation
- Module management
- Documentation lookup

**What you need:**
```elisp
(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

;; Or simpler julia-repl
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))
```

**Additional helpers needed:**
- Send code to Julia REPL
- Get evaluation results
- Query Julia variables
- Load modules
- Run tests

---

## Comparison: What's Missing

| Task | Current Helpers | Needed |
|------|----------------|--------|
| Edit files | ✅ Yes | - |
| Check config | ✅ Yes | - |
| List buffers | ✅ Yes | - |
| **Send code to REPL** | ❌ No | Language-specific |
| **Get REPL output** | ❌ No | Language-specific |
| **Evaluate expressions** | ✅ Elisp only | Each language |
| **Query debugger** | ❌ No | Language-specific |
| **Run tests** | ❌ No | Language-specific |
| **Get compilation errors** | ❌ No | Language-specific |
| **LSP queries** | ❌ No | LSP helpers |

---

## Recommended Approach

### Phase 1: Install Language Packages (Required)

**For Common Lisp:**
```elisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy)))
```

**For Clojure:**
```elisp
(use-package cider
  :ensure t)
```

**For C++:**
```elisp
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)))
```

**For Julia:**
```elisp
(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))
```

### Phase 2: Add Language-Specific Helpers

I can create additional helper files:
- `slime-helpers.el` - SLIME/SBCL interaction
- `cider-helpers.el` - CIDER/Clojure interaction
- `lsp-helpers.el` - LSP queries
- `julia-helpers.el` - Julia REPL interaction

### Phase 3: Unified Interface

Create a unified programmatic interface that works across languages.

---

## Example: What SLIME Helpers Would Look Like

```elisp
;;; slime-helpers.el --- Helpers for SLIME interaction

(defun claude/slime-eval (code)
  "Evaluate CODE in SLIME REPL and return result."
  (if (slime-connected-p)
      (condition-case err
          (let ((result (slime-eval `(cl:progn ,code))))
            (list :status "ok"
                  :result result
                  :connection (slime-connection-name)))
        (error
         (list :status "error"
               :error (error-message-string err))))
    (list :status "error"
          :message "SLIME not connected")))

(defun claude/slime-info ()
  "Get SLIME connection info."
  (if (slime-connected-p)
      (list :status "ok"
            :connected t
            :connection-name (slime-connection-name)
            :lisp-implementation (slime-lisp-implementation-type)
            :package (slime-current-package))
    (list :status "error"
          :connected nil
          :message "SLIME not connected")))

(defun claude/slime-compile-file (filepath)
  "Compile Lisp file and return results."
  (if (slime-connected-p)
      (condition-case err
          (progn
            (slime-compile-and-load-file filepath)
            (list :status "ok"
                  :file filepath
                  :message "Compilation started"))
        (error
         (list :status "error"
               :file filepath
               :error (error-message-string err))))
    (list :status "error"
          :message "SLIME not connected")))

(provide 'slime-helpers)
```

---

## Example: What CIDER Helpers Would Look Like

```elisp
;;; cider-helpers.el --- Helpers for CIDER interaction

(defun claude/cider-eval (code)
  "Evaluate CODE in CIDER REPL and return result."
  (if (cider-connected-p)
      (condition-case err
          (let ((result (cider-nrepl-sync-request:eval code)))
            (list :status "ok"
                  :result (nrepl-dict-get result "value")
                  :ns (cider-current-ns)))
        (error
         (list :status "error"
               :error (error-message-string err))))
    (list :status "error"
          :message "CIDER not connected")))

(defun claude/cider-info ()
  "Get CIDER connection info."
  (if (cider-connected-p)
      (list :status "ok"
            :connected t
            :host (cider-current-host)
            :port (cider-current-port)
            :ns (cider-current-ns)
            :clojure-version (cider--java-version))
    (list :status "error"
          :connected nil
          :message "CIDER not connected")))

(defun claude/cider-test-ns ()
  "Run tests in current namespace."
  (if (cider-connected-p)
      (condition-case err
          (progn
            (cider-test-run-ns-tests nil)
            (list :status "ok"
                  :message "Tests running"))
        (error
         (list :status "error"
               :error (error-message-string err))))
    (list :status "error"
          :message "CIDER not connected")))

(provide 'cider-helpers)
```

---

## What You Need

### For Each Language You Use

1. **Install the language package** (SLIME, CIDER, lsp-mode, etc.)
2. **Configure it** in your init.el
3. **Add language-specific helpers** (I can create these)
4. **Use unified interface** for programmatic interaction

---

## Current Limitations

### What claude-helpers.el CAN'T do:

❌ Send code to SBCL REPL
❌ Evaluate Clojure expressions in CIDER
❌ Query C++ LSP server for diagnostics
❌ Send code to Julia REPL
❌ Get REPL output from any language
❌ Run language-specific tests
❌ Control language debuggers
❌ Query language-specific state

### What it CAN do:

✅ Manage Emacs itself
✅ Work with buffers and files
✅ Check if language packages are loaded
✅ Verify configuration
✅ General Emacs operations

---

## Recommendations

### Option 1: Minimal (Use What Exists)

**For each language:**
1. Install the standard package (SLIME, CIDER, etc.)
2. Use their built-in commands manually
3. Use current helpers for file/buffer operations

**Pros:** Simple, standard, well-documented
**Cons:** Not programmatic, manual interaction

### Option 2: Enhanced (Add Language Helpers)

**For each language you actively use:**
1. Install the standard package
2. I create language-specific helper files
3. Load helpers alongside current ones
4. Get programmatic access to REPLs

**Pros:** Programmatic, structured, integrated
**Cons:** More setup, maintenance

### Option 3: LSP-First (Modern Approach)

**For most languages:**
1. Use lsp-mode as universal interface
2. Add LSP-specific helpers
3. Supplement with language REPLs where needed

**Pros:** Standardized, modern, multi-language
**Cons:** Not all languages have good LSP support

---

## My Recommendation

### Start with Standard Packages

Install what you need:
```elisp
;; For Common Lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy)))

;; For Clojure
(use-package cider
  :ensure t)

;; For C++, Julia, etc.
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)
         (julia-mode . lsp)))

;; For Julia REPL
(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))
```

### Then Add Language-Specific Helpers

**Tell me which languages you use most**, and I'll create optimized helpers for:
- REPL interaction
- Code evaluation
- Testing
- Debugging
- State inspection

---

## Example Workflow: With vs Without Helpers

### Without Language Helpers (Current)

**Check SLIME connection:**
```bash
emacsclient -e '(boundp '\''slime-net-processes)'
# => t (but no details)
```

**Evaluate Lisp code:**
```
# Can't do this programmatically!
# User must: M-x slime, then type in REPL
```

### With Language Helpers (Proposed)

**Check SLIME connection:**
```bash
emacsclient -e '(claude/slime-info)'
# => (:status "ok" :connected t :connection-name "SBCL" :package "COMMON-LISP-USER")
```

**Evaluate Lisp code:**
```bash
emacsclient -e '(claude/slime-eval "(+ 1 2 3)")'
# => (:status "ok" :result 6 :connection "SBCL")
```

---

## Summary Table

| Language | Package | Current Helpers | Need Additions? |
|----------|---------|----------------|-----------------|
| **Elisp** | Built-in | ✅ Fully supported | No |
| **Common Lisp** | SLIME | ❌ No REPL access | Yes - slime-helpers.el |
| **Clojure** | CIDER | ❌ No REPL access | Yes - cider-helpers.el |
| **C++** | lsp-mode | ❌ No LSP queries | Yes - lsp-helpers.el |
| **Julia** | julia-snail | ❌ No REPL access | Yes - julia-helpers.el |
| **Python** | python-mode | ❌ No REPL access | Yes - python-helpers.el |
| **General** | - | ✅ Files/buffers/config | Already done |

---

## Next Steps

### Option A: Tell Me Your Languages

Let me know which languages you use most, and I'll create:
1. Installation/config guide for that language
2. Language-specific helper functions
3. Integration examples
4. Testing scripts

### Option B: General LSP Helpers

I can create `lsp-helpers.el` that works with any LSP-enabled language:
- Query diagnostics
- Get hover info
- Find definitions
- Get references
- Format code

### Option C: Start with One Language

Pick one language to start with (e.g., Common Lisp + SLIME), and we'll:
1. Install and configure SLIME
2. Create slime-helpers.el
3. Test programmatic interaction
4. Expand to other languages

---

## Conclusion

**Current helpers are NOT sufficient for language-specific REPL workflows.**

They're great for:
- General Emacs operations ✅
- File/buffer management ✅
- Configuration ✅

But you need **language-specific packages and helpers** for:
- REPL interaction ❌
- Code evaluation ❌
- Language-specific state ❌

**Which languages do you want to prioritize?** I'll create the appropriate helpers for you!
