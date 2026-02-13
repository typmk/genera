# Script to add eglot-booster to init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Check if already configured
if ($content -match "eglot-booster") {
    Write-Host "eglot-booster already configured"
    exit 0
}

# Add eglot-booster after eglot configuration
$boosterConfig = @"

;; Eglot-booster - Performance boost for Eglot via emacs-lsp-booster
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  ;; For Emacs 30+: use I/O-only mode since native JSON parser is already fast
  ;; This disables bytecode conversion but keeps async I/O benefits
  (setq eglot-booster-io-only t))
"@

# Find and replace using simpler pattern
$pattern = ";; Common Lisp Development Configuration \(SLIME \+ SBCL\)"
$replacement = "$boosterConfig`n`n;; Common Lisp Development Configuration (SLIME + SBCL)"

$newContent = $content -replace $pattern, $replacement

Set-Content -Path $initFile -Value $newContent -NoNewline
Write-Host "Successfully added eglot-booster to init.el"
