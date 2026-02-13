# Script to add Eglot LSP configuration to init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Check if eglot already configured
if ($content -match "use-package eglot") {
    Write-Host "Eglot already configured in init.el"
    exit 0
}

# Find the insertion point (after flycheck-clj-kondo)
$eglotConfig = @"

;; Eglot - LSP client for Clojure (and other languages)
(use-package eglot
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure))
  :config
  ;; Optional: Add clojure-lsp to server programs if not auto-detected
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurec-mode clojurescript-mode) . ("clojure-lsp")))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l d" . eldoc-doc-buffer)
              ("C-c l q" . eglot-shutdown)))
"@

# Insert after flycheck-clj-kondo
$pattern = "(\(use-package flycheck-clj-kondo\s+:after \(flycheck clojure-mode\)\))"
$replacement = "`$1$eglotConfig"

$newContent = $content -replace $pattern, $replacement

Set-Content -Path $initFile -Value $newContent
Write-Host "Successfully added Eglot configuration to init.el"
