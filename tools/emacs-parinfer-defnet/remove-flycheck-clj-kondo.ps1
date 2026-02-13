# Script to remove flycheck-clj-kondo from init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Remove flycheck-clj-kondo and replace with explanatory comment
$pattern = "\(use-package flycheck-clj-kondo\s+:after \(flycheck clojure-mode\)\)\r?\n"
$replacement = @"
;; Note: flycheck-clj-kondo removed to avoid redundancy with clojure-lsp
;; clojure-lsp bundles clj-kondo and provides linting via Eglot/Flymake

"@

$newContent = $content -replace $pattern, $replacement

Set-Content -Path $initFile -Value $newContent -NoNewline
Write-Host "Successfully removed flycheck-clj-kondo from init.el"
