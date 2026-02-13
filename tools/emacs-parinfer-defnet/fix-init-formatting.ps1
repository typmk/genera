# Script to fix formatting in init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Fix the merged comment line by adding a blank line
$pattern = ";; clojure-lsp bundles clj-kondo and provides linting via Eglot/Flymake\r?\n;; Eglot - LSP client"
$replacement = ";; clojure-lsp bundles clj-kondo and provides linting via Eglot/Flymake`n`n;; Eglot - LSP client"

$newContent = $content -replace $pattern, $replacement

# Remove extra blank lines at the end
$newContent = $newContent.TrimEnd() + "`n"

Set-Content -Path $initFile -Value $newContent -NoNewline
Write-Host "Successfully fixed init.el formatting"
