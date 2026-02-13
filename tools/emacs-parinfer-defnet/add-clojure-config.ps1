# Script to add Clojure configuration loading to init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Check if already added
if ($content -notmatch "clojure-config\.el") {
    # Add the loader before (provide 'init)
    $loaderCode = @"

;; Load Clojure development configuration
(let ((clojure-config (expand-file-name "clojure-config.el" user-emacs-directory)))
  (when (file-exists-p clojure-config)
    (load-file clojure-config)
    (message "Clojure development configuration loaded")))
"@

    $content = $content -replace '\(provide ''init\)', ($loaderCode + "`n`n(provide 'init)")
    Set-Content -Path $initFile -Value $content -NoNewline
    Write-Host "Successfully added Clojure configuration loader to init.el"
} else {
    Write-Host "Clojure configuration loader already present in init.el"
}
