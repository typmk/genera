# Add UTF-8 settings to init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Add UTF-8 settings after straight.el setup
$utf8Settings = @'

;; Set UTF-8 as default encoding everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

'@

# Insert after the straight-use-package-by-default line
$pattern = "(setq straight-use-package-by-default t)"
$replacement = "(setq straight-use-package-by-default t)" + $utf8Settings

$content = $content -replace [regex]::Escape($pattern), $replacement

Set-Content -Path $initFile -Value $content -NoNewline
Write-Host "Successfully added UTF-8 settings to init.el"
