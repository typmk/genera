# Hide tool-bar and menu-bar
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# Add UI cleanup settings after UTF-8 settings
$uiSettings = @'

;; Clean UI - Hide tool-bar and menu-bar
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Optional: Also hide scroll-bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

'@

# Insert after the keyboard-coding-system line
$pattern = "(set-keyboard-coding-system 'utf-8)"
$replacement = "(set-keyboard-coding-system 'utf-8)" + $uiSettings

$content = $content -replace [regex]::Escape($pattern), $replacement

Set-Content -Path $initFile -Value $content -NoNewline
Write-Host "Successfully added UI cleanup settings to init.el"
Write-Host "Tool-bar, menu-bar, and scroll-bar will be hidden"
