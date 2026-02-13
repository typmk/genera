# Add masked-theme to ux-config.el
$uxFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\ux-config.el"
$content = Get-Content $uxFile -Raw

# Add masked-theme configuration before the mouse-friendly section
$themeConfig = @'
;;; Phase 3: Visual Theme

;; masked-theme - Dark theme with muted colors and low contrast
(use-package masked-theme
  :straight (:host github :repo "itix-enoks/masked-theme")
  :config
  (load-theme 'masked t))

'@

# Insert before ";;; Phase 3: Mouse-Friendly UI Features"
$pattern = ';;; Phase 3: Mouse-Friendly UI Features'
$replacement = $themeConfig + ';;; Phase 4: Mouse-Friendly UI Features'

$content = $content -replace [regex]::Escape($pattern), $replacement

Set-Content -Path $uxFile -Value $content -NoNewline
Write-Host "Successfully added masked-theme to ux-config.el"
