# Remove tab-line-mode and treemacs from ux-config.el
$uxFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\ux-config.el"
$content = Get-Content $uxFile -Raw

# Remove tab-line-mode section
$tabLinePattern = @'
;; tab-line-mode - Buffer tabs per window
(global-tab-line-mode 1)
(setq tab-line-close-button-show t)          ; Show close buttons on tabs
(setq tab-line-new-button-show nil)          ; Hide new tab button

'@
$content = $content -replace [regex]::Escape($tabLinePattern), ''

# Remove treemacs section
$treemacsPattern = @'
;; treemacs - File explorer sidebar
(use-package treemacs
  :bind (("C-c t" . treemacs)
         ("C-c f" . treemacs-select-window))
  :custom
  (treemacs-width 30)
  (treemacs-follow-mode t)           ; Auto-follow current file
  (treemacs-filewatch-mode t)        ; Auto-refresh on file changes
  (treemacs-git-mode 'simple))       ; Show git status

'@
$content = $content -replace [regex]::Escape($treemacsPattern), ''

Set-Content -Path $uxFile -Value $content -NoNewline
Write-Host "Successfully removed tab-line-mode and treemacs from ux-config.el"
