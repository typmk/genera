# Script to update init.el for UX improvements
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"
$content = Get-Content $initFile -Raw

# 1. Fix flycheck - change from global to mode-specific
$flycheckOld = "(use-package flycheck`n  :config`n  (global-flycheck-mode))"
$flycheckNew = @'
(use-package flycheck
  :hook ((emacs-lisp-mode . flycheck-mode)
         (lisp-mode . flycheck-mode))
  :config
  ;; Note: Clojure uses eglot/flymake, not flycheck
  ;; This avoids conflicts between flycheck and flymake
  (setq flycheck-mode-line-prefix "FlyC"))
'@
$content = $content -replace [regex]::Escape($flycheckOld), $flycheckNew

# 2. Add CIDER enrich-classpath
$ciderPattern = '(\(use-package cider\r?\n  :after clojure-mode\r?\n  :config)'
$ciderReplacement = @'
(use-package cider
  :after clojure-mode
  :custom
  ;; Enable better Java interop documentation
  (cider-enrich-classpath t)
  :config
'@
$content = $content -replace $ciderPattern, $ciderReplacement

# 3. Add ux-config.el loader before (provide 'init)
$uxLoader = @'

;; Load UX enhancements (completion, navigation, visual improvements)
(let ((ux-config (expand-file-name "ux-config.el" user-emacs-directory)))
  (when (file-exists-p ux-config)
    (load-file ux-config)
    (message "UX configuration loaded")))

'@
$content = $content -replace '\(provide ''init\)', ($uxLoader + "(provide 'init)")

Set-Content -Path $initFile -Value $content -NoNewline
Write-Host "Successfully updated init.el"
Write-Host "Changes made:"
Write-Host "  1. Fixed flycheck conflict (mode-specific instead of global)"
Write-Host "  2. Enabled CIDER enrich-classpath"
Write-Host "  3. Added ux-config.el loader"
