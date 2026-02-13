# Restore ux-config.el with masked-theme
$uxFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\ux-config.el"

$content = @'
;;; ux-config.el --- Modern UX improvements for Emacs

;;; Commentary:
;; This file contains UX enhancements for better discoverability,
;; completion, navigation, and visual appearance.

;;; Code:

;;; Phase 1: Essential - Discoverability & Completion

;; which-key - Show available keybindings after prefix keys
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)  ; Show popup after 0.5 seconds
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom))

;; Vertico - Vertical completion interface
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ; Cycle through candidates
  (vertico-count 15))  ; Show 15 candidates

;; Orderless - Flexible completion style with space-separated patterns
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - Rich annotations in completion buffer
(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Corfu - In-buffer completion popup (like IDE autocomplete)
(use-package corfu
  :custom
  (corfu-auto t)                      ; Enable automatic completion
  (corfu-auto-delay 0.2)              ; Show after 0.2 seconds
  (corfu-auto-prefix 2)               ; Minimum prefix length
  (corfu-cycle t)                     ; Cycle through candidates
  (corfu-quit-no-match 'separator)    ; Don't quit if no match
  (corfu-preview-current nil)         ; Don't preview current candidate
  :init
  (global-corfu-mode))

;; Savehist - Persist minibuffer history across sessions
(use-package savehist
  :init
  (savehist-mode))

;;; Phase 2: Recommended - Navigation & Tools

;; Magit - Full-featured Git interface
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Consult - Enhanced navigation and search commands
(use-package consult
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; M-s bindings (search-map)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s g" . consult-ripgrep)
         ("M-s G" . consult-git-grep)
         ("M-s f" . consult-find)
         ("M-s i" . consult-imenu)
         ("M-s I" . consult-imenu-multi)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; Other bindings
         ("M-y" . consult-yank-pop))
  :custom
  (consult-narrow-key "<")  ; Use < for narrowing
  (consult-preview-key 'any))  ; Preview immediately

;;; Phase 3: Visual Theme

;; masked-theme - Dark theme with muted colors and low contrast
(use-package masked-theme
  :straight (:host github :repo "itix-enoks/masked-theme")
  :config
  (load-theme 'masked t))

;;; Phase 4: Mouse-Friendly UI Features

;; Tier 1: Essential Built-in Features

;; window-divider-mode - Draggable dividers between windows
(window-divider-mode 1)
(setq window-divider-default-places t)       ; Show dividers everywhere
(setq window-divider-default-bottom-width 2) ; 2px bottom divider
(setq window-divider-default-right-width 2)  ; 2px right divider

;; context-menu-mode - Right-click context menus
(context-menu-mode 1)

;; winner-mode - Undo/redo window layout changes with C-c left/right
(winner-mode 1)

;; ibuffer - Better buffer list with grouping
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)  ; Don't ask for confirmation on delete

;; Tier 2: Recommended Enhancements

;; ace-window - Visual window switching with letter overlays
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; Home row keys
  (aw-dispatch-always t))                    ; Always show dispatch menu

(provide 'ux-config)
;;; ux-config.el ends here
'@

Set-Content -Path $uxFile -Value $content -NoNewline
Write-Host "Successfully restored ux-config.el with masked-theme"
