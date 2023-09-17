;;;; Theme
(require 'modus-themes)

(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-common-palette-overrides
      '((comment red-faint)
        (string green-cooler)
        (fringe unspecified)))

(global-set-key (kbd "<f5>") #'modus-themes-toggle)
(load-theme 'modus-vivendi)

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq mode-line-compact 'long)
(setq visible-bell t)
(setq use-dialog-box nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)
(setq column-number-indicator-zero-based nil)
(setq-default indicate-empty-lines t)

(blink-cursor-mode -1)
(display-time-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode -1)

(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

;;;; Parentheses
(setq show-paren-delay 0)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-context-when-offscreen 'child-frame)
(show-paren-mode 1)

;;;; Highlight keywords
(require 'hl-todo)
(global-hl-todo-mode 1)

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Tabs
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(tab-bar-mode 1)

(provide 'sndb-appearance)
