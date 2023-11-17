;;;; Theme
(require 'modus-themes)

(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)
(setq modus-themes-common-palette-overrides
      '((comment red-faint)
        (string green-cooler)
        (fringe unspecified)
        (bg-mode-line-active bg-active)
        (bg-mode-line-inactive bg-dim)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

(global-set-key (kbd "<f5>") #'modus-themes-toggle)
(load-theme 'modus-vivendi)

;;;; Breadcrumb
(require 'breadcrumb)
(breadcrumb-mode 1)

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)

(blink-cursor-mode -1)
(show-paren-mode 1)

(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Tabs
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(tab-bar-mode 1)

(provide 'sndb-appearance)
