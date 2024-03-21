;;;; Fonts
(set-face-attribute 'default nil :family "Hack" :height 105)
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 105)
(set-face-attribute 'variable-pitch nil :family "Liberation Sans" :height 105)

;;;; Theme
(require 'modus-themes)

(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)
(setq modus-themes-italic-constructs t)
(setq modus-themes-common-palette-overrides
      '((docstring yellow-faint)
        (string yellow-faint)

        (keyword fg-alt)
        (type fg-alt)

        (builtin fg-main)
        (constant fg-main)
        (fnname fg-main)
        (preprocessor fg-main)
        (variable fg-main)

        (fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

(keymap-global-set "<f5>" #'modus-themes-toggle)
(load-theme 'modus-vivendi)

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)

(blink-cursor-mode -1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t))))

(keymap-global-set "C-c w" #'whitespace-mode)

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Tabs
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-show 1)
(tab-bar-mode 1)

(provide 'sndb-appearance)
