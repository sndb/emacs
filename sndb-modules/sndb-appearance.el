;;;; Theme
(require 'modus-themes)

(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)
(setq modus-themes-common-palette-overrides
      '((comment red-faint)
        (string green-cooler)
        (fringe unspecified)
        (bg-mode-line-active bg-blue-subtle)
        (bg-mode-line-inactive bg-dim)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

(defun sndb-switch-theme (arg)
  "Toggle between the light and dark themes.
With prefix argument select a theme using minibuffer completion."
  (interactive "P")
  (call-interactively
   (if arg
       #'modus-themes-select
     #'modus-themes-toggle)))

(keymap-global-set "<f5>" #'sndb-switch-theme)

(set-face-attribute 'default nil :family "Hack" :height 105)
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

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Tabs
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-show 1)
(tab-bar-mode 1)

(provide 'sndb-appearance)
