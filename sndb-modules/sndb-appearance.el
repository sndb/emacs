;;;; Fonts
(let ((family "Hack") (height 130))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :family family :height height)))

;;;; Theme
(require 'modus-themes)

(setq modus-themes-common-palette-overrides
      '((fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)

        (type fg-alt)
        (keyword fg-alt)
        (builtin fg-alt)
        (preprocessor fg-alt)

        (string blue-faint)
        (docstring yellow-faint)
        (comment yellow-faint)

        (constant fg-main)
        (fnname fg-main)
        (variable fg-main)
        (property fg-main)))

(load-theme 'modus-vivendi :no-confirm)

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)
(setq ring-bell-function #'ignore)

(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)

;;;; Parentheses
(setq show-paren-when-point-in-periphery t)
(setq show-paren-context-when-offscreen 'child-frame)
(show-paren-mode 1)

;;;; Whitespace
(require 'whitespace)

(setq-default indicate-empty-lines t)
(setq whitespace-style '(face tabs trailing tab-mark))
(setq whitespace-global-modes '(not magit-mode go-ts-mode odin-mode))
(add-to-list 'whitespace-display-mappings '(tab-mark 9 [8250 9]) t)

(global-whitespace-mode 1)

(keymap-global-set "C-c f" #'delete-trailing-whitespace)

;;;; Frame
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;;;; Tabs
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-show 1)
(tab-bar-mode 1)

(provide 'sndb-appearance)
