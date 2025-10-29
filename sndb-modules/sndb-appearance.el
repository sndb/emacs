;;;; Theme
(require 'modus-themes)

(setq modus-themes-common-palette-overrides
      '((fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
        (bg-line-number-active unspecified)
        (bg-line-number-inactive unspecified)))
(setq modus-vivendi-palette-overrides
      '((bg-mode-line-active bg-lavender)))
(setq modus-operandi-palette-overrides
      '((bg-mode-line-active bg-blue-intense)))

(modus-themes-select 'modus-vivendi)

;;;; Fonts
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 135)
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)
(setq ring-bell-function #'ignore)
(setq shell-command-prompt-show-cwd t)
(setq-default truncate-lines t)
(setq-default fill-column 80)

(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)

;;;; Line Numbers
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
(setq-default display-line-numbers-widen t)

;;;; Parentheses
(setq show-paren-when-point-in-periphery t)
(setq show-paren-context-when-offscreen 'child-frame)

;;;; Whitespace
(require 'whitespace)

(setq whitespace-style '(face tabs trailing tab-mark))
(setq whitespace-global-modes '(not magit-mode go-ts-mode odin-mode))
(add-to-list 'whitespace-display-mappings '(tab-mark ?\t [?› ?\t]) t)

(global-whitespace-mode 1)

;;;; Format
(defun sndb-format-buffer ()
  "Format the current buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(keymap-global-set "C-c f" #'sndb-format-buffer)

;;;; Interface
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
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

;;;; Keymap
(defvar-keymap sndb-visual-map
  :doc "Keymap for appearance commands."
  "m" #'modus-themes-select
  "d" #'doric-themes-select
  "w" #'whitespace-mode
  "l" #'display-line-numbers-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "o" #'olivetti-mode)

(keymap-global-set "C-c v" sndb-visual-map)

(provide 'sndb-appearance)
