;;;; Common
(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)

;;;; Fonts
(let ((family "mononoki") (height 110))
  (set-face-attribute 'default nil :family family :height height)
  (set-face-attribute 'fixed-pitch nil :family family :height height))

;;;; Theme
(require 'ef-themes)

(defun sndb-theme-variant ()
  "Return `light' or `dark' based on the current time of day."
  (let ((hour (decoded-time-hour (decode-time (current-time)))))
    (if (<= 6 hour 18) 'light 'dark)))

(defun sndb-random-theme ()
  "Load a random theme based on the current time of day."
  (interactive)
  (ef-themes-load-random (sndb-theme-variant)))

(defun sndb-select-theme ()
  "Select a theme based on the current time of day."
  (interactive)
  (call-interactively
   (if (eq (sndb-theme-variant) 'dark)
       #'ef-themes-select-dark
     #'ef-themes-select-light)))

(keymap-global-set "<f5>" #'sndb-random-theme)
(keymap-global-set "C-<f5>" #'sndb-select-theme)

(sndb-random-theme)

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
