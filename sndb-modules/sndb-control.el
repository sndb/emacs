;;;; Commands
(dolist (c '(narrow-to-region))
  (put c 'disabled nil))

(dolist (c '(overwrite-mode suspend-frame))
  (put c 'disabled t))

(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)

;;;; Windows
(defun sndb-other-window ()
  "Select next window in cyclic ordering of windows.
Split the frame if there is a single window."
  (interactive)
  (when (one-window-p)
    (split-window-sensibly))
  (other-window 1))

(defun sndb-previous-window ()
  "Select previous window in cyclic ordering of windows."
  (interactive)
  (other-window -1))

(defun sndb-kill-buffer ()
  "Kill the current buffer and close its window."
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun sndb-delete-window ()
  "Delete the current window.
Close the current tab if that was its only window."
  (interactive)
  (if (one-window-p)
      (tab-close)
    (delete-window)))

(defvar-keymap sndb-delete-window-map
  :repeat t
  "0" #'sndb-delete-window)

(keymap-global-set "C-;" #'sndb-other-window)
(keymap-global-set "C-:" #'sndb-previous-window)
(keymap-global-set "C-x k" #'sndb-kill-buffer)
(keymap-global-set "C-x 0" #'sndb-delete-window)

(setq display-buffer-alist
      '(((or "\\*Async Shell Command\\*"
             "\\*Warnings\\*")
         display-buffer-no-window
         (allow-no-window . t))
        ("\\*eldoc\\*"
         display-buffer-below-selected
         (window-height . shrink-window-if-larger-than-buffer)
         (body-function . select-window))))

(setq help-window-select t)

;;;; Lines
(setq require-final-newline t)
(setq-default truncate-lines t)

;;;; Duplicate
(setq duplicate-line-final-position 1)
(setq duplicate-region-final-position 1)
(keymap-global-set "C-M-y" #'duplicate-dwim)

;;;; Move Text
(require 'move-text)
(keymap-global-set "C-M-S-n" #'move-text-down)
(keymap-global-set "C-M-S-p" #'move-text-up)

;;;; Read-Only
(setq view-read-only t)

;;;; Clipboard
(setq save-interprogram-paste-before-kill t)

;;;; Auto-Revert
(global-auto-revert-mode 1)

;;;; Delete selection
(delete-selection-mode 1)

;;;; Repeat
(setq set-mark-command-repeat-pop t)
(repeat-mode 1)

;;;; Mouse
(setq mouse-wheel-progressive-speed nil)
(setq mouse-yank-at-point t)
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)

;;;; Scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 1)
(setq scroll-margin 2)

(setq sndb-scroll-delta 8)

(defun sndb-scroll-up ()
  "Scroll a few lines up."
  (interactive)
  (scroll-up sndb-scroll-delta))

(defun sndb-scroll-down ()
  "Scroll a few lines down."
  (interactive)
  (scroll-down sndb-scroll-delta))

(keymap-global-set "C-S-n" #'sndb-scroll-up)
(keymap-global-set "C-S-p" #'sndb-scroll-down)

(put #'sndb-scroll-up 'isearch-scroll t)
(put #'sndb-scroll-down 'isearch-scroll t)

;;;; Search
(setq isearch-lazy-count t)
(setq isearch-yank-on-move t)
(setq isearch-allow-scroll t)
(setq isearch-repeat-on-direction-change t)

(provide 'sndb-control)
