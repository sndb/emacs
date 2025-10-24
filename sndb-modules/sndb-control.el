;;;; Control
(setq help-window-select t)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq view-read-only t)
(setq save-interprogram-paste-before-kill t)
(setq set-mark-command-repeat-pop t)

(global-auto-revert-mode 1)
(delete-selection-mode 1)
(repeat-mode 1)

;;;; Commands
(put #'narrow-to-region 'disabled nil)
(put #'overwrite-mode 'disabled t)
(put #'suspend-frame 'disabled t)

(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "M-/" #'hippie-expand)

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
  :doc "Keymap to repeat `sndb-delete-window'."
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

;;;; Duplicate
(setq duplicate-line-final-position 1)
(setq duplicate-region-final-position 1)
(keymap-global-set "C-M-y" #'duplicate-dwim)

;;;; Move Text
(require 'move-text)
(keymap-global-set "C-M-S-n" #'move-text-down)
(keymap-global-set "C-M-S-p" #'move-text-up)

;;;; Mouse
(setq mouse-wheel-progressive-speed nil)
(setq mouse-yank-at-point t)
(setq mouse-autoselect-window t)

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
(setq isearch-allow-motion t)
(setq isearch-wrap-pause nil)
(setq isearch-repeat-on-direction-change t)

(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)

(provide 'sndb-control)
