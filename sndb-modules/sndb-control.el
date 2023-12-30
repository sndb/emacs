;;;; Bindings

;; Enable commands
(dolist (c '(narrow-to-region))
  (put c 'disabled nil))

;; Disable commands
(dolist (c '(overwrite-mode suspend-frame))
  (put c 'disabled t))

;; Remap commands
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-M-y" #'duplicate-dwim)

;;;; Buffers
(defun sndb-kill-buffer ()
  "Kill the current buffer and close its window."
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(keymap-global-set "C-x k" #'sndb-kill-buffer)

;;;; Windows
(defun sndb-previous-window ()
  "Select previous window in cyclic ordering of windows."
  (interactive)
  (other-window -1))

(defun sndb-close ()
  "Delete the current window.
Close the current tab if that was its only window."
  (interactive)
  (if (one-window-p)
      (tab-close)
    (delete-window)))

(setq help-window-select t)

(defvar-keymap sndb-close-map
  :repeat t
  "0" #'sndb-close)

(keymap-global-set "C-x !" #'delete-other-windows-vertically)
(keymap-global-set "C-;" #'other-window)
(keymap-global-set "C-:" #'sndb-previous-window)
(keymap-global-set "C-x 0" #'sndb-close)

(setq display-buffer-alist
      `((,(regexp-quote shell-command-buffer-name-async)
         display-buffer-no-window)))

;;;; Lines
(defun sndb-backward-kill-line ()
  "Kill the rest of the current line backward."
  (interactive)
  (kill-line -1))

(setq delete-trailing-lines nil)
(setq require-final-newline t)

(keymap-global-set "C-S-k" #'sndb-backward-kill-line)

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
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll t)
(setq isearch-repeat-on-direction-change t)

(add-hook 'occur-mode-hook #'hl-line-mode)

;;;; Prefix
(defvar-keymap sndb-prefix-map
  :repeat t
  "n" #'next-buffer
  "p" #'previous-buffer)

(keymap-global-set "C-z" sndb-prefix-map)

(provide 'sndb-control)
