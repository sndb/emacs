;;;; Bindings
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "C-x C-c C-c") #'save-buffers-kill-emacs)
(global-set-key [remap zap-to-char] #'zap-up-to-char)
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)
(global-set-key [remap list-buffers] #'ibuffer)

(setq disabled-command-function nil)

;;;; Windows
(defun prev-window ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(global-unset-key (kbd "C-x o"))

(global-set-key [remap balance-windows] #'balance-windows-area)
(global-set-key (kbd "C-x !") #'delete-other-windows-vertically)
(global-set-key (kbd "C-;") #'other-window)
(global-set-key (kbd "C-'") #'prev-window)

;;;; Read-Only
(setq view-read-only t)

;;;; Clipboard
(setq save-interprogram-paste-before-kill t)

;;;; Auto-Revert
(require 'autorevert)
(global-auto-revert-mode 1)

;;;; Repeat
(setq set-mark-command-repeat-pop t)
(repeat-mode 1)

;;;; Mouse
(setq mouse-wheel-progressive-speed nil)
(setq mouse-yank-at-point t)

;;;; Scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 1)

(defun sndb-scroll-up ()
  "Scroll a few lines up."
  (interactive)
  (scroll-up 4))

(defun sndb-scroll-down ()
  "Scroll a few lines down."
  (interactive)
  (scroll-down 4))

(global-set-key (kbd "C-S-n") #'sndb-scroll-up)
(global-set-key (kbd "C-S-p") #'sndb-scroll-down)

(provide 'sndb-control)
