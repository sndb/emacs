;;;; Theme
(require 'modus-themes)

(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-common-palette-overrides
      '((comment red-faint)
        (string green-cooler)
        (fringe unspecified)))

(global-set-key (kbd "<f5>") #'modus-themes-toggle)
(load-theme 'modus-vivendi)

;;;; Fonts
(setq text-scale-mode-step 1.1)
(mapc (lambda (face)
        (set-face-attribute face nil :family "Fira Mono" :height 105))
      '(default fixed-pitch))

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq mode-line-compact 'long)
(setq visible-bell t)
(setq use-dialog-box nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq uniquify-buffer-name-style 'forward)
(setq column-number-indicator-zero-based nil)
(setq-default indicate-empty-lines t)

(blink-cursor-mode -1)
(display-time-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode -1)

(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

;;;; Parentheses
(setq show-paren-delay 0)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-context-when-offscreen 'child-frame)
(show-paren-mode 1)

;;;; Highlight keywords
(require 'hl-todo)
(global-hl-todo-mode 1)

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;;;; Tabs
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(tab-bar-mode 1)

;;;; Format
(require 'format-all)

(setq default-input-method "TeX")
(setq delete-trailing-lines nil)
(setq display-raw-bytes-as-hex t)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(defun sndb-format-buffer ()
  "Auto-format the source code in the current buffer."
  (interactive)
  (if (eglot-managed-p)
      (eglot-format)
    (format-all-buffer t)))

(defun sndb-indent-buffer ()
  "Indent the current buffer and delete trailing whitespace."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun sndb-replace-untypable-characters ()
  "Replace the characters that are inconvenient to type."
  (interactive)
  (save-excursion
    (dolist (pair
             '(("‘" . "'")
               ("’" . "'")
               ("“" . "\"")
               ("”" . "\"")
               ("—" . " - ")
               ("ﬃ" . "ffi")
               ("ﬁ" . "fi")
               ("ﬀ" . "ff")))
      (replace-string (car pair) (cdr pair) nil (point-min) (point-max)))))

(defun sndb-insert-underline (arg)
  "Insert an underline.
Use the character '-' by default.
If called with a prefix argument, use the provided character."
  (interactive "P")
  (if arg
      (call-interactively #'sndb-insert-underline-char)
    (sndb-insert-underline-char ?-)))

(defun sndb-insert-underline-char (c)
  "Insert an underline of the length of the previous line.
If the length of the previous line is 0, use the value of `fill-column'."
  (interactive "cType a character: ")
  (let* ((previous-line-length
          (save-excursion
            (forward-line -1)
            (- (line-end-position) (line-beginning-position))))
         (underline-length
          (if (zerop previous-line-length) fill-column previous-line-length))
         (target-length
          (max 0 (- underline-length (current-column)))))
    (insert (make-string target-length c))))

(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c f") #'sndb-format-buffer)
(global-set-key (kbd "C-c i") #'sndb-indent-buffer)
(global-set-key (kbd "C-c u") #'sndb-insert-underline)

(provide 'sndb-appearance)
