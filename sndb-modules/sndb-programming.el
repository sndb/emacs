;;;; Common
(setq default-input-method "TeX")
(setq delete-trailing-lines nil)
(setq display-raw-bytes-as-hex t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)

;;;; C
(setq c-default-style "linux")
(setq comment-style 'extra-line)

(add-hook 'c-mode-common-hook #'indent-tabs-mode)

;;;; Shell
(setq sh-basic-offset 8)

(add-hook 'sh-mode-hook #'indent-tabs-mode)

;;;; SQL
(setq sql-product 'sqlite)

(add-hook 'sql-mode-hook
          (lambda ()
            (setq sndb-auto-format-function #'format-all-buffer)
            (setq format-all-formatters
                  '(("SQL" (pgformatter
                            "--function-case" "2"
                            "--keyword-case" "2"
                            "--type-case" "2"
                            "--no-extra-line"))))))

;;;; Web
(setq css-indent-offset 2)
(setq js-indent-level 2)

;;;; Clojure
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq sndb-auto-format-function #'cider-format-buffer)))

;;;; Scheme
(require 'geiser)
(require 'geiser-guile)

(defun scheme-add-keywords (face-name keyword-rules)
  (let* ((keyword-list
          (mapcar (lambda (x)
                    (symbol-name (cdr x)))
                  keyword-rules))
         (keyword-regexp
          (concat "(" (regexp-opt keyword-list t) "\\>")))
    (font-lock-add-keywords
     'scheme-mode
     `((,keyword-regexp 1 ',face-name))))
  (mapc (lambda (x)
          (put (cdr x)
               'scheme-indent-function
               (car x)))
        keyword-rules))

(scheme-add-keywords
 'font-lock-keyword-face
 '((1 . letcc)
   (1 . try)))

(setq geiser-repl-history-filename (concat user-emacs-directory "geiser-history"))

;;;; Racket
(add-hook 'racket-before-run-hook #'racket-repl-clear)

;;;; Text
(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

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

(global-set-key (kbd "C-c u") #'sndb-insert-underline)

;;;; Puni
(require 'puni)

(setq puni-confirm-when-delete-unbalanced-active-region nil)

(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                clojure-mode-hook
                cider-repl-mode-hook
                scheme-mode-hook
                geiser-repl-mode-hook
                racket-mode-hook
                racket-repl-mode-hook))
  (add-hook hook #'puni-mode)
  (add-hook hook #'electric-pair-local-mode))

(defvar sndb-puni-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r") #'puni-raise)
    (define-key map (kbd "C-s") #'puni-splice)
    (define-key map (kbd "C-q") #'puni-squeeze)
    (define-key map (kbd "C-c") #'puni-split)
    (define-key map (kbd "C-v") #'puni-convolute)
    map))

(define-key puni-mode-map (kbd "C-'") sndb-puni-mode-map)
(define-key puni-mode-map (kbd "C-=") #'puni-expand-region)
(define-key puni-mode-map (kbd "C-)") #'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-(") #'puni-slurp-backward)
(define-key puni-mode-map (kbd "C-}") #'puni-barf-forward)
(define-key puni-mode-map (kbd "C-{") #'puni-barf-backward)

;;;; Subword
(global-subword-mode 1)

;;;; Eglot
(require 'eglot)

(dolist (hook '(c-mode-hook
                go-mode-hook
                python-mode-hook
                racket-mode-hook
                sh-mode-hook))
  (add-hook hook #'eglot-ensure))

(define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c t") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-h .") #'eldoc-box-eglot-help-at-point)

;;;; Flymake
(require 'flymake)

(setq flymake-suppress-zero-counters t)
(setq flymake-wrap-around nil)

(define-key flymake-mode-map (kbd "C-c d d") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "C-c d D") #'flymake-show-project-diagnostics)
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

(provide 'sndb-programming)
