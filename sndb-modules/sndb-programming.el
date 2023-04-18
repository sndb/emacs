;;;; C
(setq c-default-style "linux")
(setq comment-style 'extra-line)
(add-hook 'c-mode-common-hook #'indent-tabs-mode)

;;;; Go
(add-hook 'before-save-hook #'gofmt-before-save)

;;;; SQL
(setq sql-product 'sqlite)
(add-hook 'sql-mode-hook
          (lambda ()
            (setq format-all-formatters
                  '(("SQL" (pgformatter
                            "--function-case" "2"
                            "--keyword-case" "2"
                            "--type-case" "2"
                            "--no-extra-line"
                            "--tabs"))))))

;;;; Web
(setq css-indent-offset 2)
(setq js-indent-level 2)

;;;; Scheme
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

;;;; Paredit
(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook
                racket-mode-hook
                scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))

;;;; Eglot
(require 'eglot)
(require 'go-mode)

(dolist (hook '(c-mode-hook
                clojure-mode-hook
                go-mode-hook
                python-mode-hook
                racket-mode-hook
                sh-mode-hook))
  (add-hook hook #'eglot-ensure))

(define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c t") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-h .") #'eldoc-box-eglot-help-at-point)

(provide 'sndb-programming)
