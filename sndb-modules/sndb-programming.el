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
(require 'geiser)

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

;;;; Clojure
(setq cider-allow-jack-in-without-project t)

;;;; Puni
(require 'puni)

(setq puni-confirm-when-delete-unbalanced-active-region nil)

(dolist (hook '(prog-mode-hook
                text-mode-hook
                eval-expression-minibuffer-setup-hook
                cider-repl-mode-hook
                geiser-repl-mode-hook
                racket-repl-mode-hook))
  (add-hook hook #'puni-mode))

(electric-pair-mode 1)

(define-key puni-mode-map (kbd "C-)") #'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-(") #'puni-slurp-backward)
(define-key puni-mode-map (kbd "C-}") #'puni-barf-forward)
(define-key puni-mode-map (kbd "C-{") #'puni-barf-backward)
(define-key puni-mode-map (kbd "C-' r") #'puni-raise)
(define-key puni-mode-map (kbd "C-' s") #'puni-splice)
(define-key puni-mode-map (kbd "C-' C-s") #'puni-squeeze)
(define-key puni-mode-map (kbd "C-' c") #'puni-convolute)
(define-key puni-mode-map (kbd "C-' C-c") #'puni-split)

;;;; Subword
(global-subword-mode 1)

;;;; Eglot
(require 'eglot)

(dolist (hook '(c-mode-hook
                go-mode-hook
                python-mode-hook
                clojure-mode-hook
                racket-mode-hook
                sh-mode-hook
                html-mode-hook
                css-mode-hook
                js-mode-hook))
  (add-hook hook #'eglot-ensure))

(define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c t") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-h .") #'eldoc-box-eglot-help-at-point)

(provide 'sndb-programming)
