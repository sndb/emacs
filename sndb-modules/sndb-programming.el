;;;; Common
(setq default-input-method "russian-computer")
(setq display-raw-bytes-as-hex t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

(global-subword-mode 1)
(electric-indent-mode -1)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)

(keymap-global-set "C-c m" #'woman)

;;;; C
(require 'c-ts-mode)
(setq c-ts-mode-indent-offset 8)
(setq c-ts-mode-indent-style 'linux)

;;;; Go
(require 'go-ts-mode)

;;;; Python
(require 'python)

;;;; Lua
(require 'lua-ts-mode)

;;;; YAML
(require 'yaml-ts-mode)

;;;; Shell
(require 'sh-script)
(setq sh-basic-offset 8)
(add-hook 'sh-base-mode-hook #'indent-tabs-mode)

;;;; Web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist '(("go" . "\\.tmpl\\'")))

;;;; SQL
(require 'sql)
(setq sql-product 'postgres)

;;;; Clojure
(require 'clojure-mode)

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
 '((1 . let/cc)
   (1 . try)
   (2 . juxt)
   (2 . for)
   (2 . trace-let)))

(setq geiser-repl-history-filename (concat user-emacs-directory "geiser-history"))

;;;; Text
(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(defun sndb-replace-inconvenient-characters ()
  "Replace all inconvenient characters in the current buffer."
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

(keymap-global-set "C-c u" #'sndb-insert-underline)

;;;; Puni
(require 'puni)

(setq puni-confirm-when-delete-unbalanced-active-region nil)

(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                clojure-mode-hook
                cider-repl-mode-hook
                scheme-mode-hook
                geiser-repl-mode-hook))
  (add-hook hook #'electric-pair-local-mode)
  (add-hook hook #'puni-mode))

(defvar-keymap sndb-puni-mode-map
  :repeat t
  "C-r" #'puni-raise
  "C-s" #'puni-splice
  "C-q" #'puni-squeeze
  "C-c" #'puni-split
  "C-v" #'puni-convolute)

(keymap-set puni-mode-map "C-'" sndb-puni-mode-map)
(keymap-set puni-mode-map "C-)" #'puni-slurp-forward)
(keymap-set puni-mode-map "C-(" #'puni-slurp-backward)
(keymap-set puni-mode-map "C-}" #'puni-barf-forward)
(keymap-set puni-mode-map "C-{" #'puni-barf-backward)

;;;; Eglot
(require 'eglot)

(setq eglot-events-buffer-config (plist-put eglot-events-buffer-config :size 0))
(add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

(dolist (hook '(c-ts-mode-hook
                c++-ts-mode-hook
                go-ts-mode-hook
                python-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(keymap-set eglot-mode-map "C-c r" #'eglot-rename)
(keymap-set eglot-mode-map "C-c t" #'eglot-code-actions)
(keymap-set eglot-mode-map "C-c o" #'eglot-code-action-organize-imports)
(keymap-set eglot-mode-map "C-c h" #'eldoc)

;;;; Format
(require 'format-all)

(setq-default format-all-formatters
              '(("Python" ruff)
                ("SQL" (pgformatter
                        "--no-extra-line"
                        "--spaces" "2"
                        "--wrap-limit" "80"))))

(add-hook 'go-ts-mode-hook #'sndb-auto-format-mode)

(mapc (lambda (map)
        (keymap-set map "C-c f" #'format-all-buffer))
      (list python-ts-mode-map sql-mode-map))

;;;; Flymake
(require 'flymake)

(setq flymake-suppress-zero-counters t)
(setq flymake-wrap-around nil)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

;;;; Tree-sitter
(require 'treesit)

(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
        (python-mode . python-ts-mode)))

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(defun sndb-install-treesit-language-grammars ()
  "Install all the grammars from `treesit-language-source-alist'."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (treesit-install-language-grammar lang))))

;;;; Tempel
(require 'tempel)
(keymap-global-set "<backtab>" #'tempel-complete)

;;;; Expreg
(require 'expreg)
(keymap-global-set "C-=" #'expreg-expand)
(keymap-global-set "C--" #'expreg-contract)

;;;; Vundo
(require 'vundo)
(keymap-global-set "C-?" #'vundo)

(provide 'sndb-programming)
