;; -*- lexical-binding: t; -*-

;;;; Common
(setq default-input-method "russian-computer")
(setq display-raw-bytes-as-hex t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-hl-todo-mode 1)
(global-subword-mode 1)
(electric-indent-mode -1)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)

(defun add-hook-before-save (hook function)
  (add-hook hook (lambda () (add-hook 'before-save-hook function nil t))))

;;;; C
(require 'c-ts-mode)
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'linux)

;;;; Odin
(require 'odin-mode)
(add-hook 'odin-mode-hook #'indent-tabs-mode)
(keymap-set odin-mode-map "C-c C-r" #'odin-run-project)

;;;; Go
(require 'go-ts-mode)
(setq go-ts-mode-indent-offset 4)
(add-hook-before-save 'go-ts-mode-hook #'eglot-format-buffer)
(add-hook-before-save 'go-ts-mode-hook #'eglot-organize-imports)

;;;; Other
(require 'js)
(require 'python)
(require 'lua-ts-mode)
(require 'yaml-ts-mode)
(require 'sh-script)

;;;; Puni
(require 'puni)

(setq puni-confirm-when-delete-unbalanced-active-region nil)

(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                scheme-mode-hook))
  (add-hook hook #'electric-pair-local-mode)
  (add-hook hook #'puni-mode))

(defvar-keymap sndb-puni-mode-map
  :doc "Keymap for my custom `puni-mode' bindings."
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

;;;; Flymake
(require 'flymake)

(setq flymake-suppress-zero-counters t)
(setq flymake-wrap-around nil)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

;;;; Tree-sitter
(require 'treesit)

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (javascript-mode . js-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (css-mode . css-ts-mode)))

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (css "https://github.com/tree-sitter/tree-sitter-css")))

(defun sndb-install-treesit-language-grammars ()
  "Install all the grammars from `treesit-language-source-alist'."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (treesit-install-language-grammar lang))))

;;;; Eglot
(require 'eglot)

(setq eglot-sync-connect nil)
(setq eglot-autoshutdown t)
(setq eglot-events-buffer-config (plist-put eglot-events-buffer-config :size 0))
(add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

(dolist (hook '(c-ts-mode-hook
                go-ts-mode-hook
                js-ts-mode-hook
                python-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(keymap-set eglot-mode-map "C-c r" #'eglot-rename)
(keymap-set eglot-mode-map "C-c t" #'eglot-code-actions)
(keymap-set eglot-mode-map "C-c o" #'eglot-organize-imports)
(keymap-set eglot-mode-map "C-c f" #'eglot-format-buffer)

(defun eglot-organize-imports ()
  "Organize imports."
  (interactive)
  (call-interactively #'eglot-code-action-organize-imports))

(provide 'sndb-programming)
