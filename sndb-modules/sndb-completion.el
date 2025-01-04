;;;; Ignore case
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;;; Minibuffer
(setq enable-recursive-minibuffers t)
(setq resize-mini-windows t)
(setq minibuffer-default-prompt-format " [%s]")

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;;;; ElDoc
(require 'eldoc)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0.25)

;;;; Corfu
(require 'corfu)
(global-corfu-mode 1)

;;;; Vertico
(require 'vertico)
(vertico-mode 1)

(require 'vertico-directory)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(require 'vertico-multiform)
(vertico-multiform-mode 1)

;;;; Marginalia
(require 'marginalia)
(marginalia-mode 1)

;;;; Consult
(require 'consult)

(setq consult-find-args "find . -not ( -path */.git* -prune )")
(setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden --glob=!.git"))
(setq consult-preview-key '(:debounce 0.2 any))
(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(keymap-global-set "M-s M-r" #'consult-recent-file)
(keymap-global-set "M-s M-f" #'consult-find)
(keymap-global-set "M-s M-g" #'consult-ripgrep)
(keymap-global-set "M-s M-l" #'consult-line)
(keymap-global-set "M-s M-i" #'consult-imenu)
(keymap-global-set "M-s M-o" #'consult-outline)
(keymap-global-set "M-s M-d" #'consult-flymake)
(keymap-global-set "M-g M-g" #'consult-goto-line)

(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)
(keymap-global-set "C-x t b" #'consult-buffer-other-tab)
(keymap-global-set "C-x p b" #'consult-project-buffer)
(keymap-global-set "C-x r b" #'consult-bookmark)

;;;; Embark
(require 'embark)
(require 'embark-consult)

(setq embark-indicators
      '(embark-minimal-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'vertico-multiform-categories '(embark-keybinding grid))

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(keymap-global-set "C-." #'embark-act)
(keymap-global-set "M-." #'embark-dwim)

;;;; Wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(keymap-set grep-mode-map "C-x C-q" #'wgrep-change-to-wgrep-mode)

(provide 'sndb-completion)
