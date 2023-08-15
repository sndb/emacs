;;;; Orderless
(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;;;; Abbrev
(setq abbrev-file-name (locate-user-emacs-file "abbrev.el"))
(add-hook 'prog-mode-hook #'abbrev-mode)

;;;; Ignore case
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;;; ElDoc
(require 'eldoc)
(require 'eldoc-box)

(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0)

;;;; Vertico
(require 'vertico)

(setq vertico-scroll-margin 0)
(setq vertico-count 8)
(setq vertico-resize nil)

(vertico-mode 1)

;;;; Marginalia
(require 'marginalia)
(marginalia-mode 1)

;;;; Consult
(require 'consult)

(setq consult-find-args "find . -not ( -path */.git* -prune )")
(setq consult-preview-key '(:debounce 0.25 any))
(setq register-preview-delay 0.25
      register-preview-function #'consult-register-format)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(global-set-key (kbd "M-s M-r") #'consult-recent-file)
(global-set-key (kbd "M-s M-f") #'consult-find)
(global-set-key (kbd "M-s M-g") #'consult-grep)
(global-set-key (kbd "M-s M-l") #'consult-line)
(global-set-key (kbd "M-s M-i") #'consult-imenu)
(global-set-key (kbd "M-s M-o") #'consult-outline)
(global-set-key (kbd "M-s M-d") #'consult-flymake)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)

;;;; Embark
(require 'embark)
(require 'embark-consult)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)

;;;; Corfu
(require 'corfu)

(setq corfu-scroll-margin 0)
(setq corfu-count 8)

(global-corfu-mode 1)
(corfu-popupinfo-mode 1)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;;;; Wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)

(provide 'sndb-completion)
