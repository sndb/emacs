;;;; Styles
(setq completion-styles '(basic substring partial-completion flex))

;;;; Abbrev
(setq dabbrev-case-fold-search nil)
(setq abbrev-file-name (locate-user-emacs-file "abbrevs.el"))
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

;; remap
(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
(global-set-key [remap project-switch-to-buffer] #'consult-project-buffer)
(global-set-key [remap bookmark-jump] #'consult-bookmark)
(global-set-key [remap goto-line] #'consult-goto-line)
(global-set-key [remap yank-pop] #'consult-yank-pop)

;; search-map
(global-set-key (kbd "M-s d") #'consult-find)
(global-set-key (kbd "M-s g") #'consult-grep)
(global-set-key (kbd "M-s G") #'consult-git-grep)
(global-set-key (kbd "M-s D") #'consult-locate)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s L") #'consult-line-multi)

;; goto-map
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-imenu-multi)
(global-set-key (kbd "M-g e") #'consult-compile-error)
(global-set-key (kbd "M-g f") #'consult-flymake)
(global-set-key (kbd "M-g o") #'consult-outline)
(global-set-key (kbd "M-g m") #'consult-mark)

;; register
(global-set-key (kbd "M-\"") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)

;;;; Embark
(require 'embark)
(require 'embark-consult)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

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
