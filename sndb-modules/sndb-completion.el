;;;; Dabbrev
(require 'dabbrev)
(setq dabbrev-case-fold-search nil)

;;;; Abbrevs
(require 'abbrev)
(setq abbrev-file-name (locate-user-emacs-file "abbrevs.el"))
(setq abbrev-suggest t)
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
(setq vertico-cycle t)
(vertico-mode 1)

;;;; Orderless
(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles basic partial-completion))))

(setq orderless-matching-styles
      '(orderless-flex
        orderless-regexp))

(setq orderless-style-dispatchers
      '(sndb-orderless-literal-dispatcher
        sndb-orderless-initialism-dispatcher))

(defun sndb-orderless-literal-dispatcher (pattern _index _total)
  "Match component as literal if it ends in =."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun sndb-orderless-initialism-dispatcher (pattern _index _total)
  "Match component as initialism if it ends in ,."
  (when (string-suffix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

;;;; Marginalia
(require 'marginalia)
(marginalia-mode 1)

;;;; Consult
(require 'consult)

(setq consult-preview-key '(:debounce 0.5 any))
(setq register-preview-delay 0.5)
(setq register-preview-function #'consult-register-format)

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
(global-set-key (kbd "C-M-#") #'consult-register)
(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)

;;;; Embark
(require 'embark)
(require 'embark-consult)
(require 'wgrep)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

;;;; Corfu
(require 'corfu)
(setq corfu-scroll-margin 0)
(global-corfu-mode 1)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(provide 'sndb-completion)
