;; -*- lexical-binding: t -*-

;; packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; files

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; general

(setq-default truncate-lines t)
(setq-default fill-column 80)
(setq initial-major-mode 'org-mode)
(setq frame-resize-pixelwise t)
(setq sentence-end-double-space nil)
(setq scroll-conservatively 101)
(setq indent-tabs-mode nil)

; review later
;(server-start)
;(recentf-mode t)
;(fset 'yes-or-no-p 'y-or-n-p)
;(setq initial-scratch-message "")
;(setq split-width-threshold 80)

;; disable

(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

;; visuals

(setq display-time-default-load-average nil)
(line-number-mode)
(column-number-mode)
(size-indication-mode)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)

(global-display-line-numbers-mode)
(global-hl-line-mode)
(show-paren-mode)

(set-face-attribute 'default nil :family "JetBrains Mono" :height 110)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Liberation Serif" :height 160)

(use-package modus-themes
  :init
  (setq modus-themes-region '(bg-only no-extend)
        modus-themes-syntax '(yellow-comments)
        modus-themes-hl-line '(accented)
        modus-themes-italic-constructs t
        modus-themes-mode-line '(accented borderless)
        modus-themes-headings '((t . (rainbow))))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package dashboard
  :config
  (setq dashboard-center-content t
        dashboard-show-shortcuts nil)
  (dashboard-setup-startup-hook))

(use-package visual-fill-column)

;; keyboard

(use-package which-key
  :init
  (setq which-key-idle-delay 0.25)
  :config
  (which-key-mode))

(use-package centered-cursor-mode
  :config
  (global-centered-cursor-mode))

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        (remove 'org evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; org

(use-package org
  :pin gnu
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . visual-fill-column-mode))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

;; completion

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
)

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

;; applications

(use-package vterm)

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
