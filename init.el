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

(recentf-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-major-mode 'org-mode)
(setq frame-resize-pixelwise t)
(setq-default truncate-lines t)
(setq-default fill-column 80)
(setq sentence-end-double-space nil)
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))
(setq initial-scratch-message "")
(setq split-width-threshold 80)
(setq indent-tabs-mode nil)

;; disable

(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

;; modeline

(setq display-time-default-load-average nil)
(line-number-mode)
(column-number-mode)
(size-indication-mode)

;; visuals

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
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3))

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

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t
        org-appear-autolinks t
        org-appear-autosubmarkers t))
