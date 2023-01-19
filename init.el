;;; rc.el --- Personal GNU Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Daniil Sobolev

;; Author: Daniil Sobolev <sndb@sndb.xyz>
;; Created: 24 Mar 2022
;; Keywords: configuration, dotfiles
;; URL: https://github.com/sndb/emacs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my personal configuration for GNU Emacs.

;;; Code:

;;;; Auxiliary
(defun sndb-random-element (list)
  "Returns a random element from LIST."
  (nth (random (length list)) list))

;;;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq sndb-package-list
      '(;; Completion
        affe
        cape
        consult
        corfu
        embark
        embark-consult
        marginalia
        orderless
        vertico
        wgrep

        ;; Languages
        eglot
        format-all
        go-mode
        racket-mode
        rust-mode

        ;; Applications
        elfeed
        emms
        magit
        magit-todos
        nov
        password-store
        pdf-tools
        vterm
        vterm-toggle

        ;; Miscellaneous
        diff-hl
        ef-themes
        hl-todo
        modus-themes
        standard-themes))

(defun sndb-install-packages ()
  "Install all the packages from `sndb-package-list'."
  (interactive)
  (package-refresh-contents)
  (dolist (package sndb-package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;;; Hooks
(defun sndb-add-funcs-to-hook (hook &rest functions)
  "Add FUNCTIONS to HOOK."
  (dolist (function functions)
    (add-hook hook function)))

(defun sndb-add-func-to-hooks (function &rest hooks)
  "Add FUNCTION to HOOKS."
  (dolist (hook hooks)
    (add-hook hook function)))

;;;; Server
(require 'server)
(server-start)
(global-set-key (kbd "C-c k") #'save-buffers-kill-emacs)

;;;; Files
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq sndb-private-file (locate-user-emacs-file "private.el"))
(when (file-exists-p sndb-private-file)
  (load-file sndb-private-file))

(setq large-file-warning-threshold (* 50 (expt 2 20)))

;;;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 256)
(setq recentf-exclude
      '(;; Tramp
        "^/ssh:"
        "^/sudo:"

        ;; Images
        "\\.jpe?g$"
        "\\.png$"
        "\\.gif$"
        "\\.webp$"

        ;; Archives
        "\\.zip$"
        "\\.gz$"
        "\\.xz$"
        "\\.zst$"))
(recentf-mode 1)

;;;; Point history
(require 'saveplace)
(setq save-place-limit 256)
(save-place-mode 1)

;;;; Window history
(require 'winner)
(winner-mode 1)

;;;; Minibuffer history
(require 'savehist)
(setq history-length 1024)
(savehist-mode 1)

;;;; Startup
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ()
  (message (emacs-init-time)))

;;;; Indicators
(setq use-short-answers t)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq mode-line-compact 'long)
(setq visible-bell t)
(setq-default indicate-empty-lines t)
(setq use-dialog-box nil)
(setq-default display-line-numbers-widen t)
(setq display-line-numbers-width-start t)

(blink-cursor-mode -1)
(display-time-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode -1)

(sndb-add-func-to-hooks (lambda () (setq show-trailing-whitespace t))
                        'text-mode-hook
                        'prog-mode-hook)

;;;; Parentheses
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(show-paren-mode 1)

;;;; Highlight keywords
(require 'hl-todo)
(global-hl-todo-mode 1)

;;;; Windows
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

(global-set-key [remap balance-windows] #'balance-windows-area)
(global-set-key (kbd "C-x !") #'delete-other-windows-vertically)

;;;; Buffers
(setq view-read-only t)
(setq uniquify-buffer-name-style 'forward)
(setq initial-scratch-message nil)

(global-set-key [remap list-buffers] #'ibuffer)

;;;; Tabs
(require 'tab-bar)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)

;;;; Clipboard
(setq save-interprogram-paste-before-kill t)

;;;; Repeating
(setq set-mark-command-repeat-pop t)
(repeat-mode 1)

;;;; Fonts
(setq text-scale-mode-step 1.1)

(setq sndb-favorite-fonts '("Go Mono-10.5" "Fira Mono-10.5" "Cozette"))
(setq sndb-default-font (car sndb-favorite-fonts))

(set-face-attribute 'default nil :font sndb-default-font)
(set-face-attribute 'fixed-pitch nil :font sndb-default-font)
(set-face-attribute 'variable-pitch nil :font "Crimson Pro-14")

(defun sndb-set-font (font)
  "Set FONT."
  (set-face-attribute 'default nil :font font)
  (set-face-attribute 'fixed-pitch nil :font font)
  (message "Font: %s" font))

(defun sndb-rotate-fonts ()
  "Rotates the list of favorite fonts."
  (interactive)
  (setq sndb-favorite-fonts
        (append (cdr sndb-favorite-fonts)
                (list (car sndb-favorite-fonts))))
  (sndb-set-font (car sndb-favorite-fonts)))

(global-set-key (kbd "<f5> f") #'sndb-rotate-fonts)

;;;; Theme
(setq custom-safe-themes t)
(setq x-gtk-use-system-tooltips nil)

(require 'ef-themes)
(require 'modus-themes)
(require 'standard-themes)

(setq modus-themes-region '(bg-only))
(setq modus-themes-italic-constructs t)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-common-palette-overrides
      '((string green-cooler)
        (comment yellow-cooler)
        (bg-paren-match bg-magenta-intense)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
        (bg-mode-line-active bg-blue-subtle)
        (bg-mode-line-inactive bg-blue-nuanced)))

(defun sndb-disable-themes ()
  "Disable all enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "<f5> <f5>") #'sndb-disable-themes)
(global-set-key (kbd "<f5> e") #'ef-themes-select)
(global-set-key (kbd "<f5> r") #'ef-themes-load-random)
(global-set-key (kbd "<f5> m") #'modus-themes-toggle)
(global-set-key (kbd "<f5> s") #'standard-themes-toggle)

(load-theme 'modus-vivendi)

;;;; Programming

;; C
(setq c-default-style "linux")
(add-hook 'c-mode-common-hook #'indent-tabs-mode)
(setq comment-style 'extra-line)

;; Go
(add-hook 'go-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'before-save-hook #'gofmt-before-save)

;; SQL
(setq sql-product 'sqlite)
(add-hook 'sql-mode-hook
          (lambda () (setq format-all-formatters
                           '(("SQL" (pgformatter
                                     "--function-case" "2"
                                     "--keyword-case" "2"
                                     "--type-case" "2"
                                     "--no-extra-line"
                                     "--tabs"))))))

;; CSS
(setq css-indent-offset 2)

;; JS
(setq js-indent-level 2)

;;;; Mouse
(setq mouse-wheel-progressive-speed nil)
(setq mouse-yank-at-point t)

;;;; Scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 1)

(defun sndb-scroll-half-screen-up ()
  "Scroll half screen up."
  (interactive)
  (scroll-up 4))

(defun sndb-scroll-half-screen-down ()
  "Scroll half screen down."
  (interactive)
  (scroll-down 4))

(global-set-key (kbd "C-S-n") #'sndb-scroll-half-screen-up)
(global-set-key (kbd "C-S-p") #'sndb-scroll-half-screen-down)

;;;; Format
(require 'format-all)

(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq default-input-method "TeX")
(setq display-raw-bytes-as-hex t)
(setq delete-trailing-lines nil)

(defun sndb-format-buffer ()
  "Auto-format the source code in the current buffer."
  (interactive)
  (if (eglot-managed-p)
      (eglot-format)
    (format-all-buffer t)))

(defun sndb-indent-buffer ()
  "Indent the current buffer and delete trailing whitespace."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun sndb-replace-untypable-characters ()
  "Replace the characters that are inconvenient to type."
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

(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c f") #'sndb-format-buffer)
(global-set-key (kbd "C-c i") #'sndb-indent-buffer)

;;;; Auto-Revert
(require 'autorevert)
(global-auto-revert-mode 1)

;;;; Commands
(setq disabled-command-function nil)

(global-set-key [remap zap-to-char] #'zap-up-to-char)
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)

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
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0.1)

;;;; Vertico
(require 'vertico)
(setq vertico-count 20)
(setq vertico-scroll-margin 0)
(vertico-mode 1)

;;;; Orderless
(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles basic partial-completion))))
(setq orderless-matching-styles '(orderless-flex orderless-regexp))
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

;;;; Affe
(require 'affe)

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(global-set-key (kbd "M-s d") #'affe-find)
(global-set-key (kbd "M-s g") #'affe-grep)

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

;;;; Cape
(require 'cape)
(dolist (capf '(cape-dabbrev cape-file cape-keyword cape-symbol cape-line cape-tex))
  (add-to-list 'completion-at-point-functions capf))

;;;; Eglot
(require 'eglot)
(require 'go-mode)
(require 'racket-mode)
(require 'rust-mode)

(sndb-add-func-to-hooks #'eglot-ensure
                        'c-mode-hook
                        'go-mode-hook
                        'python-mode-hook
                        'racket-mode-hook
                        'rust-mode-hook
                        'sh-mode-hook)

(define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c t") #'eglot-code-actions)

;;;; Org mode
(require 'org)
(require 'org-agenda)
(setq org-modules '(org-habit org-id ol-info))

;; Source
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (C . t)
   (latex . t)))

;; UI
(setq org-catch-invisible-edits 'error)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-return-follows-link t)
(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-list-allow-alphabetical t)
(setq org-ellipsis "…")

(add-hook 'org-mode-hook #'visual-line-mode)

;; Images
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 640)

;; LaTeX
(setq org-highlight-latex-and-related '(latex))
(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-latex-preview-ltxpng-directory (concat user-emacs-directory "ltximg/"))

;; Tasks
(setq org-archive-location (concat org-directory "/archive.org::"))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-use-fast-todo-selection 'expert)

;; Clock
(setq org-clock-persist t)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-report-include-clocking-task t)
(org-clock-persistence-insinuate)

;; Agenda
(setq org-agenda-window-setup 'other-tab)
(setq org-agenda-files `(,org-directory))
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-on-weekday nil)
(setq org-habit-graph-column 88)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-skip-scheduled-if-done t)
(define-key org-mode-map (kbd "C-c y") #'org-todo-yesterday)
(define-key org-agenda-mode-map (kbd "C-c y") #'org-agenda-todo-yesterday)

;; Capture
(setq sndb-task-template "* TODO %?\n%u\n%i")
(setq sndb-bookmarks-file (concat org-directory "/bookmarks.org"))
(setq org-capture-templates
      `(("t" "Task" entry
         (file+headline "" "Tasks")
         ,sndb-task-template
         :empty-lines 1)
        ("c" "Current" entry
         (clock)
         ,sndb-task-template
         :empty-lines 1)
        ("b" "Bookmark" item
         (file+headline ,sndb-bookmarks-file "New")
         "- [[%c][%?]]")))

;; Refiling
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 3))
        (nil . (:maxlevel . 3))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun sndb-sort-headings ()
  "Sorts the contents of all headings on the first level."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p (point)))
      (while (not (= p
                     (progn (org-forward-heading-same-level 1)
                            (setq p (point)))))
        (org-sort-entries nil ?a)))))

(defun sndb-open-notes ()
  "Open `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c n") #'sndb-open-notes)

;;;; Git interface
(setq vc-follow-symlinks t)

(require 'magit)
(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~" . 3)))
(add-to-list 'magit-repolist-columns '("Flag" 4 magit-repolist-column-flag (:right-align t)))
(global-set-key (kbd "C-c g") #'magit-list-repositories)

(require 'magit-todos)
(magit-todos-mode 1)

(require 'diff-hl)
(setq diff-hl-draw-borders nil)
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode 1)
(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

;;;; Terminal emulator
(require 'vterm)
(require 'vterm-toggle)
(setq vterm-max-scrollback (expt 2 14))
(global-set-key (kbd "C-c v") #'vterm-toggle)
(global-set-key (kbd "C-c V") #'vterm-toggle-cd)
(define-key vterm-mode-map (kbd "C-<return>") #'vterm-toggle-insert-cd)
(define-key vterm-mode-map (kbd "C-S-n") #'vterm-toggle-forward)
(define-key vterm-mode-map (kbd "C-S-p") #'vterm-toggle-backward)

;;;; PDF reader
(require 'pdf-tools)
(setq pdf-info-restart-process-p t)
(setq pdf-view-resize-factor 1.1)
(pdf-tools-install)

;;;; EPUB reader
(require 'nov)
(setq nov-text-width fill-column)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;; Password manager
(require 'epg)
(setq epg-pinentry-mode 'loopback)

(require 'password-store)

(defun sndb-password-store-copy-login (entry)
  "Add login for ENTRY into the kill ring."
  (interactive (list (password-store--completing-read)))
  (password-store-copy-field entry "login"))

(global-set-key (kbd "C-c p p") #'password-store-copy)
(global-set-key (kbd "C-c p e") #'password-store-edit)
(global-set-key (kbd "C-c p l") #'sndb-password-store-copy-login)

;;;; Directory editor
(require 'dired)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lhFA")
(setq dired-switches-in-mode-line 'as-is)

;;;; Multimedia system
(require 'emms-setup)
(require 'emms-info-exiftool)
(require 'emms-history)

(emms-all)

(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "~/music/")

(emms-history-load)

(global-set-key (kbd "C-c e e") #'emms)
(global-set-key (kbd "C-c e b") #'emms-browser)
(global-set-key (kbd "C-c e p") #'emms-previous)
(global-set-key (kbd "C-c e n") #'emms-next)
(global-set-key (kbd "C-c e s") #'emms-stop)
(global-set-key (kbd "C-c e r") #'emms-random)
(global-set-key (kbd "C-c e >") #'emms-seek-forward)
(global-set-key (kbd "C-c e <") #'emms-seek-backward)
(global-set-key (kbd "C-c e SPC") #'emms-pause)

;;;; Feed reader
(require 'elfeed)

(setq sndb-feeds-file (locate-user-emacs-file "feeds.el"))
(when (file-exists-p sndb-feeds-file)
  (load-file sndb-feeds-file))

(setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
(setq-default elfeed-search-filter "@2-weeks-ago +unread -spam ")

(global-set-key (kbd "<XF86HomePage>") #'elfeed)

;;;; Mail
(setq auth-sources '("~/.authinfo.gpg")
      user-full-name "Daniil Sobolev"
      user-mail-address "sndb@sndb.xyz")

(require 'notmuch)
(global-set-key (kbd "<XF86Mail>") #'notmuch)

(require 'smtpmail)
(setq smtpmail-smtp-server "smtp.mailbox.org")
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)
(setq send-mail-function 'smtpmail-send-it)

;;; rc.el ends here
