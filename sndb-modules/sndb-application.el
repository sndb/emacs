;;;; Magit
(require 'magit)
(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~/data" . 3)))

;;;; Ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; Repositories
(require 'magit-repos)
(add-to-list 'magit-repolist-columns '("Flag" 4 magit-repolist-column-flag (:right-align t)))
(global-set-key (kbd "C-c g") #'magit-list-repositories)

;;;; Terminal emulator
(require 'vterm)

(setq vterm-max-scrollback 10000)

(add-hook 'vterm-exit-functions
          (lambda (buffer _)
            (unless (one-window-p)
              (delete-window (get-buffer-window buffer)))))

(defun sndb-local-name (name)
  (let* ((project (project-current))
         (base (file-name-nondirectory
                (directory-file-name
                 (if project
                     (project-root project)
                   default-directory)))))
    (concat "*" base "-" name "*")))

(defun sndb-vterm ()
  (interactive)
  (let* ((name (sndb-local-name "vterm"))
         (buffer (get-buffer name)))
    (if buffer
        (let ((window (get-buffer-window buffer)))
          (if (equal (current-buffer) buffer)
              (unless (one-window-p)
                (delete-window window))
            (let ((window (if (window-live-p window)
                              window
                            (split-root-window-below))))
              (set-window-buffer window buffer)
              (select-window window))))
      (let ((window (split-root-window-below)))
        (select-window window)
        (vterm name)))))

(keymap-unset vterm-mode-map "<f2>")
(keymap-global-set "<f2>" #'sndb-vterm)

;;;; Shell
(setq shell-command-prompt-show-cwd t)

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

(global-set-key (kbd "C-c p g") #'password-store-generate)
(global-set-key (kbd "C-c p n") #'password-store-generate-no-symbols)
(global-set-key (kbd "C-c p p") #'password-store-copy)
(global-set-key (kbd "C-c p f") #'password-store-copy-field)
(global-set-key (kbd "C-c p e") #'password-store-edit)
(global-set-key (kbd "C-c p r") #'password-store-remove)
(global-set-key (kbd "C-c p l") #'sndb-password-store-copy-login)

;;;; Directory editor
(require 'dired)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lhFA")
(setq dired-switches-in-mode-line 'as-is)
(setq dired-auto-revert-buffer #'dired-directory-changed-p)
(setq dired-mouse-drag-files t)
(setq dired-create-destination-dirs 'ask)
(setq dired-create-destination-dirs-on-trailing-dirsep t)
(setq dired-isearch-filenames t)

(define-key dired-mode-map (kbd "C-+") #'dired-create-empty-file)

(add-hook 'dired-mode-hook #'hl-line-mode)

;;;; Feed reader
(require 'elfeed)

(let ((feeds (locate-user-emacs-file "feeds.el")))
  (when (file-exists-p feeds)
    (load-file feeds)))

(setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))

(global-set-key (kbd "<XF86HomePage>") #'elfeed)

(provide 'sndb-application)
