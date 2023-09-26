;;;; Terminal emulator
(require 'vterm)

(setq vterm-max-scrollback 16384)

(defun sndb-vterm-buffer-name ()
  (string-join (list "*vterm* - " (buffer-name))))

(defun sndb-vterm ()
  (interactive)
  (vterm (sndb-vterm-buffer-name)))

(defun sndb-vterm-other-window ()
  (interactive)
  (vterm-other-window (sndb-vterm-buffer-name)))

(global-set-key (kbd "<f2>") #'sndb-vterm)
(global-set-key (kbd "C-<f2>") #'sndb-vterm-other-window)

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

(let ((feeds (locate-user-emacs-file "feeds.el")))
  (when (file-exists-p feeds)
    (load-file feeds)))

(setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))

(global-set-key (kbd "<XF86HomePage>") #'elfeed)

(provide 'sndb-application)
