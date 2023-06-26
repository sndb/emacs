;;;; Terminal emulator
(require 'vterm)
(global-set-key (kbd "<f2>") #'vterm)
(global-set-key (kbd "C-<f2>") #'vterm-other-window)
(setq vterm-max-scrollback 16384)

;;;; Shell
(setq shell-command-prompt-show-cwd t)

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

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
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

(provide 'sndb-application)
