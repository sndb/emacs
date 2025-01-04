;;;; Magit
(require 'magit)

(require 'magit-diff)
(setq magit-diff-refine-hunk 'all)

(require 'magit-repos)
(setq magit-repository-directories '(("~/Git" . 2)))
(add-to-list 'magit-repolist-columns '("*" 1 magit-repolist-column-flag nil))
(keymap-global-set "C-c g" #'magit-list-repositories)

;;;; Ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; Terminal
(defun sndb-launch-terminal ()
  "Launch a new instance of Ghostty."
  (interactive)
  (start-process "ghostty" nil "ghostty"))

(keymap-global-set "<f2>" #'sndb-launch-terminal)

;;;; Shell
(setq shell-command-prompt-show-cwd t)

;;;; GnuPG
(require 'epg)
(setq epg-pinentry-mode 'loopback)

;;;; Dired
(require 'dired)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lhvFA --group-directories-first")
(setq dired-switches-in-mode-line 'as-is)
(setq dired-auto-revert-buffer #'dired-directory-changed-p)
(setq dired-mouse-drag-files t)
(setq dired-create-destination-dirs 'ask)
(setq dired-create-destination-dirs-on-trailing-dirsep t)
(setq dired-isearch-filenames t)
(setq dired-movement-style 'bounded)
(setq dired-filename-display-length 'window)
(setq shell-command-guess-functions '(shell-command-guess-xdg))

(keymap-set dired-mode-map "C-<return>" #'dired-do-open)

;;;; Calc
(require 'calc)
(setq calc-display-trail nil)

(provide 'sndb-application)
