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
(require 'vterm)

(setq vterm-max-scrollback 10000)

(add-hook 'vterm-exit-functions
          (lambda (buffer _)
            (unless (one-window-p)
              (delete-window (get-buffer-window buffer)))))

(defun sndb-local-name (name)
  "Create a name related to the current project or directory."
  (let* ((project (project-current))
         (base (file-name-nondirectory
                (directory-file-name
                 (if project
                     (project-root project)
                   default-directory)))))
    (concat "*" base "-" name "*")))

(setq sndb-vterm-split-window-function #'split-window-right)

(defun sndb-vterm ()
  "Switch to the local Vterm buffer.
Close it if the Vterm buffer is selected."
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
                            (funcall sndb-vterm-split-window-function))))
              (set-window-buffer window buffer)
              (select-window window))))
      (let ((window (funcall sndb-vterm-split-window-function)))
        (select-window window)
        (vterm name)))))

(keymap-unset vterm-mode-map "<f2>")
(keymap-global-set "<f2>" #'sndb-vterm)

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
