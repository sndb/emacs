;;;; gptel
(require 'gptel)

(setq gptel-model 'qwen3:8b)
(setq gptel-default-mode 'org-mode)
(setq gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(qwen3:8b gemma3:4b)))

(keymap-global-set "C-c RET" #'gptel-send)

;;;; Magit
(require 'magit)
(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~/Git" . 2)))
(add-to-list 'magit-repolist-columns '("*" 1 magit-repolist-column-flag nil))
(keymap-global-set "C-c g" #'magit-list-repositories)

;;;; Terminal
(require 'vterm)

(setq vterm-max-scrollback 10000)

(defun sndb-vterm-exit (buffer _)
  "Delete the Vterm window on exit."
  (unless (one-window-p)
    (delete-window (get-buffer-window buffer))))

(add-hook 'vterm-exit-functions #'sndb-vterm-exit)

(defun sndb-project-name (name)
  "Create a name corresponding to the current project or directory."
  (let* ((project (project-current))
         (base (if project
                   (project-name project)
                 (file-name-nondirectory
                  (directory-file-name default-directory)))))
    (concat "*" base "-" name "*")))

(defun sndb-vterm ()
  "Switch to the local Vterm buffer.
Close it if the Vterm buffer is selected."
  (interactive)
  (let ((name (sndb-project-name "vterm"))
        (prev (current-buffer)))
    (pop-to-buffer name)
    (if (equal (current-buffer) prev)
        (unless (one-window-p)
          (delete-window (get-buffer-window)))
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))))

(defun sndb-terminal ()
  "Open a terminal window in the current directory."
  (interactive)
  (let ((term "xfce4-terminal"))
    (start-process term nil term)))

(keymap-unset vterm-mode-map "M-`")
(keymap-unset vterm-mode-map "<f2>")
(keymap-global-set "<f2>" #'sndb-vterm)
(keymap-global-set "M-<f2>" #'sndb-terminal)

;;;; GnuPG
(require 'epg)
(setq epg-pinentry-mode 'loopback)

;;;; Dired
(require 'dired)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lhvFA --group-directories-first")
(setq dired-auto-revert-buffer #'dired-directory-changed-p)
(setq dired-mouse-drag-files t)
(setq dired-free-space nil)
(setq dired-create-destination-dirs 'ask)
(setq dired-create-destination-dirs-on-trailing-dirsep t)
(setq dired-isearch-filenames t)
(setq dired-movement-style 'bounded-files)
(setq dired-filename-display-length 'window)
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq shell-command-guess-functions '(shell-command-guess-xdg))

(keymap-set dired-mode-map "C-<return>" #'dired-do-open)

(provide 'sndb-application)
