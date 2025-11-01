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
         (base (file-name-nondirectory
                (directory-file-name
                 (if project
                     (project-root project)
                   default-directory)))))
    (concat "*" base "-" name "*")))

(defun sndb-vterm ()
  "Switch to the local Vterm buffer.
Close it if the Vterm buffer is selected."
  (interactive)
  (let* ((name (sndb-project-name "vterm"))
         (buffer (get-buffer name))
         (split-function #'split-window-right))
    (if buffer
        (if (equal (current-buffer) buffer)
            (unless (one-window-p)
              (delete-window (get-buffer-window buffer)))
          (let ((window (or (get-buffer-window buffer)
                            (funcall split-function))))
            (set-window-buffer window buffer)
            (select-window window)))
      (let ((window (funcall split-function)))
        (select-window window)
        (vterm name)))))

(keymap-unset vterm-mode-map "M-`")
(keymap-unset vterm-mode-map "<f2>")
(keymap-global-set "<f2>" #'sndb-vterm)

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
