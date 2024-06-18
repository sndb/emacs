;;;; Magit
(require 'magit)

(require 'magit-diff)
(setq magit-diff-refine-hunk 'all)

(require 'magit-repos)
(setq magit-repository-directories '(("~/data/source" . 2)))

;;;; Ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; Terminal emulator
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

;;;; Password manager
(require 'epg)
(setq epg-pinentry-mode 'loopback)

(require 'password-store)

(defun sndb-password-store-copy-login (entry)
  "Add login for ENTRY into the kill ring."
  (interactive (list (password-store--completing-read)))
  (password-store-copy-field entry "login"))

(keymap-global-set "C-c p g" #'password-store-generate)
(keymap-global-set "C-c p n" #'password-store-generate-no-symbols)
(keymap-global-set "C-c p p" #'password-store-copy)
(keymap-global-set "C-c p f" #'password-store-copy-field)
(keymap-global-set "C-c p e" #'password-store-edit)
(keymap-global-set "C-c p r" #'password-store-remove)
(keymap-global-set "C-c p l" #'sndb-password-store-copy-login)

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
(setq dired-movement-style 'bounded)
(setq dired-filename-display-length 'window)
(setq shell-command-guess-functions '(shell-command-guess-xdg))

(keymap-set dired-mode-map "C-+" #'dired-create-empty-file)
(keymap-set dired-mode-map "C-<return>" #'dired-do-open)

(add-hook 'dired-mode-hook #'hl-line-mode)

;;;; EPUB
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;; Calc
(require 'calc)
(setq calc-display-trail nil)

(provide 'sndb-application)
