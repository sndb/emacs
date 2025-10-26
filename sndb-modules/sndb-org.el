;;;; General
(require 'org)

(setq org-ellipsis "…")
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-cycle-separator-lines 0)
(setq org-insert-heading-respect-content t)
(setq org-startup-folded 'showall)
(setq org-fold-catch-invisible-edits 'error)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-repeat nil)

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

;;;; Paths
(setq org-directory "~/Documents")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;;; Tags
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-agenda-tags-column 0)

;;;; Agenda
(require 'org-agenda)

(setq org-agenda-files `(,org-directory))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

(keymap-set org-mode-map "C-c y" #'org-todo-yesterday)
(keymap-set org-agenda-mode-map "C-c y" #'org-agenda-todo-yesterday)

;;;; Capture
(setq org-capture-templates
      '(("t" "Task" entry
         (file "tasks.org")
         "* TODO %?\n%i"
         :empty-lines 1)))

;;;; Refile
(setq org-refile-targets
      '((nil . (:level . 1))
        (org-agenda-files . (:level . 1))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;;;; Source
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)

;;;; LaTeX
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))

(provide 'sndb-org)
