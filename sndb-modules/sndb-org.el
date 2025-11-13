;;;; General
(require 'org)

(setq org-ellipsis "…")
(setq org-startup-indented t)
(setq org-startup-folded 'showall)
(setq org-cycle-separator-lines 0)
(setq org-yank-folded-subtrees nil)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;;;; Input
(setq org-use-speed-commands t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-return-follows-link t)
(setq org-M-RET-may-split-line nil)
(setq org-insert-heading-respect-content t)
(setq org-fold-catch-invisible-edits 'error)

;;;; Logging
(setq org-log-done 'time)
(setq org-log-repeat nil)
(setq org-log-into-drawer t)

;;;; Tags
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-agenda-tags-column 0)

;;;; Agenda
(require 'org-agenda)

(setq org-directory "~/Documents")
(setq org-default-notes-file (concat org-directory "/tasks.org"))
(setq org-agenda-files (list org-directory))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-use-time-grid nil)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-custom-commands
      '(("n" "Agenda (tasks)" ((agenda "") (alltodo ""))
         ((org-agenda-files (list org-default-notes-file))))))

(keymap-set org-mode-map "C-c y" #'org-todo-yesterday)
(keymap-set org-agenda-mode-map "C-c y" #'org-agenda-todo-yesterday)

;;;; Capture
(setq org-capture-templates
      '(("t" "Task" entry
         (file "")
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

;;;; Bindings
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c l" #'org-store-link)

;;;; Consult
(keymap-global-set "M-s M-o" #'consult-org-agenda)
(keymap-set org-mode-map "M-s M-i" #'consult-org-heading)

(provide 'sndb-org)
