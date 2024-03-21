;;;; General
(require 'org)

(setq org-directory "~/data/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-folded 'showall)
(setq org-fold-catch-invisible-edits 'error)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

;;;; Agenda
(require 'org-agenda)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files `(,org-directory))
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

(keymap-set org-mode-map "C-c y" #'org-todo-yesterday)
(keymap-set org-agenda-mode-map "C-c y" #'org-agenda-todo-yesterday)

;;;; Capture
(setq org-capture-templates
      '(("t" "Task" entry
         (file "tasks.org")
         "* TODO %?\n%u\n%i"
         :empty-lines 1)))

;;;; Refile
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 3))
        (nil . (:maxlevel . 3))))
(setq org-refile-use-outline-path 'file)

;;;; Source
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 (mapcar (lambda (a) (cons a t))
         '(C emacs-lisp latex python shell sql)))

;;;; LaTeX
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))

(provide 'sndb-org)
