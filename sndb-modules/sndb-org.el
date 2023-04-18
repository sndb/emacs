(require 'org)
(require 'org-agenda)
(setq org-modules '(org-habit org-id ol-info))

;; Source
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (C . t)
   (latex . t)))

;; UI
(setq org-catch-invisible-edits 'error)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-return-follows-link t)
(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-list-allow-alphabetical t)
(setq org-ellipsis "…")
(setq org-export-with-author nil)

(add-hook 'org-mode-hook #'visual-line-mode)

;; Images
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 640)

;; LaTeX
(setq org-highlight-latex-and-related '(latex))
(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-latex-preview-ltxpng-directory (concat user-emacs-directory "ltximg/"))

;; Tasks
(setq org-archive-location (concat org-directory "/archive.org::"))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-use-fast-todo-selection 'expert)

;; Clock
(setq org-clock-persist t)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-report-include-clocking-task t)
(org-clock-persistence-insinuate)

;; Agenda
(setq org-agenda-window-setup 'other-tab)
(setq org-agenda-files `(,org-directory))
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-on-weekday nil)
(setq org-habit-graph-column 88)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-skip-scheduled-if-done t)
(define-key org-mode-map (kbd "C-c y") #'org-todo-yesterday)
(define-key org-agenda-mode-map (kbd "C-c y") #'org-agenda-todo-yesterday)

;; Capture
(setq sndb-task-template "* TODO %?\n%u\n%i")
(setq sndb-bookmarks-file (concat org-directory "/bookmarks.org"))
(setq org-capture-templates
      `(("t" "Task" entry
         (file+headline "" "Tasks")
         ,sndb-task-template
         :empty-lines 1)
        ("c" "Current" entry
         (clock)
         ,sndb-task-template
         :empty-lines 1)
        ("b" "Bookmark" item
         (file+headline ,sndb-bookmarks-file "New")
         "- [[%c][%?]]")))

;; Refiling
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 3))
        (nil . (:maxlevel . 3))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun sndb-sort-headings ()
  "Sorts the contents of all headings on the first level."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p (point)))
      (while (not (= p
                     (progn (org-forward-heading-same-level 1)
                            (setq p (point)))))
        (org-sort-entries nil ?a)))))

(defun sndb-open-notes ()
  "Open `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c n") #'sndb-open-notes)

(provide 'sndb-org)
