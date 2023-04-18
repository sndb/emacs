(setq vc-follow-symlinks t)

;;;; Magit
(require 'magit)

(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~" . 3)))

(add-to-list 'magit-repolist-columns '("Flag" 4 magit-repolist-column-flag (:right-align t)))
(global-set-key (kbd "C-c g") #'magit-list-repositories)

(require 'magit-todos)
(magit-todos-mode 1)

;;;; Diff highlight
(require 'diff-hl)

(setq diff-hl-draw-borders nil)
(setq diff-hl-show-staged-changes nil)

(global-diff-hl-mode 1)

(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

(provide 'sndb-git)
