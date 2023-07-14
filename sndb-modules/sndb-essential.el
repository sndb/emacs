;;;; Server
(server-start)

;;;; Files
(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))

(setq custom-file (make-temp-file "emacs-custom-"))
(setq large-file-warning-threshold (* 50 1024 1024))
(setq vc-follow-symlinks t)

;;;; Backup
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;; History
(setq bookmark-set-fringe-mark nil)
(setq history-delete-duplicates t)
(setq history-length 1000)
(setq recentf-max-saved-items 200)
(setq recentf-exclude
      '(;; Tramp
        "^/ssh:"
        "^/sudo:"

        ;; Images
        "\\.jpe?g$"
        "\\.png$"
        "\\.gif$"
        "\\.webp$"

        ;; Archives
        "\\.zip$"
        "\\.gz$"
        "\\.xz$"
        "\\.zst$"))

(recentf-mode 1)
(save-place-mode 1)
(winner-mode 1)
(savehist-mode 1)

(provide 'sndb-essential)
