;;;; Server
(server-start)

;;;; Files
(setq load-prefer-newer t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq custom-file (make-temp-file "emacs-custom-"))
(setq vc-follow-symlinks t)

;;;; Backup
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;; Bookmarks
(setq bookmark-fringe-mark nil)
(setq bookmark-save-flag 1)

;;;; History
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
