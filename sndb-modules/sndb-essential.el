;;;; Server
(require 'server)
(server-start)

;;;; Files
(defun sndb-load-if-exists (file)
  (when (file-exists-p file)
    (load-file file)))

(sndb-load-if-exists (locate-user-emacs-file "custom.el"))
(sndb-load-if-exists (locate-user-emacs-file "private.el"))

(setq large-file-warning-threshold (* 50 1024 1024))

;;;; Backup
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;;; Recent
(require 'recentf)

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

;;;; History

;; Point
(require 'saveplace)
(save-place-mode 1)

;; Window
(require 'winner)
(winner-mode 1)

;; Minibuffer
(require 'savehist)
(setq history-delete-duplicates t)
(setq history-length 1000)
(savehist-mode 1)

;;;; Startup
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ()
  (message (emacs-init-time)))

(provide 'sndb-essential)
