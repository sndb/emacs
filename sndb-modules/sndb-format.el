(defun sndb-format-buffer ()
  "Format the source code in the current buffer."
  (interactive)
  (if (eglot-managed-p)
      (eglot-format)
    (sndb-format-buffer-simple)))

(defun sndb-format-buffer-simple ()
  "Indent the current buffer and delete trailing whitespace."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defvar-local sndb-auto-format-function #'sndb-format-buffer
  "Function to be used for auto-format.")

(define-minor-mode sndb-auto-format-mode
  "Auto-format the current buffer on save."
  :lighter " Afmt")

(defun sndb-auto-format-before-save ()
  "Auto-format the current buffer if `sndb-auto-format-mode' is enabled."
  (when sndb-auto-format-mode
    (funcall sndb-auto-format-function)))

(add-hook 'before-save-hook #'sndb-auto-format-before-save)

(keymap-global-set "C-c f" #'sndb-format-buffer)

(provide 'sndb-format)
