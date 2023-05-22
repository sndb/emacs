;;; init.el --- Personal GNU Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Daniil Sobolev

;; Author: Daniil Sobolev <sndb@sndb.xyz>
;; Created: 24 Mar 2022
;; Keywords: configuration, dotfiles
;; URL: https://github.com/sndb/emacs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my personal configuration for GNU Emacs.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq sndb-package-list
      '(;; Completion
        consult
        corfu
        embark
        embark-consult
        marginalia
        vertico
        wgrep

        ;; Programming
        eglot
        eldoc-box
        format-all
        puni

        ;; Languages
        cider
        clojure-mode
        racket-mode
        geiser
        geiser-guile
        go-mode

        ;; Applications
        elfeed
        emms
        magit
        magit-todos
        nov
        password-store
        pdf-tools
        vterm

        ;; Look
        diff-hl
        hl-todo
        modus-themes))

(defun sndb-install-packages ()
  "Install all the packages from `sndb-package-list'."
  (interactive)
  (package-refresh-contents)
  (dolist (package sndb-package-list)
    (unless (package-installed-p package)
      (package-install package))))

(add-to-list 'load-path (locate-user-emacs-file "sndb-modules"))

(require 'sndb-essential)
(require 'sndb-appearance)
(require 'sndb-control)
(require 'sndb-programming)
(require 'sndb-completion)
(require 'sndb-org)
(require 'sndb-git)
(require 'sndb-application)

;;; rc.el ends here
