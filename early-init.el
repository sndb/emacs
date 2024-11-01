(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
