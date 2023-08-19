(setq max-specpdl-size (* 4 max-specpdl-size))
(setq max-lisp-eval-depth (* 4 max-lisp-eval-depth))
(setq gc-cons-threshold (* 64 1024 1024))
(setq load-prefer-newer t)
(setq native-comp-async-report-warnings-errors 'silent)

(setq frame-title-format '("%b"))
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
