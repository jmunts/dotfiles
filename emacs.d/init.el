;; set package archive source
(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(package-refresh-contents)

;; use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; increased garbage collection threshold
(setq gc-cons-threshold 50000000)

;; UI
(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;; emacs' default keystore is 1 sec(too slow)
(setq echo-keystrokes 0.5)

;; yes and no are too long, use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; moved emacs' custom config location
(setq custom-file "custom.el")
(load custom-file)

;; zerodark-theme
(use-package zerodark-theme
  :ensure t
  :defer t)
