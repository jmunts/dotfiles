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
(global-linum-mode t)
(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;; show parenthesis pls
(show-paren-mode 1)
(setq show-paren-delay 0) ;; no delay

;; navigation
(windmove-default-keybindings)

;; emacs' default keystore is 1 sec(too slow)
(setq echo-keystrokes 0.5)

;; yes and no are too long, use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; i always accidentally close emacs, so prompt y or n when quiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; moved emacs' custom config location
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; winner-mode
(use-package winner
  :config (winner-mode 1))

;; zerodark-theme
(use-package zerodark-theme
  :ensure t
  :config (load-theme 'zerodark t)
          (zerodark-setup-modeline-format))

;; projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode 1))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))

;; ag
(use-package ag
  :ensure t
  :commands ag
  :init (setq ag-highlight-search t)
  :config (add-to-list 'ag-arguments "--word-regexp"))

;; ido
(use-package ido
  :ensure t
  :init (setq ido-enable-flex-matching t
              ido-everywhere t)
  :config (ido-mode 1)
          (ido-everywhere 1))

(use-package flx-ido
  :ensure t
  :init (setq ido-enable-flex-matching t)
  :config (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :init (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config (ido-vertical-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode 1))

;; magit
(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :commands (magit-status magit-checkout)
  :bind (("C-x g" . magit-status)))

;; diff - diff highlighting in gutter
(use-package diff-hl
  :ensure t
  :commands (turn-on-diff-hl-mode diff-hl-magit-post-refresh)
  :init (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
        (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
        (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; smex - ido for M-x
(use-package smex
  :ensure t
  :config (smex-initialize)
          (global-set-key (kbd "M-x") 'smex)
          (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; ruby
(use-package projectile-rails
  :ensure t
  :config (projectile-rails-global-mode 1))

;; slim
(use-package slim-mode
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; neotree
(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
          (setq neo-smart-open t))

;; html, css
(use-package web-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

;; js
(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t))

;;
