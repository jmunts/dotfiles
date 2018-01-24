;;; init.el ---  jmunts' Emacs init file

;; set package archive source
(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


(server-start)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(delete-selection-mode 1)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; js mode
(setq js-indent-level 2)

;; increased garbage collection threshold
(setq gc-cons-threshold 50000000)

(setq org-modules '(org-drill))

;; ansi-term
(setq term-buffer-maximum-size 0)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; use indentation in org
(setq org-startup-indented t)

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

(global-set-key (kbd "C-c t")
                (lambda () (interactive) (find-file "~/Dropbox/org/todo.org")))
(global-set-key (kbd "C-c s")
                (lambda () (interactive) (find-file "~/Dropbox/org/scratch.org")))

;; whick-key
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))

;; paredit
(use-package paredit
  :ensure t
  :init
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;; wrap-region
(use-package wrap-region
  :ensure   t
  :diminish wrap-region-mode
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\""))))

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
  :config (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t))

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

;; helm - http://tuhdo.github.io/helm-intro.html
(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (use-package helm-config)
          (setq helm-M-x-fuzzy-match        t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match    t)
          (helm-mode 1))
  :config (progn
            (helm-autoresize-mode t)
            (setq helm-autoresize-max-height 20
                  helm-autoresize-min-height 20)
            (use-package helm-projectile
              :ensure t
              :init (helm-projectile-on))
            (use-package helm-ag
              :ensure t)
            (use-package helm-descbinds
              :ensure t
              :init (helm-descbinds-mode)))
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-c h o" . helm-occur)
         ("C-x C-f" . helm-find-files)))

;; helm-swoop
(use-package helm-swoop
  :ensure t
  :bind ("M-i" . helm-swoop)
  )
(setq helm-swoop-split-with-multiple-windows t)

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

;; ruby
(use-package projectile-rails
  :ensure t
  :config (projectile-rails-global-mode 1))

;; rspec
(use-package rspec-mode
  :ensure t)

;; rubocop - rubocop gem is required
(use-package rubocop
  :ensure t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))

;; yaml
(use-package yaml-mode
  :ensure t)

;; smartparens
(use-package smartparens
  :ensure t
  :init
  (add-hook 'ruby-mode-hook   'smartparens-strict-mode)
  (add-hook 'elixir-mode-hook 'smartparens-strict-mode)
  (add-hook 'js2-mode-hook    'smartparens-strict-mode)
  (add-hook 'js-mode-hook     'smartparens-strict-mode)
  :diminish smartparens-mode)

(bind-keys
 :map smartparens-mode-map
 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp))
 
;; slim
(use-package slim-mode
  :ensure t)

;; haml
(use-package haml-mode
  :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

;; html, css
(use-package web-mode
  :ensure t
  :mode
  ("\\.jsx\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  :config
	(add-hook 'web-mode-hook 'my-web-mode-hook))

(defun my-web-mode-hook ()
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
)

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
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t))

;; iedit
(use-package iedit
  :ensure t)

;; avy
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char))
  :config (setq avy-background t))

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; tramp
(use-package tramp
  :ensure t
  :config (setq tramp-default-method "ssh"))

;; elixir
(use-package elixir-mode
  :ensure t
  :commands elixir-mode
  :config (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :ensure t
  :commands alchemist-mode)

;; elfeed

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config (setq elfeed-feeds
                '(("http://elixirstatus.com/rss" elixir elixirstatus)
                  ("https://www.erlang-solutions.com/news.rss" elixir news)
                  ("https://www.learnelixir.tv/feed/5b2f0e84-7e3b-4100-9b56-230831c76c8d" elixir learnelixir)
                  ("https://www.learnphoenix.tv/feed/27bcceba-6382-4f16-b910-e59f2577cd25" elixir phoenix learnphoenix)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsooa4yRKGN_zEE8iknghZA" ted)
                  ("https://emacs.cafe/feed.xml" emacs)
                  ("http://ergoemacs.org/emacs/blog.xml" emacs)
                  ("http://taylonr.com/feed/" blog)
                  ("http://confreaks.tv/confreaks-videos.atom" confreaks conf)
                  ("https://www.vbi.net/blog/feed/" elixir blog))))

(setq-default elfeed-search-filter "@6-months-ago")

;; highlight indent guide
(use-package highlight-indent-guides
  :ensure t
  :config (progn
            (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
            (setq highlight-indent-guides-method 'character)))

;; text scale mode
(use-package default-text-scale
  :ensure t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

;; emacs profiler
(use-package esup
  :ensure t)

;; xah snippets
(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-01-27"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (buffer-file-name)
               (buffer-file-name)
             (user-error "Current buffer is not associated with a file.")))))
    (kill-new
     (if *dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory -fpath))
           (file-name-directory -fpath))
       (progn
         (message "File path copied: 「%s」" -fpath)
         -fpath )))))


;; elm-mode dependencies
(use-package f
  :ensure t)

(use-package let-alist
  :ensure t)

(use-package s
  :ensure t)
;; elm
(use-package elm-mode
  :ensure t)

(setq elm-format-on-save t)

;; lorem ipsum
(use-package lorem-ipsum)

;; speed type
(use-package speed-type
  :ensure t)

(setq speed-type--gb-url-format
  "http://www.gutenberg.org/cache/epub/%d/pg%d.txt")

;; experimental speed up magit
(setq magit-commit-show-diff nil
      magit-revert-buffers 1)

;; boon - modal editing
;; (use-package boon
;;   :ensure t)
;; (use-package boon-qwerty
;;   :ensure t)
;; (use-package powerline
;;   :ensure t)
;; (use-package boon-powerline
;;   :ensure t)
;; (boon-powerline-theme)

;; evil
;; (use-package evil
;;   :ensure t)
;; (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;; (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
;; (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;; (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
;; (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
;; (evil-define-key 'normal neotree-mode-map (kbd "a") 'neotree-stretch-toggle)
;; (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
;; (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
;; (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
;; (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)

;; (evil-define-key 'normal helm-ag-mode-map (kbd "q") 'quit-window)
;; (evil-define-key 'normal helm-ag-mode-map (kbd "o") 'helm-ag-mode-jump-other-window)
;; (evil-mode 1)

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Dependencies: node.js, vmd
;; for github flavored markdown preview
(use-package vmd-mode
  :ensure t)

;; Dependency: multimarkdown cli
(use-package markdown-preview-mode
  :ensure t)

;; coffee-mode
(use-package coffee-mode
  :ensure)
(custom-set-variables '(coffee-tab-width 2))

;; custom functions
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key [(super shift return)] 'toggle-maximize-buffer)

;; fzf
(use-package fzf
  :ensure t)

;; json-mode
(use-package json-mode
  :ensure t)

;; code folding - yafolding
(use-package yafolding
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'yafolding-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t)

(global-set-key (kbd "C-c m c") 'mc/edit-lines) ; TODO: doesn't work, fix and re-bind
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; undo tree
(use-package undo-tree
  :ensure t)

(global-undo-tree-mode)

;; ediff
(use-package ediff
  :ensure t)

;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; vuejs
(use-package vue-mode
  :ensure t)

;; auto complete
(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))

;; dimmer - highlight current buffer
(use-package dimmer
  :ensure t
  :init
  (dimmer-activate)
  :config
  (setq dimmer-exclusion-regexp ".*helm.*"
        dimmer-percent 0.3))

;; check package info
(use-package pkg-info
  :ensure t)

;; auto update packages
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 5)
   (auto-package-update-maybe))

(provide 'init)
;;; init.el ends here
