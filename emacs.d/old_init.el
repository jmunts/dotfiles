
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet) 
(projectile-global-mode)

(windmove-default-keybindings)

;; Use evil-mode
(evil-mode 1)
(global-evil-surround-mode 1)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)


;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; indentation

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-always-indent 'complete)

;; UI
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode t)
(when window-system
  (scroll-bar-mode -1))

(load-theme 'arjen-grey t)

;; Projectile
(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point)

;; Use the diff-hl package to highlight changed-and-uncommitted lines when programming.
(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; Two spaces by characters
(setq-default tab-width 2)

;; smex for ido in M-x
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; magit bind magit-status globally
(global-set-key (kbd "C-x g") 'magit-status)

;; magit hook to refresh diff-hl after commt
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; ruby-mode hooks
(projectile-rails-global-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(global-company-mode t)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; slim
(require 'slim-mode)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; alchemist
(require 'alchemist)

;; smartparens
(smartparens-global-mode t)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; gc threshold
(setq gc-cons-threshold 50000000)
(setq gnutls-min-prime-bits 4096)
