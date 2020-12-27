;;; Set GUI preferences
(set-face-attribute 'default nil :height 150)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'display-line-numbers-mode))
(setq visible-bell t)
(setq inhibit-startup-message t)
;; Append color emoji font: 👌 😈
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

;;; Custom key binds
(global-set-key (kbd "C-c m") 'compile)

;;; Add the melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; modus-operandi best theme
(use-package modus-themes
  :ensure t
  :config (load-theme 'modus-operandi t))

;;; Package to hide modes from the modeline (integrates with use-package)
(use-package diminish
  :ensure t)
(diminish 'eldoc-mode)

;;; Package that shows command-completions
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;;; Nicer mini-buffer completion, with vertical view
(use-package selectrum
  :ensure t
  :config (selectrum-mode +1))

;;; Annotations in minibuffer completions (for Selectrum)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))) 
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))) 

;;; Setup of company
(use-package company
  :ensure t
  :diminish
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  ; Use C-n and C-p for company navigation
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)  
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode))

;;; Use flycheck, syntax checker, instead of built-in flymake
(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode))

(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :ensure t
  :diminish
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (tuareg-mode . lsp)
	 (html-mode . lsp)
	 (javascript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)
(use-package lsp-ui
  :ensure t
  :diminish
  :commands lsp-ui-mode)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-log-io nil)
;; (setq lsp-idle-delay 0.500)
;; (setq lsp-completion-provider :capf)

;; OCAML
(use-package tuareg
  :hook (tuareg-mode . set-dovs-make)
  :ensure t)

(defun set-dovs-make ()
  (interactive)
  (setq compilation-read-command nil)
  (setq compile-command "make -k -C ~/overs-ttelse/x86 testx86 t='scratch'"))

;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-save-query nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t))
