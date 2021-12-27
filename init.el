;; do not use the customize interface, at all
(setq custom-file "/dev/null")
;; Disable auto backup
(setq make-backup-files nil)
;; disable the bell
(setq ring-bell-function 'ignore)
;; no scroll bar
(scroll-bar-mode -1)
;; show paren matches instantly
(show-paren-mode t)
(setq show-paren-delay 0)
;; line numbers
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'display-line-numbers-mode))
;; Spelling for text
(add-hook 'text-mode-hook 'flyspell-mode)
;; wrap lines
(add-hook 'text-mode-hook 'auto-fill-mode)

;;;;;;package manager config;;;;;
;;; Add the melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Package that shows command-completions
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; web-mode - mixing html/js/jsx/ts/tsx
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
	 ("\\.json\\'" . web-mode)))
  

;;; COMPANY ;;;
(use-package company
  :ensure t
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

;;; FLYCHECK ;;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; LSP ;;;
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (javascript-mode . lsp)
	 (typescript-mode . lsp)
	 (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deffered))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;;; LSP Optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-log-io nil) ; if set to true can cause a performance hit

;;; LaTeX ;;;
(use-package tex
  :defer t
  :hook (LaTeX-mode . reftex-mode) ; C-c = ...
  :ensure auctex
  :config
  ; standard stuff
  (setq TeX-save-query nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  ; use reftex with auctex
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-horizontally-fraction 0.3))

(provide 'init)
;;; init.el ends here
