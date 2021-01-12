;;; Set GUI preferences
(set-face-attribute 'default nil :height 150)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'display-line-numbers-mode))
(setq visible-bell t)
(setq inhibit-startup-message t)
;; Append color emoji font: 👌 😈
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;; Use y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable backup-stuff
(setq make-backup-files nil)
;(setq auto-save-default nil)

;;; Custom key binds
(global-set-key (kbd "C-c m") 'compile)
;;; auto fill-mode
(add-hook 'text-mode-hook 'auto-fill-mode)
;;; Custom functions
(defun latex-insert-image (s)
  "Insert image with S filename."
  (interactive "sFilename: ")
  (insert
   (concat (concat "\\begin{figure}[h!]\n  \\centering\n  \\includegraphics[width=8cm]{" s) "}\n\\end{figure}\n" )))

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
(dolist (mode '(eldoc-mode LaTeX-mode reftex-mode auto-fill-function))
  (diminish mode))

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

;;; filtering framework for selectrum and company
(use-package prescient
  :ensure t
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config (selectrum-prescient-mode +1))

(use-package company-prescient
  :ensure t
  :config (company-prescient-mode +1))

;;; Annotations in minibuffer completions (for Selectrum)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  ; -> taken from the github page <-
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations
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
  :ensure t)

;; LaTeX
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
  (setq reftex-plug-into-AUCTeX t))

;; END
