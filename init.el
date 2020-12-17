;;; Set GUI preferences
(load-theme 'wheatgrass)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq visible-bell t)
(setq inhibit-startup-message t)
(icomplete-mode t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Package that shows command-completions
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Setup of company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :hook
  (prog-mode . company-mode))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (tuareg-mode . lsp)
          (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)
(use-package lsp-ui
  :ensure t
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

(global-set-key (kbd "C-c m") 'compile)
