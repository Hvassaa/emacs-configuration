(setq custom-file "~/.emacs.d/custom.el")
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package vertico
  :ensure t
  :config (vertico-mode t))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp)
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(use-package flycheck
  :ensure t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))


(global-display-line-numbers-mode t)
(setq ring-bell-function 'ignore)
