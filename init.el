(setq custom-file "~/.emacs.d/custom.el")

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  :init (global-corfu-mode))

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

(global-display-line-numbers-mode t)
