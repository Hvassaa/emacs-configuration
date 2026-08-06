(setopt custom-file "~/.emacs.d/custom.el")
(setopt ring-bell-function 'ignore)
(setopt truncate-lines t)
(setopt make-backup-files nil)
(setopt inhibit-startup-message t)
(which-key-mode 1)
(global-display-line-numbers-mode 1)
(pixel-scroll-precision-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(load-theme 'modus-vivendi)
(set-frame-font "Ubuntu Sans Mono Medium 12" nil t)

(keymap-set minibuffer-local-completion-map "C-n" 'minibuffer-next-completion)
(keymap-set minibuffer-local-completion-map "C-p" 'minibuffer-previous-completion)
(setopt completion-auto-help 'always)
(setopt completions-format 'one-column)
(setopt completion-show-help nil)
(setopt completion-ignore-case t)

(setopt mode-require-final-newline nil) ;; WTF

(with-eval-after-load 'eglot
  (progn
    (setopt eglot-connect-timeout nil)
    (keymap-set eglot-mode-map "C-c a" 'eglot-code-actions)
    (add-to-list 'eglot-server-programs
		 '(tsx-ts-mode
		   typescript-ts-mode
		   js-ts-mode . ("tsc" "--lsp" "--stdio")))))

(add-to-list 'auto-mode-alist '("\\.ts" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx" . tsx-ts-mode))
(setopt typescript-ts-mode-indent-offset 4)

(use-package corfu
  :ensure t
  :custom ((global-corfu-mode 1)
	   (corfu-auto 't)
	   (corfu-auto-prefix 1)
	   (corfu-preview-current nil)
	   (corfu-popupinfo-mode 1)
	   (corfu-popupinfo-delay 0.1)))

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package gdscript-mode
  :vc (:url "git@github.com:godotengine/emacs-gdscript-mode.git"))

(use-package sly
  :ensure t)
