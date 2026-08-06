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

(with-eval-after-load 'eglot
  (setopt eglot-connect-timeout nil)
  (keymap-set eglot-mode-map "C-c a" 'eglot-code-actions))

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
