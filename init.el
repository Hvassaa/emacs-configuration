(setopt custom-file "~/.emacs.d/custom.el")
(setopt ring-bell-function 'ignore)
(setopt truncate-lines t)
(setopt make-backup-files nil)
(setopt inhibit-startup-message t)
(which-key-mode 1)
(global-display-line-numbers-mode 1)
(pixel-scroll-precision-mode 1)
(load-theme 'modus-vivendi)

(setopt eglot-connect-timeout nil)

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


(use-package gdscript-mode
  :vc (:url "git@github.com:godotengine/emacs-gdscript-mode.git"))

(use-package sly
  :ensure t)


; (defun java-extract-local-variable ()
;   ""
;   (interactive)
;   ;; (eglot-code-actions (point-min) (point-max) "Assign statement to new local variable")))
;   (eglot-code-actions (line-beginning-position) (line-end-position) "Assign statement to new local variable"))
