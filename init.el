(setq ring-bell-function 'ignore)
(fido-vertical-mode 1)
(which-key-mode 1)
(global-display-line-numbers-mode 1)
(pixel-scroll-mode 1)

(use-package corfu
  :ensure t
  :custom ((global-corfu-mode 1)
	   (corfu-auto 't)
	   (corfu-auto-prefix 1)))

