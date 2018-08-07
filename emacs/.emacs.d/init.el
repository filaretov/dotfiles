;; Look, mom, I'm lisping!
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package evil
  :config
  (evil-mode 0))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :ensure t)

(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  :bind (("C-c R" . re-builder)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "TeX")
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (magit which-key evil use-package)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-light t)

(ido-mode 1)
(global-hl-line-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq eshell-visual-commands '(top))

(setq next-line-add-newlines t
      enable-recursive-minibuffers t)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(define-key global-map (kbd "RET") 'newline-and-indent)
