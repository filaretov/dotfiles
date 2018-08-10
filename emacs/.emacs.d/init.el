;; Look, mom, I'm lisping!
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

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

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  :bind (("C-c R" . re-builder)))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package interleave
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "ef98b560dcbd6af86fbe7fd15d56454f3e6046a3a0abd25314cfaaefd3744a9e" "c856158cc996d52e2f48190b02f6b6f26b7a9abd5fea0c6ffca6740a1003b333" "7d2e7a9a7944fbde74be3e133fc607f59fdbbab798d13bd7a05e38d35ce0db8d" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "TeX")
 '(fill-column 79)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (dracula-theme interleave pdf-tools whole-line-or-region markdown-mode magit which-key evil use-package)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq solarized-high-contrast-mode-line t)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-light t)

(ido-mode 1)
(global-hl-line-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq eshell-visual-commands '(top))

(setq enable-recursive-minibuffers t)

;;; Keybindings
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-i") 'imenu)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;; Filetype hooks
(add-hook 'tex-mode-hook
	  #'(lambda () (setq ispell-parser 'tex)))

(add-hook 'org-mode-hook
	  #'(lambda () (auto-fill-mode)))

;;; General hooks
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(defun edit-or-load-user-init-file ()
  (interactive)
  (if (string-equal (buffer-file-name) (file-truename user-init-file))
      (load-file user-init-file)
    (find-file user-init-file)))

(global-set-key (kbd "C-.") 'edit-or-load-user-init-file)
