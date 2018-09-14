;; You see, yak shaving is what you are doing when you're doing some
;; stupid, fiddly little task that bears no obvious relationship to what
;; you're supposed to be working on, but yet a chain of twelve causal
;; relations links what you're doing to the original meta-task. -- Jeremy H. Brown

;; * Packaging init 
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(defun hgf/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(setq use-package-always-ensure t)

(hgf/package-init)

;; * Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; * Meta
(setq my-config-file "~/.emacs.d/init.el")
(defun hgf/edit-or-load-user-init-file ()
  "Find the custom user init file if it's not the current buffer, otherwise load the proper one."
  (interactive)
  (if (string-equal (buffer-file-name) (file-truename my-config-file))
      (load-file user-init-file)
    (find-file my-config-file)))
(global-set-key (kbd "C-c .") 'hgf/edit-or-load-user-init-file)

;; * Packages
(use-package which-key
  :config
  (which-key-mode))

(use-package magit)

(use-package markdown-mode
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

;; * UI preferences
;; ** Personal info
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@protonmail.com")

;; ** Some annoying defaults
(setq make-backup-files nil
      auto-save-default nil
      indent-tabs-mode nil)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; ** Fonts
(set-face-attribute 'default nil :family "IBM Plex Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono" :height 120)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif" :height 120)

;; ** Theme
(setq dark-theme 'solarized-dark)
(setq light-theme 'solarized-light)

(setq solarized-high-contrast-mode-line t
      x-underline-at-descent-line t
      solarized-use-variable-pitch nil
      solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0)

(defun hgf/toggle-theme ()
  "Toggle between solarized variants."
  (interactive)
  (if (equal (car custom-enabled-themes) dark-theme)
      (load-theme light-theme)
    (load-theme dark-theme)))

(load-theme dark-theme t)

;; ** Commenting
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; ** ido, you do
(setq ido-enable-flex-matching t
      ido-everywhere t)
(setq enable-recursive-minibuffers t)
(ido-mode 1)
;; * General hooks
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; * Major mode configuration
;; ** Org mode
(add-hook 'org-mode-hook (lambda () (progn
                                      (auto-fill-mode))))

(setq org-adapt-indentation nil
      org-hide-leading-stars t
      org-startup-indented t)
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; ** Text mode
(add-hook 'text-mode-hook
          (lambda () (auto-fill-mode)))

(add-hook 'tex-mode-hook
          (lambda ()
              (progn
                (setq ispell-parser 'tex)
                (auto-fill-mode))))

;; ** Eshell
(setq eshell-visual-commands '(top))

;; * Minor mode configuration
;; ** Outline-minor
;; *** Init
(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :config
  (defvar outline-minor-mode-prefix (kbd "M-#"))
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

;; ** Evil
;; *** Init
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs))

(defalias 'evil-insert-state 'evil-emacs-state)

;; *** Additional evil packages
(use-package evil-surround
  :hook evil)

;; *** Escape from Emacs state
(global-set-key (kbd "<escape>") 'evil-normal-state)
(when (window-system)
  ;; Separate C-i and TAB when a window system is available
  (progn
    (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    (global-set-key (kbd "<C-[>") 'evil-normal-state)))

;; *** Keybindings
(define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "<backtab>") 'outline-cycle)
(define-key evil-normal-state-map (kbd "M-j") 'outline-move-subtree-down)
(define-key evil-normal-state-map (kbd "M-k") 'outline-move-subtree-up)
(define-key evil-normal-state-map (kbd "M-h") 'outline-promote)
(define-key evil-normal-state-map (kbd "M-l") 'outline-demote
;; * Keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
