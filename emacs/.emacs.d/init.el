;; You see, yak shaving is what you are doing when you're doing some
;; stupid, fiddly little task that bears no obvious relationship to what
;; you're supposed to be working on, but yet a chain of twelve causal
;; relations links what you're doing to the original meta-task. -- Jeremy H. Brown

;; * Packaging Preparation
(defun hgf/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(hgf/package-init)
(setq use-package-always-ensure t)


;; * Change Customfile
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

;; * Packages Proper
;; These oughta be sorted
(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  :bind (("C-c R" . re-builder)))

;; * Start server
(server-mode 1)
;; * UI preferences
;; ** Personal info
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@protonmail.com")

;; ** Startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; ** Backups and saving
(setq make-backup-files nil
      auto-save-default nil)

;; ** Disable GUI elements
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; ** Shorter Prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; ** Fonts
(set-face-attribute 'default nil :family "IBM Plex Mono" :height 110)
(set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono" :height 110)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif" :height 110)

;; ** Theme
(use-package solarized-theme
  :config
  (setq x-underline-at-descent-line t
	solarized-use-variable-pitch nil
	solarized-height-plus-1 1.0
	solarized-height-plus-2 1.0
	solarized-height-plus-3 1.0
	solarized-height-plus-4 1.0
	solarized-high-contrast-mode-line t))

(setq dark-theme 'solarized-dark)
(setq light-theme 'solarized-light)

(defun hgf/toggle-theme ()
  "Toggle between solarized variants."
  (interactive)
  (if (equal (car custom-enabled-themes) dark-theme)
      (load-theme light-theme)
    (load-theme dark-theme)))

(load-theme dark-theme t)

;; ** Cursor
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(blink-cursor-mode 0)

;; ** Scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 10))
;; ** Modeline
(use-package minions
  :config (minions-mode 1))

;; ** Help me remember things
(use-package which-key
  :config
  (which-key-mode))

;; ** Parens
(show-paren-mode 1)
;; * Typing Text
;; ** Curious Characters
(setq default-input-method "TeX")
;; ** Filling
(setq fill-column 120)
;; ** Commenting
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))


;; ** Scripts
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; ** Completion
;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

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

;; ** Tex mode
(add-hook 'tex-mode-hook
	  (lambda ()
	    (progn
	      (setq ispell-parser 'tex)
	      (auto-fill-mode))))

;; ** Eshell
(use-package conda
  :config
  (conda-env-initialize-eshell))

(setq eshell-visual-commands '(top))
(defalias 'ff #'find-file)

;; ** Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

;; ** PDF
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;; ** Common Lisp
(use-package slime
  :mode (("\\.cl\\'" . common-lisp-mode))
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))
;; ** Fish shell
(use-package fish-mode)

;; * Minor mode configuration
;; ** Outline-minor
;; *** Init
(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :config
  (defvar outline-minor-mode-prefix (kbd "M-#"))
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  ;; Append, because otherwise some functionality might not be loaded yet
  (add-hook 'outline-minor-mode-hook (lambda () (outline-hide-sublevels 1)) 'append))

;; ** Evil
;; *** Init
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursor '(bar)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit)

(use-package evil-surround
  :config
  (add-hook 'evil-mode (lambda () (evil-surround-mode 1))))

;; *** Escape from Emacs state
(global-set-key (kbd "<escape>") 'evil-normal-state)
(when (window-system)
  ;; Separate C-[ and ESC when a window system is available
  (progn
    (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    (global-set-key (kbd "<C-[>") 'evil-normal-state)))

;; *** Keybindings
(defun hgf/outline-show-complete-outline ()
  "Outline: show all, then hide body."
  (interactive)
  (outline-show-all)
  (outline-hide-body))
;; ** ido, you do
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess)
(ido-mode 1)

;; * Magit
(use-package magit)

;; * Keybindings
;; ** Helpers
(defun hgf/ansi-term-fish ()
  (interactive)
  (ansi-term "/bin/fish"))

(defun hgf/term-fish ()
  (interactive)
  (term "/bin/fish"))

;; ** Global
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-c e") 'hgf/edit-or-load-user-init-file)
(global-set-key (kbd "C-c c") 'hgf/term-fish)
(global-set-key (kbd "C-c C") 'hgf/ansi-term-fish)

;; Buffering
(global-set-key (kbd "C-x C-b") 'ibuffer)



;; ** Evil
(add-hook 'outline-minor-mode-hook (lambda ()
				     (define-key evil-normal-state-local-map (kbd "M-j") 'outline-move-subtree-down)
				     (define-key evil-normal-state-local-map (kbd "M-k") 'outline-move-subtree-up)
				     (define-key evil-normal-state-local-map (kbd "M-h") 'outline-promote)
				     (define-key evil-normal-state-local-map (kbd "M-l") 'outline-demote)
				     (define-key evil-normal-state-local-map (kbd "<backtab>") 'outshine-cycle-buffer)))

;; I like Emacs' C-x [1-3,0] commands
(define-key evil-normal-state-map (kbd "C-w 1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-w 2") 'split-window-below)
(define-key evil-normal-state-map (kbd "C-w 3") 'split-window-right)
(define-key evil-normal-state-map (kbd "C-w 0") 'delete-window)

;; Comfortable scrolling (sorry universal-argument)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

(define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)

(define-key evil-normal-state-map (kbd "C-k") 'kill-line)
(define-key evil-visual-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
