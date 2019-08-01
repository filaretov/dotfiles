;; * Initialize packaging
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(cond
 ((string-equal "quirm" (getenv "HOSTNAME"))
  (progn
    (setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")
	    ("org" . "https://orgmode.org/elpa/")))
    (setq url-proxy-services '(("http" . "153.96.56.101:3128")
			       ("https" . "153.96.56.101:3128")
			       ("no_proxy" . "^\\(localhost\\|127.*\\)"))))))


(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(use-package package-utils)

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; * Default wrangling

;; ** Hi, my name is
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

;; ** Visual clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fringe-mode 0)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message ";; Don't panic\n\n"
      ring-bell-function 'ignore
      mode-line-default-help-echo nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; ** Backups
(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

;; ** Convenience
(fset 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

;; ** Typing text
(setq-default fill-column 90)
(setq default-input-method "TeX"
      sentence-end-double-space nil)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(delete-selection-mode t)

;; ** Files
(global-auto-revert-mode 1)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ** Visual cues
(show-paren-mode 1)
(global-hl-line-mode 1)

;; ** Scrolling
(setq scroll-margin 2
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; * Visual Appearance
;; ** Cursor
(setq-default cursor-type 'bar)

;; ** Font
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil
			   :family "Inconsolata"
			   :height 120 ))
      ((eq system-type 'darwin)
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 120
			   :weight 'semi-bold))
      (t ;; t for true operating system
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 100
			   :weight 'regular)))

;; ** Custom directory
(setq custom-theme-directory (concat user-emacs-directory "themes/"))

;; ** Theme
(use-package gruvbox-theme
  :config
  (defun hgf/toggle-theme ()
    "Toggle between solarized variants."
    (interactive)
    (let ((dark-theme 'gruvbox-dark-hard)
	  (light-theme 'gruvbox-light-hard))
      (if (equal (car custom-enabled-themes) dark-theme)
	  (progn
	    (disable-theme dark-theme)
	    (load-theme light-theme t))
	(progn
	  (disable-theme light-theme)
	  (load-theme dark-theme t)))))
  (general-def "C-c z" 'hgf/toggle-theme)
  (hgf/toggle-theme))

;; ** Modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (column-number-mode t))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
	minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; * general.el keybindings
(use-package general
  :config
  (general-def
    "M-o" 'other-window
    "M-;" 'comment-dwim-2
    "C-c b" 'hgf/switch-to-previous-buffer)
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

;; * Helper functions

;; ** Switch to previous buffer
(defun hgf/switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; * Major modes
;; ** Org
(use-package org
  :config
  (setq org-adapt-indentation t
	org-hide-leading-stars t
	org-src-fontify-natively t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t
	org-goto-interface 'outline-path-completionp
	org-outline-path-complete-in-steps nil
	org-M-RET-may-split-line nil
	org-cycle-separator-lines 0)
  (setq org-agenda-files
	'("~/.journal/tasks.org"
	  "~/.journal/inbox.org"))
  (setq org-archive-location "~/.journal/archive.org::* From %s")
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d)")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/.journal/tasks.org")
	   "* TODO %?\n")
	  ("n" "Note" entry (file "~/.journal/notes.org")
	   "*  %?\n")
	  ("i" "In" entry (file "~/.journal/inbox.org")
	   "* TODO %?\nSCHEDULED: %t")))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
		 '("book"
		   "\\documentclass{book}\n[NO-DEFAULT-PACKAGES]\n[EXTRA]\n"
		   ("\\chapter{%s}" . "\\chapter*{%s}")
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  (general-def
    "C-c c" 'org-capture
    "C-c a" 'org-agenda
    "C-c t" (lambda () (interactive) (org-capture nil "t")))
  (use-package htmlize)
  (use-package ox-extra
    :ensure org-plus-contrib
    :config
    (ox-extras-activate '(ignore-headlines))))

;; ** Python
(use-package python-mode
  :config
  (setq py-shell-name "python3")
  (setq python-shell-interpreter "python3")
  (add-to-list 'exec-path "~/.local/bin"))

(use-package blacken)

;; ** Fish
(use-package fish-mode)

;; ** Ledger
(use-package ledger-mode)

;; ** YAML
(use-package yaml-mode)

;; ** Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

;; ** LISP
(use-package racket-mode)
(use-package scribble-mode)

(use-package slime
  :mode (("\\.cl\\'" . common-lisp-mode))
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; ** TeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq TeX-PDF-mode t))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(defun hgf--latex-hook ()
  (progn
    (setq ispell-parser 'tex)
    (auto-fill-mode t)
    (TeX-source-correlate-mode 1)))

(add-hook 'LaTeX-mode-hook 'hgf--latex-hook)

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)

(defun hgf--bibtex-hook ()
  (progn
    (setq comment-start "%")))

(add-hook 'bibtex-mode-hook 'hgf--bibtex-hook)

(setq-default TeX-auto-save t
              TeX-parse-self t
              TeX-PDF-mode t
              TeX-auto-local "~/.emacs.d/auctex-auto")
(setq bibtex-dialect 'biblatex)

;; ** Eshell
(use-package eshell
  :config
  (general-def "<f1>" 'eshell)
  (add-hook 'eshell-mode-hook (lambda () (setq-local cursor-type 'bar))))

;; * Minor modes
;; ** Olivetti
(use-package olivetti
  :config
  (setq-default olivetti-body-width 95))

;; ** Hydra
(use-package hydra
  :config
  (defhydra hydra-window ()
    "Window management"
    ("o" other-window "other")
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("s" split-window-below "sp-below")
    ("v" split-window-right "sp-right")
    ("d" delete-window "delete")
    ("f" find-file "file")
    ("b" ivy-switch-buffer "buffer")
    ("m" kill-this-buffer "murder")
    ("1" delete-other-windows "highlander")
    ("." nil "stop"))
  (defhydra hydra-freq-files (:exit t)
    "Frequent files"
    ("e" (find-file user-init-file) "conf")
    ("i" (find-file "~/.journal/inbox.org") "inbox")
    ("n" (find-file "~/.journal/notes.org") "notes")
    ("u" (find-file "~/.journal/uniplan.org") "uniplan")
    ("t" (find-file "~/.journal/time.ledger") "time")
    ("w" (find-file "~/.config/i3/config") "i3wm")
    ("p" (find-file "~/Development/crucible/tasks/packages.yml") "packages"))
  (general-def
    "C-c w" 'hydra-window/body
    "C-c f" 'hydra-freq-files/body))
;; ** Magit
(use-package magit
  :config
  (general-def "C-c d" 'magit-list-repositories))

;; *** Repolist
(defun hgf/list-subdirs (dir)
  "List all subdirs, not recursive, absolute names, DIR shouldn't have a / at the end."
  (let ((base dir)
	(result))
    (dolist (f (directory-files base) result)
      (let ((name (concat base "/" f)))
	(when (and (file-directory-p name)
		   (not (equal f ".."))
		   (not (equal f ".")))
	  (add-to-list 'result name))))
    result))

(defun hgf/contains-git-repo-p (dir)
  "Check if there's  a .git directory in DIR."
  (let ((dirs (directory-files dir)))
    (member ".git" dirs)))

(defun hgf/filter-git-repos (dirs)
  "Remove all directories without a .git subdirectory in DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (when (hgf/contains-git-repo-p dir)
	(add-to-list 'result dir)))
    result))

(defun hgf/make-magit-repolist (dirs)
  "Make a list of the form (dir 0) for the magit-list-repositories function."
  (let ((result))
    (dolist (dir dirs result)
      (add-to-list 'result `(,dir 0)))
    result))

(defun hgf/repolist-refresh ()
  (setq magit-repository-directories
	(hgf/make-magit-repolist
	 (hgf/filter-git-repos
	  (hgf/list-subdirs "~/Development")))))

(advice-add 'magit-list-repositories :before #'hgf/repolist-refresh)

(setq magit-repolist-columns
      '(("Name" 12 magit-repolist-column-ident nil)
	("Branch" 10 magit-repolist-column-branch nil)
	("Dirty" 6 magit-repolist-column-dirty nil)
	("B<U" 3 magit-repolist-column-unpulled-from-upstream
	 ((:right-align t)
	  (:help-echo "Upstream changes not in branch")))
	("B>U" 3 magit-repolist-column-unpushed-to-upstream
	 ((:right-align t)
	  (:help-echo "Local changes not in upstream")))
	("Version" 30 magit-repolist-column-version nil)
	("Path" 99 magit-repolist-column-path nil)))

;; ** Outshine
(use-package outshine
  :config
  (setq outshine-startup-folded-p t)
  (add-hook 'conf-mode-hook #'outshine-mode 1)
  (add-hook 'prog-mode-hook #'outshine-mode 1)
  (add-hook 'bibtex-mode-hook #'outshine-mode 1)
  (add-hook 'LaTeX-mode-hook #'outshine-mode 1))

;; ** Which key
(use-package which-key
  :config
  (which-key-mode))

;; ** Ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-initial-inputs-alist nil
	count-format "(%d/%d) "))

(use-package counsel
  :config
  (counsel-mode 1)
  (use-package flx)
  (use-package smex))

;; ** God mode
(use-package god-mode
  :config
  (defun hgf/update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
			  'box
			'bar)))
  (add-hook 'god-mode-enabled-hook 'hgf/update-cursor)
  (add-hook 'god-mode-disabled-hook 'hgf/update-cursor)
  (general-def "M-`" 'god-mode-all)
  (general-def god-local-mode-map
    "i" 'god-mode-all
    "." 'repeat))

;; * Utilities
;; ** =expand-region=
(use-package expand-region
  :config
  (general-def "C-c v" 'er/expand-region))

;; ** Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode t)
  (setq yas/indent-line t))

;; ** =change-inner=
(use-package change-inner)

;; * Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
