;;; * Initialize packaging
(defun hgf/package-init ()
  "Initialize the package manager and install use-package."
  (progn
    (require 'package)
    (setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")
	    ("org" . "https://orgmode.org/elpa/")))
    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))))

(hgf/package-init)

(use-package use-package
  :config
  (setq use-package-always-ensure t))

;;; * Introduction
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

;;; ** No startup noise
(defun display-startup-echo-area-message ()
  (message "Don't panic!"))

(setq gc-cons-threshold 50000000
      ;; Quiet startup
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ;; Backups and saving
      make-backup-files nil
      auto-save-default nil
      ;; De facto smooth scrolling
      scroll-conservatively 100
      ;; Bells
      ring-bell-function 'ignore
      ;; VC symlinks
      vc-follow-symlinks t
      default-input-method "TeX")


;; Disabling things
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(fringe-mode '(0 . 0))

;; Enabling things
(global-visual-line-mode 1)
(show-paren-mode t)
(global-hl-line-mode t)

;; Shorter Prompts
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file (concat user-emacs-directory "custom.el")
      custom-theme-directory (concat user-emacs-directory "themes/"))
(when (file-exists-p custom-file)
  (load custom-file))

(defun hgf/load-user-init-file ()
  (interactive)
  (load-file user-init-file))

(defun hgf/find-or-load-user-init-file ()
  "Find the custom user init file if it's not the current buffer, otherwise load it."
  (interactive)
    (if (string-equal (buffer-file-name) (file-truename user-init-file))
	(load-file user-init-file)
      (find-file user-init-file)))

(use-package general
  :config
  (general-create-definer def-cc-key
    :prefix "C-c")
  (general-def
    "M-;" 'hgf/comment-or-uncomment-region-or-line
    "C-s" 'swiper
    "C-c d" 'magit-list-repositories
    "M-o" 'other-window)
  ;; Swapity swap
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(global-auto-revert-mode t)

(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil
			   :family "Inconsolata"
			   :height 120 ))
      ((eq system-type 'darwin)
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 120
			   :weight 'semi-bold))
      (t ;; for true operating system
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 100
			   :weight 'regular)))

(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil
	solarized-emphasize-indicators nil
	solarized-high-contrast-mode-line nil
	solarized-scale-org-headlines nil
	solarized-height-plus-1 1.0
	solarized-height-plus-2 1.0
	solarized-height-plus-3 1.0
	solarized-height-plus-4 1.0
	dark-theme 'solarized-dark
	light-theme 'solarized-light)
  (load-theme dark-theme t)
  (defun hgf/toggle-theme ()
    "Toggle between solarized variants."
    (interactive)
    (if (equal (car custom-enabled-themes) dark-theme)
	(progn
	  (disable-theme dark-theme)
	  (load-theme light-theme))
      (progn
	(disable-theme light-theme)
	(load-theme dark-theme)))))

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

;; Credit: https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun hgf/switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun hgf/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
  there's no active region. Credit to Harry R. Schwartz and his
  sensible-defaults package."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq-default fill-column 90)

(add-hook 'prog-mode-hook 'subword-mode)

(setq sentence-end-double-space nil)

(delete-selection-mode t)

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
  (setq org-publish-project-alist
	'(("org-notes"
	   :base-directory "~/Documents/blog/org/"
	   :base-extension "org"
	   :publishing-directory "~/Documents/blog/public_html/"
	   :recursive t
	   :publishing-function org-html-publish-to-html
	   :headline-levels 4
	   :auto-preamble t
	   )
	  ("org-static"
	   :base-directory "~/Documents/blog/org/"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	   :publishing-directory "~/Documents/blog/public_html/"
	   :recursive t
	   :publishing-function org-publish-attachment
	   )
	  (" org" :components ("org-notes" "org-static"))))
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
  (require 'ob-lilypond))

  (use-package htmlize)

(use-package ox-extra
  :ensure org-plus-contrib
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package lilypond-mode
  :ensure nil)

(use-package python-mode
  :config
  (setq py-shell-name "python3")
  (setq python-shell-interpreter "python3")
  (add-to-list 'exec-path "~/.local/bin"))

(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-shell-use-project-root nil))

(remove-hook 'elpy-modules 'elpy-module-flymake)
(remove-hook 'elpy-modules 'elpy-module-company)
(remove-hook 'elpy-modules 'elpy-module-eldoc)
(remove-hook 'elpy-modules 'elpy-module-django)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(defun hgf/python-mode-hook ()
  (progn
    (add-to-list 'company-backends 'company-jedi)
    (jedi:setup)))

(use-package company-jedi
  :config
  (add-hook 'python-mode-hook 'hgf/python-mode-hook)
  (setq jedi:complete-on-dot t))

(use-package blacken)

(setenv "WORKON_HOME" "~/.miniconda3/envs/")

(use-package fish-mode)

(use-package ledger-mode)

(use-package yaml-mode)

(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(c-add-style
 "linux-tabs-only"
 '("linux" (c-offsets-alist
	    (arglist-cont-nonempty
	     c-lineup-gcc-asm-reg
	     c-lineup-arglist-tabs-only))))

(defun ccc-astyle ()
  "Format C++ code with astyle."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning)
	      end (region-end))
      (setq beg (point-min)
	    end (point-max)))
    (shell-command-on-region
     beg end
     "astyle --style=linux -t"
     nil t)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux-tabs-only")))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux-tabs-only")))

(defun lisp-modes ())
(add-hook 'lisp-mode-hook 'lisp-modes)
(add-hook 'racket-mode-hook 'lisp-modes)
(add-hook 'emacs-lisp-mode-hook 'lisp-modes)

(use-package racket-mode)
(use-package scribble-mode)

(use-package slime
  :mode (("\\.cl\\'" . common-lisp-mode))
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package tex
  :defer t
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

(use-package rust-mode)
(use-package racer)
(use-package cargo)
(defun hgf--rust-hook ()
  (progn
    (racer-mode 1)
    (cargo-minor-mode 1)))

(add-hook 'rust-mode-hook 'hgf--rust-hook)

(defun hgf--eshell-hook ()
  (progn
    (def-g-key
      :keymaps 'eshell-mode-map
      "i" 'hgf/insert-end-of-buffer)
    (general-def 'eshell-mode-map
      [remap beginning-of-line] 'eshell-bol)
    (general-define-key
     :states 'normal
     :keymaps 'eshell-mode-map
     (kbd "C-p") 'eshell-previous-matching-input-from-input
     (kbd "C-n") 'eshell-next-matching-input-from-input)))

(add-hook 'eshell-mode-hook 'hgf--eshell-hook)

(setq eshell-visual-commands '(top))
(defalias 'ff #'find-file)

(use-package prog-mode
  :ensure nil
  :config
  (setq-default prettify-symbols-alist
		'(("#+BEGIN_SRC"     . "λ")
		  ("#+END_SRC"       . "λ")
		  ("#+begin_src"     . "λ")
		  ("#+end_src"       . "λ")))
  (global-prettify-symbols-mode t))

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

(defun hgf/evil-normal-state-if-evil ()
  (when (bound-and-true-p evil-local-mode) (evil-normal-state)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode)
  (setq evil-emacs-state-cursor '(bar)
	evil-search-module 'evil-search
	evil-ex-search-case 'smart)
  (general-def "M-~" 'evil-local-mode)
  (general-def 'normal
    "L" 'evil-end-of-visual-line
    "H" 'evil-first-non-blank-of-visual-line
    "C-s" 'swiper
    "C-u" 'evil-scroll-up)
  (general-def 'insert
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line
    "C-k" 'kill-line
    "C-x C-f" 'company-files
    "C-y" 'yank)
  (general-def 'visual
    "L" 'end-of-line
    "H" 'beginning-of-line)
  (advice-add 'keyboard-quit :before #'hgf/evil-normal-state-if-evil)
  (global-set-key [remap evil-next-line] 'evil-next-visual-line)
  (global-set-key [remap evil-previous-line] 'evil-previous-visual-line))
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

(defun hgf/insert-end-of-buffer ()
  (interactive)
  (progn
    (end-of-buffer)
    (evil-insert-state)))

(use-package evil-magit)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

(use-package which-key
  :config
  (which-key-mode))

(use-package olivetti
  :config
  (setq-default olivetti-body-width 95))

(use-package elfeed
  :config
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
	  "https://harryrschwartz.com/atom.xml"
	  "https://www.jvns.ca/atom.xml"
	  "https://emptysqua.re/blog/index.xml"
	  "http://feeds2.feedburner.com/stevelosh"))
  (defun hgf/olivetti () (olivetti-mode 1))
  (advice-add 'elfeed :after #'hgf/olivetti))

(use-package expand-region
  :config
  (general-def "C-c v" 'er/expand-region))

(use-package yasnippet
  :config
  (setq yas/indent-line t))
(yas-global-mode t)

(use-package magit)

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

(use-package company)
;; (add-hook 'after-init-hook 'global-company-mode)

(use-package outshine
  :config
  (setq outshine-startup-folded-p t))

(add-hook 'conf-mode-hook #'outshine-mode 1)
(add-hook 'prog-mode-hook #'outshine-mode 1)
(add-hook 'bibtex-mode-hook #'outshine-mode 1)
(add-hook 'LaTeX-mode-hook #'outshine-mode 1)

(use-package undo-tree)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

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

(use-package projectile
  :config
  (general-def '(normal visual insert) "C-p" 'projectile-find-file)
  (setq projectile-completion-system 'ivy
	projectile-switch-project-action 'projectile-dired
	projectile-require-project-root nil))
