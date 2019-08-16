;; * Licensing
;; SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
;; SPDX-License-Identifier: MIT
;; * Bootstrap
(load "~/.emacs.d/bootstrap.el")

;; * =use-package=
(use-package use-package
  :config
  (setq use-package-always-ensure t))

(use-package package-utils)

;; * Startup benchmarking
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; * General keybindings
(use-package general
  :config
  (general-def
    "C-x C-r" 'hgf-rename-this-file
    "C-x C-k" 'hgf-delete-this-file
    "C-c k" 'kill-this-buffer
    "M-j" 'hgf-join-line
    "M-i" 'imenu
    "M-o" 'other-window
    "M-i" 'imenu
    "C-c b" 'hgf-switch-to-previous-buffer)
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

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
      initial-scratch-message ";; Don't panic!\n\n"
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
(setq-default fill-column 90
      sentence-end-double-space nil)
(setq default-input-method "TeX"
      mouse-yank-at-point t
      require-final-newline t)
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
  (defun hgf-toggle-theme ()
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
  (general-def "C-c z" 'hgf-toggle-theme)
  (hgf-toggle-theme))

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


;; * Helper functions
;; ** Switch to previous buffer
(defun hgf-switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ** Join lines
(defun hgf--join-lines-region ()
  (let ((n (hgf--count-lines-region))
	(flip-p (eq (region-end) (point))))
    (progn
      (when flip-p (exchange-point-and-mark))
      (dotimes (_ (max 1 (1- n))) (join-line -1)))))

(defun hgf--count-lines-region ()
  (interactive)
  (let ((numlines (count-lines (region-beginning) (region-end)))
	(beginning-of-line-p (= (line-beginning-position) (point))))
    (if beginning-of-line-p
	(1+ numlines)
      numlines)))

(defun hgf-join-line ()
  (interactive)
  (if (region-active-p)
      (hgf--join-lines-region)
    (join-line -1)))

;; ** Delete file
(defun visiting-file-p ()
  (let ((filename (buffer-file-name)))
    (and filename (file-exists-p filename))))

(defun hgf-delete-this-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (visiting-file-p))
	(kill-buffer buffer)
      (when (yes-or-no-p "Delete this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File %s successfully removed" filename)))))

;; ** Rename file
(defun hgf-rename-this-file ()
  "Renames current buffer and associated file."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (visiting-file-p))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

;; ** Close all magit buffers
(defun hgf-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; ** Threading macro
(defmacro ~> (init &rest lst)
  "Evaluate transformation pipeline. Either append argument at
the end or replace all :arg occurences.

Example:
(pipe (number-sequence 1 10)
      (-filter 'evenp)
      (mapcar '1+))
=> (3 5 7 9 11)

(pipe (number-sequence 1 10)
    (-filter 'evenp)
    (mapcar '1+)
    (-filter (lambda (x) (= 0 (mod x 3))))
    (mapcar '1+)
    (-filter (lambda (x) (= 0 (mod x 5)))))
=> (10)

(pipe 1
      (1+)
      (1+)
      ((lambda (x y) (+ y x)) :arg 100))
=> 103"
  (reduce (lambda (acc el)
	    (if (member :arg el)
		(-replace :arg acc el)
	      (append el `(,acc))))
	  lst
	  :initial-value init))

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
	org-cycle-separator-lines 0
	org-latex-with-hyperref nil)
  (setq org-agenda-files
	'("~/.journal/tasks.org"
	  "~/.journal/inbox.org"))
  (setq org-archive-location "~/.journal/archive.org::* From %s")
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
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
    (add-to-list 'org-latex-classes
		 '("ieee"
		   "\\documentclass{IEEEtran}\n[NO-DEFAULT-PACKAGES]\n[EXTRA]\n"
		   ("\\section{%s}" . "")
		   ("\\subsection{%s}" . "")
		   ("\\subsubsection{%s}" . "")))
    (add-to-list 'org-latex-classes
		 '("blank"
		   ""
		   ("\\section{%s}" . "")
		   ("\\subsection{%s}" . "")
		   ("\\subsubsection{%s}" . ""))))
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

;; ** Lisp
;; *** All lisps
(use-package rainbow-delimiters)

;; *** Racket
(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode)))
(use-package scribble-mode)
(use-package quack)
(use-package geiser)

;; *** Common Lisp
(use-package slime
  :mode (("\\.cl\\'" . common-lisp-mode))
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; ** TeX
;; (use-package tex
;;   :ensure nil
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-master nil)
;;   (setq TeX-PDF-mode t))

;; (use-package auctex-latexmk
;;   :ensure nil
;;   :config
;;   (auctex-latexmk-setup)
;;   (setq auctex-latexmk-inherit-TeX-PDF-mode t))

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
  (setq eshell-cmpl-ignore-case t
	eshell-cmpl-cycle-cutoff-length nil
	eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|\\.svn\\|\\.git\\)/\\'"
	eshell-banner-message "Don't panic!\n")
  (general-def "<f1>" 'eshell)
  (add-hook 'eshell-mode-hook (lambda () (setq-local cursor-type 'bar))))

;; *** pcomplete
(defconst pcmpl-exercism-commands
  '("configure" "download" "help" "open" "prepare"
    "submit" "troubleshoot" "upgrade" "workspace"
    "version"))

(defun pcomplete/exercism ()
  "Completion for `exercism'"
  (pcomplete-here* pcmpl-exercism-commands)

  (if (pcomplete-match "help")
      (pcomplete-here* pcmpl-exercism-commands)
    (while (pcomplete-here (pcomplete-entries)))))

;; ** C#
(use-package csharp-mode)

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
  (defhydra hydra-package (:exit t)
    "Package management"
    ("r" (package-refresh-contents) "refresh")
    ("i" (call-interactively #'package-install) "install")
    ("u" (package-utils-upgrade-all) "upgrade")
    ("d" (call-interactively #'package-delete) "delete"))
  (defhydra hydra-project (:exit t)
    "Package management"
    ("c" (call-interactively #'compile) "compile"))
  (general-def
    "C-c w" 'hydra-window/body
    "C-c f" 'hydra-freq-files/body
    "C-c i" 'hydra-package/body
    "C-c p" 'hydra-project/body))

;; ** Magit
(use-package magit
  :config
  (general-def "C-c d" 'magit-list-repositories)
  (general-def magit-status-mode-map "q" #'hgf-magit-kill-buffers))

;; *** Repolist
(defun hgf-list-subdirs (dir)
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

(defun hgf-contains-git-repo-p (dir)
  "Check if there's  a .git directory in DIR."
  (let ((dirs (directory-files dir)))
    (member ".git" dirs)))

(defun hgf-filter-git-repos (dirs)
  "Remove all directories without a .git subdirectory in DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (when (hgf-contains-git-repo-p dir)
	(add-to-list 'result dir)))
    result))

(defun hgf-make-magit-repolist (dirs)
  "Make a list of the form (dir 0) for the magit-list-repositories function."
  (let ((result))
    (dolist (dir dirs result)
      (add-to-list 'result `(,dir 0)))
    result))

(defun hgf-repolist-refresh ()
  (setq magit-repository-directories
	(hgf-make-magit-repolist
	 (hgf-filter-git-repos
	  (hgf-list-subdirs "~/Development")))))

(advice-add 'magit-list-repositories :before #'hgf-repolist-refresh)

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
  (defun hgf-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
			  'box
			'bar)))
  (add-hook 'god-mode-enabled-hook 'hgf-update-cursor)
  (add-hook 'god-mode-disabled-hook 'hgf-update-cursor)
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

;; ** =comment-dwim-2=
(use-package comment-dwim-2
  :config
  (general-def "M-;" 'comment-dwim-2))
;; ** =memento-mori=
(use-package memento-mori
  :config
  (setq memento-mori-birth-date "1996-10-03"
	initial-scratch-message (format ";; You are %.3f years old!\n\n" (memento-mori-age)))
  (memento-mori-mode 1))

;; ** =smartparens=
(use-package smartparens
  :config
  (general-def
    "C-c ei" 'sp-change-inner))

;; ** =selected=
(defun hgf-shell-command-on-region-replace ()
  "Just like its namesake, but always replace."
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively #'shell-command-on-region)))

(defun hgf-tabulate-region ()
  "Use `column` to tabulate region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "column -t -o' '" :replace t))

(use-package selected
  :commands selected-minor-mode
  :config
  (general-def selected-keymap
    "u" 'upcase-region
    "d" 'downcase-region
    "w" 'count-words-region
    "i" 'string-insert-rectangle
    "!" 'hgf-shell-command-on-region-replace
    "t" 'hgf-tabulate-region
    "s" 'replace-string
    "x" 'exchange-point-and-mark
    "p" 'previous-line
    "n" 'next-line
    "v" 'rectangle-mark-mode
    "<backspace>" 'kill-region))

;; * Custom file
(load (format "~/.emacs.d/machine/%s/post.el" (getenv "HOSTNAME")) 'noerror)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
