;; * Preamble
;; Remove when Debian gets Emacs 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; * Base
;; ** Packaging
(use-package use-package
  :config
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (setq load-prefer-newer t))

;; ** Keybindings
(use-package general)

;; * Personal Information
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de"
      calendar-latitude 52.52
      calendar-latitude 13.41
      calendar-location-name "Berlin, Germany")

(setq gc-cons-threshold 200000000
      vc-follow-symlinks t
      sentence-end-double-space nil
      require-final-newline t
      confirm-kill-emacs 'y-or-n-p
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function 'ignore
      mode-line-default-help-echo nil
      show-paren-delay 0.0
      mouse-yank-at-point t
      default-input-method "TeX")

(fset 'yes-or-no-p 'y-or-n-p)

;; ** Backups
(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(fringe-mode 0)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; ** Scrolling
(setq scroll-margin 2
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; * Theming
;; ** Helpers
(defun hgf-load-theme (theme)
  "Disable all themes and load a new one."
  (interactive)
  (progn
    (hgf-disable-all-themes)
    (load-theme theme t)))

(defun hgf-disable-all-themes ()
  "Disable all custom enabled themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun hgf-toggle-theme ()
  "Toggle customvar between light and dark themes."
  (interactive)
  (if (equal (car custom-enabled-themes) 'solarized-dark)
      (hgf-load-theme 'solarized-light)
    (hgf-load-theme 'solarized-dark))
  (hgf-load-theme))

;; ** Solarized
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0
	solarized-distrinct-fringe-background t
	solarized-use-less-bold t
	solarized-use-less-italic t
	solarized-scale-org-headlines nil
	solarized-emphasize-indicators t)
  (load-theme 'solarized-dark t)
  (general-def "C-c z" 'hgf-toggle-theme))

;; ** Fonts
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil
			   :family "Consolas"
			   :height 110))
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
;; ** Modeline
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
	minions-mode-line-delimiters '("" . ""))
  (minions-mode 1)
  (column-number-mode 1))

;; * Typing text
(setq-default fill-column 100
	      cursor-type 'bar)

;; * Major modes
;; ** C
;; ** C++
;; ** Go
;; ** Python
;; ** Bash
;; ** Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

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
		   ("\\subsubsection{%s}" . "")))
    (add-to-list 'org-latex-classes
		 '("coverletter"
		   "\\documentclass[a4paper,11pt]{letter}\n[EXTRA]\n")))
  (general-def
    "C-c c" 'org-capture
    "C-c a" 'org-agenda
    "C-c t" (lambda () (interactive) (org-capture nil "t")))
  (use-package htmlize)
  (use-package ox-extra
    :ensure org-plus-contrib
    :config
    (ox-extras-activate '(ignore-headlines))))

;; ** Fish
(use-package fish-mode)

;; ** Ledger
(use-package ledger-mode)

;; ** YAML
(use-package yaml-mode)

;; ** TeX
(use-package tex
  :ensure nil
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq TeX-PDF-mode t))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

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

;; * Evil
(use-package evil
  :config
  (evil-mode 1)
  (general-def 'insert
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line)
  (general-def 'normal
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank))

;; * Minor modes
;; ** Which key
(use-package which-key
  :config
  (which-key-mode))


;; ** Olivetti
(use-package olivetti
  :config
  (setq-default olivetti-body-width 120))

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

;; ** Outshine
(use-package outshine
  :config
  (setq outshine-startup-folded-p t)
  (add-hook 'conf-mode-hook #'outshine-mode 1)
  (add-hook 'prog-mode-hook #'outshine-mode 1)
  (add-hook 'bibtex-mode-hook #'outshine-mode 1)
  (add-hook 'LaTeX-mode-hook #'outshine-mode 1))

;; ** Hydra
(use-package hydra
  :config
  (defhydra hydra-shell ()
    "Execute shell command."
    ("m" (start-process "hydramake" nil "make") "make"))
  (general-def
    "C-c s" 'hydra-shell/body))
;; * Custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
;; * Utils
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
