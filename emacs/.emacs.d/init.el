;;; init.el --- my attempt at configuring Emacs

;;; Commentary:
;;; Yak shaving is a way of life.

;;; Code:
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
(use-package general
  :config
  (general-def
    "M-i" 'imenu))

;; * Theming
;; ** Solarized
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0
	solarized-distinct-fringe-background t
	solarized-use-less-bold t
	solarized-scale-org-headlines nil
	solarized-emphasize-indicators t)
  (general-def "C-c z" 'my/toggle-theme))

;; ** Dracula
(use-package dracula-theme)

;; ** Nord
(use-package nord-theme)

;; ** Helpers
(defun my/load-theme (theme)
  "Disable all themes and load THEME."
  (interactive (list
		(intern
		 (completing-read "Load custom theme: "
				  (mapcar 'symbol-name
					  (custom-available-themes))))))
  (progn
    (my/disable-all-themes)
    (load-theme theme t)))

(defun my/disable-all-themes ()
  "Disable all custom enabled themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/toggle-theme ()
  "Toggle customvar between light and dark themes."
  (interactive)
  (let ((dark 'nord)
	(light 'solarized-light-high-contrast))
    (if (equal (car custom-enabled-themes) dark)
	(my/load-theme light)
      (my/load-theme dark))))

(my/load-theme 'dracula)

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
			   :family "Fira Code"
			   :height 100
			   :weight 'normal)))

(defun my/org-mode-hook ()
  "Disable header variable font size."
  (progn
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5
		    org-document-title))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
  (set-face-attribute 'org-block nil :foreground nil))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(defun my/outline-mode-hook ()
  "Disable header variable font size."
  (progn
    (dolist (face '(outline-1
		    outline-2
		    outline-3
		    outline-4
		    outline-5
		    outline-6
		    outline-7
		    outline-8))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0))))

(add-hook 'outshine-mode-hook 'my/outline-mode-hook)
(add-hook 'outline-mode-hook 'my/outline-mode-hook)
;; ** Modeline
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
	minions-mode-line-delimiters '("" . ""))
  (minions-mode 1)
  (column-number-mode 1))

;; * Personal Information
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

;; * Default wrangling
;; ** Enable all
(global-auto-revert-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; ** Disable all
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(fringe-mode 0)

;; ** Tweak
(setq vc-follow-symlinks t
      sentence-end-double-space nil
      require-final-newline t
      confirm-kill-emacs 'y-or-n-p
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'org-mode
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


;; ** Scrolling
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; * Typing text
(setq-default fill-column 100
	      cursor-type 'bar)

;; * Code Completion
(use-package yasnippet)

;; * Major modes
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
	org-outline-path-complete-in-steps nil
	org-M-RET-may-split-line nil
	org-cycle-separator-lines 0
	org-latex-hyperref-template nil)
  (setq org-agenda-files
	'("~/cloud/journal/tasks.org"
	  "~/cloud/journal/inbox.org"))
  (setq org-archive-location "~/cloud/journal/archive.org::")
  (setq org-capture-templates
	'(("n" "Note" entry (file "~/cloud/journal/notes.org")
	   "*  %?\n")
	  ("i" "Inbox" entry (file "~/cloud/journal/inbox.org")
	   "* TODO %?\n")))
  (setq org-todo-keywords
	'((sequence "TODO" "DONE")
	  (sequence "READ" "WATCH" "LISTEN" "DONE")))
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
    :commands ox-extras-activate
    :ensure org-plus-contrib
    :config
    (ox-extras-activate '(ignore-headlines))))


;; ** Ledger
(use-package ledger-mode)

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

(defun my/bibtex-hook ()
  "My bibtex hook."
  (progn
    (setq comment-start "%")))

(add-hook 'bibtex-mode-hook 'my/bibtex-hook)

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
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursos 'bar
	evil-search-module 'evil-search
	evil-ex-search-case 'smart)
  (general-def 'normal
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank-of-visual-line
    "C-s" 'swiper
    "C-u" 'evil-scroll-up)
  (general-def 'insert
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line
    "C-k" 'kill-line
    "C-y" 'yank))

(use-package evil-magit)

(use-package evil-numbers)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

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

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

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
  (defhydra hydra-shell (:exit t)
    "Execute shell command."
    ("m" (start-process "hydramake" nil "make") "make"))
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
  (defhydra hydra-files (:exit t)
    "Frequent files"
    ("e" (find-file (emacs.d "init.el")) "conf")
    ("i" (find-file (journal.d "inbox.org")) "inbox")
    ("n" (find-file (journal.d "notes.org")) "notes")
    ("u" (find-file (journal.d "uniplan.org")) "uniplan")
    ("s" (find-file (journal.d "scratch.org")) "scratch")
    ("r" (find-file (journal.d "reading-list.org")) "to read")
    ("w" (find-file (journal.d "wiki.org")) "wiki")
    ("p" (find-file (journal.d "poetry.org") "packages")))
  (defhydra hydra-package (:exit t)
    "Package management"
    ("r" (package-refresh-contents) "refresh")
    ("i" (call-interactively #'package-install) "install")
    ("u" (package-utils-upgrade-all) "upgrade")
    ("d" (call-interactively #'package-delete) "delete"))
  (general-def
    "C-c s" 'hydra-shell/body
    "C-c f" 'hydra-files/body
    "C-c p" 'hydra-package/body
    "C-c w" 'hydra-window/body))

;; ** Magit
(use-package magit
  :config
  (general-def "C-c d" 'magit-list-repositories))

;; *** Repolist
(defun my/list-subdirs (dir)
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

(defun my/contains-git-repo-p (dir)
  "Check if there's  a .git directory in DIR."
  (let ((dirs (directory-files dir)))
    (member ".git" dirs)))

(defun my/filter-git-repos (dirs)
  "Remove all directories without a .git subdirectory in DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (when (my/contains-git-repo-p dir)
	(add-to-list 'result dir)))
    result))

(defun my/make-magit-repolist (dirs)
  "Make a list of the form (dir 0) for the magit-list-repositories function from DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (add-to-list 'result `(,dir 0)))
    result))

(defun my/repolist-refresh ()
  "Hi."
  (setq magit-repository-directories
	(~> "~/dev"
	    (my/list-subdirs)
	    (my/filter-git-repos)
	    (my/make-magit-repolist))))

(advice-add 'magit-list-repositories :before #'my/repolist-refresh)

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


;; * Custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
;; * Utils
;; * Helper functions
;; ** Directories
(setq my/journal-path "~/cloud/journal/")
(defun emacs.d (filename)
  "Return the file path of FILENAME relative to the Emacs directory."
  (format "%s%s" user-emacs-directory filename))

(defun journal.d (filename)
  "Return the file path of FILENAME relative to the Journal directory."
  (format "%s%s" my/journal-path filename))

;; ** Switch to previous buffer
(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ** Join lines
(defun my/join-lines-region ()
  "Join lines like J in Vim."
  (let ((n (my/count-lines-region))
	(flip-p (eq (region-end) (point))))
    (progn
      (when flip-p (exchange-point-and-mark))
      (dotimes (_ (max 1 (1- n))) (join-line -1)))))

(defun my/count-lines-region ()
  "Count the lines in the region."
  (interactive)
  (let ((numlines (count-lines (region-beginning) (region-end)))
	(beginning-of-line-p (= (line-beginning-position) (point))))
    (if beginning-of-line-p
	(1+ numlines)
      numlines)))

(defun my/join-line ()
  "Join lines in the opposite direction."
  (interactive)
  (if (region-active-p)
      (my/join-lines-region)
    (join-line -1)))

;; ** Delete file
(defun visiting-file-p ()
  "Check whether current buffer is visiting an existing file."
  (let ((filename (buffer-file-name)))
    (and filename (file-exists-p filename))))

(defun my/delete-this-file ()
  "Remove file connected to current buffer and kill buffer."
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
(defun my/rename-this-file ()
  "Rename current buffer and associated file."
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
  "Pipe INIT through LST.

Evaluate transformation pipeline LST. Either append argument at
the end or replace all :arg occurences (starting with INIT).

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

;;; init.el ends here
;; ** Activate current task
(defun my/activate-current-task ()
  "Activate task under cursor."
  (interactive)
  (progn
    (message "hi")
    (let ((task (mapconcat 'identity (org-get-outline-path t) " -> ")))
      (progn
	(message task)
	(write-region task nil "~/.current_task")))))

(general-def "C-c h" 'my/activate-current-task)
