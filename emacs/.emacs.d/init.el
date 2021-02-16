(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(set-face-font 'default "Iosevka-11")
(set-face-font 'fixed-pitch "Iosevka-11")

(defconst *sys/linux*
  (eq system-type 'gnu/linux))

(defconst *sys/windows*
  (eq system-type 'windows-nt))

(defconst *sys/mac*
  (eq system-type 'darwin))

(defun hgf/emacs-path (filename)
  "Return the file path of FILENAME relative to the Emacs directory."
  (format "%s%s" user-emacs-directory filename))

(defun hgf/journal-path (filename)
  "Return the file path of FILENAME relative to the Journal directory."
  (format "%s%s" "~/cloud/journal/" filename))

(defun hgf/org-path (filename)
  "Return the file path of FILENAME relative to the Org directory."
  (format "%s%s" "~/cloud/org/" filename))

(defun hgf/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defmacro pipe (init &rest lst)
  "Pipe INIT through LST.

Evaluate transformation pipeline LST. Either append argument at
the end or replace all :arg occurences (starting with INIT).

Example:
(pipe (number-sequence 1 10)
      (-filter 'cl-evenp)
      (mapcar '1+))
=> (3 5 7 9 11)

(pipe (number-sequence 1 10)
    (-filter 'cl-evenp)
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
  (-reduce-from (lambda (acc el)
		  (if (member :arg el)
		      (-replace :arg acc el)
		    (append el `(,acc))))
		init
		lst))

(defun hgf/edit-weatherwax ()
  "Edit the Weatherwax theme."
  (interactive)
  (find-file (hgf/emacs-path "themes/weatherwax-theme.el")))

(defun hgf/disable-all-themes ()
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun hgf/load-theme (theme)
  "Disable all loaded themes and load THEME. Also sets certain face attributes I like to use."
  (interactive
   (list (intern (completing-read "Load custom theme: "
				  (mapcar 'symbol-name
					  (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (progn
    (hgf/disable-all-themes)
    (load-theme theme t)
    ))

(setq custom-theme-directory (hgf/emacs-path "themes/"))

(defvar hgf/gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold hgf/gc-cons-threshold)
	    (setq file-name-handler-alist file-name-handler-alist-original)
	    (makunbound 'file-name-handler-alist-original)))

(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

(add-to-list 'load-path (hgf/emacs-path "lisp"))

(setq custom-file (hgf/emacs-path "custom.el"))
(load custom-file 'noerror)

(global-auto-revert-mode +1)

(show-paren-mode 1)

(setq show-paren-delay 0.0)

(setq require-final-newline t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(setq-default cursor-type 'bar)

(blink-cursor-mode 0)

(setq mouse-yank-at-point t)

(setq vc-follow-symlinks t)

(setq sentence-end-double-space nil)

(setq confirm-kill-emacs 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(defun hgf/modeline-modified ()
  "Return buffer status: read-only (-), modified (·) or read-write ( )."
  (let ((read-only buffer-read-only)
	(modified  (and buffer-file-name (buffer-modified-p))))
    (cond
     (modified (propertize " ∙ " 'face 'bold))
     (read-only " - ")
     (t "   "))))

(setq-default
 mode-line-format
 '(
   ""
   (:eval (hgf/modeline-modified))
   ;; Buffer name
   "%b"
   "  "
   ;; VC Branch
   (:eval (when-let (vc vc-mode)
	    (propertize (substring vc 5)
			'background nil)))))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(use-package general
  :config
  (general-evil-setup)
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(general-create-definer hgf/leader-def
  :prefix "C-c")

(general-def "M-i" 'imenu)

(general-def "M-n" 'scroll-up-command)
(general-def "M-p" 'scroll-down-command)

(use-package s)
(use-package dash)

(use-package gruvbox-theme)
(hgf/load-theme 'gruvbox-dark-medium)

(use-package undo-tree)
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-abbrev-expand-on-insert-exit nil
	evil-want-Y-yank-to-eol t
	evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (global-undo-tree-mode +1)
  (setq evil-emacs-state-cursor 'bar
	evil-search-module 'evil-search
	evil-ex-search-case 'smart)
  (general-nmap
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank-of-visual-line
    "g E" 'eval-buffer
    "g e" 'eval-last-sexp
    "g C-e" 'eval-defun
    "C-u" 'evil-scroll-up
    "C-w 1" 'delete-other-windows
    "C-w x" 'kill-this-buffer
    "C-w C-h" 'evil-window-left
    "C-w C-j" 'evil-window-down
    "C-w C-k" 'evil-window-up
    "C-w C-l" 'evil-window-right)
  (general-vmap
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank-of-visual-line)
  (general-imap
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line
    "C-k" 'kill-line
    "C-d" 'delete-char
    "C-y" 'yank))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary
  :config
  (evil-commentary-mode 1)
  (general-nmap evil-commentary-mode-map
    "M-;" 'evil-commentary-line))
(use-package evil-exchange
  :config
  (evil-exchange-cx-install))
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package avy
  :general
  ('normal "s" 'avy-goto-char-timer)
  ('emacs "<C-i>" 'avy-goto-char-timer))

(general-def input-decode-map [?\C-i] [C-i])
(general-def 'normal "<C-i>" 'evil-jump-forward)
(general-unbind evil-motion-state-map "TAB")

(use-package selectrum
  :init
  (selectrum-mode +1)
  :custom 
  (completion-styles '(flex substring partial-completion)))

(defun hgf/-close-compilation-if-successful (buf str)
  "Close the compilation window if it is successful."
  (if (null (string-match ".*exited abnormally.*" str))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
	(run-at-time
	 "1 sec" nil 'kill-buffer
	 (get-buffer-create "*compilation*"))
	(message "No Compilation Errors!"))))

(general-def "C-x c" 'recompile)
(add-hook 'compilation-finish-functions
	  'hgf/-close-compilation-if-successful)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-use-property-inheritance t
	org-startup-folded t
	org-adapt-indentation nil
	org-hide-leading-stars t
	org-cycle-separator-lines 0
	org-hide-emphasis-markers t
	org-fontify-done-headline nil
	org-M-RET-may-split-line nil
	org-outline-path-complete-in-steps nil
	org-refile-use-outline-path 'file
	org-refile-targets '((org-agenda-files . (:maxlevel . 2)))
	org-archive-location (hgf/org-path "archive.org::* %s")
	org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "SOMEDAY"))
	org-log-done-with-time nil
	org-log-done 'note
	;; Does this variable even exist?
	;; org-clock-into-drawer nil
	;; Agenda
	org-default-notes-files "~/cloud/org/notes.org"
	org-agenda-files '("~/cloud/org/")
	org-agenda-window-setup 'current-window
	org-agenda-time-grid nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-timestamp-if-done t
	org-agenda-start-on-weekday nil
	org-reverse-note-order t
	org-fast-tag-selection-single-key t
	;; Babel
	org-src-fontify-natively t
	org-src-preserve-indentation nil
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-src-window-setup 'current-window
	)
  
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (general-add-advice 'org-capture :after '(lambda () (evil-append 0)))
  (general-def 'normal org-mode-map
    "g t" 'org-todo
    "g e" 'org-set-effort)

  (defun org-capture-inbox ()
    (interactive)
    (condition-case nil
	(call-interactively 'org-store-link)
      (error nil))
    (org-capture nil "i"))
  
  (defun org-agenda-today ()
    (interactive)
    (org-agenda-list nil nil 7))

  (setq org-priority-faces
	'((?A . (:weight 'normal))
	  (?B . (:weight 'normal))
	  (?C . (:weight 'normal))))
  (setq org-capture-templates
	'(("n" "Note" entry (file "~/cloud/org/notes.org")
	   "* %u %?")
	  ("i" "Inbox" entry (file "~/cloud/org/inbox.org")
	   "* TODO %?\n%u")))

  (defun hgf/visit-inbox ()
    (interactive)
    (find-file (hgf/org-path "inbox.org")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (ipython . t)))


  (general-def
    "<f1>" 'org-capture-inbox
    "<f2>" 'hgf/visit-inbox
    "<f3>" 'org-agenda-today)
  (general-def 'motion org-agenda-mode-map
    "d" 'org-agenda-day-view
    "w" 'org-agenda-week-view
    "e" 'org-agenda-set-effort))


(with-eval-after-load 'org-agenda
  (setf (alist-get 'agenda org-agenda-prefix-format)
	"%-6e %-10c %?t ")
  (setf (alist-get 'todo org-agenda-prefix-format)
	"%-8e %-16:(hgf/title-case-filename (buffer-name)) %?t "))

(defun hgf/ts-make-from-iso8601 (line)
  "Return a ts struct from an iso8601 string.

The ts library doesn't contain this by default, but there's an open PR:
https://github.com/alphapapa/ts.el/pull/15"
  (thread-last line
    (org-read-date nil nil)
    (parse-iso8601-time-string)
    (float-time)
    (make-ts :unix)))


(defun hgf/ts-days-diff (then &optional now)
  "Return the difference in days between THEN and NOW.

I was having trouble using ts-diff and ts-human-difference, for whatever reason.
This function works well enough."
  (let* ((now (or now (ts-now)))
	 (then-doy (ts-doy then))
	 (then-year (ts-year then))
	 (now-doy (ts-doy now))
	 (now-year (ts-year now)))
    (- then-doy now-doy (* -365 (- then-year now-year)))))

(defun hgf/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos date diff line)
      (while (setq pos (text-property-any
			(point) (point-max) 'org-agenda-date-header t))
	(goto-char pos)
	;; Line formats can be:
	;; Tuesday 16 February 2021
	;; Tuesday 16 February 2021 W07
	;; Parsing works best if we only keep the day, month and year
	;; hence the thread below
	(setq diff (->> (thing-at-point 'line t)
			(s-split-words)
			(cdr)
			(-take 3)
			(s-join " ")
			(hgf/ts-make-from-iso8601)
			(hgf/ts-days-diff)))
	(end-of-line)
	(insert-and-inherit (concat " (" (hgf/org-ql-effort-on diff)  ")"))
	(forward-line)))))
(add-hook 'org-agenda-finalize-hook 'hgf/org-agenda-insert-efforts)

(defun hgf/org-mode-hook ()
  "Disable header variable font size."
  (progn
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5
		    org-document-title))
      (set-face-attribute face nil :weight 'normal :height 1.0))))

(add-hook 'org-mode-hook 'hgf/org-mode-hook)

(use-package org-cliplink
  :after org
  :config
  (general-def org-mode-map "C-x C-l" 'org-cliplink))

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
(use-package ox-extra
  :ensure org-plus-contrib
  :commands ox-extras-activate
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package org-ql
  :config
  (general-unbind org-ql-view-map "C-x C-s"))
(use-package org-super-agenda
  :config
  (setq org-super-agenda-header-map nil))
(use-package ts)

(defun hgf/org-ql-effort-on (&optional date type)
  "Return the amount of estimated effort on a date.

If DATE is nil, 'today' is used.
TYPE may be 'scheduled' or 'closed'. If nil, it is set to 'total'."
  (let ((date (or date 'today))
	(type (or type 'scheduled)))
    (thread-last (org-ql-query
		   :from (org-agenda-files)
		   :where `(and (,type :on ,date) (property "EFFORT"))
		   :select '(org-entry-get (org-get-at-bol) "EFFORT"))
      (-map #'org-duration-to-minutes)
      (-reduce #'+)
      (org-duration-from-minutes))))

(defun hgf/org-super-group-append-efforts-to-item (it)
  (let* ((name (plist-get it :name))
	 (date (cadr (text-properties-at 0 name))))
    (plist-put it :name 
	       (s-concat name " [" (hgf/org-ql-effort-on date) "]"))))

(defun hgf/org-super-weekly-agenda ()
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(scheduled :from today :to 7)
    :sort '(date priority todo)
    :super-groups '((:auto-ts t :transformer hgf/org-super-group-append-efforts-to-item))))

(require 'reftex-parse)
(require 'bibtex)
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . tex-mode)
  :config
  (general-def tex-mode-map
    "C-M-g" 'hgf/pdf-view-first-page-other-window
    "C-M-n" 'hgf/pdf-view-next-page-other-window
    "C-M-p" 'hgf/pdf-view-previous-page-other-window)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq TeX-PDF-mode t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package auctex-latexmk
  :after tex
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(with-eval-after-load 'bibtex
(defun hgf/bibtex-complete ()
  (interactive)
  (let ((bib-files (reftex-locate-bibliography-files "."))
	entries)
    (dolist (file bib-files)
      (message "Completing: %s" file)
      (with-temp-buffer
	(insert-file-contents file)
	(bibtex-mode)
	(setq entries (-concat (bibtex-parse-keys nil t) entries))))
    (insert (completing-read "cite: " (-map #'car entries))))))
      
(general-def "C-c [" 'hgf/bibtex-complete)

(defun hgf/bibtex-hook ()
  "My bibtex hook."
  (progn
    (setq comment-start "%")))

(add-hook 'bibtex-mode-hook 'hgf/bibtex-hook)

(setq-default TeX-auto-save t
	      TeX-parse-self t
	      TeX-PDF-mode t
	      TeX-auto-local (hgf/emacs-path "auctex-auto"))
(setq bibtex-dialect 'biblatex)

(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :config
  (add-to-list 'ledger-reports '("diet" "%(binary) -f %(ledger-file) reg --value Assets --budget --daily"))
  (add-to-list 'ledger-reports '("work" "%(binary) -f %(ledger-file) bal --add-budget")))

(use-package vterm
  :general ("<f4>" 'vterm)
  :config
  (setq vterm-shell "/usr/bin/fish"
	vterm-kill-buffer-on-exit t
	vterm-copy-exclude-prompt t))

(defun hgf/named-term (term-name)
  "Generate a terminal with buffer name TERM-NAME."
  (interactive "sTerminal purpose: ")
  (vterm (concat "term-" term-name)))

(hgf/leader-def "t" 'hgf/named-term)

(use-package which-key
  :config
  (which-key-mode))

(use-package visual-fill-column
  :defer t
  :config
  (setq-default visual-fill-column-width 90))

(use-package outshine
  :hook (prog-mode . outshine-mode)
  :config
  (setq outshine-startup-folded-p t))

(use-package engine-mode
  :defer 2
  :config
  (engine-mode 1)
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis."))

(use-package magit
  :commands (magit-status magit-list-repositories)
  :config
  (advice-add 'magit-list-repositories :before #'hgf/repolist-refresh))

(defun hgf/contains-git-repo-p (dir)
  "Check if there's  a .git directory in DIR."
  (let ((dirs (directory-files dir)))
    (member ".git" dirs)))

(defun file-directory-real-p (dir)
  (and (file-directory-p dir)
       (not (equal (substring dir -1) "."))))

(defun hgf/find-git-repos-recursive (basedir)
  "Return a list of directories containing a .git directory."
  (let ((result))
    (dolist (f (-filter 'file-directory-real-p (directory-files basedir t)) result)
      (if (hgf/contains-git-repo-p f)
	  (add-to-list 'result f)
	(setq result (append result (hgf/find-git-repos-recursive f)))))
    result))

(defun hgf/make-magit-repolist (dirs)
  "Make a list of the form (dir 0) for the magit-list-repositories function from DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (add-to-list 'result `(,dir 0)))
    result))

(defun hgf/repolist-refresh ()
  "Hi."
  (setq magit-repository-directories
	(pipe "~/dev"
	      (hgf/find-git-repos-recursive)
	      (hgf/make-magit-repolist))))


(setq magit-repolist-columns
      '(("Name" 12 magit-repolist-column-ident nil)
	("Branch" 10 magit-repolist-column-branch nil)
	("B<U" 3 magit-repolist-column-unpulled-from-upstream
	 ((:right-align t)
	  (:help-echo "Upstream changes not in branch")))
	("B>U" 3 magit-repolist-column-unpushed-to-upstream
	 ((:right-align t)
	  (:help-echo "Local changes not in upstream")))
	("Version" 30 magit-repolist-column-version nil)
	("Path" 99 magit-repolist-column-path nil)))

(use-package treemacs
  :general ("C-c T" 'treemacs)
  :config
  (setq treemacs-no-png-images t
	treemacs-width 24))

(defun hgf/title-case-filename (filename)
  (thread-last filename
    (file-name-sans-extension)
    (s-replace "_" " ")
    (upcase-initials)))

(defun hgf/treemacs-file-name-org-title (filename)
  (pcase (file-name-extension filename)
    ("org" (hgf/title-case-filename filename))
    (otherwise filename)))

(setq treemacs-file-name-transformer 'hgf/treemacs-file-name-org-title)

(use-package project
  :ensure nil
  :config
  (general-def
    "C-x p f" 'project-find-file
    "C-x p p" 'project-select-project))

(defun project--build-project-list ()
  "Create a list of all git repos."
  (hgf/find-git-repos-recursive "~/dev"))

(setq display-buffer-alist
      '((".*" (display-buffer-reuse-window display-buffer-same-window))))

(setq display-buffer-reuse-frames t
      even-window-sizes nil)

(use-package yasnippet
  :commands yas-minor-mode
  :init
  (setq yas-indent-line 'fixed)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package helpful
  :defer t
  :config
  (hgf/leader-def
    "h h" 'helpful-at-point)
  (general-def
    "C-h h" 'helpful-at-point
    "C-h k" 'helpful-key
    "C-h F" 'helpful-function
    "C-h C" 'helpful-command
    "C-c C-d" 'helpful-at-point))

(use-package hydra
  :defer t
  :config
  (defhydra hydra-org-mode (:exit t)
    "Org mode"
    ("c" org-capture "capture")
    ("i" org-capture-inbox "inbox")
    ("t" org-todo-list "todos")
    ("a" org-agenda "agenda"))
  (defhydra hydra-window ()
    "Window management"
    ("o" other-window "other")
    ("h" evil-window-left "left")
    ("j" evil-window-down "down")
    ("k" evil-window-up "up")
    ("l" evil-window-right "right")
    ("s" evil-window-split "split")
    ("v" evil-window-vsplit "vsplit")
    ("q" evil-quit "quit")
    ("f" find-file "file")
    ("m" kill-this-buffer "murder")
    ("1" delete-other-windows "highlander")
    ("." nil "stop"))
  (defhydra hydra-files (:exit t)
    "Frequent files"
    ;; Configuration
    ("c" (hydra-configs/body) "configs")
    ("e" (find-file (hgf/emacs-path "configuration.org")) "config")
    ;; Org
    ("b" (find-file (hgf/journal-path "blog.org")) "blog")
    ("d" (find-file (hgf/journal-path "diet/diet.ledger")) "diet")
    ("D" (find-file (hgf/journal-path "diet/food.ledger")) "food")
    ("m" (find-file (hgf/journal-path "calendar.org")) "calendar")
    ("i" (find-file (hgf/journal-path "inbox.org")) "inbox")
    ("n" (find-file (hgf/journal-path "notes.org")) "notes")
    ("p" (find-file (hgf/journal-path "projects.org")) "projects")
    ("w" (find-file (hgf/journal-path "wiki.org")) "wiki")
    ;; Work
    ("f" (hydra-work/body) "fraunhofer")
    ;; Scratch
    ("s" (hgf/make-scratch-buffer) "scratch"))
  (defhydra hydra-configs (:exit t)
    "Configuration files"
    ("i" (find-file "~/.config/i3/config") "i3")
    ("g" (find-file "~/.config/git") "git")
    ("k" (find-file "~/.config/kitty/kitty.conf") "kitty")
    ("r" (find-file "~/.config/ranger/rc.conf") "ranger")
    ("R" (find-file "~/.config/rofi/config") "Rofi")
    ("e" (find-file (hgf/emacs-path "configuration.org")) "emacs")
    ("f" (find-file "~/.config/fish/config.fish") "fish"))
  (defhydra hydra-work (:exit t)
    "Work related files"
    ("n" (find-file (hgf/journal-path "fraunhofer/notes.org")) "notes")
    ("t" (find-file (hgf/journal-path "fraunhofer/working_hours.ledger")) "working hours"))
  (defhydra hydra-package (:exit t)
    "Package management"
    ("r" (package-refresh-contents) "refresh")
    ("i" (call-interactively #'package-install) "install")
    ("u" (package-utils-upgrade-all) "upgrade")
    ("d" (call-interactively #'package-delete) "delete"))
  (hgf/leader-def
    "P" 'hydra-package/body
    "f" 'hydra-files/body
    "w" 'hydra-window/body
    "o" 'hydra-org-mode/body))

(defun hgf/pdf-view-next-page-other-window ()
  (interactive)
  (with-selected-window (get-buffer-window (find-buffer-visiting (concat (cdr (project-current)) "build/main.pdf")))
    (pdf-view-next-page)))

(defun hgf/pdf-view-previous-page-other-window ()
  (interactive)
  (with-selected-window (get-buffer-window (find-buffer-visiting (concat (cdr (project-current)) "build/main.pdf")))
    (pdf-view-previous-page)))

(defun hgf/pdf-view-first-page-other-window ()
  (interactive)
  (with-selected-window (get-buffer-window (find-buffer-visiting (concat (cdr (project-current)) "build/main.pdf")))
    (pdf-view-first-page)))

(use-package pdf-tools
  :defer 2
  :config
  (add-to-list 'global-auto-revert-ignore-modes 'pdf-view-mode))

(defun hgf/switch-to-previous-buffer ()
  "Switch to previously open buffer.
      Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun visiting-file-p ()
  "Check whether current buffer is visiting an existing file."
  (let ((filename (buffer-file-name)))
    (and filename (file-exists-p filename))))

(defun hgf/delete-this-file ()
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

(defun hgf/rename-this-file ()
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

(defun hgf/get-org-title ()
  "Get the raw string of the current buffer's #+TITLE property."
  (substring-no-properties
   (car (plist-get (org-export-get-environment) :title))))

(defun hgf/org-export-file-to-file (infile outfile backend)
  (write-region (org-export-string-as
		 (with-temp-buffer
		   (insert-file-contents infile)
		   (buffer-string))
		 backend)
		nil
		outfile))

(defun hgf/make-scratch-directory ()
  "Create a temporary scratch directory."
  (interactive)
  (find-file (make-temp-file "scratch-" t)))

(defun hgf/make-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(use-package text-mode
  :ensure nil
  :config
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook 'auto-fill-mode))

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :ensure nil
  :config
  (setq ispell-program-name "aspell"
	ispell-extra-args '("--sug-mode=ultra")))
