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
(defun my-emacs-path (filename)
  "Return the file path of FILENAME relative to the Emacs directory."
  (format "%s%s" user-emacs-directory filename))

(defun my-journal-path (filename)
  "Return the file path of FILENAME relative to the Journal directory."
  (format "%s%s" "~/cloud/journal/" filename))

(defun my-find-file-as-sudo ()
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
(setq custom-theme-directory (my-emacs-path "themes/"))
(if (package-installed-p 'autothemer)
    (load-theme 'weatherwax t))
(defvar my-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold my-gc-cons-threshold)
	    (setq file-name-handler-alist file-name-handler-alist-original)
	    (makunbound 'file-name-handler-alist-original)))
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")
(add-to-list 'load-path (my-emacs-path "lisp"))
(setq custom-file (my-emacs-path "custom.el"))
(load custom-file 'noerror)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0.0)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
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
(defun my-modeline-modified ()
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
   (:eval (my-modeline-modified))
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
(general-create-definer my-leader-def
  :keymaps '(normal visual)
  :prefix "SPC")

(general-create-definer my-c-def
  :prefix "C-c")
(general-def
  "C-s" 'save-buffer
  "M-i" 'imenu)

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-abbrev-expand-on-insert-exit nil
	evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursor 'bar
	evil-search-module 'evil-search
	evil-ex-search-case 'smart)
  (general-nmap
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank-of-visual-line
    "?" 'swiper
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
(general-def input-decode-map [?\C-i] [C-i])
(general-def 'normal "<C-i>" 'evil-jump-forward)
(general-unbind evil-motion-state-map "TAB")
(use-package org
  :config
  (setq org-use-property-inheritance t)
  (general-nmap org-mode-map
    "g t" 'org-todo))
(add-to-list 'org-modules 'habits)

(defun org-capture-inbox ()
  (interactive)
  (condition-case nil
      (call-interactively 'org-store-link)
    (error nil))
  (org-capture nil "i"))
(general-add-advice 'org-capture-inbox :after '(lambda () (evil-append 0)))
(setq org-refile-use-outline-path 'file
      org-clock-into-drawer nil
      org-log-done 'time)
(setq org-refile-targets `((,(my-journal-path "projects.org") :maxlevel . 2)
			   (,(my-journal-path "fraunhofer/notes.org") :maxlevel . 2)))
(setq org-archive-location "~/cloud/journal/archive.org::* %s")
(setq org-capture-templates
      '(("n" "Note" entry (file "~/cloud/journal/notes.org")
	 "*  %?\n")
	("i" "Inbox" entry (file "~/cloud/journal/inbox.org")
	 "* TODO %?\n")))
(setq org-agenda-files
      '(
	"~/cloud/journal/inbox.org"
	"~/cloud/journal/projects.org"
	"~/cloud/journal/calendar.org"
	"~/cloud/journal/habits.org"
	"~/cloud/journal/fraunhofer/"
	))
(defun org-generate-report ()
  (interactive)
  (let ((header "|Task|Duration|"))
    (insert (s-join "\n" (nconc `(,header) (org-element-map (org-element-parse-buffer) 'clock
					     (lambda (clock)
					       (let ((task (org-element-property :title (org-element-property :parent (org-element-property :parent clock))))
						     (val  (org-element-property :duration clock)))
						 (format "| %s | %s |" (car task) val)))))))))
(general-nmap "C-c C-x C-r" 'org-generate-report)
(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
	'(;; Group conds are ORed
	  (:name "Fraunhofer"
		 :tag "ipk")
	  (:name "MSC Thesis"
		 :tag "msc")
	  (:name "Habits"
		 :tag "habit")
	  ))
  (org-super-agenda-mode 1))
(general-add-advice 'org-clock-in :after 'my-activate-current-task)
(add-to-list 'org-structure-template-alist
	     '("el" . "src emacs-lisp"))
(setq org-src-fontify-natively t
      org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-src-window-setup 'current-window)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)
   (octave . t)
   (python . t)
   (ruby . t)))
(setq org-adapt-indentation nil
      org-hide-leading-stars t
      org-cycle-separator-lines 0
      org-hide-emphasis-markers t
      org-fontify-done-headline nil)
(add-hook
 'org-mode-hook
 (lambda ()
   "Beautify Org Symbols"
   (push '("#+begin_src" . "λ") prettify-symbols-alist)
   (push '("#+end_src" . "~") prettify-symbols-alist)
   (push '("TODO" . "?") prettify-symbols-alist)
   (push '("DONE" . "!") prettify-symbols-alist)
   (prettify-symbols-mode)))
(defun my-org-mode-hook ()
  "Disable header variable font size."
  (progn
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5
		    org-document-title))
      (set-face-attribute face nil :weight 'normal :height 1.0))))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(setq org-M-RET-may-split-line nil
      org-outline-path-complete-in-steps nil)
(use-package org-cliplink
  :config
  (general-def org-mode-map "C-x C-l" 'org-cliplink))
(setq reftex-default-bibliography '("~/media/bibliographies/all.bib"))
(setq bibtex-completion-bibliography '("~/media/bibliographies/all.bib"))
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




(use-package magit
  :defer t
  :config
  (my-c-def "d" 'magit-list-repositories))
(defun my-contains-git-repo-p (dir)
  "Check if there's  a .git directory in DIR."
  (let ((dirs (directory-files dir)))
    (member ".git" dirs)))

(defun file-directory-real-p (dir)
  (and (file-directory-p dir)
       (not (equal (substring dir -1) "."))))

(defun my-find-git-repos-recursive (basedir)
  "Return a list of directories containing a .git directory."
  (let ((result))
  (dolist (f (-filter 'file-directory-real-p (directory-files basedir t)) result)
    (if (my-contains-git-repo-p f)
	(add-to-list 'result f)
     (setq result (append result (my-find-git-repos-recursive f)))))
    result))

(defun my-make-magit-repolist (dirs)
  "Make a list of the form (dir 0) for the magit-list-repositories function from DIRS."
  (let ((result))
    (dolist (dir dirs result)
      (add-to-list 'result `(,dir 0)))
    result))

(defun my-repolist-refresh ()
  "Hi."
  (setq magit-repository-directories
	(pipe "~/dev"
	      (my-find-git-repos-recursive)
	      (my-make-magit-repolist))))

(advice-add 'magit-list-repositories :before #'my-repolist-refresh)

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
(use-package vterm
  :config
  (setq vterm-shell "/usr/bin/fish"
	vterm-kill-buffer-on-exit t
	vterm-copy-exclude-prompt t)
  (general-nmap "<f4>" 'vterm))
(defun my-named-term (term-name)
  "Generate a terminal with buffer name TERM-NAME."
  (interactive "sTerminal purpose: ")
  (vterm (concat "term-" term-name)))

(my-leader-def "r t" 'my-named-term)
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-initial-inputs-alist nil
	count-format "(%d/%d) "))
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))
(use-package counsel
  :config
  (counsel-mode 1)
  (use-package flx)
  (use-package smex))
(use-package ivy-bibtex
  :config
  (setq ivy-re-builders-alist
	'((ivy-bibtex . ivy--regex-ignore-order)
	  (t . ivy--regex-plus)))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-cite-default-command "autocite"
	bibtex-completion-cite-prompt-for-optional-arguments nil
	bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "zathura" nil 0 nil fpath)))
  (general-def "C-x [" 'ivy-bibtex))
(use-package company
  :config
  (general-imap company-mode-map
    "C-x C-o" 'company-complete
    "C-x C-f" 'company-files))
(use-package hydra
  :defer t)
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
  ("b" ivy-switch-buffer "buffer")
  ("m" kill-this-buffer "murder")
  ("1" delete-other-windows "highlander")
  ("." nil "stop"))
(defhydra hydra-files (:exit t)
  "Frequent files"
  ;; Configuration
  ("c" (hydra-configs/body) "configs")
  ("e" (find-file (my-emacs-path "configuration.org")) "config")
  ;; Org
  ("b" (find-file (my-journal-path "blog.org")) "blog")
  ("d" (find-file (my-journal-path "diet/diet.ledger")) "diet")
  ("D" (find-file (my-journal-path "diet/food.ledger")) "food")
  ("m" (find-file (my-journal-path "calendar.org")) "calendar")
  ("h" (find-file (my-journal-path "habits.org")) "habits")
  ("i" (find-file (my-journal-path "inbox.org")) "inbox")
  ("n" (find-file (my-journal-path "notes.org")) "notes")
  ("p" (find-file (my-journal-path "projects.org")) "projects")
  ("w" (find-file (my-journal-path "wiki.org")) "wiki")
  ;; Work
  ("f" (hydra-work/body) "fraunhofer")
  ;; Scratch
  ("s" (my-generate-scratch-buffer) "scratch"))
(defhydra hydra-configs (:exit t)
  "Configuration files"
  ("i" (find-file "~/.config/i3/config") "i3")
  ("g" (find-file "~/.config/git") "git")
  ("k" (find-file "~/.config/kitty/kitty.conf") "kitty")
  ("r" (find-file "~/.config/ranger/rc.conf") "ranger")
  ("R" (find-file "~/.config/rofi/config") "Rofi")
  ("e" (find-file (my-emacs-path "configuration.org")) "emacs")
  ("f" (find-file "~/.config/fish/config.fish") "fish"))
(defhydra hydra-work (:exit t)
  "Work related files"
  ("n" (find-file (my-journal-path "fraunhofer/notes.org")) "notes")
  ("t" (find-file (my-journal-path "fraunhofer/working_hours.ledger")) "working hours")
  ("p" (counsel-find-file (my-journal-path "fraunhofer/projects")) "projects"))
(defhydra hydra-package (:exit t)
  "Package management"
  ("r" (package-refresh-contents) "refresh")
  ("i" (call-interactively #'package-install) "install")
  ("u" (package-utils-upgrade-all) "upgrade")
  ("d" (call-interactively #'package-delete) "delete"))
(my-leader-def
  "P" 'hydra-package/body
  "f" 'hydra-files/body
  "w" 'hydra-window/body
  "o" 'hydra-org-mode/body)
(use-package yasnippet
  :defer t
  :commands yas-minor-mode
  :init
  (setq yas-indent-line 'fixed)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)
(use-package project
  :ensure nil
  :config
  (general-nmap
    "C-x p f" 'project-find-file
    "C-x p p" 'project-select-project))
(use-package ripgrep)
(defun project-select-project ()
  "Select a project from the project list."
  (interactive)
  (ivy-read
   "Project: "
   (project--build-project-list)
   :action (lambda (p) (dired p))))

(defun project--build-project-list ()
  "Create a list of all git repos."
  (my-find-git-repos-recursive "~/dev"))

(use-package outshine
  :config
  (setq outshine-startup-folded-p t)
  (add-hook 'conf-mode-hook #'outshine-mode 1)
  (add-hook 'prog-mode-hook #'outshine-mode 1)
  (add-hook 'bibtex-mode-hook #'outshine-mode 1)
  (add-hook 'LaTeX-mode-hook #'outshine-mode 1))
