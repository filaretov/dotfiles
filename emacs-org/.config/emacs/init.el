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

(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

(use-package nord-theme
  :config (load-theme 'nord t))

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
    "C-w C-l" 'evil-window-right
    "s" nil
    "sj" 'evil-window-down
    "sk" 'evil-window-up
    "sh" 'evil-window-left
    "sl" 'evil-window-right
    "sv" 'evil-window-vsplit
    "ss" 'evil-window-split
    "sd" 'evil-window-delete
    "sq" 'evil-quit)
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

(use-package selectrum
  :init
  (selectrum-mode +1)
  :custom 
  (completion-styles '(flex substring partial-completion)))

(use-package org
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
	org-todo-keywords '(
			    (sequence "TODO" "NEXT" "|" "DONE")
			    (type "WAITING" "SOMEDAY" "PJ"))
	org-log-done-with-time nil
	org-log-done 'time
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
   '((emacs-lisp . t)))


  (general-def
    "<f1>" 'org-capture-inbox
    "<f2>" 'hgf/visit-inbox
    "<f3>" 'org-agenda-today)
  (general-def 'motion org-agenda-mode-map
    "d" 'org-agenda-day-view
    "w" 'org-agenda-week-view
    "e" 'org-agenda-set-effort))

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
    '(or (scheduled :from today :to 7)
	 (todo))
    :sort '(date priority todo)
    :super-groups '(
		    (:name "Scheduled"
			   :auto-ts t
			   :transformer hgf/org-super-group-append-efforts-to-item)
		    (:name "Unsorted"
			   :anything t))))

(let ((org-super-agenda-groups
       '((:name "Scheduled"
		:auto-ts t)
	 (:name "Unsorted"
		:anything t)))))

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
