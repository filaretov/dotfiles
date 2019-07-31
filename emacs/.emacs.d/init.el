;; * Initialize packaging
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(when (string-equal "quirm" (getenv "HOSTNAME"))
  (progn
    (setq package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("melpa" . "https://melpa.org/packages/")
            ("org" . "https://orgmode.org/elpa/")))
    (setq url-proxy-services '(("http" . "153.96.56.101:3128")
                               ("https" . "153.96.56.101:3128")
                               ("no_proxy" . "^\\(localhost\\|127.*\\)")))))


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

;; * Editor theme

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

;; ** Editor theme
(use-package sourcerer-theme
  :config
  (load-theme 'sourcerer t)
  (defun hgf/toggle-theme ()
    "Toggle between solarized variants."
    (interactive)
    (let ((dark-theme 'sourcerer)
	  (light-theme 'adwaita))
      (if (equal (car custom-enabled-themes) dark-theme)
	  (progn
	    (disable-theme dark-theme)
	    (load-theme light-theme t))
	(progn
	  (disable-theme light-theme)
	  (load-theme dark-theme t)))))
  (general-def "C-c z" 'hgf/toggle-theme))

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

;; * Minor modes

;; ** Git
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

;; * Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
