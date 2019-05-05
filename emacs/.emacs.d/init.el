;; You see, yak shaving is what you are doing when you're doing some stupid, fiddly little
;; task that bears no obvious relationship to what you're supposed to be working on, but
;; yet a chain of twelve causal relations links what you're doing to the original
;; meta-task.
;;
;; -- Jeremy H. Brown

;; * Packaging Preparation
;; ** Helpers
(defun hgf/windows-os-p ()
  (string= system-type "windows-nt"))

(defun hgf/darwin-os-p ()
  (eq system-type 'darwin))

(defun hgf/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun hgf/proxy-on ()
  (interactive)
  (setq url-proxy-services
	'(("http"     . "153.96.56.101:3128")
	  ("https"    . "153.96.56.101:3128")
	  ("ftp"      . "153.96.56.101:3128")
	  ("no_proxy" . "^.*153.96.56.101"))))

(defun hgf/proxy-off ()
  (interactive)
  (setq url-proxy-services nil))

;; ** Blasphemy
(when (hgf/windows-os-p)
  (hgf/proxy-on))

;; ** Routine
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(hgf/package-init)
(setq use-package-always-ensure t)

;; ** Package shenanigans
(use-package quelpa-use-package
  :config (quelpa-use-package-activate-advice))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; * Change Customfile
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(setq custom-theme-directory (concat user-emacs-directory "themes/"))


;; * Meta
(defun hgf/edit-or-load-user-init-file ()
  "Find the custom user init file if it's not the current buffer, otherwise load it."
  (interactive)
  (if (string-equal (buffer-file-name) (file-truename user-init-file))
      (load-file user-init-file)
    (find-file user-init-file)))

;; * Start server
(server-mode 1)

;; * Keybindings setup
;; ** General.el
(use-package general)

;; ** Definers
(general-create-definer def-leader-key
  :prefix "SPC"
  :states 'normal
  :keymaps 'override)

(general-create-definer def-help-key
  :prefix "SPC h"
  :states 'normal)

(general-create-definer def-file-key
  :prefix "SPC f"
  :states 'normal)

(general-create-definer def-dispatch-key
  :prefix "SPC d"
  :states 'normal)

(general-create-definer def-mode-key
  :prefix "SPC m"
  :states 'normal)

(general-create-definer def-ge-key
  :prefix "g e"
  :states 'normal)

(general-create-definer def-g-key
  :prefix "g"
  :states 'normal)

(general-create-definer def-w-key
  :prefix "SPC w"
  :states 'normal)

(general-create-definer def-ctrlw-key
  :prefix "C-w"
  :states 'normal)

;; ** Helper functions
(defun hgf/insert-end-of-buffer ()
  (interactive)
  (end-of-buffer)
  (evil-insert-state))

(defun hgf/ansi-term-fish ()
  (interactive)
  (ansi-term "/usr/bin/fish"))

(defun hgf/term-fish ()
  (interactive)
  (term "/usr/bin/fish"))

;; Credit: https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun hgf/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ** Fundamental binds
(def-help-key
  "v" 'counsel-describe-variable
  "f" 'counsel-describe-function
  "k" 'describe-key
  "w" 'where-is
  "m" 'describe-mode
  "P" 'describe-package)

(def-file-key
  "f" 'find-file
  "s" 'save-buffer
  "b" 'ibuffer
  "o" 'ivy-switch-buffer
  "i" 'hgf/switch-to-previous-buffer
  "e" 'hgf/edit-or-load-user-init-file)

(def-dispatch-key
  "d" 'magit
  "l" 'magit-list-repositories
  "e" 'eshell
  "t" 'hgf/ansi-term-fish
  "T" 'hgf/term-fish)

(def-w-key
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "q" 'evil-window-delete
  "m" 'kill-this-buffer
  "1" 'delete-other-windows
  "0" 'delete-window
  "w" 'hydra-window/body)

(def-ge-key
  "e" 'eval-last-sexp
  "i" 'eval-defun)

(def-ctrlw-key
  "1" 'delete-other-windows)

(general-def '(normal visual)
  "L" 'evil-end-of-visual-line
  "H" 'evil-first-non-blank-of-visual-line
  "?"   'swiper
  "C-u" 'evil-scroll-up ;; sorry universal-argument
  )

(general-def '(normal visual insert)
  "C-e" 'end-of-line
  "C-a" 'beginning-of-line
  "C-k" 'kill-line
  "C-s" 'save-buffer)

(general-def 'insert
  "C-x C-f" 'company-files
  "C-y" 'evil-paste-before
  "<C-i>" 'company-complete)

;; Going back to evil from emacs state
(general-def "<C-[>" 'evil-normal-state)
;; Swapity swap
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key [remap evil-next-line] 'evil-next-visual-line)
(global-set-key [remap evil-previous-line] 'evil-previous-visual-line)

;; ** Hydra
(use-package hydra)
(defhydra hydra-window ()
  "Window management" 
  ("h" evil-window-left "left")
  ("j" evil-window-down "down")
  ("k" evil-window-up "up")
  ("l" evil-window-right "right")
  ("s" evil-window-split "split")
  ("v" evil-window-vsplit "vsplit")
  ("q" evil-window-delete "delete")
  ("f" find-file "file")
  ("o" ivy-switch-buffer "buffer")
  ("m" kill-this-buffer "murder")
  ("1" delete-other-windows "highlander")
  ("." nil "stop"))
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
(cond ((hgf/windows-os-p)
       (set-face-attribute 'default nil
			   :family "Inconsolata"
			   :height 120 ))
      ((hgf/darwin-os-p)
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 120
			   :weight 'semi-bold))
      (t
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :height 100
			   :weight 'regular)))

;; ** Theme
(setq solarized-use-variable-pitch nil
      solarized-emphasize-indicators nil
      solarized-high-contrast-mode-line t
      solarized-scale-org-headlines nil)
(defun hgf/toggle-theme ()
  "Toggle between solarized variants."
  (interactive)
  (if (equal (car custom-enabled-themes) dark-theme)
      (progn
	(disable-theme dark-theme)
	(load-theme light-theme))
    (progn
      (disable-theme light-theme)
      (load-theme dark-theme))))

(setq dark-theme 'nord)
(setq light-theme 'solarized-light)
(load-theme dark-theme t)

;; ** Cursor
(blink-cursor-mode 0)

;; ** Scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

;; ** Help me remember things
(use-package which-key
  :config
  (which-key-mode))

;; ** Parens
(use-package highlight-sexp
  :ensure nil)

(show-paren-mode 1)

;; ** Bells
(setq ring-bell-function 'ignore)
;; ** VC symlinks
(setq vc-follow-symlinks t)

;; ** Modeline
(column-number-mode 1)
(setq evil-normal-state-tag   (propertize " N " 'face '((:background "dark khaki" :foreground "black")))
      evil-emacs-state-tag    (propertize " E " 'face '((:background "turquoise" :foreground "black")))
      evil-insert-state-tag   (propertize " I " 'face '((:background "dark sea green" :foreground "black")))
      evil-replace-state-tag  (propertize " R " 'face '((:background "dark orange" :foreground "black")))
      evil-motion-state-tag   (propertize " M " 'face '((:background "khaki" :foreground "black")))
      evil-visual-state-tag   (propertize " V " 'face '((:background "light salmon" :foreground "black")))
      evil-operator-state-tag (propertize " O " 'face '((:background "sandy brown" :foreground "black"))))
(setq evil-mode-line-format '(before . mode-line-front-space))
(setq-default mode-line-format
	      (list
	       '#("%e")
	       'mode-line-front-space
	       'mode-line-buffer-identification
	       " "
	       ;; 'mode-line-mule-info
	       ;; 'mode-line-client
	       ;; 'mode-line-modified
	       '#("[%*]")
	       " "
	       "("
	       'mode-name
	       ")"
	       " "
	       '(vc-mode vc-mode)
	       'mode-misc-info
	       " "
	       'mode-line-position
	       'mode-line-end-spaces))
;; 'mode-line-remote
;; 'mode-line-frame-identification
;; "   "
;; "   "
;; '(vc-mode vc-mode)
;; "  "
;; 'mode-line-modes
;; 'mode-line-misc-info
;; 'mode-line-end-spaces))))
;; * Typing Text
;; ** Curious Characters
(setq default-input-method "TeX")
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

;; ** Being all fancy
(use-package olivetti)

;; ** Visual lines
(global-visual-line-mode 1)
(fringe-mode '(0 . 0))

;; ** Filling
(setq-default fill-column 90)

;; ** Aggressive-indent-mode
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes '(python-mode rst-mode)))
;; * Major mode configuration
;; ** LISPS
;; *** General
(defun lisp-modes ())

;; *** Common Lisp
(use-package slime
  :mode (("\\.cl\\'" . common-lisp-mode))
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(add-hook 'lisp-mode-hook 'lisp-modes)
(add-hook 'racket-mode-hook 'lisp-modes)
(add-hook 'emacs-lisp-mode-hook 'lisp-modes)

;; *** Racket
(use-package racket-mode)
(use-package scribble-mode)

;; ** C mode
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

;; ** Org mode
(use-package org)
(use-package htmlize)

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-goto-interface 'outline-path-completionp
      org-outline-path-complete-in-steps nil
      org-M-RET-may-split-line nil)

(use-package ox-extra
  :ensure org-plus-contrib
  :config
  (ox-extras-activate '(ignore-headlines)))


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

(general-def 'normal org-mode-map
  "M-i" 'org-goto
  ">" 'org-do-demote
  "<" 'org-do-promote
  "<backtab>" 'org-shifttab)

(def-g-key
  :keymaps 'org-mode-map
  "t" 'org-todo
  "x" 'org-open-at-point)

(def-file-key 'normal org-mode-map
  "n" 'org-narrow-to-subtree
  "w" 'widen)

;; ** LaTex mode
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
    (auto-fill-mode 1)
    (TeX-source-correlate-mode 1)))

(add-hook 'LaTeX-mode-hook 'hgf--latex-hook)

;; to use pdfview with auctex
(unless (hgf/windows-os-p)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)

(defun hgf--bibtex-hook ()
  (progn
    (setq comment-start "%")))

(add-hook 'bibtex-mode-hook 'hgf--bibtex-hook)

(setq-default TeX-auto-save t)
(setq-default TeX-parse-self t)
(setq-default TeX-PDF-mode t)
(setq-default TeX-auto-local "~/.emacs.d/auctex-auto")
(setq bibtex-dialect 'biblatex)

;; ** Eshell
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

;; ** Term
(add-hook 'term-mode-hook #'toggle-truncate-line 1)

;; ** Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

;; ** Fish
(use-package fish-mode)

;; ** Python
(use-package python-mode
  :config
  (setq py-shell-name "python3")
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-shell-use-project-root nil))

(remove-hook 'elpy-modules 'elpy-module-flymake)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(use-package company-jedi)
(use-package blacken)

(setenv "WORKON_HOME" "~/.miniconda3/envs/")

(def-mode-key
  :keymaps 'python-mode-map
  "f" 'blacken-buffer)

;; ** Rust
(use-package rust-mode)
(use-package racer)
(use-package cargo)
(defun hgf--rust-hook ()
  (progn
    (racer-mode 1)
    (cargo-minor-mode 1)))

(add-hook 'rust-mode-hook 'hgf--rust-hook)

;; ** Yaml
(use-package yaml-mode)

;; ** ReStructured Text
(eval-after-load "rst"
  '(setq rst-mode-map (make-sparse-keymap)))
;; ** Ledger
(use-package ledger-mode)

(use-package evil-ledger
  :after '(ledger-mode evil-mode)
  :config
  (add-hook 'ledger-mode-hook 'evil-ledger-mode))

;; * Minor mode configuration
;; ** Outshine
(use-package outshine
  :config
  (setq outshine-startup-folded-p t))

(add-hook 'conf-mode-hook #'outshine-mode 1)
(add-hook 'prog-mode-hook #'outshine-mode 1)
(add-hook 'bibtex-mode-hook #'outshine-mode 1)
(add-hook 'LaTeX-mode-hook #'outshine-mode 1)

(general-def 'normal outshine-mode-map
  "M-i" 'outshine-imenu
  "<backtab>" 'outshine-cycle-buffer
  "<tab>" 'evil-toggle-fold)

(def-file-key 'normal outshine-mode-map
  "n" 'outshine-narrow-to-subtree
  "w" 'widen)

;; ** Evil
;; *** Init
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
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
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

;; *** Escape from Emacs state
(global-set-key (kbd "<escape>") 'evil-normal-state)
(when (window-system)
  ;; Separate C-[ and ESC when a window system is available
  (progn
    (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    (define-key input-decode-map [?\C-i] (kbd "<C-i>"))))

;; *** Keybindings
(defun hgf/outline-show-complete-outline ()
  "Outline: show all, then hide body."
  (interactive)
  (outline-show-all)
  (outline-hide-body))
;; ** Company
;; *** Init
(use-package company
  :hook (after-init . global-company-mode))
;; *** Add backends
;; **** Global

;; **** Python
(add-hook 'python-mode-hook (lambda ()
			      (add-to-list (make-local-variable 'company-backends)
					   'company-jedi)))

;; ** Rainbow mode
(use-package rainbow-mode)
;; ** Ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :config
  (counsel-mode 1))

;; ** Magit
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
;; * RSS
(use-package elfeed
  :config
  (setq eleed-feeds
	'("http://nullprogram.com/feed/"
	  "https://harryrschwartz.com/atom.xml")))
