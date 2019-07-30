(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(use-package evil-numbers)

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


(use-package change-inner)
(use-package dumb-jump)
(use-package zenburn-theme)
