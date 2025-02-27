(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, ipt's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(defconst *sys/linux*
  (eq system-type 'gnu/linux))

(defconst *sys/windows*
  (eq system-type 'windows-nt))

(defconst *sys/mac*
  (eq system-type 'darwin))

(defun hgf/emacs-path (filename)
  "Return the file path of FILENAME relative to the Emacs directory."
  (format "%s%s" user-emacs-directory filename))

(defun hgf/org-path (filename)
  "Return the file path of FILENAME relative to the Org directory."
  (format "%s%s" "~/Projects/org" filename))

;; Personal Information
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@protonmail.com")

;; GUI stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore)
(let* '(font "Triplicate B Code-14")
  (set-face-font 'default font)
  (set-face-font 'fixed-pitch font))

;; Scrolling
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Backup (TODO: this should probably be more intelligent, actually)
(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

;; Load path
(add-to-list 'load-path (hgf/emacs-path "lisp"))

;; Custom file
(setq custom-file (hgf/emacs-path "custom.el"))
(load custom-file 'noerror)

;; For heresy (files changing outside Emacs)
(global-auto-revert-mode 1)

;; Show parens
(show-paren-mode 1)
(setq show-paren-delay 0.0)

;; Formatting stuff
(setq require-final-newline t
      sentence-end-double-space nil)

;; Quick y\n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make scripts executable
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Cursory changes
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(setq mouse-yank-at-point t)

;; VC
(setq vc-follow-symlinks t)

(setq mac-command-modifier 'meta)

(defun hgf/modeline-modified ()
  "Return buffer status: read-only (-), modified (·) or read-write ( )."
  (let ((read-only buffer-read-only)
	(modified  (and buffer-file-name (buffer-modified-p))))
    (cond
     (modified (propertize " + " 'face 'bold))
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
(general-def "C-c d" 'org-cut-subtree)

(use-package s)
(use-package dash)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-medium t))

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
	org-fontify-done-headline nil
	org-src-fontify-natively t
	org-src-preserve-indentation nil
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-src-window-setup 'current-window)
  (general-def org-mode-map
    "C-c d" 'org-cut-subtree
    "C-M-n" 'org-metadown
    "C-M-p" 'org-metaup
    "C-M-f" 'org-metaright
    "C-M-b" 'org-metaleft))

(use-package ledger-mode)
