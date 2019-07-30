;;; * Initialize packaging
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; * Default wrangling

;;; ** Hi, my name is
(setq user-full-name "Hristo Filaretov"
      user-mail-address "h.filaretov@campus.tu-berlin.de")

;;; ** Visual clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fringe-mode 0)

(setq left-margin-width 10
      right-margin-width 50)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message ";; Don't panic\n\n"
      ring-bell-function 'ignore
      mode-line-default-help-echo nil
      left-margin-width nil
      right-margin-width nil)

(defun display-startup-echo-area-message ()
  (message ""))

;;; ** Backups
(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

;;; ** Convenience
(fset 'yes-or-no-p 'y-or-n-p)

;;; ** Visual cues
(show-paren-mode 1)
(global-hl-line-mode 1)

;;; * Theme
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (setq solarized-use-variable-pitch nil))

;;; * Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
