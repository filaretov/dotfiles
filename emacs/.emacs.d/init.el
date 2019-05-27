;; * 0. Defun package-init
(defun hgf/package-init ()
  "Initialize the package manager and install use-package."
  (progn
    (require 'package)
    (setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")
	    ("org" . "https://orgmode.org/elpa/")))
    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))))

;; * 1. Prepare packaging
(hgf/package-init)

;; * 2. Let it rip
(org-babel-load-file "~/.emacs.d/configuration.org")
(put 'set-goal-column 'disabled nil)
