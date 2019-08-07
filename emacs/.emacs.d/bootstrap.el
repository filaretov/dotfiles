;; SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
;; SPDX-License-Identifier: MIT
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(load (format "~/.emacs.d/machine/%s/pre.el" (getenv "HOSTNAME")) 'noerror)

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
