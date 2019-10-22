;; SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
;; SPDX-License-Identifier: MIT
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(load (emacs.d (format "machine/%s/pre.el" (system-name))) 'noerror)

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
