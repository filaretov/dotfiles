;;; lipwig-theme.el --- An impostor's light theme.

;; Copyright (C) 2020 Hristo Filaretov

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; <insert lipwig quote here>

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme for Emacs, on which this source
;; code is based.
;; https://github.com/bbatsov/zenburn-emacs

;;; Code:

(deftheme lipwig "The Moist von Lipwig color theme")

;;; Color Palette
(defvar lipwig-default-colors-alist
  '(("lipwig-white"                  . "#ffffff")
    ("lipwig-black"                  . "#24292e")

    ("lipwig-orange"                 . "#ff5200")
    ("lipwig-red"                    . "#fe2500")
    ("lipwig-purple"                 . "#9d2dab")
    ("lipwig-teal"                   . "#04c4c7")
    ("lipwig-blue"                   . "#005cc5")
    ("lipwig-green"                  . "#10d7ae")
    ))

;;; Theme Faces
;;; Theme Variables

;;; Rainbow support
(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar lipwig-add-font-lock-keywords nil
  "Whether to add font-lock keywords for lipwig color names.

In buffers visiting library `lipwig-theme.el' the lipwig
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
lipwig-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'.")

(defvar lipwig-colors-font-lock-keywords nil)

(defun lipwig--rainbow-turn-on ()
  "Maybe also add font-lock keywords for lipwig colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
	     (or lipwig-add-font-lock-keywords
		 (and (buffer-file-name)
		      (equal (file-name-nondirectory (buffer-file-name))
			     "lipwig-theme.el"))))
    (unless lipwig-colors-font-lock-keywords
      (setq lipwig-colors-font-lock-keywords
	    `((,(regexp-opt (mapcar 'car lipwig-default-colors-alist) 'words)
	       (0 (rainbow-colorize-by-assoc lipwig-default-colors-alist))))))
    (font-lock-add-keywords nil lipwig-colors-font-lock-keywords 'end)))

(defun lipwig--rainbow-turn-off ()
  "Also remove font-lock keywords for lipwig colors."
  (font-lock-remove-keywords nil lipwig-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'lipwig--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'lipwig--rainbow-turn-off))


