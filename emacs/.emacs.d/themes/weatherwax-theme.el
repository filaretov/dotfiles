;;; weatherwax.el --- A simple theme, mostly suited for headology

;; Copyright (C) 2020-present

;; Title: Weatherwax
;; Project: weatherwax-emacs

;;; Commentary:

;;; References:

;;; Code:

(deftheme weatherwax "A theme for headology")

;;; Color Palette

(defvar weatherwax-default-colors-alist
  '(("weatherwax-blue"    . "#54b5de")
    ("weatherwax-cyan"    . "#54dade")
    ("weatherwax-lime"    . "#80d3a4")
    ("weatherwax-yellow"  . "#d3cb80")
    ("weatherwax-red"     . "#de6954")
    ("weatherwax-orange"  . "#de8a54")
    ("weatherwax-white"   . "#f0f0f0")
    ("weatherwax-black"   . "#2e3440")
    ("weatherwax-grey"    . "#aaaaaa")
    ("weatherwax-swamp"   . "#91de54"))

  "Weatherwax color palette. Each element has the form (NAME . HEX).")

(defvar weatherwax-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar weatherwax-colors-alist
  (append weatherwax-default-colors-alist weatherwax-override-colors-alist))

(defmacro weatherwax-with-color-variables (&rest body)
  "`let' bind all colors defined in `weatherwax-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
	 ,@(mapcar (lambda (cons)
		     (list (intern (car cons)) (cdr cons)))
		   weatherwax-colors-alist))
     ,@body))

(weatherwax-with-color-variables
 (custom-theme-set-faces
  'weatherwax
   ;; --- Base ---
   `(default          ((t (:foreground ,weatherwax-white :background ,weatherwax-black))))

   `(cursor           ((t (:background ,weatherwax-white :foreground ,weatherwax-black))))
   `(font-lock-comment-face ((t (:foreground ,weatherwax-grey))))

   ;; --- UI ---
   `(mode-line           ((t (:foreground ,weatherwax-black :background ,weatherwax-lime))))
   `(mode-line-inactive  ((t (:foreground ,weatherwax-grey :underline t :overline t))))
   `(vertical-border     ((t (:foreground ,weatherwax-white))))

   ;; --- Base Programming ---
   `(font-lock-string-face  ((t (:foreground ,weatherwax-lime))))
   `(font-lock-keyword-face ((t (:foreground ,weatherwax-orange))))
   `(font-lock-warning-face ((t (:foreground ,weatherwax-red))))
   `(font-lock-builtin-face ((t (:foreground ,weatherwax-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,weatherwax-yellow))))
   `(font-lock-type-face    ((t (:foreground ,weatherwax-orange))))

   ;; --- Outline ---
   `(outline-1      ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(outline-2      ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))
   `(outline-3      ((t (:foreground ,weatherwax-cyan :background ,weatherwax-black :weight normal))))
   `(outline-4      ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(outline-5      ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))
   `(outline-6      ((t (:foreground ,weatherwax-cyan :background ,weatherwax-black :weight normal))))
   `(outline-7      ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(outline-8      ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))

   ;; --- Org ---
   `(org-level-1      ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(org-level-2      ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))
   `(org-level-3      ((t (:foreground ,weatherwax-cyan :background ,weatherwax-black :weight normal))))
   `(org-level-4      ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(org-level-5      ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))
   `(org-level-6      ((t (:foreground ,weatherwax-cyan :background ,weatherwax-black :weight normal))))
   `(org-level-7      ((t (:foreground ,weatherwax-white :background ,weatherwax-black :weight normal))))
   `(org-level-8      ((t (:foreground ,weatherwax-white :background ,weatherwax-black :weight normal))))
   `(org-date         ((t (:foreground ,weatherwax-blue :background ,weatherwax-black :weight normal))))
   `(org-done         ((t (:foreground ,weatherwax-lime :background ,weatherwax-black :weight normal))))
   `(org-todo         ((t (:foreground ,weatherwax-orange :background ,weatherwax-black :weight normal))))
   `(org-link         ((t (:foreground ,weatherwax-cyan :background ,weatherwax-black :weight normal :underline t))))

   ;; --- Flymake ---
   `(flymake-error    ((t (:underline (:style wave :color ,weatherwax-red)))))
   
   ))

;;; Rainbow support
(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar weatherwax-add-font-lock-keywords nil
  "Whether to add font-lock keywords for weatherwax color names.

In buffers visiting library `weatherwax-theme.el' the weatherwax
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
definded).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
weatherwax-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'.")

(defvar weatherwax-colors-font-lock-keywords nil)

(defun weatherwax--rainbow-turn-on ()
  "Maybe also add font-lock keywords for weatherwax colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
	     (or weatherwax-add-font-lock-keywords
		 (and (buffer-file-name)
		      (or
		       (equal (file-name-nondirectory (buffer-file-name))
			      "weatherwax-theme.el")
		       (equal (file-name-nondirectory (buffer-file-name))
			      "lipwig-theme.el")))))
    (unless weatherwax-colors-font-lock-keywords
      (setq weatherwax-colors-font-lock-keywords
	    `((,(regexp-opt (mapcar 'car weatherwax-default-colors-alist) 'words)
	       (0 (rainbow-colorize-by-assoc weatherwax-default-colors-alist))))))
    (font-lock-add-keywords nil weatherwax-colors-font-lock-keywords 'end)))

(defun weatherwax--rainbow-turn-off ()
  "Also remove font-lock keywords for weatherwax colors."
  (font-lock-remove-keywords nil weatherwax-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'weatherwax--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'weatherwax--rainbow-turn-off))


(provide-theme 'weatherwax)
