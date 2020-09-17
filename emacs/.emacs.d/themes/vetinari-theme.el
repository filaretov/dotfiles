;;; vetinari.el --- SI NON CONFECTVS, NON REFICIAT

;; Copyright (C) 2020-present

;; Title: Vetinari
;; Project: vetinari-emacs

;;; Commentary:

;;; References:

;;; Code:

(deftheme vetinari "Si non convectvs, non reficiat.")

;;; Color Palette

(defvar vetinari-default-colors-alist
  '(("vetinari-blue"    . "#54b5de")
    ("vetinari-cyan"    . "#54dade")
    ("vetinari-lime"    . "#80d3a4")
    ("vetinari-yellow"  . "#d3cb80")
    ("vetinari-red"     . "#de6954")
    ("vetinari-orange"  . "#de8a54")
    ("vetinari-white"   . "#f0f0f0")
    ("vetinari-black"   . "#2e3440")
    ("vetinari-grey"    . "#aab2c2"))

  "Vetinari color palette. Each element has the form (NAME . HEX).")

(defvar vetinari-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar vetinari-colors-alist
  (append vetinari-default-colors-alist vetinari-override-colors-alist))

(defmacro vetinari-with-color-variables (&rest body)
  "`let' bind all colors defined in `vetinari-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
	 ,@(mapcar (lambda (cons)
		     (list (intern (car cons)) (cdr cons)))
		   vetinari-colors-alist))
     ,@body))

(vetinari-with-color-variables
 (custom-theme-set-faces
  'vetinari
   ;; --- Base ---
   `(default          ((t (:foreground ,vetinari-day :background ,vetinari-night))))

   `(cursor           ((t (:background ,vetinari-day :foreground ,vetinari-night))))
   `(font-lock-comment-face ((t (:foreground ,vetinari-grey))))

   ;; --- Base Programming ---
   `(font-lock-string-face  ((t (:foreground ,vetinari-lime))))
   `(font-lock-keyword-face ((t (:foreground ,vetinari-blue))))
   `(font-lock-warning-face ((t (:foreground ,vetinari-yellow))))
   `(font-lock-builtin-face ((t (:foreground ,vetinari-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,vetinari-lime))))
   `(font-lock-function-name-face ((t (:foreground ,vetinari-blue))))
   `(font-lock-type-face    ((t (:foreground ,vetinari-blue))))

   ;; --- Outline ---
   `(outline-1      ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(outline-2      ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))
   `(outline-3      ((t (:foreground ,vetinari-cyan :background ,vetinari-night :weight normal))))
   `(outline-4      ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(outline-5      ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))
   `(outline-6      ((t (:foreground ,vetinari-cyan :background ,vetinari-night :weight normal))))
   `(outline-7      ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(outline-8      ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))

   ;; --- Org ---
   `(org-level-1      ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(org-level-2      ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))
   `(org-level-3      ((t (:foreground ,vetinari-cyan :background ,vetinari-night :weight normal))))
   `(org-level-4      ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(org-level-5      ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))
   `(org-level-6      ((t (:foreground ,vetinari-cyan :background ,vetinari-night :weight normal))))
   `(org-level-7      ((t (:foreground ,vetinari-day :background ,vetinari-night :weight normal))))
   `(org-level-8      ((t (:foreground ,vetinari-day :background ,vetinari-night :weight normal))))
   `(org-date         ((t (:foreground ,vetinari-blue :background ,vetinari-night :weight normal))))
   `(org-done         ((t (:foreground ,vetinari-lime :background ,vetinari-night :weight normal))))
   `(org-todo         ((t (:foreground ,vetinari-orange :background ,vetinari-night :weight normal))))
   `(org-link         ((t (:foreground ,vetinari-cyan :background ,vetinari-night :weight normal :underline t))))

   ;; --- Flymake ---
   `(flymake-error    ((t (:underline (:style wave :color ,vetinari-red)))))
   ))

;;; Rainbow support
(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar vetinari-add-font-lock-keywords nil
  "Whether to add font-lock keywords for vetinari color names.

In buffers visiting library `vetinari-theme.el' the vetinari
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
definded).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
vetinari-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'.")

(defvar vetinari-colors-font-lock-keywords nil)

(defun vetinari--rainbow-turn-on ()
  "Maybe also add font-lock keywords for vetinari colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
	     (or vetinari-add-font-lock-keywords
		 (and (buffer-file-name)
		      (or
		       (equal (file-name-nondirectory (buffer-file-name))
			      "vetinari-theme.el")
		       (equal (file-name-nondirectory (buffer-file-name))
			      "lipwig-theme.el")))))
    (unless vetinari-colors-font-lock-keywords
      (setq vetinari-colors-font-lock-keywords
	    `((,(regexp-opt (mapcar 'car vetinari-default-colors-alist) 'words)
	       (0 (rainbow-colorize-by-assoc vetinari-default-colors-alist))))))
    (font-lock-add-keywords nil vetinari-colors-font-lock-keywords 'end)))

(defun vetinari--rainbow-turn-off ()
  "Also remove font-lock keywords for vetinari colors."
  (font-lock-remove-keywords nil vetinari-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'vetinari--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'vetinari--rainbow-turn-off))


(provide-theme 'vetinari)
