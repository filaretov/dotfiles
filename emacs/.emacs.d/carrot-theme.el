;;; carrot.el --- A black on white theme.
;;; Code:
(unless (>= emacs-major-version 24)
  (error "Carrot requires Emacs 24 or later!"))


(deftheme carrot "A black on white theme.")

(let (
      (class '((class color) (min-colors 89)))
      (white       "#F5F5F5")
      (light-grey  "#EFEFEF")
      (grey        "#777777")
      (black       "#101010")
      (red         "#F02000")
      )
  (custom-theme-set-faces
   'carrot
   ;; --- Base ---
   `(default                 ((,class  (:foreground  ,black     :background  ,white))))
   `(cursor                  ((,class  (:background  ,black     :foreground  ,white))))
   `(font-lock-comment-face  ((,class  (:foreground  ,grey))))

   ;; --- UI Elements ---
   `(hl-line  ((,class  (:background ,light-grey))))
   `(minibuffer-prompt  ((,class  (:foreground  ,black))))

   ;; --- Base Programming ---
   `(font-lock-string-face         ((,class  (:foreground  ,black))))
   `(font-lock-keyword-face        ((,class  (:foreground  ,black))))
   `(font-lock-constant-face       ((,class  (:foreground  ,black))))
   `(font-lock-warning-face        ((,class  (:foreground  ,black))))
   `(font-lock-builtin-face        ((,class  (:foreground  ,black))))
   `(font-lock-function-name-face  ((,class  (:foreground  ,black))))
   `(font-lock-type-face           ((,class  (:foreground  ,black))))

   ;; --- Outline ---
   `(outline-1  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-2  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-3  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-4  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-5  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-6  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-7  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(outline-8  ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))

   ;; --- Org ---
   `(org-level-1                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-2                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-3                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-4                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-5                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-6                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-7                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-level-8                ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-date                   ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-done                   ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-todo                   ((,class  (:foreground  ,red    :background  ,white  :weight  normal))))
   `(org-verbatim               ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-drawer                 ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-special-keyword        ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-hide                   ((,class  (:foreground  ,white  :background  ,white  :weight  normal))))
   `(org-tag                    ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-document-title         ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-document-info          ((,class  (:foreground  ,black  :background  ,white  :weight  normal))))
   `(org-document-info-keyword  ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-block-begin-line       ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-block-end-line         ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-block                  ((,class  (:foreground  ,black  :background  ,white  :weight  normal :inherit fixed-pitch))))
   `(org-link                   ((,class  (:foreground  ,black  :background  ,white  :weight  normal :underline  t))))

   ;; --- Misc ---
   ))

(provide-theme 'carrot)
