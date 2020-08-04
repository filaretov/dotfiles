;;; weatherwax.el --- A simple theme, mostly suited for headology

;; Copyright (C) 2020-present

;; Title: Weatherwax
;; Project: weatherwax-emacs

;;; Commentary:

;;; References:

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Weatherwax requires Emacs 24 or later!"))

(deftheme weatherwax "A headology based theme")

(let (
      (class '((class color) (min-colors 89)))
      (wblue    "#54B5De")
      (wcyan    "#54DADE")
      (wlime    "#80D3A4")
      (wgreen   "#54DE7B")
      (wyellow  "#D3CB80")
      (wred     "#DE6954")
      (worange  "#DE8A54")
      (wwhite   "#F0F0F0")
      (wblack   "#2E3440")
      (wgrey    "#AAAAAA")
      (swamp "#91DE54")
      (leaf  "#54DE98")
      (sky   "#54DADE")
      (lake  "#54B5DE")
      (dusk  "#DE54AF")
      (cap   "#DE6554")
      (ember "#DE8A54")
      (solar "#DED854")
      (night "#2E3440")
      (day   "#F0F2F4")
      (grey  "#AAB2C2")
      )
  (custom-theme-set-faces
   'weatherwax
   ;; --- Base ---
   `(default          ((,class (:foreground ,day :background ,night))))
   `(cursor           ((,class (:background ,day :foreground ,night))))
   `(font-lock-comment-face ((,class (:foreground ,wgrey))))

   ;; --- Base Programming ---
   `(font-lock-string-face  ((,class (:foreground ,leaf))))
   `(font-lock-keyword-face ((,class (:foreground ,ember))))
   `(font-lock-warning-face ((,class (:foreground ,cap))))
   `(font-lock-builtin-face ((,class (:foreground ,solar))))
   `(font-lock-function-name-face ((,class (:foreground ,solar))))
   `(font-lock-type-face    ((,class (:foreground ,ember))))

   ;; --- Outline ---
   `(outline-1      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-2      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-3      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-4      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-5      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-6      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-7      ((,class (:foreground ,day :background ,night :weight normal))))
   `(outline-8      ((,class (:foreground ,day :background ,night :weight normal))))

   ;; --- Org ---
   `(org-level-1      ((,class (:foreground ,leaf :background ,night :weight normal))))
   `(org-level-2      ((,class (:foreground ,lake :background ,night :weight normal))))
   `(org-level-3      ((,class (:foreground ,sky :background ,night :weight normal))))
   `(org-level-4      ((,class (:foreground ,leaf :background ,night :weight normal))))
   `(org-level-5      ((,class (:foreground ,lake :background ,night :weight normal))))
   `(org-level-6      ((,class (:foreground ,sky :background ,night :weight normal))))
   `(org-level-7      ((,class (:foreground ,day :background ,night :weight normal))))
   `(org-level-8      ((,class (:foreground ,day :background ,night :weight normal))))
   `(org-date         ((,class (:foreground ,lake :background ,night :weight normal))))
   `(org-done         ((,class (:foreground ,leaf :background ,night :weight normal))))
   `(org-todo         ((,class (:foreground ,ember :background ,night :weight normal))))
   `(org-link         ((,class (:foreground ,sky :background ,night :weight normal :underline t))))
   ))

(provide-theme 'weatherwax)
