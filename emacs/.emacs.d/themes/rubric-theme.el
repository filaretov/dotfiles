;;; rubric-theme.el ---  A more classical approach to theming -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.
;; Copyright (C) 2013 Amirreza Ghaderi
;; Copyright (C) 2017-18 Simon Zelazny
;; Copyright (C) 2018 Hristo Filaretov
;; Authors: Amirreza Ghaderi <amirreza.blog@gmail.com>,
;;          Simon Zelazny <zelazny@mailbox.org>
;;          Hristo Filaretov <h.filaretov@protonmail.com>,
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;;; Credits:

;; This theme is based on commentary-theme.el by Amirezza Ghaderi, which is based on
;; minimal-light-theme by Amirezza Ghaderi.

;;; License:
;; Use of this source code is governed by the 'Revised BSD License'
;; which can be found in the LICENSE file.

;;; Code:

(deftheme rubric
  "A minimal theme with contrasting comments")

(let* ((black016   "#404040")
       (white231   "#ffffff")
       (yellow230  "#fffffa")   ;; background
       (gray       "#909090")   ;; text, code
       (gray255    "#eeeeee")   ;; fringe
       (ruby       "#b03010")   ;; match
       (blue066    "#5f8787")   ;; strings
       (gray253    "#dadada")
       (soapy      "#eaffff")
       (strawberry "#f2e6e4")
       (yellow187  "#dfdfaf")   ;; current line (unused)
       (default-layer `((t (:foreground ,black016 :background ,white231))))
       (rubric-layer `((t (:foreground ,ruby :background ,white231))))
       (string-layer `((t (:foreground ,gray :background ,white231))))
       (bold-layer `((t (:foreground ,black016 :weight bold))))
       (rubric-bold-layer `((t (:foreground ,ruby :weight bold :background ,white231))))
      )


  ;; Set faces
  (custom-theme-set-faces
   'rubric

   `(default ,default-layer)
   `(cursor  ((t (:background ,black016))))

   ;; Highlighting
   `(fringe    ((t (:background ,gray255))))
   `(highlight ((t (:background ,white231))))
   `(region    ((t (:background ,yellow187))))

   ;; Comments, documentation are contrastive
   `(font-lock-comment-face ,rubric-layer)
   `(font-lock-doc-face ,rubric-layer)

   ;; String literals are highlighted
   `(font-lock-string-face ,string-layer)

   ;; Defintions are set in bold
   `(font-lock-function-name-face ,bold-layer)

   ;; Everything else belongs in the normal layer
   `(font-lock-constant-face      ,default-layer)
   `(font-lock-variable-name-face ,default-layer)
   `(font-lock-builtin-face       ,default-layer)
   `(font-lock-keyword-face       ,default-layer)
   `(font-lock-type-face          ,default-layer)

   ;; Parentheses
   `(show-paren-mismatch
     ((t (:foreground ,blue066 :background ,gray255 :weight bold))))
   `(show-paren-match
     ((t (:foreground ,ruby  :background ,gray255 :weight bold))))

   ;; Line numbers, current line, mode-line
   `(hl-line      ((t (:background ,yellow187))))  ;;current line
   `(linum        ((t (:background ,gray253))))    ;;line numbers

   ;; Coloring for external modes
   ;; Elixir-mode
   `(elixir-attribute-face ,default-layer)
   `(elixir-atom-face ,default-layer)

   `(org-level-2 ,default-layer)
   `(org-level-3 ,default-layer)
   `(org-level-4 ,default-layer)
   `(org-level-5 ,default-layer)
   `(org-level-6 ,default-layer)
   `(org-level-7 ,default-layer)
   `(org-level-8 ,default-layer)
   `(org-level-9 ,default-layer)
   `(org-todo ,rubric-bold-layer)

   )

  ;; Set variables
  (custom-theme-set-variables 'rubric)
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'rubric)
(provide 'rubric-theme)

;; Local Variables:
;; ----no-byte-compile: t
;; End:
;;; rubric-theme.el ends here
