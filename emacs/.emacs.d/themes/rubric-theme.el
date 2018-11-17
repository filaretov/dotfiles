;; * rubric-theme.el ---  A more classical approach to theming -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.
;; Copyright (C) 2013 Amirreza Ghaderi
;; Copyright (C) 2017-18 Simon Zelazny
;; Copyright (C) 2018 Hristo Filaretov
;; Authors: Amirreza Ghaderi <amirreza.blog@gmail.com>,
;;          Simon Zelazny <zelazny@mailbox.org>
;;          Hristo Filaretov <h.filaretov@protonmail.com>,
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; * Commentary:

;; * Credits:

;; This theme is based on commentary-theme.el by Amirezza Ghaderi, which is based on
;; minimal-light-theme by Amirezza Ghaderi.

;; * License:
;; Use of this source code is governed by the 'Revised BSD License'
;; which can be found in the LICENSE file.

;; * Code:
;; ** Proper
(deftheme rubric
  "A minimal theme with contrasting comments")


(let* ((black   "#404040")
       (white   "#ffffff")
       (gray      "#909090")   ;; text, code
       (gray255   "#eeeeee")   ;; fringe
       ;; (blue      "#40a0e0")   ;; fringe
       (blue      "#268bd2")
       ;; (red       "#f06545")   ;; match
       (red       "#dc322f")
       ;; (orange    "#ff9000")   ;; match
       (orange    "#cb4b16")
       ;; (green     "#20c020")
       (brown     "#c09040")
       ;; Solarized accented colors
       (yellow    "#b58900")
       (magenta   "#d33682")
       (violet    "#6c71c4")
       (cyan      "#2aa198")
       (green     "#859900")
       (gray253    "#dadada")
       (soapy      "#eaffff")
       (strawberry "#f2e6e4")
       (yellow187  "#dfdfaf")   ;; current line (unused)
       (default-layer `((t (:foreground ,black :background ,white))))
       (string-layer `((t (:foreground ,gray :background ,white))))
       (bold-layer `((t (:foreground ,black :weight bold))))
       (comment-layer `((t (:foreground ,gray :background ,white :slant italic))))
       (blue-layer `((t (:foreground ,blue :background ,white))))
       (green-layer `((t (:foreground ,green :background ,white))))
       (brown-layer `((t (:foreground ,brown :background ,white))))
       (red-layer `((t (:foreground ,red :background ,white))))
       (red-bold-layer `((t (:foreground ,red :weight bold :background ,white))))
       (orange-layer `((t (:foreground ,orange :background ,white))))
      )


  ;; Set faces
  (custom-theme-set-faces
   'rubric

   `(default ,default-layer)
   `(cursor  ((t (:background ,black))))

   ;; Highlighting
   `(fringe    ((t (:background ,gray255))))
   `(highlight ((t (:background ,white))))
   `(region    ((t (:background ,yellow187))))

   ;; Comments, documentation are contrastive
   `(font-lock-comment-face       ,comment-layer)
   `(font-lock-doc-face           ,comment-layer)
   `(font-lock-string-face        ,string-layer)
   `(font-lock-function-name-face ,blue-layer)
   `(font-lock-constant-face      ,green-layer)
   `(font-lock-variable-name-face ,default-layer)
   `(font-lock-builtin-face       ,blue-layer)
   `(font-lock-keyword-face       ,bold-layer)
   `(font-lock-type-face          ,default-layer)

   ;; Parentheses
   `(show-paren-mismatch
     ((t (:foreground ,blue :background ,gray255 :weight bold))))
   `(show-paren-match
     ((t (:foreground ,red  :background ,gray255 :weight bold))))

   ;; Line numbers, current line, mode-line
   `(hl-line      ((t (:background ,yellow187))))  ;;current line
   `(linum        ((t (:background ,gray253))))    ;;line numbers


;; ** Org
   `(org-level-2 ,default-layer)
   `(org-level-3 ,default-layer)
   `(org-level-4 ,default-layer)
   `(org-level-5 ,default-layer)
   `(org-level-6 ,default-layer)
   `(org-level-7 ,default-layer)
   `(org-level-8 ,default-layer)
   `(org-level-9 ,default-layer)
   `(org-todo ,red-bold-layer)
   `(outline-1 ,bold-layer)

;; ** LaTeX
   `(font-latex-sectioning-1-face ,default-layer)

;; ** Python
   `(py-number-face ((t (:inherit 'font-lock-constant-face))))

   )

  ;; Set variables
  (custom-theme-set-variables 'rubric)
)

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'rubric)
(provide 'rubric-theme)

;; Local Variables:
;; ----no-byte-compile: t
;; End:
;; * rubric-theme.el ends here
