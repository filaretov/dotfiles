;;; weatherwax.el --- A simple theme, well suited for headology

;; Copyright (C) 2020-present

;; Title: Weatherwax
;; Project: weatherwax-emacs

;;; Commentary:

;;; References:

;;; Code:
;;;; Colors
(require 'autothemer)

(autothemer-deftheme
 weatherwax "A theme for headology."

 ((((class color) (min-colors #xFFFFFF)))

  (ww-purple   "#c991e1")
  (ww-pink     "#f5a4da")
  (ww-orange   "#ffaf5f")
  (ww-lime     "#82d9a9")
  (ww-yellow   "#e9e58a")
  (ww-blue     "#7ea0d3")
  (ww-cyan     "#5cc9e8")
  (ww-red      "#de6954")
  (ww-lred     "#ff8080")
  (ww-white    "#f0f0f0")
  (ww-hl       "#ced2db")
  (ww-grey     "#aaaaaa")
  (ww-lblack   "#4B5467")
  (ww-ablack   "#313844")
  (ww-black    "#100e23")
  )

;;;; Base
 ((default          (:foreground ww-white :background ww-black))
  (error            (:foreground ww-red))
  (cursor           (:background ww-white :foreground ww-black))
  (font-lock-comment-face (:foreground ww-grey))

;;;; UI
  (mode-line            (:foreground ww-black :background ww-yellow))
  (mode-line-inactive   (:background ww-lblack))
  (mode-line-highlight   (:background ww-ablack :foreground ww-white))
  (mode-line-emphasis   (:background ww-ablack :foreground ww-white :weight 'bold))
  (header-line            (:foreground ww-black :background ww-lime))
  (header-line-inactive   (:background ww-lblack))
  (header-line-highlight   (:background ww-ablack :foreground ww-white))
  (header-line-emphasis   (:background ww-ablack :foreground ww-white :weight 'bold))
  (vertical-border      (:foreground ww-white))
  (region               (:background ww-hl))
  (fringe               (:background ww-black))
  (tab-bar              (:background ww-black))
  (tab-bar-tab          (:background ww-lblack :underline t))
  (tab-bar-tab-inactive (:background ww-black))

  (minibuffer-prompt    (:foreground ww-lime))

;;;; Base
  (font-lock-string-face   (:foreground ww-lime))
  (font-lock-keyword-face  (:foreground ww-orange))
  (font-lock-warning-face  (:foreground ww-red))
  (font-lock-builtin-face  (:foreground ww-yellow))
  (font-lock-function-name-face (:foreground ww-yellow))
  (font-lock-type-face     (:foreground ww-orange))
  (font-lock-constant-face (:foreground ww-yellow))

;;;; Outline
  (outline-1      (:foreground ww-orange :weight 'normal))
  (outline-2      (:foreground ww-lime :weight 'normal))
  (outline-3      (:foreground ww-yellow :weight 'normal))
  (outline-4      (:foreground ww-lred :weight 'normal))
  (outline-5      (:foreground ww-orange :weight 'normal))
  (outline-6      (:foreground ww-lime :weight 'normal))
  (outline-7      (:foreground ww-yellow :weight 'normal))
  (outline-8      (:foreground ww-lred :weight 'normal))

;;;; Org
  (org-level-1      (:foreground ww-orange :weight 'normal))
  (org-level-2      (:foreground ww-lime :weight 'normal))
  (org-level-3      (:foreground ww-yellow :weight 'normal))
  (org-level-4      (:foreground ww-lred :weight 'normal))
  (org-level-5      (:foreground ww-orange :weight 'normal))
  (org-level-6      (:foreground ww-lime :weight 'normal))
  (org-level-7      (:foreground ww-white :weight 'normal))
  (org-level-8      (:foreground ww-white :weight 'normal))
  (org-date         (:foreground ww-blue :weight 'normal))
  (org-done         (:foreground ww-lime :weight 'normal))
  (org-todo         (:foreground ww-orange :weight 'normal))
  (org-link         (:foreground ww-cyan :weight 'normal :underline t))
  (org-document-title         (:foreground ww-white :weight 'normal :underline nil))
  (org-document-info         (:foreground ww-white  :weight 'normal :underline nil))
  (org-document-info-keyword (:foreground ww-grey :weight 'normal :underline nil))
  (org-block (:inherit nil))

;;;; Flymake
  (flymake-error    (:underline (:style 'wave :color ww-red)))

;;;; Yasnippet
  (yas-field-highlight-face (:background ww-lblack))

;;;; VTerm
  (vterm               (:foreground ww-white))
  (vterm-color-black   (:background ww-black   :foreground ww-black))
  (vterm-color-red     (:background ww-red     :foreground ww-red))
  (vterm-color-green   (:background ww-lime    :foreground ww-lime))
  (vterm-color-yellow  (:background ww-yellow  :foreground ww-yellow))
  (vterm-color-blue    (:background ww-blue    :foreground ww-blue))
  (vterm-color-magenta (:background ww-purple  :foreground ww-purple))
  (vterm-color-cyan    (:background ww-cyan    :foreground ww-cyan))
  (vterm-color-white   (:background ww-white   :foreground ww-white))

;;;; Eshell
  (eshell-prompt (:foreground ww-lime))
  
;;;; Treemacs
  (treemacs-root-face (:height 1.0))
;;;; Company
  (company-tooltip    (:background ww-lblack :foreground ww-white))

;;;; Latex
  (font-latex-sectioning-1-face (:height 1.0))
  (font-latex-sectioning-2-face (:height 1.0))
  (font-latex-sectioning-3-face (:height 1.0))
  (font-latex-sectioning-4-face (:height 1.0))
  (font-latex-sectioning-5-face (:height 1.0))
  (font-latex-sectioning-6-face (:height 1.0))

  (font-latex-slide-title-face (:height 1.0))
  ))

;;; I ATE'NT DEAD
(provide-theme 'weatherwax)
