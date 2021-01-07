;;; weatherwax.el --- A simple theme, well suited for headology

;; Copyright (C) 2020-present

;; Title: Weatherwax
;; Project: weatherwax-emacs

;;; Commentary:

;;; References:

;;; Code:
(require 'autothemer)

(autothemer-deftheme
 weatherwax "A theme for headology."

 ((((class color) (min-colors #xFFFFFF)))

  (weatherwax-purple   "#a37ed3")
  (weatherwax-orange   "#d3a37e")
  (weatherwax-lime     "#80d3a4")
  (weatherwax-yellow   "#d3cd7e")
  (weatherwax-blue     "#7ea0d3")
  (weatherwax-cyan     "#7ed3cd")
  (weatherwax-red      "#de6954")
  (weatherwax-white    "#f0f0f0")
  (weatherwax-hl       "#ced2db")
  (weatherwax-grey     "#aaaaaa")
  (weatherwax-lblack   "#4B5467")
  (weatherwax-ablack   "#313844")
  (weatherwax-black    "#252A33")
  ;; (weatherwax-black    "#2e3440")
  )

 ;; --- Base ---
 ((default          (:foreground weatherwax-white :background weatherwax-black))
  (error            (:foreground weatherwax-red))
  (cursor           (:background weatherwax-white :foreground weatherwax-black))
  (font-lock-comment-face (:foreground weatherwax-grey))

  ;; --- UI ---
  (mode-line            (:foreground weatherwax-black :background weatherwax-lime))
  (mode-line-inactive   (:background weatherwax-lblack))
  (mode-line-highlight   (:background weatherwax-ablack :foreground weatherwax-white))
  (mode-line-emphasis   (:background weatherwax-ablack :foreground weatherwax-white :weight 'bold))
  (header-line            (:foreground weatherwax-black :background weatherwax-lime))
  (header-line-inactive   (:background weatherwax-lblack))
  (header-line-highlight   (:background weatherwax-ablack :foreground weatherwax-white))
  (header-line-emphasis   (:background weatherwax-ablack :foreground weatherwax-white :weight 'bold))
  (vertical-border      (:foreground weatherwax-white))
  (region               (:background weatherwax-hl))
  (fringe               (:background weatherwax-black))
  (tab-bar              (:background weatherwax-black))
  (tab-bar-tab          (:background weatherwax-lblack :underline t))
  (tab-bar-tab-inactive (:background weatherwax-black))

  (minibuffer-prompt    (:foreground weatherwax-lime))

  ;; --- Base Programming ---
  (font-lock-string-face   (:foreground weatherwax-lime))
  (font-lock-keyword-face  (:foreground weatherwax-orange))
  (font-lock-warning-face  (:foreground weatherwax-red))
  (font-lock-builtin-face  (:foreground weatherwax-yellow))
  (font-lock-function-name-face (:foreground weatherwax-yellow))
  (font-lock-type-face     (:foreground weatherwax-orange))
  (font-lock-constant-face (:foreground weatherwax-purple))

  ;; --- Outline ---
  (outline-1      (:foreground weatherwax-lime :weight 'normal))
  (outline-2      (:foreground weatherwax-blue :weight 'normal))
  (outline-3      (:foreground weatherwax-cyan :weight 'normal))
  (outline-4      (:foreground weatherwax-lime :weight 'normal))
  (outline-5      (:foreground weatherwax-blue :weight 'normal))
  (outline-6      (:foreground weatherwax-cyan :weight 'normal))
  (outline-7      (:foreground weatherwax-lime :weight 'normal))
  (outline-8      (:foreground weatherwax-blue :weight 'normal))

  ;; --- Org ---
  (org-level-1      (:foreground weatherwax-lime :weight 'normal))
  (org-level-2      (:foreground weatherwax-blue :weight 'normal))
  (org-level-3      (:foreground weatherwax-cyan :weight 'normal))
  (org-level-4      (:foreground weatherwax-lime :weight 'normal))
  (org-level-5      (:foreground weatherwax-blue :weight 'normal))
  (org-level-6      (:foreground weatherwax-cyan :weight 'normal))
  (org-level-7      (:foreground weatherwax-white :weight 'normal))
  (org-level-8      (:foreground weatherwax-white :weight 'normal))
  (org-date         (:foreground weatherwax-blue :weight 'normal))
  (org-done         (:foreground weatherwax-lime :weight 'normal))
  (org-todo         (:foreground weatherwax-orange :weight 'normal))
  (org-link         (:foreground weatherwax-cyan :weight 'normal :underline t))
  (org-document-title         (:foreground weatherwax-white :weight 'normal :underline nil))
  (org-document-info         (:foreground weatherwax-white  :weight 'normal :underline nil))
  (org-document-info-keyword (:foreground weatherwax-grey :weight 'normal :underline nil))
  (org-block (:inherit nil))

  ;; --- Flymake ---
  (flymake-error    (:underline (:style 'wave :color weatherwax-red)))

  ;; --- Yasnippet ---
  (yas-field-highlight-face (:background weatherwax-lblack))

  ;; --- VTerm ---
  (vterm               (:foreground weatherwax-white))
  (vterm-color-black   (:background weatherwax-black   :foreground weatherwax-black))
  (vterm-color-red     (:background weatherwax-red     :foreground weatherwax-red))
  (vterm-color-green   (:background weatherwax-lime    :foreground weatherwax-lime))
  (vterm-color-yellow  (:background weatherwax-yellow  :foreground weatherwax-yellow))
  (vterm-color-blue    (:background weatherwax-blue    :foreground weatherwax-blue))
  (vterm-color-magenta (:background weatherwax-purple  :foreground weatherwax-purple))
  (vterm-color-cyan    (:background weatherwax-cyan    :foreground weatherwax-cyan))
  (vterm-color-white   (:background weatherwax-white   :foreground weatherwax-white))

  ;; --- NeoTree ---
  (neo-file-link-face (:foreground weatherwax-white :height 'unspecified))
  (neo-dir-link-face (:foreground weatherwax-white))

  ;; --- Company ---
  (company-tooltip    (:background weatherwax-lblack :foreground weatherwax-white))
  ))

;;; I ATE'NT DEAD
(provide-theme 'weatherwax)
