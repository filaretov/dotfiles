;; SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
;; SPDX-License-Identifier: MIT
(defun hgf/evil-normal-state-if-evil ()
  (when (bound-and-true-p evil-local-mode) (evil-normal-state)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode)
  (setq evil-emacs-state-cursor '(bar)
	evil-search-module 'evil-search
	evil-ex-search-case 'smart)
  (general-def "M-~" 'evil-local-mode)
  (general-def 'normal
    "L" 'evil-end-of-visual-line
    "H" 'evil-first-non-blank-of-visual-line
    "C-s" 'swiper
    "C-u" 'evil-scroll-up)
  (general-def 'insert
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line
    "C-k" 'kill-line
    "C-x C-f" 'company-files
    "C-y" 'yank)
  (general-def 'visual
    "L" 'end-of-line
    "H" 'beginning-of-line)
  (advice-add 'keyboard-quit :before #'hgf/evil-normal-state-if-evil)
  (global-set-key [remap evil-next-line] 'evil-next-visual-line)
  (global-set-key [remap evil-previous-line] 'evil-previous-visual-line))
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

(defun hgf/insert-end-of-buffer ()
  (interactive)
  (progn
    (end-of-buffer)
    (evil-insert-state)))

(use-package evil-magit)

(use-package evil-numbers)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

