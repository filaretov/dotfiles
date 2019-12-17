(defun emacs.d (filename)
  "Return file path relative to the Emacs directory."
  (format "%s%s" user-emacs-directory filename))

(defun journal.d (filename)
  "Return file path relative to the Journal directory."
  (format "%s%s" "~/cloud/journal/" filename))
