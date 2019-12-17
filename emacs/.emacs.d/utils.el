(defun emacs.d (filename)
  "Return the complete file path."
  (format "%s%s" user-emacs-directory filename))

(defun journal.d (filename)
  "Return complete path."
  (format "%s%s" hgf-journal-path filename))
