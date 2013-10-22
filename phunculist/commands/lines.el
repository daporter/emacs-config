;;;###autoload
(defun phunculist/newline-below (skip-eol)
  "Insert a new line below the current line and indent it."
  (interactive "P")
  (unless (or (eolp) skip-eol)
    (end-of-line))
  (newline-and-indent))

;;;###autoload
(defun phunculist/newline-above ()
  "Insert a new line above the current line and indent it."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (previous-line)
  (indent-according-to-mode))

;;;###autoload
(defun phunculist/newline-between ()
  "Splits content before and after the point to insert new content between."
  (interactive)
  (phunculist/trim-backwards-and-forwards)
  (newline)
  (newline)
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))
