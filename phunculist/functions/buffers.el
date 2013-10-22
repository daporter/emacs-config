;;;###autoload
(defun phunculist/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun phunculist/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun phunculist/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (phunculist/untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun phunculist/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (phunculist/cleanup-buffer-safe)
  (phunculist/indent-buffer))
