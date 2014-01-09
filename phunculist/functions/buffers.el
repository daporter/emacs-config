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

;; From http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;;;###autoload
(defun phunculist/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; From http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;;;###autoload
(defun phunculist/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
