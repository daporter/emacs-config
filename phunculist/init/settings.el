(defun phunculist/load-settings ()
  "Loads all Lisp files in the settings subdirectory of the init directory."
  (dolist (file (directory-files (concat user-emacs-directory
                                         "phunculist/settings")
                                 nil
                                 "\\.el$"))
    (phunculist/load-init-file (concat "phunculist/settings/" file))))

(phunculist/load-settings)
