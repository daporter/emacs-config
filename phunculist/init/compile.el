(defun phunculist/byte-recompile-init ()
  "Recompile the files I use to setup Emacs."
  (byte-recompile-directory (expand-file-name "phunculist"
                                              user-emacs-directory)
                            0)
  ;; don't compile init.el because it's too late to freshen it on load
  ;; loaddefs.el cannot be compiled
  )

(phunculist/byte-recompile-init)
