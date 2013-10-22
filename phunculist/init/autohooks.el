;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-hooks.html

(defun phunculist/autohooks ()
  "Autoload everthing in the hooks directory as a hook for the named mode."
  (interactive)
  (dolist (path (directory-files (concat user-emacs-directory
                                         "phunculist/hooks")
                                 t
                                 "\\.el$"))
    (let* ((mode       (file-name-nondirectory
                        (file-name-sans-extension path)))
           (hook-name  (intern (concat mode "-hook")))
           (defun-name (intern (concat "phunculist/" mode "-hook")))
           (lisp       (phunculist/read-file-to-string path)))
      (eval (read (concat "(defun " (symbol-name defun-name) " () " lisp ")")))
      (and (functionp defun-name)
           (remove-hook hook-name defun-name))
      (add-hook hook-name defun-name))))

(phunculist/autohooks)
