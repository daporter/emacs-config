;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-osx.html

;; Load OS specific settings.
(phunculist/load-init-file (concat "phunculist/system/"
                                   (symbol-name system-type))
                           t)

;; Load system specific settings.
(phunculist/load-init-file (concat "phunculist/system/"
                                   (car (split-string (system-name) "\\.")))
                           t)

;; load minimal early system settings
(phunculist/load-init-file "phunculist/system/minimal.el")
