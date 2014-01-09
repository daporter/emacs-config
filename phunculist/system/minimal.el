;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-sanity.html

;; This file is also loaded very early in the Emacs boot sequence, so it's
;; the right place to make changes that need to be in effect even during
;; the boot sequence.

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

;; Keep emacs Custom-settings in separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
