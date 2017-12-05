;;; package --- Summary
;;; Commentary:

;; Phunculist's Emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "backups/auto-save-list/.saves-" user-emacs-directory))


;; Suppress `ad-handle-definition' warnings (mostly from 3rd-party packages).
(setq ad-redefinition-action 'accept)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(setq-default user-full-name    "David Porter"
              user-mail-address "david.a.porter@gmail.com")

;; Prevent extraneous tabs.
(setq-default indent-tabs-mode nil)

;; On OS X, use the GNU Coreutils version of `ls', installed by
;; Homebrew, which is called `gls'.
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

;; `ibuffer' is a bit nicer than `list-buffers'.
(defalias 'list-buffers 'ibuffer)


;;; Configure libraries


(use-package dash :defer t)
(use-package flymake-easy :defer t)

(use-package define-word
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package chruby
  :config (chruby "ruby-2.2.3"))

(use-package paredit
  :commands paredit-mode)

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package markdown-mode
  :mode "\\.markdown\\'"
  :commands markdown-mode
  :init (use-package markdown-mode+))

(use-package server
  :config (unless (server-running-p) (server-start)))

(use-package tramp
  :config (progn
            ;; Configure Tramp for use with the NCI cloud VMs.
            (setq tramp-default-method "ssh")
            (add-to-list 'tramp-default-proxies-alist
                         '("130\\.56\\."
                           nil
                           "/ssh:dap900@cloudlogin.nci.org.au:"))))

(use-package twittering-mode
  :commands twit)

(use-package yaml-mode
  :mode "\\.yaml\\'")


;;; Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


(org-babel-load-file (expand-file-name "my-init.org" user-emacs-directory))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme ace-window which-key try counsel swiper markdown-mode+ git-commit-mode magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
