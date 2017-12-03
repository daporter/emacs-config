;;; package --- Summary
;;; Commentary:

;; Phunculist's Emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))

(when (window-system)
  (progn
    (set-face-attribute 'default
                        nil
                        :font "Fira Code"
                        :width 'normal
                        :height 120
                        :weight 'light)
    (setq-default line-spacing 3)       ; in pixels

    ;; Enable the Fira Code ligatures.
    (mac-auto-operator-composition-mode 1)))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Suppress `ad-handle-definition' warnings (mostly from 3rd-party packages).
(setq ad-redefinition-action 'accept)

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "override" "lisp")))

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


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(require 'bind-key)
(require 'diminish nil t)

(defvar user-data-directory (expand-file-name "data" user-emacs-directory))


(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "backups/auto-save-list/.saves-" user-emacs-directory))

;; Keep all auto-save files in the temp directory.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default user-full-name    "David Porter"
              user-mail-address "david.a.porter@gmail.com")

(when (eq system-type 'darwin)
  (progn
    (setq mac-control-modifier 'control)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Prevent extraneous tabs.
(setq-default indent-tabs-mode nil)

;; On OS X, use the GNU Coreutils version of `ls', installed by
;; Homebrew, which is called `gls'.
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

;;; Configure libraries

(eval-and-compile
  (push (expand-file-name "lib" user-emacs-directory) load-path))

(use-package dash :ensure t :defer t)
(use-package flymake-easy :ensure t :defer t)

(use-package bookmark
  :ensure t
  :config (setq bookmark-default-file
                (expand-file-name "bookmarks" user-data-directory)))

(use-package company
  :ensure t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

(use-package define-word
  :ensure t
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package chruby
  :ensure t
  :config (chruby "ruby-2.2.3"))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :diminish paredit-mode)

(use-package lisp-mode
  :defer t
  :preface
  (defun my-lisp-mode-hook ()
    (progn
      (use-package edebug)
      (use-package eldoc
        :diminish eldoc-mode
        :commands eldoc-mode))

    (auto-fill-mode 1)
    (paredit-mode 1)

    (local-set-key (kbd "<return>") 'paredit-newline)

    (add-hook 'after-save-hook 'check-parens nil t))

  :init
  (apply #'hook-into-modes 'my-lisp-mode-hook lisp-mode-hooks))

(use-package macrostep
  :ensure t
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :mode "\\.markdown\\'"
  :commands markdown-mode
  :init (use-package markdown-mode+ :ensure t))

(use-package org-journal
  :ensure t
  :config (progn
            (setq org-journal-dir "~/Dropbox/journal/")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package recentf
  :config (setq recentf-save-file
                (expand-file-name "recentf" user-data-directory)))

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
  :ensure t
  :commands twit)

(use-package url-cache
  :init (setq url-cache-directory (expand-file-name "url/cache" user-data-directory)))

(use-package yaml-mode
  :ensure t
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (markdown-mode+ git-commit-mode magit)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
