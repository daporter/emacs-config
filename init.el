;;; package --- Summary
;;; Commentary:

;; Phunculist's Emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(when (window-system)
  (progn
    (set-face-attribute 'default
                        nil
                        :font "Fira Code"
                        :width 'normal
                        :height 120
                        :weight 'normal)
    (setq-default line-spacing 3)       ; in pixels

    ;; Enable the Fira Code ligatures.
    (mac-auto-operator-composition-mode 1)))

(setq inhibit-startup-message t)

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "override" "lisp")))

(defvar user-data-directory (expand-file-name "data" user-emacs-directory))

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "backups/auto-save-list/.saves-" user-emacs-directory))

;; Keep all auto-save files in the temp directory.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (eq system-type 'darwin)
  (progn
    (setq mac-control-modifier 'control)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

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

(eval-and-compile
  (push (expand-file-name "lib" user-emacs-directory) load-path))

(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :init (progn
          (setq ivy-use-virtual-buffers t)
          (setq ivy-count-format "(%d/%d) "))
  :config (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate)))

(use-package dash :ensure t :defer t)
(use-package flymake-easy :ensure t :defer t)

(use-package bookmark
  :ensure t
  :config (setq bookmark-default-file
                (expand-file-name "bookmarks" user-data-directory)))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

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
  :ensure t)

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
 '(package-selected-packages
   (quote
    (which-key try counsel swiper markdown-mode+ git-commit-mode magit)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
