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

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "backups/auto-save-list/.saves-" user-emacs-directory))

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
(setq use-package-always-ensure t)

(use-package try)

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package which-key
  :config (which-key-mode 1))

(use-package swiper
  :bind (("C-s"     . swiper)
         ("C-c C-r" . ivy-resume))
  :config (progn
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (setq ivy-display-style 'fancy)
            (ivy-mode 1)))

(use-package counsel
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

(use-package avy
  :bind (("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config (avy-setup-default))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package dash :defer t)
(use-package flymake-easy :defer t)

(use-package company
  :hook (after-init . global-company-mode))

(use-package define-word
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package chruby
  :config (chruby "ruby-2.2.3"))

(use-package paredit
  :commands paredit-mode)

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :mode "\\.markdown\\'"
  :commands markdown-mode
  :init (use-package markdown-mode+))

(use-package projectile
  :commands projectile-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

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
