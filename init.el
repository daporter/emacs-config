;; Record current time in order to calculate time taken to for Emacs
;; to start.
(defconst emacs-start-time (current-time))

;; Configure package system.
(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; For Org Mode.
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;;; Display.

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (tool-bar-mode -1)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

(when (window-system)
  (progn
    (set-face-attribute 'default
                        nil
                        :family "DejaVu Sans Mono"
                        :width 'normal
                        :height 120
                        :weight 'normal)

    (setq-default line-spacing 4)       ; in pixels

    ;; Enable ligatures.
    (mac-auto-operator-composition-mode 1)))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'ansi-color)
(defun dp/colourise-compilation ()
  "Colourise from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'dp/colourise-compilation)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq gc-cons-threshold 50000000)

(setq gnutls-min-prime-bits 4096)

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "backups/auto-save-list/.saves-" user-emacs-directory))

(setq-default user-full-name    "David Porter"
              user-mail-address "david.a.porter@gmail.com")

(setq ad-redefinition-action 'accept)

(setq-default indent-tabs-mode nil)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(defalias 'list-buffers 'ibuffer)

(setq-default fill-column 80)

;; Remove trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Smart tab behavior - indent or complete.
(setq tab-always-indent 'complete)

(global-set-key (kbd "C-c c") 'compile)

(global-hl-line-mode 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package epa
  :config (setq epa-pinentry-mode 'loopback))

(use-package zenburn-theme
  :disabled
  ;; :custom-face (region ((t (:background "#5F5F5F"))))
  :config (load-theme 'zenburn t))

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config (load-theme 'sanityinc-tomorrow-night t))

(use-package zerodark-theme
  :config (progn
            (load-theme 'zerodark t)
            (zerodark-setup-modeline-format)))

;; (load-theme 'typo t)

(use-package powerline
  :config (powerline-default-theme))

(use-package exec-path-from-shell
  :config (progn
            (setq exec-path-from-shell-arguments
                  (delete "-i" exec-path-from-shell-arguments))
            (setq exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-initialize)))

(use-package server
  :config (unless (server-running-p) (server-start)))

(use-package which-key
  :diminish which-key-mode
  :config (progn
            (which-key-mode 1)))

(use-package flx)

(use-package swiper
  :after flx
  :bind (:map isearch-mode-map
              ("C-." . swiper-from-isearch))
  :bind (:map swiper-map
              ("M-h" . swiper-avy))
  :diminish ivy-mode
  :config (progn
            (setq ivy-use-virtual-buffers t)
            ;; Make the default completion mechanism a fuzzy search. However,
            ;; you don't really want to use fuzzy matching on lists that have
            ;; content with a lot of spaces (like documents), so disable for
            ;; swiper.
            (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                          (t . ivy--regex-fuzzy)))
            (ivy-mode 1)))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-c C-r" . ivy-resume)
         ("C-c e" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate)
         ("M-y"     . counsel-yank-pop)))

(use-package avy
  :bind (("C-c j"   . avy-goto-word-1)
         ("M-g C-g" . avy-goto-line))
  :config (progn
            (avy-setup-default)
            ;; Favour keys on the Dvorak home row.
            (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))))

(use-package company
  :bind ("C-c C-<tab>" . company-complete)
  :hook (after-init . global-company-mode)
  :diminish company-mode
  :config (progn
            (setq company-idle-delay 0.5
                  company-tooltip-limit 10
                  company-minimum-prefix-length 2
                  company-show-numbers t
                  ;; Invert the navigation direction if the the completion
                  ;; popup-isearch-match is displayed on top (happens near the
                  ;; bottom of windows).
                  company-tooltip-flip-when-above t)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  ;; Use the Dvorak home row keys instead of numbers for the window labels.
  :config (setq aw-keys '(?a ?o ?e ?u ?d ?h ?t ?n ?s)))

(use-package org
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages '((sh . t)))
            ;; syntax-highlight source code blocks:
            (setq org-src-fontify-natively t)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :after org
  :bind ("C-c C-j" . org-journal-new-entry))

(use-package ws-butler
  :diminish ws-butler-mode
  :config (ws-butler-global-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-completing-read-function 'ivy-completing-read)
            (setq git-commit-style-convention-checks
                  '(overlong-summary-line non-empty-second-line))))

(use-package smartparens-config
  :ensure smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a"           . sp-beginning-of-sexp)
              ("C-M-e"           . sp-end-of-sexp)
              ("C-S-u"           . sp-up-sexp)
              ("C-S-d"           . sp-backward-down-sexp)
              ("C-S-f"           . sp-forward-symbol)
              ("C-S-b"           . sp-backward-symbol)
              ("M-]"             . sp-unwrap-sexp)
              ("M-["             . sp-backward-unwrap-sexp)
              ("C-M-t"           . sp-transpose-sexp)
              ("M-<backspace>"   . sp-backward-kill-sexp)
              ("C-S-<backspace>" . sp-kill-whole-line))
  :config (progn
            (setq sp-base-key-bindings 'sp)
            (smartparens-global-strict-mode 1)
            (show-smartparens-global-mode 1)
            (sp-use-smartparens-bindings)))

(use-package paren-face
  :config (global-paren-face-mode))

(use-package projectile
  :config (progn
            (setq projectile-completion-system 'ivy)
            (setq projectile-switch-project-action #'projectile-find-dir
                  projectile-find-dir-includes-top-level t)
            (setq projectile-mode-line
                  '(:eval (format " Proj[%s]" (projectile-project-name))))
            (projectile-mode 1)))

(use-package counsel-projectile
  :after projectile
  :commands counsel-projectile
  :config (counsel-projectile-on))

(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer)
  :config (when (executable-find "hunspell")
            (setq-default ispell-program-name "hunspell")
            (setq ispell-really-hunspell t)))

(use-package flyspell
  :commands (flyspell-mode
             turn-on-flyspell
             turn-off-flyspell
             flyspell-prog-mode)
  :hook (((text-mode git-commit-mode-hook) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package define-word
  :bind (("C-c w" . define-word-at-point)
         ("C-c W" . define-word)))

(use-package keyfreq
  :config (progn
            (setq keyfreq-file
                  (expand-file-name "emacs.keyfreq" user-emacs-directory))
            (setq keyfreq-excluded-commands '(backward-char
                                              forward-char
                                              next-line
                                              org-self-insert-command
                                              previous-line
                                              self-insert-command))
            (keyfreq-mode 1)
            (keyfreq-autosave-mode 1)))

(use-package switch-buffer-functions
  :disabled)

(use-package flycheck
  :config (progn
            (flycheck-define-checker proselint
              "A linter for prose."
              :command ("proselint" source-inplace)
              :error-patterns ((warning
                                line-start
                                (file-name) ":" line ":" column ": "
                                (id (one-or-more (not (any " "))))
                                (message) line-end))
              :modes (text-mode markdown-mode gfm-mode message-mode))

            (add-to-list 'flycheck-checkers 'proselint)
            (global-flycheck-mode 1)))

(use-package avy-flycheck
  :after (avy flycheck)
  :config (avy-flycheck-setup))

(use-package yasnippet
  :commands (yas-minor-mode)
  :hook (prog-mode . yas-minor-mode)
  :diminish yas-minor-mode
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (global-aggressive-indent-mode 1))

(use-package visible-mark
  ;; :custom-face (visible-mark-face1
  ;;               ((t (:inherit widget-field))))
  ;; :custom-face (visible-mark-face2
  ;;               ((t (:inherit ivy-minibuffer-match-face-1))))
  :config (progn
            (setq visible-mark-max 2)
            (setq visible-mark-faces
                  '(visible-mark-face2 visible-mark-face1))
            (global-visible-mark-mode 1)))

(add-hook 'ruby-mode-hook (lambda () (subword-mode 1)))

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode))

(use-package rvm
  :hook (ruby-mode . rvm-activate-corresponding-ruby)
  :config (rvm-use-default))

(use-package minitest
  :hook (ruby-mode . minitest-enable-appropriate-mode)
  :config (minitest-install-snippets))

(use-package yari)

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c d" . dash-at-point))

(use-package idle-highlight-mode
  :hook prog-mode)

(use-package markdown-mode
  :mode "\\.markdown\\'"
  :commands markdown-mode)

(use-package markdown-mode+
  :after markdown-mode)

(use-package twittering-mode
  :commands twit)

(use-package yaml-mode
  :mode "\\.yaml\\'")

;; Since `proced' doesn't work on MacOS, use `vkill' instead.
(use-package vkill
  :commands (list-unix-processes))

(use-package hardcore-mode
  :diminish hardcore-mode
  :init (setq too-hardcore-backspace t)
  :config (global-hardcore-mode 1))

(use-package try
  :commands try)

(setq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory))

;; Calculate and report the time taken for Emacs to start.
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
