;; Phunculist's Emacs configuration.

;; Turn off mouse interface early in startup to avoid momentary
;; display.
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq user-data-directory
      (concat (expand-file-name user-emacs-directory) "data/"))

(defun phunculist/load-init-file (path &optional noerror)
  "This loads a file from inside the the .emacs.d directory"
  (let ((file (file-name-sans-extension
               (expand-file-name path user-emacs-directory))))
    (load file noerror)))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;; Emacs server
(require 'server)
(unless (server-running-p) (server-start))

(set-face-attribute 'default nil :font "DejaVu LGC Sans Mono" :height 140)

(setq backup-directory-alist
      (list (cons "." (concat user-emacs-directory "backups"))))

(setq auto-save-list-file-prefix
      (concat user-emacs-directory "backups/auto-save-list/.saves-"))

;; Keep all auto-save files in the temp directory.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq recentf-save-file (concat user-data-directory "recentf"))
(setq url-cache-directory (concat user-data-directory "url/cache"))

;; Use GNU Coreutils version of `ls', which is called `gls' when installed via
;; Homebrew.
(setq insert-directory-program "gls")

;; Replace list-buffers with ibuffer.
(defalias 'list-buffers 'ibuffer)

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(bind-key "C-c n" 'cleanup-buffer)

;;;_. Packages

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(use-package auto-complete-config
  :config (progn
            (ac-config-default)
            (ac-set-trigger-key "TAB")
            (ac-flyspell-workaround)

            (setq ac-comphist-file
                  (concat user-data-directory "ac-comphist.dat"))

            ;; Advice for whitespace-mode conflict.
            ;; Copied from https://github.com/bbatsov/prelude/issues/19
            (defvar my-prev-whitespace-mode nil)
            (make-variable-buffer-local 'my-prev-whitespace-mode)

            (defadvice popup-draw (before my-turn-off-whitespace)
              "Turn off whitespace mode before showing autocomplete box"
              (make-local-variable 'my-prev-whitespace-mode)
              (if whitespace-mode
                  (progn
                    (setq my-prev-whitespace-mode t)
                    (whitespace-mode -1))
                (setq my-prev-whitespace-mode nil)))

            (defadvice popup-delete (after my-restore-whitespace)
              "Restore previous whitespace mode when deleting autocomplete box"
              (if my-prev-whitespace-mode
                  (whitespace-mode 1)))

            (ad-activate 'popup-draw)
            (ad-activate 'popup-delete)))

(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(use-package solarized-theme
  :config (load-theme 'solarized-dark t))

(unless (package-installed-p 'fill-column-indicator)
  (package-install 'fill-column-indicator))
(use-package fill-column-indicator)

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(unless (package-installed-p 'ace-jump-mode)
  (package-install 'ace-jump-mode))
(use-package ace-jump-mode
  :bind ("C-c j" . ace-jump-mode))

(unless (package-installed-p 'ag)
  (package-install 'ag))
(use-package ag)

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(use-package projectile
  :config (progn
            (setq projectile-cache-file
                  (concat user-data-directory "projectile.cache"))

            (unless (package-installed-p 'flx-ido)
              (package-install 'flx-ido))
            (use-package flx-ido)
            (projectile-global-mode 1)))

(use-package ido
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)

            ;; Disable ido faces to see flx highlights.
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces            nil)

            (setq ido-save-directory-list-file
                  (concat user-data-directory "ido.last"))

            (unless (package-installed-p 'ido-vertical-mode)
              (package-install 'ido-vertical-mode))
            (use-package ido-vertical-mode
              :config (ido-vertical-mode 1))

            (unless (package-installed-p 'flx-ido)
              (package-install 'flx-ido))
            (use-package flx-ido
              :config (flx-ido-mode 1))

            (unless (package-installed-p 'ido-ubiquitous)
              (package-install 'ido-ubiquitous))
            (use-package ido-ubiquitous
              :config (ido-ubiquitous-mode 1))))

(unless (package-installed-p 'smex)
  (package-install 'smex))
(use-package smex
  :config (progn
            (smex-initialize)
            (global-set-key (kbd "M-x") 'smex)
            (global-set-key (kbd "M-X") 'smex-major-mode-commands)
            ;; This is your old M-x.
            (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(unless (package-installed-p 'magit)
  (package-install 'magit))
(use-package magit
  :config (bind-key "C-x g" 'magit-status)
  :init (progn
          (unless (package-installed-p 'magit-commit-training-wheels)
            (package-install 'magit-commit-training-wheels))
          (use-package magit-commit-training-wheels
            :init (ad-activate 'magit-log-edit-commit))
          (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")))

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(use-package multiple-cursors)

(unless (package-installed-p 'smartparens)
  (package-install 'smartparens))
(use-package smartparens)

;; ;;;_ , AUCTeX

;; (use-package tex-site
;;   :load-path "site-lisp/auctex/preview/"

;;   :init (progn
;;           (hook-into-modes 'TeX-source-correlate-mode '(LaTeX-mode-hook))
;;           (hook-into-modes 'TeX-PDF-mode '(LaTeX-mode-hook))
;;           (hook-into-modes (lambda ()
;;                              (add-to-list 'TeX-expand-list
;;                                           '("%q" make-skim-url)))
;;                            '(LaTeX-mode-hook))

;;           (use-package))

;;;_ , undo-tree

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

;;;_ , uniquify

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;;_ , web-mode

;; (use-package web-mode
;;   :init (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;;;_ , whitespace

(unless (package-installed-p 'whitespace)
  (package-install 'whitespace))
(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :init (progn
          (hook-into-modes 'whitespace-mode
                           '(prog-mode-hook
                             c-mode-common-hook
                             ruby-mode-hook
                             haml-mode-hook))

          (defun normalize-file ()
            (interactive)
            (save-excursion
              (goto-char (point-min))
              (whitespace-cleanup)
              (delete-trailing-whitespace)
              (goto-char (point-max))
              (delete-blank-lines)
              (set-buffer-file-coding-system 'unix)
              (goto-char (point-min))
              (while (re-search-forward "\r$" nil t)
                (replace-match ""))
              (set-buffer-file-coding-system 'utf-8)
              (let ((require-final-newline t))
                (save-buffer))))

          (defun maybe-turn-on-whitespace ()
            "Depending on the file, maybe clean up whitespace."
            (let ((file (expand-file-name ".clean"))
                  parent-dir)
              (while (and (not (file-exists-p file))
                          (progn
                            (setq parent-dir
                                  (file-name-directory
                                   (directory-file-name
                                    (file-name-directory file))))
                            ;; Give up if we are already at the root dir.
                            (not (string= (file-name-directory file)
                                          parent-dir))))
                ;; Move up to the parent dir and try again.
                (setq file (expand-file-name ".clean" parent-dir)))
              ;; If we found a change log in a parent, use that.
              (when (and (file-exists-p file)
                         (not (file-exists-p ".noclean"))
                         (not (and buffer-file-name
                                   (string-match "\\.texi\\'"
                                                 buffer-file-name))))
                (add-hook 'write-contents-hooks
                          #'(lambda ()
                              (ignore (whitespace-cleanup))) nil t)
                (whitespace-cleanup))))

          (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t))

  :config (progn
            (remove-hook 'find-file-hooks 'whitespace-buffer)
            (remove-hook 'kill-buffer-hook 'whitespace-buffer)
            (setq whitespace-style '(empty face tabs trailing))))


(unless (package-installed-p 'dash-at-point)
  (package-install 'dash-at-point))
(use-package dash-at-point)

(unless (package-installed-p 'chruby)
  (package-install 'chruby))
(use-package chruby
  :init (chruby "ruby-2.1.2"))

(unless (package-installed-p 'puppet-mode)
  (package-install 'puppet-mode))
(use-package puppet-mode
  :config (progn
            (unless (package-installed-p 'flymake-puppet)
              (package-install 'flymake-puppet))
            (use-package flymake-puppet
              :init (hook-into-modes 'flymake-puppet-load '(puppet-mode-hook)))))

(unless (package-installed-p 'flymake-cursor)
  (package-install 'flymake-cursor))
(use-package flymake-cursor)

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(use-package markdown-mode
  :config (progn
            (unless (package-installed-p 'markdown-mode+)
              (package-install 'markdown-mode+))
            (use-package markdown-mode+)))

(use-package tramp
  :config (progn
            ;; Configure Tramp for use with NCI cloud VMs.
            (add-to-list 'tramp-default-proxies-alist
                         '("130\\.56\\." nil "/ssh:dap900@cloudlogin.nci.org.au:"))))

(unless (package-installed-p 'window-number)
  (package-install 'window-number))
(use-package window-number
  :init (progn
          (window-number-mode 1)
          (window-number-meta-mode 1)))

(unless (package-installed-p 'winner)
  (package-install 'winner))
(use-package winner
  :if (not noninteractive)
  :diminish winner-mode

  :init (progn
          (winner-mode 1)
          (bind-key "M-N" 'winner-redo)
          (bind-key "M-P" 'winner-undo)))

;; ;;;_ , YAML mode

(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))
(use-package yaml-mode)

;; ;;;_ , yasnippet

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(use-package yasnippet
  :mode     ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :commands (yas/minor-mode yas/expand)
  :diminish yas/minor-mode

  ;; :init (yas-global-mode 1)

  :config (progn
            (setq yas-snippet-dirs     (concat user-emacs-directory "snippets")
                  yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

            ;; Use only my own snippets, not the bundled ones.
            (yas-load-directory yas-snippet-dirs)))

(unless (package-installed-p 'keyfreq)
  (package-install 'keyfreq))
(use-package keyfreq
  :init (keyfreq-mode 1))

(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
(use-package ledger-mode
  :config (progn
            (unless (package-installed-p 'flycheck-ledger)
              (package-install 'flycheck-ledger))
            (use-package flycheck-ledger)))

(unless (package-installed-p 'notmuch)
  (package-install 'notmuch))
(use-package notmuch
  :bind ("C-c m" . notmuch)
  :config (progn
            (setq notmuch-hello-thousands-separator ",")
            (setq notmuch-search-oldest-first       nil)
            (setq notmuch-wash-wrap-lines-length    80)

            (setq notmuch-tag-formats
                  ;; Set to red from sanityinc-tomorrow-night.
                  '(("unread" (propertize
                               tag 'face '(:foreground "#cc6666")))
                    ("flagged" (notmuch-tag-format-image-data
                                tag (notmuch-tag-star-icon)))))

            (setq notmuch-search-line-faces
                  '(("unread" :weight bold)
                    ;; Set to green from sanityinc-tomorrow-night.
                    ("flagged" :foreground "#b5bd68")))

            (setq notmuch-archive-tags '("-inbox" "-unread" "+archive"))

            (setq notmuch-fcc-dirs
                  '(("david.a.porter@gmail.com" . "gmail/sent")
                    ("david.porter@anu.edu.au"  . "anu/sent")
                    (".*" . "gmail/sent")))

            (setq notmuch-saved-searches
                  '((:name "unread" :query "tag:unread" :sort-order 'oldest-first)
                    (:name "anu unread" :query "tag:anu AND tag:unread" :sort-order 'oldest-first)
                    (:name "gmail unread" :query "tag:gmail AND tag:unread" :sort-order 'oldest-first)
                    (:name "inbox" :query "tag:inbox" :sort-order 'newest-first)
                    (:name "flagged" :query "tag:flagged" :sort-order 'newest-first)
                    (:name "drafts" :query "tag:draft" :sort-order 'newest-first)
                    (:name "sent" :query "tag:sent" :sort-order 'newest-first)
                    (:name "all mail" :query "*" :sort-order 'newest-first)))

            (defun notmuch-search-mark-deleted ()
              "Mark this email as deleted."
              (interactive)
              (when (y-or-n-p "Are you sure you want to delete this message?")
                (notmuch-search-tag '("-inbox" "-archive" "-unread" "+trash"))
                (notmuch-search-next-thread)))

            (defun notmuch-show-mark-deleted ()
              "Mark this email as deleted."
              (interactive)
              (when (y-or-n-p "Are you sure you want to delete this message?")
                (notmuch-show-tag '("-inbox" "-archive" "-unread" "+trash"))
                (notmuch-show-next-thread)))

            (define-key
              notmuch-hello-mode-map  (kbd "g")   'notmuch-refresh-this-buffer)
            (define-key
              notmuch-search-mode-map (kbd "g")   'notmuch-refresh-this-buffer)
            (define-key
              notmuch-search-mode-map (kbd "d")   'notmuch-search-mark-deleted)
            (define-key
              notmuch-show-mode-map   (kbd "d")   'notmuch-show-mark-deleted)
            (define-key
              notmuch-show-mode-map   (kbd "RET") 'goto-address-at-point)
            (define-key
              notmuch-show-mode-map   (kbd "TAB") 'notmuch-show-toggle-message)
            (define-key
              notmuch-show-mode-map   (kbd "C-c n") 'notmuch-show-next-button)

            (use-package notmuch-address
              :config (progn
                        (setq notmuch-address-command "notmuch-contacts")
                        (notmuch-address-message-insinuate)

                        ;; We need to override this function to make
                        ;; it work nicely with `ido-completing-read'.
                        (defun notmuch-address-expand-name ()
                          (let* ((end (point))
                                 (beg (save-excursion
                                        (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                                        (goto-char (match-end 0))
                                        (point)))
                                 (orig (buffer-substring-no-properties beg end))
                                 (completion-ignore-case t)
                                 (options (notmuch-address-options orig))
                                 (num-options (length options))
                                 (chosen (cond
                                          ((eq num-options 0)
                                           nil)
                                          ((eq num-options 1)
                                           (car options))
                                          (t
                                           ;; (funcall notmuch-address-selection-function
                                           ;;       (format "Address (%s matches): " num-options)
                                           ;;       (cdr options) (car options))))))
                                           ;;
                                           ;; Instead of choosing the first option, as in the
                                           ;; default implementation, we pass the whole list of
                                           ;; options, and use the string entered so far for the
                                           ;; selection.
                                           (funcall notmuch-address-selection-function
                                                    (format "Address (%s matches): " num-options)
                                                    options orig)))))
                            (if chosen
                                (progn
                                  (push chosen notmuch-address-history)
                                  (delete-region beg end)
                                  (insert chosen))
                              (message "No matches.")
                              (ding))))))))

;; Mail.

(setq mail-user-agent 'message-user-agent
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      ;; This is needed to allow msmtp to do its magic:
      message-sendmail-f-is-evil t
      ;; Need to tell msmtp which account we're using.
      message-sendmail-extra-arguments '("--read-envelope-from"))

(setq user-mail-address "david.a.porter@gmail.com"
      user-full-name    "David Porter")

;; Misc functions.

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Make "RET" do whatever "M-j" does.
(defun phunculist/rebind-return ()
  (local-set-key (kbd "RET") (key-binding (kbd "M-j"))))

(hook-into-modes 'phunculist/rebind-return '(prog-mode-hook))

;; Colorise ansi escape codes in compilation buffers.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode 1))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Use ediff in single-frame mode.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ;; Local Variables:
;; ;;   mode: emacs-lisp
;; ;;   mode: allout
;; ;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; ;; End:
