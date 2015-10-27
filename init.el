;;; package --- Summary
;;; Commentary:

;; Phunculist's Emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Suppress `ad-handle-definition' warnings (mostly from 3rd-party packages).
(setq ad-redefinition-action 'accept)

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "site-lisp/use-package" "override" "lisp")))

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

(use-package dash         :load-path "site-lisp/dash"         :defer t)
(use-package flymake-easy :load-path "site-lisp/flymake-easy" :defer t)

(use-package bookmark
  :config (setq bookmark-default-file
                (expand-file-name "bookmarks" user-data-directory)))

(use-package company
  :load-path "site-lisp/company-mode"
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
  :load-path "site-lisp/define-word"
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package exec-path-from-shell
  :load-path "site-lisp/exec-path-from-shell"
  :config (exec-path-from-shell-initialize))

(use-package helm-config
  :demand t
  :load-path "site-lisp/helm"

  :bind (("C-c h"     . helm-mini)
         ("C-h a"     . helm-apropos)
         ("C-x C-b"   . helm-buffers-list)
         ("C-x b"     . helm-buffers-list)
         ("M-x"       . helm-M-x)
         ("C-x c o"   . helm-occur)
         ;;("C-x c s"   . helm-swoop)
         ("C-x c y"   . helm-yas-complete)
         ("C-x c Y"   . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings))
  
  :config
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-ls-git
    :load-path "site-lisp/helm-ls-git")

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i"   'helm-execute-persistent-action helm-map)
  (bind-key "C-z"   'helm-select-action             helm-map)

  (bind-key "M-x"     'helm-M-x)
  (bind-key "M-y"     'helm-show-kill-ring)
  (bind-key "C-x b"   'helm-mini)
  (bind-key "C-x r b" 'helm-filtered-bookmarks)

  (helm-autoresize-mode 1)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package chruby
  :load-path "site-lisp/chruby"
  :init (chruby "ruby-2.2.3"))


;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :init (progn
;;           (use-package helm-config)

;;           (setq helm-candidate-number-limit 100)
;;           ;; From https://gist.github.com/antifuchs/9238468
;;           (setq helm-idle-delay 0.0  ; update fast sources immediately (doesn't)
;;                 ;; This actually updates things relatively quickly:
;;                 helm-input-idle-delay 0.01

;;                 helm-yas-display-key-on-candidate t
;;                 helm-quick-update t
;;                 helm-M-x-requires-pattern nil
;;                 helm-ff-skip-boring-files t)
;;           (helm-mode 1)
;;           ;; (use-package helm-eshell)
;;           ;; (use-package helm-files)
;;           ;; (use-package helm-grep
;;           ;;   :init (progn
;;           ;;           (bind-key "<return>"
;;           ;;                     'helm-grep-mode-jump-other-window
;;           ;;                     helm-grep-mode-map)
;;           ;;           (bind-key "n"
;;           ;;                     'helm-grep-mode-jump-other-window-forward
;;           ;;                     helm-grep-mode-map)
;;           ;;           (bind-key "p"
;;           ;;                     'helm-grep-mode-jump-other-window-backward
;;           ;;                     helm-grep-mode-map)))

;;           ;; (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
;;           ;; (bind-key "C-i"   'helm-execute-persistent-action helm-map)
;;           ;; (bind-key "C-z"   'helm-select-action             helm-map)

;;           ;; (setq
;;           ;;  helm-truncate-lines t
;;           ;;  helm-google-suggest-use-curl-p t
;;           ;;  helm-scroll-amount 4
;;           ;;  helm-quick-update t
;;           ;;  helm-idle-delay 0.01
;;           ;;  helm-input-idle-delay 0.01
;;           ;;  helm-ff-search-library-in-sexp t

;;           ;;  helm-split-window-default-side 'other
;;           ;;  helm-split-window-in-side-p t
;;           ;;  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
;;           ;;                                      '(picture-mode artist-mode))
;;           ;;  helm-candidate-number-limit 200
;;           ;;  helm-M-x-requires-pattern 0
;;           ;;  helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$"
;;           ;;                                 "\\.CVS$" "\\._darcs$" "\\.la$"
;;           ;;                                 "\\.o$" "\\.i$")
;;           ;;  helm-ff-file-name-history-use-recentf t
;;           ;;  helm-move-to-line-cycle-in-source t
;;           ;;  ido-use-virtual-buffers t
;;           ;;  helm-buffers-fuzzy-matching t)

;;           ;; ;; Save current position to mark ring when jumping to a different
;;           ;; ;; place.
;;           ;; (add-hook 'helm-goto-line-before-hook
;;           ;;           'helm-save-current-pos-to-mark-ring)

;;           ;; (bind-key "M-x"     'helm-M-x)
;;           ;; (bind-key "M-y"     'helm-show-kill-ring)
;;           ;; (bind-key "C-x b"   'helm-mini)
;;           ;; (bind-key "C-x r b" 'helm-filtered-bookmarks)

;;           )
;;   :bind (("C-c h"     . helm-mini)
;;          ("C-h a"     . helm-apropos)
;;          ("C-x C-b"   . helm-buffers-list)
;;          ("C-x b"     . helm-buffers-list)
;;          ("M-x"       . helm-M-x)
;;          ("C-x c o"   . helm-occur)
;;          ("C-x c s"   . helm-swoop)
;;          ("C-x c y"   . helm-yas-complete)
;;          ("C-x c Y"   . helm-yas-create-snippet-on-region)
;;          ("C-x c b"   . my/helm-do-grep-book-notes)
;;          ("C-x c SPC" . helm-all-mark-rings)))

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
  :load-path "site-lisp/macrostep"
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :load-path "site-lisp/magit"
  :bind (("C-x g" . magit-status))
  :init (progn
          (use-package git-commit-mode :load-path "site-lisp/git-modes" :defer t)
          (setq magit-last-seen-setup-instructions "1.4.0"))
  :config (progn
            (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
            (setq magit-use-overlays nil)))

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode "\\.markdown\\'"
  :commands markdown-mode
  :init (use-package markdown-mode+
          :load-path "site-lisp/markdown-mode-plus"))

(use-package notmuch
  :load-path "site-lisp/notmuch/emacs"
  :bind ("C-c m" . notmuch)
  :config (progn

            (setq mail-user-agent 'message-user-agent
                  message-send-mail-function 'message-send-mail-with-sendmail
                  ;; This is needed to allow msmtp to do its magic:
                  message-sendmail-f-is-evil t)

            (use-package sendmail
              :config (setq sendmail-program "/usr/local/bin/msmtp"))

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

            (setq notmuch-fcc-dirs nil)

            (setq notmuch-saved-searches
                  '((:name "anu unread"
                           :query "tag:anu AND tag:unread"
                           :sort-order oldest-first)
                    (:name "gmail unread"
                           :query "tag:gmail AND tag:unread"
                           :sort-order oldest-first)
                    (:name "unread"
                           :query "tag:unread"
                           :key "u"
                           :sort-order oldest-first)
                    (:name "inbox"
                           :query "tag:inbox"
                           :key "i"
                           :sort-order oldest-first)
                    (:name "flagged"
                           :query "tag:flagged"
                           :key "f"
                           :sort-order newest-first)
                    (:name "drafts"
                           :query "tag:draft"
                           :key "d"
                           :sort-order newest-first)
                    (:name "sent"
                           :query "tag:sent"
                           :key "s"
                           :sort-order newest-first)
                    (:name "all mail"
                           :query "*"
                           :key "a"
                           :sort-order newest-first)))

            (defun my-notmuch-search-mark-flagged ()
              "Flagg this email."
              (interactive)
              (notmuch-search-tag '("+flagged"))
              (notmuch-search-next-thread))

            (defun notmuch-search-mark-deleted ()
              "Mark this email as deleted."
              (interactive)
              (notmuch-search-tag '("-inbox" "-archive" "-unread" "+trash"))
              (notmuch-search-next-thread))

            (defun my-notmuch-show-mark-flagged ()
              "Flagg this email."
              (interactive)
              (notmuch-show-tag '("+flagged"))
              (notmuch-show-next-thread-show))

            (defun notmuch-show-mark-deleted ()
              "Mark this email as deleted."
              (interactive)
              (notmuch-show-tag '("-inbox" "-archive" "-unread" "+trash"))
              (notmuch-show-next-thread-show))

            (defun notmuch-show-bounce-message (&optional address)
              "Bounce the current message."
              (interactive "sBounce To: ")
              (notmuch-show-view-raw-message)
              (message-resend address))

            (define-key notmuch-hello-mode-map
              (kbd "g") 'notmuch-refresh-this-buffer)
            (define-key notmuch-search-mode-map
              (kbd "g") 'notmuch-refresh-this-buffer)
            (define-key notmuch-search-mode-map
              (kbd "C-c f") 'my-notmuch-search-mark-flagged)
            (define-key notmuch-search-mode-map
              (kbd "d") 'notmuch-search-mark-deleted)
            (define-key notmuch-show-mode-map
              (kbd "C-c f") 'my-notmuch-show-mark-flagged)
            (define-key notmuch-show-mode-map
              (kbd "d") 'notmuch-show-mark-deleted)
            (define-key notmuch-show-mode-map
              (kbd "RET") 'goto-address-at-point)
            (define-key notmuch-show-mode-map
              (kbd "TAB") 'notmuch-show-toggle-message)
            (define-key notmuch-show-mode-map
              (kbd "C-c n") 'notmuch-show-next-button)
            (define-key notmuch-show-mode-map
              (kbd "b") 'notmuch-show-bounce-message)

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

(use-package org-journal
  :load-path "site-lisp/org-journal"
  :config (progn
            (setq org-journal-dir "~/Dropbox/journal/")))

(use-package paredit
  :load-path "site-lisp/paredit"
  :commands paredit-mode
  :diminish paredit-mode)

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

(use-package puppet-mode
  :load-path "site-lisp/puppet-mode"
  :mode "\\.pp\\'"
  :init (use-package flymake-puppet
          :load-path "site-lisp/flymake-puppet"
          :init (add-hook 'puppet-mode-hook 'flymake-puppet-load)))

(use-package recentf
  :config (setq recentf-save-file
                (expand-file-name "recentf" user-data-directory)))

(use-package server
  :config (unless (server-running-p) (server-start)))

(use-package sx-load
  :load-path "site-lisp/sx"
  :commands sx-tab-all-questions
  :init (setq sx-cache-directory (expand-file-name "sx" user-data-directory)))

(use-package tramp
  :config (progn
            ;; Configure Tramp for use with the NCI cloud VMs.
            (setq tramp-default-method "ssh")
            (add-to-list 'tramp-default-proxies-alist
                         '("130\\.56\\."
                           nil
                           "/ssh:dap900@cloudlogin.nci.org.au:"))))

(use-package twittering-mode
  :load-path "site-lisp/twittering-mode"
  :commands twit)

(use-package url-cache
  :init (setq url-cache-directory (expand-file-name "url/cache" user-data-directory)))

(use-package yaml-mode
  :load-path "site-lisp/yaml-mode"
  :mode "\\.yaml\\'")

;; (setq-default eval-expression-print-level nil)
;; (setq-default case-fold-search nil)

;; (setq gc-cons-threshold (* 25 1024 1024))

;; (defmacro hook-into-modes (func modes)
;;   "Add a hook for function FUNC to the modes MODES."
;;   `(dolist (mode-hook ,modes)
;;      (add-hook mode-hook ,func)))


;; (require 'package)
;; (package-initialize)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-refresh-contents)

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (use-package smart-mode-line
;;   :ensure t
;;   :defer 3
;;   :config (progn
;;             (setq-default mode-line-format
;;                           '("%e"
;;                             mode-line-front-space
;;                             mode-line-mule-info
;;                             mode-line-client
;;                             mode-line-modified
;;                             mode-line-remote
;;                             mode-line-frame-identification
;;                             mode-line-buffer-identification
;;                             "   "
;;                             mode-line-position
;;                             (vc-mode vc-mode)
;;                             "  "
;;                             mode-line-modes
;;                             mode-line-misc-info
;;                             mode-line-end-spaces))
;;             (smart-mode-line-enable)))

;; (use-package powerline
;;   :ensure t
;;   :config (progn
;;             (powerline-default-theme)))

;; (use-package move-text
;;   :ensure t)

;; (use-package git-gutter-fringe
;;   :ensure t)

;; (use-package hydra
;;   :ensure t
;;   :config (progn
;;             (key-chord-define-global
;;              "hh"
;;              (defhydra hydra-error ()
;;                "goto-error"
;;                ("h" first-error "first")
;;                ("j" next-error "next")
;;                ("k" previous-error "prev")))


;;             (defhydra hydra-yank-pop ()
;;               "yank"
;;               ("C-y" yank nil)
;;               ("M-y" yank-pop nil)
;;               ("y" (yank-pop 1) "next")
;;               ("Y" (yank-pop -1) "prev")
;;               ("l" helm-show-kill-ring "list" :color blue))
;;             (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
;;             (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)


;;             (defhydra hydra-goto-line (goto-map ""
;;                                                 :pre (linum-mode 1)
;;                                                 :post (linum-mode -1))
;;               "goto-line"
;;               ("g" goto-line "go")
;;               ("m" set-mark-command "mark" :bind nil)
;;               ("q" nil "quit"))
;;             (global-set-key (kbd "M-g") #'hydra-goto-line/goto-line)

;;             (key-chord-define-global
;;              "uu"
;;              (defhydra hydra-move-text ()
;;                "Move text"
;;                ("u" move-text-up "up")
;;                ("d" move-text-down "down")))

;;             (key-chord-define-global
;;              "ww"
;;              (defhydra hydra-window (:color red :hint nil)
;;                "
;;  Split: _v_ert _x_:horz
;; Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
;;   Move: _s_wap
;; Frames: _f_rame new  _df_ delete
;;   Misc: _m_ark _a_ce  _u_ndo  _r_edo"
;;                ("h" windmove-left)
;;                ("j" windmove-down)
;;                ("k" windmove-up)
;;                ("l" windmove-right)
;;                ("H" hydra-move-splitter-left)
;;                ("J" hydra-move-splitter-down)
;;                ("K" hydra-move-splitter-up)
;;                ("L" hydra-move-splitter-right)
;;                ("|" (lambda ()
;;                       (interactive)
;;                       (split-window-right)
;;                       (windmove-right)))
;;                ("_" (lambda ()
;;                       (interactive)
;;                       (split-window-below)
;;                       (windmove-down)))
;;                ("v" split-window-right)
;;                ("x" split-window-below)
;;                ("t" transpose-frame "'")
;;                ("u" winner-undo)
;;                ("r" winner-redo) ;;Fixme, not working?
;;                ("o" delete-other-windows :exit t)
;;                ("a" ace-window :exit t)
;;                ("f" make-frame :exit t)
;;                ("s" ace-swap-window)
;;                ("da" ace-delete-window)
;;                ("dw" delete-window)
;;                ("db" kill-this-buffer)
;;                ("df" delete-frame :exit t)
;;                ("q" nil)
;;                ("i" ace-maximize-window "ace-one" :color blue)
;;                ;;("b" ido-switch-buffer "buf")
;;                ("m" headlong-bookmark-jump)))

;;             (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
;;                                                   :hint nil)
;;               "
;; Git gutter:
;;   _n_: next hunk        _s_tage hunk     _q_uit
;;   _p_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
;;   ^ ^                   _P_opup hunk
;;   _f_: first hunk
;;   _l_: last hunk        set start _R_evision
;; "
;;               ("n" git-gutter:next-hunk)

;;               ("p" git-gutter:previous-hunk)
;;               ("f" (progn (goto-char (point-min))
;;                           (git-gutter:next-hunk 1)))
;;               ("l" (progn (goto-char (point-min))
;;                           (git-gutter:previous-hunk 1)))
;;               ("s" git-gutter:stage-hunk)
;;               ("r" git-gutter:revert-hunk)
;;               ("P" git-gutter:popup-hunk)
;;               ("R" git-gutter:set-start-revision)
;;               ("q" nil :color blue)
;;               ("Q" (progn (git-gutter-mode -1)
;;                           ;; git-gutter-fringe doesn't seem to
;;                           ;; clear the markup right away
;;                           (sit-for 0.1)
;;                           (git-gutter:clear))
;;                :color blue))
;;             (global-set-key (kbd "C-c g")
;;                             'hydra-git-gutter/body)))

;; (setq-default case-fold-search t)

;; (defun dap/current-file ()
;;   "Gets the \"file\" of the current buffer.

;; The file is the buffer's file name, or the `default-directory' in
;; `dired-mode'."
;;   (if (eq major-mode 'dired-mode)
;;       default-directory
;;     (buffer-file-name)))

;; (defun dap/copy-filename-as-kill (&optional arg)
;;   "Copy the name of the currently visited file to kill ring.

;; With a zero prefix arg, copy the absolute file name.  With
;; \\[universal-argument], copy the file name relative to the
;; current buffer's `default-directory'.  Otherwise copy the
;; non-directory part only."
;;   (interactive "P")
;;   (-if-let* ((filename (dap/current-file))
;;              (name-to-copy (cond ((zerop (prefix-numeric-value arg)) filename)
;;                                  ((consp arg) (file-relative-name filename))
;;                                  (:else (file-name-nondirectory filename)))))
;;       (progn
;;         (kill-new name-to-copy)
;;         (message "%s" name-to-copy))
;;     (user-error "This buffer is not visiting a file")))

;; (defun dap/rename-file-and-buffer ()
;;   "Rename the current file and buffer."
;;   (interactive)
;;   (let* ((filename (buffer-file-name))
;;          (old-name (if filename
;;                        (file-name-nondirectory filename)
;;                      (buffer-name)))
;;          (new-name (read-file-name "New name: " nil nil nil old-name)))
;;     (cond
;;      ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
;;      ((vc-backend filename) (vc-rename-file filename new-name))
;;      (:else
;;       (rename-file filename new-name :force-overwrite)
;;       (set-visited-file-name new-name :no-query :along-with-file)))))

;; (defun dap/delete-file-and-buffer ()
;;   "Delete the current file and kill the buffer."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (cond
;;      ((not filename) (kill-buffer))
;;      ((vc-backend filename) (vc-delete-file filename))
;;      (:else
;;       (delete-file filename)
;;       (kill-buffer)))))

;; (defun dap/find-user-init-file-other-window ()
;;   "Edit the `user-init-file', in another window."
;;   (interactive)
;;   (find-file-other-window user-init-file))

;; (defvar dap/files-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "R") 'dap/rename-file-and-buffer)
;;     (define-key map (kbd "D") 'dap/delete-file-and-buffer)
;;     (define-key map (kbd "w") 'dap/copy-filename-as-kill)
;;     (define-key map (kbd "i") 'dap/find-user-init-file-other-window)
;;     map)
;;   "Keymap for file operations.")

;; (defvar dap/toggle-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "c") 'toggle-case-fold-search)
;;     (define-key map (kbd "d") 'toggle-debug-on-error)
;;     (define-key map (kbd "l") 'linum-mode)
;;     (define-key map (kbd "r") 'dired-toggle-read-only)
;;     (define-key map (kbd "t") 'toggle-truncate-lines)
;;     map)
;;   "Keymap for toggle operations.")

;; (global-set-key (kbd "C-c t") dap/toggle-map)
;; (global-set-key (kbd "C-c f") dap/files-map)


;; (use-package alert
;;   :ensure t
;;   :config (progn
;;             (setq alert-fade-time 10)
;;             (setq alert-default-style 'growl)
;;             (setq alert-reveal-idle-time 120)))

;; (set-face-attribute 'default nil :font "DejaVu LGC Sans Mono" :height 130)

;; (setq make-pointer-invisible 1)

;; (use-package key-chord
;;   :ensure t
;;   :config (progn
;;             (key-chord-mode 1)
;;             (key-chord-define-global (concat "<" "_")
;;                                      (lambda () (interactive) (insert "←")))
;;             (key-chord-define-global (concat "_" ">")
;;                                      (lambda () (interactive) (insert "→")))))

;; (setq-default fill-column 80)

;; (defconst dap/lispy-modes '(emacs-lisp-mode-hook
;;                             ielm-mode-hook
;;                             lisp-interaction-mode-hook
;;                             scheme-mode-hook))

;; (hook-into-modes 'eldoc-mode dap/lispy-modes)

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config (progn
;;             (hook-into-modes 'fci-mode '(prog-mode-hook))
;;             (hook-into-modes 'fci-mode dap/lispy-modes)))

;; (blink-cursor-mode 0)
;; (setq-default cursor-type 'box)
;; (setq x-stretch-cursor 1)

;; (global-font-lock-mode 1)

;; (setq blink-matching-paren nil)
;; (show-paren-mode 1)

;; ;; Pref splitting windows horizontally when reasonable, otherwise split
;; ;; vertically.
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 80)

;; (require 'paren)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)

;; (setq ring-bell-function 'ignore)
;; (setq visible-bell 1)

;; (use-package ace-window
;;   :ensure t
;;   :init (progn
;;           (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
;;           ;(key-chord-define-global "ww" 'ace-window)
;;           (ace-window-display-mode)))

;; (defadvice yes-or-no-p (around prevent-dialog activate)
;;   "Prevent `yes-or-no-p' from activating a dialog."
;;   (let ((use-dialog-box nil))
;;     ad-do-it))
;; (defadvice y-or-n-p (around prevent-dialog-yorn activate)
;;   "Prevent `y-or-n-p' from activating a dialog."
;;   (let ((use-dialog-box nil))
;;     ad-do-it))


;; (setq browse-url-browser-function 'browse-url-generic)

;; (use-package osx-browse
;;   :ensure t
;;   :init (osx-browse-mode 1))

;; (use-package solarized-theme
;;   :ensure t
;;   :config (progn
;;             (setq solarized-distinct-fringe-background 1)
;;             (setq solarized-high-contrast-mode-line 1)
;;             (setq solarized-use-less-bold 1)
;;             (setq solarized-use-more-italic nil)
;;             (setq solarized-emphasize-indicators nil)
;;             (load-theme 'solarized-dark t)))

;; (use-package monokai-theme
;;   :ensure t
;;   :disabled
;;   :config (progn
;;             (load-theme 'monokai t)))

;; (use-package pretty-mode
;;   :ensure t
;;   :init (hook-into-modes 'turn-on-pretty-mode dap/lispy-modes))

;; (setq make-pointer-invisible 1)

;; (require 'desktop)
;; (setq desktop-restore-eager 10)
;; (setq desktop-dirname dap/user-data-directory)
;; (desktop-save-mode 1)

;; (use-package uniquify
;;   :config (setq uniquify-buffer-name-style 'forward))

;; (setq backup-inhibited 1)

;; (defun dap/cleanup-buffer-safe ()
;;   "Perform a bunch of safe operations on the whitespace content of a buffer.
;; Does not indent buffer, because it is used for a `before-save-hook', and that
;; might be bad."
;;   (interactive)
;;   (dap/untabify-buffer)
;;   (delete-trailing-whitespace)
;;   (set-buffer-file-coding-system 'utf-8))

;; (add-hook 'before-save-hook 'dap/cleanup-buffer-safe)

;; (prefer-coding-system 'utf-8)

;; (use-package undo-tree
;;   :ensure t
;;   :diminish undo-tree-mode
;;   :init (global-undo-tree-mode 1))

;; (setq require-final-newline t)

;; (global-auto-revert-mode 1)

;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind ("C-0" . ace-jump-mode)
;;   :init (progn
;;           (key-chord-define-global "jj" 'ace-jump-mode)
;;           (bind-key"C-x SPC" 'ace-jump-mode-pop-mark)))

;; (let ((text-buffer (get-buffer-create "*text*")))
;;   (with-current-buffer text-buffer
;;     (text-mode)
;;     (insert "Scratch text:\n\n")
;;     (beginning-of-line)))

;; (setq isearch-lax-whitespace 1)
;; (setq isearch-regexp-lax-whitespace 1)

;; (use-package boxquote
;;   :ensure t)

;; (setq track-eol 1)
;; (setq line-move-visual nil)

;; (size-indication-mode)
;; (column-number-mode 1)

;; (defadvice kill-line (around kill-line-remove-newline activate)
;;   "Make `kill-line' kill the whole line."
;;   (let ((kill-whole-line t))
;;     ad-do-it))

;; (delete-selection-mode 1)

;; (fset 'yes-or-no-p 'y-or-n-p)

;; (setq resize-mini-windows 1)
;; (setq max-mini-window-height 0.33)

;; (setq enable-recursive-minibuffers t)
;; (minibuffer-depth-indicate-mode 1)

;; (defadvice global-set-key (before check-keymapping activate)
;;   "Check for existing binding when setting a global keybinding."
;;   (let* ((key (ad-get-arg 0))
;;          (new-command (ad-get-arg 1))
;;          (old-command (lookup-key global-map key)))
;;     (when
;;         (and
;;          old-command
;;          (not (equal old-command new-command))
;;          (not (equal old-command 'digit-argument))
;;          (not (equal old-command 'negative-argument))
;;          (not (equal old-command 'ns-print-buffer))
;;          (not (equal old-command 'move-beginning-of-line))
;;          (not (equal old-command 'execute-extended-command))
;;          (not (equal new-command 'execute-extended-command))
;;          (not (equal old-command 'ns-prev-frame))
;;          (not (equal old-command 'ns-next-frame))
;;          (not (equal old-command 'mwheel-scroll))
;;          (not (equal new-command 'diff-hl-mode))
;;          )
;;       (warn "Just stomped the global-map binding for %S, replaced %S with %S"
;;             key old-command new-command))))


;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; (setq echo-keystrokes 0.02)

;; (defun dap/beginning-of-line-dwim ()
;;   "Move to beginning of line intelligently.
;; Toggle between moving point to the first non-whitespace
;; character, and the start of the line."
;;   (interactive)
;;   (let ((start-position (point)))
;;     ;; see if going to the beginning of the line changes our position
;;     (move-beginning-of-line nil)

;;     (when (= (point) start-position)
;;       ;; we're already at the beginning of the line, so go to the
;;       ;; first non-whitespace character
;;       (back-to-indentation))))

;; (bind-key "C-a" 'dap/beginning-of-line-dwim)

;; (use-package expand-region
;;   :ensure t
;;   :config (bind-key "C-'" 'er/expand-region))

;; (use-package multiple-cursors
;;   :ensure t
;;   :config (progn
;;             (setq mc/list-file (concat dap/user-data-directory ".mc-lists.el"))
;;             (bind-key "M-9" 'mc/edit-lines)
;;             (bind-key "M-0" 'mc/mark-next-like-this)
;;             (bind-key "M--" 'mc/mark-all-like-this)
;;             (bind-key "M-8" 'mc/mark-previous-like-this)))

;; (use-package yasnippet
;;   :ensure t
;;   :mode     ("/\\.emacs\\.d/snippets/" . snippet-mode)
;;   :commands (yas-minor-mode yas-expand)
;;   :diminish yas-minor-mode
;;   :init (yas-global-mode 1)
;;   :config (progn
;;             (setq yas-snippet-dirs     (concat (expand-file-name user-emacs-directory)
;;                                                "snippets")
;;                   yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;;             ;; Use only my own snippets, not the bundled ones.
;;             (yas-load-directory yas-snippet-dirs)
;;             (bind-key "C-4" 'yas/expand)))

;; (bind-key "C-5"        'comment-dwim)
;; (bind-key "s-<return>" 'dap/smart-open-line)
;; (bind-key "C-7"        'dap/insert-timestamp)
;; (bind-key "M-7"        'dap/insert-datestamp)

;; (bind-key "C-<f2>" 'emacs-index-search)
;; (bind-key "S-<f2>" 'elisp-index-search)

;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind ("C-<f3>". imenu-anywhere))

;; (bind-key "s-<up>"   'enlarge-window)
;; (bind-key "s-<down>" 'shrink-window)
;; (bind-key "s-<right>"'enlarge-window-horizontally)
;; (bind-key "s-<left>" 'shrink-window-horizontally)

;; (setq-default ispell-program-name "aspell")

;; (use-package whitespace
;;   :ensure t
;;   :diminish (global-whitespace-mode
;;              whitespace-mode)
;;   :config (progn
;;             (setq whitespace-line-column 80)
;;             (setq whitespace-style '(trailing lines tab-mark))
;;             (global-whitespace-mode 1)))

;; (use-package rainbow-mode
;;   :ensure t
;;   :diminish rainbow-mode
;;   :config (progn
;;             (hook-into-modes 'rainbow-mode dap/lispy-modes)
;;             (rainbow-mode 1)))

;; (use-package ido
;;   :ensure t
;;   :disabled t
;;   :config (progn
;;             (ido-mode 1)
;;             (ido-everywhere 1)

;;             ;; Disable ido faces to see flx highlights.
;;             (setq ido-enable-flex-matching t)
;;             (setq ido-use-faces            nil)
;;             (setq ido-create-new-buffer    'always)

;;             (setq ido-save-directory-list-file
;;                   (concat dap/user-data-directory "ido.last"))

;;             (unless (package-installed-p 'ido-vertical-mode)
;;               (package-install 'ido-vertical-mode))
;;             (use-package ido-vertical-mode
;;               :config (ido-vertical-mode 1))

;;             (unless (package-installed-p 'ido-hacks)
;;               (package-install 'ido-hacks))
;;             (use-package ido-hacks)

;;             (unless (package-installed-p 'flx-ido)
;;               (package-install 'flx-ido))
;;             (use-package flx-ido
;;               :config (flx-ido-mode 1))

;;             (unless (package-installed-p 'ido-ubiquitous)
;;               (package-install 'ido-ubiquitous))
;;             (use-package ido-ubiquitous
;;               :config (ido-ubiquitous-mode 1))))

;; (use-package projectile
;;   :ensure t
;;   :config (progn
;;             (setq projectile-cache-file
;;                   (concat dap/user-data-directory "projectile.cache"))
;;             (projectile-global-mode 1)))

;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :config (progn
;;             (hook-into-modes 'turn-on-smartparens-strict-mode dap/lispy-modes)
;;             (hook-into-modes 'turn-on-smartparens-mode '(puppet-mode-hook))
;;             (setq sp-show-pair-from-inside nil)))

;; (require 'dired)
;; (setq dired-listing-switches  "-alh")
;; (setq dired-recursive-deletes 1)

;; (use-package dired-details+
;;   :ensure t
;;   :config (setq-default dired-details-hidden-string ""))

;; (use-package diff-hl
;;   :ensure t)

;; (defun dap/dired-mode-hook ()
;;   "Personal dired customizations."
;;   (diff-hl-dired-mode 1)
;;   (use-package dired-x))

;; (add-hook 'dired-mode-hook 'dap/dired-mode-hook)

;; (use-package find-dired
;;   :config (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

;; (use-package wdired
;;   :config (progn
;;             (setq wdired-allow-to-change-permissions t)
;;             (setq wdired-allow-to-redirect-links     t)
;;             (setq wdired-use-interactive-rename      t)
;;             (setq wdired-confirm-overwrite           t)
;;             (setq wdired-use-dired-vertical-movement 'sometimes)))

;; (require 'savehist)
;; (let ((savehist-file-store (concat dap/user-data-directory "savehist")))
;;   (when (not (file-exists-p savehist-file-store))
;;     (warn
;;      (concat "Can't seem to find a savehist store file where it was expected "
;;              "at: " savehist-file-store " . Savehist should continue "
;;              "to function normally; but your history may be lost.")))
;;   (setq savehist-file savehist-file-store))

;; (savehist-mode 1)
;; (setq savehist-save-minibuffer-history 1)
;; (setq savehist-additional-variables '(kill-ring
;;                                       search-ring
;;                                       regexp-search-ring))

;; (defun dap/display-code-line-counts (ov)
;;   "Displaying overlay (as OV) content in echo area or tooltip."
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (overlay-put ov 'help-echo
;;                  (buffer-substring (overlay-start ov)
;;                                    (overlay-end ov)))))

;; (defun dap/untabify-buffer ()
;;   "Remove tabs from the whole buffer."
;;   (interactive)
;;   (untabify (point-min) (point-max)))

;; (defun dap/untabify-buffer-hook ()
;;   "Add a buffer-local untabify on save hook."
;;   (interactive)
;;   (add-hook 'after-save-hook
;;             (lambda () (dap/untabify-buffer))
;;             nil
;;             'true))

;; (use-package org
;;   :init (progn
;;           (fci-mode 1)
;;           (dap/untabify-buffer-hook)
;;           (local-set-key (kbd "C-1") 'org-narrow-to-subtree)
;;           (local-set-key (kbd "M-1") 'widen)
;;           (local-set-key (kbd "C-2") 'org-edit-special)

;;           (setq org-completion-use-ido 1)
;;           (setq org-use-speed-commands 1)
;;           (setq org-confirm-shell-link-function 'y-or-n-p)
;;           (setq org-confirm-elisp-link-function 'y-or-n-p)
;;           (setq org-pretty-entities 1)
;;           (setq org-ellipsis "…")
;;           (setq org-hide-leading-stars 1)
;;           (setq org-src-fontify-natively nil)
;;           (setq org-fontify-emphasized-text 1)
;;           (setq org-src-preserve-indentation 1)
;;           (setq org-edit-src-content-indentation 0)
;;           (setq org-highlight-latex-and-related '(latex script entities))

;;           (setq org-footnote-define-inline 1)
;;           (setq org-footnote-auto-label 'random)
;;           (setq org-footnote-auto-adjust nil)
;;           (setq org-footnote-section nil)

;;           (setq org-catch-invisible-edits 'error)

;;           (unless (package-installed-p 'hideshow-org)
;;             (package-install 'hideshow-org))
;;           (use-package hideshow-org)

;;           (unless (package-installed-p 'org-ac)
;;             (package-install 'org-ac))
;;           (use-package org-ac
;;             :init (org-ac/config-default))))

;; (use-package hideshow
;;   :init (progn
;;           (setq hs-hide-comments-when-hiding-all 1)
;;           (setq hs-isearch-open 1)
;;           (setq hs-set-up-overlay 'dap/display-code-line-counts)

;;           (defadvice goto-line (after expand-after-goto-line activate compile)
;;             "How do I get it to expand upon a goto-line? hideshow-expand affected block when using goto-line in a collapsed buffer."
;;             (save-excursion
;;               (hs-show-block)))))

;; (use-package flyspell
;;   :ensure t
;;   :init (progn
;;           (hook-into-modes 'turn-on-flyspell '(text-mode-hook
;;                                                org-mode-hook))
;;           (hook-into-modes 'flyspell-prog-mode '(prog-mode-hook))))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode 1))

;; (use-package fancy-narrow
;;   :ensure t)

;; (defun dap/disable-tabs ()
;;   "Disable tabs."
;;   (setq indent-tabs-mode nil))

;; (defun dap/newline ()
;;   "Locally binds newline."
;;   (local-set-key (kbd "RET") 'sp-newline))

;; (hook-into-modes 'dap/newline              dap/lispy-modes)
;; (hook-into-modes 'dap/untabify-buffer-hook dap/lispy-modes)
;; (hook-into-modes 'dap/disable-tabs         dap/lispy-modes)
;; (hook-into-modes (lambda ()
;;                    (add-hook 'local-write-file-hooks 'check-parens))
;;                  dap/lispy-modes)

;; (defun dap/elisp-mode-local-bindings ()
;;   "Helpful behavior for Elisp buffers."
;;   (local-set-key (kbd "s-l eb") 'eval-buffer)
;;   (local-set-key (kbd "s-l ep") 'eval-print-last-sexp)
;;   (local-set-key (kbd "s-l td") 'toggle-debug-on-error)
;;   (local-set-key (kbd "s-l mef") 'macroexpand)
;;   (local-set-key (kbd "s-l mea") 'macroexpand-all))

;; (use-package lexbind-mode
;;   :ensure t
;;   :init (hook-into-modes 'lexbind-mode '(elisp-mode-hook)))

;; (defun dap/elisp-mode-hook ()
;;   "My elisp-mode hook."
;;   (dap/elisp-mode-local-bindings)
;;   (turn-on-eldoc-mode))

;; (hook-into-modes 'emacs-lisp-mode-hook '(dap/elisp-mode-hook))

;; (setq initial-scratch-message nil)

;; (use-package js2-mode
;;   :ensure t
;;   :config (progn
;;             (local-set-key (kbd "RET") 'newline-and-indent)
;;             (fci-mode 1)
;;             (visual-line-mode)
;;             (dap/untabify-buffer-hook)))

;; (use-package web-mode
;;   :ensure t
;;   :init (progn
;;           (setq web-mode-enable-block-partial-invalidation t)
;;           (setq web-mode-engines-alist '(("ctemplate" . "\\.html$"))))
;;   :config (progn
;;             (whitespace-turn-off)
;;             (rainbow-turn-off)
;;             (visual-line-mode)
;;             (local-set-key (kbd "RET") 'newline-and-indent)
;;             (setq web-mode-markup-indent-offset 2)
;;             (setq web-mode-css-indent-offset 2)
;;             (setq web-mode-code-indent-offset 2)
;;             (setq web-mode-indent-style 2)
;;             (setq web-mode-style-padding 1)
;;             (setq web-mode-script-padding 1)
;;             (setq web-mode-block-padding 0)
;;             (dap/untabify-buffer-hook)))

;; (use-package json-reformat
;;   :ensure t)

;; (use-package css-mode
;;   :ensure t
;;   :config (progn
;;             (fci-mode 1)
;;             (whitespace-turn-on)
;;             (rainbow-mode)
;;             (visual-line-mode)
;;             (dap/untabify-buffer-hook)
;;             (local-set-key (kbd "RET") 'newline-and-indent)))

;; (add-hook 'makefile-mode-hook
;;           (lambda ()
;;             (fci-mode 1)
;;             (whitespace-turn-on)
;;             (rainbow-mode)
;;             (visual-line-mode)
;;             (local-set-key (kbd "RET") 'newline-and-indent)))

;; (use-package diff-hl
;;   :disabled
;;   :ensure t
;;   :init (global-diff-hl-mode))

;; (hook-into-modes 'turn-on-visual-line-mode '(org-mode-hook))
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; (use-package ace-link
;;   :ensure t
;;   :init (ace-link-setup-default))

;; (use-package ruby-mode
;;   :ensure t
;;   :config (progn
;;             (fci-mode 1)
;;             (rainbow-mode)
;;             (dap/untabify-buffer-hook)
;;             (visual-line-mode)
;;             (local-set-key (kbd "RET") 'newline-and-indent)))

;; (use-package erc
;;   :ensure t
;;   :init (progn
;;           (whitespace-turn-off)

;;           (use-package erc-join
;;             :init (progn
;;                     (erc-autojoin-mode 1)
;;                     (setq erc-autojoin-channels-alist
;;                           '((".*\\.freenode\\.net" .
;;                              '("#emacs" "#idris" "#haskell-beginners"))))))

;;           (use-package erc-button
;;             :init (progn
;;                     (erc-button-mode 1)
;;                     (setq erc-button-wrap-long-urls  nil)
;;                     (setq erc-button-buttonize-nicks nil)))

;;           (use-package erc-fill
;;             :init (progn
;;                     (erc-fill-mode 1)
;;                     (setq erc-fill-column        80)
;;                     (setq erc-fill-function      'erc-fill-static)
;;                     (setq erc-fill-static-center 0)))

;;           (use-package erc-netsplit
;;             :init (erc-netsplit-mode 1))

;;           (use-package erc-ring
;;             :init (erc-ring-mode 1))

;;           (use-package erc-track
;;             :init (progn
;;                     (setq erc-track-switch-direction 'importance)
;;                     (setq erc-track-position-in-mode-line 1)
;;                     (setq erc-track-exclude-types
;;                           '("324" "329" "332" "333" "353"
;;                             "JOIN" "NAMES" "NICK" "QUIT" "PART" "TOPIC"))
;;                     (add-to-list 'erc-modules 'track)))

;;           (add-to-list 'erc-modules 'notify)
;;           (add-to-list 'erc-modules 'scrolltobottom)
;;           (erc-update-modules)))

;; (defun dap/smart-open-line ()
;;   "Insert a new line, indent it, and move the cursor there.

;; This behavior is different then the typical function bound to
;; return which may be `open-line' or `newline-and-indent'.  When
;; you call with the cursor between ^ and $, the contents of the
;; line to the right of it will be moved to the newly inserted line.
;; This function will not do that.  The current line is left alone,
;; a new line is inserted, indented, and the cursor is moved there.

;; Attribution: URL http://emacsredux.com/blog/2013/03/26/smarter-open-line/"
;;   (interactive)
;;   (move-end-of-line nil)
;;   (newline-and-indent))

;; (defun dap/insert-timestamp ()
;;   "Insert a full ISO 8601 format timestamp."
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%dT%T%z")))

;; (defun dap/insert-datestamp ()
;;   "Insert a partial ISO 8601 format timestamp."
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d")))

;; (defun dap/text-mode-hook ()
;;   "My `text-mode' hook."
;;   (rainbow-mode)
;;   (fci-mode)
;;   (visual-line-mode)
;;   (dap/untabify-buffer-hook))

;; (add-hook 'text-mode-hook 'dap/text-mode-hook)

;; (defun dap/indent-buffer ()
;;   "Indent the whole buffer."
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; (defun dap/cleanup-buffer ()
;;   "Perform a bunch of operations on the whitespace content of a buffer.
;; Including indent-buffer, which should not be called automatically on save."
;;   (interactive)
;;   (dap/cleanup-buffer-safe)
;;   (dap/indent-buffer))

;; (use-package ag
;;   :ensure t)

;; ;; ;; ;;;_ , AUCTeX

;; ;; ;; (use-package tex-site
;; ;; ;;   :load-path "site-lisp/auctex/preview/"

;; ;; ;;   :init (progn
;; ;; ;;           (hook-into-modes 'TeX-source-correlate-mode '(LaTeX-mode-hook))
;; ;; ;;           (hook-into-modes 'TeX-PDF-mode '(LaTeX-mode-hook))
;; ;; ;;           (hook-into-modes (lambda ()
;; ;; ;;                              (add-to-list 'TeX-expand-list
;; ;; ;;                                           '("%q" make-skim-url)))
;; ;; ;;                            '(LaTeX-mode-hook))


;; (use-package dash-at-point
;;   :ensure t)

;; (use-package flymake-cursor
;;   :ensure t)

;; (use-package keyfreq
;;   :ensure t
;;   :init (keyfreq-mode 1))

;; (use-package ledger-mode
;;   :ensure t
;;   :config (progn
;;             (unless (package-installed-p 'flycheck-ledger)
;;               (package-install 'flycheck-ledger))
;;             (use-package flycheck-ledger)))

;; (unless (package-installed-p 'guide-key)
;;   (package-install 'guide-key))
;; (use-package guide-key
;;   :init (progn
;;           (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"
;;                                                (org-mode "C-c C-x")
;;                                                (dired-mode "%")))
;;           (guide-key-mode 1)))


;; ;; Mail.


;; (use-package ansi-color
;;   :init (progn
;;           (defun colorize-compilation-buffer ()
;;             (read-only-mode -1)
;;             (ansi-color-apply-on-region (point-min) (point-max))
;;             (read-only-mode 1))
;;           (hook-into-modes 'colorize-compilation-buffer
;;                            '(compilation-filter-hook))))

;; (use-package haskell-mode
;;   :ensure t
;;   :init (progn
;;           (unless (package-installed-p 'flycheck-haskell)
;;             (package-install 'flycheck-haskell))
;;           (use-package flycheck-haskell
;;             :init (progn
;;                     (hook-into-modes 'flycheck-haskell-setup
;;                                      '(flycheck-mode-hook))))

;;           (turn-on-haskell-doc-mode)
;;           (turn-on-haskell-indentation)
;;           (turn-on-haskell-decl-scan)

;;           (define-key haskell-mode-map (kbd "C-x C-d") nil)
;;           (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;           (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;;           (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
;;           (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;           (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;           (define-key haskell-mode-map (kbd "C-c M-.") nil)
;;           (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; (use-package php-mode
;;   :ensure t
;;   :init (progn
;;           (unless (package-installed-p 'flymake-php)
;;             (package-install 'flymake-php))
;;           (use-package flymake-php)))

;; (use-package hungry-delete
;;   :ensure t
;;   :init (progn
;;           (global-hungry-delete-mode 1)))

;; (require 'ediff)
;; ;; Use ediff in single-frame mode.
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; (use-package helm-swoop
;;   :ensure t
;;   :defer t
;;   :bind (("C-S-s"   . helm-swoop)
;;          ("M-i"     . helm-swoop)
;;          ("M-s s"   . helm-swoop)
;;          ("M-s M-s" . helm-swoop)
;;          ("M-I"     . helm-swoop-back-to-last-point)
;;          ("C-c M-i" . helm-multi-swoop)
;;          ("C-x M-i" . helm-multi-swoop-all))
;;   :config (progn
;;             (define-key isearch-mode-map (kbd "M-i")
;;               'helm-swoop-from-isearch)
;;             (define-key helm-swoop-map (kbd "M-i")
;;               'helm-multi-swoop-all-from-helm-swoop)))

;; (use-package helm-descbinds
;;   :defer t
;;   :bind (("C-h b" . helm-descbinds)
;;          ("C-h w" . helm-descbinds)))

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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
;;  '(magit-use-overlays nil)
;;  '(package-selected-packages
;;    (quote
;;     (git-gutter-fringe notmuch-address yasnippet yaml-mode web-mode use-package undo-tree twittering-mode sx solarized-theme smex smartparens smart-mode-line rainbow-mode puppet-mode projectile pretty-mode powerline php-mode osx-browse org-journal org-ac notmuch-labeler multiple-cursors move-text monokai-theme markdown-mode+ magit lexbind-mode ledger-mode keyfreq key-chord json-reformat js2-mode imenu-anywhere ido-vertical-mode ido-ubiquitous ido-hacks hydra hungry-delete hideshow-org helm-swoop guide-key flymake-puppet flymake-php flymake-cursor flycheck-ledger flycheck-haskell flx-ido fill-column-indicator fancy-narrow expand-region exec-path-from-shell dired-details+ diff-hl dash-at-point company chruby boxquote alert ag ace-window ace-link ace-jump-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; (provide 'init)
;; ;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
