;; Phunculist's Emacs configuration.

(setq-default user-full-name    "David Porter"
              user-mail-address "david.a.porter@gmail.com")

(setq-default eval-expression-print-level nil)
(setq-default case-fold-search nil)

(setq gc-cons-threshold (* 25 1024 1024))

(setq user-data-directory
      (concat (expand-file-name user-emacs-directory) "data/"))
(setq recentf-save-file (concat user-data-directory "recentf"))
(setq url-cache-directory (concat user-data-directory "url/cache"))

(setq backup-directory-alist
      (list (cons "." (concat user-emacs-directory "backups"))))

(setq auto-save-list-file-prefix
      (concat user-emacs-directory "backups/auto-save-list/.saves-"))

;; Keep all auto-save files in the temp directory.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Use GNU Coreutils version of `ls', which is called `gls' when installed via
;; Homebrew.
(setq insert-directory-program "gls")

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(unless (package-installed-p 'alert)
  (package-install 'alert))
(use-package alert
  :config (progn
            (setq alert-fade-time 10)
            (setq alert-default-style 'growl)
            (setq alert-reveal-idle-time 120)))

(set-face-attribute 'default nil :font "DejaVu LGC Sans Mono" :height 130)

(setq make-pointer-invisible 1)

(unless (package-installed-p 'key-chord)
  (package-install 'key-chord))
(use-package key-chord
  :config (progn
            (key-chord-mode 1)
            (key-chord-define-global (concat "<" "_")
                                     (lambda () (interactive) (insert "←")))
            (key-chord-define-global (concat "_" ">")
                                     (lambda () (interactive) (insert "→")))))

(setq-default fill-column 80)

(unless (package-installed-p 'fill-column-indicator)
  (package-install 'fill-column-indicator))
(use-package fill-column-indicator
  :config (progn
            (hook-into-modes 'fci-mode '(prog-mode-hook))
            (hook-into-modes 'fci-mode dap/lispy-modes)))

(blink-cursor-mode 0)
(setq-default cursor-type 'box)
(setq x-stretch-cursor 1)

(global-font-lock-mode 1)

(setq blink-matching-paren nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(setq ring-bell-function 'ignore)
(setq visible-bell 1)

(unless (package-installed-p 'winner)
  (package-install 'winner))
(use-package winner
  :if (not noninteractive)
  :diminish winner-mode
  :init (winner-mode 1))

(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))
(use-package ace-window
  :init (progn
          (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
          (key-chord-define-global "bm" 'ace-window)))

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq browse-url-browser-function 'browse-url-generic)

(unless (package-installed-p 'osx-browse)
  (package-install 'osx-browse))
(use-package osx-browse
  :init (osx-browse-mode 1))

(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(use-package solarized-theme
  :config (progn
            (setq solarized-distinct-fringe-background 1)
            (setq solarized-high-contrast-mode-line 1)
            (setq solarized-use-less-bold 1)
            (setq solarized-use-more-italic nil)
            (setq solarized-emphasize-indicators nil)
            (load-theme 'solarized-dark t)))

(unless (package-installed-p 'pretty-mode)
  (package-install 'pretty-mode))
(use-package pretty-mode
  :init (hook-into-modes 'turn-on-pretty-mode dap/lispy-modes))

(setq make-pointer-invisible 1)

(desktop-save-mode 1)
(setq desktop-restore-eager 10)
(setq desktop-dirname user-data-directory)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(setq backup-inhibited 1)

(defun dap/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (dap/untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'dap/cleanup-buffer-safe)

(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

(setq require-final-newline t)

(global-auto-revert-mode 1)

(unless (package-installed-p 'ace-jump-mode)
  (package-install 'ace-jump-mode))
(use-package ace-jump-mode
  :bind ("C-0" . ace-jump-mode)
  :init (progn
          (key-chord-define-global "nh" 'ace-jump-mode)
          (ace-jump-mode-enable-mark-sync)
          (bind-key"C-x SPC" 'ace-jump-mode-pop-mark)))

(let ((text-buffer (get-buffer-create "*text*")))
  (with-current-buffer text-buffer
    (text-mode)
    (insert "Scratch text:\n\n")
    (beginning-of-line)))

(setq isearch-lax-whitespace 1)
(setq isearch-regexp-lax-whitespace 1)

(unless (package-installed-p 'boxquote)
  (package-install 'boxquote))
(use-package boxquote)

(setq track-eol 1)
(setq line-move-visual nil)

(size-indication-mode)
(column-number-mode 1)

(defadvice kill-line (around kill-line-remove-newline activate)
  (let ((kill-whole-line t))
    ad-do-it))

(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq resize-mini-windows 1)
(setq max-mini-window-height 0.33)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(defadvice global-set-key (before check-keymapping activate)
  (let* ((key (ad-get-arg 0))
         (new-command (ad-get-arg 1))
         (old-command (lookup-key global-map key)))
    (when
        (and
         old-command
         (not (equal old-command new-command))
         (not (equal old-command 'digit-argument))
         (not (equal old-command 'negative-argument))
         (not (equal old-command 'ns-print-buffer))
         (not (equal old-command 'move-beginning-of-line))
         (not (equal old-command 'execute-extended-command))
         (not (equal new-command 'execute-extended-command))
         (not (equal old-command 'ns-prev-frame))
         (not (equal old-command 'ns-next-frame))
         (not (equal old-command 'mwheel-scroll))
         (not (equal new-command 'diff-hl-mode))
         )
      (warn "Just stomped the global-map binding for %S, replaced %S with %S"
            key old-command new-command))))

(setq mac-control-modifier 'control)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq echo-keystrokes 0.02)

(defun dap/beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace
    character, and the start of the line. Src:
    http://www.wilfred.me.uk/"
  (interactive)
  (let ((start-position (point)))
    ;; see if going to the beginning of the line changes our position
    (move-beginning-of-line nil)

    (when (= (point) start-position)
      ;; we're already at the beginning of the line, so go to the
      ;; first non-whitespace character
      (back-to-indentation))))

(bind-key "C-a" 'dap/beginning-of-line-dwim)

(unless (package-installed-p 'expand-region)
  (package-install 'expand-region))
(use-package expand-region
  :config (bind-key "C-'" 'er/expand-region))

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(use-package multiple-cursors
  :config (progn
            (setq mc/list-file (concat user-data-directory ".mc-lists.el"))
            (bind-key "M-9" 'mc/edit-lines)
            (bind-key "M-0" 'mc/mark-next-like-this)
            (bind-key "M--" 'mc/mark-all-like-this)
            (bind-key "M-8" 'mc/mark-previous-like-this)))

(unless (package-installed-p 'smex)
  (package-install 'smex))
(use-package smex
  :config (progn
            (smex-initialize)
            (global-set-key (kbd "M-x") 'smex)
            (global-set-key (kbd "M-X") 'smex-major-mode-commands)
            ;; This is your old M-x.
            (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(use-package auto-complete-config
  :diminish auto-complete-mode
  :config (progn
            (setq ac-quick-help-prefer-pos-tip nil)
            (setq ac-comphist-file
                  (concat user-data-directory "ac-comphist.dat"))

            (ac-config-default)
            (setq ac-auto-start nil)
            (ac-set-trigger-key "TAB")
            (bind-key "s-<tab>" 'auto-complete)

            (ac-flyspell-workaround)

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

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(use-package yasnippet
  :mode     ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :commands (yas/minor-mode yas/expand)
  :diminish yas/minor-mode
  :init (yas-global-mode 1)
  :config (progn
            (setq yas-snippet-dirs     (concat user-emacs-directory "snippets")
                  yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

            ;; Use only my own snippets, not the bundled ones.
            (yas-load-directory yas-snippet-dirs)
            (bind-key "C-4" 'yas/expand)))

(bind-key "C-5"        'comment-dwim)
(bind-key "s-<return>" 'dap/smart-open-line)
(bind-key "C-7"        'dap/insert-timestamp)
(bind-key "M-7"        'dap/insert-datestamp)

(bind-key "C-<f2>" 'emacs-index-search)
(bind-key "S-<f2>" 'elisp-index-search)

(unless (package-installed-p 'imenu-anywhere)
  (package-install 'imenu-anywhere))
(use-package imenu-anywhere
  :bind ("C-<f3>". imenu-anywhere))

(bind-key "s-<up>"   'enlarge-window)
(bind-key "s-<down>" 'shrink-window)
(bind-key "s-<right>"'enlarge-window-horizontally)
(bind-key "s-<left>" 'shrink-window-horizontally)

(setq-default ispell-program-name "aspell")

(unless (package-installed-p 'whitespace)
  (package-install 'whitespace))
(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode)
  :config (progn
            (setq whitespace-line-column 80)
            (setq whitespace-style '(trailing lines tab-mark))
            (global-whitespace-mode 1)))

(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))
(use-package rainbow-mode
  :diminish rainbow-mode
  :config (progn
            (hook-into-modes 'rainbow-mode dap/lispy-modes)
            (rainbow-mode 1)))

(use-package ido
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)

            ;; Disable ido faces to see flx highlights.
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces            nil)
            (setq ido-create-new-buffer    'always)

            (setq ido-save-directory-list-file
                  (concat user-data-directory "ido.last"))

            (unless (package-installed-p 'ido-vertical-mode)
              (package-install 'ido-vertical-mode))
            (use-package ido-vertical-mode
              :config (ido-vertical-mode 1))

            (unless (package-installed-p 'ido-hacks)
              (package-install 'ido-hacks))
            (use-package ido-hacks)

            (unless (package-installed-p 'flx-ido)
              (package-install 'flx-ido))
            (use-package flx-ido
              :config (flx-ido-mode 1))

            (unless (package-installed-p 'ido-ubiquitous)
              (package-install 'ido-ubiquitous))
            (use-package ido-ubiquitous
              :config (ido-ubiquitous-mode 1))))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(use-package projectile
  :config (progn
            (setq projectile-cache-file
                  (concat user-data-directory "projectile.cache"))
            (projectile-global-mode 1)))

(unless (package-installed-p 'smartparens)
  (package-install 'smartparens))
(use-package smartparens-config
  :diminish smartparens-mode
  :config (progn
            (hook-into-modes 'turn-on-smartparens-strict-mode dap/lispy-modes)
            (setq sp-show-pair-from-inside nil)))

(use-package tramp
  :config (progn
            ;; Configure Tramp for use with NCI cloud VMs.
            (add-to-list 'tramp-default-proxies-alist
                         '("130\\.56\\."
                           nil
                           "/ssh:dap900@cloudlogin.nci.org.au:"))))

(setq dired-listing-switches  "-alh")
(setq dired-recursive-deletes 1)

(unless (package-installed-p 'dired-details+)
  (package-install 'dired-details+))
(use-package dired-details+
  :config (setq-default dired-details-hidden-string ""))

(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))
(use-package diff-hl)

(defun dap/dired-mode-hook ()
  "Personal dired customizations."
  (diff-hl-dired-mode 1)
  (use-package dired-x))

(add-hook 'dired-mode-hook 'dap/dired-mode-hook)

(use-package find-dired
  :config (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package wdired
  :config (progn
            (setq wdired-allow-to-change-permissions t)
            (setq wdired-allow-to-redirect-links     t)
            (setq wdired-use-interactive-rename      t)
            (setq wdired-confirm-overwrite           t)
            (setq wdired-use-dired-vertical-movement 'sometimes)))

(let ((savehist-file-store (concat user-data-directory "savehist")))
  (when (not (file-exists-p savehist-file-store))
    (warn
     (concat "Can't seem to find a savehist store file where it was expected "
             "at: " savehist-file-store " . Savehist should continue "
             "to function normally; but your history may be lost.")))
  (setq savehist-file savehist-file-store))

(savehist-mode 1)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(defun dap/display-code-line-counts (ov)
  "Displaying overlay content in echo area or tooltip"
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(use-package org
  :init (progn
          (fci-mode 1)
          (dap/untabify-buffer-hook)
          (local-set-key (kbd "C-1") 'org-narrow-to-subtree)
          (local-set-key (kbd "M-1") 'widen)
          (local-set-key (kbd "C-2") 'org-edit-special)

          (setq org-completion-use-ido 1)
          (setq org-use-speed-commands 1)
          (setq org-confirm-shell-link-function 'y-or-n-p)
          (setq org-confirm-elisp-link-function 'y-or-n-p)
          (setq org-pretty-entities 1)
          (setq org-ellipsis "…")
          (setq org-hide-leading-stars 1)
          (setq org-src-fontify-natively nil)
          (setq org-fontify-emphasized-text 1)
          (setq org-src-preserve-indentation 1)
          (setq org-edit-src-content-indentation 0)
          (setq org-highlight-latex-and-related '(latex script entities))

          (setq org-footnote-define-inline 1)
          (setq org-footnote-auto-label 'random)
          (setq org-footnote-auto-adjust nil)
          (setq org-footnote-section nil)

          (setq org-catch-invisible-edits 'error)

          (unless (package-installed-p 'hideshow-org)
            (package-install 'hideshow-org))
          (use-package hideshow-org)

          (unless (package-installed-p 'org-ac)
            (package-install 'org-ac))
          (use-package org-ac
            :init (org-ac/config-default))))

(use-package hideshow
  :init (progn
          (setq hs-hide-comments-when-hiding-all 1)
          (setq hs-isearch-open 1)
          (setq hs-set-up-overlay 'dap/display-code-line-counts)
          (hook-into-modes 'hs-org/minor-mode dap/lispy-modes)

          (defadvice goto-line (after expand-after-goto-line activate compile)
            "How do I get it to expand upon a goto-line? hideshow-expand affected block when using goto-line in a collapsed buffer."
            (save-excursion
              (hs-show-block)))))

(unless (package-installed-p 'flyspell)
  (package-install 'flyspell))
(use-package flyspell
  :init (progn
          (hook-into-modes 'turn-on-flyspell '(text-mode-hook
                                               org-mode-hook))
          (hook-into-modes 'flyspell-prog-mode '(prog-mode-hook))))

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(use-package flycheck
  :init (global-flycheck-mode 1))

(unless (package-installed-p 'fancy-narrow)
  (package-install 'fancy-narrow))
(use-package fancy-narrow)

(defun dap/disable-tabs ()
  "Disables tabs."
  (setq indent-tabs-mode nil))

(defun dap/newline ()
  "Locally binds newline."
  (local-set-key (kbd "RET") 'sp-newline))

(defconst dap/lispy-modes '(emacs-lisp-mode-hook
                            ielm-mode-hook
                            lisp-interaction-mode-hook
                            scheme-mode-hook))

(hook-into-modes 'dap/newline              dap/lispy-modes)
(hook-into-modes 'dap/untabify-buffer-hook dap/lispy-modes)
(hook-into-modes 'dap/disable-tabs         dap/lispy-modes)
(hook-into-modes (lambda ()
                   (add-hook 'local-write-file-hooks 'check-parens))
                 dap/lispy-modes)

(defun dap/elisp-eval-buffer ()
  "Intelligently evaluate an Elisp buffer."
  (interactive)
  (dap/save-all-file-buffers)
  (eval-buffer))

(defun dap/elisp-mode-local-bindings ()
  "Helpful behavior for Elisp buffers."
  (local-set-key (kbd "s-l eb") 'dap/elisp-eval-buffer)
  (local-set-key (kbd "s-l ep") 'eval-print-last-sexp)
  (local-set-key (kbd "s-l td") 'toggle-debug-on-error)
  (local-set-key (kbd "s-l mef") 'macroexpand)
  (local-set-key (kbd "s-l mea") 'macroexpand-all))

(unless (package-installed-p 'lexbind-mode)
  (package-install 'lexbind-mode))
(use-package lexbind-mode)

(defun dap/elisp-mode-hook ()
  (dap/elisp-mode-local-bindings)
  (lexbind-mode)
  (turn-on-eldoc-mode))

(hook-into-modes 'emacs-lisp-mode-hook '(dap/elisp-mode-hook))

(setq initial-scratch-message nil)

(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))
(use-package js2-mode
  :config (progn
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq js-indent-level 2)
            (fci-mode 1)
            (visual-line-mode)
            (dap/untabify-buffer-hook)))

(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))
(use-package web-mode
  :init (progn
          (setq web-mode-enable-block-partial-invalidation t)
          (setq web-mode-engines-alist '(("ctemplate" . "\\.html$"))))
  :config (progn
            (whitespace-turn-off)
            (rainbow-turn-off)
            (visual-line-mode)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-indent-style 2)
            (setq web-mode-style-padding 1)
            (setq web-mode-script-padding 1)
            (setq web-mode-block-padding 0)
            (dap/untabify-buffer-hook)))

(unless (package-installed-p 'json-reformat)
  (package-install 'json-reformat))
(use-package json-reformat)

(unless (package-installed-p 'css-mode)
  (package-install 'css-mode))
(use-package css-mode
  :config (progn
            (fci-mode 1)
            (whitespace-turn-on)
            (rainbow-mode)
            (visual-line-mode)
            (dap/untabify-buffer-hook)
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (fci-mode 1)
            (whitespace-turn-on)
            (rainbow-mode)
            (visual-line-mode)
            (local-set-key (kbd "RET") 'newline-and-indent)))

(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))
(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package visual-line-mode
  :diminish (visual-line-mode
             'global-visual-line-mode)
  :init ((global-visual-line-mode 1)))

(unless (package-installed-p 'ace-link)
  (package-install 'ace-link))
(use-package ace-link
  :init (ace-link-setup-default))

(use-package ruby-mode
  :config (progn
            (fci-mode 1)
            (rainbow-mode)
            (dap/untabify-buffer-hook)
            (visual-line-mode)
            (local-set-key (kbd "RET") 'newline-and-indent)))

(use-package erc
  :init (progn
          (erc-irccontrols-enable)
          (whitespace-turn-off)

          (use-package erc-join
            :init (progn
                    (erc-autojoin-mode 1)
                    (setq erc-autojoin-channels-alist
                          '((".*\\.freenode\\.net" .
                             '("#emacs" "#idris" "#haskell-beginners"))))))

          (use-package erc-button
            :init (progn
                    (erc-button-mode 1)
                    (setq erc-button-wrap-long-urls  nil)
                    (setq erc-button-buttonize-nicks nil)))

          (use-package erc-fill
            :init (progn
                    (erc-fill-mode 1)
                    (setq erc-fill-column        80)
                    (setq erc-fill-function      'erc-fill-static)
                    (setq erc-fill-static-center 0)))

          (use-package erc-netsplit
            :init (erc-netsplit-mode 1))

          (use-package erc-ring
            :init (erc-ring-mode 1))

          (use-package erc-track
            :init (progn
                    (setq erc-track-switch-direction 'importance)
                    (setq erc-track-position-in-mode-line 1)
                    (setq erc-track-exclude-types
                          '("324" "329" "332" "333" "353"
                            "JOIN" "NAMES" "NICK" "QUIT" "PART" "TOPIC"))
                    (add-to-list 'erc-modules 'track)))

          (add-to-list 'erc-modules 'notify)
          (add-to-list 'erc-modules 'scrolltobottom)
          (erc-update-modules)))






(defun dap/smart-open-line ()
  "Insert a new line, indent it, and move the cursor there.

This behavior is different then the typical function bound to return
which may be `open-line' or `newline-and-indent'. When you call with
the cursor between ^ and $, the contents of the line to the right of
it will be moved to the newly inserted line. This function will not
do that. The current line is left alone, a new line is inserted, indented,
and the cursor is moved there.

Attribution: URL http://emacsredux.com/blog/2013/03/26/smarter-open-line/"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun dap/insert-timestamp ()
  "Produces and inserts a full ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

(defun dap/insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun dap/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun dap/untabify-buffer-hook ()
  "Adds a buffer-local untabify on save hook"
  (interactive)
  (add-hook 'after-save-hook
            (lambda () (dap/untabify-buffer))
            nil
            'true))

(defun dap/text-mode-hook ()
  (rainbow-mode)
  (fci-mode)
  (visual-line-mode)
  (dap/untabify-buffer-hook))

(add-hook 'text-mode-hook 'dap/text-mode-hook)






;; (defconst emacs-start-time (current-time))

;; (unless noninteractive
;;   (message "Loading %s..." load-file-name))

;; (defun phunculist/load-init-file (path &optional noerror)
;;   "This loads a file from inside the the .emacs.d directory"
;;   (let ((file (file-name-sans-extension
;;                (expand-file-name path user-emacs-directory))))
;;     (load file noerror)))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (eval-when-compile
;;   (setq use-package-verbose (null byte-compile-current-file)))

;; ;; Emacs server
;; (require 'server)
;; (unless (server-running-p) (server-start))




;; ;; Replace list-buffers with ibuffer.
;; (defalias 'list-buffers 'ibuffer)

;; ;; "y or n" instead of "yes or no"
;; (fset 'yes-or-no-p 'y-or-n-p)


;; (defun indent-buffer ()
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; (defun cleanup-buffer ()
;;   "Perform a bunch of operations on the whitespace content of a buffer.
;; Including indent-buffer, which should not be called automatically on save."
;;   (interactive)
;;   (cleanup-buffer-safe)
;;   (indent-buffer))

;; (bind-key "C-c n" 'cleanup-buffer)

;; ;;;_. Packages

;; (unless (package-installed-p 'fill-column-indicator)
;;   (package-install 'fill-column-indicator))
;; (use-package fill-column-indicator)

(unless (package-installed-p 'ag)
  (package-install 'ag))
(use-package ag)





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

;; ;; ;;;_ , AUCTeX

;; ;; (use-package tex-site
;; ;;   :load-path "site-lisp/auctex/preview/"

;; ;;   :init (progn
;; ;;           (hook-into-modes 'TeX-source-correlate-mode '(LaTeX-mode-hook))
;; ;;           (hook-into-modes 'TeX-PDF-mode '(LaTeX-mode-hook))
;; ;;           (hook-into-modes (lambda ()
;; ;;                              (add-to-list 'TeX-expand-list
;; ;;                                           '("%q" make-skim-url)))
;; ;;                            '(LaTeX-mode-hook))

;; ;;           (use-package))

;; ;;;_ , undo-tree

;; ;;;_ , uniquify


;; ;;;_ , web-mode

;; ;; (use-package web-mode
;; ;;   :init (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; ;;;_ , whitespace



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
              :init (add-hook 'puppet-mode-hook 'flymake-puppet-load))))

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

;; ;; ;;;_ , YAML mode

;; (unless (package-installed-p 'yaml-mode)
;;   (package-install 'yaml-mode))
;; (use-package yaml-mode)

;; ;; ;;;_ , yasnippet


;; (unless (package-installed-p 'keyfreq)
;;   (package-install 'keyfreq))
;; (use-package keyfreq
;;   :init (keyfreq-mode 1))

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

            (setq notmuch-fcc-dirs '(("david.porter@anu.edu.au" . "anu/sent")
                                     (".*" . nil)))

            (setq notmuch-saved-searches
                  '((:name "anu unread" :query "tag:anu AND tag:unread" :sort-order oldest-first)
                    (:name "gmail unread" :query "tag:gmail AND tag:unread" :sort-order oldest-first)
                    (:name "unread" :query "tag:unread" :sort-order oldest-first)
                    (:name "inbox" :query "tag:inbox" :sort-order newest-first)
                    (:name "flagged" :query "tag:flagged" :sort-order newest-first)
                    (:name "drafts" :query "tag:draft" :sort-order newest-first)
                    (:name "sent" :query "tag:sent" :sort-order newest-first)
                    (:name "all mail" :query "*" :sort-order newest-first)))

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

;; Misc functions.

;; (defun goto-line-with-feedback ()
;;   "Show line numbers temporarily, while prompting for the line number input"
;;   (interactive)
;;   (unwind-protect
;;       (progn
;;         (linum-mode 1)
;;         (call-interactively 'goto-line))
;;     (linum-mode -1)))

;; ;; Make "RET" do whatever "M-j" does.
;; (defun phunculist/rebind-return ()
;;   (local-set-key (kbd "RET") (key-binding (kbd "M-j"))))

;; (hook-into-modes 'phunculist/rebind-return '(prog-mode-hook))

;; ;; Colorise ansi escape codes in compilation buffers.
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (read-only-mode -1)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (read-only-mode 1))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
