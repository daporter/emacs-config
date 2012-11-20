;;;_. Initialization

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" user-emacs-directory))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(set-face-attribute 'default nil :font "Inconsolata" :height 140)

(setq auto-save-list-file-prefix
      (concat user-emacs-directory "backups/auto-save-list/.saves-"))

;; Use GNU Coreutils version of `ls', which is called `gls' when installed via
;; Homebrew.
(setq insert-directory-program "gls")

;; Replace list-buffers with ibuffer.
(defalias 'list-buffers 'ibuffer)

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;;;_ , exec-path-from-shell

(use-package exec-path-from-shell
  :config (progn
            (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "LEDGER_FILE")))

;; rbenv

(setq    exec-path (cons   "/usr/local/opt/rbenv/bin"      exec-path))
(setenv  "PATH"    (concat "/usr/local/opt/rbenv/bin:"     (getenv "PATH")))
(setq    exec-path (cons   "/usr/local/opt/rbenv/shims"    exec-path))
(setenv  "PATH"    (concat "/usr/local/opt/rbenv/shims:"   (getenv "PATH")))
(setq    exec-path (cons   "/usr/local/opt/rbenv/libexec"  exec-path))
(setenv  "PATH"    (concat "/usr/local/opt/rbenv/libexec:" (getenv "PATH")))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_. Keybindings

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;;;_ , global-map

(global-unset-key (kbd "s-q"))       ; prevent s-q from quitting emacs

;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(defun beginning-of-indentation-or-line ()
  "Move to beginning of indentation, or line."
  (interactive)
  (let ((p (point)))
    (back-to-indentation)
    (when (eq p (point))
      (beginning-of-line))))

(bind-key "C-a" 'beginning-of-indentation-or-line)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

;;;_  . M-?

(bind-key "M-!" 'async-shell-command)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(bind-key "M-s n" 'find-name-dired)
(bind-key "M-s o" 'occur)

;;;_  . M-C-?

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(defun isearch-backward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-backward))

(bind-key "C-M-r" 'isearch-backward-other-window)

(defun isearch-forward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-forward))

(bind-key "C-M-s" 'isearch-forward-other-window)

;; Some further isearch bindings
(bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
(bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
(bind-key "C-^" 'isearch-edit-string isearch-mode-map)
(bind-key "C-i" 'isearch-complete isearch-mode-map)

;;;_  . A-?

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;;_ , ctl-x-map

;;;_  . C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

;;;_  .

;;;_  . C-x C-?

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)


(defun find-alternate-file-with-sudo (filename)
  (interactive
   (list (read-file-name "Find alternate file: " nil
                         nil nil (concat "/sudo::" (buffer-file-name)))))
  (find-alternate-file filename))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;;;_  . C-x M-?

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

;;;_ , mode-specific-map

;;;_  . C-c ?

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(bind-key "C-c 0"
          (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
          (recursive-edit-preserving-window-config
           (if (one-window-p 'ignore-minibuffer)
               (error "Current window is the only window in its frame")
             (delete-other-windows))))

(defun insert-date (arg)
  (interactive "P")
  (let ((format (if arg "%d/%m/%y" "%Y-%m-%d")))
    (insert (format-time-string format))))

(bind-key "C-c d" 'insert-date)

(bind-key "C-c e E" 'elint-current-buffer)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'flush-lines)

(bind-key "C-c k" 'keep-lines)

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

(defun my-newline-previous ()
  "Insert a blank line above the cursor and move the cursor up
one line."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun my-newline-next ()
  "Inserts an indented newline after the current line and moves
the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(bind-key "C-c o" 'my-newline-next)
(bind-key "C-c O" 'my-newline-previous)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)
(bind-key "C-c v" 'ffap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_  . C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_  . C-c M-?

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;;_ , ctl-period-map

;;;_  . C-. ?

(bind-key "C-. m" 'kmacro-keymap)

;;;_  . C-. C-i

(bind-key "C-. C-i" 'indent-rigidly)

;;;_ , help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_  . C-h e ?

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(defun my-describe-symbol  (symbol &optional mode)
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (let (info-buf find-buf desc-buf cust-buf)
    (save-window-excursion
      (ignore-errors
        (info-lookup-symbol symbol mode)
        (setq info-buf (get-buffer "*info*")))
      (let ((sym (intern-soft symbol)))
        (when sym
          (if (functionp sym)
              (progn
                (find-function sym)
                (setq find-buf (current-buffer))
                (describe-function sym)
                (setq desc-buf (get-buffer "*Help*")))
            (find-variable sym)
            (setq find-buf (current-buffer))
            (describe-variable sym)
            (setq desc-buf (get-buffer "*Help*"))
            ;;(customize-variable sym)
            ;;(setq cust-buf (current-buffer))
            ))))

    (delete-other-windows)

    (flet ((switch-in-other-buffer
            (buf)
            (when buf
              (split-window-vertically)
              (switch-to-buffer-other-window buf))))
      (switch-to-buffer find-buf)
      (switch-in-other-buffer desc-buf)
      (switch-in-other-buffer info-buf)
      ;;(switch-in-other-buffer cust-buf)
      (balance-windows))))

(bind-key "C-h e d" 'my-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (eq current-mode 'emacs-lisp-mode)
        (funcall current-mode))))

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)

;;;_. Packages

;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :bind ("C-. C-s" . ace-jump-mode))

;;;_ , AUCTeX

(use-package tex-site
  :load-path "site-lisp/auctex/preview/"

  :init (progn
          (hook-into-modes 'TeX-source-correlate-mode '(LaTeX-mode-hook))
          (hook-into-modes 'TeX-PDF-mode '(LaTeX-mode-hook))
          (hook-into-modes (lambda ()
                             (add-to-list 'TeX-expand-list
                                          '("%q" make-skim-url)))
                           '(LaTeX-mode-hook))

          (use-package reftex
            :init (hook-into-modes 'turn-on-reftex '(LaTeX-mode-hook))))

  :config (progn
            (use-package preview-latex)

            ;; Make AUCTeX aware of style files and multi-file documents.
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            (setq-default TeX-master nil)

            (defun make-skim-url ()
              (concat
               (TeX-current-line)
               " \""
               (expand-file-name (funcall file (TeX-output-extension) t)
                                 (file-name-directory (TeX-master-file)))
               "\" \""
               (buffer-file-name)
               "\""))

            (setq TeX-source-correlate-method 'synctex)
            (setq TeX-view-program-list
                  '(("Skim"
                     "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))
            (setq TeX-view-program-selection '((output-pdf "Skim")))))

;;;_ , auto-complete

(use-package auto-complete-config
  :diminish auto-complete-mode

  :config (progn
            (use-package popup)
            (use-package fuzzy)

            (setq ac-use-menu-map          t
                  ac-user-dictionary-files (concat user-data-directory "dict"))

            (ac-config-default)
            (ac-set-trigger-key "TAB")))

;;;_ , autopair

(use-package autopair
  :commands autopair-mode
  :diminish autopair-mode

  :init (hook-into-modes #'autopair-mode '(c-mode-common-hook
                                           text-mode-hook
                                           ruby-mode-hook
                                           python-mode-hook
                                           sh-mode-hook)))

;;;_ , autorevert

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode

  :init (add-hook 'find-file-hook
                  #'(lambda ()
                      (auto-revert-mode 1))))

;;;_ , bookmark

(use-package bookmark
  :config (setq bookmark-default-file
                (concat user-data-directory "bookmarks")))

;;;_ , buffer-move

(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

;;;_ , coffee-mode

(use-package coffee-mode)

;;;_ , css-mode

(use-package css-mode
  :config (progn
            (setq css-indent-offset 2)))

;;;_ , eproject

(use-package eproject
  :load-path ("site-lisp/eproject/lang/" "site-lisp/eproject/contrib")

  :init (progn
          (use-package eproject-extras
            :init (progn
                    (bind-key "C-x p k" 'eproject-kill-project-buffers)
                    (bind-key "C-x p v" 'eproject-revisit-project)
                    (bind-key "C-x p b" 'eproject-ibuffer)
                    (bind-key "C-x p o" 'eproject-open-all-project-files)))

          (use-package eproject-ruby)
          (use-package eproject-ruby-on-rails)
          (use-package eproject-tags))

  :config (progn
            (setq eproject-completing-read-function
                  'eproject--ido-completing-read)

            (bind-key "C-x f" 'eproject-find-file)))

;;;_ , expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;_ , find-file-in-project

(use-package find-file-in-project
  :disabled t
  :bind ("C-x f" . find-file-in-project)

  :config (progn
            ;; Use eproject to find the project root.
            (setq ffip-project-root-function 'eproject-root)))

;;;_ , flyspell

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region))

  :init (setq ispell-dictionary-base-alist
              '((nil                    ; default
                 "[A-Za-z]" "[^A-Za-z]" "[']" t
                 ("-d" "/Library/Spelling/en_GB" "-i" "utf-8") nil utf-8)
                ("british"              ; British English
                 "[A-Za-z]" "[^A-Za-z]" "[']" t
                 ("-d" "/Library/Spelling/en_GB" "-i" "utf-8") nil utf-8)
                ("australian"           ; Australian English
                 "[A-Za-z]" "[^A-Za-z]" "[']" t
                 ("-d" "/Library/Spelling/en_AU" "-i" "utf-8") nil utf-8)
                ("american"             ; American English
                 "[A-Za-z]" "[^A-Za-z]" "[']" t
                 ("-d" "/Library/Spelling/en_US" "-i" "utf-8") nil utf-8)
                ("dutch"                ; Dutch
                 "[A-Za-z]" "[^A-Za-z]" "[']" t
                 ("-d" "/Library/Spelling/nl_NL" "-i" "utf-8") nil utf-8)))

  :config (progn
            (setq ispell-program-name "hunspell"
                  ispell-dictionary "british"
                  ispell-extra-args nil
                  ispell-silently-savep t)))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config (define-key flyspell-mode-map [(control ?.)] nil))

;;;_ , haml-mode

(use-package haml-mode)

;;;_ , haskell-mode

(use-package haskell-mode
  :commands haskell-mode

  :init (progn
          (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

  :config (progn
            (use-package inf-haskell)))

;;;_ , ido

(use-package ido
  :init (progn
          (ido-mode t))

  :config (progn
            (use-package ido-ubiquitous
              :init (ido-ubiquitous-mode 1))

            (use-package ido-springboard)

            (setq ido-enable-prefix         nil
                  ido-enable-flex-matching  t
                  ido-create-new-buffer     'always
                  ido-use-filename-at-point nil
                  ido-max-prospects         10)
            (setq ido-save-directory-list-file
                  (concat user-data-directory "ido.last"))
            ;; Always rescan buffer for imenu.
            (set-default 'imenu-auto-rescan t)

            (bind-key "C-w" 'ido-delete-backward-updir
                      ido-file-completion-map)
            (bind-key "C-x C-w" 'ido-copy-current-file-name
                      ido-file-completion-map)))

;;;_ , jump-char

(use-package jump-char
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

;;;_ , Ledger

(use-package ldg-new
  :load-path "/Library/Caches/Homebrew/ledger--git/lisp"
  :commands  ledger-mode

  :init (progn
          (use-package ledger)

          (add-to-list 'auto-mode-alist
                       (cons (file-name-nondirectory (getenv "LEDGER_FILE"))
                             'ledger-mode))

          (defun my-goto-ledger ()
            "Goto my ledger file."
            (interactive)
            (find-file (getenv "LEDGER_FILE"))
            (ledger-find-slot (current-time)))

          (bind-key "C-c l" 'my-goto-ledger)

          (defun my-ledger-start-entry (&optional arg)
            (interactive "p")
            (find-file-other-window (getenv "LEDGER_FILE"))
            (goto-char (point-max))
            (skip-syntax-backward " ")
            (if (looking-at "\n\n")
                (goto-char (point-max))
              (delete-region (point) (point-max))
              (insert ?\n)
              (insert ?\n))
            (insert (format-time-string "%Y/%m/%d ")))

          (bind-key "C-c L" 'my-ledger-start-entry)))

;;;_ , linum

(use-package linum
  :commands linum-mode
  :init (hook-into-modes 'linum-mode '(prog-mode-hook
                                       haml-mode-hook)))

;;;_ , lorem-ipsum

(use-package lorem-ipsum
  :commands (Lorem-ipsum-insert-paragraphs
             Lorem-ipsum-insert-sentences
             Lorem-ipsum-insert-list))

;;;_ , magit

(use-package magit
  :bind ("C-x g" . magit-status)

  :config (progn
            (setenv "GIT_PAGER" "")

            (setq magit-repo-dirs
                  (list user-emacs-directory "~/Documents/Projects"))

            (add-hook 'magit-log-edit-mode-hook
                      #'(lambda ()
                          (set-fill-column 72)
                          (flyspell-mode)))

            (require 'magit-topgit)
            (require 'rebase-mode)))

;;;_ , markdown-mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

;;;_ , multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("S-s-SPC"     . set-rectangular-region-anchor)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this))

  :config (progn
            (setq mc/list-file (concat user-data-directory "mc-lists.el"))))

;;;_ , paredit

(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode

  :init (hook-into-modes 'paredit-mode '(emacs-lisp-mode-hook)))

;;;_ , perspective

(use-package perspective
  :init (progn (persp-mode t)))

;;;_ , recentf

(use-package recentf
  :if (not noninteractive)
  :init (progn
          (recentf-mode 1)

          (defun recentf-add-dired-directory ()
            (if (and dired-directory
                     (file-directory-p dired-directory)
                     (not (string= "/" dired-directory)))
                (let ((last-idx (1- (length dired-directory))))
                  (recentf-add-file
                   (if (= ?/ (aref dired-directory last-idx))
                       (substring dired-directory 0 last-idx)
                     dired-directory)))))

          (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

;;;_ , ruby-mode

(use-package ruby-mode
  :mode (("\\.rb\\'"     . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Gemfile\\'"   . ruby-mode))

  :interpreter ("ruby" . ruby-mode)

  :config (progn
            (use-package yari)
            (use-package inf-ruby)
            (use-package ruby-tools)
            (use-package ruby-mode-expansions)

            (defun my-ruby-smart-return ()
              (interactive)
              (when (memq (char-after) '(?\| ?\" ?\'))
                (forward-char))
              (call-interactively 'newline-and-indent))

            (defun my-ruby-mode-hook ()
              (bind-key "<return>" 'my-ruby-smart-return ruby-mode-map))

            ;; This gets called automatically by auto-complete.
            (defun ac-ruby-mode-setup ()
              (setq ac-sources '(ac-source-yasnippet
                                 ac-source-abbrev
                                 ac-source-imenu
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers)))

            (defun my-ruby-align-setup()
              (require 'align)

              (defconst align-ruby-modes '(ruby-mode)
                "align-perl-modes is a variable defined in `align.el'.")

              (defconst ruby-align-rules-list
                '((ruby-comma-delimiter
                   (regexp . ",\\(\\s-*\\)[^/ \t\n]")
                   (modes  . align-ruby-modes)
                   (repeat . t))
                  (ruby-string-after-func
                   (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
                   (modes  . align-ruby-modes)
                   (repeat . t))
                  (ruby-symbol-after-func
                   (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
                   (modes  . align-ruby-modes)))
                "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

              (dolist (it '(align-perl-modes
                            align-dq-string-modes
                            align-sq-string-modes
                            align-open-comment-modes))
                (add-to-list it 'ruby-mode))

              (dolist (it ruby-align-rules-list)
                (add-to-list 'align-rules-list it)))

            (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
            (add-hook 'ruby-mode-hook 'my-ruby-align-setup)))

;;;_ , scss-mode

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode))

;;;_ , server

(use-package server
  :init (unless (server-running-p) (server-start)))

;;;_ , slim-mode

(use-package slim-mode
  :mode ("\\.slim\\'" . slim-mode)

  :config (progn
            (setq css-indent-offset 2)))

;;;_ , smex

(use-package smex
  :bind (("M-x"         . smex)
         ("M-X"         . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))

  :config (progn
            (smex-initialize)
            (setq smex-save-file (concat user-data-directory "smex-items"))))

;;;_ , solarized-theme

(use-package solarized-light-theme
  :disabled t)

(use-package solarized-dark-theme
  :init (load-theme 'solarized-dark t))

;;;_ , undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

;;;_ , uniquify

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;;_ , whitespace

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
            (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

;;;_ , winner

(use-package winner
  :if (not noninteractive)
  :diminish winner-mode

  :init (progn
          (winner-mode 1)

          (bind-key "M-N" 'winner-redo)
          (bind-key "M-P" 'winner-undo)))

;;;_ , YAML mode

(use-package yaml-mode)

;;;_ , yasnippet

(use-package yasnippet
  :mode     ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :commands (yas/minor-mode yas/expand)
  :diminish yas/minor-mode

  :init (progn
          (hook-into-modes #'(lambda () (yas/minor-mode 1))
                           '(prog-mode-hook ruby-mode-hook)))

  :config (progn
            (setq yas/snippet-dirs     (concat user-emacs-directory "snippets")
                  yas/trigger-key      (kbd "C-c y TAB")
                  yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))

            (bind-key "TAB" 'yas/next-field-or-maybe-expand yas/keymap)
            (bind-key "RET" 'yas/exit-all-snippets          yas/keymap)

            (yas/initialize)
            ;; Use only my own snippets, not the bundled ones.
            (yas/load-directory yas/snippet-dirs)))

;;;_ , zenburn-theme

(use-package zenburn-theme
  :disabled t)


;;;_. Settings

(setq custom-file (concat user-emacs-directory "settings.el"))
(load custom-file)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
