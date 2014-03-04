;; Phunculist's Emacs configuration.

;; Turn off mouse interface early in startup to avoid momentary
;; display.
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

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


;; (defun phunculist/load-init-file (path &optional noerror)
;;   "This loads a file from inside the the .emacs.d directory"
;;   (let ((file (file-name-sans-extension
;;                (expand-file-name path user-emacs-directory))))
;;     (load file noerror)))

;; (phunculist/load-init-file "phunculist/init/freshen.el")
;; (phunculist/load-init-file "phunculist/init/system.el")
;; (phunculist/load-init-file "phunculist/init/autoloads.el")
;; (phunculist/load-init-file "phunculist/init/packages.el")
;; (phunculist/load-init-file "phunculist/init/compile.el")
;; (phunculist/load-init-file "phunculist/init/autohooks.el")
;; (phunculist/load-init-file "phunculist/init/settings.el")




;; ;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))
;; (add-to-list 'load-path user-settings-dir)

;; ;; Add external projects to load path
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))



;; ;; god-mode
;; ;; (require 'god-mode)
;; ;; (global-set-key (kbd "<escape>") 'god-local-mode)

;; ;; Load stuff on demand
;; (autoload 'flycheck-mode "setup-flycheck" nil t)
;; (autoload 'auto-complete-mode "auto-complete" nil t)
;; (require 'setup-auto-complete)

;; ;; Map files to modes
;; (require 'mode-mappings)

;; ;; Highlight escape sequences
;; (require 'highlight-escape-sequences)
;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; ;; Visual regexp
;; (require 'visual-regexp)
;; (define-key global-map (kbd "M-&") 'vr/query-replace)
;; (define-key global-map (kbd "M-/") 'vr/replace)

;; ;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; (require 'jump-char)
;; (require 'wgrep-ag)
;; (require 'multifiles)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)

;; ;; Browse kill ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; ;; Setup key bindings
;; (require 'key-bindings)

;; ;; Misc
;; (require 'my-misc)
;; (when is-mac (require 'mac))

;; ;; Emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; ;; Run at full power please
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))







;;; Phunculist's emacs configuration.  Mostly stolen from
;;; https://github.com/JEG2/dotfiles

;; (defun phunculist/load-init-file (path &optional noerror)
;;   "This loads a file from inside the the .emacs.d directory"
;;   (let ((file (file-name-sans-extension
;;                (expand-file-name path user-emacs-directory))))
;;     (load file noerror)))

;; (phunculist/load-init-file "phunculist/init/freshen.el")
;; (phunculist/load-init-file "phunculist/init/system.el")
;; (phunculist/load-init-file "phunculist/init/autoloads.el")
;; (phunculist/load-init-file "phunculist/init/packages.el")
;; (phunculist/load-init-file "phunculist/init/compile.el")
;; (phunculist/load-init-file "phunculist/init/autohooks.el")
;; (phunculist/load-init-file "phunculist/init/settings.el")




;; ;;;_. Initialization

;; (defconst emacs-start-time (current-time))

;; (unless noninteractive
;;   (message "Loading %s..." load-file-name))

;; (load (expand-file-name "load-path" user-emacs-directory))

;; (server-start)


;; (require 'use-package)
;; (eval-when-compile
;;   (setq use-package-verbose (null byte-compile-current-file)))

(set-face-attribute 'default nil :font "Inconsolata" :height 140)

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

;; Replace list-buffers with ibuffer.
(defalias 'list-buffers 'ibuffer)

;; ;;; NEW

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; ;;; OLD



;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)




;; ;;;_ , Enable disabled commands

;; (put 'downcase-region  'disabled nil)   ; Let downcasing work
;; (put 'erase-buffer     'disabled nil)
;; (put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
;; (put 'narrow-to-page   'disabled nil)   ; Let narrowing work
;; (put 'narrow-to-region 'disabled nil)   ; Let narrowing work
;; (put 'set-goal-column  'disabled nil)
;; (put 'upcase-region    'disabled nil)   ; Let upcasing work

;; ;;;_. Keybindings

;; ;; Main keymaps for personal bindings are:
;; ;;
;; ;;   C-x <letter>  primary map (has many defaults too)
;; ;;   C-c <letter>  secondary map (not just for mode-specific)
;; ;;   C-. <letter>  tertiary map
;; ;;
;; ;;   M-g <letter>  goto map
;; ;;   M-s <letter>  search map
;; ;;   M-o <letter>  markup map (even if only temporarily)
;; ;;
;; ;;   C-<capital letter>
;; ;;   M-<capital letter>
;; ;;
;; ;;   A-<anything>
;; ;;   M-A-<anything>
;; ;;
;; ;; Single-letter bindings still available:
;; ;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;; ;;   M- ?#

;; ;;;_ , global-map

;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)

;; (global-unset-key (kbd "s-q"))       ; prevent s-q from quitting emacs

;; ;;;_  . C-?

;; (defvar ctl-period-map)
;; (define-prefix-command 'ctl-period-map)
;; (bind-key "C-." 'ctl-period-map)

;; (defun beginning-of-indentation-or-line ()
;;   "Move to beginning of indentation, or line."
;;   (interactive)
;;   (let ((p (point)))
;;     (back-to-indentation)
;;     (when (eq p (point))
;;       (beginning-of-line))))

;; (bind-key "C-a" 'beginning-of-indentation-or-line)

;; (defun collapse-or-expand ()
;;   (interactive)
;;   (if (> (length (window-list)) 1)
;;       (delete-other-windows)
;;     (bury-buffer)))

;; (bind-key "C-z" 'collapse-or-expand)

;; ;;;_  . M-?

;; (bind-key "M-!" 'async-shell-command)
;; (bind-key "M-/" 'dabbrev-expand)
;; (bind-key "M-'" 'insert-pair)
;; (bind-key "M-\"" 'insert-pair)

;; (defun align-code (beg end &optional arg)
;;   (interactive "rP")
;;   (if (null arg)
;;       (align beg end)
;;     (let ((end-mark (copy-marker end)))
;;       (indent-region beg end-mark nil)
;;       (align beg end-mark))))

;; (bind-key "M-[" 'align-code)
;; (bind-key "M-`" 'other-frame)

;; (defun delete-indentation-forward ()
;;   (interactive)
;;   (delete-indentation t))

;; (bind-key "M-j" 'delete-indentation-forward)
;; (bind-key "M-J" 'delete-indentation)

;; (bind-key "M-W" 'mark-word)

;; (defun mark-line (&optional arg)
;;   (interactive "p")
;;   (beginning-of-line)
;;   (let ((here (point)))
;;     (dotimes (i arg)
;;       (end-of-line))
;;     (set-mark (point))
;;     (goto-char here)))

;; (bind-key "M-L" 'mark-line)

;; (defun mark-sentence (&optional arg)
;;   (interactive "P")
;;   (backward-sentence)
;;   (mark-end-of-sentence arg))

;; (bind-key "M-S" 'mark-sentence)
;; (bind-key "M-X" 'mark-sexp)
;; (bind-key "M-H" 'mark-paragraph)
;; (bind-key "M-D" 'mark-defun)

;; (bind-key "M-g c" 'goto-char)
;; (bind-key "M-g l" 'goto-line)

;; (bind-key "M-s n" 'find-name-dired)
;; (bind-key "M-s o" 'occur)

;; ;;;_  . M-C-?

;; (bind-key "<C-M-backspace>" 'backward-kill-sexp)

;; (defun isearch-backward-other-window ()
;;   (interactive)
;;   (split-window-vertically)
;;   (call-interactively 'isearch-backward))

;; (bind-key "C-M-r" 'isearch-backward-other-window)

;; (defun isearch-forward-other-window ()
;;   (interactive)
;;   (split-window-vertically)
;;   (call-interactively 'isearch-forward))

;; (bind-key "C-M-s" 'isearch-forward-other-window)

;; ;; Some further isearch bindings
;; (bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
;; (bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
;; (bind-key "C-^" 'isearch-edit-string isearch-mode-map)
;; (bind-key "C-i" 'isearch-complete isearch-mode-map)

;; ;;;_  . A-?

;; (define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;; ;;;_ , ctl-x-map

;; ;;;_  . C-x ?

;; (bind-key "C-x F" 'set-fill-column)
;; (bind-key "C-x t" 'toggle-truncate-lines)

;; ;;;_  .

;; ;;;_  . C-x C-?

;; (defun duplicate-line ()
;;   "Duplicate the line containing point."
;;   (interactive)
;;   (save-excursion
;;     (let (line-text)
;;       (goto-char (line-beginning-position))
;;       (let ((beg (point)))
;;         (goto-char (line-end-position))
;;         (setq line-text (buffer-substring beg (point))))
;;       (if (eobp)
;;           (insert ?\n)
;;         (forward-line))
;;       (open-line 1)
;;       (insert line-text))))

;; (bind-key "C-x C-d" 'duplicate-line)
;; (bind-key "C-x C-e" 'pp-eval-last-sexp)
;; (bind-key "C-x C-n" 'next-line)


;; (defun find-alternate-file-with-sudo (filename)
;;   (interactive
;;    (list (read-file-name "Find alternate file: " nil
;;                          nil nil (concat "/sudo::" (buffer-file-name)))))
;;   (find-alternate-file filename))

;; (bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;; ;;;_  . C-x M-?

;; (bind-key "C-x M-n" 'set-goal-column)

;; (defun refill-paragraph (arg)
;;   (interactive "*P")
;;   (let ((fun (if (memq major-mode '(c-mode c++-mode))
;;                  'c-fill-paragraph
;;                (or fill-paragraph-function
;;                    'fill-paragraph)))
;;         (width (if (numberp arg) arg))
;;         prefix beg end)
;;     (forward-paragraph 1)
;;     (setq end (copy-marker (- (point) 2)))
;;     (forward-line -1)
;;     (let ((b (point)))
;;       (skip-chars-forward "^A-Za-z0-9`'\"(")
;;       (setq prefix (buffer-substring-no-properties b (point))))
;;     (backward-paragraph 1)
;;     (if (eolp)
;;         (forward-char))
;;     (setq beg (point-marker))
;;     (delete-horizontal-space)
;;     (while (< (point) end)
;;       (delete-indentation 1)
;;       (end-of-line))
;;     (let ((fill-column (or width fill-column))
;;           (fill-prefix prefix))
;;       (if prefix
;;           (setq fill-column
;;                 (- fill-column (* 2 (length prefix)))))
;;       (funcall fun nil)
;;       (goto-char beg)
;;       (insert prefix)
;;       (funcall fun nil))
;;     (goto-char (+ end 2))))

;; (bind-key "C-x M-q" 'refill-paragraph)

;; ;;;_ , mode-specific-map

;; ;;;_  . C-c ?

;; (bind-key "C-c <tab>" 'ff-find-other-file)
;; (bind-key "C-c SPC" 'just-one-space)

;; ;; inspired by Erik Naggum's `recursive-edit-with-single-window'
;; (defmacro recursive-edit-preserving-window-config (body)
;;   "*Return a command that enters a recursive edit after executing BODY.
;;  Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
;;  or \\[abort-recursive-edit] (abort)), restore window configuration
;;  in current frame."
;;   `(lambda ()
;;      "See the documentation for `recursive-edit-preserving-window-config'."
;;      (interactive)
;;      (save-window-excursion
;;        ,body
;;        (recursive-edit))))

;; (bind-key "C-c 0"
;;           (recursive-edit-preserving-window-config (delete-window)))
;; (bind-key "C-c 1"
;;           (recursive-edit-preserving-window-config
;;            (if (one-window-p 'ignore-minibuffer)
;;                (error "Current window is the only window in its frame")
;;              (delete-other-windows))))

;; (defun insert-date (arg)
;;   (interactive "P")
;;   (let ((format (if arg "%d/%m/%y" "%Y-%m-%d")))
;;     (insert (format-time-string format))))

;; (bind-key "C-c d" 'insert-date)

;; ;; From http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; (defun my-delete-file-and-buffer ()
;;   "Kill the current buffer and deletes the file it is visiting."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (when filename
;;       (if (vc-backend filename)
;;           (vc-delete-file filename)
;;         (progn
;;           (delete-file filename)
;;           (message "Deleted file %s" filename)
;;           (kill-buffer))))))

;; (bind-key "C-c D" 'my-delete-file-and-buffer)

;; (bind-key "C-c e E" 'elint-current-buffer)

;; (defun do-eval-buffer ()
;;   (interactive)
;;   (call-interactively 'eval-buffer)
;;   (message "Buffer has been evaluated"))

;; (bind-key "C-c e b" 'do-eval-buffer)
;; (bind-key "C-c e c" 'cancel-debug-on-entry)
;; (bind-key "C-c e d" 'debug-on-entry)
;; (bind-key "C-c e e" 'toggle-debug-on-error)
;; (bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
;; (bind-key "C-c e l" 'find-library)
;; (bind-key "C-c e r" 'eval-region)
;; (bind-key "C-c e v" 'edit-variable)

;; (defun find-which (name)
;;   (interactive "sCommand name: ")
;;   (find-file-other-window
;;    (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

;; (bind-key "C-c e w" 'find-which)
;; (bind-key "C-c e z" 'byte-recompile-directory)

;; (bind-key "C-c f" 'flush-lines)

;; (bind-key "C-c k" 'keep-lines)

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

;; (defun my-newline-previous ()
;;   "Insert a blank line above the cursor and move the cursor up
;; one line."
;;   (interactive)
;;   (beginning-of-line)
;;   (newline)
;;   (forward-line -1)
;;   (indent-according-to-mode))

;; (defun my-newline-next ()
;;   "Inserts an indented newline after the current line and moves
;; the point to it."
;;   (interactive)
;;   (end-of-line)
;;   (newline-and-indent))

;; (bind-key "C-c o" 'my-newline-next)
;; (bind-key "C-c O" 'my-newline-previous)

;; (bind-key "C-c q" 'fill-region)

;; ;; From http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; (defun my-rename-file-and-buffer ()
;;   "Rename the current buffer and file it is visiting."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (message "Buffer is not visiting a file!")
;;       (let ((new-name (read-file-name "New name: " filename)))
;;         (cond
;;          ((vc-backend filename) (vc-rename-file filename new-name))
;;          (t
;;           (rename-file filename new-name t)
;;           (set-visited-file-name new-name t t)))))))

;; (bind-key "C-c R" 'my-rename-file-and-buffer)

;; (bind-key "C-c s" 'replace-string)
;; (bind-key "C-c u" 'rename-uniquely)
;; (bind-key "C-c v" 'ffap)

;; (defun view-clipboard ()
;;   (interactive)
;;   (delete-other-windows)
;;   (switch-to-buffer "*Clipboard*")
;;   (let ((inhibit-read-only t))
;;     (erase-buffer)
;;     (clipboard-yank)
;;     (goto-char (point-min))
;;     (html-mode)
;;     (view-mode)))

;; (bind-key "C-c V" 'view-clipboard)
;; (bind-key "C-c z" 'clean-buffer-list)

;; (bind-key "C-c [" 'align-regexp)
;; (bind-key "C-c =" 'count-matches)
;; (bind-key "C-c ;" 'comment-or-uncomment-region)

;; ;;;_  . C-c C-?

;; (defun delete-to-end-of-buffer ()
;;   (interactive)
;;   (kill-region (point) (point-max)))

;; (bind-key "C-c C-z" 'delete-to-end-of-buffer)

;; ;;;_  . C-c M-?

;; (defun unfill-paragraph (arg)
;;   (interactive "*p")
;;   (let (beg end)
;;     (forward-paragraph arg)
;;     (setq end (copy-marker (- (point) 2)))
;;     (backward-paragraph arg)
;;     (if (eolp)
;;         (forward-char))
;;     (setq beg (point-marker))
;;     (when (> (count-lines beg end) 1)
;;       (while (< (point) end)
;;         (goto-char (line-end-position))
;;         (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
;;           (delete-indentation 1)
;;           (if sent-end
;;               (insert ? )))
;;         (end-of-line))
;;       (save-excursion
;;         (goto-char beg)
;;         (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
;;           (replace-match " " nil nil nil 1))))))

;; (bind-key "C-c M-q" 'unfill-paragraph)

;; (defun unfill-region (beg end)
;;   (interactive "r")
;;   (setq end (copy-marker end))
;;   (save-excursion
;;     (goto-char beg)
;;     (while (< (point) end)
;;       (unfill-paragraph 1)
;;       (forward-paragraph))))

;; ;;;_  . C-c w ?

;; (defun my-stack-window ()
;;   "Split window such that the current window is `pushed onto the stack'"
;;   (interactive)
;;   (split-window-vertically -10))

;; (bind-key "C-c w s" 'my-stack-window)

;; ;;;_ , ctl-period-map

;; ;;;_  . C-. ?

;; (bind-key "C-. m" 'kmacro-keymap)

;; ;;;_  . C-. C-i

;; (bind-key "C-. C-i" 'indent-rigidly)

;; ;;;_ , help-map

;; (defvar lisp-find-map)
;; (define-prefix-command 'lisp-find-map)

;; (bind-key "C-h e" 'lisp-find-map)

;; ;;;_  . C-h e ?

;; (bind-key "C-h e c" 'finder-commentary)
;; (bind-key "C-h e e" 'view-echo-area-messages)
;; (bind-key "C-h e f" 'find-function)
;; (bind-key "C-h e F" 'find-face-definition)

;; (defun my-describe-symbol  (symbol &optional mode)
;;   (interactive
;;    (info-lookup-interactive-arguments 'symbol current-prefix-arg))
;;   (let (info-buf find-buf desc-buf cust-buf)
;;     (save-window-excursion
;;       (ignore-errors
;;         (info-lookup-symbol symbol mode)
;;         (setq info-buf (get-buffer "*info*")))
;;       (let ((sym (intern-soft symbol)))
;;         (when sym
;;           (if (functionp sym)
;;               (progn
;;                 (find-function sym)
;;                 (setq find-buf (current-buffer))
;;                 (describe-function sym)
;;                 (setq desc-buf (get-buffer "*Help*")))
;;             (find-variable sym)
;;             (setq find-buf (current-buffer))
;;             (describe-variable sym)
;;             (setq desc-buf (get-buffer "*Help*"))
;;             ;;(customize-variable sym)
;;             ;;(setq cust-buf (current-buffer))
;;             ))))

;;     (delete-other-windows)

;;     (flet ((switch-in-other-buffer
;;             (buf)
;;             (when buf
;;               (split-window-vertically)
;;               (switch-to-buffer-other-window buf))))
;;       (switch-to-buffer find-buf)
;;       (switch-in-other-buffer desc-buf)
;;       (switch-in-other-buffer info-buf)
;;       ;;(switch-in-other-buffer cust-buf)
;;       (balance-windows))))

;; (bind-key "C-h e d" 'my-describe-symbol)
;; (bind-key "C-h e i" 'info-apropos)
;; (bind-key "C-h e k" 'find-function-on-key)
;; (bind-key "C-h e l" 'find-library)

;; (defun scratch ()
;;   (interactive)
;;   (let ((current-mode major-mode))
;;     (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
;;     (goto-char (point-min))
;;     (when (looking-at ";")
;;       (forward-line 4)
;;       (delete-region (point-min) (point)))
;;     (goto-char (point-max))
;;     (if (eq current-mode 'emacs-lisp-mode)
;;         (funcall current-mode))))

;; (bind-key "C-h e s" 'scratch)
;; (bind-key "C-h e v" 'find-variable)
;; (bind-key "C-h e V" 'apropos-value)

;; (defun phunculist/regex-replace
;;   (str regex replacement &optional fixedcase literal)
;;   "Replace a regular expression in the passed string, if it occurs."
;;   (or (when (string-match regex str)
;;         (replace-match replacement fixedcase literal str))
;;       str))

;; (defun phunculist/string-trim (str)
;;   "Trim whitespace from both ends of the passed string."
;;   (phunculist/regex-replace (phunculist/regex-replace str "[ \t]+\\'" "" t t)
;;                             "\\`[ \t]+" "" t t))

;; (defun phunculist/camelize (str)
;;   "Forces a string into CamelCase."
;;   (mapconcat (lambda (s)
;;                (if (string-match "[aeiouy]" s)
;;                    (capitalize s)
;;                  (upcase s)))
;;              (split-string str "[^A-Za-z0-9]")
;;              ""))

;; (defun phunculist/find-subpath-in-path (subpath path)
;;   "Walks up the passed path hunting for subpath at each level."
;;   (let ((match (concat (file-name-as-directory path) subpath)))
;;     (if (file-exists-p match)
;;         match
;;       (unless (string= path "/")
;;         (phunculist/find-subpath-in-path
;;          subpath
;;          (file-name-directory (substring path 0 -1)))))))

;; (defun phunculist/find-in-path (subpath)
;;   "Walks up the current path hunting for subpath at each level."
;;   (phunculist/find-subpath-in-path
;;    subpath
;;    (expand-file-name (if (buffer-file-name)
;;                          (file-name-directory (buffer-file-name))
;;                        default-directory))))

;; (defun phunculist/read-rails-database-config (path)
;;   "Loads the database config as:  adapter database username [password]."
;;   (split-string
;;    (shell-command-to-string
;;     (concat "ruby -ryaml -rerb -e 'puts YAML.load(ERB.new(ARGF.read).result)[%q{"
;;             (or (getenv "RAILS_ENV") "development")
;;             "}].values_at(*%w[adapter database username password])"
;;             ".compact.join(%q{ })' "
;;             path))))

;; (require 'cl-macs)
;; (require 'sql)

;; (defun phunculist/rails-console ()
;;   "Invoke inf-ruby with Rails environment loaded."
;;   (interactive)
;;   (let ((config (phunculist/find-in-path "config/environment.rb")))
;;     (if config
;;         (let ((binstub (concat (file-name-directory
;;                                 (substring (file-name-directory config) 0 -1))
;;                                "bin/rails")))
;;           (if (file-exists-p binstub)
;;               (run-ruby (concat binstub " console") "rails")
;;             (run-ruby "bundle exec rails console" "rails"))))))

;; (defun phunculist/rails-dbconsole ()
;;   "Open a SQL shell using the settings from config/database.yml."
;;   (interactive)
;;   (let ((config (phunculist/find-in-path "config/database.yml")))
;;     (if config
;;         (let* ((env     (phunculist/read-rails-database-config config))
;;                (adapter (car env))
;;                (db      (cond ((string-match "\\`mysql"   adapter)
;;                                "mysql")
;;                               ((string-match "\\`sqlite"  adapter)
;;                                "sqlite")
;;                               ((string=      "postgresql" adapter)
;;                                "postgres"))))
;;           (let ((sql-fun      (intern (concat "sql-" db)))
;;                 (sql-database (cadr   env))
;;                 (sql-user     (or (caddr  env) user-login-name))
;;                 (sql-password (cadddr env)))
;;             (cl-letf (((symbol-function 'sql-get-login)  ; silence confirmation
;;                        #'(lambda (&rest what) t)))
;;               (funcall sql-fun)))))))

;; ;;;_. Packages

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(use-package auto-complete-config
  :config (progn
            (ac-config-default)
            (ac-set-trigger-key "TAB")))

(unless (package-installed-p 'color-theme-sanityinc-tomorrow)
  (package-install 'color-theme-sanityinc-tomorrow))
(use-package color-theme-sanityinc-tomorrow
  :config (load-theme 'sanityinc-tomorrow-night t))

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
  :config (projectile-global-mode 1))

(unless (package-installed-p 'helm)
  (package-install 'helm))
(use-package helm-config
  :config (progn
            (bind-key "C-c h" 'helm-mini)
            (bind-key "M-x" 'helm-M-x)
            (helm-mode 1)))

(unless (package-installed-p 'helm-projectile)
  (package-install 'helm-projectile))
(use-package helm-projectile
  :config (use-package projectile))

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

;; ;;;_ , undo-tree

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

;; ;;;_ , uniquify

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; ;;;_ , web-mode

;; (use-package web-mode
;;   :init (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; ;;;_ , whitespace

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
  :init (chruby "ruby-2.1"))

(unless (package-installed-p 'puppet-mode)
  (package-install 'puppet-mode))
(use-package puppet-mode
  :config (progn
            (unless (package-installed-p 'flymake-puppet)
              (package-install 'flymake-puppet))
            (use-package flymake-puppet
              :init (hook-into-modes 'flymake-puppet-load '(puppet-mode-hook)))))

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

;; ;;;_ , zenburn-theme

;; (use-package zenburn-theme
;;   :disabled t)

(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
(use-package ledger-mode
  :config (progn
            (unless (package-installed-p 'flycheck-ledger)
              (package-install 'flycheck-ledger))
            (use-package flycheck-ledger)))

;; Misc functions.

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

                                        ;(load (expand-file-name "key-bindings" user-emacs-directory))

(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key "<f1>" 'help-command)

(bind-key "M-h" 'backward-kill-word)

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


;; ;;;_. Settings

;; (setq custom-file (concat user-emacs-directory "settings.el"))
;; (load custom-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:inherit lazy-highlight))))
 '(helm-source-header ((t (:inherit (info-title-3 secondary-selection))))))
;; ;; Local Variables:
;; ;;   mode: emacs-lisp
;; ;;   mode: allout
;; ;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; ;; End:
