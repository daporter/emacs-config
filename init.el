;;;_. Initialization

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;;;_ , Read system environment

(let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
  (when (file-readable-p plist)
    (let ((dict (cdr (assq 'dict (cdar (xml-parse-file plist))))))
      (while dict
        (if (and (listp (car dict))
                 (eq 'key (caar dict)))
            (setenv (car (cddr (car dict)))
                    (car (cddr (car (cddr dict))))))
        (setq dict (cdr dict))))

    ;; Configure exec-path based on the new PATH
    (setq exec-path nil)
    (mapc (apply-partially #'add-to-list 'exec-path)
          (nreverse (split-string (getenv "PATH") ":")))))

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_. Display

(setq redisplay-dont-pause t)

(set-face-attribute 'default nil :font "Inconsolata" :height 140)
(setq-default line-spacing 0.2)

(setq inhibit-startup-screen t)
(menu-bar-mode 1)
(tool-bar-mode 0)
(column-number-mode 1)
(scroll-bar-mode 0)
(global-hl-line-mode 1)
(setq visible-bell t)

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

;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

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

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-s n" 'find-name-dired)
(bind-key "M-s o" 'occur)

(bind-key "A-M-w" 'copy-code-as-rtf)

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

(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

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

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

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
(bind-key "C-c e s" 'scratch)
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'flush-lines)
(bind-key "C-c g" 'goto-line)

(bind-key "C-c k" 'keep-lines)

(eval-when-compile
  (defvar emacs-min-top)
  (defvar emacs-min-left)
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(unless noninteractive
  (defvar emacs-min-top 22)
  (defvar emacs-min-left (- (x-display-pixel-width) 918))
  (defvar emacs-min-height (if (= 1050 (x-display-pixel-height)) 55 64))
  (defvar emacs-min-width 100)))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (when running-alternate-emacs
    (set-background-color "grey85")
    (set-face-background 'fringe "gray80")))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" 'emacs-toggle-size)

(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" 'insert-user-timestamp)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

(defvar printf-index 0)

(defun insert-counting-printf (arg)
  (interactive "P")
  (if arg
      (setq printf-index 0))
  (if t
      (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                      (setq printf-index (1+ printf-index))))
    (insert (format "printf(\"step %d..\\n\");\n"
                    (setq printf-index (1+ printf-index)))))
  (forward-line -1)
  (indent-according-to-mode)
  (forward-line))

(bind-key "C-c p" 'insert-counting-printf)
(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(autoload 'auth-source-search "auth-source")

(defun tinify-url (url)
  (interactive "sURL to shorten: ")
  (let* ((api-login "jwiegley")
         (api-key
          (funcall
           (plist-get
            (car (auth-source-search :host "api.j.mp" :user api-login
                                     :type 'netrc :port 80))
            :secret))))
    (flet ((message (&rest ignore)))
      (with-current-buffer
          (let ((query
                 (format "format=txt&longUrl=%s&login=%s&apiKey=%s"
                         (url-hexify-string url) api-login api-key)))
            (url-retrieve-synchronously
             (concat "http://api.j.mp/v3/shorten?" query)))
        (goto-char (point-min))
        (re-search-forward "^$")
        (prog1
            (kill-new (buffer-substring (1+ (point)) (1- (point-max))))
          (kill-buffer (current-buffer)))))))

(bind-key "C-c U" 'tinify-url)
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

;; ;; Run in server-mode so other sessions can connect.
;; (add-hook 'server-visit-hook (lambda () (raise-frame)))
;; (add-hook 'server-done-hook
;;           (lambda ()
;;             (shell-command "osascript -e \"tell application \\\"System Events\\\" to keystroke tab using command down\"")))
;; (server-start)


;;                                         ;(load-theme 'zenburn t)

;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (winner-mode 1)
;; (windmove-default-keybindings 'ctrl)

;; (setq uniquify-buffer-name-style 'forward)


;; ;;;; Keyboard.

;; (global-unset-key (kbd "s-q"))          ; prevent s-q from quitting emacs
;; (global-unset-key (kbd "C-z"))          ; prevent C-z from minimising the frame

;; ;; For some reason the above line doesn't remove the keybinding for C-z (in
;; ;; emacs 24.0.50.1) so we need this.
;; (global-set-key (kbd "C-z") nil)

;; (global-set-key (kbd "RET")        'reindent-then-newline-and-indent)
;; (global-set-key (kbd "C-z <up>")   'phunculist/move-line-up)
;; (global-set-key (kbd "C-z <down>") 'phunculist/move-line-down)
;; (global-set-key (kbd "C-z a")      'align-regexp)
;; (global-set-key (kbd "C-z c")      'phunculist/cleanup-buffer)
;; (global-set-key (kbd "C-z C")      'phunculist/copy-line)
;; (global-set-key (kbd "C-z d")      'phunculist/duplicate-line)
;; (global-set-key (kbd "C-z l")      'phunculist/ledger-edit)
;; (global-set-key (kbd "C-z w")      'whitespace-cleanup)
;; (global-set-key (kbd "C-z |")      'align)


;; ;;;; Editing settings.

;; (setq scroll-preserve-screen-position t) ; don't move point when scrolling
;; (setq-default indent-tabs-mode nil)     ; don't insert tabs on indent
;; (setq-default fill-column 80)
;; (setq require-final-newline t)          ; crontabs break without this

;; ;; Put all backups in a single directory.
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; (show-paren-mode 1)

;; ;; Configure automatic scrolling to prevent the window from jumping around too
;; ;; much.
;; (setq scroll-margin 1)
;; (setq-default scroll-up-aggressively   0.0
;;               scroll-down-aggressively 0.0)

;; ;; Activating cua-mode enables yasnippet to wrap snippets around the current
;; ;; region.
;; (cua-mode 1)
;; ;; But we don't want the CUA keybindings unless the last region was marked
;; ;; using a shifted movement key.
;; (setq cua-enable-cua-keys 'shift)

;; (require 'whitespace)
;; (setq whitespace-style '(face
;;                          trailing
;;                          tabs
;;                          lines-tail
;;                          empty
;;                          indentation
;;                          space-before-tab
;;                          space-after-tab))
;; (global-whitespace-mode 1)

;; (require 'uniquify)
;; (setq uniquify-buffer-file-name 'forward)

;; (ido-mode 1)
;; (setq ido-enable-flex-matching t
;;       ido-create-new-buffer 'always)

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)

;; ;; Hippie expand: at times perhaps too hip
;; (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
;;   (delete f hippie-expand-try-functions-list))

;; ;; Add this back in at the end of the list.
;; (add-to-list 'hippie-expand-try-functions-list
;;              'try-complete-file-name-partially
;;              t)

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; (setq ispell-program-name "aspell")

;; (add-hook 'css-mode
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-css-property)
;;             (setq css-indent-offset 2)))

;; (setq ediff-split-window-function 'split-window-horizontally)

;; ;; Wrap commit messages when created from command-line git.
;; (add-hook 'diff-mode-hook (lambda () (setq fill-column 72)))

;; (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
;; (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; ;; By default Emacs will pass -exec to find, and that makes it very slow. It's
;; ;; better to collate the matches and then use xargs to run the command.
;; (require 'find-dired)
;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; ;; Find recent files with Ido.

;; (require 'recentf)
;; (recentf-mode t)
;; (setq recentf-max-saved-items 50)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (find-file (ido-completing-read "Find recent file: " recentf-list)))

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open) ; was find-file-read-only


;; ;;; Ledger config.

;; (require 'ledger)

;; (defun phunculist/ledger-edit ()
;;   "Open my ledger."
;;   (interactive)
;;   (find-file (getenv "LEDGER_FILE"))
;;   (ledger-find-slot (current-time)))


;; ;;; Regex Builder config.

;; (require 're-builder)
;; (setq reb-re-syntax 'string)            ; backslashes don't need to be escaped


;; ;; Wrap manpages at 80 chars.
;; (setq Man-width 80)


;; ;;; Ruby Configuration.

;; ;; Setup align for ruby-mode.

;; (require 'align)

;; (defconst align-ruby-modes '(ruby-mode)
;;   "align-ruby-modes is a variable defined in `phunculist.el'.")

;; (defconst ruby-align-rules-list
;;   '((ruby-comma-delimiter
;;      (regexp . ",\\(\\s-*\\)[^/ \t\n]")
;;      (modes . align-ruby-modes)
;;      (repeat . t))
;;     (ruby-string-after-func
;;      (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
;;      (modes . align-ruby-modes)
;;      (repeat . t))
;;     (ruby-symbol-after-func
;;      (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
;;      (modes . align-ruby-modes)))
;;   "Alignment rules specific to the ruby mode.
;; See the variable `align-rules-list' for more details.")

;; (add-to-list 'align-perl-modes         'ruby-mode)
;; (add-to-list 'align-dq-string-modes    'ruby-mode)
;; (add-to-list 'align-sq-string-modes    'ruby-mode)
;; (add-to-list 'align-open-comment-modes 'ruby-mode)
;; (dolist (it ruby-align-rules-list)
;;   (add-to-list 'align-rules-list it))

;; ;; Some handy ruby-mode functions.

;; (defun phunculist/braces-to-do-end-delimiters (beg end)
;;   "Change all `{}' block delimiters in region to `do ... end'"
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (goto-char beg)
;;       (while (re-search-forward "{\\s *\\(|\\sw+|\\)" nil t)
;;         (replace-match "do \\1\n" nil nil))
;;       (goto-char beg)
;;       (while (re-search-forward "\\s *}" nil t)
;;         (replace-match "\nend" nil t))
;;       (indent-region (point-min) (point-max)))))

;; (defun phunculist/parenthesise-function-call (beg end)
;;   "Turn a single-line function call into multiline format.

;; Parenthesises the argument list and places each argument on a
;; separate line."
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (goto-char beg)
;;       (re-search-forward "\\(\\S +\\s *=\\s *\\)?\\(\\S +\\)\\s +" nil t)
;;       (replace-match "\\1\\2(" nil nil)
;;       (phunculist/separate-items (point) end)
;;       (re-search-forward "\\(\\S \\)\\s *$" nil t)
;;       (replace-match "\\1)" nil nil)
;;       (indent-region (point-min) (point-max)))))

;; (defun phunculist/one-item-per-line (beg end)
;;   "Make the comma-delimited items in region one per line."
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (phunculist/separate-items (point-min) (point-max))
;;       (indent-region (point-min) (point-max)))))

;; (defun phunculist/separate-items (beg end)
;;   "Separate comma-delimited items in region one per line."
;;   (goto-char beg)
;;   (while (re-search-forward ",[\\s \n]*\\(\\S \\)" end t)
;;     (replace-match ",\n\\1" nil nil)))

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-z {")
;;                            'phunculist/braces-to-do-end-delimiters)
;;             (local-set-key (kbd "C-z p") 'phunculist/parenthesise-function-call)
;;             (local-set-key (kbd "C-z i") 'phunculist/one-item-per-line)
;;             (local-set-key (kbd "RET")   'reindent-then-newline-and-indent)))


;; ;;;; Misc functions.

;; (defun phunculist/copy-line (arg)
;;   "Copy the current line to the kill ring.
;; With prefix arg, copy this many lines."
;;   (interactive "p")
;;   (kill-ring-save (line-beginning-position)
;;                   (line-beginning-position (+ 1 arg)))
;;   (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; (defun phunculist/duplicate-line (n)
;;   "Duplicates the current line.
;; With prefix arg, duplicate current line this many times."
;;   (interactive "p")
;;   (save-excursion
;;     (phunculist/copy-line 1)
;;     (forward-line)
;;     (dotimes (i n)
;;       (yank)))
;;   (message "Current line duplicated%s"
;;            (if (< n 2)
;;                ""
;;              (concat " " (int-to-string n) " times"))))

;; ;; Easily move lines around in buffer.
;; (defun phunculist/move-line (n)
;;   "Move the current line up or down by N lines."
;;   (interactive "p")
;;   (let ((col (current-column))
;;         start
;;         end)
;;     (beginning-of-line)
;;     (setq start (point))
;;     (end-of-line)
;;     (forward-char)
;;     (setq end (point))
;;     (let ((line-text (delete-and-extract-region start end)))
;;       (forward-line n)
;;       (insert line-text)
;;       ;; restore point to original column in moved line
;;       (forward-line -1)
;;       (forward-char col))))

;; (defun phunculist/move-line-up (n)
;;   "Move the current line up by N lines."
;;   (interactive "p")
;;   (phunculist/move-line (if (null n) -1 (- n))))

;; (defun phunculist/move-line-down (n)
;;   "Move the current line down by N lines."
;;   (interactive "p")
;;   (phunculist/move-line (if (null n) 1 n)))

;; (defun phunculist/swap-buffers ()
;;   "Swap the buffer in the current window with the buffer in the
;; other window."
;;   (interactive)
;;   (cond ((one-window-p) (display-buffer (other-buffer)))
;;         ((let* ((buffer-a (current-buffer))
;;                 (window-b (cadr (window-list)))
;;                 (buffer-b (window-buffer window-b)))
;;            (set-window-buffer window-b buffer-a)
;;            (switch-to-buffer buffer-b)
;;            (other-window 1)))))

;; (defun phunculist/rename-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to
;; NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;       (if (get-buffer new-name)
;;           (message "A buffer named '%s' already exists!" new-name)
;;         (progn
;;           (rename-file name new-name 1)
;;           (rename-buffer new-name)
;;           (set-visited-file-name new-name)
;;           (set-buffer-modified-p nil))))))

;; (defun phunculist/insert-current-date ()
;;   "Insert current date in yyyy-mm-dd format."
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d")))

;; (defun phunculist/cleanup-buffer ()
;;   "Indent and remove whitespace from buffer."
;;   (interactive)
;;   (indent-region (point-min) (point-max))
;;   (whitespace-cleanup))

;;;_. Packages

;;;_ , el-get

(use-package el-get
  ;; :disabled t
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages)
  :init (defvar el-get-sources nil)

  :config (defun el-get-read-status-file ()
            (mapcar #'(lambda (entry)
                        (cons (plist-get entry :symbol)
                              `(status "installed" recipe ,entry)))
                    el-get-sources))

  (defalias 'el-get-init 'ignore
    "Don't use el-get for making packages available for use."))

(use-package ace-jump-mode
  :bind ("S-<return>" . ace-jump-mode))

;;;_ , auto-complete

(use-package auto-complete-config
  :commands auto-complete-mode
  :diminish auto-complete-mode
  :config (progn
            (ac-set-trigger-key "TAB")
            (setq ac-use-menu-map t)))

;;;_ , autorevert

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init (add-hook 'find-file-hook
                  #'(lambda ()
                      (auto-revert-mode 1))))

;;;_ , zenburn-theme

(use-package zenburn-theme
  :init (progn (load-theme 'zenburn)))

;;;_ , helm

(use-package helm-config
  :init (progn
          (bind-key "C-c M-x" 'helm-M-x)
          (bind-key "C-h a" 'helm-c-apropos)
          (bind-key "M-s a" 'helm-do-grep)

          (defun my-helm-occur ()
            (interactive)
            (require 'helm-regexp)
            (helm-other-buffer 'helm-c-source-occur "*Helm Occur*"))

          (bind-key "M-s b" 'my-helm-occur)
          (bind-key "M-s F" 'helm-for-files)

          (defun my-helm-apropos ()
            (interactive)
            (require 'helm-elisp)
            (require 'helm-misc)
            (let ((default (thing-at-point 'symbol)))
              (helm
               :prompt "Info about: "
               :candidate-number-limit 15
               :sources
               (append (mapcar (lambda (func)
                                 (funcall func default))
                               '(helm-c-source-emacs-commands
                                 helm-c-source-emacs-functions
                                 helm-c-source-emacs-variables
                                 helm-c-source-emacs-faces
                                 helm-c-source-helm-attributes))
                       '(helm-c-source-info-emacs
                         helm-c-source-info-elisp
                         helm-c-source-info-gnus
                         helm-c-source-info-org
                         helm-c-source-info-cl
                         helm-c-source-emacs-source-defun)))))

          (bind-key "C-h e a" 'my-helm-apropos)

          (defun helm-c-source-git-files-init ()
            "Build `helm-candidate-buffer' of Git files."
            (with-current-buffer (helm-candidate-buffer 'local)
              (mapcar
               (lambda (item)
                 (insert (expand-file-name item) ?\n))
               (split-string (shell-command-to-string "git ls-files") "\n"))))

          (defun helm-find-git-file ()
            (interactive)
            (helm :sources 'helm-c-source-git-files
                  :input ""
                  :prompt "Find file: "
                  :buffer "*Helm git file*"))

          (bind-key "C-x f" 'helm-find-git-file)
          (bind-key "M-g g" 'helm-find-git-file)
          (bind-key "C-h b" 'helm-descbinds))

  :config (progn
            (helm-match-plugin-mode t)

            (use-package helm-descbinds
              :commands helm-descbinds
              :init (fset 'describe-bindings 'helm-descbinds))

            (defvar helm-c-source-git-files
              '((name . "Files under Git version control")
                (init . helm-c-source-git-files-init)
                (candidates-in-buffer)
                (type . file))
              "Search for files in the current Git project.")

            (eval-after-load "helm-files"
              '(add-to-list 'helm-for-files-prefered-list
                            'helm-c-source-git-files))))

;;;_ , magit

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (progn
	    (setenv "GIT_PAGER" "")

	    (setq magit-repo-dirs '("~/.emacs.d" "~/Documents/Projects"))

	    (add-hook 'magit-log-edit-mode-hook
		      #'(lambda ()
			  (set-fill-column 72)
			  (flyspell-mode)))

	    (require 'magit-topgit)
	    (require 'rebase-mode)))

;; Add the user-contributed repository of emacs packages.
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("technomancy" . "http://repo.technomancy.us/emacs/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("ELPA" . "http://tromey.com/elpa/") t)


;; ;; Install el-get.
;; (unless (require 'el-get nil t)
;;   (url-retrieve
;;    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;;    (lambda (s)
;;      (end-of-buffer)
;;      (eval-print-last-sexp))))

;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (require 'el-get)

;; ;; Local recipes.
;; (setq el-get-sources
;;       '((:name expand-region
;;                :after (progn
;;                         (global-set-key (kbd "C-@") 'er/expand-region)
;;                         (global-set-key (kbd "C-M-@") 'er/contract-region)))

;;         (:name buffer-move
;;                :after (progn
;;                         (global-set-key (kbd "<M-up>")    'buf-move-up)
;;                         (global-set-key (kbd "<M-down>")  'buf-move-down)
;;                         (global-set-key (kbd "<M-left>")  'buf-move-left)
;;                         (global-set-key (kbd "<M-right>") 'buf-move-right)))

;;         (:name tiling
;;                :type emacswiki
;;                :features "tiling"
;;                :after (progn
;;                         (define-key global-map
;;                           (kbd "C-\\") 'tiling-cycle)
;;                         (define-key global-map
;;                           (kbd "C-M-<up>") 'tiling-tile-up)
;;                         (define-key global-map
;;                           (kbd "C-M-<down>") 'tiling-tile-down)
;;                         (define-key global-map
;;                           (kbd "C-M-<right>") 'tiling-tile-right)
;;                         (define-key global-map
;;                           (kbd "C-M-<left>") 'tiling-tile-left)))

;;         (:name auctex
;;                :build `("./autogen.sh"
;;                         ,(concat "./configure"
;;                                  " --with-lispdir=`pwd`"
;;                                  " --with-texmf-dir=/Library/TeX/Root/texmf"
;;                                  " --with-emacs=" el-get-emacs)
;;                         "make")
;;                :after (progn
;;                         (add-hook 'latex-mode-hook 'TeX-PDF-mode)
;;                         (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;;                         (setq LaTeX-command "latex -synctex=1"
;;                               TeX-view-program-list '(("Skim"
;;                                                        "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))
;;                               TeX-view-program-selection '(((output-dvi style-pstricks)
;;                                                             "dvips and gv")
;;                                                            (output-dvi "xdvi")
;;                                                            (output-pdf "Skim")
;;                                                            (output-html
;;                                                             "xdg-open")))))

;;         (:name magit
;;                :features (magit magit-svn)
;;                :after (progn
;;                         (global-set-key (kbd "C-z g") 'magit-status)
;;                         (setq magit-repo-dirs
;;                               '("~/.emacs.d" "~/Documents/Projects"))
;;                         (add-hook 'magit-log-edit-mode-hook
;;                                   (lambda () (setq fill-column 72)))))

;;         (:name auto-complete
;;                :after (progn
;;                         (setq-default ac-sources
;;                                       '(ac-source-yasnippet
;;                                         ac-source-filename
;;                                         ac-source-abbrev
;;                                         ac-source-dictionary
;;                                         ac-source-words-in-same-mode-buffers))
;;                         (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)))

;;         (:name yasnippet
;;                :after (progn
;;                         (add-to-list 'yas/root-directory
;;                                      "~/.emacs.d/phunculist-snippets")
;;                         (mapc 'yas/load-directory yas/root-directory)
;;                         (setq yas/wrap-around-region 'cua)))

;;         (:name ruby-complexity
;;                :type git
;;                :url "git://github.com/jsmestad/ruby-complexity.git"
;;                :after (progn
;;                         (add-hook 'ruby-mode-hook
;;                                   (lambda ()
;;                                     (flymake-mode 1)
;;                                     (linum-mode 1)
;;                                     (ruby-complexity-mode 1)))))

;;         (:name haskell-mode
;;                :after (progn
;;                         (add-hook 'haskell-mode-hook
;;                                   'turn-on-haskell-doc-mode)
;;                         (add-hook 'haskell-mode-hook
;;                                   'turn-on-haskell-indentation)))

;;         (:name rvm
;;                :after (progn (rvm-use-default)))

;;         (:name smex
;;                :after (progn
;;                         (global-set-key (kbd "M-x") 'smex)
;;                         (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;                         ;; This is the old M-x:
;;                         (global-set-key (kbd "C-c C-c M-x")
;;                                         'execute-extended-command)))

;;         (:name lorem-ipsum
;;                :type emacswiki
;;                :features lorem-ipsum)

;;         (:name sql-indent
;;                :type emacswiki
;;                :features sql-indent
;;                :after (progn
;;                         (eval-after-load "sql"
;;                           (load-library "sql-indent"))))
;;         (:name helm
;;                :after (progn
;;                         (global-set-key (kbd "C-z h") 'helm-mini)))
;;         (:name ace-jump-mode
;;                :after (progn
;;                         (global-set-key (kbd "C-c SPC") 'ace-jump-mode)))
;;         (:name idomenu
;;                :type emacswiki
;;                :features idomenu
;;                :after (progn (global-set-key (kbd "C-z m") 'idomenu)))))

;; (setq my:el-get-packages '(el-get
;;                            mode-compile
;;                            markdown-mode
;;                            haml-mode
;;                            rinari
;;                            ruby-end
;;                            ruby-electric
;;                            flymake-ruby
;;                            full-ack
;;                            yari
;;                            color-theme
;;                            paredit
;;                            yaml-mode
;;                            growl
;;                            rhtml-mode
;;                            slim-mode
;;                            scss-mode))

;; (setq my:el-get-packages
;;       (append
;;        my:el-get-packages
;;        (loop for src in el-get-sources collect (el-get-source-name src))))

;; ;; Install new packages and init already installed packages.
;; (el-get 'sync my:el-get-packages)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
