;;;_. Initialization

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(set-face-attribute 'default nil :font "Inconsolata" :height 140)

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

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
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'flush-lines)

(bind-key "C-c k" 'keep-lines)

(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

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

;;;_ , el-get

(use-package el-get
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages)

  :init (progn
          (defvar el-get-sources nil)
          (setq el-get-auto-update-cached-recipes nil)
          (setq el-get-dir "~/.emacs.d/site-lisp/")
          (setq el-get-generate-autoloads nil))


  :config (defun el-get-read-status-file ()
            (mapcar #'(lambda (entry)
                        (cons (plist-get entry :symbol)
                              `(status "installed" recipe ,entry)))
                    el-get-sources))

  (defalias 'el-get-init 'ignore
    "Don't use el-get for making packages available for use."))

;;;_ , flyspell

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config (define-key flyspell-mode-map [(control ?.)] nil))

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

;;;_ , ido

(use-package ido
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :init (ido-mode 'buffer)

  :config (progn
            (use-package ido-hacks
              :init (ido-hacks-mode 1))

    (defun ido-smart-select-text ()
      "Select the current completed item.  Do NOT descend into directories."
      (interactive)
      (when (and (or (not ido-require-match)
                     (if (memq ido-require-match
                               '(confirm confirm-after-completion))
                         (if (or (eq ido-cur-item 'dir)
                                 (eq last-command this-command))
                             t
                           (setq ido-show-confirm-message t)
                           nil))
                     (ido-existing-item-p))
                 (not ido-incomplete-regexp))
        (when ido-current-directory
          (setq ido-exit 'takeprompt)
          (unless (and ido-text (= 0 (length ido-text)))
            (let ((match (ido-name (car ido-matches))))
              (throw 'ido
                     (setq ido-selected
                           (if match
                               (replace-regexp-in-string "/\\'" "" match)
                             ido-text)
                           ido-text ido-selected
                           ido-final-text ido-text)))))
        (exit-minibuffer)))
    
    (add-hook 'ido-minibuffer-setup-hook
              #'(lambda ()
                  (bind-key "<return>" 'ido-smart-select-text
                            ido-file-completion-map)))
    
    (defun ido-switch-buffer-tiny-frame (buffer)
      (interactive (list (ido-read-buffer "Buffer: " nil t)))
      (with-selected-frame
          (make-frame '((width                . 80)
                        (height               . 22)
                        (left-fringe          . 0)
                        (right-fringe         . 0)
                        (vertical-scroll-bars . nil)
                        (unsplittable         . t)
                        (has-modeline-p       . nil)
                        (minibuffer           . nil)))
        (switch-to-buffer buffer)
        (set (make-local-variable 'mode-line-format) nil)))
    
    (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)))

;;;_ , Ledger

(use-package "ldg-new"
  :commands ledger-mode
  :init (progn
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

;;;_ , Enhanced-Ruby-Mode

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :config (progn
            (setq enh-ruby-program
                  (concat (car rvm--current-ruby-binary-path) "ruby"))

            (use-package yari
              :init ((progn )
                     (defvar yari-helm-source-ri-pages
                       '((name . "RI documentation")
                         (candidates . (lambda () (yari-ruby-obarray)))
                         (action  ("Show with Yari" . yari))
                         (candidate-number-limit . 300)
                         (requires-pattern . 2)
                         "Source for completing RI documentation."))

                     (defun helm-yari (&optional rehash)
                       (interactive (list current-prefix-arg))
                       (when current-prefix-arg (yari-ruby-obarray rehash))
                       (helm 'yari-helm-source-ri-pages
                             (yari-symbol-at-point)))))

            (use-package inf-ruby)

            (defun my-ruby-smart-return ()
              (interactive)
              (when (memq (char-after) '(?\| ?\" ?\'))
                (forward-char))
              (call-interactively 'newline-and-indent))

            (defun my-ruby-mode-hook ()
              (bind-key "<return>" 'my-ruby-smart-return ruby-mode-map)
              (bind-key "C-h C-i" 'helm-yari ruby-mode-map)

              (set (make-local-variable 'yas/fallback-behavior)
                   '(apply ruby-indent-command . nil))
              (bind-key "<tab>" 'yas/expand-from-trigger-key ruby-mode-map))

            (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

;;;_ , rvm

(use-package rvm
  :config (progn (rvm-use-default)))

;;;_ , scss-mode

(use-package scss-mode)

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
                             c-mode-common-hook))

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
  :diminish winner-mode
  :if (not noninteractive)
  :init (progn
          (winner-mode 1)
          
          (bind-key "M-N" 'winner-redo)
          (bind-key "M-P" 'winner-undo)))

;;;_ , yasnippet

(use-package yasnippet
  :if (not noninteractive)
  :diminish yas/minor-mode
  :commands (yas/minor-mode yas/expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init (hook-into-modes #'(lambda () (yas/minor-mode 1))
                         '(prog-mode-hook
                           org-mode-hook
                           ruby-mode-hook
                           message-mode-hook
                           gud-mode-hook
                           erc-mode-hook))
  :config (progn
            (yas/initialize)
            (yas/load-directory
             (expand-file-name "snippets/" user-emacs-directory))

            (bind-key "<tab>" 'yas/next-field-or-maybe-expand yas/keymap)
            
            (defun yas/new-snippet (&optional choose-instead-of-guess)
              (interactive "P")
              (let ((guessed-directories (yas/guess-snippet-directories)))
                (switch-to-buffer "*new snippet*")
                (erase-buffer)
                (kill-all-local-variables)
                (snippet-mode)
                (set (make-local-variable 'yas/guessed-modes)
                     (mapcar #'(lambda (d)
                                 (intern (yas/table-name (car d))))
                             guessed-directories))
                (unless (and choose-instead-of-guess
                             (not (y-or-n-p
                                   "Insert a snippet with useful headers? ")))
                  (yas/expand-snippet "\
# -*- mode: snippet -*-
# name: $1
# --
$0"))))

            (bind-key "C-c y TAB" 'yas/expand)
            (bind-key "C-c y n" 'yas/new-snippet)
            (bind-key "C-c y f" 'yas/find-snippets)
            (bind-key "C-c y r" 'yas/reload-all)
            (bind-key "C-c y v" 'yas/visit-snippet-file)))

;;;_ , zenburn-theme

(use-package zenburn-theme
  :init (progn (load-theme 'zenburn t)))




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


(setq custom-file "~/.emacs.d/settings.el")
(load custom-file)


;;;_. My stuff

;; ;;;; Editing settings.

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


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
