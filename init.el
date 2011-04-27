(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(server-start)

(set-face-attribute 'default nil :font "Inconsolata" :height 140)
(setq-default line-spacing 0.2)

(setq inhibit-startup-screen t)
(menu-bar-mode 1)
(tool-bar-mode 0)
(column-number-mode 1)
(scroll-bar-mode 0)
(global-hl-line-mode 1)
(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(windmove-default-keybindings 'ctrl)


;;;; Keyboard.

(global-unset-key (kbd "s-q"))          ; prevent s-q from quitting emacs
(global-unset-key (kbd "C-z"))          ; prevent C-z from minimising the frame

;; For some reason the above line doesn't remove the keybinding for C-z (in
;; emacs 24.0.50.1) so we need this.
(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "RET")        'reindent-then-newline-and-indent)
(global-set-key (kbd "C-z <up>")   'phunculist/move-line-up)
(global-set-key (kbd "C-z <down>") 'phunculist/move-line-down)
(global-set-key (kbd "C-z a")      'align-regexp)
(global-set-key (kbd "C-z c")      'phunculist/cleanup-buffer)
(global-set-key (kbd "C-z C")      'phunculist/copy-line)
(global-set-key (kbd "C-z d")      'phunculist/duplicate-line)
(global-set-key (kbd "C-z l")      'phunculist/ledger-edit)
(global-set-key (kbd "C-z w")      'whitespace-cleanup)
(global-set-key (kbd "C-z |")      'align)


;;;; Editing settings.

(setq scroll-preserve-screen-position t) ; don't move point when scrolling
(setq-default indent-tabs-mode nil)     ; don't insert tabs on indent
(setq-default fill-column 80)
(setq require-final-newline t)          ; crontabs break without this

;; Put all backups in a single directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(show-paren-mode 1)

;; Configure automatic scrolling to prevent the window from jumping around too
;; much.
(setq scroll-margin 1)
(setq-default scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

;; Activating cua-mode enables yasnippet to wrap snippets around the current
;; region.
(cua-mode 1)
;; But we don't want the CUA keybindings unless the last region was marked
;; using a shifted movement key.
(setq cua-enable-cua-keys 'shift)

(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         lines-tail
                         empty
                         indentation
                         space-before-tab
                         space-after-tab))
(global-whitespace-mode 1)

(require 'uniquify)
(setq uniquify-buffer-file-name 'forward)

(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list
             'try-complete-file-name-partially
             t)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq ispell-program-name "aspell")

(add-hook 'css-mode
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-css-property)
            (setq css-indent-level 2)))

(setq ediff-split-window-function 'split-window-horizontally)

;; Wrap commit messages when created from command-line git.
(add-hook 'diff-mode-hook (lambda () (setq fill-column 72)))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; By default Emacs will pass -exec to find, and that makes it very slow. It's
;; better to collate the matches and then use xargs to run the command.
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; Find recent files with Ido.

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ; was find-file-read-only


;;; Ledger config.

(require 'ledger)

(defun phunculist/ledger-edit ()
  "Open my ledger."
  (interactive)
  (find-file (getenv "LEDGER_FILE"))
  (ledger-find-slot (current-time)))


;;; Regex Builder config.

(require 're-builder)
(setq reb-re-syntax 'string)            ; backslashes don't need to be escaped


;;; Ruby Configuration.

;; Setup align for ruby-mode.

(require 'align)

(defconst align-ruby-modes '(ruby-mode)
  "align-ruby-modes is a variable defined in `phunculist.el'.")

(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

(add-to-list 'align-perl-modes         'ruby-mode)
(add-to-list 'align-dq-string-modes    'ruby-mode)
(add-to-list 'align-sq-string-modes    'ruby-mode)
(add-to-list 'align-open-comment-modes 'ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))

;; Some handy ruby-mode functions.

(defun phunculist/braces-to-do-end-delimiters (beg end)
  "Change all `{}' block delimiters in region to `do ... end'"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward "{\\s *\\(|\\sw+|\\)" nil t)
        (replace-match "do \\1\n" nil nil))
      (goto-char beg)
      (while (re-search-forward "\\s *}" nil t)
        (replace-match "\nend" nil t))
      (indent-region (point-min) (point-max)))))

(defun phunculist/parenthesise-function-call (beg end)
  "Turn a single-line function call into multiline format.

Parenthesises the argument list and places each argument on a
separate line."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (re-search-forward "\\(\\S +\\s *=\\s *\\)?\\(\\S +\\)\\s +" nil t)
      (replace-match "\\1\\2(" nil nil)
      (phunculist/separate-items (point) end)
      (re-search-forward "\\(\\S \\)\\s *$" nil t)
      (replace-match "\\1)" nil nil)
      (indent-region (point-min) (point-max)))))

(defun phunculist/one-item-per-line (beg end)
  "Make the comma-delimited items in region one per line."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (phunculist/separate-items (point-min) (point-max))
      (indent-region (point-min) (point-max)))))

(defun phunculist/separate-items (beg end)
  "Separate comma-delimited items in region one per line."
  (goto-char beg)
  (while (re-search-forward ",[\\s \n]*\\(\\S \\)" end t)
    (replace-match ",\n\\1" nil nil)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-z {")
                           'phunculist/braces-to-do-end-delimiters)
            (local-set-key (kbd "C-z p") 'phunculist/parenthesise-function-call)
            (local-set-key (kbd "C-z i") 'phunculist/one-item-per-line)
            (local-set-key (kbd "RET")   'reindent-then-newline-and-indent)))


;;;; Misc functions.

(defun phunculist/copy-line (arg)
  "Copy the current line to the kill ring.
With prefix arg, copy this many lines."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun phunculist/duplicate-line (n)
  "Duplicates the current line.
With prefix arg, duplicate current line this many times."
  (interactive "p")
  (save-excursion
    (phunculist/copy-line 1)
    (forward-line)
    (dotimes (i n)
      (yank)))
  (message "Current line duplicated%s"
           (if (< n 2)
               ""
             (concat " " (int-to-string n) " times"))))

;; Easily move lines around in buffer.
(defun phunculist/move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun phunculist/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (phunculist/move-line (if (null n) -1 (- n))))

(defun phunculist/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (phunculist/move-line (if (null n) 1 n)))

(defun phunculist/swap-buffers ()
  "Swap the buffer in the current window with the buffer in the
other window."
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(defun phunculist/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun phunculist/insert-current-date ()
  "Insert current date in yyyy-mm-dd format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun phunculist/cleanup-buffer ()
  "Indent and remove whitespace from buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

;;;;
;;;; Install and configure 3rd-party libraries using el-get.
;;;;

;; Add the user-contributed repository of emacs packages.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)


;; Install el-get.  (Detects when el-get is already installed.)
;; (url-retrieve
;;  "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;;  (lambda (s)
;;    (end-of-buffer)
;;    (eval-print-last-sexp)))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(setq el-get-sources
      '(el-get
        mode-compile
        markdown-mode
        haml-mode
        rinari
        ruby-end
        flymake-ruby
        full-ack
        yari
        color-theme
        paredit
        yaml-mode

        (:name color-theme-zenburn
               :features zenburn)

        (:name buffer-move
               :after (lambda ()
                        (global-set-key (kbd "<M-up>")    'buf-move-up)
                        (global-set-key (kbd "<M-down>")  'buf-move-down)
                        (global-set-key (kbd "<M-left>")  'buf-move-left)
                        (global-set-key (kbd "<M-right>") 'buf-move-right)))

        (:name tiling
               :type emacswiki
               :features "tiling"
               :after (lambda ()
                        (define-key global-map
                          (kbd "C-\\") 'tiling-cycle)
                        (define-key global-map
                          (kbd "C-M-<up>") 'tiling-tile-up)
                        (define-key global-map
                          (kbd "C-M-<down>") 'tiling-tile-down)
                        (define-key global-map
                          (kbd "C-M-<right>") 'tiling-tile-right)
                        (define-key global-map
                          (kbd "C-M-<left>") 'tiling-tile-left)))

        (:name auctex
               :build `("./autogen.sh"
                        ,(concat "./configure"
                                 " --with-lispdir=`pwd`"
                                 " --with-texmf-dir=/Library/TeX/Root/texmf"
                                 " --with-emacs=" el-get-emacs)
                        "make")
               :after (lambda ()
                        (add-hook 'latex-mode-hook 'TeX-PDF-mode)
                        (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
                        (setq LaTeX-command "latex -synctex=1"
                              TeX-view-program-list '(("Skim"
                                                       "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))
                              TeX-view-program-selection '(((output-dvi style-pstricks)
                                                            "dvips and gv")
                                                           (output-dvi "xdvi")
                                                           (output-pdf "Skim")
                                                           (output-html
                                                            "xdg-open")))))

        (:name magit
               :features (magit magit-svn)
               :after (lambda ()
                        (global-set-key (kbd "C-z g") 'magit-status)
                        (setq magit-repo-dirs
                              '("~/.emacs.d" "~/Documents/Projects"))
                        (add-hook 'magit-log-edit-mode-hook
                                  (lambda () (setq fill-column 72)))))

        (:name auto-complete
               :after (lambda ()
                        (setq-default ac-sources
                                      '(ac-source-yasnippet
                                        ac-source-filename
                                        ac-source-abbrev
                                        ac-source-dictionary
                                        ac-source-words-in-same-mode-buffers))
                        (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)))

        (:name yasnippet
               :after (lambda ()
                        (add-to-list 'yas/root-directory
                                     "~/.emacs.d/phunculist-snippets")
                        (mapc 'yas/load-directory yas/root-directory)
                        (setq yas/wrap-around-region 'cua)))

        (:name ruby-complexity
               :type git
               :url "git://github.com/jsmestad/ruby-complexity.git"
               :after (lambda ()
                        (add-hook 'ruby-mode-hook
                                  (lambda ()
                                    (flymake-mode 1)
                                    (linum-mode 1)
                                    (ruby-complexity-mode 1)))))

        (:name haskell-mode
               :after (lambda ()
                        (add-hook 'haskell-mode-hook
                                  'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook
                                  'turn-on-haskell-indentation)))

        (:name rvm
               :after (lambda () (rvm-use-default)))

        (:name smex
               :after (lambda ()
                        (global-set-key (kbd "M-x") 'smex)
                        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                        ;; This is the old M-x:
                        (global-set-key (kbd "C-c C-c M-x")
                                        'execute-extended-command)))

        (:name lorem-ipsum
               :type emacswiki
               :features lorem-ipsum)

        (:name color-theme-solarized
               :type git
               :url "https://github.com/altercation/solarized.git"
               :load "emacs-colors-solarized/color-theme-solarized.el"
               :after (lambda () (color-theme-solarized-dark)))

        (:name idomenu
               :type emacswiki
               :features idomenu
               :after (lambda () (global-set-key (kbd "C-z m") 'idomenu)))))

(el-get 'sync)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
