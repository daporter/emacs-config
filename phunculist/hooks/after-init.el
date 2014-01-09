;; I've swapped my command and option keys in OS X.
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

                                        ; Prevent s-q from quitting emacs.
(global-unset-key (kbd "s-q"))

;; Setup environment variables from the user's shell.
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

(require 'undo-tree)
(global-undo-tree-mode 1)

(require 'hardcore-mode)
(global-hardcore-mode)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(require 'multiple-cursors)
(require 'expand-region)
(require 'delsel)
(require 'smart-forward)
(require 'change-inner)

(require 'achievements-mode)
(achievements-mode 1)

(require 'projectile)
(projectile-global-mode 1)

(require 'saveplace)

(add-hook 'before-save-hook 'phunculist/cleanup-buffer-safe)

;; Use shell-like backspace C-h, rebind help to F1.
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; frame and window management
(global-set-key (kbd "C-c f c") 'make-frame-command)
(global-set-key (kbd "C-c f k") 'delete-frame)
(global-set-key (kbd "C-c f f") (lambda ()
                                  (interactive)
                                  (other-frame -1)))
(global-set-key (kbd "C-c f F") (lambda ()
                                  (interactive)
                                  (other-frame 1)))

(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-x o") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window 1)))
(winner-mode)

;; Command shortcuts.
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c j") 'ace-jump-mode)

(global-set-key (kbd "C-c l")   'phunculist/newline-below)
(global-set-key (kbd "C-c L")   'phunculist/newline-above)
(global-set-key (kbd "C-c RET") 'phunculist/newline-between)

(global-set-key (kbd "C-c ;") 'phunculist/insert-header-comment)

(global-set-key (kbd "C-c r")   'replace-string)
(global-set-key (kbd "C-c R")   'replace-regexp)
(global-set-key (kbd "C-c o e") 're-builder)

(global-set-key (kbd "C-c e") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(global-set-key (kbd "C-c a") 'ack-same)
(global-set-key (kbd "C-c A") 'ack)

(global-set-key (kbd "C-c H") 'phunculist/htmlify-buffer)
(global-set-key (kbd "C-c h") 'phunculist/htmlify-buffer-light)

(global-set-key (kbd "C-c f r") 'phunculist/rename-file-and-buffer)
(global-set-key (kbd "C-c f d") 'phunculist/delete-file-and-buffer)

(global-set-key (kbd "C-c g g") 'magit-status)
(global-set-key (kbd "C-c g b") 'vc-annotate)
(global-set-key (kbd "C-c g p") 'yagist-region-or-buffer)

(global-set-key (kbd "C-c w b") 'phunculist/trim-backwards)
(global-set-key (kbd "C-c w f") 'phunculist/trim-forwards)
(global-set-key (kbd "C-c w a") 'phunculist/trim-backwards-and-forwards)
(global-set-key (kbd "C-c w t") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c w w") 'fixup-whitespace)
(global-set-key (kbd "C-c SPC") 'phunculist/fix-all-whitespace)

(global-set-key (kbd "C-c v l") 'linum-mode)

(global-set-key (kbd "C-c o r") 'inf-ruby)
(global-set-key (kbd "C-c o R") 'phunculist/rails-console)
(global-set-key (kbd "C-c o d") 'phunculist/rails-dbconsole)
(global-set-key (kbd "C-c o s") 'phunculist/open-shell-in-ansi-term)
(global-set-key (kbd "C-c o m") 'man)
(global-set-key (kbd "C-c o c") 'calc)
(global-set-key (kbd "C-c o l") 'ielm)
(global-set-key (kbd "C-c o i") 'phunculist/erc-connect)
