(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start 3)
 '(ac-comphist-file "~/.emacs.d/data/ac-comphist.dat")
 '(ac-ignore-case nil)
 '(ac-use-fuzzy nil)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bookmark-default-file "~/.emacs.d/data/bookmarks")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(diff-mode-hook (quote (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(el-get-auto-update-cached-recipes nil t)
 '(el-get-generate-autoloads nil t)
 '(emacs-lisp-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))) eldoc-mode (lambda nil (local-set-key [(meta 46)] (quote find-function)) (local-set-key [(control 109)] (quote newline-and-indent))))))
 '(fill-column 79)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-extra-args (quote ("--sug-mode=fast" "--keyboard=dvorak")))
 '(ledger-reports (quote (("bal" "ledger -f %(ledger-file) bal") ("reg" "ledger -f %(ledger-file) reg") ("payee" "ledger -f %(ledger-file) reg -- %(payee)") ("account" "ledger -f %(ledger-file) reg %(account)") ("buckets" "ledger -f %(ledger-file) bal Buckets"))))
 '(line-spacing 0.2)
 '(menu-bar-mode t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:")))
 '(recentf-max-saved-items 200)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))))))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/backups")
 '(visible-bell t)
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab lines))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-header ((t (:background "#1d1f21" :foreground "#969896" :box nil :underline nil))))
 '(helm-selection ((t (:inherit region))))
 '(helm-source-header ((t (:inherit (hl-line font-lock-doc-string-face))))))
