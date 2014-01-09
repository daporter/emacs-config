;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-packages.html

(require 'package)

(dolist (repo '(("elpa"      . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(defun phunculist/flatten (x)
  "Flatten a list."
  (cond ((null x) nil)
        ((listp x) (append (phunculist/flatten (car x))
                           (phunculist/flatten (cdr x))))
        (t (list x))))

(defun phunculist/package-refresh-and-install (name)
  "Ensure we have a fresh package list, then install."
  (package-refresh-contents)
  (package-install name))

(defun phunculist/package-install-unless-installed (name)
  "Install a package by name unless it is already installed."
  (or (package-installed-p name) (phunculist/package-refresh-and-install name)))

(defun phunculist/package-version-for (package)
  "Get the version of a loaded package."
  (package-desc-vers (cdr (assoc package package-alist))))

(defun phunculist/package-delete-by-name (package)
  "Remove a package by name."
  (package-delete (symbol-name package)
                  (package-version-join (phunculist/package-version-for package))))

(defun phunculist/package-delete-unless-listed (packages)
  "Remove packages not explicitly declared."
  (let ((packages-and-dependencies (phunculist/packages-requirements packages)))
    (dolist (package (mapcar 'car package-alist))
      (unless (memq package packages-and-dependencies)
        (phunculist/package-delete-by-name package)))))

(defun phunculist/packages-requirements (packages)
  "List of dependencies for packages."
  (delete-dups (apply 'append (mapcar 'phunculist/package-requirements packages))))

(defun phunculist/package-requirements (package)
  "List of recursive dependencies for a package."
  (let ((package-info (cdr (assoc package package-alist))))
    (cond ((null package-info) (list package))
          (t
           (phunculist/flatten
            (cons package
                  (mapcar 'phunculist/package-requirements
                          (mapcar 'car (package-desc-reqs package-info)))))))))

(defun phunculist/package-install-and-remove-to-match-list (packages)
  "Sync packages so the installed list matches the passed list."
  (package-initialize)
  (condition-case nil ;; added to handle no-network situations
      (mapc 'phunculist/package-install-unless-installed packages)
    (error (message "Couldn't install package. No network connection?")))
  (phunculist/package-delete-unless-listed packages))

(phunculist/package-install-and-remove-to-match-list
 '(ace-jump-mode
   achievements-mode
   ag
   auctex
   auto-complete
   autopair
   bash-completion
   browse-kill-ring
   change-inner
   coffee-mode
   color-theme-sanityinc-solarized
   color-theme-sanityinc-tomorrow
   css-eldoc
   dash
   dash-at-point
   diminish
   dired-details
   exec-path-from-shell
   expand-region
   feature-mode
   fill-column-indicator
   flx
   flx-ido
   flycheck
   fuzzy
   gist
   gitconfig-mode
   gitignore-mode
   god-mode
   graphviz-dot-mode
   guide-key
   haml-mode
   hardcore-mode
   haskell-mode
   helm
   helm-projectile
   highlight-escape-sequences
   ido-at-point
   ido-ubiquitous
   ido-vertical-mode
   inf-ruby
   js2-mode
   jump-char
   keyfreq
   ledger-mode
   magit
   markdown-mode
   mmm-mode
   move-text
   multifiles
   multiple-cursors
   org
   paredit
   projectile
   rainbow-mode
   robe
   rspec-mode
   rubocop
   ruby-compilation
   ruby-refactor
   ruby-tools
   scss-mode
   shell-command
   slim-mode
   smart-forward
   smex
   smooth-scrolling
   smartparens
   undo-tree
   use-package
   visual-regexp
   web-mode
   wgrep-ag
   whitespace-cleanup-mode
   window-number
   yaml-mode
   yari
   yasnippet
   zoom-frm))
