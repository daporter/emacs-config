;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-packages.html

(require 'package)

(dolist (repo '(("elpa"      . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

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

(defun phunculist/package-install-and-remove-to-match-list (&rest packages)
  "Sync packages so the installed list matches the passed list."
  (package-initialize)
  (condition-case nil ;; added to handle no-network situations
      (mapc 'phunculist/package-install-unless-installed packages)
    (error (message "Couldn't install package. No network connection?")))
  (phunculist/package-delete-unless-listed packages))

(phunculist/package-install-and-remove-to-match-list
 'ag
 'auctex
 'auto-complete
 'autopair
 'coffee-mode
 'color-theme-sanityinc-tomorrow
 'dash
 'exec-path-from-shell
 'expand-region
 'fill-column-indicator
 'flx
 'flycheck
 'fuzzy
 'graphviz-dot-mode
 'haml-mode
 'haskell-mode
 'helm
 'helm-c-yasnippet
 'helm-git
 'helm-projectile
 'inf-ruby
 'js2-mode
 'jump-char
 'magit
 'markdown-mode
 'mmm-mode
 'multiple-cursors
 'paredit
 'projectile
 'rspec-mode
 'ruby-compilation
 'ruby-refactor
 'scss-mode
 'slim-mode
 'smartparens
 'undo-tree
 'web-mode
 'window-number
 'yaml-mode
 'yari
 'yasnippet)
