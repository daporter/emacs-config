((ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode :bind
                       ("S-<return>" . ace-jump-mode)
                       :symbol ace-jump-mode))
 (zenburn-theme status "installed" recipe
                (:name zenburn-theme :description "Zenburn theme for Emacs" :type http :url "https://raw.github.com/djcb/elisp/master/themes/zenburn-theme.el" :post-init
                       (add-to-list 'custom-theme-load-path default-directory))))
