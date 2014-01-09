(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")

;; (use-package auto-complete-config
;;   :diminish auto-complete-mode

;;   :config (progn
;;             (use-package popup)
;;             (use-package fuzzy)

;;             (setq ac-use-menu-map          t
;;                   ac-user-dictionary-files (concat user-data-directory "dict"))

;;             (ac-config-default)
;;             (ac-set-trigger-key "TAB")))

(provide 'setup-auto-complete)
