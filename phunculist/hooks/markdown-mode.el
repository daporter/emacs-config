;; show wrap guide and use soft wrap
(require 'fill-column-indicator)
(setq fci-rule-column phunculist-wrap-limit)
(fci-mode)

(setq fill-column phunculist-wrap-limit)
(longlines-mode)

(local-set-key (kbd "C-c v w") 'phunculist/toggle-wrap)

;; show junk whitespace
(whitespace-mode)

;; activate character pairing
(smartparens-mode)

(sp-with-modes (list major-mode)
  (sp-local-pair "`" "`"))

;; command shortcuts
(local-set-key (kbd "C-c y")   'phunculist/markdown-yank-as-pre)
(local-set-key (kbd "C-c o p") 'phunculist/markdown-preview)

;; limit the spell checking activated by text-mode
(setq flyspell-generic-check-word-predicate 'phunculist/markdown-mode-flyspell-verify)
