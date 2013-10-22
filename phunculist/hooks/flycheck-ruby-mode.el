;; show wrap guide
(require 'fill-column-indicator)
(setq fci-rule-column phunculist-wrap-limit)
(fci-mode)

(local-set-key (kbd "C-c v w") 'fci-mode)

;; show junk whitespace
(whitespace-mode)

;; activate character pairing
(smartparens-mode)

(sp-with-modes (list major-mode)
  (sp-local-pair "|" "|"))

;; do some spell checking (requires:  brew install aspell --lang=en)
(flyspell-prog-mode)

(local-set-key (kbd "C-c v s") 'phunculist/toggle-program-spelling)

;; command shortcuts
(local-set-key (kbd "C-c =") 'phunculist/align=)

(local-set-key (kbd "C-c t s") 'phunculist/toggle-string-type)

(local-set-key (kbd "C-c t t") 'phunculist/toggle-ruby-test)
(local-set-key (kbd "C-c t b") 'phunculist/toggle-ruby-block-type)
(local-set-key (kbd "C-c t r") 'phunculist/toggle-ruby-regex-type)
(local-set-key (kbd "C-c t S") 'phunculist/toggle-ruby-string-and-symbol)
(local-set-key (kbd "C-c t h") 'phunculist/toggle-ruby-hash-type)
