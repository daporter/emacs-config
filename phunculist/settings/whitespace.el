;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width        4)
(setq         tab-stop-list    (number-sequence 4 120 4))

(require 'whitespace)
(setq whitespace-style '(empty face tabs trailing))

(global-set-key (kbd "C-c n") 'phunculist/cleanup-buffer)
