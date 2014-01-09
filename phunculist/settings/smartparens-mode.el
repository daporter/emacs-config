;; Don't clobber our global `backward-kill-word' binding.
(setq sp-smartparens-bindings
      (delete (assoc "M-<backspace>" sp-smartparens-bindings)
              sp-smartparens-bindings))

(sp-use-smartparens-bindings)

;; Set some more convenient keybindings.
(local-set-key (kbd "C-<backspace>") 'sp-backward-unwrap-sexp)
(local-set-key (kbd "M-<backspace>") 'sp-backward-kill-sexp)

;; Don't pair apostrophes.
(sp-pair "'" nil :unless '(sp-point-after-word-p))
