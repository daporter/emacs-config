;; Fix S-<up> in xterm.  Why:
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; Integrate with Mac OS X's pasteboard.  Inspried by:
;; https://gist.github.com/the-kenny/267162
(defvar phunculist-pbpaste-executable (executable-find "pbpaste")
  "The pbpaste executable")

(defvar phunculist-pbcopy-executable (executable-find "pbcopy")
  "The pbcopy executable")

(defun phunculist/pbpaste ()
  (shell-command-to-string phunculist-pbpaste-executable))

(defun phunculist/pbcopy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" phunculist-pbcopy-executable)))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function   'phunculist/pbcopy)
(setq interprogram-paste-function 'phunculist/pbpaste)
