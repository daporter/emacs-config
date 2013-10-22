;; restrict spelling
(setq phunculist-spelling-skipped-markdown-faces
      '(markdown-pre-face markdown-reference-face markdown-url-face))

(defun phunculist/markdown-mode-flyspell-verify ()
  "An override used to shut Flyspell of in some parts of Markdown."
  (let ((face (get-text-property (point) 'face)))
    (not (memq face phunculist-spelling-skipped-markdown-faces))))
