;;;###autoload
(defun phunculist/string-trim (str)
  "Trim whitespace from both ends of the passed string."
  (phunculist/regex-replace (phunculist/regex-replace str "[ \t]+\\'" "" t t)
                            "\\`[ \t]+" "" t t))

;;;###autoload
(defun phunculist/camelize (str)
  "Forces a string into CamelCase."
  (mapconcat (lambda (s)
               (if (string-match "[aeiouy]" s)
                   (capitalize s)
                 (upcase s)))
             (split-string str "[^A-Za-z0-9]")
             ""))
