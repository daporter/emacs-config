;;;###autoload
(defun phunculist/flatten (x)
  "Flatten a list."
  (cond ((null x) nil)
        ((listp x) (append (phunculist/flatten (car x))
                           (phunculist/flatten (cdr x))))
        (t (list x))))
