
(require 'cl)
(require 'ispell)

(defun ce-spell-check ()
  (interactive)
  (ispell))

(defun ce-spell-change-dictionary ()
  (interactive)
  (ispell-change-dictionary))

;;; ce-spell.el ends here
