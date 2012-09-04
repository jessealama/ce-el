
(eval-when-compile
  (require 'cl))

(require 'ispell)

(defun ce-spell-check ()
  (interactive)
  (ispell))

(defun ce-spell-change-dictionary (dictionary)
  (interactive "s")
  (let ((dicts (ispell-valid-dictionary-list)))
    (if (member dictionary dicts)
	(ispell-change-dictionary dictionary)
      (error "The dictionary '%s' is unknown." dictionary))))

(provide 'ce-spell)

;;; ce-spell.el ends here
