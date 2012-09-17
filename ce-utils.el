
(require 'cl)
(require 'nxml-mode)
(require 'ce-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun member-of-some-array (thing list-of-arrays)
  "Does THING belong to any of the arrays in LIST-OF-ARRAYS?"
  (some #'(lambda (entity-array)
	    (or (string= thing (aref entity-array 0))
		(string= thing (aref entity-array 1))
		(string= thing (aref entity-array 2))))
	list-of-arrays))

(provide 'ce-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-utils.el ends here
