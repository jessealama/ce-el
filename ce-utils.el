
(require 'cl)
(require 'nxml-mode)
(require 'ce-macros)
(require 'eieio)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ce-dash-squeeze-spaces (string)
  (let ((space-space-position (string-match "[[:space:]][[:space:]]" string)))
    (if space-space-position
	(concat (subseq string 0 (1- space-space-position))
		(ce-dash-squeeze-spaces (subseq string (1+ space-space-position))))
      string)))

(defun ce-dash-first-non-space-position (string)
  (string-match "[^[:space:]]" string))

(defun ce-dash-kill-initial-whitespace (string)
  (let ((first-non-space-position (ce-dash-first-non-space-position string)))
    (if first-non-space-position
	(subseq string first-non-space-position)
      "")))

(defun ce-dash-nuke-whitespace (string)
  (let ((no-initial-whitespace (ce-dash-kill-initial-whitespace string)))
    (let ((no-newlines (substitute ?\s ?\n no-initial-whitespace)))
      (let ((no-tabs (substitute ?\s ?\t no-newlines)))
	(ce-dash-squeeze-spaces no-tabs)))))

(provide 'ce-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-utils.el ends here
