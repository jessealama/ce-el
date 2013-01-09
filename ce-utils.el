
(eval-when-compile
  (require 'cl)
  (require 'nxml-mode)
  (require 'ce-macros))

(declare-function some "cl-seq.el")
(declare-function subseq "cl-extra.el")
(declare-function substitute "cl-seq.el")

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

(defun entirely-whitespace (string)
  (not (null (string-match "^[[:space:]]*$" string))))

(defun position-of-substring-in-string (string substring)
  (let ((substring-len (length substring)))
    (loop
     for i from 0 upto (1- (length string))
     for window = (substring string i (+ i substring-len))
     when (string= window substring) do (return i)
     finally (return nil))))

(defun position-of-substring-in-string-from-end (string substring)
  (let ((substring-len (length substring))
	(len (length string)))
    (loop
     for i from (1- len) downto 0
     for window = (substring string i (min len (+ i substring-len)))
     when (string= window substring) do (return i)
     finally (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-n (list n)
  (loop
   for i from 1 upto n
   for elt in list
   collect elt into answer
   finally (return answer)))

(provide 'ce-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-utils.el ends here
