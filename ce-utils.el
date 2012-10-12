
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
;; Strings with cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cursored-string ()
  ((string :initarg :string
	   :initform ""
	   :type string
	   :documentation "The underlying string (sequence of characters).")
   (cursor :initarg :initial-position
	   :initform 0
	   :type integer
	   :documentation "The current position of the cursored string.")))

(defmethod cs-length ((cs cursored-string))
  (values (length (oref cs string))))

(defconst +cs-render-separator+ "|"
  "The string used to separate the characters of a rendered cursorsed-string.")

(defmethod cs-digitize ((cs cursored-string))
  (let ((string (copy-sequence (oref cs string)))
	(cursor-pos (oref cs cursor)))
    (with-output-to-string
      (loop
       initially (princ (format "%s" +cs-render-separator+))
       for char across string
       do
       (princ (format " %c %s" char +cs-render-separator+))))))

(defmethod cursor-in-digitized-string ((cs cursored-string))
  "What is the cursor in CS, were it digitized?

If cs-digitize changes, this function may need to be changed."
  (let ((cursor (oref cs cursor)))
    (if (zerop cursor)
	2
      (+ (* cursor 4) 3))))

(defmethod range-of-cursor-in-digitized-string ((cs cursored-string))
  "What would the range of the cursor of CS be if it were digitized?

The answer is a pair of integers.

If cs-digitize changes, then this function may need to be changed."
  (let ((cursor-in-digitized (cursor-in-digitized-string cs)))
    (cons (- cursor-in-digitized 2)
	  (+ cursor-in-digitized 1))))

(defmethod cs-render ((cs cursored-string))
  (let ((digitized (cs-digitize cs)))
    (let ((range (range-of-cursor-in-digitized-string cs)))
      (destructuring-bind (begin . end)
	  range
	(add-text-properties begin end (list 'face 'highlight) digitized)))
    digitized))

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
