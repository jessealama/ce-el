
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings with cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cursored-string ()
  ((string :initarg :string
	   :initform (error "To create a cursored-string, a string must be provided.")
	   :type string
	   :documentation "The underlying string (sequence of characters).")
   (cursor :initarg :initial-position
	   :initform 0
	   :type integer
	   :documentation "The current position of the cursored string.")))

(defmethod cs-length ((cs cursored-string))
  (length (oref cs string)))

(defconst +cs-render-separator+ "|"
  "The string used to separate the characters of a rendered cursorsed-string.")

(defmethod cs-discretize ((cs cursored-string))
  (let ((string (copy-sequence (oref cs string)))
	(cursor-pos (oref cs cursor)))
    (with-output-to-string
      (loop
       initially (princ (format "%s" +cs-render-separator+))
       for char across string
       do
       (princ (format " %c %s") char +cs-render-separator+)))))

(defmethod range-of-cursor-in-digitized-string ((cs cursored-string))
  (let ((cursor (ce-dash-position-in-digitized-string string position)))
    (cons (- position-in-digitized 2)
	  (+ position-in-digitized 1))))

(defmethod cs-render ((cs cursored-string))
  (let ((cursor-pos (oref cs cursor)))
    (let ((discretized (cs-discretize cs))))))

(provide 'ce-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-utils.el ends here
