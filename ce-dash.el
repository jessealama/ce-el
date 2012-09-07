
(require 'cl)
(require 'nxml-mode) ; for navigating among XML tokens

(defun ce-dash-first-dash-in-string (str)
  (let ((dashes-pos (string-match "[-]+[^-]" str))
	(ndash-pos (string-match "&ndash;" str))
	(mdash-pos (string-match "&mdash;" str)))
    (cond
     (dashes-pos
      (let ((dashes-end (string-match "[^-]" str dashes-pos)))
	(cond ((and ndash-pos mdash-pos)
	       (let ((min (min dashes-pos mdash-pos ndash-pos)))
		 (cond ((= min dashes-pos)
			(values dashes-pos dashes-end))
		       ((= min mdash-pos)
			(values mdash-pos (+ mdash-pos 7)))
		       (t
			(values ndash-pos (+ ndash-pos 7))))))
	      (mdash-pos
	       (if (< dashes-pos mdash-pos)
		   (values dashes-pos dashes-end)
		 (values mdash-pos (+ mdash-pos 7))))
	      (ndash-pos
	       (if (< dashes-pos ndash-pos)
		   (values dashes-pos dashes-end)
		 (values ndash-pos (+ ndash-pos 7))))
	      (t
	       (values dashes-pos dashes-end)))))
     (ndash-pos
      (if mdash-pos
	  (let ((first (min mdash-pos ndash-pos)))
	    (values first (+ first 7)))
	(values mdash-pos (+ mdash-pos 7))))
     (mdash-pos
      (values mdash-pos (+ mdash-pos 7)))
     (t (values nil nil)))))

(defun ce-dash-next-dash ()
  (let ((begin nil)
	(end nil))
    (save-excursion
      (goto-char (point-min))
      (let* ((begin (point-min))
	     (previous-xml-token nil)
	     (current-xml-token (xmltok-forward))
	     (end (point)))
	(while current-xml-token
	  (when (and (eq previous-xml-token 'start-tag)
		     (eq current-xml-token 'data))
	    (let ((data (buffer-substring-no-properties begin end)))
	      (multiple-value-bind (dash-begin dash-end)
		  (ce-dash-first-dash-in-string data)
		(when (and dash-begin dash-end)
		  (setf begin dash-begin
			end dash-end
			current-xml-token nil)))))
	  (setf begin end
		end (point)
		previous-xml-token current-xml-token
		current-xml-token (xmltok-forward)))))
    (when (and begin end)
      (list begin end))))

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
