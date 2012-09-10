
(require 'cl)
(require 'nxml-mode) ; for navigating among XML tokens

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

(defun ce-dash-next-dash ()
  (let ((begin nil)
	(end nil)
	(dash-begin nil)
	(dash-end nil)
	(dash-found nil))
    (let* ((begin (point-min))
	   (previous-xml-token nil)
	   (current-xml-token (xmltok-forward))
	   (end (point)))
      (while (and current-xml-token
		  (not dash-found))
	(let ((data (buffer-substring-no-properties begin end)))
	  (cond ((eq current-xml-token 'entity-ref)
		 (when (ce-dash-is-dash-entity data)
		   (setf dash-begin begin
			 dash-end end
			 dash-found t)))
		((and (eq previous-xml-token 'start-tag)
		      (eq current-xml-token 'data))
		 (let ((dash-position (position ?\- data)))
		   (when dash-position
		     (let ((local-end (string-match "[^-]" data dash-position)))
		       ;; (error "Data is '%s'%cdash-position is %d%cand the the local end is '%s'" data ?\n dash-position ?\n local-end)
		       (setf dash-begin (+ begin dash-position))
		       (setf dash-end (1- (+ begin local-end)))
		       (setf dash-found t)))))))
	(setf begin end
	      previous-xml-token current-xml-token
	      current-xml-token (xmltok-forward)
	      end (point))))
    (when dash-found
      (goto-char dash-begin)
      (list dash-begin dash-end))))

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
