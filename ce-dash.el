
(require 'cl)
(require 'nxml-mode)
(require 'ce-xhtml)
(require 'ce-entities)

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

(defconst *ce-dash-dashes* (list "-" "–" "—"))

(defconst *ce-dash-dash-regexp* (regexp-opt *ce-dash-dashes*))

(defun ce-dash-discretize-string (string)
  (let ((len (length string)))
    (cond ((= len 0) "")
	  ((= len 1) string)
	  (t
	   (with-output-to-string
	     (loop
	      initially (princ "| ")
	      for i from 0 upto (- len 2)
	      for char across string do (princ (format "%c | " char))
	      finally (princ (format "%c |" (aref string (- len 1))))))))))

(defun ce-dash-position-in-digitized-string (string position)
  (let ((len (length string)))
    (unless (and (<= 0 position)
		 (< position len))
      (error "Cannot look at position %d of the string '%s' because the string has only %d characters." position string len))
    (if (zerop position)
	2
      (+ (* position 4) 3))))

(defun ce-dash-range-of-position-in-digitized-string (string position)
  (let ((len (length string)))
    (unless (and (<= 0 position)
		 (< position len))
      (error "Cannot look at position %d of the string '%s' because the string has only %d characters." position string len))
    (let ((position-in-digitized (ce-dash-position-in-digitized-string string position)))
      (cons (- position-in-digitized 2)
	    (+ position-in-digitized 1)))))

(defun ce-dash-inspect-string (string)
  (let ((dash-position (string-match *ce-dash-dash-regexp* string)))
    (when dash-position
      (let ((dash-editor-buf (get-buffer-create "*Dash Editor*"))
	    (digitized (ce-dash-discretize-string string))
	    (digitized-cursor (ce-dash-position-in-digitized-string string dash-position)))
	(put-text-property dash-position dash-position 'face 'bold digitized)
	(let ((range (ce-dash-range-of-position-in-digitized-string string dash-position)))
	    (destructuring-bind (begin . end)
		range
	      (add-text-properties begin end (list 'face 'highlight) digitized)))
	(with-current-buffer dash-editor-buf
	  (erase-buffer)
	  (insert digitized)
	  (goto-char digitized-cursor)))))
  string)

(defun ce-dash-inspect-nxml-thing (thing)
  (cond ((stringp thing)
	 (ce-dash-inspect-string thing))
	((null thing)
	 nil)
	((consp thing)
	 (let ((element nil)
	       (attributes nil)
	       (children nil))
	   (condition-case nil
	       (destructuring-bind (local-element local-attributes . local-children)
		   thing
		 (setf element local-element
		       attributes local-attributes
		       children local-children))
	     (error
	      (error "Unable to make sense of the node '%s'" thing)))
	   (append (list element attributes)
		   (mapcar 'ce-dash-inspect-nxml-thing children))))
	(t
	 (error "Don't know how to make sense of the nxml object '%s'" thing))))

(defun ce-dash-inspect-dashes ()
  (interactive)
  (ce-entities-resolve-named-entities-decimally)
  (save-buffer)
  (let ((tree (condition-case nxml-parse-error
		  (nxml-parse-file (buffer-file-name))
		(error
		 (error "Unable to parse the current buffer as XML:

%s" (error-message-string nxml-parse-error))))))
    (let ((new-tree (ce-dash-inspect-nxml-thing tree)))
      (erase-buffer)
      (insert (ce-xhtml-render-nxml-thing new-tree)))))

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
