
(require 'cl)
(require 'nxml-mode)
(require 'ce-xhtml)
(require 'ce-entities)

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

(defun ce-dash-inspect-string (string)
  string)

(defun ce-dash-inspect-nxml-thing (thing)
  (cond ((stringp thing) (ce-dash-inspect-string thing))
	((null thing) nil)
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
		 (error "Unable to parse the current buffer as XML:%c%c%s" ?\n ?\n (error-message-string nxml-parse-error))))))
    (let ((new-tree (ce-dash-inspect-nxml-thing tree)))
      (erase-buffer)
      (insert (ce-xhtml-render-nxml-thing new-tree)))))

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
