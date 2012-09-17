
(require 'cl)
(require 'nxml-mode)

(defun ce-xhtml-character-blocks-of (thing)
  (cond ((null thing) nil)
	((symbolp thing) nil)
	((stringp thing) (list thing))
	((consp thing)
	 (condition-case nil
	     (destructuring-bind (element attributes . children)
		 thing
	       (reduce 'append (mapcar 'ce-xhtml-character-blocks-of children)))
	   (error
	    (error "Don't know how to extract the character blocks of '%s'" thing))))
	(t
	 (error "Don't know how to extract the character blocks of '%s'" thing))))

(defun ce-xhtml-character-data-blocks ()
  (let ((file (buffer-file-name)))
    (unless file
      (error "No file is associated with the current buffer."))
    (let ((tree (condition-case nxml-parse-error
		    (nxml-parse-file file)
		  (error "Unable to parse %s.%cThe error was:%c%c%s" file ?\n ?\n ?\n (error-message-string nxml-parse-error)))))
      (ce-xhtml-character-blocks-of tree))))

(provide 'ce-xhtml)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-xhtml.el ends here
