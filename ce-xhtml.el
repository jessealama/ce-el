
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

(defun ce-xhtml-render-attribute (attribute)
  (cond ((stringp attribute) attribute)
	((consp attribute)
	 (condition-case nil
	     (destructuring-bind (namespace . name)
		 attribute
	       ;; ignore namespace
	       name)
	   (error
	    (error "The attribute '%s' does not have the expected shape of a cons cell." attribute))))
	(t
	 (error "Unable to handle the attribute '%s'" attribute))))

(defun ce-xhtml-render-attribute-and-value (attribute-value-pair)
  (condition-case nil
      (destructuring-bind (attribute . value)
	  attribute-value-pair
	(format "%s=%c%s%c" (ce-xhtml-render-attribute attribute) ?\" value ?\"))
    (error
     (error "Unable to render the attribute-value pair '%s'" attribute-value-pair))))

(defun ce-xhtml-render-tag (element attributes)
  (condition-case structure-error
      (destructuring-bind (namespace . name)
	  element
	(unless (string= (symbol-name namespace)
			 ":http://www.w3.org/1999/xhtml")
	  (error "Non-XHTML namespace in an nXML node: '%s'" (symbol-name namespace)))
	(with-output-to-string
	  (princ "<")
	  (princ name)
	  (dolist (attribute attributes)
	    (princ " ")
	    (princ (ce-xhtml-render-attribute-and-value attribute)))
	  (princ ">")))
    (error
     (error "Cannot render tag '%s' with attributes '%s':%c%c%s" element attributes ?\n ?\n (error-message-string structure-error)))))

(defun ce-xhtml-render-closing-tag (element)
  (condition-case structure-error
      (destructuring-bind (namespace . name)
	  element
	(unless (string= (symbol-name namespace)
			 ":http://www.w3.org/1999/xhtml")
	  (error "Non-XHTML namespace in an nXML node: '%s'" (symbol-name namespace)))
	(with-output-to-string
	  (princ "</")
	  (princ name)
	  (princ ">")))
    (error
     (error "Cannot render closing tag for '%s':%c%c%s" element ?\n ?\n (error-message-string structure-error)))))

(defun ce-xhtml-render-nxml-thing (thing)
  (cond ((null thing)
	 "")
	((symbolp thing)
	 (error "Don't know how to handle a symbol '%s'" thing))
	((stringp thing)
	 thing)
	((consp thing)
	 (condition-case structure-error
	     (destructuring-bind (element attributes . children)
		 thing
	       (message "there are %d children" (length children))
	       (with-output-to-string
		 (princ (ce-xhtml-render-tag element attributes))
		 (dolist (child children)
		   (princ (ce-xhtml-render-nxml-thing child)))
		 (princ (ce-xhtml-render-closing-tag element))))
	   (error
	    (error "Unable to make sense of the nXML thing%c%c%s%c%cas an nXML element.%c%cThe error is:%c%s" ?\n ?\n thing ?\n ?\n ?\n ?\n ?\n (error-message-string structure-error)))))
	(t
	 (error "Unable to handle nXML thing%c%c'%s'" ?\n ?\n thing))))

(provide 'ce-xhtml)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-xhtml.el ends here
