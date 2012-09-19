
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
     (error "Cannot render tag

%s

with attributes

  %s

The error was:

%s" element attributes (error-message-string structure-error)))))

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
     (error "Cannot render closing tag for '%s'.  The error was:

%s" element (error-message-string structure-error)))))

(defun ce-xhtml-render-nxml-thing (thing)
  (cond ((null thing)
	 "")
	((stringp thing)
	 thing)
	((consp thing)
	 (condition-case structure-error
	     (destructuring-bind (element attributes . children)
		 thing
	       (with-output-to-string
		 (princ (ce-xhtml-render-tag element attributes))
		 (dolist (child children)
		   (princ (ce-xhtml-render-nxml-thing child)))
		 (princ (ce-xhtml-render-closing-tag element))))
	   (error
	    (error "Unable to make sense of the nXML thing

%s

cas an nXML element.

The error is:

%s" thing (error-message-string structure-error)))))
	(t
	 (error "Unable to handle the nXML thing

%s" thing))))

(provide 'ce-xhtml)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-xhtml.el ends here
