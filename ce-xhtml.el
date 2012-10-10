
(require 'cl)
(require 'nxml-mode)

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
	(format "</%s>" name))
    (error
     (error "Cannot render closing tag for '%s'.  The error was:

%s" element (error-message-string structure-error)))))

(defun ce-xhtml-nxml-elementp (thing)
  (and (consp thing)
       (condition-case nil
	   (destructuring-bind (element attributes . children)
	       thing
	     t)
	 (error nil))))

(defun ce-xhtml-render-nxml-thing (thing)
  (cond ((stringp thing)
	 thing)
	((ce-xhtml-nxml-elementp thing)
	 (destructuring-bind (element attributes . children)
	     thing
	   (with-output-to-string
	     (princ (ce-xhtml-render-tag element attributes))
	     (dolist (child children)
	       (princ (ce-xhtml-render-nxml-thing child)))
	     (princ (ce-xhtml-render-closing-tag element)))))
	(t
	 (error "Unable to render the nXML thing

%s

because it is neither a string nor an nXML element." thing))))

(provide 'ce-xhtml)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-xhtml.el ends here
