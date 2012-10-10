
(require 'cl)
(require 'nxml-mode)
(require 'eieio)

(defmethod ce-xhtml-render-attribute ((attribute string))
  attribute)

(defmethod ce-xhtml-render-attribute ((attribute cons))
  (destructuring-bind (namespace . name)
      attribute
    (unless (string= (symbol-name namespace) ":http://www.w3.org/1999/xhtml")
      (error "Non-XHTML namespace (%s) in an nXML node

%s

The only namespace we expect is \"http://www.w3.org/1999/xhtml\"." (symbol-name namespace) attribute))
    (unless (stringp name)
      (error "A non-string appears as the cdr of a cons cell

%s ,

We expect to be an nXML attribute element; the cdr should be a string." attribute))
    name))

;; If we haven't covered ce-xhtml-render-attribute by another method,
;; then we have been given an argument that we don't handle at all.
(defmethod ce-xhtml-render-attribute ((attribute t))
  (error "Unable to handle the attribute

%s

because we do not know how to handle such objects." attribute))

(defun ce-xhtml-render-attribute-and-value (attribute-value-pair)
  (condition-case nil
      (destructuring-bind (attribute . value)
	  attribute-value-pair
	(format "%s=\"%s\"" (ce-xhtml-render-attribute attribute) (ce-xhtml-escape-string value)))
    (error
     (error "Unable to render the attribute-value pair

%s

because it does not have the expected shape (it is not a cons cell)." attribute-value-pair))))

(defun ce-xhtml-render-tag (element attributes)
  (condition-case structure-error
      (destructuring-bind (namespace . name)
	  element
	(unless (string= (symbol-name namespace)
			 ":http://www.w3.org/1999/xhtml")
	  (error "Non-XHTML namespace (%s) in an nXML node

%s

The only namespace we expect is \"http://www.w3.org/1999/xhtml\"." (symbol-name namespace) element))
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

(defun ce-xhtml-escape-string (string)
  "Escape ampersands and less-than symbols in STRING."
  (with-output-to-string
    (loop
     with len = (length string)
     for i from 1
     for c across string
     do
     (cond ((char-equal c ?<)
	    (princ "&lt;"))
	   ((char-equal c ?>)
	    (princ "&gt;"))
	   ((char-equal c ?&)
	    (if (<= (+ i 4) len)
		(let ((next-four-characters (substring string i (+ i 4))))
		  (if (string= next-four-characters "amp;")
		      (princ "&")
		    (princ "&amp;")))
	      (princ "&amp;")))
	   (t
	    (princ (format "%c" c)))))))

(defun ce-xhtml-render-nxml-thing (thing)
  (cond ((stringp thing)
	 (ce-xhtml-escape-string thing))
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
