
(require 'cl)
(require 'nxml-mode)
(require 'ce-entities)
(require 'ce-utils)

(declare-function remove-if "cl-seq.el")
(declare-function remove-if-not "cl-seq.el")
(declare-function reduce "cl-seq.el")

(defun ce-xhtml-render-attribute (attribute)
  (cond ((stringp attribute)
	 attribute)
	((consp attribute)
	 (condition-case nil
	     (destructuring-bind (namespace . name)
		 attribute
	       ;; ignore namespace -- this ought to always be the XHTML namespace
	       name)
	   (error
	    (error "The attribute '%s' does not have the expected shape of a cons cell." attribute))))
	(t
	 (error "Unable to handle the attribute '%s'" attribute))))

(defun ce-xhtml-render-attribute-and-value (attribute-value-pair)
  (condition-case nil
      (destructuring-bind (attribute . value)
	  attribute-value-pair
	(format "%s=\"%s\"" (ce-xhtml-render-attribute attribute) (ce-entities-name-unicode-characters (ce-xhtml-escape-string value))))
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

(defconst +xhtml-1.0-transitional-doctype+ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")

(defun ce-xhtml-render-nxml-thing (thing)
  (cond ((stringp thing)
	 (ce-entities-name-unicode-characters (ce-xhtml-escape-string thing)))
	((ce-xhtml-nxml-elementp thing)
	 (destructuring-bind (element attributes . children)
	     thing
	   (with-output-to-string
	     (destructuring-bind (namespace . name)
		 element
	       (when (string= name "html")
		 (princ +xhtml-1.0-transitional-doctype+)
		 (terpri))
	       (cond ((and (string= (cdr element) "p")
			 (find (cons "class" "comment") attributes :test 'equalp)
			 children
			 (stringp (first children))
			 (not (rest children)))
		    (princ (format "<!--%s-->" (first children))))
		   (t
		    (princ "<")
		    (princ name)
		    (dolist (attribute attributes)
		      (princ " ")
		      (princ (ce-xhtml-render-attribute-and-value attribute)))
		    (cond (children
			   (princ ">")
			   ;; (princ (ce-xhtml-render-tag element attributes))
			   (dolist (child children)
			     (princ (ce-xhtml-render-nxml-thing child)))
			   (princ (ce-xhtml-render-closing-tag element)))
			  (t
			   (princ "/>")))))))))
	(t
	 (error "Unable to render the nXML thing

%s

because it is neither a string nor an nXML element." thing))))

(defun ce-xhtml-map-cdata-sections (nxml-thing function)
  (cond ((stringp nxml-thing)
	 (funcall function nxml-thing))
	((ce-xhtml-nxml-elementp nxml-thing)
	 (destructuring-bind (element attributes . children)
	     nxml-thing
	   (append (list element attributes)
		   (mapcar (lambda (child)
			     (ce-xhtml-map-cdata-sections child function))
			   children))))
	(t
	 (error "Unable to render the nXML thing

%s

because it is neither a string nor an nXML element." nxml-thing))))

(defun ce-xhtml-replace-thing-at-address (tree address thing)
  (when (null thing)
    (error "The replacement object should not be null."))
  (when (and (not (stringp thing))
	     (not (listp thing)))
    (error "The replacement object should be either a string or an nXML node."))
  (if (null address)
      thing
    (let ((element nil)
	  (attributes nil)
	  (children nil))
      (condition-case nil
	  (destructuring-bind (local-element local-attributes . local-children)
	      tree
	    (setf element local-element
		  attributes local-attributes
		  children local-children))
	(error
	 (error "Unable to make sense of the nXML node '%s'" tree)))
      (let ((num-children (length children))
	    (child-number (first address)))
	(when (< num-children child-number)
	  (error "The current node has only %d children, but the address indicates that we are supposed to inspect child number %d." num-children child-number))
	(when (< child-number 1)
	  (error "The first component of the current address is less than 1."))
	(let ((previous-children (first-n children (1- child-number)))
	      (child (nth (1- child-number) children))
	      (following-children (nthcdr child-number children)))
	  (append (list element attributes)
		  previous-children
		  (list (ce-xhtml-replace-thing-at-address child (rest address) thing))
		  following-children))))))

(defun ce-xhtml-count-leaves (nxml-thing)
  (if (stringp nxml-thing)
      1
    (let ((element nil)
	  (attributes nil)
	  (children nil))
      (condition-case nil
	  (destructuring-bind (local-element local-attributes . local-children)
	      nxml-thing
	    (setf element local-element
		  attributes local-attributes
		  children local-children))
	(error
	 (error "Unable to make sense of the nXML node '%s'" nxml-thing)))
      (let ((string-children (remove-if-not 'stringp children))
	    (element-children (remove-if 'stringp children)))
	(+ (length string-children)
	   (reduce '+ (mapcar 'ce-xhtml-count-leaves element-children)))))))

(defun ce-xhtml-nth-leaf (nxml-tree n)
  "In a depth-first enumeration of the leaves of NXML-TREE, what
  is leaf N?  We start counting at 1, not 0."
  (when (< n 1)
    (error "Illegal value '%d'; we cannot find the leaf with that index.  (We start counting at 1, not 0.)" n))
  (if (stringp nxml-tree)
      (if (= n 1)
	  nxml-tree
	(error "A string nXML node has zero children, so we cannot compute leaf number %d.  The string we are looking at is%c%c%s%c" n ?\n ?\n nxml-tree ?\n))
    (let ((element nil)
	  (attributes nil)
	  (children nil))
      (condition-case nil
	  (destructuring-bind (local-element local-attributes . local-children)
	      nxml-tree
	    (setf element local-element
		  attributes local-attributes
		  children local-children))
	(error
	 (error "Unable to make sense of the nXML node '%s'" nxml-tree)))
      (ce-xhtml-nth-leaf-under-nodes children n))))

(defun ce-xhtml-nth-leaf-under-nodes (nodes n)
  "In a depth-first enumeration of the list of nXML nodes NODES, what is leaf N?

We start counting at 1, not 0."
  (when (< n 1)
    (error "Illegal value '%d'; we cannot find the leaf with that index.  (We start counting from 1, not 0.)" n))
  (when (null nodes)
    (error "The list of nodes must be non-empty, and we were asked to compute node number #%d under this list of nodes." n))
  (let ((node (first nodes)))
    (let ((num-leaves (ce-xhtml-count-leaves node)))
      (if (< num-leaves n)
	  (ce-xhtml-nth-leaf-under-nodes (rest nodes) (- n num-leaves))
	(ce-xhtml-nth-leaf node n)))))

(defun ce-xhtml-leaf-number-of-leaf-address (tree address &optional encountered)
  "Assuming that ADDRESS is the address of a leaf of the nXML
  tree TREE, what is the index of that leaf in a depth-first
  enumeration of the leaves of TREE?

We start counting at 1, not 0."
  (when (null encountered)
    (setf encountered 0))
  (when (null address)
    (error "A null address is not permitted."))
  (when (null tree)
    (error "A null tree is not permitted."))
  (when (stringp tree)
    (error "We assume we are dealing with trees, not with strings."))
  (let ((first-address-component (first address)))
    (let ((element nil)
	  (attributes nil)
	  (children nil))
      (condition-case nil
	  (destructuring-bind (local-element local-attributes . local-children)
	      tree
	    (setf element local-element
		  attributes local-attributes
		  children local-children))
	(error
	 (error "Unable to make sense of the nXML node '%s'" tree)))
      (let ((earlier-children (first-n children (1- first-address-component)))
	    (child (nth (1- first-address-component) children)))
	(let ((earlier-leaves (remove-if-not 'stringp earlier-children))
	      (earlier-elements (remove-if 'stringp earlier-children)))
	  (let ((earlier-leaf-count (+ (length earlier-leaves)
				       (reduce '+ (mapcar 'ce-xhtml-count-leaves
							  earlier-elements)))))
	    (if (rest address)
		(ce-xhtml-leaf-number-of-leaf-address child
						      (rest address)
						      (+ encountered earlier-leaf-count))
	      (1+ (+ earlier-leaf-count encountered)))))))))

(defun ce-xhtml-node-with-address (node address)
  (if address
      (let ((first-address-component (first address))
	    (remaining-address (rest address)))
	(assert (integerp first-address-component))
	(assert (> first-address-component 0))
	(if (stringp node)
	    (if remaining-address
		(error "The child at position %d of the current nXML tree is a string, but the address we were asked to inspect %s presumes that this child is a cons cell, not a string." first-address-component address)
	      node)
	  (let ((element nil)
		(attributes nil)
		(children nil))
	    (condition-case nil
		(destructuring-bind (local-element local-attributes . local-children)
		    node
		  (setf element local-element
			attributes local-attributes
			children local-children))
	      (error
	       (error "Unable to make sense of the nXML node '%s'" node)))
	    (let ((num-children (length children)))
	  (assert (<= first-address-component num-children))
	  (let ((node (nth (1- first-address-component) children)))
	    (ce-xhtml-node-with-address node remaining-address))))))
    node))

(defun ce-xhtml-address-of-leaf-number-1 (tree leaf-number address-so-far)
  (unless (integerp leaf-number)
    (error "We cannot find leaf number '%s' because that is not an integer." leaf-number))
  (when (< leaf-number 0)
    (error "We cannot find leaf number '%d', because that is less than zero.  (We start counting at 1, not 0.)" leaf-number))
  (if (stringp tree)
      (if (= leaf-number 1)
	  address-so-far
	(error "We have arrived at a leaf (%s), but the leaf-number to inspect (%d) is not equal to 1." tree leaf-number))
    (let ((element nil)
	  (attributes nil)
	  (children nil))
      (condition-case nil
	  (destructuring-bind (local-element local-attributes . local-children)
	      tree
	    (setf element local-element
		  attributes local-attributes
		  children local-children))
	(error
	 (error "Unable to make sense of the nXML node '%s'" tree)))
      (when (null children)
	(error "We have arrived at a node that has no children, but this is impossible."))
      (let ((leaves-so-far 0)
	    (child-number 0)
	    (child (first children)))
	(while (< leaves-so-far leaf-number)
	  (incf leaves-so-far (if (stringp child) 1 (ce-xhtml-count-leaves child)))
	  (incf child-number)
	  (setf child (nth child-number children)))
	(setf child (nth (1- child-number) children))
	(decf leaves-so-far (ce-xhtml-count-leaves child))
	(if (= leaves-so-far leaf-number)
	    (cons child-number address-so-far)
	  (ce-xhtml-address-of-leaf-number-1 child
					     (- leaf-number leaves-so-far)
					     (cons child-number address-so-far)))))))

(defun ce-xhtml-address-of-leaf-number (tree leaf-number)
  (reverse (ce-xhtml-address-of-leaf-number-1 tree leaf-number nil)))

(defun ce-xhtml-first-leaf-satisfying (tree predicate &optional start end)
  (let ((num-leaves (ce-xhtml-count-leaves tree)))
    (when (null start)
      (setf start 1))
    (when (null end)
      (setf end num-leaves))
    (loop
     for i from start upto end
     for leaf = (ce-xhtml-nth-leaf tree i)
     do
     (when (funcall predicate leaf)
       (return leaf))
     finally
     (return nil))))

(defun ce-xhtml-index-of-first-leaf-satisfying (tree predicate &optional start end)
  (let ((num-leaves (ce-xhtml-count-leaves tree)))
    (when (null start)
      (setf start 1))
    (when (null end)
      (setf end num-leaves))
    (loop
     for i from start upto end
     for leaf = (ce-xhtml-nth-leaf tree i)
     do
     (when (funcall predicate leaf)
       (return i))
     finally
     (return nil))))

(defun ce-xhtml-comments-as-paragraphs ()
  "Replace all named XHTML entities by their decimal character reference equivalents."
  (interactive)
  (ensure-nxml-mode)
  (foreach-xml-token
   (when (eq current-xml-token 'comment)
     (let ((data (buffer-substring-no-properties begin end)))
       (cond ((string-match "^<!--\\\(.*\\\)-->$" data)
	      (let ((comment (match-string-no-properties 1 data)))
		(delete-region begin end)
		(insert (format "<p class=\"comment\">%s</p>" comment))
		(setf end (point))))
	     (t
	      (error "We found a comment that does not match our comment regular expression.")))))))

(provide 'ce-xhtml)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-xhtml.el ends here
