;;; ce-quotes.el --- The variety of quotes


;;; Commentary:
;;
;; This file contains Emacs Lisp functions for working with quotes in
;; HTML files.  See 'ce-quote-fix-quotes for a list of the current
;; functionality we support.

(require 'cl)

;;; Code:

(defun ce-quotes-inspect-quotes-in-string (string)
  (loop
   with new-string = (copy-seq string)
   with len = (length string)
   with offset = 0
   for dquote-1 = (position ?\" new-string :start offset)
   for dquote-2 = (position ?\" new-string :start (or dquote-1 len))
   when (and (integerp dquote-1) (integerp dquote-2))
   do (setf (aref new-string dquote-1) ?\“
	    (aref new-string dquote-2) ?\”
	    offset dquote-2)
   when (or (not (integerp dquote-1))
	    (not (integerp dquote-2))) do (return new-string)))

(defun ce-quotes-inspect-quotes ()
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "The buffer has been modified since it was last saved.  Save before continuing? ")
      (save-buffer)))
  (let* ((temp-file (make-temp-file "quote-editor-"))
	 (current-contents (buffer-string))
	 (new-contents current-contents)
	 (point (point))
	 (error-message nil))
    (unwind-protect
	(progn
	  (with-temp-file temp-file
	    (insert current-contents)
	    (ce-entities-resolve-named-entities-decimally)
	    (ce-xhtml-comments-as-paragraphs))
	  (let ((tree (condition-case nxml-parse-error
			  (nxml-parse-file temp-file)
			(error
			 (setf error-message
			       (error-message-string nxml-parse-error))
			 nil))))
	    (if tree
		(let ((new-tree (ce-xhtml-map-cdata-sections tree
							     'ce-quotes-inspect-quotes-in-string)))
		  (setf new-contents (ce-xhtml-render-nxml-thing new-tree))
		  (erase-buffer)
		  (insert new-contents)
		  (goto-char point))
	      (error "Buffer is not valid XML:%c%c%s" ?\n ?\n error-message))))
      (when (file-exists-p temp-file)
	(delete-file temp-file)))))

(provide 'ce-quotes)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-quotes.el ends here
