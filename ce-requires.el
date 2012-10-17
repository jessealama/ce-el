(require 'cl)

(defun file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun read-from-elisp-file (path)
  (car (read-from-string (format "(%s)" (file-contents path)))))

(defun require-form-p (form)
  (condition-case nil
      (destructuring-bind (require-symbol (quote-symbol package-symbol))
	  form
	(when (symbolp require-symbol)
	  (when (string= require-symbol "require")
	    (when (symbolp quote-symbol)
	      (when (string= quote-symbol "quote")
		(when (symbolp package-symbol)
		  package-symbol))))))
    (error nil)))

(defun require-forms (form)
  (cond ((null form)
	 nil)
	((consp form)
	 (let ((required (require-form-p form)))
	   (if required
	       (list required)
	     (append (require-forms (car form))
		     (require-forms (cdr form))))))
	(t
	 nil)))

(defun require-forms-in-file (path)
  (require-forms (read-from-elisp-file path)))

(defun batch-require-forms-in-file (path)
  (dolist (require-form (require-forms-in-file path) t)
    (princ (format "%s" require-form))
    (terpri)))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-requires.el ends here
