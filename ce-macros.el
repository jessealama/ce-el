;;; ce-macros.el --- Macros for ce

;;; Commentary:
;;
;; This file defines some useful Emacs Lisp macros for the ce-el project.

;;; Code:

(eval-when-compile
  (require 'cl))

(defmacro at-least-two (list)
  "Quickly determine whether LIST has at least two members."
  `(and ,list (cdr ,list)))

(defmacro stringify (symbol-or-string)
  "Given SYMBOL-OR-STRING, return a string representation."
  `(if (stringp ,symbol-or-string)
       ,symbol-or-string
     (symbol-name ,symbol-or-string)))

(defmacro symbolify (symbol-or-string)
  "Return a symbolic representation of SYMBOL-OR-STRING."
  `(if (symbolp ,symbol-or-string)
       ,symbol-or-string
     (make-symbol ,symbol-or-string)))

(defmacro beg-of-buffer ()
  "Go to the beginning of the buffer.

The Emacs Lisp function BEGINNING-OF-BUFFER can claim the most
perspicuous name, but the Emacs Lisp compiler warns against
using this function.  The documentation for
`beginning-of-buffer' (as of Emacs 23.3) gives a hint:

  Don't use this command in Lisp programs!
  (goto-char (point-min)) is faster.

But writing '(goto-char (point-min))' is the kind of
speaking-in-code that I dislike so much.  Whence this macro."
  `(goto-char (point-min)))

(defmacro keep-evaluating (&rest body)
  (let ((condition-case-forms
	 (mapcar #'(lambda (form)
		     (list 'condition-case
			   'error-var
			   form
			   (list 'error
				 (list 'message
				       "Something went wrong; the error was:%c%c  %s%c"
				       ?\n ?\n (list 'error-message-string 'error-var) ?\n))))
		 body)))
    `(progn
       ,@condition-case-forms)))

(defmacro ensure-nxml-mode ()
  `(progn
     (when (not (fboundp 'nxml-mode))
       (error "It seems that nxml-mode is not available."))
     (when (not (eq major-mode 'nxml-mode))
       (nxml-mode))))

(defmacro foreach-xml-token (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (let* ((begin (point))
	    (current-xml-token (xmltok-forward))
	    (end (point)))
       (while current-xml-token
	 ,@body
	 (setf begin end
	       current-xml-token (xmltok-forward)
	       end (point))))))

(provide 'ce-macros)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-macros.el ends here
