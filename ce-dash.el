
(require 'cl)
(require 'eieio)
(require 'nxml-parse)
(require 'ce-xhtml)
(require 'ce-entities)
(require 'ce-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with dashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +ce-dash-hyphen+ ?-)
(defconst +ce-dash-endash+ ?–)
(defconst +ce-dash-emdash+ ?—)
(defconst +ce-dash-minus+ ?−)

(defconst +ce-dash-dashes+ (list +ce-dash-hyphen+
				 +ce-dash-endash+
				 +ce-dash-emdash+
				 +ce-dash-minus+))

(defconst +ce-dash-whitespace-chars+ (list ?\s ?\n ?\t))

(defconst +ce-dash-dash-regexp+
  (regexp-opt (mapcar 'string +ce-dash-dashes+)))

(defun ce-dash-is-dash-character (c)
  (member c +ce-dash-dashes+))

(defun ce-dash-is-whitespace-character (c)
  (member c +ce-dash-whitespace-chars+))

(defun ce-dash-position-of-dash (string &optional begin)
  (when (< (1+ begin) (length string))
    (string-match +ce-dash-dash-regexp+ string (or begin 0))))

(defun ce-dash-next-dash-occurrence (string &optional begin)
  (unless (stringp string)
    (error "The argument of ce-dash-next-dash-occurrence should be a string."))
  (when (null begin)
    (setf begin 0))
  (let ((len (length string))
	(position (ce-dash-position-of-dash string begin)))
    (when position
      ;; there is a dash character.  Now look around it, grabbing
      ;; whitespace and more dash characters until we reach either the
      ;; beginning of the string, the end of the string, or a
      ;; non-space non-dash character

      (let ((dash-begin position)
	    (dash-end position))

	;; check for whitespace before this dash
	(while (and (> dash-begin 0)
		   (ce-dash-is-whitespace-character (aref string (1- dash-begin))))
	  (decf dash-begin))

	;; keep grabbing dashes until we either reach the end of
	;; the string or encounter a non-dash
	(unless (= (1+ position) len)
	  (let ((j (1+ position)))
	    (while (and (< j len)
			(ce-dash-is-dash-character (aref string j)))
	      (incf j))
	    (setf dash-end (1- j))))

	;; check for whitespace at the end of this dash
	(while (and (< (1+ dash-end) len)
		    (ce-dash-is-whitespace-character (aref string (1+ dash-end))))
	  (incf dash-end))

	(cons dash-begin dash-end)))))

(defun ce-dash-count-dash-occurrences-in-string (string)
  (let ((occurrence (ce-dash-next-dash-occurrence string))
	(count 0))
    (while occurrence
      (incf count)
      (destructuring-bind (dash-begin . dash-end)
	  occurrence
	(setf occurrence (ce-dash-next-dash-occurrence string (1+ dash-end)))))
    count))

(defun ce-dash-inspect-dashes-in-string (string)
  (let ((occurrence (ce-dash-next-dash-occurrence string)))
    (if occurrence
	(let ((dash-end (cdr occurrence))
	      (edited-string (copy-seq string))
	      (num-dashes (ce-dash-count-dash-occurrences-in-string string))
	      (i 0))
	  (while (and occurrence (< i num-dashes))
	    (let ((new-string (ce-dash-fix-dash-occurrence edited-string occurrence)))
	      (when (stringp new-string)
		(setf edited-string new-string))
	      (setf occurrence (ce-dash-next-dash-occurrence edited-string
							     (1+ dash-end)))
	      (setf dash-end (cdr occurrence)))
	    (incf i))
	  edited-string)
      string)))

(defun ce-dash-inspect-dashes ()
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "The buffer has been modified since it was last saved.  Save before continuing? ")
      (save-buffer)))
  (let* ((temp-file (make-temp-file "dash-editor-"))
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
							     'ce-dash-inspect-dashes-in-string)))
		  (setf new-contents (ce-xhtml-render-nxml-thing new-tree))
		  (erase-buffer)
		  (insert new-contents)
		  (goto-char point))
	      (error "Buffer is not valid XML:%c%c%s" ?\n ?\n error-message))))
      (when (file-exists-p temp-file)
	(delete-file temp-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dash predicates, dash fixers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ce-dash-might-be-an-emdash (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (let ((window (substring string dash-begin (1+ dash-end))))
	    (or (string-match "^[[:space:]]+[-–—]+[[:space:]]+$" window)
		(string-match "[-–—][-–—]+" window))))))))

(defun ce-dash-fix-emdash (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s—%s" before after))))

(defun ce-dash-looks-like-a-born/died-range (string occurrence)
  "To handle cases like \"b. 1987- d. 2012\"."
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (+ dash-end 2) len) ;; there should be at least a "d" following the dash occurrence
	  (let ((window (substring string (1- dash-begin) (+ dash-end 2))))
	    (string-match "[[:digit:]][[:space:]-–—]+[d]" window)))))))

(defun ce-dash-fix-born/died-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s–%s" before after))))

(defun ce-dash-looks-like-a-ce-year-range (string occurrence)
  "To handle cases like \"6/5 BCE-38/39 CE\"."
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 2) ;; there should be at least "CE" before
			     ;; the dash occurrence
	(when (< (+ dash-end 2) len) ;; there should be at least a "d"
				     ;; following the dash occurrence
	  (let ((window (substring string (- dash-begin 3) (+ dash-end 2))))
	    (string-match "[C][E][[:space:]-–—]+[[:digit:]]" window)))))))

(defun ce-dash-fix-ce-year-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s–%s" before after))))

(defun ce-dash-endash-should-be-emdash (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  ;; look at the characters immediately preceding and following the occurrence
	  (when (= dash-end dash-begin)
	    (let ((window (substring string
				     (- dash-begin 1)
				     (+ dash-end 2))))
	      (string-match "[[:alpha:]]–[[:alpha:]]" window))))))))

(defun ce-dash-endash->emdash (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s—%s" before after))))

(defun ce-dash-numeric-range-needs-fixing (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (let ((window (substring string (- dash-begin 1) (+ dash-end 2))))
	    (string-match "^[[:digit:]].+[[:digit:]]$" window)))))))

(defun ce-dash-fix-numeric-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before-digit (substring string 0 (- dash-begin 1)))
	  (digit-before-dash (aref string (- dash-begin 1)))
	  (digit-after-dash (aref string (1+ dash-end)))
	  (after-digit (substring string (+ dash-end 2))))
      (format "%s%c–%c%s" before-digit digit-before-dash digit-after-dash after-digit))))

(defun ce-dash-numeric-range+letter-p (string occurrence)
  "Does the dash occurrence OCCURRENCE in STRING look like a
numeric range where the 'numbers' end in characters?

Typical example: \"25a-35b\"."
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (when (> len 4) ;; shortest possible example looks like "5a-6b"
	    (let ((before (ce-dash-word-before string dash-begin))
		  (after (ce-dash-word-after string dash-end))
		  (pattern "^[[:digit:]]+[[:alpha:]]$"))
	      (and (stringp before)
		   (stringp after)
		   (string-match pattern before)
		   (string-match pattern after)))))))))

(defun ce-dash-fix-numeric+letter-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (ce-dash-word-before string dash-begin))
	  (after (ce-dash-word-after string dash-end))
	  (up-to-begin (substring string 0 dash-begin))
	  (after-end (substring string (1+ dash-end))))
      (let ((pos-of-before (position-of-substring-in-string-from-end up-to-begin before))
	    (pos-of-after (position-of-substring-in-string string after)))
	(let ((before-before (substring string 0 pos-of-before))
	      (after-after (substring string (min (length string)
						  (+ pos-of-after (length after))))))
	  (format "%s%s–%s%s" before-before before after after-after))))))

(defun ce-dash-parenthesized-numeric-range-p (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (let ((before (substring string 0 dash-begin))
		(after (substring string (1+ dash-end)))
		(pattern "[(][[:digit:]][[:alpha:]]?+[)]"))
	    (and (string-match (format "%s$" pattern) before)
		 (string-match (format "^%s" pattern) after))))))))

(defun ce-dash-fix-parenthesized-numeric-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s–%s" before after))))

(defun ce-dash-parenthesized-roman-numeric-range-p (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (let ((before (substring string 0 dash-begin))
		(after (substring string (1+ dash-end)))
		(pattern "[(][ivxmcIVMXC]+[)]"))
	    (and (string-match (format "%s$" pattern) before)
		 (string-match (format "^%s" pattern) after))))))))

(defun ce-dash-fix-parenthesized-roman-numeric-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s–%s" before after))))

(defun ce-dash-parenthesized-alphabetic-range-p (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (> dash-begin 0) ;; occurrence starts after the beginning of the string
	(when (< (1+ dash-end) len) ;; occurrence ends before the string does
	  (let ((before (substring string 0 dash-begin))
		(after (substring string (1+ dash-end)))
		(pattern "[(][[:alpha:]][)]"))
	    (and (string-match (format "%s$" pattern) before)
		 (string-match (format "^%s" pattern) after))))))))

(defun ce-dash-fix-parenthesized-alphabetic-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((before (substring string 0 dash-begin))
	  (after (substring string (1+ dash-end))))
      (format "%s–%s" before after))))

(defun ce-dash-looks-like-a-minus (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (< (1+ dash-end) len) ;; occurrence ends before the string does
	(if (zerop dash-begin)
	    (let ((window (substring string 0 (+ dash-end 2))))
	      (string-match "^[[:space:]]*-[[:space:]]*[[:digit:]]$" window))
	  (let ((window (substring string (- dash-begin 1) (+ dash-end 2))))
	    (or (string-match "^[[:digit:]][[:space:]]*-[[:space:]]*[[:alnum:]]$"
			      window)
		(string-match "^[[:alnum:]][[:space:]]*-[[:space:]]*[[:digit:]]$"
			      window))))))))

(defun ce-dash-make-a-minus (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (if (zerop dash-begin)
	(format "−%s" (substring string (1+ dash-end)))
      (let ((window (substring string (- dash-begin 1) (+ dash-end 2))))
	(cond ((string-match "^[[:digit:]][[:space:]]*-[[:space:]]*[[:digit:]]$" window)
	       (format "%s−%s"
		       (substring string 0 dash-begin)
		       (substring string (1+ dash-end))))
	      ((string-match "^[^[:space:]][[:space:]]*-[[:space:]]*[[:digit:]]$" window)
	       (format "%s −%s"
		       (substring string 0 dash-begin)
		       (substring string (1+ dash-end))))
	      (t
	       string))))))

(defgeneric ce-dash-word-before (string position))

(defmethod ce-dash-word-before :before (string position)
  (unless (stringp string)
    (error "First argument of ce-dash-word-before should be a string."))
  (let ((len (length string)))
    (cond ((zerop len)
	   (error "Cannot look for words inside the empty string."))
	  ((integerp position)
	   (error "Second argument of ce-dash-word-before should be a number."))
	  ((< position 0)
	   (error "Second argument of ce-dash-word-before should not be negative."))
	  ((> (1+ position) len)
	   (error "Second argument (%d) of ce-dash-word-before exceeds the length of the string%c%c%s%c" position ?\n ?\n string ?\n)))))

(defmethod ce-dash-word-before (string position)
  (let ((up-to-position (substring string 0 position)))
    (let ((no-newlines (substitute ?\  ?\n up-to-position)))
      (cond ((string-match "^[()[:alnum:]]+$" no-newlines)
	   no-newlines)
	  ((string-match "[^()[:alnum:]]\\([()[:alnum:]]+\\)$" no-newlines)
	   (match-string-no-properties 1 no-newlines))
	  (t
	   nil)))))

(defun ce-dash-exists-word-after (string position)
  (not (entirely-whitespace (substring string 0 position))))

(defun ce-dash-exists-word-before (string position)
  (not (entirely-whitespace (substring string position))))

(defgeneric ce-dash-word-after (string position))

(defmethod ce-dash-word-after :before (string position)
  (unless (stringp string)
    (error "First argument of ce-dash-word-after should be a string."))
  (let ((len (length string)))
    (cond ((zerop len)
	   (error "Cannot look for words inside the empty string."))
	  ((integerp position)
	   (error "Second argument of ce-dash-word-after should be a number."))
	  ((< position 0)
	   (error "Second argument of ce-dash-word-after should not be negative."))
	  ((> (1+ position) len)
	   (error "Second argument (%d) of ce-dash-word-after exceeds the length of the string%c%c%s%c" position ?\n ?\n string ?\n))
	  ((= (1+ position) len)
	   (error "Second argument (%d) of ce-dash-word-after is equal to the length of the string%c%c%s%c" position ?\n ?\n string ?\n)))))

(defmethod ce-dash-word-after (string position)
  (let ((after-position (substring string (1+ position))))
    (let ((no-newlines (substitute ?\ ?\n after-position)))
      (when (string-match "^[[:space:]]*$" no-newlines)
	(error "The string%c%c%s%c%c after position %d is nothing but whitespace." ?\n ?\n string ?\n ?\n position))
      (cond ((string-match "^[[:alnum:]]+$" no-newlines)
	     no-newlines)
	    ((string-match "\\([[:alnum:]]+\\)[^[:alnum:]]" no-newlines)
	     (match-string-no-properties 1 no-newlines))
	    (t
	     nil)))))

(defun ce-dash-roman-numeral-p (string)
  (unless (stringp string)
    (error "Only strings can be Roman numerals."))
  (or (string-match "^[ivlxcm]+$" string)
      (string-match "^[IVLXCM]+$" string)))

(defun ce-dash-roman-numeral-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((len (length string)))
      (when (< (1+ dash-end) len)
	(when (> dash-begin 0)
	  (when (ce-dash-exists-word-after string (1+ dash-end))
	    (when (ce-dash-exists-word-before string (1- dash-end))
	      (let ((previous-word (ce-dash-word-before string dash-begin))
		    (next-word (ce-dash-word-after string dash-end)))
		(and (stringp previous-word)
		     (stringp next-word)
		     (ce-dash-roman-numeral-p previous-word)
		     (ce-dash-roman-numeral-p next-word))))))))))

(defclass dash-fixer ()
  ((test
    :initarg :test)
   (fixer
    :initarg :fixer)
   (name
    :initarg :name
    :type string)))

(defconst +ce-dash-emdash-fixer+
  (dash-fixer
   "Emdash"
   :test 'ce-dash-might-be-an-emdash
   :fixer 'ce-dash-fix-emdash
   :name "Emdash"))

(defconst +ce-dash-life-fixer+
  (dash-fixer
   "Born/died range"
   :test 'ce-dash-looks-like-a-born/died-range
   :fixer 'ce-dash-fix-born/died-range
   :name "Born/died range"))

(defconst +ce-dash-ce-year-fixer+
  (dash-fixer
   "(B)CE year range"
   :test 'ce-dash-looks-like-a-ce-year-range
   :fixer 'ce-dash-fix-ce-year-range
   :name "(B)CE year range"))

(defconst +ce-dash-endash-to-emdash-fixer+
  (dash-fixer
   "Endash-to-emdash"
   :test 'ce-dash-endash-should-be-emdash
   :fixer 'ce-dash-endash->emdash
   :name "Endash-to-emdash"))

(defconst +ce-dash-numeric-range-fixer+
  (dash-fixer
   "Numeric range"
   :test 'ce-dash-numeric-range-needs-fixing
   :fixer 'ce-dash-fix-numeric-range
   :name "Numeric range"))

(defconst +ce-dash-numeric-range+letter-fixer+
  (dash-fixer
   "Numeric range, where \"numbers\" end with letters, such as \"245a\"."
   :test 'ce-dash-numeric-range+letter-p
   :fixer 'ce-dash-fix-numeric+letter-range
   :name "Numeric range (\"numbers\" end with letters)"))

(defconst +ce-dash-minus-sign-fixer+
  (dash-fixer
   "Minus"
   :test 'ce-dash-looks-like-a-minus
   :fixer 'ce-dash-make-a-minus
   :name "Minus"))

(defconst +ce-dash-roman-numeral-range-fixer+
  (dash-fixer
   "Roman numeral range"
   :test 'ce-dash-roman-numeral-range
   :fixer 'ce-dash-fix-numeric-range
   :name "Roman numeral range"))

(defconst +ce-dash-parenthesized-numeric-range-fixer+
  (dash-fixer
   "Parenthesized numeric range"
   :test 'ce-dash-parenthesized-numeric-range-p
   :fixer 'ce-dash-fix-parenthesized-numeric-range
   :name "Parenthesized numeric range (\"numbers\" may end with letters)"))

(defconst +ce-dash-parenthesized-roman-numeric-range-fixer+
  (dash-fixer
   "Parenthesized Roman numeric range"
   :test 'ce-dash-parenthesized-roman-numeric-range-p
   :fixer 'ce-dash-fix-parenthesized-roman-numeric-range
   :name "Parenthesized Roman numeric range"))

(defconst +ce-dash-parenthesized-alphabetic-range-fixer+
  (dash-fixer
   "Parenthesized alphabetic range"
   :test 'ce-dash-parenthesized-alphabetic-range-p
   :fixer 'ce-dash-fix-parenthesized-alphabetic-range
   :name "Parenthesized alphabetic range"))

(defconst +ce-dash-fixers+
  (list +ce-dash-numeric-range-fixer+
	+ce-dash-numeric-range+letter-fixer+
	+ce-dash-minus-sign-fixer+
	+ce-dash-roman-numeral-range-fixer+
	+ce-dash-parenthesized-numeric-range-fixer+
	+ce-dash-parenthesized-roman-numeric-range-fixer+
	+ce-dash-parenthesized-alphabetic-range-fixer+
	+ce-dash-emdash-fixer+
	+ce-dash-life-fixer+
	+ce-dash-ce-year-fixer+
	+ce-dash-endash-to-emdash-fixer+))

(defun ce-dash-applicable-dash-fixers (string occurrence)
  (remove-if-not (lambda (dash-fixer)
		   (funcall (oref dash-fixer test) string occurrence))
		 +ce-dash-fixers+))

(defun ce-dash-elide-around-occurrence (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((len (length string)))
      (unless (and (< dash-end len)
	       (<= 0 dash-begin))
	(error "Cannot elide string around a dash occurrence (%s) that is greater than the length (%d) of a string" occurrence len))
      (highlight-string-region string
			       dash-begin
			       dash-end))))

(defun ce-dash-fix-dash-occurrence (string occurrence)
  "Try to fix the dash occurrence OCCURRENCE of STRING.  Returns
an edited copy of STRING."
  (let ((fixers (ce-dash-applicable-dash-fixers string occurrence)))
    (when fixers
      (if (rest fixers)
	  (let ((names (mapcar (lambda (fixer) (oref fixer name)) fixers)))
	    (let ((prompt (with-output-to-string
			    (loop
			     initially
			     (princ (ce-dash-elide-around-occurrence string occurrence))
			     (terpri)
			     (terpri)
			     with num-names = (length names)
			     for i from 1 upto num-names
			     for name in names
			     do
			     (princ (format "(%d) %s" i name))
			     (when (= i 1) (princ " (default)"))
			     (cond ((< i num-names)
				    (terpri))
				   (t
				    (terpri)
				    (princ "(E) Edit")
				    (terpri)
				    (terpri)
				    (princ "(RET for default): ")))))))
	      (let ((acceptable-response nil)
		    (fixed nil))
		(flet ((unacceptable (response)
		         (message "Invalid response (%s).%c%cEnter a number between 1 and %d, or 'e' to edit the current string." response ?\n ?\n (length fixers))
			 (sleep-for 2)))
		  (while (not acceptable-response)
		    (let* ((response (read-from-minibuffer prompt))
			   (response-as-number (string-to-number response)))
		      (cond ((string= response "")
			     (setf acceptable-response t)
			     (let ((dash-fixer (first fixers)))
			       (setf fixed (funcall (oref dash-fixer fixer)
						    string occurrence))))
			    ((or (string= response "e")
				 (string= response "E"))
			     (setf acceptable-response t)
			     (setf fixed
				   (read-from-minibuffer "" string)))
			    ((not (integerp response-as-number))
			     (unacceptable response))
			    ((not (positivep response-as-number))
			     (unacceptable response))
			    ((> response-as-number (length fixers))
			     (unacceptable response))
			    (t
			     (setf acceptable-response t)
			     (let ((dash-fixer (nth (1- response-as-number)
						    fixers)))
			       (setf fixed (funcall (oref dash-fixer fixer)
						    string occurrence))))))))
		fixed)))
	(let ((dash-fixer (first fixers)))
	  (funcall (oref dash-fixer fixer)
		   string occurrence))))))

(provide 'ce-dash)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
