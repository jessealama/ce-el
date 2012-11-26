
(require 'cl)
(require 'eieio)
(require 'nxml-parse)
(require 'ce-xhtml)
(require 'ce-entities)
(require 'ce-utils)

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
  (let ((dashes-as-strings (mapcar (lambda (char) (format "%c" char)) +ce-dash-dashes+)))
    (regexp-opt dashes-as-strings)))

(defun ce-dash-is-dash-character (c)
  (member c +ce-dash-dashes+))

(defun ce-dash-is-whitespace-character (c)
  (member c +ce-dash-whitespace-chars+))

(defconst +ce-dash-editor-buffer-name+ "*Dash Editor*")

(defvar ce-dash-document-tree nil)
(defvar ce-dash-cdata-sections-containing-dashes nil)
(defvar ce-dash-occurence-list nil)
(defvar ce-dash-original-buffer nil)

(defun ce-dash-string-contains-dash (string)
  (not (null (string-match-p +ce-dash-dash-regexp+ string))))

(defun ce-dash-position-of-dash (string &optional begin)
  (when (null begin)
    (setf begin 0))
  (let ((len (length string)))
    (when (< (1+ begin) len)
      (string-match +ce-dash-dash-regexp+ string begin))))

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

(defun ce-dash-inspect-dashes-in-string (string)
  (let ((occurrence (ce-dash-next-dash-occurrence string)))
    (if occurrence
	(destructuring-bind (dash-begin . dash-end)
	    occurrence
	  (let ((end dash-end)
		(edited-string (copy-seq string))
		(len (length string)))
	    (while occurrence
	      (let ((new-string (ce-dash-fix-dash-occurrence edited-string occurrence)))
		(setf edited-string new-string
		      end (or (mismatch edited-string new-string) len))
		(setf occurrence (ce-dash-next-dash-occurrence edited-string (1+ end)))))
	    edited-string))
      string)))

(defun ce-dash-inspect-dashes ()
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "The buffer has been modified since it was last saved.  Save before continuing? ")
      (save-buffer)))
  (let* ((temp-file (make-temp-file "dash-editor-"))
	 (current-contents (buffer-string))
	 (current-buffer (current-buffer))
	 (current-file (buffer-file-name))
	 (new-contents current-contents)
	 (point (point)))
    (with-temp-file temp-file
      (insert current-contents)
      (ce-entities-resolve-named-entities-decimally))
    (let ((tree (condition-case nxml-parse-error
		    (nxml-parse-file temp-file)
		  (error
		   (message "Unable to parse the current buffer as XML:

%s" (error-message-string nxml-parse-error))
		   nil))))
      (when tree
	(let ((new-tree (ce-xhtml-map-cdata-sections tree
						     'ce-dash-inspect-dashes-in-string)))
	  (setf new-contents (ce-xhtml-render-nxml-thing new-tree)))))
    (delete-file temp-file)
    (erase-buffer)
    (insert new-contents)
    (goto-char point)
    t))

(defcustom *ce-dash-preview-window-padding* 25
  "The number of characters to be displayed before and after an occurrence of a dash.

Don't give this variabe a big value unless you ensure that your
window width is wide: the overall length of the preview string to
be displayed is generally two times the value of this variable."
  :group 'ce)

(defun ce-dash-highlight-string-at-position (string position)
  (let ((new-string (copy-seq string)))
    (add-text-properties position (1+ position) (list 'face 'highlight) new-string)
    new-string))

(defun ce-dash-highlight-string-region (string begin end)
  (let ((new-string (copy-seq string)))
    (add-text-properties begin (1+ end) (list 'face 'highlight) new-string)
    new-string))

(defun ce-dash-prepend-^-sigil (string)
  (let ((new-string (format "^%s" string)))
    (add-text-properties 0 1 (list 'face 'trailing-whitespace) new-string)
    new-string))

(defun ce-dash-append-$-sigil (string)
  (let* ((new-string (format "%s$ " string))
	 (len (length new-string)))
    (add-text-properties (- len 2) (- len 1) (list 'face 'trailing-whitespace) new-string)
    new-string))

(defun ce-dash-elide-string-around (string position)
  (let ((len (length string)))
    (if (and (< position len)
	     (<= 0 position))
	(let* ((highlighted-string (ce-dash-highlight-string-at-position string position))
	       (string-window-padding *ce-dash-preview-window-padding*)
	       (pos-before-window (- position string-window-padding))
	       (pos-after-window (+ position string-window-padding)))
	  (if (< position string-window-padding)
	      (if (< len (* string-window-padding 2))
		  (ce-dash-append-$-sigil (ce-dash-prepend-^-sigil highlighted-string))
		(ce-dash-prepend-^-sigil (format "%s…" (substring highlighted-string
								  0
								  pos-after-window))))
	    (if (> pos-after-window len)
		(ce-dash-append-$-sigil (format "…%s" (substring highlighted-string
								 pos-before-window)))
	      (format "…%s…" (substring highlighted-string
					pos-before-window
					pos-after-window)))))
      (error "Cannot elide string around a position (%d) that is greater than the length (%d) of a string" position len))))

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

(defun ce-dash-looks-like-a-minus (string occurrence)
  (let ((len (length string)))
    (destructuring-bind (dash-begin . dash-end)
	occurrence
      (when (< (1+ dash-end) len) ;; occurrence ends before the string does
	(if (zerop dash-begin)
	    (let ((window (substring string 0 (+ dash-end 2))))
	      (string-match "^[[:space:]]*-[[:space:]]*[[:digit:]]$" window))
	  (let ((window (substring string (- dash-begin 1) (+ dash-end 2))))
	    (or (string-match "^[[:digit:]][[:space:]]*-[[:space:]]*[[:digit:]]$" window)
		(string-match "^[^[:space:]][[:space:]]*-[[:space:]]*[[:digit:]]$" window))))))))

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

(defun ce-dash-multiple-hyphens+space (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((fragment (substring string dash-begin (1+ dash-end))))
      (or (string-match "^[[:space:]]+---*[[:space:]]*$" fragment)
	  (string-match "^---*[[:space:]]+$" fragment)))))

(defun ce-dash-mdash-it (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((fragment (substring string dash-begin (1+ dash-end))))
      (format "%s—%s"
	      (substring string 0 dash-begin)
	      (substring string (1+ dash-end))))))

(defclass dash-fixer ()
  ((test
    :initarg :test)
   (fixer
    :initarg :fixer)
   (name
    :initarg :name
    :type string)))

(defconst +ce-dash-numeric-range-fixer+
  (dash-fixer
   "Numeric range"
   :test 'ce-dash-numeric-range-needs-fixing
   :fixer 'ce-dash-fix-numeric-range
   :name "Numeric range"))

(defconst +ce-dash-emdash-fixer+
  (dash-fixer
   "Emdash"
   :test 'ce-dash-multiple-hyphens+space
   :fixer 'ce-dash-mdash-it
   :name "Emdash"))

(defconst +ce-dash-minus-sign-fixer+
  (dash-fixer
   "Minus"
   :test 'ce-dash-looks-like-a-minus
   :fixer 'ce-dash-make-a-minus
   :name "Minus"))

(defconst +ce-dash-fixers+
  (list +ce-dash-numeric-range-fixer+
	+ce-dash-emdash-fixer+
	+ce-dash-minus-sign-fixer+))

(defun ce-dash-applicable-dash-fixers (string occurrence)
  (remove-if-not (lambda (dash-fixer)
		   (funcall (oref dash-fixer test) string occurrence))
		 +ce-dash-fixers+))

(defun ce-dash-elide-around-occurrence (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((len (length string)))
      (if (and (< dash-end len)
	       (<= 0 dash-begin))
	  (let* ((highlighted-string (ce-dash-highlight-string-region string
								      dash-begin
								      dash-end))
		 (string-window-padding *ce-dash-preview-window-padding*)
		 (pos-before-window (- dash-begin string-window-padding))
		 (pos-after-window (+ dash-end string-window-padding)))
	    (if (< dash-begin string-window-padding)
		(if (< len (* string-window-padding 2))
		    (ce-dash-append-$-sigil (ce-dash-prepend-^-sigil highlighted-string))
		  (ce-dash-prepend-^-sigil (format "%s…" (substring highlighted-string
								    0
								    pos-after-window))))
	      (if (> pos-after-window len)
		  (ce-dash-append-$-sigil (format "…%s" (substring highlighted-string
								   pos-before-window)))
		(format "…%s…" (substring highlighted-string
					  pos-before-window
					  pos-after-window)))))
	(error "Cannot elide string around a dash occurrence (%s) that is greater than the length (%d) of a string" occurrence len)))))

(defun ce-dash-fix-dash-occurrence (string occurrence)
  "Try to fix the dash occurrence OCCURRENCE of STRING.  Returns
an edited copy of STRING."
  (let ((fixers (ce-dash-applicable-dash-fixers string occurrence)))
    (if fixers
	(if (rest fixers)
	    (let ((names (mapcar (lambda (fixer) (oref fixer name)) fixers)))
	      (let ((prompt (with-output-to-string
			      (loop
			       initially
			       (princ (ce-dash-elide-around-occurrence string occurrence))
			       (terpri)
			       with num-names = (length names)
			       for i from 1 upto num-names
			       for name in names
			       do
			       (princ (format "[%d] %s" i name))
			       (if (< i num-names)
				   (princ "; ")
				 (princ ": "))))))
		(let ((response (read-from-minibuffer prompt)))
		  (let ((n (string-to-number response)))
		    (let ((dash-fixer (nth (1- n) fixers)))
		      (funcall (oref dash-fixer fixer) string occurrence))))))
	  (let ((dash-fixer (first fixers)))
	    (funcall (oref dash-fixer fixer) string occurrence)))
      string)))

(defun ce-dash-mark-ambiguous-occurrence (string occurrence)
  "Indicate that the region of STRING delimited by the dash
occurrence OCCURRENCE could be fixed by more than one dash-fixing
function."
  (destructuring-bind (begin . end)
      occurrence
    (add-text-properties begin (1+ end) (list 'face 'comment) string)
    string))

(defun ce-dash-mark-unknown-occurrence (string occurrence)
  "Indicate that the region of STRING delimited by the dash
occurrence OCCURRENCE requires manual fixing because no automatic
dash-fixing function could be applied."
  (destructuring-bind (begin . end)
      occurrence
    (add-text-properties begin (1+ end) (list 'face 'highlight) string)
    string))

(provide 'ce-dash)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
