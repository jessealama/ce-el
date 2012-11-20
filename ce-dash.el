
(require 'cl)
(require 'nxml-parse)
(require 'ce-xhtml)
(require 'ce-entities)
(require 'ce-utils)

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

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
  (regexp-opt (mapcar (lambda (char) (format "%c" char)) +ce-dash-dashes+)))

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
  (string-match +ce-dash-dash-regexp+ string begin))

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
	(when (and (> dash-begin 0)
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
	(when (and (< (1+ dash-end) len)
		   (ce-dash-is-whitespace-character (aref string (1+ dash-end))))
	  (incf dash-end))

	(cons dash-begin dash-end)))))

(defun ce-dash-inspect-dashes-in-string (string)
  (let ((occurrence (ce-dash-next-dash-occurrence string)))
    (if occurrence
	(destructuring-bind (dash-begin . dash-end)
	    occurrence
	  (let ((end dash-end)
		(edited-string (copy-seq string)))
	    (while occurrence
	      (multiple-value-bind (new-string end-of-edit)
		  (ce-dash-fix-dash-occurrence edited-string occurrence)
		(setf edited-string new-string
		      end (1+ end-of-edit))
		(setf occurrence (ce-dash-next-dash-occurrence edited-string end))))
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

(defconst +ce-dash-number-chars+ (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

(defun ce-dash-numeric-range-needs-fixing (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (when (= dash-begin dash-end)
      (let ((position dash-begin))
	(let ((dash (aref string position))
	      (len (length string)))
	  (when (char-equal dash ?-)
	    (when (< (+ position 2) len)
	      (unless (zerop position)
		(let ((preceding-char (aref string (1- position)))
		      (following-char (aref string (1+ position))))
		  (and (find preceding-char +ce-dash-number-chars+)
		       (find following-char +ce-dash-number-chars+)))))))))))

(defun ce-dash-fix-numeric-range (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (values
     (format "%s–%s"
	     (substring string 0 dash-begin)
	     (substring string (1+ dash-begin)))
     dash-end)))

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
      (values (format "%s—%s"
		      (substring string 0 dash-begin)
		      (substring string (1+ dash-end)))
	      (+ dash-begin 2)))))

(defconst +ce-dash-predicates-and-fixers+
  (list (cons 'ce-dash-numeric-range-needs-fixing 'ce-dash-fix-numeric-range)
	(cons 'ce-dash-multiple-hyphens+space 'ce-dash-mdash-it)))

(defun ce-dash-applicable-dash-fixers (string occurrence)
  (remove-if 'null
	     (mapcar (lambda (predicate-and-fixer)
		       (when (funcall (car predicate-and-fixer) string occurrence)
			 (cdr predicate-and-fixer)))
		     +ce-dash-predicates-and-fixers+)))

(defun ce-dash-fix-dash-occurrence (string occurrence)
  "Try to fix the dash occurrence OCCURRENCE of STRING.  Returns
two values: the fixed string (which may be string= to STRING, if
no edits were available) and the index in the fixed string after
which no edits took place."
  (let ((fixers (ce-dash-applicable-dash-fixers string occurrence)))
    (if fixers
	(if (rest fixers)
	    (destructuring-bind (dash-begin . dash-end)
		occurrence
	      (values (ce-dash-mark-ambiguous-occurrence string occurrence)
		      dash-end))
	  (let ((fixer (first fixers)))
	    (funcall fixer string occurrence)))
      (destructuring-bind (dash-begin . dash-end)
	  occurrence
	(values (ce-dash-mark-unknown-occurrence string occurrence) dash-end)))))

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
