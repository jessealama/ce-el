
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

(defconst +ce-dash-header+
  "{*} = automatically fixed; [e]ndash, e[m]dash, [s]ubtraction, [h]yphen; q to quit")

(defconst ce-dash-editor-mode-map
  (let ((map (make-keymap)))

    ;; navigation
    (define-key map (kbd "<down>") 'ce-dash-next-line)
    (define-key map (kbd "<up>") 'ce-dash-previous-line)
    (define-key map (kbd "<left>") 'ce-dash-go-nowhere)
    (define-key map (kbd "<right>") 'ce-dash-go-nowhere)

    ;; editing
    (define-key map (kbd "SPC") 'ce-dash-accept-dash-occurrence)

    (define-key map (kbd "m") 'ce-dash-replace-w/-mdash)
    (define-key map (kbd "e") 'ce-dash-replace-w/-ndash)
    (define-key map (kbd "s") 'ce-dash-replace-w/-minus)
    (define-key map (kbd "h") 'ce-dash-replace-w/-hyphen)

    ;; help
    (define-key map (kbd "?") 'describe-mode)

    ;; bailing, committing
    (define-key map (kbd "q") 'ce-dash-quit)

    map))

(defmacro ce-dash-with-dash-editor (buffer &rest body)
  (declare (indent 1))
  (unless (symbolp buffer)
    (error "A symbol is required for a buffer name"))
  `(let ((,buffer (get-buffer +ce-dash-editor-buffer-name+)))
     (unless (bufferp ,buffer)
       (error "The dash editor buffer could not be found!"))
     ,@body))

(defun ce-dash-go-nowhere ()
  (interactive)
  t)

(defun ce-dash-next-line ()
  (interactive)
  (ce-dash-with-dash-editor dash-editor-buf
    (let ((line (current-line))
	  (dash-positions (buffer-local-value 'ce-dash-cdata-sections-containing-dashes dash-editor-buf)))
      (let ((num-dash-occurrences (reduce '+ (mapcar 'length dash-positions))))
	(cond ((<= line num-dash-occurrences)
	       (forward-line 1)
	       (beginning-of-line)
	       (forward-char 1))
	      ((> line num-dash-occurrences)
	       (goto-char (point-min))
	       (forward-line (1- num-dash-occurrences))
	       (beginning-of-line)
	       (forward-char 1)))))))

(defun ce-dash-previous-line ()
  (interactive)
  (ce-dash-with-dash-editor dash-editor-buf
    (let ((line (current-line))
	  (dash-positions (buffer-local-value 'ce-dash-cdata-sections-containing-dashes dash-editor-buf)))
      (let ((num-dash-occurrences (reduce '+ (mapcar 'length dash-positions))))
	(cond ((<= line num-dash-occurrences)
	       (forward-line -1)
	       (beginning-of-line)
	       (forward-char 1))
	      ((> line num-dash-occurrences)
	       (goto-char (point-min))
	       (forward-line (1- num-dash-occurrences))
	       (forward-char 1)))))))

(defun ce-dash-nth-dash (strings n)
  "Given a list STRINGS of strings, return the string that contains that dash number N.

N starts from 1, not 0."
  (cond ((<= n 0)
	 (error "The second argument of ce-dash-nth-dash must be greater than zero."))
	((null strings)
	 (error "The list of strings must be non-empty"))
	(t
	 (let ((string (first strings)))
	   (let ((num-dashes (ce-dash-count-dashes-in-string string)))
	     (if (< num-dashes n)
		 (ce-dash-nth-dash (rest strings) (- n num-dashes))
	       string))))))

(defun ce-dash-update-dash-editor (tree source-buffer)
  (ce-dash-with-dash-editor dash-editor-buffer
    (let* ((cdata-sections (ce-dash-character-data-sections tree))
	   (dash-positions (mapcar 'ce-dash-dash-positions cdata-sections)))
      (set (make-local-variable 'ce-dash-document-tree) tree)
      (set (make-local-variable 'ce-dash-cdata-sections-containing-dashes) dash-positions)
      (set (make-local-variable 'ce-dash-original-buffer) source-buffer)
      (set (make-local-variable 'ce-dash-occurence-list)
	   (let (occurrences)
	     (loop
	      for i from 1 upto (length cdata-sections)
	      for cdata-section in cdata-sections
	      do
	      (when (ce-dash-string-contains-dash cdata-section)
		(let ((dash-positions (ce-dash-dash-positions cdata-section)))
		  (dolist (dash-position dash-positions)
		    (push (list (1- i) dash-position) occurrences)))))
	     (reverse occurrences))))))

(defun ce-dash-replace-character-at-position-with (string position character)
  (format "%s%c%s"
	  (substring string 0 position)
	  character
	  (substring string (1+ position))))

(defun ce-dash-count-cdata-sections (nxml-thing)
  (cond ((stringp nxml-thing) 1)
	((null nxml-thing) 0)
	((consp nxml-thing)
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
	   (reduce '+ (mapcar 'ce-dash-count-cdata-sections children))))
	(t
	 (error "Don't know how to make sense of the nXML object '%s'" nxml-thing))))

(defun ce-dash-replace-nth-cdata-section (nxml-thing cdata-section-number new-cdata-section &optional seen-so-far)
  (let ((seen seen-so-far))
    (cond ((stringp nxml-thing)
	   (if (= (1+ seen-so-far) cdata-section-number)
	       new-cdata-section
	     nxml-thing))
	  ((consp nxml-thing)
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
	     (let (new-children)
	       (dolist (child children)
		 (push (ce-dash-replace-nth-cdata-section child
							  cdata-section-number
							  new-cdata-section
							  seen)
		       new-children)
		 (incf seen (ce-dash-count-cdata-sections child)))
	       (append (list element attributes)
		       (reverse new-children))))))))

(defmacro ce-dash-replace-w/-character (character)
  `(ce-dash-with-dash-editor buf
     (let ((tree (buffer-local-value 'ce-dash-document-tree buf))
	   (cdata-dash-positions (buffer-local-value 'ce-dash-cdata-sections-containing-dashes buf))
	   (dash-occurrences (buffer-local-value 'ce-dash-occurence-list buf))
	   (line-number (current-line))
	   (original-buffer (buffer-local-value 'ce-dash-original-buffer buf)))
       (let ((cdata-sections (ce-dash-character-data-sections tree)))
	 (assert (= (length cdata-dash-positions) (length cdata-sections)))
	 (when (> line-number (length dash-occurrences))
	   (error "The current line number is greater than the total number of dash occurences."))
	 (let ((dash-occurrence (nth (1- line-number) dash-occurrences)))
	   (destructuring-bind (cdata-section-number dash-position)
	       dash-occurrence
	     (let ((cdata-section (nth cdata-section-number cdata-sections)))
	       (macrolet ((replace-with-char (char)
					     `(let ((new-cdata-section (ce-dash-replace-character-at-position-with cdata-section dash-position ,char)))
						(ce-dash-replace-nth-cdata-section tree (1+ cdata-section-number) new-cdata-section 0))))
		 (let ((new-tree (replace-with-char ,character)))
		   (ce-dash-update-dash-editor new-tree original-buffer)
		   (ce-dash-render-dash-editor buf)
		   (goto-char (point-min))
		   (forward-line (1- line-number))
		   (ce-dash-next-line))))))))))

(defun ce-dash-replace-w/-ndash ()
  (interactive)
  (ce-dash-replace-w/-character +ce-dash-endash+))

(defun ce-dash-replace-w/-mdash ()
  (interactive)
  (ce-dash-replace-w/-character +ce-dash-emdash+))

(defun ce-dash-replace-w/-minus ()
  (interactive)
  (ce-dash-replace-w/-character +ce-dash-minus+))

(defun ce-dash-replace-w/-hyphen ()
  (interactive)
  (ce-dash-replace-w/-character +ce-dash-hyphen+))

(defun ce-dash-accept-dash-occurrence ()
  (interactive)
  (ce-dash-with-dash-editor buf
    (let ((line (current-line))
	  (tree (buffer-local-value 'ce-dash-document-tree buf))
	  (source-buf (buffer-local-value 'ce-dash-original-buffer buf)))
      (ce-dash-update-dash-editor tree source-buf)
      (ce-dash-render-dash-editor buf)
      (goto-char (point-min))
      (forward-line (1- line))
      (ce-dash-next-line))))

(defun ce-dash-quit ()
  (interactive)
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "Where did the dash editor buffer go?"))
    (let ((tree (buffer-local-value 'ce-dash-document-tree dash-editor-buf))
	  (source-buf (buffer-local-value 'ce-dash-original-buffer dash-editor-buf)))
      (let ((rendered-tree (ce-xhtml-render-nxml-thing tree)))
	(unless (buffer-live-p source-buf)
	  (error "The buffer from which the dash editor was launched has somehow been killed; unable to replace its contents with the results of dash editing."))
	(switch-to-buffer source-buf)
	(when buffer-read-only
	  (error "The buffer from which the dash editor was launched is somehow read-only now; we are unable to replace its contents with the results of dash editing."))
	(when (buffer-modified-p)
	  (error "The buffer from which the dash editor was launched has been modified since we started to edit dashes.  Refusing to overwrite the new contents; sorry."))
	(erase-buffer)
	(insert rendered-tree)
	(newline) ;; force newline at end of file
	(kill-buffer dash-editor-buf)
	t))))

(defun ce-dash-string-contains-dash (string)
  (not (null (string-match-p +ce-dash-dash-regexp+ string))))

(defun ce-dash-count-dashes-in-string (string)
  (let ((count 0))
    (loop
     for char across string
     do
     (when (member char +ce-dash-dashes+)
       (incf count)))
    count))

(defun ce-dash-character-data-sections (nxml-thing)
  (cond ((stringp nxml-thing) (list nxml-thing))
	((null nxml-thing) nil)
	((consp nxml-thing)
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
	   (reduce 'append
		   (mapcar 'ce-dash-character-data-sections children))))
	(t
	 (error "Don't know how to make sense of the nXML object '%s'" nxml-thing))))

(defun ce-dash-some-dash-in-nxml-thing (nxml-thing)
  (cond ((stringp nxml-thing) (string-match +ce-dash-dash-regexp+ nxml-thing))
	((null nxml-thing) nil)
	((consp nxml-thing)
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
	   (some 'ce-dash-some-dash-in-nxml-thing children)))
	(t
	 (error "Don't know how to make sense of the nXML object '%s'" nxml-thing))))

(defun ce-dash-dash-positions (string)
  (let ((positions nil)
	(position (string-match +ce-dash-dash-regexp+ string)))
    (while position
      (push position positions)
      (setf position (string-match +ce-dash-dash-regexp+ string (1+ position))))
    (reverse positions)))

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

(defun ce-dash-occurrences-in-string (string)
  (let ((occurrence (ce-dash-next-dash-occurrence string))
	(occurrences nil))
    (while occurrence
      (push occurrence occurrences)
      (destructuring-bind (begin . end)
	  occurrence
	(setf occurrence (ce-dash-next-dash-occurrence string (1+ end)))))
    (reverse occurrences)))

(defun ce-dash-next-dash-in-nxml-tree (tree)
  (cond ((consp tree)
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
	   (let ((i 1)
		 (address nil))
	     (while (and (<= i (length children))
			 (not address))
	       (let ((child (nth (1- i) children)))
		 (if (stringp child)
		     (when (ce-dash-string-contains-dash child)
		       (setf address (list i)))
		   (let ((next-dash-in-child (ce-dash-next-dash-in-nxml-tree child)))
		     (when next-dash-in-child
		       (setf address (cons i next-dash-in-child))))))
	       (incf i))
	     address)))
	(t
	 (error "Don't know how to make sense of the nXML object '%s'" tree))))

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

(defun ce-dash-next-dash-in-nxml-tree-after (tree address)
  "The address is the next CDATA section of TREE after ADDRESS
  that contains a dash.  NIL if there is no such address."
  (let ((leaf-number (ce-xhtml-leaf-number-of-leaf-address tree address)))
    (let ((index (ce-xhtml-index-of-first-leaf-satisfying tree
							  'ce-dash-string-contains-dash
							  (1+ leaf-number))))
      (when index
	(ce-xhtml-address-of-leaf-number tree index)))))

(defun ce-dash-inspect-dashes ()
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "The buffer has been modified since it was last saved.  Save before continuing? ")
      (save-buffer)))
  (let* ((temp-file (make-temp-file "dash-editor-"))
	 (current-contents (buffer-string))
	 (current-buffer (current-buffer))
	 (current-file (buffer-file-name))
	 (new-contents current-contents))
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
	(let ((next-dash-cdata-address (ce-dash-next-dash-in-nxml-tree tree)))
	  (while next-dash-cdata-address
	    (let ((thing-at-address (ce-xhtml-node-with-address tree
								next-dash-cdata-address)))
	      (let ((edited (ce-dash-inspect-dashes-in-string thing-at-address)))
		(setf tree (ce-xhtml-replace-thing-at-address tree
							      next-dash-cdata-address
							      edited))))
	    (setf next-dash-cdata-address
		  (ce-dash-next-dash-in-nxml-tree-after tree
							next-dash-cdata-address))))
	(setf new-contents (ce-xhtml-render-nxml-thing tree))))
    (delete-file temp-file)
    (erase-buffer)
    (insert new-contents)
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

(defun ce-dash-render-dash-editor (editor-buffer)
  (let* ((tree (buffer-local-value 'ce-dash-document-tree editor-buffer))
	 (cdata-dash-positions (buffer-local-value 'ce-dash-cdata-sections-containing-dashes
						   editor-buffer))
	 (cdata-sections (ce-dash-character-data-sections tree)))
    (assert (= (length cdata-sections) (length cdata-dash-positions)))
    (if (some 'identity cdata-sections)
	(let ((dash-occurrence-number 0))
	  (with-current-buffer editor-buffer
	    (setf buffer-read-only nil)
	    (erase-buffer)
	    (loop
	     for candidate-section in cdata-sections
	     for dash-positions in cdata-dash-positions
	     do
	     (when dash-positions
	       (dolist (dash-position dash-positions)
		 (incf dash-occurrence-number)
		 (multiple-value-bind (fixed replacement-char)
		     (ce-dash-maybe-fix-dash-occurrence candidate-section dash-position)
		   (if replacement-char
		       (let ((elided (ce-dash-elide-string-around fixed dash-position)))
			 (insert "{" (cond ((char-equal replacement-char +ce-dash-endash+) "e")
					   ((char-equal replacement-char +ce-dash-emdash+) "m")
					   ((char-equal replacement-char +ce-dash-minus+) "s")
					   ((char-equal replacement-char +ce-dash-hyphen+) "h")
					   (t "?")) "}")
			 (insert " " (ce-dash-nuke-whitespace elided)))
		     (let ((elided (ce-dash-elide-string-around candidate-section dash-position))
			   (dash (aref candidate-section dash-position)))
		       (insert "[" (cond ((char-equal dash +ce-dash-endash+) "e")
					 ((char-equal dash +ce-dash-emdash+) "m")
					 ((char-equal dash +ce-dash-minus+) "s")
					 ((char-equal dash +ce-dash-hyphen+) "h")
					 (t "?")) "]")
		       (insert " " (ce-dash-nuke-whitespace elided)))))
		 (newline))))

	    ;; kill the final newline
	    (delete-char -1)

	    (goto-char (point-min))
	    (ce-dash-previous-line) ;; ensure that we put the cursor in the right spot
	    (set-buffer-modified-p nil)
	    (setf mode-name (format "Dash Editor [%d occurrences]" dash-occurrence-number))
	    (setf header-line-format '(:eval (substring +ce-dash-header+
							(min (length +ce-dash-header+)
							     (window-hscroll)))))
	    (setf buffer-read-only t)))
      (message "No dashes to edit."))))

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
      (message "this is '%s'" fragment)
      (or (string-match "^[[:space:]]+---*[[:space:]]*$" fragment)
	  (string-match "^---*[[:space:]]+$" fragment)))))

(defun ce-dash-mdash-it (string occurrence)
  (destructuring-bind (dash-begin . dash-end)
      occurrence
    (let ((fragment (substring string dash-begin (1+ dash-end))))
      (message "this is '%s'" fragment)
      (values
       (format "%s—%s"
	       (substring string 0 dash-begin)
	       (substring string (1+ dash-end)))
       (+ dash-begin 2)))))

(defconst +ce-dash-predicates-and-fixers+
  (list (cons 'ce-dash-numeric-range-needs-fixing 'ce-dash-fix-numeric-range)
	(cons 'ce-dash-multiple-hyphens+space 'ce-dash-mdash-it)))

(defun ce-dash-fix-dash-occurrence (string occurrence)
  "Try to fix the dash occurrence OCCURRENCE of STRING.  Returns
two values: the fixed string (which may be string= to STRING, if
no edits were available) and the index in the fixed string after
which no edits took place."
  (let ((applicable-predicate nil)
	(fixer-function nil)
	(fixer-found? nil))
    (if (some (lambda (predicate-and-fixer)
		(destructuring-bind (predicate . fixer)
		    predicate-and-fixer
		  (when (funcall predicate string occurrence)
		    (setf applicable-predicate predicate
			  fixer-function fixer)
		    t)))
	      +ce-dash-predicates-and-fixers+)
	(funcall fixer-function string occurrence)
      (destructuring-bind (dash-begin . dash-end)
	  occurrence
	(values string dash-end)))))

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
