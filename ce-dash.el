
(eval-when-compile
  (require 'cl)
  (require 'nxml-parse)
  (require 'ce-xhtml)
  (require 'ce-entities)
  (require 'ce-utils))

;;; Informing the Emacs Lisp compiler that some Common Lisp functions,
;;; available through the cl package, are suitably defined.  The
;;; compiler generates warings even though the cl package is required
;;; at compile time.  Sigh.  I really don't see why using the cl
;;; package needs to be so annoying.
(declare-function reduce "cl-seq.el")
(declare-function some "cl-extra.el")
(declare-function member* "cl-seq.el")

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

(defconst +ce-dash-dash-regexp+
  (regexp-opt (mapcar (lambda (char) (format "%c" char)) +ce-dash-dashes+)))

(defconst +ce-dash-editor-buffer-name+ "*Dash Editor*")

(defvar ce-dash-document-tree nil)
(defvar ce-dash-cdata-sections-containing-dashes nil)
(defvar ce-dash-occurence-list nil)
(defvar ce-dash-original-buffer nil)
(defvar ce-dash-dealt-with nil)

(defconst +ce-dash-header+
  "SPC to accept; [e]ndash, e[m]dash, [m]inus sign, [h]yphen; [q]uit and commit")

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
    (error "A symbol is rqeuired for a buffer name"))
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
	       ;; to get to the 'X' or ' ' in '[X]' or '[ ]'
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
	       ;; to get to the 'X' or ' ' in '[X]' or '[ ]'
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

(defun ce-dash-update-dash-editor (tree source-buffer dealt-with)
  (ce-dash-with-dash-editor dash-editor-buffer
    (let* ((cdata-sections (ce-dash-character-data-sections tree))
	   (dash-positions (mapcar 'ce-dash-dash-positions cdata-sections)))
      (set (make-local-variable 'ce-dash-document-tree) tree)
      (set (make-local-variable 'ce-dash-cdata-sections-containing-dashes) dash-positions)
      (set (make-local-variable 'ce-dash-original-buffer) source-buffer)
      (set (make-local-variable 'ce-dash-dealt-with) dealt-with)
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
	   (original-buffer (buffer-local-value 'ce-dash-original-buffer buf))
	   (dealt-with (buffer-local-value 'ce-dash-dealt-with buf)))
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
		   (unless (member line-number dealt-with)
		     (push line-number dealt-with))
		   (ce-dash-update-dash-editor new-tree original-buffer dealt-with)
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
	  (dealt-with (buffer-local-value 'ce-dash-dealt-with buf))
	  (tree (buffer-local-value 'ce-dash-document-tree buf))
	  (source-buf (buffer-local-value 'ce-dash-original-buffer buf)))
      (pushnew line dealt-with :test '=)
      (ce-dash-update-dash-editor tree source-buf dealt-with)
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

(defun ce-dash-inspect-dashes ()
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "The buffer has been modified since it was last saved.  Save before continuing? ")
      (save-buffer)))
  (let ((temp-file (make-temp-file "dash-editor-"))
	(current-contents (buffer-string))
	(current-buffer (current-buffer))
	(current-file (buffer-file-name)))
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
	(cond ((ce-dash-some-dash-in-nxml-thing tree)
	       (when (get-buffer +ce-dash-editor-buffer-name+)
		 (kill-buffer (get-buffer +ce-dash-editor-buffer-name+)))
	       (let ((dash-editor-buffer (get-buffer-create +ce-dash-editor-buffer-name+)))
		 (with-current-buffer dash-editor-buffer
		   (kill-all-local-variables)
		   (use-local-map ce-dash-editor-mode-map)
		   (ce-dash-update-dash-editor tree current-buffer nil)
		   (ce-dash-render-dash-editor dash-editor-buffer))
		 (switch-to-buffer dash-editor-buffer)))
	      (t
	       (message "No dashes to edit.")))))
    (delete-file temp-file)))

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
	 (cdata-sections (ce-dash-character-data-sections tree))
	 (dealt-with (buffer-local-value 'ce-dash-dealt-with editor-buffer)))
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
		 (let ((elided-string (ce-dash-elide-string-around candidate-section dash-position)))
		   (if (member dash-occurrence-number dealt-with)
		       (insert "[X]")
		     (insert "[ ]"))
		   (insert " " (ce-dash-nuke-whitespace elided-string))
		   (newline)))))

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

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
