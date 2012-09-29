
(require 'cl)
(require 'nxml-mode)
(require 'ce-xhtml)
(require 'ce-entities)
(require 'ce-utils)

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

(defconst +ce-dash-dashes+ (list ?- ?– ?—))

(defconst +ce-dash-dash-regexp+
  (regexp-opt (mapcar (lambda (char) (format "%c" char)) +ce-dash-dashes+)))

(defconst +ce-dash-editor-buffer-name+ "*Dash Editor*")

(defun ce-dash-inspect-string (string)
  (if (string-match +ce-dash-dash-regexp+ string)
      (ce-dash-edit-dashes string 0)
    string))

(defvar ce-dash-editor-mode-map
  (let ((map (make-keymap)))

    (define-key map (kbd "d") 'ce-dash-delete-character)
    (define-key map (kbd "m") 'ce-dash-replace-with-mdash)
    (define-key map (kbd "n") 'ce-dash-replace-with-ndash)
    (define-key map (kbd "-") 'ce-dash-replace-with-minus)
    (define-key map (kbd "l") 'ce-dash-insert-space-left)
    (define-key map (kbd "r") 'ce-dash-insert-space-right)
    (define-key map (kbd "u") 'ce-dash-undo)
    (define-key map (kbd "c") 'ce-dash-commit-edits)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "q") 'ce-dash-quit)

    map))

(defun ce-dash-edit-dashes (string initial-search-position)
  (let ((dash-position (string-match +ce-dash-dash-regexp+
				     string initial-search-position)))
    (if (null dash-position)
	string
      (let ((dash-editor-buffer (get-buffer-create +ce-dash-editor-buffer-name+)))
	(switch-to-buffer dash-editor-buffer)
	(erase-buffer)
	(kill-all-local-variables)
	(use-local-map ce-dash-editor-mode-map)
	(insert string)
	(add-text-properties (+ dash-position 1) (+ dash-position 2) (list 'face 'highlight))
	(setf mode-name "Dash Editor")
	(setf buffer-read-only t)
	(message "Commands: n, p, SPC, RET; h for help")))))

(defun ce-dash-inspect-nxml-thing (thing)
  (cond ((stringp thing)
	 (ce-dash-inspect-string thing))
	((null thing)
	 nil)
	((consp thing)
	 (let ((element nil)
	       (attributes nil)
	       (children nil))
	   (condition-case nil
	       (destructuring-bind (local-element local-attributes . local-children)
		   thing
		 (setf element local-element
		       attributes local-attributes
		       children local-children))
	     (error
	      (error "Unable to make sense of the node '%s'" thing)))
	   (append (list element attributes)
		   (mapcar 'ce-dash-inspect-nxml-thing children))))
	(t
	 (error "Don't know how to make sense of the nxml object '%s'" thing))))

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

(defvar ce-dash-document-tree nil)

(defun ce-dash-inspect-dashes ()
  (interactive)
  (ce-entities-resolve-named-entities-decimally)
  (save-buffer)
  (let ((tree (condition-case nxml-parse-error
		  (nxml-parse-file (buffer-file-name))
		(error
		 (error "Unable to parse the current buffer as XML:

%s" (error-message-string nxml-parse-error))))))
    (if (ce-dash-some-dash-in-nxml-thing tree)
	(progn
	  (when (get-buffer +ce-dash-editor-buffer-name+)
	    (kill-buffer (get-buffer +ce-dash-editor-buffer-name+)))
	  (let ((dash-editor-buffer (get-buffer-create +ce-dash-editor-buffer-name+)))
	    (with-current-buffer dash-editor-buffer
	      (kill-all-local-variables)
	      (use-local-map ce-dash-editor-mode-map)
	      (set (make-local-variable 'ce-dash-document-tree) tree)
	      (ce-dash-render-dash-editor dash-editor-buffer))
	    (switch-to-buffer dash-editor-buffer)))
      (message "No dashes to edit."))))

(defun ce-dash-elide-string-around (string position)
  (let ((new-string (copy-seq string)))
    (add-text-properties position (1+ position) (list 'face 'highlight) new-string)
    (let ((padding 10)
	  (len (length string)))
      (if (< position padding)
	  (if (> (+ position padding) len)
	      new-string
	    (format "%s…" (substring new-string 0 (+ position padding))))
	(if (> (+ position padding) len)
	    (format "…%s" (substring new-string (- position padding)))
	  (format "…%s…" (substring new-string (- position padding) (+ position padding))))))))

(defun ce-dash-render-dash-editor (editor-buffer)
  (let* ((tree (buffer-local-value 'ce-dash-document-tree editor-buffer))
	 (character-data-sections (ce-dash-character-data-sections tree))
	 (candidate-sections (remove-if-not 'ce-dash-string-contains-dash
					    character-data-sections)))
    (if candidate-sections
	(let ((total-dashes (reduce '+ (mapcar 'ce-dash-count-dashes-in-string
					       character-data-sections)))
	      (num-candidate-sections (length character-data-sections)))
	  (with-current-buffer editor-buffer
	    (erase-buffer)
	    (message "%d dashes across %d character data sections"
		     total-dashes
		     num-candidate-sections)
	    (dolist (candidate-section candidate-sections)
	      (setf candidate-section (ce-dash-nuke-newlines candidate-section))
	      (let ((dash-position (string-match +ce-dash-dash-regexp+ candidate-section 0)))
		(while dash-position
		  (let ((elided-string (ce-dash-elide-string-around candidate-section dash-position)))
		    (insert elided-string)
		    (newline)
		    (insert "======================================================================")
		    (newline))
		  (setf dash-position (string-match +ce-dash-dash-regexp+
						    candidate-section
						    (1+ dash-position))))))
	    (set-buffer-modified-p nil)
	    (setf buffer-read-only t)))
      (message "No dashes to edit."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ce-dash mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ce-dash-refresh-dash-editor (dash-editor-buffer)
  (let ((str (buffer-local-value 'string dash-editor-buffer))
	(pos (buffer-local-value 'cursor dash-editor-buffer)))
    (cond ((null str)
	   (error "The dash editor buffer lacks a string!"))
	  ((null pos)
	   (kill-buffer dash-editor-buffer)
	   str)
	  ((numberp pos)
	   (erase-buffer)
	   (insert str)
	   (add-text-properties (+ pos 1) (+ pos 2) (list 'face 'highlight))
	   (goto-char (point-min))
	   str)
	  (t
	   (error "Cannot refresh the dash editor buffer because we are in an unknown state.")))))

(defun ce-dash-delete-character ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let* ((string (buffer-local-value 'string dash-editor-buf))
	   (cursor (buffer-local-value 'cursor dash-editor-buf))
	   (before-cursor (subseq string 0 (1- cursor)))
	   (after-cursor (subseq string (1+ cursor)))
	   (new-string (concat before-cursor after-cursor))
	   (new-dash-position (string-match +ce-dash-dash-regexp+
					    new-string
					    (1- cursor))))
      (setf string new-string
	    cursor new-dash-position))
    (ce-dash-refresh-dash-editor dash-editor-buf)))

(defun ce-dash-replace-with-mdash ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (let ((before-cursor (subseq string 0 (1- cursor)))
	    (after-cursor (subseq string (1+ cursor))))
	(let ((new-string (concat before-cursor "—" after-cursor)))
	  (setq string new-string)
	  (let ((new-dash-position (string-match +ce-dash-dash-regexp+
						 new-string
						 cursor)))
	    (setq cursor new-dash-position))
	  new-string)))))

(defun ce-dash-replace-with-ndash ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (let ((before-cursor (subseq string 0 (1- cursor)))
	    (after-cursor (subseq string (1+ cursor))))
	(let ((new-string (concat before-cursor "–" after-cursor)))
	  (setq string new-string)
	  (let ((new-dash-position (string-match +ce-dash-dash-regexp+
						 new-string
						 cursor)))
	    (setq cursor new-dash-position))
	  new-string)))))

(defun ce-dash-replace-with-minus ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (let ((before-cursor (subseq string 0 (1- cursor)))
	    (after-cursor (subseq string (1+ cursor))))
	(let ((new-string (concat before-cursor "-" after-cursor)))
	  (setq string new-string)
	  (let ((new-dash-position (string-match +ce-dash-dash-regexp+
						 new-string
						 cursor)))
	    (setq cursor new-dash-position))
	  new-string)))))

(defun ce-dash-insert-space-left ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (let ((before-cursor (subseq string 0 (1- cursor)))
	    (from-cursor (subseq string cursor)))
	(let ((new-string (concat before-cursor " " from-cursor)))
	  (setq string new-string)
	  (let ((new-dash-position (string-match +ce-dash-dash-regexp+
						 new-string
						 cursor)))
	    (setq cursor new-dash-position))
	  new-string)))))

(defun ce-dash-insert-space-right ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (let ((to-cursor (subseq string 0 cursor))
	    (after-cursor (subseq string (1+ cursor))))
	(let ((new-string (concat to-cursor " " after-cursor)))
	  (setq string new-string)
	  (let ((new-dash-position (string-match +ce-dash-dash-regexp+
						 new-string
						 cursor)))
	    (setq cursor new-dash-position))
	  new-string)))))

(defun ce-dash-undo ()
  (message "Undo not supported yet."))

(defun ce-dash-commit-edits ()
  (let ((dash-editor-buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless (bufferp dash-editor-buf)
      (error "The dash editor buffer could not be found!"))
    (let ((string (buffer-local-value 'string dash-editor-buf))
	  (cursor (buffer-local-value 'cursor dash-editor-buf)))
      (setq cursor (length string))
      string)))

(define-derived-mode ce-dash-mode
  special-mode
  "Dashes"
  "Major mode for editing dashes in text.

\\{ce-dash-mode-map}")

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
