
(require 'cl)
(require 'nxml-mode)
(require 'ce-xhtml)
(require 'ce-entities)
(require 'ce-utils)

(defun ce-dash-is-dash-entity (entity-str)
  (or (string= entity-str "&ndash;")
      (string= entity-str "&mdash;")))

(defconst *ce-dash-dashes* (list "-" "–" "—"))

(defconst *ce-dash-dash-regexp* (regexp-opt *ce-dash-dashes*))

(defconst +ce-dash-editor-buffer-name+ "*Dash Editor*")

(defun ce-dash-inspect-string (string)
  (let ((dash-position (string-match *ce-dash-dash-regexp* string)))
    (cond ((numberp dash-position)
	   (let ((dash-editor-buf (get-buffer-create +ce-dash-editor-buffer-name+)))
	     (if (bufferp dash-editor-buf)
		 (ce-dash-edit-dashes-in-buffer string dash-editor-buf)
	       (error "We could neither find nor create the dash editor buffer."))))
	  ((null dash-position)
	   string)
	  (t
	    (error "Unknown result

%s

from string-match applied to the string

  %s" dash-position string)))))

(defun ce-dash-edit-dashes-in-buffer (str buffer)
  (unless (stringp str)
    (error "Cannot edit dashes of a non-string: '%s'" str))
  (unless (bufferp buffer)
    (error "Cannot edit dashes in a non-buffer."))
  (let ((dash-position (string-match *ce-dash-dash-regexp* str)))
    (cond (dash-position
	   (switch-to-buffer buffer)
	   (erase-buffer)
	   (insert str)
	   (add-text-properties (+ dash-position 1) (+ dash-position 2) (list 'face 'highlight))
	   (goto-char dash-position)
	   (let ((command (read-string "d to delete: ")))
	     (cond ((string= command "d")
		    (let ((before-dash (subseq str 0 (1- dash-position)))
			  (after-dash (subseq str (1+ dash-position))))
		      (kill-buffer buffer)
		      (ce-dash-inspect-string (concat before-dash after-dash))))
		   (t
		    (error "Unknown command '%s'" command)))))
	  (t
	   str))))

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

(defun ce-dash-inspect-dashes ()
  (interactive)
  (ce-entities-resolve-named-entities-decimally)
  (save-buffer)
  (let ((tree (condition-case nxml-parse-error
		  (nxml-parse-file (buffer-file-name))
		(error
		 (error "Unable to parse the current buffer as XML:

%s" (error-message-string nxml-parse-error))))))
    (let ((new-tree (ce-dash-inspect-nxml-thing tree)))
      (erase-buffer)
      (insert (ce-xhtml-render-nxml-thing new-tree)))))

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
	   (new-dash-position (string-match *ce-dash-dash-regexp* new-string (1- cursor))))
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
	  (let ((new-dash-position (string-match *ce-dash-dash-regexp* new-string cursor)))
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
	  (let ((new-dash-position (string-match *ce-dash-dash-regexp* new-string cursor)))
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
	  (let ((new-dash-position (string-match *ce-dash-dash-regexp* new-string cursor)))
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
	  (let ((new-dash-position (string-match *ce-dash-dash-regexp* new-string cursor)))
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
	  (let ((new-dash-position (string-match *ce-dash-dash-regexp* new-string cursor)))
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

(define-key ce-dash-mode-map [d] 'ce-dash-delete-character)
(define-key ce-dash-mode-map [m] 'ce-dash-replace-with-mdash)
(define-key ce-dash-mode-map [n] 'ce-dash-replace-with-ndash)
(define-key ce-dash-mode-map [-] 'ce-dash-replace-with-minus)
(define-key ce-dash-mode-map [l] 'ce-dash-insert-space-left)
(define-key ce-dash-mode-map [r] 'ce-dash-insert-space-right)
(define-key ce-dash-mode-map [u] 'ce-dash-undo)
(define-key ce-dash-mode-map [c] 'ce-dash-commit-edits)

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
