
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
  (when (string-match *ce-dash-dash-regexp* string)
    (let* ((dash-editor-buf (get-buffer-create +ce-dash-editor-buffer-name+))
	   )
      (with-current-buffer dash-editor-buf
	(erase-buffer)
	(make-local-variable 'cursored-string)
	(setf cursored-string cs)
	(ce-dash-edit-dashes string))))
  string)

(defun ce-dash-redraw-dash-editor ()
  (let ((buf (get-buffer +ce-dash-editor-buffer-name+)))
    (unless buf
      (error "The dash editor buffer is missing!"))
    (let ((cs (buffer-local-value cursored-string)))
      (unless cs
	(error "The dash editor buffer lacks a buffer-local cursored string!"))
      (with-current-buffer buf
	(erase-buffer)
	(insert (cs-render cs))))))

(defun ce-dash-edit-dashes (string)
  (ce-dash-mode)
  (let ((dash-position (string-match *ce-dash-dash-regexp* string)))
    (if dash-position
	t
      string)
    (let ((cs (cursored-string "dash-ful"
			     :string string
			     :initial-position dash-position))
	(pos (cursor-in-digitized-string cs))))))

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

(defun ce-dash-delete-character ()
  (let ((dash-editor-buf (get-buffer "*Dash Editor*")))
    (unless dash-editor-buf
      (error "The dash editor buffer could not be found!"))
    (switch-to-buffer dash-editor-buf)
    (let ((cs (buffer-local-value cursored-string)))
      (unless cs
	(error "The dash editor buffer lacks a cursored string!"))
      (let ((string (oref cs string))
	    (cursor (oref cs cursor)))
	(let ((before-cursor (subseq string 0 (1- cursor)))
	      (after-cursor (subseq string (1+ cursor))))
	  (let ((new-string (concat before-cursor after-cursor)))
	    ;; return a string or a cursored-string?
	    ))))))

(define-derived-mode ce-dash-mode
  special-mode
  "Dashes"
  "Major mode for editing dashes in text.

\\{ce-dash-mode-map}")

(define-key ce-dash-mode-map [d] 'ce-dash-delete-character)
(define-key ce-dash-mode-map [m] 'ce-dash-replace-with-mdash)
(define-key ce-dash-mode-map [n] 'ce-dash-replace-with-ndash)
(define-key ce-dash-mode-map [-] 'ce-dash-replace-with-minus)
(define-key ce-dash-mode-map [a] 'ce-dash-accept-dash)
(define-key ce-dash-mode-map [l] 'ce-dash-insert-space-left)
(define-key ce-dash-mode-map [r] 'ce-dash-insert-space-right)
(define-key ce-dash-mode-map [u] 'ce-dash-undo)

(provide 'ce-dash)



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-dash.el ends here
