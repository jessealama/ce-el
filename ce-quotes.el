;;; ce-quotes.el --- The variety of quotes

(defvar *ce-quote-regexp* "[']\\(.\\)"
  "A regular expression matching the quotes we need to treat.")

(defun ce-quote-num-candidates ()
  (save-excursion
    (goto-char (point-min))
    (how-many *ce-quote-regexp*)))

(defvar *ce-quote-keymap*
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    map))

(defun y-n-or-q (message)
  (let ((key (read-event message)))
    (setq key (vector key))
    (lookup-key *ce-quote-keymap* key)))

(defun ce-quote-fix-quotes ()
  (interactive)
  (let ((num-candidates-remaining (ce-quote-num-candidates)))
    (if (zerop num-candidates-remaining)
	(message "No candidate quotes to inspect.")
      (save-excursion
	(if (> num-candidates-remaining 1)
	    (message "Inspecting %d posessive quote candidates..."
		     num-candidates-remaining)
	  (message "Inspecting 1 possessive quote candidate..."))
	(goto-char (point-min))
	(while (re-search-forward *ce-quote-regexp* nil t)
	  (decf num-candidates-remaining)
	  (goto-char (match-beginning 0))
	  (let ((message (concat "Replace 's by &apos;s? "
				 (if (= num-candidates-remaining 1)
				     "[1 candidate remaining] "
				   (format "[%d candidates remaining] "
					   num-candidates-remaining))
				 "([y]es, [n]o, [q]uit) ")))
	    (let ((response (y-n-or-q message)))
	      (cond ((string= response "y")
		     (delete-char 2)
		     (insert "&apos;s"))
		    ((string= response "n")
		     (forward-char 2))
		    ((string= response "q")
		     (goto-char (point-max)))
		    (t
		     (message "Unable to process the reponse '%s'; quitting" response)
		     (goto-char (point-max)))))))))))

;;; ce-quotes.el ends here
