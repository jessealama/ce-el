;;; ce-quotes.el --- The variety of quotes

(defvar *ce-quote-regexp* "'s\\($\\| \\)"
  "A regular expression matching the quotes we need to treat.")

(defun ce-quote-num-candidates ()
  (save-excursion
    (goto-char (point-min))
    (how-many *ce-quote-regexp*)))

(defun y-n-or-q (message)
  (let (input)
    (while  (not (or (string= input "y")
		     (string= input "Y")
		     (string= input "n")
		     (string= input "N")
		     (string= input "q")
		     (string= input "Q")))
      (setf input (read-from-minibuffer message)))
    (setf input (downcase input))))

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
