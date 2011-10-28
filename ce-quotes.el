(defvar *ce-quote-regexp* "'s\\($\\| \\)"
  "A regular expression matching the quotes we need to treat.")

(defun ce-quote-num-candidates ()
  (how-many *ce-quote-regexp*))

(defun ce-quote-fix-quotes ()
  (interactive)
  (let ((num-candidates-remaining (ce-quote-num-candidates)))
    (if (zerop num-candidates-remaining)
	(message "No candidate quotes to inspect.")
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward *ce-quote-regexp* nil t)
	  (when (or (looking-at " ")
		    (looking-at "$"))
	    (when (y-or-n-p "Replace this occurrence of 's by &apos;s? ")
	      (delete-char 2)
	      (insert "&apos;s"))))))))