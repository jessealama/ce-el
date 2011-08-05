;;; ce-validate.el --- Validate the HTML of SEP entries

;;; Commentary:
;; 
;; This file defines functionality concerning the validation of SEP entries.

;;; Code:

(require 'cl)
(require 'ce-macros)

(defgroup ce-validation nil
  "Variables for controlling how validation is carried out."
  :group 'ce)

(defcustom ce-validator-program
  "onsgmls"
  "The name of the program that is used to validate files."
  :group 'ce-validation)

(defcustom ce-validator-program-environment
  "SP_CHARSET_FIXED=YES SP_ENCODING=XML"
  "A string representing the environment variables under which to
execute `ce-validator-program'."
  :group 'ce-validation)

(defcustom ce-validator-program-flags
  "-s"
  "The arguments that will be passed to `ce-validator-program'.
A leading space is unnecessary."
  :group 'ce-validation)

(defcustom ce-validator-error-regexp
  (concat ce-validator-program ":"
	  "\\(" "\\(/[a-zA-Z0-9]\\)+" "\\)" ":"
	  "\\(" "[0-9]+" "\\)" ":"
	  "\\(" "[0-9]+" "\\)" "\\(" "[EW]" "\\)" " " ".+$")
  "A regular expression that suffices for compile."
  :group 'ce-validation)

(defcustom ce-validation-buffer-name
  "*Validation*"
  "The name of the buffer used for validating entries."
  :group 'ce-validation)
  
(defun ce-validate-files (&rest files)
  "Validate FILES using `ce-validator-program'."
  (let ((compile-command (apply 'concat ce-validator-program-environment " " ce-validator-program " " ce-validator-program-flags " " files)))
    (compile compile-command)))

(defun ce-validate-entry (entry)
  "Validate all XHTML files under ENTRY.
The current window is
split, and a new buffer (called `ce-validation-buffer-name')
  will be visited."
  (let ((entry-files (ce-entry-html-files entry)))
    (when (get-buffer ce-validation-buffer-name)
      (kill-buffer ce-validation-buffer-name))
    (let ((global-validation-buffer (generate-new-buffer ce-validation-buffer-name))
	  (file-validation-buffers nil))
      (dolist (entry-file entry-files)
	(let ((full-filename (ce-entry-file-fullname entry entry-file)))
	  (let ((compile-command (concat ce-validator-program-environment " " ce-validator-program " " ce-validator-program-flags " " full-filename)))
	    (compile compile-command)
	    (with-current-buffer "*compilation*"
	      (rename-buffer (concat "*validation: " entry-file "*") t)
	      (push (cons entry-file (current-buffer)) file-validation-buffers)))))
      (setq file-validation-buffers (reverse file-validation-buffers))
      (with-current-buffer global-validation-buffer
	(dolist (file-validation-buffer file-validation-buffers)
	  (let ((file (car file-validation-buffer))
		(buffer (cdr file-validation-buffer)))
	    (insert file ":")
	    (newline)
	    (insert-buffer-substring buffer)
	    (kill-buffer buffer)
	    (insert "")
	    (newline))))
      (switch-to-buffer-other-window global-validation-buffer))))

(defun ce-validate-find-command (directory type permission name command)
  "A string representing an invocation of find.

DIRECTORY is the directory in which the find command will be run.
TYPE is either NIL or a symbol, 'directory or 'file, indicating
the type of results desired from find.  PERMISSION is a string
presenting permission bits for the resulting
files/directories (e.g., '755', '644').  NAME is the
name (properly speaking, a pattern) of the files/directories to
match.  COMMAND is a command that will be executed for each of
the found files/directories.

All arguments can be NIL, except DIRECTORY."
  (concat ce-validator-program-environment " "
	  "find" " "
	  directory " "
	  (case type
	    (directory "-type d")
	    (file "-type f")
	    (otherwise "")) " "
	  (if permission
	      (concat "-perm " permission)
	    "") " "
	  (if name
	      (concat "-name " "\"" name "\"")
	    "") " "
	  (if command
	      (concat "-exec " command " " "\";\""))))

(defun ce-validate-find-command-with-validator (directory type permission name)
  "Run CE-VALIDATOR-PROGRAM in DIRECTORY.
See the documentation of
`ce-validate-find-command` to learn how TYPE, PERMISSION, and NAME are used."
  (ce-validate-find-command directory
			    type
			    permission
			    name
			    (concat ce-validator-program " " ce-validator-program-flags " {}")))

(defun ce-validate-find-and-validate-readable-html-files-in-directory (directory)
  "Apply the validator to all readable HTML files in DIRECTORY."
  (ce-validate-find-command-with-validator directory
					   'file
					   "664"
					   "*.html"))


(defun ce-validate-entry-quick (entry)
  "Validate ENTRY \"quickly\".

'Quickly' means: if all files under ENTRY's directory are valid,
then print a message saying so.  Otherwise, print a message
indicating that at least one file under ENTRY is invalid."
  (when (get-buffer ce-validation-buffer-name)
    (kill-buffer ce-validation-buffer-name))
  (let ((validation-buffer (generate-new-buffer ce-validation-buffer-name))
	(entry-directory (ce-entry-directory entry))
	(entry-html-files (ce-entry-html-files entry))
	(entry-as-string (stringify entry)))
    (let ((find-command (ce-validate-find-and-validate-readable-html-files-in-directory entry-directory)))
      (message "executring command %s" find-command)
      (message "Validating XHTML files for %s..." entry-as-string)
      (let ((results (shell-command-to-string find-command)))
	(cond ((string= results "")
	       (if (at-least-two entry-html-files)
		   (message "Validating XHTML files for %s...done (all %d are valid)" entry-as-string (length entry-html-files))
		 (message "Validating XHTML files for %s...done (%s, the only XHTML file for this entry, is valid)" entry-as-string (car entry-html-files))))
	       (t
		(switch-to-buffer-other-window validation-buffer)
		(insert results)
		(compilation-mode)
		(message "Validating XHTML files for %s...done (some are invalid)" entry-as-string)))))))

(defun ce-validate-entries (&rest entries)
  "Validate ENTRIES.
A new buffer (called `ce-validation-buffer-name') containing the results of the
validation will be visited."
  (when (get-buffer ce-validation-buffer-name)
    (kill-buffer ce-validation-buffer-name))
  (dolist (entry entries)
    (let ((validation-buffer (generate-new-buffer ce-validation-buffer-name))
	  (entry-directory (ce-entry-directory entry))
	  (entry-html-files (ce-entry-html-files entry))
	  (entry-as-string (stringify entry)))
      (let ((find-command (ce-validate-find-and-validate-readable-html-files-in-directory entry-directory)))
	(message "Validating XHTML files for %s..." entry-as-string)
	(let ((results (shell-command-to-string find-command)))
	  (cond ((string= results "")
		 (if (at-least-two entry-html-files)
		     (message "Validating XHTML files for %s...done (all %d XHTML files are valid)" entry-as-string (length entry-html-files))
		   (message "Validating XHTML files for %s...done (%s, the only XHTML file for this entry, is valid)" entry-as-string (car entry-html-files))))
		(t
		 (switch-to-buffer-other-window validation-buffer)
		 (insert results)
		 (compilation-mode)
		 (message "Validating XHTML files for %s...done (some are invalid)" entry-as-string))))))))

(defun ce-valid-entry-name? (entry-name)
  "Is ENTRY-NAME the name of a published entry?

ENTRY-NAME can be either a symbol or a string, ."
  (let ((entry-name-as-string (stringify entry-name)))
    (member entry-name-as-string ce-published-entries)))

(defun ce-onsgmls-warning-or-error-line (line)
  "Return the LINEs output by onsgmls that are either warning or error lines."
  (let ((warning-or-error-regexp (concat "onsgmls:" ce-entries-directory "/" ce-entry-name-regexp "/" ".+\.html" ":" "[0-9]+" ":" "[0-9]+" ":" "[WE]")))
    (let ((result (string-match warning-or-error-regexp line)))
      (when result
	(message "ce-onsgmls-warning-or-error-line: %s matches!" line)
	result))))

(defun ce-validate-entries-quick (&rest entries)
  "Validate ENTRIES quickly.

'Quickly' means: test, for each entry, whether there is at least
one validation error.  Return a list of pairs (ENTRY
. BAD-FILES), where ENTRY is a member of ENTRIES and BAD-FILES
are the invalid XHTML files under ENTRY's directory."
  (if (every #'ce-valid-entry-name? entries)
      (let (invalid-entries)
	(dolist (entry entries (reverse invalid-entries))
	  (let ((validation-buffer (generate-new-buffer ce-validation-buffer-name))
		(entry-directory (ce-entry-directory entry))
		(entry-html-files (ce-entry-html-files entry))
		(entry-as-string (stringify entry)))
	    (let ((ce-validator-program-flags (concat ce-validator-program-flags " -E1")))
	      (let ((find-command (ce-validate-find-and-validate-readable-html-files-in-directory entry-directory)))
		(message "executing find command %s" find-command)
		(message "Validating XHTML files for %s..." entry-as-string)
		(let ((entry-results (shell-command-to-string find-command)))
		  (unless (string= entry-results "")
		    (let (invalid-file-names)
		      (let ((output-lines (split-string entry-results "\n" t)))
			(let ((warning-or-error-lines (remove-if-not #'ce-onsgmls-warning-or-error-line output-lines)))
			  (dolist (line warning-or-error-lines)
			    (let ((error-line-regexp (concat "onsgmls:" entry-directory "/" "\\(.+\\)\.html")))
			      (message "Matching %s against %s" line error-line-regexp)
			      (string-match error-line-regexp line)
			      (push (match-string-no-properties 1 line) invalid-file-names)))
			  (setq invalid-file-names (delete-dups invalid-file-names))
			  (push (cons entry invalid-file-names) invalid-entries)))))))))))
    (let (invalid-entry-names)
      (dolist (entry entries)
	(unless (ce-valid-entry-name? entry)
	  (push entry invalid-entry-names)))
      (setq invalid-entry-names (nreverse invalid-entry-names))
      (error "Invalid entries: %S" invalid-entry-names))))

(defun ce-validate-all-entries-quick ()
  "Validate all files in all published entries \"quickly\":
return just a list of invalid entries without regard for what the
errors in them are in particular."
  (apply #'ce-validate-entries-quick ce-published-entries))

(defun ce-validate-published-entries ()
  "Validate all files in all published entries."
  (interactive)
  (apply #'ce-validate-entries ce-published-entries))

(defun ce-validate ()
  "Validate the current file."
  (interactive)
  (let (do-not-save)
  (when (buffer-modified-p)
    (if (y-or-n-p "The buffer has not been saved; save now before validating? ")
      (save-buffer)
      (setq do-not-save t)))
  (unless do-not-save
    nil)))
(provide 'ce-validate)

;;; ce-validate.el ends here
