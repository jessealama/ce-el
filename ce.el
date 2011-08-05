;;; ce.el --- Copyedit entries for the Stanford Encyclopedia of Philosophy

;;; History:
;;
;; Created: August 27, 2008

;;; Commentary:
;;
;; Copyediting has many facets.  The Stanford Encyclopedia of
;; Philosophy (SEP) has a number of copyediting tools that are all
;; individually quite useful.  As it stands, though (at least, at the
;; time this project began), they are a grab-bag of tools that, as far
;; as I can tell, are not always applied uniformly.  It would be nice
;; if there were just one tool that synthesized all the copyediting
;; tasks, a single tool that could be used to carry out all (or nearly
;; all) the copyediting tasks that come up when copyediting entries
;; for the SEP.
;;
;; This tool is inspired by a number of Emacs utilities: checkdoc,
;; ediff, and nxhtml-mode lie in the background.
;;
;; The tool ought to provide a number of services:
;;
;;   * validation
;;   * natural language checking (spelling, grammar, usage)
;;   * checking for adherence to house style
;;
;; The tool should provide a way for a copyeditor to step through an
;; entry, accept of reject suggested changes, and, when accepting a
;; suggested change, to apply it automatically.  Copyediting cannot be
;; entirely automated, but many things can be done mechanically
;; (sometimes interactively with a human copyeditor, sometimes
;; non-interactively).

(require 'cl)
(require 'custom)
(require 'pp)

(eval-when (load eval)
  (message "Initializing copyeditor tools..."))

(eval-after-load 'ce
  (message "Initializing copyeditor tools...done"))

;;; Our own splitoff packages

(require 'ce-unadorned)

;;; Macros

(defmacro at-least-two (list)
  "Quickly determine whether LIST has at least two members."
  `(and ,list (cdr ,list)))

(defmacro stringify (symbol-or-string)
  "Given SYMBOL-OR-STRING, return a string representation."
  `(if (stringp ,symbol-or-string) ,symbol-or-string (symbol-name ,symbol-or-string)))

(defmacro symbolify (symbol-or-string)
  "Return a symbolic representation of SYMBOL-OR-STRING."
  `(if (symbolp ,symbol-or-string) ,symbol-or-string (make-symbol ,symbol-or-string)))

(defmacro beg-of-buffer ()
  "Go to the beginning of the buffer.

The Emacs Lisp function BEGINNING-OF-BUFFER can claim the most
perspicuous name, but the Emacs Lisp compiler warns against
using this function.  The documentation for
`beginning-of-buffer' (as of Emacs 23.3) gives a hint:

  Don't use this command in Lisp programs!
  (goto-char (point-min)) is faster.

But writing '(goto-char (point-min))' is the kind of
speaking-in-code that I dislike so much.  Whence this macro."
  `(goto-char (point-min)))

;;; User variables and customization

(defgroup sep nil
  "Customization options for utilities for dealing with the
Stanford Encyclopedia of Philosophy."
  :tag "SEP"
  :group 'emacs)

(defgroup ce nil
  "A collection of tools for doing copyediting for the Stanford
Encyclopedia of Philosophy."
  :tag "Copyediting"
  :group 'sep)

;; Entries and their locations

(defcustom ce-entries-directory
  "/usr/local/etc/httpd/htdocs/entries"
  "The entries subdirectory of the webspace directory."
  :type 'directory
  :group 'ce)

(defcustom ce-entry-name-regexp
  "[a-zA-Z0-9]+\\(-[a-zA-Z0-9]+\\)*"
  "A regular expression that matches potentially entry names."
  :group 'ce)

(defun ce-published-entries ()
  "A list of the published entries.
Members of the list are
strings.  The special entries \"sample\" and \"template\" are
  excluded."
  (let ((candidates (directory-files ce-entries-directory nil "^[a-z0-9]"))
	(winners nil))
    (dolist (candidate candidates (reverse winners))
      (unless (or (string= candidate "sample") (string= candidate "template"))
	(let ((candidate-filename (concat ce-entries-directory "/" candidate)))
	  (when (file-directory-p candidate-filename)
	    ;; Uri's heuristic: directories in webspace that contain an
	    ;; index.html file whose size is at least 2000 bytes.  "sample" is
	    ;; not a real entry.
	    ;;
	    ;; Perhaps this would be quicker if we just ran the command
	    ;;
	    ;;   find ce-entries-directory -name "index.html" -size=+2000c | cut -d '/' -f 8
	    ;;
	    ;; But the Lisp approach seems quick enough.
	    (let ((index-filename (concat candidate-filename "/" "index.html")))
	      (when (file-exists-p index-filename)
		(let ((index-size (nth 7 (file-attributes index-filename))))
		  (when (> index-size 2000)
		    (push candidate winners)))))))))))
	
(defvar ce-published-entries (ce-published-entries)
  "The list of published entries for the SEP.")

(defun ce-refresh-published-entries ()
  "Refresh `ce-published-entries'."
  ;; typo corrected by paul on 20110625
  (setq ce-published-entries (ce-published-entries)))

(defun ce-entry-directory (entry)
  "The webspace directory name for ENTRY.  It may not exist."
  (let ((entry-as-string (stringify entry)))
    (concat ce-entries-directory "/" entry-as-string)))

(defun ce-entry-index (entry)
  "The index filename for the entry ENTRY.  It may not exist."
  (concat (ce-entry-directory entry) "/" "index.html"))

(defun ce-trim-filename-with-respect-to-entry (entry filename)
  "Given ENTRY and FILENAME, delete the initial prefix.
Entry may be either a symbol of a string, but FILENAME must be a
string.  Uses \ce-entries-directory\.  What this
function in fact does is just take a certain substring of
FILENAME; for now, it does not enforce any relation between ENTRY
and FILENAME."
  (let ((entry-as-string (stringify entry)))
    (substring filename (+ (length ce-entries-directory)
			   1 ;; for "/"
			   (length entry-as-string)
			   1 ;; counting starts at 0
			   ))))

(defun ce-entry-files (entry)
  "All the files under the entry ENTRY.  The result is a list of relative paths."
  (let ((entry-directory (ce-entry-directory entry)))
    (let ((find-command (concat "find " entry-directory " -type f -perm 664")))
      (let ((all-files (split-string (shell-command-to-string find-command) "\n" t)))
	(mapcar #'(lambda (file) (ce-trim-filename-with-respect-to-entry entry file)) all-files)))))

(defun ce-entry-file-fullname (entry file)
  "The full filename of FILE under the directory for ENTRY."
  (concat (ce-entry-directory entry) "/" file))

(defun ce-entry-html-files (entry)
  "All the HTML files under the entry ENTRY.  The result is a list of relative paths."
  (let (html-files)
    (dolist (file (ce-entry-files entry) (reverse html-files))
      (when (string-match "\.html$" file)
	(push file html-files)))))

(defun ce-published-entries-all-files ()
  "Return a list of all the files in an entry's subdirectory.
The special \"source\" subdirectory is not excluded."
  (let (results)
    (dolist (entry ce-published-entries (reverse results))
      (let ((entry-files (ce-entry-files entry)))
	(push (cons entry entry-files) results)))))

(defun ce-entry-title (entry)
  "The title of ENTRY."
  (let ((entry-as-string (stringify entry)))
    (let ((index-file (ce-entry-index entry)))
      (if (file-exists-p index-file)
	  (let ((buf (or (get-file-buffer index-file) (find-file-noselect index-file)))
		(title-regexp "<title>\\(.+\\) (Stanford Encyclopedia of Philosophy)</title>")
		(title nil))
	    (with-current-buffer buf
	      (save-excursion
		(beg-of-buffer)
		(re-search-forward title-regexp nil t)
		(setq title (match-string-no-properties 1))))
	    (unless (get-file-buffer index-file)
	      (kill-buffer buf))
	    title)
	(error "No such entry %s" entry-as-string)))))

(defun ce-find-entry-noselect (entry)
  "Visit (but don't select) a buffer visiting the index file for ENTRY."
  (let ((entry-as-string (stringify entry)))
    (let ((entry-filename (ce-entry-index entry)))
      (if (file-exists-p entry-filename)
	  (let ((all-files (directory-files (ce-entry-directory entry) nil "[^.]")) ;; exclude dot files
		(entry-title (ce-entry-title entry))
		(entry-buffer (find-file-noselect entry-filename)))
	    (with-current-buffer entry-buffer
	      (rename-buffer (concat entry-as-string ":" " " entry-title))
	    (if (at-least-two all-files)
		(values entry-buffer t)
		(values entry-buffer nil)))
	  (error "No such entry %s" entry))))))

(defun ce-find-entry (entry)
  "Open the index file in the webspace directory for ENTRY.

ENTRY can be either a symbol or a string."
  (interactive
   (list (completing-read "Entry: " ce-published-entries nil t)))
  (let ((entry-as-string (stringify entry)))
    (multiple-value-bind (entry-buffer more-files)
	(ce-find-entry-noselect entry)
      (if more-files
	  (progn
	    (switch-to-buffer entry-buffer)
	    (message "Visiting index file for %s [there are more files in the entry's directory]" entry-as-string))
	(progn
	  (switch-to-buffer entry-buffer)
	  (message "Visiting %s" entry-as-string))))))

(defun ce-find-entry-other-window (entry)
  "In another window, open the index file in the webspace directory for ENTRY.
ENTRY can be either a symbol or a string."
  (interactive
   (list (completing-read "Entry: " ce-published-entries nil t)))
  (let ((entry-as-string (stringify entry)))
    (multiple-value-bind (entry-buffer more-files)
	(ce-find-entry-noselect entry)
      (if more-files
	  (progn
	    (switch-to-buffer entry-buffer)
	    (message "Visiting index file for %s [there are more files in the entry's directory]" entry-as-string))
	(progn
	  (switch-to-buffer-other-window entry-buffer)
	  (message "Visiting %s" entry-as-string))))))

(defun ce-find-entry-other-frame (entry)
  "In another frame, open the index file in the webspace directory for ENTRY.
ENTRY can be either a symbol or a string."
  (interactive
   (list (completing-read "Entry: " ce-published-entries nil t)))
  (let ((entry-as-string (stringify entry)))
    (multiple-value-bind (entry-buffer more-files)
	(ce-find-entry-noselect entry)
      (if more-files
	  (progn
	    (switch-to-buffer entry-buffer)
	    (message "Visiting index file for %s [there are more files in the entry's directory]" entry-as-string))
	(progn
	  (switch-to-buffer-other-frame entry-buffer)
	  (message "Visiting %s" entry-as-string))))))

;;; Copyediting utilities

(defun ce-spell ()
  "Spell-check the current entry."
  nil)

(defun ce-diction ()
  "Interactively run diction(1) on the current entry."
  nil)

(defun ce-aorq ()
  "Do it."
  nil)

(defun ce-wrap ()
  "Wrap the current entry."
  nil)

(defun ce-checklinks ()
  "Determine whether all links on this page are valid."
  nil)

(defun ce-check-everything ()
  "Check everything."
  (ce-spell)
  (ce-checklinks)
  (ce-aorq)
  (ce-wrap))

;Function to run Tidy HTML parser on buffer
; NOTE: this requires external Tidy program.
(defun ce-tidy ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
    "tidy -f /tmp/tidy-errs -q -i -wrap 72 -c" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'd"))

(defun ce-pubminor (entry)
  "Call pubminor on entry ENTRY."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'ce)

;;; ce.el ends here
