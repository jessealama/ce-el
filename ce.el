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

(require 'ce-macros) ;; needs to come first among the splitoff
                     ;; packages to ensure that any uses of macros are
                     ;; properly expanded
(require 'ce-unadorned)
(require 'ce-validate)
(require 'ce-quotes)

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

\"Published\" means (at least): it occurs as a subdirectory of
`ce-entries-directory'.  Members of the list are strings.  The
special entries \"sample\" and \"template\" are excluded."
  (when (file-directory-p ce-entries-directory)
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
                      (push candidate winners))))))))))))

(defvar ce-published-entries nil
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
;;; Definition of the minor mode and its keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ce-mode-map nil
  "Keymap used by ce-mode.")

(unless ce-mode-map
  (setf ce-mode-map (make-sparse-keymap)))

(defvar ce-menu
  '(list "SEP"
	 '("Validation"
	   ["Locally validate the HTML of the current buffer"
	    (call-interactively 'ce-validate-current-buffer-locally)
	    (fboundp 'ce-validate-current-buffer-locally)]
	   ["Validate the HTML of the current buffer on Leibniz"
	    (call-interactively 'ce-validate-current-buffer-on-leibniz)
	    (fboundp 'ce-validate-current-buffer-on-leibniz)])
	 "-"
	 ["Customize CE mode"
	  (customize-group 'ce)
	  t]))

(defun ce-mode-menu ()
  "Set up a menu for the CE minor mode (which is not yet defined)."
  (easy-menu-define ce-menu-map
                    ce-mode-map
		    ""
		    (eval ce-menu)))

(defun ce-initialize ()
  (interactive)
  (message "Initializing CE-mode...")
  (ce-refresh-published-entries)
  (message "Initializing CE-mode...done."))

(define-minor-mode ce-mode
  "SEP copyediting utilities"
  :lighter " CE"
  :require nil
  :global t
  :version "0.1"
  :group 'ce
  (ce-mode-menu)
  (ce-initialize))

(provide 'ce)

;;; ce.el ends here
