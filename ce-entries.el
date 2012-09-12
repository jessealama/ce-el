
;; Entries and their locations

(require 'cl)
(require 'ce-macros)

(defcustom ce-entries-directory
  "/usr/local/etc/httpd/htdocs/entries"
  "The entries subdirectory of the webspace directory."
  :type 'directory
  :group 'ce)

(defcustom ce-entry-name-regexp
  "[a-zA-Z0-9]+\\(-[a-zA-Z0-9]+\\)*"
  "A regular expression that matches potential entry names."
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

(defun ce-pubminor (entry)
  "Call pubminor on entry ENTRY."
  nil)

(provide 'ce-entries)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-entries.el ends here
