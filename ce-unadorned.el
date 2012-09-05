;;; ce-unadorned.el --- Check for and fix unadorned text following HTML elements

;;; Commentary:
;;
;; In XHTML 1.0 Transitional, it is valid to write HTML such as this:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; <blockquote>Some quote here</blockquote>
;;
;; Implicit paragraph starting here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Such "unadorned" text -- text that isn't wrapped in any element
;; (except as raw text in the ambient HTML BODY element) can present
;; some problems for generating PDF representations of articles.  We
;; thus need a way of identifying and fixing such instances of
;; unadorned text.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ce-entries)

(defcustom ce-unadorned-fixed-site-directory
  "/tmp/ce"
  "The directory under which entries will by copied and unadorned text fixed."
  :group 'ce)

(defcustom ce-unadorned-potential-violator-tags
  '(h1 h2 h3 h4 h5 h6 ul ol table div blockquote dt dl menu dir pre hr address center noframes)
  "Symbolic names of XHTML tags after whose closing unadorned
text shouldn't come.

Ed Zalta and I (Jesse Alama) talked about this list on August 27,
2008.  It may be incomplete."
  :group 'ce)

(defun ce-unadorned-tag-at-point ()
  "Determine what tag we're looking at."
  (save-excursion
    (re-search-forward "<\([a-z0-9]+\)[^>]*>")
    (make-symbol (match-string-no-properties 1))))

(defun ce-unadorned-within-comment ()
  "Determine whether point is in the scope of an XHTML comment."
  nil) ;; sophisticated check

(defun ce-unadorned-opening-form (tag)
  "The XHTML opening form for TAG (assumed to be a symbol)."
  (concat "<" (symbol-name tag)))

(defun ce-unadorned-closing-form (tag)
  "The XHTML closing form for TAG (assumed to be a symbol).
Just to be safe, we allow whitespace to come between the tag and the
  closing \">\"."
  (concat "</" (symbol-name tag) "[:white:]*" ">"))

(defun ce-unadorned-files-in-entry (entry)
  "Determine the XHTML files stored in the directory for ENTRY."
  (let ((entry-as-string (if (stringp entry) entry (symbol-name entry))))
    (let ((entry-dir (concat ce-entries-directory "/" entry-as-string "/")))
      (directory-files entry-dir nil "\.html$"))))

;; TODO: This doesn't pay attention to comments in a robust way.
(defun ce-unadorned-check-entry (entry)
  "Find all violations for ENTRY.
ENTRY can be either a symbol
or a string.  Return a list of cons cells whose car a
filename (e.g., \"index.html\"), and whose cdr is a natural
number indicating the character number where a violations in the
corresponding file was found."
  (let ((entry-as-string (if (stringp entry) entry (symbol-name entry)))
	(all-violations nil)
	(files (ce-unadorned-files-in-entry entry)))
    (dolist (file files (nreverse all-violations))
      (let ((filename (concat ce-entries-directory "/" entry-as-string "/" file))
	    (violations-for-this-file nil))
	(when (get-buffer filename)
	  (error "A buffer is already visiting %S.  Kill the buffer and try again" filename))
	(unless (file-exists-p filename)
	  (error "No such file %S" filename))
	(let ((buf (find-file-noselect filename t)))
	  (with-current-buffer buf
	    (dolist (potential-violator-tag ce-unadorned-potential-violator-tags)
	      (let ((opening-form (ce-unadorned-opening-form potential-violator-tag))
		    (closing-form (ce-unadorned-closing-form potential-violator-tag)))
		(beg-of-buffer)
		(while (re-search-forward closing-form nil t)
		  (save-excursion
		    (re-search-forward "[^[:space:]]") ;; find the next non-whitespace character
		    (backward-char) ;; now we're at the next non-whitespace character
		    (unless (char-equal (char-after) ?<)
		      ;; I don't like this logic.  I feel like it
		      ;; won't catch everything, e.g., the bare list
		      ;; of a tags following "Related Entries".  But
		      ;; it seems to do the job for now.
		      (push (point) violations-for-this-file)))))))
	  (kill-buffer buf))
	(setq violations-for-this-file (sort violations-for-this-file #'<))
	(when violations-for-this-file
	  (push (cons file violations-for-this-file) all-violations))))))

(defun ce-unadorned-check-buffer (buf)
  "Check buffer BUF for unadorned text."
  (let ((violations nil))
    (dolist (potential-violator-tag ce-unadorned-potential-violator-tags (sort violations #'<))
      (let ((opening-form (ce-unadorned-opening-form potential-violator-tag))
	    (closing-form (ce-unadorned-closing-form potential-violator-tag)))
	(save-excursion
	  (beg-of-buffer)
	  (while (re-search-forward closing-form nil t)
	    (save-excursion
	      (re-search-forward "[^[:space:]]") ;; find the next non-whitespace character
	      (backward-char) ;; now we're at the next non-whitespace character
	      (unless (char-equal (char-after) ?<)
		;; I don't like this logic.  I feel like it
		;; won't catch everything, e.g., the bare list
		;; of a tags following "Related Entries".  But
		;; it seems to do the job for now.
		(push (line-number-at-pos) violations)))))))))

(defun ce-unadorned-check-this-buffer ()
  "Check the current buffer for unadorned text."
  (interactive)
  (let ((violations (ce-unadorned-check-buffer (current-buffer))))
    (message "Found violations: %S" violations)))

(defun ce-unadorned-check-entries ()
  "Check all SEP entries."
  (let ((violators nil))
    (dolist (entry ce-published-entries)
      (let ((violators-for-entry (ce-unadorned-check-entry entry)))
	(when violators-for-entry
	  (push (cons entry violators-for-entry) violators))))
    (setq violators (nreverse violators))))

;;; Fixing (not just identifying) entries with unadorned text.

(defun ce-unadorned-copy-entry (entry)
  "Copy the files under ENTRY into `ce-unadorned-fixed-site-directory'."
  (let ((entry-as-string (if (stringp entry) entry (symbol-name entry)))
	(entry-files (ce-unadorned-files-in-entry entry)))
    (let ((local-entry-dir (concat ce-unadorned-fixed-site-directory "/" entry-as-string))
	  (webspace-entry-dir (concat ce-entries-directory "/" entry-as-string)))
      (unless (file-exists-p ce-unadorned-fixed-site-directory)
	(make-directory ce-unadorned-fixed-site-directory))
      (unless (file-exists-p local-entry-dir)
	(make-directory local-entry-dir))
      (dolist (entry-file entry-files t)
	(let ((local-entry-filename (concat local-entry-dir "/" entry-file))
	      (webspace-entry-filename (concat webspace-entry-dir "/" entry-file)))
	  (when (file-exists-p local-entry-filename)
	    (delete-file local-entry-filename))
	  (copy-file webspace-entry-filename local-entry-filename))))))

;; TODO: deal with p tags that should go before </li> and </blockquote>
;; As it stands, my code deals with cases such as
;;
;; <ul>
;; <li> Hello
;;   <blockquote>Quote</blockuoqte>
;;   Some text
;; </li>
;;
;; by putting a </p> after </li>.  Same for </td>.
;;
;; TODO: The code also does not deal with nested blockquotes (it
;; mistakenly thinks that the end of the first </blockquote> should be
;; followed by a <p>).

(defun ce-unadorned-fix-entry (entry)
  "Copy the files for ENTRY to
`ce-unadorned-fixed-site-directory', then fix those files."
  (let ((violations (ce-unadorned-check-entry entry))
	(entry-as-string (if (stringp entry) entry (symbol-name entry))))
    (ce-unadorned-copy-entry entry)
    (dolist (violation violations (save-buffer))
      (let ((violating-file (car violation))
	    (violations-in-file (cdr violation)))
	(let ((use-hard-newlines t))
	  (let ((local-entry-filename (concat ce-unadorned-fixed-site-directory "/" entry-as-string "/" violating-file)))
	    (let ((buf (find-file-noselect local-entry-filename)))
	      (with-current-buffer buf
		;; we need to edit from the end toward the beginning
		;; so that buffer positions make sense
		(dolist (violation-in-file (reverse violations-in-file) (save-buffer))
		  (goto-char violation-in-file)
		  (insert "<p>")
		  (newline 2)
		  (end-of-paragraph-text)
		  (insert "</p>")))
	      (kill-buffer buf))))))))

(defun ce-unadorned-fix-everything ()
  "Fix every entry.
Entries that do not need to be fixed are not
copied; the entries that do require fixing are copied and fixed."
  (dolist (entry ce-published-entries)
    (ce-unadorned-fix-entry entry)))

(provide 'ce-unadorned)

(provide 'ce-unadorned)

;;; ce-unadorned.el ends here
