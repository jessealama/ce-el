;;; ce-validate.el --- Validate the HTML of SEP entries

;;; Commentary:
;;
;; This file defines functionality concerning the validation of SEP entries.

;;; Code:

(require 'cl)
(require 'nxml-mode)
(require 'rng-valid)

;; Our stuff
(require 'ce-macros)
(require 'ce-utils)
(require 'ce-entities)

(declare-function rng-validate-mode "rng-valid.el")

(defconst +non-ascii-regexp+ "[[:nonascii:]]")

(defun ce-validate ()
  "Validate the current buffer."
  (interactive)
  ;; can we run rng-validate-mode?
  (when (not (and (fboundp 'rng-validate-mode)
		  (fboundp 'nxml-mode)))
    (error "Unable to run the validator (either nxml-mode or rng-validate-mode seems to be missing)"))
  ;; switch to nxml mode
  (when (not (eq major-mode 'nxml-mode))
    (nxml-mode))
  (when (not (member 'rng-validate-mode minor-mode-list))
    (rng-validate-mode))
  (save-excursion
    (let ((next-structural-error (ce-validate-next-structural-error)))
      (if next-structural-error
	  (goto-char next-structural-error)
	(let ((next-entity-error (ce-validate-next-entity-error)))
	  (if next-entity-error
	      (ce-validate-entities)
	    (let ((num-non-ascii-chars (count-matches +non-ascii-regexp+)))
	      (if (zerop num-non-ascii-chars)
		  (message "XHTML is structurally valid, all entities are known, and there are no non-ASCII characters.")
		  (progn
		    (occur +non-ascii-regexp+)
		    (message "XHTML is structurally valid and all entities are valid XHTML entities, but there is at least one non-ASCII character in the current buffer."))))))))))

(defun ce-validate-next-entity-error ()
  "The position of the first entity error in the current buffer.

If there are no errors, return NIL."
  (let (bad-position)
    (save-excursion
      (goto-char (point-min))
      (let* ((begin (point-min))
	     (token (xmltok-forward))
	     (end (point)))
	(while token
	  (if (eq token 'entity-ref)
	      (let ((entity (buffer-substring-no-properties begin end)))
		(if (ce-entities-known-entity entity)
		    (setf begin end
			  token (xmltok-forward)
			  end (point))
		  (setf bad-position begin
			token nil)))
	    (setf begin end
		  token (xmltok-forward)
		  end (point))))))
    bad-position))

(defun ce-validate-entities ()
  "Check that all entities in the current buffer are valid XHTML entities."
  (interactive)
  (let ((bad-position (ce-validate-next-entity-error)))
    (if bad-position
	(progn
	  (goto-char bad-position)
	  (message "Bad entity here."))
      (message "All entities in the current buffer are valid."))))

(defun ce-validate-next-error ()
  "Go to the next structural XML error."
  (interactive)
  (let ((structure-error (ce-validate-next-structural-error)))
    (if structure-error
	(progn
	  (message "Malformed/unexpected XHTML structure.")
	  (goto-char structure-error))
      (let ((entity-error (ce-validate-next-entity-error)))
	(message "") ; clear the echo area in case some rng or nxml
		  ; function put something there
	(if entity-error
	    (progn
	      (goto-char entity-error)
	      (message "Bad entity here."))
	  (message "No XHTML errors in the current buffer."))))))

(defun ce-validate-next-structural-error ()
  "The position of the next structural XML error.

If there is no structural XML error, return NIL."
  (let (error-position)
    (save-excursion
      (goto-char (point-min))
      (let ((err (rng-next-error 1)))
	(when err
	  (setf error-position (point))))
      (message ""))
    error-position))

(defun ce-validate-previous-error ()
  "Go to the previous structural XML error."
  (interactive)
  (rng-next-error -1))

(defun ce-validate-confirm ()
  (start-process "validate"
		 (current-buffer)
		 "/Users/alama/sources/sep/ce-el/validate.pl"
		 (buffer-file-name)))

(provide 'ce-validate)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-validate.el ends here
