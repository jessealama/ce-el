;;; ce-quotes.el --- The variety of quotes


;;; Commentary:
;;
;; This file contains Emacs Lisp functions for working with quotes in
;; HTML files.  See 'ce-quote-fix-quotes for a list of the current
;; functionality we support.

(require 'cl)

;;; Code:

(defvar *ce-quote-sharp-quote-regexp* "[']\\(.\\)"
  "A regular expression matching the quotes we need to treat.")

(defvar *ce-quote-rsquo-entity* "&rsquo;")

(defvar *ce-quote-rdquo-entity* "&rdquo;")

(defvar *ce-quote-right-quote-entity-regexp*
  (regexp-opt (list *ce-quote-rsquo-entity*
		    *ce-quote-rdquo-entity*))
  "A regular expression matching the entities of right quotes.")

(defun ce-quote-num-sharp-quote-candidates ()
  "Count the number of sharp quotes in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (how-many *ce-quote-sharp-quote-regexp*)))

(defun ce-quote-num-quote-entity-candidates ()
  "Count the number of quote entities in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (how-many *ce-quote-right-quote-entity-regexp*)))

(defvar *ce-quote-fix-keymap*
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "e" 'edit)
    (define-key map "E" 'edit)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    map))

(defun y-n-p-or-q (message)
  "Print MESSAGE, then wait for a yes, no, pause, quit, or pause
response.  This function repeatedly prompts for a suitable
response until an acceptable one is received."
  (let (response)
    (while (not response)
      (let ((key (read-event message)))
	(setq key (vector key))
	(setq response
	      (lookup-key *ce-quote-fix-keymap* key))))
    response))

(defun ce-quote-fix-non-ascii-quotes ()
  "Fix all non-ASCII quotes in the current buffer.

The replacement is at the moment non-interactive in the sense
that suitable transformations will be applied without any user
intervention."
  (interactive)
  (dolist (bad-good (list (cons "[“]" "&ldquo;")
                          (cons "[”]" "&rdquo;")
                          (cons "[‘]" "&lsquo;")
                          (cons "[’]" "&rsquo;")))
    (destructuring-bind (quote-pattern . replacement)
        bad-good
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward quote-pattern nil t)
          (replace-match replacement nil nil)))))
  (message nil) ; clear anything that might have popped up in the minibuffer
  (message "Done replacing non-ASCII quotes with their XHTML entity equivalents.")
  t)

(defvar *ce-quote-position* nil
  "The position where we last paused editing quotes.")

(defvar *ce-quote-paused-from* nil
  "Within which of the many interactive quote fix-up functions
was repair paused for editing.")

(defun ce-quote-resolve-quote-function ()
  "Map a symbol to a quote fixup function."
  (case *ce-quote-paused-from*
    (fix-sharp-quotes 'ce-quote-fix-sharp-quotes)
    (otherwise nil)))

(defun ce-quote-fix-resume ()
  "Pick up where we left off."
  (interactive)
  (let ((function-to-resume (ce-quote-resolve-quote-function)))
    (if function-to-resume
        (progn
          (assert '(fboundp function-to-resume))
          (if *ce-quote-position*
              (let ((buffer-size (buffer-size)))
                (if (numberp *ce-quote-position*)
                    (cond ((< *ce-quote-position* 0)
                           (funcall function-to-resume))
                          ((> *ce-quote-position* (buffer-size))
                           (funcall function-to-resume))
                          (t
                           (funcall function-to-resume *ce-quote-position*)))
                  (funcall function-to-resume)))
            (funcall function-to-resume)))
      (error "Unable to resume fixing quotes because we don't know how we left off"))))

(defun ce-quote-fix-sharp-quotes (&optional starting-position)
  "Interactively fix sharp quotes (') in the current buffer.

If STARTING-POSITION is nil, then do this for the whole buffer.
Otherwise, go to STARTING-POSITION.  (It is assumed that
STARTING-POSITION makes sense as a buffer position.)"
  (interactive)
  (let ((num-candidates-remaining (ce-quote-num-sharp-quote-candidates))
        (num-fixed 0)
        (bail-out nil)
        (last-match-begin nil)
        (last-match-end nil))
    (if (zerop num-candidates-remaining)
        (message "No candidate quotes to inspect.")
      (if (> num-candidates-remaining 1)
          (message "Inspecting %d sharp quote candidates..."
                   num-candidates-remaining)
        (message "Inspecting 1 sharp quote candidate..."))
      (goto-char (cond (starting-position
                        starting-position)
                       (*ce-quote-position*
                        *ce-quote-position*)
                       (t (point-min))))
      (save-excursion
	(unwind-protect
          (while (and (not bail-out)
                      (re-search-forward *ce-quote-sharp-quote-regexp* nil t))
            (let ((match-begin (match-beginning 0))
                  (match-end (match-end 0)))
              (decf num-candidates-remaining)
              (setf last-match-begin match-begin
                    last-match-end match-end)
              (put-text-property match-begin
                                 match-end
                                 'font-lock-face
                                 'cursor)
              (goto-char match-begin)
              (let* ((after-quote-char (match-string-no-properties 1))
                     (message (concat (format "Replace '%s by &apos;%s? "
                                              after-quote-char
                                              after-quote-char)
                                      (if (= num-candidates-remaining 1)
                                          "[1 candidate remaining] "
                                        (format "[%d candidates remaining] "
                                                num-candidates-remaining))
                                      "([y]es, [n]o, [e]dit, [q]uit) ")))
                (let ((response (y-n-p-or-q message)))
                  (case response
                    (edit
                     (setf *ce-quote-paused-from* 'fix-sharp-quotes)
                     (setf *ce-quote-position* (point))
                     (setf bail-out t))
                    (skip
                     (forward-char 2))
                    (exit
                     (goto-char (point-max)))
                    (act
                     (incf num-fixed)
                     (delete-char 2)
                     (insert "&apos;")
                     (insert after-quote-char))
		    (otherwise
		     (message "Unknown response '%a'; skipping this quote..." response)
		     (forward-char 2))))
                (remove-list-of-text-properties match-begin
                                                match-end
                                                (list 'font-lock-face)))))

        ;; if the user quit during the middle of editing quotes, or if
        ;; for some reason something goes wrong, we want to ensure
        ;; that there aren't any regions in the buffer that we've
        ;; highlighted
        (progn
	  (remove-list-of-text-properties last-match-begin
                                          last-match-end
                                          (list 'font-lock-face))))))
    (when bail-out
      (message "Stopping for editing.  After editing, type C-x r RETURN to resume."))
    num-fixed))

(defun ce-quote-count-quote (quote)
  "Count the number of occurrences of QUOTE in the buffer."
  (save-excursion
    (goto-char (point-min))
    (how-many quote)))

(defun ce-quote-count-lsquo ()
  "Count the number of &lsquo; entities in the current buffer."
  (ce-quote-count-quote "&lsquo;"))

(defun ce-quote-count-rsquo ()
  "Count the number of &rsquo; entities in the current buffer."
  (ce-quote-count-quote "&rsquo;"))

(defun ce-quote-count-ldquo ()
  "Count the number of &ldquo; entities in the current buffer."
  (ce-quote-count-quote "&ldquo;"))

(defun ce-quote-count-rdquo ()
  "Count the number of &rdquo; entities in the current buffer."
  (ce-quote-count-quote "&rdquo;"))

(defun ce-quote-check-balanced ()
  "Do a quick count of the quote entities in the current buffer.
Report our findings."
  (let ((lsquo (ce-quote-count-lsquo))
        (rsquo (ce-quote-count-rsquo))
        (ldquo (ce-quote-count-ldquo))
        (rdquo (ce-quote-count-rdquo)))
    (message "In this buffer, there are:
* %d lsquo entities,
* %d rsquo entities,
* %d ldquo entities, and
* %d rdquo entities" lsquo rsquo ldquo rdquo)))

(defun ce-quote-fix-right-quote-entities ()
  "Interactiely fix right quote entities in the current buffer."
  (interactive)
  (let ((num-candidates-remaining (ce-quote-num-quote-entity-candidates)))
    (if (zerop num-candidates-remaining)
        (message "No candidate right quote entities to inspect.")
      (save-excursion
        (if (> num-candidates-remaining 1)
            (message "Inspecting %d right quote entity candidates..."
                     num-candidates-remaining)
          (message "Inspecting 1 right quote entity candidate..."))
        (goto-char (point-min))
        (while (re-search-forward *ce-quote-right-quote-entity-regexp* nil t)
          (decf num-candidates-remaining)
          (goto-char (match-beginning 0))
          ;; ugh -- magic constant.  Look at the definition of the
          ;; regular expression to see why we choose 3 here
          (let* ((after-quote-char (match-string-no-properties 3))
                 (message (concat (format "Replace '%s by &apos;%s? "
                                          after-quote-char
                                          after-quote-char)
                                  (if (= num-candidates-remaining 1)
                                      "[1 candidate remaining] "
                                    (format "[%d candidates remaining] "
                                            num-candidates-remaining))
                                  "([y]es, [n]o, [q]uit) ")))
            (let ((response (y-n-p-or-q message)))
              (case response
                (skip
                 (forward-char 2))
                (exit
                 (goto-char (point-max)))
                (act
                 ;; ugh.  Thankfully '&lsquo;' and '&rsquo;' have the
                 ;; same length.
                 (delete-char (length "&lsquo;"))
                 (insert "&apos;")
                 (insert after-quote-char))
                (otherwise
                 (message "Unknown response '%a'; skipping this quote..." response)
                 (forward-char 2))))))))))

(defun ce-quote-fix-quotes ()
  "Apply all our quote-fixing utilities.

This means:

* fixing non-ASCII quotes,
* fixing sharp quotes ('), and
* fixing right-quote entities."
  (interactive)
  ;; deal with non-ascii quotes
  (ce-quote-fix-non-ascii-quotes)
  (ce-quote-fix-sharp-quotes)
  (ce-quote-fix-right-quote-entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ce-menu
  '(list "SEP"
         '("Quotes"
           ["Inspect all quotes"
            (call-interactively 'ce-quote-fix-quotes)
            (fboundp 'ce-quote-fix-quotes)]
           ["Check quote balance"
            (call-interactively 'ce-quote-check-balanced)
            (fboundp 'ce-quote-check-balanced)]
           "-"
           ["Fix non-ASCII quotes"
            (call-interactively 'ce-quote-fix-non-ascii-quotes)
            (fboundp 'ce-quote-fix-non-ascii-quotes)]
           ["Fix sharp quotes"
            (call-interactively 'ce-quote-fix-sharp-quotes)
            (fboundp 'ce-quote-fix-sharp-quotes)]
           ["Fix right quote entities"
            (call-interactively 'ce-quote-fix-right-quote-entities)
            (fboundp 'ce-quote-fix-right-quote-entities)])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ce-mode-map nil
  "Keymap used by 'ce-mode'.")

(unless ce-mode-map
  (setf ce-mode-map (make-sparse-keymap)))

;; we define just one-key for now
(define-key ce-mode-map (kbd "C-x r RET") 'ce-quote-fix-resume)

(defun ce-initialize ()
  "Initialize our mode.

This currently is a stub; we don't do anything."
  (interactive)
  ;; do initialization stuff here

  ;; currently nothing
  )

(define-minor-mode ce-mode
  "SEP copyediting utilities"
  :lighter " CE"
  :require nil
  :global t
  :version "0.1"
  :group 'ce
  (ce-mode-menu)
  (ce-initialize))

(provide 'ce-quotes)

;;; ce-quotes.el ends here
