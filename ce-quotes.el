;;; ce-quotes.el --- The variety of quotes

(require 'cl)

(defvar *ce-quote-sharp-quote-regexp* "[']\\(.\\)"
  "A regular expression matching the quotes we need to treat.")

(defvar *ce-quote-right-quote-entity-regexp* "\\(&rsquo;\\)|\\(&rdquo;\\)\\(.\\)"
  "A regular expression matching the entities of right quotes.")

(defun ce-quote-num-sharp-quote-candidates ()
  (save-excursion
    (goto-char (point-min))
    (how-many *ce-quote-sharp-quote-regexp*)))

(defun ce-quote-num-quote-entity-candidates ()
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
  (let ((key (read-event message)))
    (setq key (vector key))
    (lookup-key *ce-quote-fix-keymap* key)))

(defun ce-quote-fix-non-ascii-quotes ()
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
  "A symbol indicating within which of the many interactive quote
  fix-up functions was the repair paused for editing.")

(defun ce-quote-resolve-quote-function ()
  (case *ce-quote-paused-from*
    (fix-sharp-quotes 'ce-quote-fix-sharp-quotes)
    (otherwise nil)))

(defun ce-quote-fix-resume ()
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
      (error "Unable to resume fixing quotes because we don't know how we left off."))))

(defun ce-quote-fix-sharp-quotes (&optional starting-position)
  (interactive)
  (let ((num-candidates-remaining (ce-quote-num-sharp-quote-candidates))
        (num-fixed 0)
        (bail-out nil))
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
      (while (and (not bail-out)
                  (re-search-forward *ce-quote-sharp-quote-regexp* nil t))
        (let ((match-begin (match-beginning 0))
              (match-end (match-end 0)))
          (decf num-candidates-remaining)
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
            (ecase response
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
               (insert after-quote-char))))
          (remove-text-properties match-begin
                                  match-end
                                  'font-lock-face)))))
    (when bail-out
      (message "Stopping for editing.  After editing, type C-x r RETURN to resume."))
    num-fixed))

(defun ce-quote-count-quote (quote)
  (save-excursion
    (goto-char (point-min))
    (how-many quote)))

(defun ce-quote-count-lsquo ()
  (ce-quote-count-quote "&lsquo;"))

(defun ce-quote-count-rsquo ()
  (ce-quote-count-quote "&rsquo;"))

(defun ce-quote-count-ldquo ()
  (ce-quote-count-quote "&ldquo;"))

(defun ce-quote-count-rdquo ()
  (ce-quote-count-quote "&rdquo;"))

(defun ce-quote-check-balanced ()
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
              (ecase response
                (skip
                 (forward-char 2))
                (exit
                 (goto-char (point-max)))
                (act
                 ;; ugh.  Thankfully '&lsquo;' and '&rsquo;' have the
                 ;; same length.
                 (delete-char (length "&lsquo;"))
                 (insert "&apos;")
                 (insert after-quote-char))))))))))

(defun ce-quote-fix-quotes ()
  (interactive)
  ;; deal with non-ascii quotes
  (ce-quote-fix-non-ascii-quotes)
  (ce-quote-fix-sharp-quotes)
  (ce-quote-fix-right-quote-entities)
  (ce-quote-check-balanced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ce-menu
  '(list "SEP"
         '("Quotes"
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
