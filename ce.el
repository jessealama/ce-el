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

(defconst +ce-packages+
  (list 'ce-macros
	'ce-entries
	'ce-quotes
	'ce-spell
	'ce-xhtml
	'ce-dash
	'ce-validate
	'ce-entities))

(dolist (package +ce-packages+)
  (require package))

(declare-function ce-validate "ce-validate.el")
(declare-function keep-evaluating "ce-macros.el")
(declare-function ce-quote-fix-quotes "ce-quotes.el")
(declare-function ce-spell-check "ce-spell.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The whole shebang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ce-inspect-everything ()
  (interactive)
  (keep-evaluating
   (ce-quote-fix-quotes)
   (ce-spell-check)
   (ce-validate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of the minor mode and its keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ce-mode-map nil
  "Keymap used by 'ce-mode'.")

(unless ce-mode-map
  (setf ce-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "\C-c\C-n" 'ce-validate-next-error)
	  (define-key map "\C-c\C-p" 'ce-validate-previous-error)
	  (define-key map "\C-c\C-v" 'ce-validate)
	  (define-key map "\C-c\C-e" 'ce-validate-entities)
	  map)))

(defvar ce-menu
  '(list "SEP"
	 ["Check everything"
	  (call-interactively 'ce-inspect-everything)
	  (fboundp 'ce-inspect-everything)]
	 ["Inspect quotes"
	    (call-interactively 'ce-quote-fix-quotes)
	    (fboundp 'ce-quote-fix-quotes)]
	 '("Validation"
	   ["Validate XHTML of current buffer"
	    (call-interactively 'ce-validate)
	    (fboundp 'ce-validate)]
	   ["Next error"
	    (call-interactively 'ce-validate-next-error)
	    (fboundp 'ce-validate-next-error)]
	   ["Previous error"
	    (call-interactively 'ce-validate-previous-error)
	    (fboundp 'ce-validate-previous-error)])
	 '("Entities"
	   ["Rewrite entities decimally"
	    (call-interactively 'ce-entities-resolve-named-entities-decimally)
	    (fboundp 'ce-entities-resolve-named-entities-decimally)]
	   ["Name (hexi)decimal entities"
	    (call-interactively 'ce-entities-name-numeric-entities)
	    (fboundp 'ce-entities-name-numeric-entities)]
	   ["Check XHTML entities"
	    (call-interactively 'ce-validate-entities)
	    (fboundp 'ce-validate-entities)])
	 ["Inspect dashes"
	  (call-interactively 'ce-dash-inspect-dashes)
	  (fboundp 'ce-dash-inspect-dashes)]
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

;; we define just one-key for now
(define-key ce-mode-map (kbd "C-x r RET") 'ce-quote-fix-resume)

(defun ce-initialize ()
  (message "Initializing CE-mode...")
  (dolist (package +ce-packages+)
    (require package))
  (message "Initializing CE-mode...done."))

(defun ce-reload (&optional recompile)
  (interactive)
  (dolist (package +ce-packages+ t)
    (let ((package-el (format "%s.el" package)))
      (if recompile
	  (byte-compile-file package-el t)
	(load-file package-el)))))

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

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce.el ends here
