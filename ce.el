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
(require 'nxml-mode)
(require 'rng-valid)

(eval-when (load eval)
  (message "Initializing copyeditor tools..."))

(eval-after-load 'ce
  (message "Initializing copyeditor tools...done"))

;;; Our own splitoff packages

(require 'ce-macros) ;; needs to come first among the splitoff
                     ;; packages to ensure that any uses of macros are
                     ;; properly expanded
;(require 'ce-unadorned)
;(require 'ce-validate)
;(require 'ce-quotes)

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
;;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *xhtml-latin-1-entites*
  '(
    ["160" "A0" "nbsp"]
    ["161" "A1" "iexcl"]
    ["162" "A2" "cent"]
    ["163" "A3" "pound"]
    ["164" "A4" "curren"]
    ["165" "A5" "yen"]
    ["166" "A6" "brvbar"]
    ["167" "A7" "sect"]
    ["168" "A8" "uml"]
    ["169" "A9" "copy"]
    ["170" "AA" "ordf"]
    ["171" "AB" "laquo"]
    ["172" "AC" "not"]
    ["173" "AD" "shy"]
    ["174" "AE" "reg"]
    ["175" "AF" "macr"]
    ["176" "B0" "deg"]
    ["177" "B1" "plusmn"]
    ["178" "B2" "sup2"]
    ["179" "B3" "sup3"]
    ["180" "B4" "acute"]
    ["181" "B5" "micro"]
    ["182" "B6" "para"]
    ["183" "B7" "middot"]
    ["184" "B8" "cedil"]
    ["185" "B9" "sup1"]
    ["186" "BA" "ordm"]
    ["187" "BB" "raquo"]
    ["188" "BC" "frac14"]
    ["189" "BD" "frac12"]
    ["190" "BE" "frac34"]
    ["191" "BF" "iquest"]
    ["192" "C0" "Agrave"]
    ["193" "C1" "Aacute"]
    ["194" "C2" "Acirc"]
    ["195" "C3" "Atilde"]
    ["196" "C4" "Auml"]
    ["197" "C5" "Aring"]
    ["198" "C6" "AElig"]
    ["199" "C7" "Ccedil"]
    ["200" "C8" "Egrave"]
    ["201" "C9" "Eacute"]
    ["202" "CA" "Ecirc"]
    ["203" "CB" "Euml"]
    ["204" "CC" "Igrave"]
    ["205" "CD" "Iacute"]
    ["206" "CE" "Icirc"]
    ["207" "CF" "Iuml"]
    ["208" "D0" "ETH"]
    ["209" "D1" "Ntilde"]
    ["210" "D2" "Ograve"]
    ["211" "D3" "Oacute"]
    ["212" "D4" "Ocirc"]
    ["213" "D5" "Otilde"]
    ["214" "D6" "Ouml"]
    ["215" "D7" "times"]
    ["216" "D8" "Oslash"]
    ["217" "D9" "Ugrave"]
    ["218" "DA" "Uacute"]
    ["219" "DB" "Ucirc"]
    ["220" "DC" "Uuml"]
    ["221" "DD" "Yacute"]
    ["222" "DE" "THORN"]
    ["223" "DF" "szlig"]
    ["224" "E0" "agrave"]
    ["225" "E1" "aacute"]
    ["226" "E2" "acirc"]
    ["227" "E3" "atilde"]
    ["228" "E4" "auml"]
    ["229" "E5" "aring"]
    ["230" "E6" "aelig"]
    ["231" "E7" "ccedil"]
    ["232" "E8" "egrave"]
    ["233" "E9" "eacute"]
    ["234" "EA" "ecirc"]
    ["235" "EB" "euml"]
    ["236" "EC" "igrave"]
    ["237" "ED" "iacute"]
    ["238" "EE" "icirc"]
    ["239" "EF" "iuml"]
    ["240" "F0" "eth"]
    ["241" "F1" "ntilde"]
    ["242" "F2" "ograve"]
    ["243" "F3" "oacute"]
    ["244" "F4" "ocirc"]
    ["245" "F5" "otilde"]
    ["246" "F6" "ouml"]
    ["247" "F7" "divide"]
    ["248" "F8" "oslash"]
    ["249" "F9" "ugrave"]
    ["250" "FA" "uacute"]
    ["251" "FB" "ucirc"]
    ["252" "FC" "uuml"]
    ["253" "FD" "yacute"]
    ["254" "FE" "thorn"]
    ["255" "FF" "yuml"]))

(defvar *xhtml-special-entities*
  '(
    ["34" "22" "quot"]
    ["38" "26" "amp"]
    ["60" "3C" "lt"]
    ["62" "3E" "gt"]
    ["39" "27" "apos"]
    ["338" "152" "OElig"]
    ["339" "153" "oelig"]
    ["352" "160" "Scaron"]
    ["353" "161" "scaron"]
    ["376" "178" "Yuml"]
    ["710" "2C6" "circ"]
    ["732" "2DC" "tilde"]
    ["8194" "2002" "ensp"]
    ["8195" "2003" "emsp"]
    ["8201" "2009" "thinsp"]
    ["8204" "200C" "zwnj"]
    ["8205" "200D" "zwj"]
    ["8206" "200E" "lrm"]
    ["8207" "200F" "rlm"]
    ["8211" "2013" "ndash"]
    ["8212" "2014" "mdash"]
    ["8216" "2018" "lsquo"]
    ["8217" "2019" "rsquo"]
    ["8218" "201A" "sbquo"]
    ["8220" "201C" "ldquo"]
    ["8221" "201D" "rdquo"]
    ["8222" "201E" "bdquo"]
    ["8224" "2020" "dagger"]
    ["8225" "2021" "Dagger"]
    ["8240" "2030" "permil"]
    ["8249" "2039" "lsaquo"]
    ["8250" "203A" "rsaquo"]
    ["8364" "20AC" "euro"]
    ))

(defvar *xhtml-symbol-entities*
  '(
    ["402" "192" "fnof"]
    ["913" "391" "Alpha"]
    ["914" "392" "Beta"]
    ["915" "393" "Gamma"]
    ["916" "394" "Delta"]
    ["917" "395" "Epsilon"]
    ["918" "396" "Zeta"]
    ["919" "397" "Eta"]
    ["920" "398" "Theta"]
    ["921" "399" "Iota"]
    ["922" "39A" "Kappa"]
    ["923" "39B" "Lambda"]
    ["924" "39C" "Mu"]
    ["925" "39D" "Nu"]
    ["926" "39E" "Xi"]
    ["927" "39F" "Omicron"]
    ["928" "3A0" "Pi"]
    ["929" "3A1" "Rho"]
    ["931" "3A3" "Sigma"]
    ["932" "3A4" "Tau"]
    ["933" "3A5" "Upsilon"]
    ["934" "3A6" "Phi"]
    ["935" "3A7" "Chi"]
    ["936" "3A8" "Psi"]
    ["937" "3A9" "Omega"]
    ["945" "3B1" "alpha"]
    ["946" "3B2" "beta"]
    ["947" "3B3" "gamma"]
    ["948" "3B4" "delta"]
    ["949" "3B5" "epsilon"]
    ["950" "3B6" "zeta"]
    ["951" "3B7" "eta"]
    ["952" "3B8" "theta"]
    ["953" "3B9" "iota"]
    ["954" "3BA" "kappa"]
    ["955" "3BB" "lambda"]
    ["956" "3BC" "mu"]
    ["957" "3BD" "nu"]
    ["958" "3BE" "xi"]
    ["959" "3BF" "omicron"]
    ["960" "3C0" "pi"]
    ["961" "3C1" "rho"]
    ["962" "3C2" "sigmaf"]
    ["963" "3C3" "sigma"]
    ["964" "3C4" "tau"]
    ["965" "3C5" "upsilon"]
    ["966" "3C6" "phi"]
    ["967" "3C7" "chi"]
    ["968" "3C8" "psi"]
    ["969" "3C9" "omega"]
    ["977" "3D1" "thetasym"]
    ["978" "3D2" "upsih"]
    ["982" "3D6" "piv"]
    ["8226" "2022" "bull"]
    ["8230" "2026" "hellip"]
    ["8242" "2032" "prime"]
    ["8243" "2033" "Prime"]
    ["8254" "203E" "oline"]
    ["8260" "2044" "frasl"]
    ["8472" "2118" "weierp"]
    ["8465" "2111" "image"]
    ["8476" "211C" "real"]
    ["8482" "2122" "trade"]
    ["8501" "2135" "alefsym"]
    ["8592" "2190" "larr"]
    ["8593" "2191" "uarr"]
    ["8594" "2192" "rarr"]
    ["8595" "2193" "darr"]
    ["8596" "2194" "harr"]
    ["8629" "21B5" "crarr"]
    ["8656" "21D0" "lArr"]
    ["8657" "21D1" "uArr"]
    ["8658" "21D2" "rArr"]
    ["8659" "21D3" "dArr"]
    ["8660" "21D4" "hArr"]
    ["8704" "2200" "forall"]
    ["8706" "2202" "part"]
    ["8707" "2203" "exist"]
    ["8709" "2205" "empty"]
    ["8711" "2207" "nabla"]
    ["8712" "2208" "isin"]
    ["8713" "2209" "notin"]
    ["8715" "220B" "ni"]
    ["8719" "220F" "prod"]
    ["8721" "2211" "sum"]
    ["8722" "2212" "minus"]
    ["8727" "2217" "lowast"]
    ["8730" "221A" "radic"]
    ["8733" "221D" "prop"]
    ["8734" "221E" "infin"]
    ["8736" "2220" "ang"]
    ["8743" "2227" "and"]
    ["8744" "2228" "or"]
    ["8745" "2229" "cap"]
    ["8746" "222A" "cup"]
    ["8747" "222B" "int"]
    ["8756" "2234" "there4"]
    ["8764" "223C" "sim"]
    ["8773" "2245" "cong"]
    ["8776" "2248" "asymp"]
    ["8800" "2260" "ne"]
    ["8801" "2261" "equiv"]
    ["8804" "2264" "le"]
    ["8805" "2265" "ge"]
    ["8834" "2282" "sub"]
    ["8835" "2283" "sup"]
    ["8836" "2284" "nsub"]
    ["8838" "2286" "sube"]
    ["8839" "2287" "supe"]
    ["8853" "2295" "oplus"]
    ["8855" "2297" "otimes"]
    ["8869" "22A5" "perp"]
    ["8901" "22C5" "sdot"]
    ["8968" "2308" "lceil"]
    ["8969" "2309" "rceil"]
    ["8970" "230A" "lfloor"]
    ["8971" "230B" "rfloor"]
    ["9001" "2329" "lang"]
    ["9002" "232A" "rang"]
    ["9674" "25CA" "loz"]
    ["9824" "2660" "spades"]
    ["9827" "2663" "clubs"]
    ["9829" "2665" "hearts"]
    ["9830" "2666" "diams"]
    ))

(defun member-of-some-array (thing list-of-arrays)
  (some #'(lambda (entity-array)
	    (or (string= thing (aref entity-array 0))
		(string= thing (aref entity-array 1))
		(string= thing (aref entity-array 2))))
	list-of-arrays))

(defun ce-validate-known-latin-1-entity (thing)
  (member-of-some-array thing *xhtml-latin-1-entites*))

(defun ce-validate-known-special-entity (thing)
  (member-of-some-array thing *xhtml-special-entities*))

(defun ce-validate-known-symbol (thing)
  (member-of-some-array thing *xhtml-symbol-entities*))

(defun maybe-extract-entity-name (thing)
  (string-match "\\([&]\\)?\\([a-zA-Z0-9]+\\)\\([;]\\)?" thing)
  (match-string-no-properties 2 thing))

(defun ce-validate-known-entity (thing)
  (let ((entity-name (maybe-extract-entity-name thing)))
    (or (ce-validate-known-latin-1-entity entity-name)
	(ce-validate-known-special-entity entity-name)
	(ce-validate-known-symbol entity-name))))

(defun ce-validate ()
  "Validate the current buffer."
  (interactive)
  ;; can we run rng-validate-mode?
  (when (not (and (fboundp 'rng-validate-mode)
		  (fboundp 'nxml-mode)))
    (error "Unable to run the validator (either nxml-mode or rng-validate-mode seems to be missing)."))
  ;; switch to nxml mode
  (when (not (eq major-mode 'nxml-mode))
    (nxml-mode))
  (when (not (member 'rng-validate-mode minor-mode-list))
    (rng-validate-mode))
  (let ((next-structural-error (ce-validate-next-structural-error)))
    (if next-structural-error
	(goto-char next-structural-error)
      (let ((next-entity-error (ce-validate-next-entity-error)))
	(if next-entity-error
	    (ce-validate-entities)
	  (message "XHTML is structurally valid and all entities are known."))))))

(defun ce-validate-next-entity-error ()
  (let (bad-position)
    (save-excursion
      (goto-char (point-min))
      (let* ((begin (point-min))
	     (token (xmltok-forward))
	     (end (point)))
	(while token
	  (if (eq token 'entity-ref)
	      (let ((entity (buffer-substring-no-properties begin end)))
		(if (ce-validate-known-entity entity)
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

(defun ce-validate-next-error ()
  (interactive)
  (rng-next-error 1))

(defun ce-validate-next-structural-error ()
  (let (error-position)
    (save-excursion
      (goto-char (point-min))
      (setf error-position (rng-next-error 1)))
    error-position))

(defun ce-validate-previous-error ()
  (interactive)
  (rng-next-error -1))

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
	 '("Validation"
	   ["Validate XHTML of current buffer"
	    (call-interactively 'ce-validate)
	    (fboundp 'ce-validate)]
	   ["Next error"
	    (call-interactively 'ce-validate-next-error)
	    (fboundp 'ce-validate-next-error)]
	   ["Previous error"
	    (call-interactively 'ce-validate-previous-error)
	    (fboundp 'ce-validate-previous-error)]
	   ["Check XHTML entities"
	    (call-interactively 'ce-validate-entities)
	    (fboundp 'ce-validate-entities)])
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
