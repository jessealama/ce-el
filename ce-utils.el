
(require 'cl)
(require 'nxml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun member-of-some-array (thing list-of-arrays)
  "Does THING belong to any of the arrays in LIST-OF-ARRAYS?"
  (some #'(lambda (entity-array)
	    (or (string= thing (aref entity-array 0))
		(string= thing (aref entity-array 1))
		(string= thing (aref entity-array 2))))
	list-of-arrays))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *xhtml-latin-1-entites*
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

(defconst *xhtml-special-entities*
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

(defconst *xhtml-symbol-entities*
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

(defconst *decimal-code-point-for-entity*
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (triple (append *xhtml-latin-1-entites*
			    *xhtml-special-entities*
			    *xhtml-symbol-entities*))
      (let ((code-point (aref triple 0))
	    (entity (aref triple 2)))
	(setf (gethash entity hash) code-point)))
    hash))

(defun resolve-named-entities-decimally ()
  (when (not (fboundp 'nxml-mode))
    (error "We rely on the nXML parser to resolve entities, but it seems that nxml-mode is not available."))
  ;; switch to nxml mode
  (when (not (eq major-mode 'nxml-mode))
    (nxml-mode))
  (save-excursion
    (goto-char (point-min))
    (let* ((begin (point))
	   (current-xml-token (xmltok-forward))
	   (end (point)))
      (while current-xml-token
	(when (eq current-xml-token 'entity-ref)
	  (let ((data (buffer-substring-no-properties begin end)))
	    (if (string-match "^[&]\\\([[:alnum:]]+\\\)[;]$" data)
		(let* ((entity (match-string 1 data))
		       (code-point (gethash entity *decimal-code-point-for-entity*)))
		  (when code-point
		    (delete-region begin end)
		    (insert ?\& ?\# code-point ?\;)
		    (setf end (point))))
	      (error "The string '%s' appears not to be an entity." data))))
	(setf begin end
	      current-xml-token (xmltok-forward)
	      end (point))))))

(provide 'ce-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ce-utils.el ends here
