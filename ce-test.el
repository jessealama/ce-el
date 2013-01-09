
(require 'ert)
(require 'cl)

(require 'ce-dash)

;;; endashes

(defconst +test-answer-1+
  "Friedrich Ludwig Gottlob Frege (1848–1925) was a German")

(defun test-dashes (to-inspect answer)
  (should (null (mismatch answer (ce-dash-inspect-dashes-in-string to-inspect)))))

(ert-deftest endash-test-1 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848 - 1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-2 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848-1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-3 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848 – 1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-4 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848–1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-5 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848 — 1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-6 ()
  (test-dashes "Friedrich Ludwig Gottlob Frege (1848—1925) was a German"
	       +test-answer-1+))

(ert-deftest endash-test-7 ()
  (test-dashes "some ranges: i-iii,"
	       "some ranges: i–iii,"))

(ert-deftest endash-test-8 ()
  (test-dashes "some ranges: I-IV,"
	       "some ranges: I–IV,"))

(ert-deftest endash-test-9 ()
  (test-dashes "and XLV-LVI."
	       "and XLV–LVI."))

(ert-deftest endash-test-10 ()
  (test-dashes "Other ranges are mixed, such as 234a-235b."
	       "Other ranges are mixed, such as 234a–235b."))

(ert-deftest endash-test-11 ()
  (test-dashes "some ranges use parentheses, such as (1)-(4),"
	       "some ranges use parentheses, such as (1)–(4),"))

(ert-deftest endash-test-12 ()
  (test-dashes "some ranges use parentheses, such as (i)-(v),"
	       "some ranges use parentheses, such as (i)–(v),"))

(ert-deftest endash-test-13 ()
  (test-dashes "even letter-denoted ranges such as (a)-(d)"
	       "even letter-denoted ranges such as (a)–(d)"))

;; emdashes

(defconst +test-answer-2+
  "mathematician, logician, and philosopher—who worked at the University")

(ert-deftest emdash-test-1 ()
  (test-dashes "mathematician, logician, and philosopher - who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-2 ()
  (test-dashes "mathematician, logician, and philosopher -- who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-3 ()
  (test-dashes "mathematician, logician, and philosopher--who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-4 ()
  (test-dashes "mathematician, logician, and philosopher---who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-5 ()
  (test-dashes "mathematician, logician, and philosopher – who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-6 ()
  (test-dashes "mathematician, logician, and philosopher–who worked at the University"
	       +test-answer-2+))

(ert-deftest emdash-test-7 ()
  (test-dashes "mathematician, logician, and philosopher—who worked at the University"
	       +test-answer-2+))

;; dashes and whitespace

(ert-deftest whitespace-test-1 ()
  (test-dashes "Another type of emdash to test is in the context of a paragraph.  In
particular, there are cases with emdashes set at the end of a line —
in which case there is a hard return."
	       "Another type of emdash to test is in the context of a paragraph.  In
particular, there are cases with emdashes set at the end of a line—in which case there is a hard return."))

(ert-deftest whitespace-test-2 ()
  (test-dashes "Here is a sentence
— and there is the emdash at the beginning of the line."
	       "Here is a sentence—and there is the emdash at the beginning of the line."))
