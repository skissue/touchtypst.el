;;; touchtypst-tests.el --- ERT suite for touchtypst -*- lexical-binding: t -*-

;; Run from the repository root:
;;   emacs --batch -L . -l test/touchtypst-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'touchtypst)

;;;; Fixture ------------------------------------------------------------------

(defmacro touchtypst-tests--with-buffer (initial &rest body)
  "Run BODY in a synthetic Typst buffer with INITIAL contents.
A literal `|' in INITIAL marks where point should be placed (and is
removed).  Registers a typst tree-sitter parser and pretends the minor
mode is active without invoking the major mode."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (insert ,initial)
     (goto-char (point-min))
     (when (search-forward "|" nil t) (delete-char -1))
     (treesit-parser-create 'typst)
     (setq-local touchtypst-mode t)
     ,@body))

(defun touchtypst-tests--char-after-string ()
  "Return the character after point as a string, or nil at end of buffer."
  (and (char-after) (string (char-after))))

;;;; Context ------------------------------------------------------------------

(ert-deftest touchtypst-test-in-math-inside ()
  (touchtypst-tests--with-buffer "Hello $x + |y$ world"
    (should (touchtypst-in-math-p))))

(ert-deftest touchtypst-test-in-math-outside ()
  (touchtypst-tests--with-buffer "Hello |world"
    (should-not (touchtypst-in-math-p))))

(ert-deftest touchtypst-test-in-math-at-opening-dollar ()
  (touchtypst-tests--with-buffer "before |$x$ after"
    (should-not (touchtypst-in-math-p))))

(ert-deftest touchtypst-test-in-mat-row-inside ()
  (touchtypst-tests--with-buffer "$ mat(1, 2|; 3, 4) $"
    (should (touchtypst-in-mat-row-p))))

(ert-deftest touchtypst-test-in-mat-row-not-in-frac ()
  (touchtypst-tests--with-buffer "$ frac(a|, b) $"
    (should-not (touchtypst-in-mat-row-p))))

(ert-deftest touchtypst-test-in-mat-row-outside-math ()
  (touchtypst-tests--with-buffer "plain |text"
    (should-not (touchtypst-in-mat-row-p))))

;;;; previous-node ------------------------------------------------------------

(ert-deftest touchtypst-test-prev-node-letter ()
  (touchtypst-tests--with-buffer "$ x|$"
    (let ((n (touchtypst-previous-node)))
      (should n)
      (should (equal (treesit-node-text n t) "x")))))

(ert-deftest touchtypst-test-prev-node-call ()
  (touchtypst-tests--with-buffer "$ bar(x)|$"
    (let ((n (touchtypst-previous-node)))
      (should n)
      (should (equal (treesit-node-text n t) "bar(x)")))))

(ert-deftest touchtypst-test-prev-node-smallest-after-binop ()
  (touchtypst-tests--with-buffer "$ x + y|$"
    (let ((n (touchtypst-previous-node)))
      (should n)
      (should (equal (treesit-node-text n t) "y")))))

(ert-deftest touchtypst-test-prev-node-nested-call ()
  (touchtypst-tests--with-buffer "$ frac(a, b)|$"
    (let ((n (touchtypst-previous-node)))
      (should n)
      (should (equal (treesit-node-text n t) "frac(a, b)")))))

;;;; Stop collection ----------------------------------------------------------

(ert-deftest touchtypst-test-stops-frac-filled ()
  (touchtypst-tests--with-buffer "$ frac(a|, b) $"
    (should (= 2 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-frac-empty ()
  (touchtypst-tests--with-buffer "$ frac(|, ) $"
    (should (= 2 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-mat-four-cells ()
  (touchtypst-tests--with-buffer "$ mat(1|, 2; 3, 4) $"
    (should (= 4 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-mat-empty ()
  (touchtypst-tests--with-buffer "$ mat(|, ; , ) $"
    (should (= 4 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-sum-empty-groups ()
  (touchtypst-tests--with-buffer "$ sum|_()^() $"
    (should (= 2 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-sqrt-single ()
  (touchtypst-tests--with-buffer "$ sqrt(|) $"
    (should (= 1 (length (touchtypst--stops-in-block))))))

(ert-deftest touchtypst-test-stops-no-math ()
  (touchtypst-tests--with-buffer "no math |here"
    (should-not (touchtypst--stops-in-block))))

(ert-deftest touchtypst-test-stops-bare-attach-no-stop ()
  ;; `x^n' is a plain attach with `sup: letter' -- no group, no stop.
  (touchtypst-tests--with-buffer "$ x|^n $"
    (should-not (touchtypst--stops-in-block))))

;;;; Navigation ---------------------------------------------------------------

(ert-deftest touchtypst-test-next-stop-into-frac-first-slot ()
  (touchtypst-tests--with-buffer "$ |frac(a, b) $"
    (should (touchtypst-next-stop))
    (should (eq (char-after) ?a))))

(ert-deftest touchtypst-test-next-stop-frac-first-to-second ()
  (touchtypst-tests--with-buffer "$ frac(|a, b) $"
    (should (touchtypst-next-stop))
    (should (eq (char-after) ?b))))

(ert-deftest touchtypst-test-next-stop-no-more ()
  (touchtypst-tests--with-buffer "$ frac(a, |b) $"
    (should-not (touchtypst-next-stop))))

(ert-deftest touchtypst-test-prev-stop-frac ()
  (touchtypst-tests--with-buffer "$ frac(a, |b) $"
    (should (touchtypst-prev-stop))
    (should (eq (char-after) ?a))))

(ert-deftest touchtypst-test-prev-stop-none ()
  (touchtypst-tests--with-buffer "$ |frac(a, b) $"
    (should-not (touchtypst-prev-stop))))

(ert-deftest touchtypst-test-next-stop-empty-slots ()
  (touchtypst-tests--with-buffer "$ |frac(, ) $"
    (should (touchtypst-next-stop))
    ;; First empty slot: cursor lands between '(' and ',' -> char-after is ','.
    (should (eq (char-after) ?,))
    (should (touchtypst-next-stop))
    ;; Second empty slot: cursor lands between ',' and ' )' -> char-after is space.
    (should (eq (char-after) ?\s))))

;;;; Snippet expansion --------------------------------------------------------

(ert-deftest touchtypst-test-snippet-fr-expands-to-frac ()
  (touchtypst-tests--with-buffer "$ fr|$"
    (should (touchtypst-try-expand-snippet))
    (should (string-match-p "frac(, )" (buffer-string)))))

(ert-deftest touchtypst-test-snippet-fr-lands-on-first-stop ()
  (touchtypst-tests--with-buffer "$ fr|$"
    (touchtypst-try-expand-snippet)
    ;; First empty slot is between '(' and ',' -- char-after is ','.
    (should (eq (char-after) ?,))))

(ert-deftest touchtypst-test-snippet-sm-expands-to-sum ()
  (touchtypst-tests--with-buffer "$ sm|$"
    (should (touchtypst-try-expand-snippet))
    (should (string-match-p "sum_()\\^()" (buffer-string)))))

(ert-deftest touchtypst-test-snippet-unknown-trigger ()
  (touchtypst-tests--with-buffer "$ xyz|$"
    (should-not (touchtypst-try-expand-snippet))
    (should (string= "$ xyz$" (buffer-string)))))

(ert-deftest touchtypst-test-snippet-no-word ()
  (touchtypst-tests--with-buffer "$ |$"
    (should-not (touchtypst-try-expand-snippet))))

;;;; Modifier wrapping --------------------------------------------------------

(ert-deftest touchtypst-test-wrap-letter-bar ()
  (touchtypst-tests--with-buffer "$ x|$"
    (let ((n (touchtypst-previous-node)))
      (touchtypst--wrap-region (treesit-node-start n)
                               (treesit-node-end n)
                               "bar"))
    (should (string= "$ bar(x)$" (buffer-string)))
    (should (eq (char-before) ?\)))))

(ert-deftest touchtypst-test-wrap-call-bar ()
  (touchtypst-tests--with-buffer "$ frac(a, b)|$"
    (let ((n (touchtypst-previous-node)))
      (touchtypst--wrap-region (treesit-node-start n)
                               (treesit-node-end n)
                               "bar"))
    (should (string= "$ bar(frac(a, b))$" (buffer-string)))))

(ert-deftest touchtypst-test-wrap-region-overrides-prev-node ()
  (touchtypst-tests--with-buffer "$ x + y|$"
    ;; Manually mark `x + y' (positions 3..8 in this buffer).
    (let ((beg 3) (end 8))
      (touchtypst--wrap-region beg end "bar")
      (should (string= "$ bar(x + y)$" (buffer-string))))))

;;;; Smart insertions ---------------------------------------------------------

(ert-deftest touchtypst-test-smart-_-in-math ()
  (touchtypst-tests--with-buffer "$ x|$"
    (touchtypst-smart-_)
    (should (string= "$ x_()$" (buffer-string)))
    (should (eq (char-after) ?\)))))

(ert-deftest touchtypst-test-smart-_-outside-math ()
  (touchtypst-tests--with-buffer "plain |text"
    (touchtypst-smart-_)
    (should (string= "plain _text" (buffer-string)))))

(ert-deftest touchtypst-test-smart-^-in-math ()
  (touchtypst-tests--with-buffer "$ x|$"
    (touchtypst-smart-^)
    (should (string= "$ x^()$" (buffer-string)))
    (should (eq (char-after) ?\)))))

(ert-deftest touchtypst-test-smart-dollar-outside-math ()
  (touchtypst-tests--with-buffer "|"
    (touchtypst-smart-$)
    (should (string= "$$" (buffer-string)))
    (should (= (point) 2))))

(ert-deftest touchtypst-test-smart-dollar-in-math-closes ()
  (touchtypst-tests--with-buffer "$x|$"
    (touchtypst-smart-$)
    (should (string= "$x$$" (buffer-string)))))

(ert-deftest touchtypst-test-smart-spc-between-dollars ()
  (touchtypst-tests--with-buffer "$|$"
    (touchtypst-smart-SPC)
    (should (string= "$  $" (buffer-string)))
    (should (= (point) 3))))

(ert-deftest touchtypst-test-smart-spc-not-between-dollars ()
  (touchtypst-tests--with-buffer "$ x|$"
    (touchtypst-smart-SPC)
    (should (string= "$ x $" (buffer-string)))))

(ert-deftest touchtypst-test-smart-spc-outside-anything ()
  (touchtypst-tests--with-buffer "hello|world"
    (touchtypst-smart-SPC)
    (should (string= "hello world" (buffer-string)))))

;;;; Symbol dispatch (verified via table, not read-char) ----------------------

(ert-deftest touchtypst-test-symbol-table-greek ()
  (should (equal "alpha" (alist-get ?a touchtypst-symbol-alist)))
  (should (equal "beta"  (alist-get ?b touchtypst-symbol-alist)))
  (should (equal "infinity" (alist-get ?8 touchtypst-symbol-alist))))

(ert-deftest touchtypst-test-modifier-table ()
  (should (equal "bar"   (alist-get ?b touchtypst-modifier-alist)))
  (should (equal "arrow" (alist-get ?v touchtypst-modifier-alist)))
  (should (equal "bold"  (alist-get ?B touchtypst-modifier-alist))))

;;;; End-to-end: snippet + nested smart-_ + tab navigation --------------------

(ert-deftest touchtypst-test-e2e-snippet-then-nested-script ()
  "Expand fr -> frac(, ); type x in slot 1; smart-_; type n; TAB -> slot 2."
  (touchtypst-tests--with-buffer "$ fr|$"
    ;; Expand snippet -- lands at first empty slot.
    (touchtypst-try-expand-snippet)
    (should (string-match-p "frac(, )" (buffer-string)))
    ;; Type `x' in the first slot.
    (insert "x")
    (should (string-match-p "frac(x, )" (buffer-string)))
    ;; Smart subscript -- inserts _() at point.
    (touchtypst-smart-_)
    (should (string-match-p "frac(x_(), )" (buffer-string)))
    ;; Type `n' inside the empty subscript group.
    (insert "n")
    (should (string-match-p "frac(x_(n), )" (buffer-string)))
    ;; TAB now: next stop forward is the second frac slot (after the comma).
    (should (touchtypst-next-stop))
    ;; Cursor should land at the second slot of frac.  It's empty -> char-after
    ;; is the trailing space before `)'.
    (should (eq (char-after) ?\s))))

(provide 'touchtypst-tests)
;;; touchtypst-tests.el ends here
