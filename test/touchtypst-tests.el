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
removed).  Enables `touchtypst-mode' without invoking a Typst major mode."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (insert ,initial)
     (goto-char (point-min))
     (when (search-forward "|" nil t) (delete-char -1))
     (touchtypst-mode 1)
     ,@body))

(defun touchtypst-tests--char-after-string ()
  "Return the character after point as a string, or nil at end of buffer."
  (and (char-after) (string (char-after))))

;;;; Mode activation ----------------------------------------------------------

(ert-deftest touchtypst-test-mode-reuses-existing-parser ()
  (with-temp-buffer
    (let ((parser (treesit-parser-create 'typst)))
      (touchtypst-mode 1)
      (should touchtypst-mode)
      (should (equal (treesit-parser-list) (list parser))))))

;;;; Context ------------------------------------------------------------------

(ert-deftest touchtypst-test-math-input-without-parser ()
  (with-temp-buffer
    (insert "$x$")
    (backward-char)
    (should-not (touchtypst-math-input-p))
    (should-not (touchtypst-math-block))
    (should-not (touchtypst-previous-node))
    (should-not (treesit-parser-list))))

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

(ert-deftest touchtypst-test-tab-snippet-matches-whole-math-token ()
  (let ((touchtypst-snippet-alist (append '(("s2" . "sqrt()")
                                           ("éx" . "frac(, )")
                                           ("x" . "xi"))
                                         touchtypst-snippet-alist))
        (map (make-sparse-keymap)))
    (define-key map (kbd "TAB") (lambda () (interactive) (insert "!")))
    (dolist (case '(("$fr|ac(a,b)$" . "$frac(|a,b)$")
                    ("$abc2fr|$" . "$abc2fr!|$")
                    ("$alpha.fr|$" . "$alpha.fr!|$")
                    ("$fr|.alpha$" . "$fr!|.alpha$")
                    ("$fr|()$" . "$fr(|)$")
                    ("$x|()$" . "$x(|)$")
                    ("$s2|$" . "$sqrt(|)$")
                    ("$éx|$" . "$frac(|, )$")
                    ("$x|$" . "$xi|$")))
      (ert-info ((format "TAB in %S" (car case)))
        (save-window-excursion
          (touchtypst-tests--with-buffer (car case)
            (switch-to-buffer (current-buffer))
            (use-local-map map)
            (execute-kbd-macro (kbd "TAB"))
            (insert "|")
            (should (equal (buffer-string) (cdr case)))))))))

(ert-deftest touchtypst-test-tab-snippet-uses-only-template-slots ()
  (let ((touchtypst-snippet-alist '(("aa" . "alpha")
                                   ("pad" . " alpha")
                                   ("fr" . "frac(, )")
                                   ("fx" . "f(, y)"))))
    (dolist (case '(("$aa|$" . "$alpha|$")
                    ("$aa| + frac(x,y)$" . "$alpha| + frac(x,y)$")
                    ("$frac(pad|,y)$" . "$frac( alpha|,y)$")
                    ("$frac(fr|,y)$" . "$frac(frac(|, ),y)$")
                    ("$f(x, aa|, y)$" . "$f(x, alpha|, y)$")
                    ("$vec(a,  value: fx|)$" . "$vec(a,  value: f(|, y))$")
                    ("$aaa| + frac(x,y)$" . "$aaa + frac(|x,y)$")))
      (ert-info ((format "TAB in %S" (car case)))
        (save-window-excursion
          (touchtypst-tests--with-buffer (car case)
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd "TAB"))
            (insert "|")
            (should (equal (buffer-string) (cdr case)))))))))

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

(ert-deftest touchtypst-test-equation-keys-with-default-bindings ()
  (dolist (case '(("|" "$" "$$" 2)
                  ("|" "$ x $" "$x$" 4)
                  ("|" "$ SPC x $" "$ x $" 6)
                  ("$ x| $" "$" "$ x $" 6)
                  ("$ x|  $" "$" "$ x  $" 7)
                  ("$\nx|\n$" "$" "$\nx\n$" 6)
                  ("$ x // cost\n|$" "$" "$ x // cost\n$" 14)
                  ("$ x \\\\| $" "$" "$ x \\\\ $" 9)
                  ("$x$|$y$" "$" "$x$$$$y$" 5)
                  ("$|$" "SPC" "$  $" 3)
                  ("$x$|$y$" "SPC" "$x$ $y$" 5)
                  ("plain|text" "C-u 3 SPC" "plain   text" 9)
                  ("`raw|`" "$" "`raw$`" 6)
                  ("`raw`|" "$" "`raw`$$" 7)
                  ("\\\\|" "$" "\\\\$$" 4)))
    (pcase-let ((`(,initial ,keys ,expected ,position) case))
      (ert-info ((format "%s in %S" keys initial))
        (save-window-excursion
          (touchtypst-tests--with-buffer initial
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd keys))
            (should (equal (buffer-string) expected))
            (should (= (point) position))))))))

(ert-deftest touchtypst-test-equation-keys-fall-through-to-active-keymap ()
  (let ((map (make-sparse-keymap)))
    (dolist (key '("$" "SPC"))
      (define-key map (kbd key) (lambda () (interactive) (insert "!"))))
    (dolist (case '(("$x$|$y$" "SPC" "$x$!$y$")
                    ("`$|$`" "SPC" "`$!$`")
                    ("hello|world" "SPC" "hello!world")
                    ("$ x|$" "SPC" "$ x!$")
                    ("$ x |y $" "$" "$ x !y $")
                    ("$x| $" "$" "$x! $")
                    ("$ x // cost|\n$" "$" "$ x // cost!\n$")
                    ("$ x \\| $" "$" "$ x \\! $")
                    ("$\"a|$\"$" "$" "$\"a!$\"$")
                    ("`raw|`" "$" "`raw!`")
                    ("```\nraw|\n```" "$" "```\nraw!\n```")
                    ("// note|" "$" "// note!")
                    ("\\|" "$" "\\!")))
      (pcase-let ((`(,initial ,key ,expected) case))
        (ert-info ((format "%s in %S" key initial))
          (save-window-excursion
            (touchtypst-tests--with-buffer initial
              (switch-to-buffer (current-buffer))
              (use-local-map map)
              (execute-kbd-macro (kbd key))
              (should (equal (buffer-string) expected)))))))))

(ert-deftest touchtypst-test-equation-commands-insert-literal-when-called-directly ()
  (dolist (case '(("plain|text" touchtypst-smart-SPC "plain text" 7)
                  ("$x$|$y$" touchtypst-smart-SPC "$x$ $y$" 5)
                  ("$\"a|$\"$" touchtypst-smart-$ "$\"a$$\"$" 5)
                  ("$ x // cost|\n$" touchtypst-smart-$ "$ x // cost$\n$" 13)
                  ("$ x \\| $" touchtypst-smart-$ "$ x \\$ $" 7)))
    (pcase-let ((`(,initial ,command ,expected ,position) case))
      (ert-info ((format "%s in %S" command initial))
        (touchtypst-tests--with-buffer initial
          (call-interactively command)
          (should (equal (buffer-string) expected))
          (should (= (point) position)))))))

;;;; Symbol dispatch ---------------------------------------------------------

(ert-deftest touchtypst-test-symbol-key-spacing ()
  (dolist (case '(("$|$" "` a ` b" "$alpha beta|$")
                  ("$alpha|gamma$" "` b" "$alpha beta| gamma$")
                  ("$alpha2|$" "` b" "$alpha2 beta|$")
                  ("$|2$" "` a" "$alpha| 2$")
                  ("$α|β$" "` a" "$α alpha| β$")
                  ("$|·$" "` a" "$alpha| ·$")
                  ("$·|$" "` b" "$·beta|$")
                  ("$x·|$" "` b" "$x· beta|$")
                  ("$|‿$" "` a" "$alpha| ‿$")
                  ("$‿|$" "` b" "$‿beta|$")
                  ("$x‿|$" "` b" "$x‿ beta|$")
                  ("$|beta$" "` e . a l t" "$epsilon.alt| beta$")
                  ("$|.alt$" "` e" "$epsilon|.alt$")
                  ("$|_i$" "` a" "$alpha|_i$")
                  ("$12|$" "` a" "$12alpha|$")
                  ("$alpha | beta$" "` g" "$alpha gamma| beta$")))
    (pcase-let ((`(,initial ,keys ,expected) case))
      (ert-info ((format "%s in %S" keys initial))
        (save-window-excursion
          (touchtypst-tests--with-buffer initial
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd keys))
            (insert "|")
            (should (equal (buffer-string) expected))))))))

(ert-deftest touchtypst-test-symbol-key-spacing-custom-mappings ()
  (dolist (case '(("+" "$x+|y$")
                  ("+alpha" "$x+alpha| y$")
                  ("alpha+" "$x alpha+|y$")
                  ("2" "$x 2|y$")
                  ("" "$x|y$")))
    (pcase-let ((`(,mapping ,expected) case))
      (ert-info ((format "Mapping to %S" mapping))
        (save-window-excursion
          (touchtypst-tests--with-buffer "$x|y$"
            (switch-to-buffer (current-buffer))
            (let ((touchtypst-symbol-alist `((?q . ,mapping))))
              (execute-kbd-macro (kbd "` q")))
            (insert "|")
            (should (equal (buffer-string) expected))))))))

(ert-deftest touchtypst-test-symbol-table-greek ()
  (should (equal "alpha" (alist-get ?a touchtypst-symbol-alist)))
  (should (equal "beta"  (alist-get ?b touchtypst-symbol-alist)))
  (should (equal "infinity" (alist-get ?8 touchtypst-symbol-alist))))

(ert-deftest touchtypst-test-modifier-table ()
  (should (equal "bar"   (alist-get ?b touchtypst-modifier-alist)))
  (should (equal "arrow" (alist-get ?v touchtypst-modifier-alist)))
  (should (equal "bold"  (alist-get ?B touchtypst-modifier-alist))))

(ert-deftest touchtypst-test-chord-fallback-uses-active-keymaps ()
  (let ((map (make-sparse-keymap)))
    (dolist (key '("RET" "q" "C-c x"))
      (define-key map (kbd key) (lambda () (interactive) (insert "!"))))
    (dolist (prefix '("`" ";"))
      (dolist (case '(("RET" "$x%!|$") ("<left>" "$x|%$") ("q" "$x%q|$")
                      ("é" "$x%é|$") ("C-c x" "$x%!|$") ("C-g" "$x|$")))
        (pcase-let ((`(,suffix ,expected) case))
          (ert-info ((format "%s %s" prefix suffix))
            (save-window-excursion
              (touchtypst-tests--with-buffer "$x|$"
                (switch-to-buffer (current-buffer))
                (use-local-map map)
                (let ((keys (kbd (concat prefix " " suffix))))
                  (if (equal suffix "C-g")
                      (should (eq (condition-case nil
                                    (execute-kbd-macro keys)
                                  (quit 'quit))
                                  'quit))
                    (execute-kbd-macro keys)))
                (insert "|")
                (should (equal (buffer-string) (string-replace "%" prefix expected)))))))))))

;;;; End-to-end: snippet + nested smart-_ + tab navigation --------------------

(defvar touchtypst-tests--tab-mode nil)

(ert-deftest touchtypst-test-tab-math-argument-slots ()
  (dolist (stops '(("$f(|x, y)$" "$f(x, |y)$")
                   ("$vec(|a,  delim: \"[\")$" "$vec(a,  |delim: \"[\")$")
                   ("$f(|, x,, )$" "$f(, |x,, )$" "$f(, x,|, )$" "$f(, x,,| )$")
                   ("$f(|x, (y,z), \"a,b\", \\,, q)$"
                    "$f(x, |(y,z), \"a,b\", \\,, q)$"
                    "$f(x, (|y,z), \"a,b\", \\,, q)$"
                    "$f(x, (y,z), |\"a,b\", \\,, q)$"
                    "$f(x, (y,z), \"a,b\", |\\,, q)$"
                    "$f(x, (y,z), \"a,b\", \\,, |q)$")))
    (dolist (direction `(("TAB" ,@stops) ("<backtab>" ,@(reverse stops))))
      (pcase-let ((`(,key ,initial . ,expected-stops) direction))
        (ert-info ((format "%s in %S" key initial))
          (save-window-excursion
            (touchtypst-tests--with-buffer initial
              (switch-to-buffer (current-buffer))
              (dolist (expected expected-stops)
                (execute-kbd-macro (kbd key))
                (should (equal (concat (buffer-substring-no-properties (point-min) (point))
                                       "|" (buffer-substring-no-properties (point) (point-max)))
                               expected))))))))))

(ert-deftest touchtypst-test-math-keys-with-foreign-parser-first ()
  (skip-unless (treesit-language-available-p 'json))
  (dolist (case '(("$x|$" "; b" "$bar(x)$" 8)
                  ("$mat(a|, b)$" "; b" "$mat(a;b, b)$" 9)
                  ("$fr|$" "TAB" "$frac(, )$" 7)
                  ("$frac(|a, b)$" "TAB" "$frac(a, b)$" 10)
                  ("$frac(a, |b)$" "S-<tab>" "$frac(a, b)$" 7)
                  ("$x|$" "$" "$x$" 4)
                  ("$\"x|\"$" "$" "$\"x$\"$" 5)
                  ("$|$" "SPC" "$  $" 3)))
    (pcase-let ((`(,initial ,keys ,expected ,position) case))
      (ert-info ((format "%s in %S" keys initial))
        (save-window-excursion
          (touchtypst-tests--with-buffer initial
            (treesit-parser-create 'json)
            (should (eq (treesit-parser-language (car (treesit-parser-list))) 'json))
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd keys))
            (should (equal (buffer-string) expected))
            (should (= (point) position))))))))

(ert-deftest touchtypst-test-math-input-falls-through-in-literal-and-code-contexts ()
  (let ((map (make-sparse-keymap)))
    (dolist (key '("_" "^" "`" ";" "TAB" "<backtab>"))
      (define-key map (kbd key) (lambda () (interactive) (insert "!"))))
    ;; Bracket the literal/code cases with math stops to exercise navigation too.
    (dolist (initial '("$frac(a, \"fr|\", b)$"
                       "$frac(a, /* fr| */ b)$"
                       "$frac(a, // fr|\n b)$"
                       "$frac(a, #`fr|`, b)$"
                       "$frac(a, #```\nfr|\n```, b)$"
                       "$frac(a, #foo(fr|, zz), b)$"
                       "$frac(a, #fr|, b)$"
                       "$frac(a, #[inner fr| text], b)$"
                       "$frac(a, #[inner $x$| text], b)$"
                       "plain fr|"))
      (dolist (case '(("_" "!") ("^" "!") ("` a" "!a") ("; b" "!b")
                      ("<tab>" "!") ("S-<tab>" "!")))
        (pcase-let ((`(,keys ,inserted) case))
          (ert-info ((format "%s in %S" keys initial))
            (save-window-excursion
              (touchtypst-tests--with-buffer initial
                (switch-to-buffer (current-buffer))
                (use-local-map map)
                (execute-kbd-macro (kbd keys))
                (should (equal (buffer-string)
                               (string-replace "|" inserted initial)))))))))))

(ert-deftest touchtypst-test-math-input-key-boundaries ()
  (dolist (case '(("$#foo|bar$" "_" "$#foo_bar$")
                  ("$\"fr|\"$" "TAB" "$\"fr\t\"$")
                  ("$\"fr\"|$" "_" "$\"fr\"_()$")
                  ("$|\"fr\"$" "_" "$_()\"fr\"$")
                  ("$#foo(x)|$" "^" "$#foo(x)^()$")
                  ("$ x // fr\n|$" "^" "$ x // fr\n^()$")
                  ("$#[inner $x|$ text]$" "_" "$#[inner $x_()$ text]$")
                  ("$#[inner $fr|$ text]$" "TAB" "$#[inner $frac(, )$ text]$")
                  ("$#mat([inner $x|$ text])$" "; b" "$#mat([inner $bar(x)$ text])$")))
    (pcase-let ((`(,initial ,keys ,expected) case))
      (ert-info ((format "%s in %S" keys initial))
        (save-window-excursion
          (touchtypst-tests--with-buffer initial
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd keys))
            (should (equal (buffer-string) expected))))))))

(ert-deftest touchtypst-test-tab-falls-through-to-active-keymaps ()
  (let ((minor-map (make-sparse-keymap))
        (local-map (make-sparse-keymap))
        (user-map (make-sparse-keymap))
        (original-global-map (current-global-map)))
    (define-key minor-map (kbd "<tab>") (lambda () (interactive) (insert "minor")))
    (define-key local-map (kbd "TAB") (lambda () (interactive) (insert "local")))
    (dolist (key '("<backtab>" "S-TAB"))
      (define-key user-map (kbd key) (lambda () (interactive) (insert "user"))))
    (set-keymap-parent user-map original-global-map)
    (unwind-protect
        (progn
          (use-global-map user-map)
          (dolist (case '(("fr|" "<tab>" nil "frlocal")
                          ("$x|$" "TAB" nil "$xlocal$")
                          ("$ frac(a, |b) $" "<tab>" t "$ frac(a, minorb) $")
                          ("$ frac(|a, b) $" "<backtab>" nil "$ frac(usera, b) $")
                          ("$ frac(a, |b) $" "S-TAB" nil "$ frac(a, userb) $")))
            (pcase-let ((`(,initial ,key ,minor-active ,expected) case))
              (ert-info ((format "%s in %S" key initial))
                (save-window-excursion
                  (touchtypst-tests--with-buffer initial
                    (switch-to-buffer (current-buffer))
                    (use-local-map local-map)
                    (let ((touchtypst-tests--tab-mode minor-active)
                          (minor-mode-map-alist
                           (append minor-mode-map-alist
                                   (list (cons 'touchtypst-tests--tab-mode minor-map)))))
                      (execute-kbd-macro (kbd key))
                      (should (equal (buffer-string) expected)))))))))
      (use-global-map original-global-map))))

(ert-deftest touchtypst-test-tab-respects-higher-priority-binding ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") (lambda () (interactive) (insert "!")))
    (dolist (key '("TAB" "<tab>"))
      (save-window-excursion
        (touchtypst-tests--with-buffer "$fr|$"
          (switch-to-buffer (current-buffer))
          (let ((touchtypst-tests--tab-mode t)
                (minor-mode-map-alist
                 (cons (cons 'touchtypst-tests--tab-mode map) minor-mode-map-alist)))
            (execute-kbd-macro (kbd key))
            (should (equal (buffer-string) "$fr!$"))))))))

(ert-deftest touchtypst-test-e2e-snippet-then-nested-script ()
  "Expand fr -> frac(, ); type x in slot 1; smart-_; type n; TAB -> slot 2."
  (save-window-excursion
    (touchtypst-tests--with-buffer "$ fr|$"
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "<tab>"))
      (should (equal (buffer-string) "$ frac(, )$"))
      (should (eq (char-after) ?,))
      (execute-kbd-macro (kbd "x _ n"))
      (should (equal (buffer-string) "$ frac(x_(n), )$"))
      ;; The next stop is the empty second frac slot just after `,'.
      (execute-kbd-macro (kbd "TAB"))
      (should (eq (char-before) ?,))
      (should (eq (char-after) ?\s))
      ;; The previous stop is the nested subscript slot, not the first frac slot.
      (execute-kbd-macro (kbd "S-<tab>"))
      (should (eq (char-after) ?n)))))

(ert-deftest touchtypst-test-e2e-integral-and-membership-tab ()
  (dolist (case '(("$frac(|, y)$" "` i TAB" "$frac(in, y)$" 11)
                  ("$|$" "i n t TAB" "$integral_()^()$" 12)))
    (pcase-let ((`(,initial ,keys ,expected ,position) case))
      (ert-info ((format "%s in %S" keys initial))
        (save-window-excursion
          (touchtypst-tests--with-buffer initial
            (switch-to-buffer (current-buffer))
            (execute-kbd-macro (kbd keys))
            (should (equal (buffer-string) expected))
            (should (= (point) position))))))))

(provide 'touchtypst-tests)
;;; touchtypst-tests.el ends here
