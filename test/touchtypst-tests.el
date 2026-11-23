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

(defvar touchtypst-tests--tab-mode nil)

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

(provide 'touchtypst-tests)
;;; touchtypst-tests.el ends here
