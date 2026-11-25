;;; org-touchtypst-tests.el --- Org fragment input tests -*- lexical-binding: t -*-

(require 'ert)
(require 'org)
(require 'org-touchtypst)

(defmacro org-touchtypst-tests--with-source (source &rest body)
  "Run BODY in an Org buffer containing SOURCE with point marked by │."
  (declare (indent 1))
  `(save-window-excursion
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "│")
       (delete-char -1)
       (org-mode)
       (switch-to-buffer (current-buffer))
       ,@body)))

(ert-deftest org-touchtypst-edits-fragment-source ()
  (dolist (case `(("π @@typst:$broken@@ @@typst:$fr│$@@ end" "<tab> x TAB y"
                   "π @@typst:$broken@@ @@typst:$frac(x,y│ )$@@ end")
                  ("#+name: eq\n  #+BEGIN_EXPORT Typst\n    $ fr│ $\n  #+END_EXPORT\n\n"
                   "TAB ` a TAB z <backtab> <backtab>"
                   "#+name: eq\n  #+BEGIN_EXPORT Typst\n    $ frac(│alpha,z ) $\n  #+END_EXPORT\n\n")
                  ("@@typst:│@@" "$ SPC x $" "@@typst:$ x $│@@")
                  ("#+begin_export typst\n│\n#+end_export" "$ SPC x ; b $"
                   "#+begin_export typst\n$ bar(x) $│\n#+end_export")
                  ("[cite:@paper, @@typst:$p$@@ and @@typst:$x│$@@]" "_"
                   ,(if (memq 'export-snippet (org-element-restriction 'citation-reference))
                        "[cite:@paper, @@typst:$p$@@ and @@typst:$x_(│)$@@]"
                      ;; Org 9.6 does not parse export snippets in citation affixes.
                      "[cite:@paper, @@typst:$p$@@ and @@typst:$x_│$@@]"))
                  ("@@typst:$\"a│\"$@@" "_" "@@typst:$\"a_│\"$@@")))
    (pcase-let ((`(,source ,keys ,expected) case))
      (ert-info ((format "%s in %S" keys source))
        (org-touchtypst-tests--with-source source
          (org-touchtypst-mode 1)
          (execute-kbd-macro (kbd keys))
          (should (equal expected
                         (concat (buffer-substring-no-properties (point-min) (point))
                                 "│" (buffer-substring-no-properties (point) (point-max)))))
          (should-not (treesit-parser-list)))))))

(ert-deftest org-touchtypst-leaves-org-bindings-alone ()
  (dolist (source '("* heading│\nbody" "| table │ | cell |" "ordinary $fr│$"
                    "@@typst│:$x$@@" "@@typst:$x$@│@" "@@typst:$x$@@ │ tail"
                    "@@typst:$fr│$" "@@html:$fr│$@@" "=@@typst:$fr│$@@="
                    "[cite:@@typst:$fr│$@@ cf. @paper]" ; Org parses this as a key.
                    "[cite:@paper, @@typst:$x$@@ and @@html:$fr│$@@]"
                    "#+caption: │math\n#+begin_export typst\n$x$\n#+end_export"
                    "#+begin_export typst│\n$x$\n#+end_export"
                    "#+begin_export typst\n$x$\n│  #+end_export\n"
                    "#+begin_export typst\n│#+end_export" "#+begin_export typst\n$fr│$"
                    "#+begin_src typst\n$fr│$\n#+end_src"))
    (ert-info (source)
      (org-touchtypst-tests--with-source source
        (let* ((keys (mapcar #'kbd '("`" ";" "_" "^" "$" "SPC" "TAB" "<backtab>")))
               (original (mapcar #'key-binding keys)))
          (org-touchtypst-mode 1)
          (should (equal (mapcar #'key-binding keys) original)))))))

(ert-deftest org-touchtypst-selection-and-mode-boundaries ()
  (org-touchtypst-tests--with-source "@@typst:$p$@@ text @@typst:$a+b│$@@"
    (let ((transient-mark-mode t)
          (original (key-binding (kbd ";")))
          (parser (treesit-parser-create 'typst)))
      (org-touchtypst-mode 1)
      (push-mark 10 t t) ; In the other Typst fragment.
      (should (eq (key-binding (kbd ";")) original))
      (set-mark (- (point) 3))
      (execute-kbd-macro (kbd "; b"))
      (should (equal (buffer-string) "@@typst:$p$@@ text @@typst:$bar(a+b)$@@"))
      (org-touchtypst-mode -1)
      (should (eq (key-binding (kbd ";")) original))
      (should (equal (treesit-parser-list) (list parser))))))

(ert-deftest org-touchtypst-shift-tab-in-table ()
  (org-touchtypst-tests--with-source "| left | @@typst:$frac(a,b│)$@@ |\n"
    (org-touchtypst-mode 1)
    (dolist (text '("b)" "a,b" "left"))
      (execute-kbd-macro (kbd "S-TAB"))
      (should (looking-at-p (regexp-quote text))))))

(ert-deftest org-touchtypst-preserves-another-provider ()
  (org-touchtypst-tests--with-source "#+begin_src typst\n$x│$\n#+end_src\n\n@@typst:$y$@@"
    (touchtypst-mode 1)
    (setq-local touchtypst--context-function
                (lambda (_pos _other-end run)
                  (when (org-in-src-block-p)
                    (save-restriction
                      (narrow-to-region (line-beginning-position) (line-end-position))
                      (funcall run (treesit-parser-create 'typst))))))
    (dolist (enable '(nil 1 1 -1 -1))
      (when enable (org-touchtypst-mode enable))
      (goto-char (point-min))
      (search-forward "$x")
      (execute-kbd-macro "_")
      (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "$x_()$"))
      (delete-region (- (point) 2) (1+ (point)))
      (search-forward "$y")
      (should (eq (eq (key-binding "_") 'touchtypst-smart-_) (eq enable 1))))
    ;; The Org keymap alone must not claim the other provider's source block.
    (touchtypst-mode -1)
    (goto-char (point-min))
    (search-forward "$x")
    (let ((original (key-binding "_")))
      (org-touchtypst-mode 1)
      (should (eq (key-binding "_") original)))))

(ert-deftest org-touchtypst-keeps-literals-in-claimed-fragment ()
  (org-touchtypst-tests--with-source "@@typst:`$x│$`@@"
    ;; A second provider sees the inner $x$ without the surrounding backticks.
    (setq-local touchtypst--context-function
                (lambda (pos _other-end run)
                  (let ((text (buffer-substring-no-properties (- pos 2) (1+ pos))))
                    (with-temp-buffer
                      (insert text)
                      (backward-char)
                      (funcall run (treesit-parser-create 'typst))))))
    (touchtypst-mode 1)
    (should (eq (key-binding "_") 'touchtypst-smart-_))
    (org-touchtypst-mode 1)
    (execute-kbd-macro "_")
    (should (equal (buffer-string) "@@typst:`$x_$`@@"))))

(ert-deftest org-touchtypst-isolates-native-key-customizations ()
  (org-touchtypst-tests--with-source "* heading│"
    (let ((org-return (key-binding (kbd "RET")))
          (native-return (keymap-lookup touchtypst-mode-map "RET")))
      (unwind-protect
          (progn
            (keymap-set touchtypst-mode-map "RET" #'newline)
            (org-touchtypst-mode 1)
            (should (eq (key-binding (kbd "RET")) org-return)))
        (keymap-set touchtypst-mode-map "RET" native-return)))))

(provide 'org-touchtypst-tests)
;;; org-touchtypst-tests.el ends here
