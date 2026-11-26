;;; org-touchtypst.el --- TouchTypst input in Org export fragments -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (touchtypst "0.1.0"))
;; Homepage: https://github.com/skissue/touchtypst.el
;; Keywords: convenience, text

;;; Commentary:

;; Requires Emacs with Tree-sitter support and the Typst grammar installed.
;;
;; Enable with (add-hook 'org-mode-hook #'org-touchtypst-mode).
;; Type Typst math directly in @@typst:$...$@@ or in a #+begin_export
;; typst block.  As in a .typ file, the body is markup until you enter
;; an equation.  Other Org text keeps its usual bindings.

;;; Code:

(require 'org-element)
(require 'touchtypst)

(defun org-touchtypst--snippet-bounds (snippet)
  "Return the Typst body bounds of SNIPPET if they contain point."
  (when (equal (org-element-property :back-end snippet) "typst")
    (let ((beg (+ (org-element-property :begin snippet) (length "@@typst:")))
          (end (- (org-element-property :end snippet)
                  (org-element-property :post-blank snippet) 2)))
      (and (<= beg (point) end) (cons beg end)))))

(defun org-touchtypst--body-bounds ()
  "Return the Typst source bounds of the Org export fragment around point.
Both bounds are insertion positions inside the Org wrapper."
  (save-excursion
    (save-restriction
      (widen)
      (let ((element (org-element-context)))
        (pcase (org-element-type element)
          ('export-snippet (org-touchtypst--snippet-bounds element))
          ;; Org keeps snippets in citation affixes as parsed secondary values.
          ((or 'citation 'citation-reference)
           (org-element-map element 'export-snippet #'org-touchtypst--snippet-bounds nil t))
          ('export-block
           (when (equal (org-element-property :type element) "TYPST")
             (goto-char (org-element-property :post-affiliated element))
             (forward-line 1)
             (let ((beg (point)))
               (goto-char (org-element-property :end element))
               (skip-chars-backward " \t\r\n")
               ;; Keep the newline before #+end_export; inserting on the
               ;; directive line itself would damage the Org wrapper.
               (cons beg (1- (line-beginning-position)))))))))))

(defun org-touchtypst--context (pos other-end run)
  "Run a Typst query RUN when POS and optional OTHER-END are in one body."
  (save-match-data
    (when-let* ((bounds (org-touchtypst--body-bounds))
                (beg (car bounds)) (end (cdr bounds))
                ((<= (point-min) beg pos end (point-max)))
                ((or (not other-end) (<= beg other-end end))))
      (let ((body (buffer-substring-no-properties beg end)))
        (with-temp-buffer
          (insert body)
          (goto-char (+ (point-min) (- pos beg)))
          (funcall run (treesit-parser-create 'typst)))))))

(defvar org-touchtypst-mode-map
  ;; Org binds shifted ASCII TAB separately from <backtab>.
  (touchtypst--make-keymap
   (cons '("S-TAB" touchtypst-shift-tab-fallthrough touchtypst--backtab-filter) touchtypst--bindings)
   #'org-touchtypst--context)
  "Keymap for `org-touchtypst-mode'.")

;;;###autoload
(define-minor-mode org-touchtypst-mode
  "Toggle TouchTypst math input in this buffer's Org Typst export fragments."
  :lighter " OrgTT"
  :keymap org-touchtypst-mode-map
  :group 'touchtypst
  (if org-touchtypst-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq org-touchtypst-mode nil)
          (user-error "Org TouchTypst requires Org mode"))
        (unless (treesit-language-available-p 'typst)
          (setq org-touchtypst-mode nil)
          (user-error "TouchTypst requires the Typst Tree-sitter grammar"))
        (add-hook 'touchtypst--context-functions #'org-touchtypst--context nil t))
    (remove-hook 'touchtypst--context-functions #'org-touchtypst--context t)))

(provide 'org-touchtypst)
;;; org-touchtypst.el ends here
