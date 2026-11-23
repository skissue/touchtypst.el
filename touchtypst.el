;;; touchtypst.el --- Fast Typst math input -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/skissue/touchtypst
;; Keywords: convenience, text


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; A cdlatex-inspired fast input system for Typst math, built on tree-sitter.
;; Requires Emacs with Tree-sitter support and the Typst grammar installed.
;;
;; Enable the minor mode in a Typst buffer.  If you use the optional
;; `typst-ts-mode', you can enable it automatically:
;;
;;   (add-hook 'typst-ts-mode-hook #'touchtypst-mode)
;;
;; Triggers (only active inside `$...$'):
;;
;;   ` <c>     symbol entry        e.g. `a -> alpha
;;   ; <c>     wrap previous node  e.g. ;b on x  -> bar(x)
;;   _ / ^    smart subscript     inserts `_()` / `^()' with point inside
;;   $        enter/leave math    inserts `$$' or steps over the closing `$'
;;   SPC      between `$|$'       expands to `$ | $' (display math)
;;   TAB      next tab stop       jumps to next slot in current math block
;;   S-TAB    previous tab stop
;;   <word> + TAB  expand snippet (if word matches a trigger)

;;; Code:

(require 'touchtypst-parse)

(defgroup touchtypst nil
  "Fast input system for Typst math."
  :group 'typst
  :prefix "touchtypst-")

;;;; Snippets

(defcustom touchtypst-snippet-alist
  '(("fr"  . "frac(, )")
    ("sq"  . "sqrt()")
    ("rt"  . "root(, )")
    ("sm"  . "sum_()^()")
    ("pr"  . "product_()^()")
    ("in"  . "integral_()^()")
    ("lm"  . "lim_()")
    ("mt"  . "mat(, ; , )")
    ("vc"  . "vec(, )")
    ("cs"  . "cases(, )")
    ("bn"  . "binom(, )")
    ("ab"  . "abs()")
    ("nm"  . "norm()"))
  "Alist mapping trigger words to template strings.
Triggered by typing the word and pressing TAB while in math context."
  :type '(alist :key-type string :value-type string)
  :group 'touchtypst)

(defun touchtypst--word-before-point ()
  "Return (BEG END WORD) for the alpha word ending at point, or nil."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward "a-zA-Z")
      (let ((beg (point)))
        (when (< beg end)
          (list beg end (buffer-substring-no-properties beg end)))))))

(defun touchtypst-try-expand-snippet ()
  "If the word before point is a snippet trigger, expand it.
Returns t on success, nil otherwise."
  (when-let* ((info (touchtypst--word-before-point))
              (template (cdr (assoc (nth 2 info) touchtypst-snippet-alist))))
    (delete-region (nth 0 info) (nth 1 info))
    (insert template)
    ;; Start the search at the beginning of the inserted text.
    (goto-char (nth 0 info))
    (touchtypst--refresh-parse)
    (touchtypst-next-stop)
    t))

;;;; Symbol and modifier chords

(defcustom touchtypst-symbol-alist
  '((?a . "alpha")    (?A . "Alpha")
    (?b . "beta")     (?B . "Beta")
    (?g . "gamma")    (?G . "Gamma")
    (?d . "delta")    (?D . "Delta")
    (?e . "epsilon")  (?E . "Epsilon")
    (?z . "zeta")     (?Z . "Zeta")
    (?h . "eta")      (?H . "Eta")
    (?t . "theta")    (?T . "Theta")
    (?k . "kappa")    (?K . "Kappa")
    (?l . "lambda")   (?L . "Lambda")
    (?m . "mu")       (?M . "Mu")
    (?n . "nu")       (?N . "Nu")
    (?x . "xi")       (?X . "Xi")
    (?p . "pi")       (?P . "Pi")
    (?r . "rho")      (?R . "Rho")
    (?s . "sigma")    (?S . "Sigma")
    (?u . "upsilon")  (?U . "Upsilon")
    (?f . "phi")      (?F . "Phi")
    (?c . "chi")      (?C . "Chi")
    (?y . "psi")      (?Y . "Psi")
    (?o . "omega")    (?O . "Omega")
    (?i . "in")
    (?8 . "infinity")
    (?0 . "emptyset")
    (?N . "NN") (?Z . "ZZ") (?Q . "QQ") (?R . "RR") (?C . "CC"))
  "Alist mapping char (after `) to Typst symbol identifier."
  :type '(alist :key-type character :value-type string)
  :group 'touchtypst)

(defun touchtypst-symbol-dispatch ()
  "Read the next character; if it maps to a symbol in math, insert it."
  (interactive)
  (if (not (touchtypst-in-math-p))
      (insert ?`)
    (let* ((ch (read-char "Symbol `"))
           (sym (alist-get ch touchtypst-symbol-alist)))
      (if sym
          (insert sym)
        (insert ?` ch)))))

(defcustom touchtypst-modifier-alist
  '((?b . "bar")
    (?h . "hat")
    (?v . "arrow")
    (?t . "tilde")
    (?d . "dot")
    (?D . "dot.double")
    (?B . "bold")
    (?I . "italic")
    (?c . "cal")
    (?f . "frak")
    (?s . "sans")
    (?m . "mono")
    (?u . "underline")
    (?o . "overline"))
  "Alist mapping char (after `;') to Typst wrap function name."
  :type '(alist :key-type character :value-type string)
  :group 'touchtypst)

(defun touchtypst--wrap-region (beg end fn-name)
  "Wrap region [BEG, END) as `FN-NAME(...)' in place.
Point ends up after the closing paren."
  (save-excursion
    (goto-char end)
    (insert ")"))
  (save-excursion
    (goto-char beg)
    (insert fn-name "("))
  (goto-char (+ end (length fn-name) 2)))  ; "fn(" + ")"

(defun touchtypst-modifier-dispatch ()
  "Read next char; wrap previous node (or region) with the mapped function.
Falls through to literal `;' insertion outside math or inside `mat(...)'."
  (interactive)
  (cond
   ((not (touchtypst-in-math-p))
    (insert ?\;))
   ((touchtypst-in-mat-row-p)
    (insert ?\;))
   (t
    (let* ((ch (read-char "Modifier ;"))
           (fn (alist-get ch touchtypst-modifier-alist)))
      (cond
       ((null fn)
        (insert ?\; ch))
       ((use-region-p)
        (touchtypst--wrap-region (region-beginning) (region-end) fn)
        (deactivate-mark))
       (t
        (let ((bounds (touchtypst--previous-node-bounds)))
          (if bounds
              (touchtypst--wrap-region (car bounds) (cdr bounds) fn)
            ;; No previous node: insert `fn()' and place point inside.
            (insert fn "()")
            (backward-char 1)))))))))

;;;; Smart insertion

(defun touchtypst-smart-_ ()
  "Insert `_()' in math with point inside; bare `_' otherwise."
  (interactive)
  (if (touchtypst-in-math-p)
      (progn (insert "_()") (backward-char 1))
    (insert ?_)))

(defun touchtypst-smart-^ ()
  "Insert `^()' in math with point inside; bare `^' otherwise."
  (interactive)
  (if (touchtypst-in-math-p)
      (progn (insert "^()") (backward-char 1))
    (insert ?^)))

(defun touchtypst-smart-$ ()
  "Insert `$$' with point inside, or step over the closing `$' of math.
Elsewhere inside math, insert a single `$'."
  (interactive)
  (if-let* ((math (touchtypst-math-block)))
      (let ((closer (treesit-node-child math -1)))
        (if (and closer
                 (equal (treesit-node-type closer) "$")
                 (= (treesit-node-start closer) (point)))
            (goto-char (treesit-node-end closer))
          (insert ?$)))
    (insert "$$")
    (backward-char 1)))

(defun touchtypst-smart-SPC ()
  "If point is exactly between two `$', expand to `$ | $' (display math).
Otherwise insert a normal space."
  (interactive)
  (if (and (> (point) (point-min))
           (< (point) (point-max))
           (eq (char-before) ?$)
           (eq (char-after)  ?$))
      (progn (insert "  ") (backward-char 1))
    (insert ?\s)))

;;;; Minor mode and keymap

(defun touchtypst-tab-or-expand ()
  "TAB: try snippet expansion in math; else jump to next stop; else indent."
  (interactive)
  (cond
   ((and (touchtypst-in-math-p) (touchtypst-try-expand-snippet)))
   ((and (touchtypst-in-math-p) (touchtypst-next-stop)))
   (t (call-interactively #'indent-for-tab-command))))

(defun touchtypst-shift-tab-fallthrough ()
  "S-TAB: jump to previous stop in math; else no-op."
  (interactive)
  (unless (and (touchtypst-in-math-p) (touchtypst-prev-stop))
    (ignore)))

(defvar touchtypst-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "`")          #'touchtypst-symbol-dispatch)
    (define-key m (kbd ";")          #'touchtypst-modifier-dispatch)
    (define-key m (kbd "_")          #'touchtypst-smart-_)
    (define-key m (kbd "^")          #'touchtypst-smart-^)
    (define-key m (kbd "$")          #'touchtypst-smart-$)
    (define-key m (kbd "SPC")        #'touchtypst-smart-SPC)
    (define-key m (kbd "TAB")        #'touchtypst-tab-or-expand)
    (define-key m (kbd "<tab>")      #'touchtypst-tab-or-expand)
    (define-key m (kbd "<backtab>")  #'touchtypst-shift-tab-fallthrough)
    (define-key m (kbd "S-TAB")      #'touchtypst-shift-tab-fallthrough)
    m)
  "Keymap for `touchtypst-mode'.")

;;;###autoload
(define-minor-mode touchtypst-mode
  "Toggle touchtypst fast math input in this buffer."
  :lighter " touchtypst"
  :keymap touchtypst-mode-map
  :group 'touchtypst
  (when touchtypst-mode
    (unless (treesit-language-available-p 'typst)
      (touchtypst-mode -1)
      (user-error "TouchTypst requires the Typst Tree-sitter grammar"))
    (unless (treesit-parser-list nil 'typst)
      (treesit-parser-create 'typst))))

(provide 'touchtypst)

;;; touchtypst.el ends here
