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
;; Formula input and tab stops are inactive in literal text and embedded
;; code.  Dollar and space handle equation delimiters separately.
;;
;;   ` <c>     symbol entry        e.g. `a -> alpha
;;   ; <c>     wrap previous node  e.g. ;b on x  -> bar(x)
;;   _ / ^    smart subscript     inserts `_()` / `^()' with point inside
;;   $        enter/leave math    inserts `$$' or steps over the closing `$'
;;   SPC      between `$|$'       expands to `$ | $' (display math)
;;   TAB      next tab stop       jumps to next slot in current math block
;;   Shift-Tab    previous tab stop
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
    ("int" . "integral_()^()")
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

(defun touchtypst--snippet-before-point ()
  "Return (BEG END TEMPLATE) for a snippet trigger ending at point, or nil."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward "a-zA-Z")
      (when-let* ((beg (point))
                  ((< beg end))
                  (word (buffer-substring-no-properties beg end))
                  (template (cdr (assoc word touchtypst-snippet-alist))))
        (list beg end template)))))

(defun touchtypst-try-expand-snippet ()
  "If the word before point is a math snippet trigger, expand it.
Returns t on success, nil otherwise."
  (when-let* ((info (touchtypst--snippet-before-point))
              ((touchtypst-math-input-p)))
    (delete-region (nth 0 info) (nth 1 info))
    (insert (nth 2 info))
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

(defun touchtypst--math-identifier-char (char)
  "Classify CHAR as `start', `continue', or nil in a math identifier."
  ;; Approximate Typst's XID classes with Unicode categories and exceptions.
  ;; Underscore is a script operator, even though it is connector punctuation.
  (when (and char (not (eq char ?_)))
    (let ((category (get-char-code-property char 'general-category)))
      (cond
       ((or (memq category '(Lu Ll Lt Lm Lo Nl))
            (memq char '(#x1885 #x1886 #x2118 #x212e)))
        'start)
       ((or (memq category '(Mn Mc Nd Pc))
            (memq char '(#xb7 #x387 #x19da #x200c #x200d #x30fb #xff65))
            (<= #x1369 char #x1371))
        'continue)))))

(defun touchtypst--math-identifier-before-p ()
  "Return non-nil if the identifier run before point contains a starter."
  (save-excursion
    (let (kind)
      (while (eq (setq kind (touchtypst--math-identifier-char (char-before)))
                 'continue)
        (backward-char))
      (eq kind 'start))))

(defun touchtypst-symbol-dispatch ()
  "Read the next character; if it maps to a symbol in math, insert it."
  (interactive)
  (if (not (touchtypst-math-input-p))
      (insert ?`)
    (let* ((ch (read-char "Symbol `"))
           (sym (alist-get ch touchtypst-symbol-alist)))
      (if sym
          (when (> (length sym) 0)
            (when (and (touchtypst--math-identifier-char (aref sym 0))
                       (touchtypst--math-identifier-before-p))
              (insert " "))
            (insert sym)
            ;; Leave point before the separator so a suffix can still attach.
            (when (and (touchtypst--math-identifier-char (char-after))
                       (touchtypst--math-identifier-before-p))
              (save-excursion (insert " "))))
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
   ((not (touchtypst-math-input-p))
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
  (if (touchtypst-math-input-p)
      (progn (insert "_()") (backward-char 1))
    (insert ?_)))

(defun touchtypst-smart-^ ()
  "Insert `^()' in math with point inside; bare `^' otherwise."
  (interactive)
  (if (touchtypst-math-input-p)
      (progn (insert "^()") (backward-char 1))
    (insert ?^)))

(defun touchtypst--smart-dollar-action ()
  "Return `open', an equation exit position, or nil for literal dollar."
  (unless (touchtypst--literal-dollar-p)
    (if-let* ((math (touchtypst-math-block)))
        (touchtypst--math-exit-position math)
      'open)))

(defun touchtypst-smart-$ ()
  "Pair or exit an equation, or insert a literal dollar otherwise."
  (interactive)
  (let ((action (touchtypst--smart-dollar-action)))
    (cond ((eq action 'open) (insert "$$") (backward-char 1))
          (action (goto-char action))
          (t (insert ?$)))))

(defun touchtypst-smart-SPC ()
  "Pad an empty equation to make it display math, or insert one space."
  (interactive)
  (if (touchtypst--empty-math-p)
      (progn (insert "  ") (backward-char 1))
    (insert ?\s)))

;;;; Minor mode and keymap

(defun touchtypst-tab-or-expand ()
  "Expand a snippet or jump to the next stop in math."
  (interactive)
  (or (touchtypst-try-expand-snippet)
      (when (touchtypst-math-input-p)
        (touchtypst-next-stop))))

(defun touchtypst-shift-tab-fallthrough ()
  "Jump to the previous stop in math."
  (interactive)
  (when (touchtypst-math-input-p)
    (touchtypst-prev-stop)))

(defun touchtypst--math-input-filter (command)
  "Return COMMAND when point is in formula syntax."
  (when (touchtypst-math-input-p)
    command))

(defun touchtypst--dollar-filter (command)
  "Return COMMAND when dollar can open or exit an equation."
  (when (touchtypst--smart-dollar-action)
    command))

(defun touchtypst--spc-filter (command)
  "Return COMMAND when SPC can pad an empty equation."
  (when (touchtypst--empty-math-p)
    command))

(defun touchtypst--tab-filter (command)
  "Return COMMAND when Tab can expand a snippet or move to a stop."
  (when (and (touchtypst-math-input-p)
             (or (touchtypst--snippet-before-point) (touchtypst--next-stop-position)))
    command))

(defun touchtypst--backtab-filter (command)
  "Return COMMAND when Shift-Tab can move to a previous stop."
  (when (and (touchtypst-math-input-p) (touchtypst--prev-stop-position))
    command))

(defvar touchtypst-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "`")
                '(menu-item "" touchtypst-symbol-dispatch :filter touchtypst--math-input-filter))
    (define-key m (kbd ";")
                '(menu-item "" touchtypst-modifier-dispatch :filter touchtypst--math-input-filter))
    (define-key m (kbd "_")
                '(menu-item "" touchtypst-smart-_ :filter touchtypst--math-input-filter))
    (define-key m (kbd "^")
                '(menu-item "" touchtypst-smart-^ :filter touchtypst--math-input-filter))
    (define-key m (kbd "$")
                '(menu-item "" touchtypst-smart-$ :filter touchtypst--dollar-filter))
    (define-key m (kbd "SPC")
                '(menu-item "" touchtypst-smart-SPC :filter touchtypst--spc-filter))
    ;; Let Emacs translate <tab> to TAB and S-<tab> to <backtab> so that
    ;; higher-priority bindings for the translated keys still win.
    (define-key m (kbd "TAB")
                '(menu-item "" touchtypst-tab-or-expand :filter touchtypst--tab-filter))
    (define-key m (kbd "<backtab>")
                '(menu-item "" touchtypst-shift-tab-fallthrough :filter touchtypst--backtab-filter))
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
    (treesit-parser-create 'typst)))

(provide 'touchtypst)

;;; touchtypst.el ends here
