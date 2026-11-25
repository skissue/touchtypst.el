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
;; For inline Typst export snippets and export blocks in Org, use:
;;
;;   (add-hook 'org-mode-hook #'org-touchtypst-mode)
;;
;; Formula input and tab stops are inactive in literal text and embedded
;; code.  Dollar and space handle equation delimiters separately.
;;
;;   ` <c>     layer-one symbol    e.g. `a -> alpha, `Q -> QQ
;;   `` <c>    layer-two symbol    e.g. ``N -> NN, ``Z -> ZZ, ``Q -> QQ,
;;                                      ``R -> RR, ``C -> CC
;;   ; <c>     wrap previous node  e.g. ;b on x  -> bar(x)
;;   _ / ^    smart subscript     inserts `_()` / `^()' with point inside
;;   $        enter/leave math    inserts `$$' or steps over the closing `$'
;;   SPC      between `$|$'       expands to `$ | $' (display math)
;;   TAB      next tab stop       jumps to next slot in current math block
;;   Shift-Tab    previous tab stop
;;   <word> + TAB  expand snippet (if word matches a trigger)
;;
;; Type the backticks and symbol key without spaces.  Each extra backtick
;; advances one layer, wrapping to layer one after the last configured layer.
;; Customize the ordered list `touchtypst-symbol-alist' to set all layers.
;; Each layer is an alist of (KEY . OUTPUT) pairs; add a third alist to
;; make its symbols available after three backticks.

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

(defun touchtypst--snippet-before-point (snippets)
  "Return the SNIPPETS entry for a whole math trigger ending at point."
  (when-let* ((node (touchtypst-previous-node))
              ((member (treesit-node-type node) '("ident" "letter")))
              ;; Field components and existing callees are not standalone triggers.
              ((not (member (treesit-node-type (treesit-node-parent node))
                            '("field" "call" "apply"))))
              (entry (assoc (treesit-node-text node t) snippets))
              ((cdr entry)))
    entry))

(defun touchtypst--tab-action (&optional direction)
  "Return a snippet entry or a source stop position for DIRECTION.
With nil, only look for a snippet.  With `next', prefer a snippet to a
stop.  With `previous', only look for a stop."
  (touchtypst--query
   (lambda (snippets)
     (when (touchtypst-math-input-p)
       (if (eq direction 'previous)
           (touchtypst--prev-stop-position)
         (or (touchtypst--snippet-before-point snippets)
             (and (eq direction 'next) (touchtypst--next-stop-position))))))
   touchtypst-snippet-alist))

(defun touchtypst--expand-snippet (snippet)
  "Replace the trigger ending at point with SNIPPET's template."
  (let* ((template (cdr snippet))
         (beg (- (point) (length (car snippet)))))
    (delete-region beg (point))
    (insert template)
    ;; Resolve the fragment again: its size and its tree may both have changed.
    (when-let* ((stop (touchtypst--query #'touchtypst--first-template-stop (- (point) beg))))
      (goto-char stop)))
  t)

(defun touchtypst-try-expand-snippet ()
  "If the word before point is a math snippet trigger, expand it.
Move point to the first template slot, or leave it after the inserted text.
Returns t on success, nil otherwise."
  (when-let* ((snippet (touchtypst--tab-action)))
    (touchtypst--expand-snippet snippet)))

;;;; Symbol and modifier chords

(defun touchtypst--read-chord (prompt prefix layers &optional repeat)
  "Read a key with PROMPT and return its mapping in the first of LAYERS.
When REPEAT is non-nil, each additional PREFIX key cycles to the next layer.
For an unmapped key, insert all PREFIX keys and either insert the printable
key or let the command loop handle it."
  (let ((key (read-key prompt t))
        (layer layers)
        (count 1))
    (while (and repeat (eq key prefix))
      (setq layer (or (cdr layer) layers)
            count (1+ count)
            key (read-key prompt t)))
    ;; `read-key' can return C-g as an event, e.g. in a keyboard macro.
    (when (eq key ?\C-g)
      (keyboard-quit))
    (or (alist-get key (car layer))
        (progn
          (insert (make-string count prefix))
          (if (and (characterp key)
                   (not (eq (get-char-code-property key 'general-category) 'Cc)))
              (insert key)
            (push (cons 'no-record key) unread-command-events))
          nil))))

(defcustom touchtypst-symbol-alist
  '(((?a . "alpha")    (?A . "Alpha")
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
     (?Q . "QQ"))
    ((?N . "NN") (?Z . "ZZ") (?Q . "QQ") (?R . "RR") (?C . "CC")))
  "Ordered list of symbol layers, each an alist from keys to output strings.
One backtick selects the first layer.  Each additional backtick advances
one layer, cycling to the first after the last.  Only the selected layer
is searched."
  :type '(repeat (alist :key-type character :value-type string))
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
  "Read a symbol key in math, cycling layers on additional backticks."
  (interactive)
  (if (not (touchtypst--query #'touchtypst-math-input-p))
      (insert ?`)
    (when-let* ((sym (touchtypst--read-chord "Symbol `" ?` touchtypst-symbol-alist t))
                ((> (length sym) 0)))
      (when (and (touchtypst--math-identifier-char (aref sym 0))
                 (touchtypst--query #'touchtypst--math-identifier-before-p))
        (insert " "))
      (insert sym)
      ;; Leave point before the separator so a suffix can still attach.
      (when (touchtypst--query
             (lambda () (and (touchtypst--math-identifier-char (char-after))
                             (touchtypst--math-identifier-before-p))))
        (save-excursion (insert " "))))))

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
  "Read next key; wrap previous node (or region) with the mapped function.
Falls through to literal `;' insertion outside math or inside `mat(...)'."
  (interactive)
  (if (not (touchtypst--query (lambda () (and (touchtypst-math-input-p)
                                              (not (touchtypst-in-mat-row-p))))))
      (insert ?\;)
    (when-let* ((fn (touchtypst--read-chord "Modifier ;" ?\; (list touchtypst-modifier-alist))))
      (if (use-region-p)
          (progn
            (touchtypst--wrap-region (region-beginning) (region-end) fn)
            (deactivate-mark))
        (if-let* ((beg (touchtypst--query #'touchtypst--previous-operand-start)))
            (touchtypst--wrap-region beg (point) fn)
          (insert fn "()")
          (backward-char 1))))))

;;;; Smart insertion

(defun touchtypst-smart-_ ()
  "Insert `_()' in math with point inside; bare `_' otherwise."
  (interactive)
  (if (touchtypst--query #'touchtypst-math-input-p)
      (progn (insert "_()") (backward-char 1))
    (insert ?_)))

(defun touchtypst-smart-^ ()
  "Insert `^()' in math with point inside; bare `^' otherwise."
  (interactive)
  (if (touchtypst--query #'touchtypst-math-input-p)
      (progn (insert "^()") (backward-char 1))
    (insert ?^)))

(defun touchtypst--smart-dollar-action ()
  "Return `open', an equation exit position, or nil to fall through."
  (unless (touchtypst--literal-dollar-p)
    (if-let* ((math (touchtypst-math-block)))
        (touchtypst--math-exit-position math)
      (unless (touchtypst--unfinished-math-p)
        'open))))

(defun touchtypst-smart-$ ()
  "Pair or exit an equation, or insert a literal dollar otherwise."
  (interactive)
  (let ((action (touchtypst--query #'touchtypst--smart-dollar-action)))
    (cond ((eq action 'open) (insert "$$") (backward-char 1))
          (action (goto-char action))
          (t (insert ?$)))))

(defun touchtypst-smart-SPC ()
  "Pad an empty equation to make it display math, or insert one space."
  (interactive)
  (if (touchtypst--query #'touchtypst--empty-math-p)
      (progn (insert "  ") (backward-char 1))
    (insert ?\s)))

;;;; Minor mode and keymap

(defun touchtypst-tab-or-expand ()
  "Expand a snippet or jump to the next stop in math."
  (interactive)
  (when-let* ((action (touchtypst--tab-action 'next)))
    (if (consp action) (touchtypst--expand-snippet action)
      (goto-char action) t)))

(defun touchtypst-shift-tab-fallthrough ()
  "Jump to the previous stop in math."
  (interactive)
  (when-let* ((stop (touchtypst--tab-action 'previous)))
    (goto-char stop) t))

(defun touchtypst--math-input-filter (command)
  "Return COMMAND when point is in formula syntax."
  (when (touchtypst--query #'touchtypst-math-input-p)
    command))

(defun touchtypst--dollar-filter (command)
  "Return COMMAND when dollar can open or exit an equation."
  (when (touchtypst--query #'touchtypst--smart-dollar-action)
    command))

(defun touchtypst--spc-filter (command)
  "Return COMMAND when SPC can pad an empty equation."
  (when (touchtypst--query #'touchtypst--empty-math-p)
    command))

(defun touchtypst--tab-filter (command)
  "Return COMMAND when Tab can expand a snippet or move to a stop."
  (when (touchtypst--tab-action 'next)
    command))

(defun touchtypst--backtab-filter (command)
  "Return COMMAND when Shift-Tab can move to a previous stop."
  (when (touchtypst--tab-action 'previous)
    command))

(defconst touchtypst--bindings
  '(("`" touchtypst-symbol-dispatch touchtypst--math-input-filter)
    (";" touchtypst-modifier-dispatch touchtypst--math-input-filter)
    ("_" touchtypst-smart-_ touchtypst--math-input-filter)
    ("^" touchtypst-smart-^ touchtypst--math-input-filter)
    ("$" touchtypst-smart-$ touchtypst--dollar-filter)
    ("SPC" touchtypst-smart-SPC touchtypst--spc-filter)
    ;; Let Emacs translate <tab> and S-<tab> so higher-priority bindings win.
    ("TAB" touchtypst-tab-or-expand touchtypst--tab-filter)
    ("<backtab>" touchtypst-shift-tab-fallthrough touchtypst--backtab-filter)))

(defun touchtypst--make-keymap (bindings &optional provider)
  "Build a keymap from BINDINGS, optionally limiting its filters to PROVIDER."
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key ,command ,filter) bindings)
      (keymap-set map key
                  `(menu-item "" ,command :filter
                              ,(if (not provider) filter
                                 (lambda (cmd)
                                   (let ((touchtypst--context-functions nil)
                                         (touchtypst--context-function provider))
                                     (funcall filter cmd)))))))
    map))

(defvar touchtypst-mode-map (touchtypst--make-keymap touchtypst--bindings)
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
