;;; touchtypst-parse.el --- Typst math context and tab stops -*- lexical-binding: t -*-

;;; Commentary:

;; Use the Typst parse tree to find math expressions and tab stops.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)

;;;; Math context

(defun touchtypst--node-ancestor (node type)
  "Return nearest ancestor (or NODE itself) whose type is TYPE, else nil."
  (let ((n node))
    (while (and n (not (equal (treesit-node-type n) type)))
      (setq n (treesit-node-parent n)))
    n))

(defun touchtypst--strictly-inside-p (node pos)
  "Return non-nil if POS is strictly inside NODE (excludes both endpoints).
This matters at delimiter boundaries: point exactly at an opening `$'
is at the start of the math node but typing there would insert outside it."
  (and node
       (< (treesit-node-start node) pos)
       (< pos (treesit-node-end node))))

(defun touchtypst-math-block (&optional pos)
  "Return the enclosing `math' node at POS (or point), or nil.
Returns nil at the exact opening or closing `$' boundary."
  (let* ((p (or pos (point)))
         (node (treesit-node-at p))
         (math (touchtypst--node-ancestor node "math")))
    (and (touchtypst--strictly-inside-p math p) math)))

(defun touchtypst-in-math-p (&optional pos)
  "Return non-nil if POS (or point) is inside a `math' node (strictly)."
  (touchtypst-math-block pos))

(defun touchtypst--call-callee-text (call-node)
  "Return text of CALL-NODE's callee child, or nil."
  (when-let* ((first (car (treesit-node-children call-node))))
    (treesit-node-text first t)))

(defun touchtypst-in-mat-row-p (&optional pos)
  "Return non-nil if POS (or point) is inside a `mat(...)' argument list."
  (let ((node (treesit-node-at (or pos (point))))
        (found nil))
    (while (and node (not found))
      (when (and (member (treesit-node-type node) '("call" "apply"))
                 (equal (touchtypst--call-callee-text node) "mat"))
        (setq found t))
      (setq node (treesit-node-parent node)))
    found))

(defun touchtypst-previous-node (&optional pos)
  "Return the smallest node ending exactly at POS (or point).
Walks up from leaf so that anonymous tokens like `)' resolve to their parent.
Returns nil if no such node exists."
  (let ((p (or pos (point))))
    (when (> p (point-min))
      (let* ((leaf (treesit-node-at (1- p)))
             (node leaf))
        ;; If leaf is anonymous (e.g. ')'), prefer parent.
        (while (and node
                    (not (treesit-node-check node 'named))
                    (treesit-node-parent node))
          (setq node (treesit-node-parent node)))
        ;; Require the chosen node to actually end at p.
        (when (and node (= (treesit-node-end node) p))
          node)))))

(defun touchtypst--previous-node-bounds ()
  "Return (BEG . END) for the expression ending at point, or nil."
  (when-let* ((node (touchtypst-previous-node)))
    (cons (treesit-node-start node) (treesit-node-end node))))

(defun touchtypst--refresh-parse ()
  "Force pending edits to be parsed before looking up new tab stops."
  (treesit-parser-root-node (car (treesit-parser-list))))

;;;; Tab stops

;; Stops are positions inside the argument slots of `call', `apply', and
;; `group' nodes -- one stop per slot, regardless of whether the slot is
;; empty.  Stops are derived fresh on every TAB; no buffer-local state.

(defun touchtypst--slot-stops (node has-callee)
  "Enumerate stop positions for slots inside NODE.
HAS-CALLEE means the first child is the callee (skip it)."
  (let ((children (treesit-node-children node))
        stops
        prev-delim-end
        slot-formula)
    (when has-callee (pop children))
    (dolist (child children)
      (let ((type (treesit-node-type child)))
        (cond
         ((and (null prev-delim-end) (member type '("(" "[")))
          (setq prev-delim-end (treesit-node-end child)
                slot-formula nil))
         ((member type '("," ";"))
          (push (or (and slot-formula (treesit-node-start slot-formula))
                    prev-delim-end)
                stops)
          (setq prev-delim-end (treesit-node-end child)
                slot-formula nil))
         ((member type '(")" "]"))
          (push (or (and slot-formula (treesit-node-start slot-formula))
                    prev-delim-end)
                stops)
          (setq prev-delim-end nil))
         ((equal type "formula")
          (setq slot-formula child)))))
    (nreverse stops)))

(defun touchtypst--node-stops (node)
  "Return list of stop positions contributed by NODE itself."
  (pcase (treesit-node-type node)
    ((or "call" "apply") (touchtypst--slot-stops node t))
    ("group"             (touchtypst--slot-stops node nil))))

(defun touchtypst--collect-stops (node)
  "Recursively collect all stops in subtree rooted at NODE, sorted ascending."
  (let (stops)
    (cl-labels ((walk (n)
                  (let ((own (touchtypst--node-stops n)))
                    (when own (setq stops (nconc own stops))))
                  (dolist (c (treesit-node-children n))
                    (walk c))))
      (walk node))
    (sort stops #'<)))

(defun touchtypst--stops-in-block ()
  "Return sorted list of stop positions in the math block at point, or nil."
  (when-let* ((block (touchtypst-math-block)))
    (touchtypst--collect-stops block)))

(defun touchtypst--next-stop-position ()
  "Return the first math stop strictly after point, or nil."
  (seq-find (lambda (p) (> p (point))) (touchtypst--stops-in-block)))

(defun touchtypst--prev-stop-position ()
  "Return the last math stop strictly before point, or nil."
  (car (last (seq-filter (lambda (p) (< p (point))) (touchtypst--stops-in-block)))))

(defun touchtypst-next-stop ()
  "Move point to the next tab stop in the current math block.
Returns t if a stop was found, nil otherwise (caller should fall through)."
  (interactive)
  (let ((next (touchtypst--next-stop-position)))
    (when next (goto-char next) t)))

(defun touchtypst-prev-stop ()
  "Move point to the previous tab stop in the current math block."
  (interactive)
  (let ((prev (touchtypst--prev-stop-position)))
    (when prev (goto-char prev) t)))

(provide 'touchtypst-parse)
;;; touchtypst-parse.el ends here
