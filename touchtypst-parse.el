;;; touchtypst-parse.el --- Typst math context and tab stops -*- lexical-binding: t -*-

;;; Commentary:

;; Use the Typst parse tree to find math expressions and tab stops.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)

;;;; Math context

(defun touchtypst--parser ()
  "Return the buffer's existing Typst parser, or nil."
  (cl-find 'typst (treesit-parser-list) :key #'treesit-parser-language))

(defun touchtypst--node-at (pos)
  "Return the Typst leaf at POS, or nil if there is no Typst parser."
  (when-let* ((parser (touchtypst--parser)))
    (treesit-node-at pos parser)))

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
         (node (touchtypst--node-at p))
         (math (touchtypst--node-ancestor node "math")))
    (and (touchtypst--strictly-inside-p math p) math)))

(defun touchtypst-in-math-p (&optional pos)
  "Return non-nil if POS (or point) is inside a `math' node (strictly)."
  (touchtypst-math-block pos))

(defun touchtypst--literal-node-at-p (node pos)
  "Return non-nil if inserting at POS is inside literal NODE."
  (pcase (treesit-node-type node)
    ((or "raw_span" "raw_blck" "string")
     (touchtypst--strictly-inside-p node pos))
    ("comment"
     (or (touchtypst--strictly-inside-p node pos)
         ;; A line comment also covers insertion at its end.
         (and (= pos (treesit-node-end node))
              (eq (char-after (1+ (treesit-node-start node))) ?/))))))

(defun touchtypst-math-input-p (&optional pos)
  "Return non-nil if insertion at POS (or point) is in Typst math syntax."
  (let ((pos (or pos (point))))
    (when-let* (((> pos (point-min)))
                ;; Inspect the token to the left: point may be at its end.
                (left (touchtypst--node-at (1- pos))))
      (cl-loop for node = left then (treesit-node-parent node)
               while node
               ;; The closest enclosing syntax wins, including a nested equation.
               when (and (equal (treesit-node-type node) "math")
                         (touchtypst--strictly-inside-p node pos))
               return t
               when (or (touchtypst--literal-node-at-p node pos)
                        (and (equal (treesit-node-type node) "code")
                             (or (touchtypst--strictly-inside-p node pos)
                                 ;; Typing here can extend the code identifier.
                                 (and (= pos (treesit-node-end node))
                                      (equal (treesit-node-type left) "ident")))))
               return nil))))

(defun touchtypst--math-exit-position (math)
  "Return the position after MATH's closer if point can exit there."
  (let ((closer (treesit-node-child math -1)))
    (when (and (equal (treesit-node-type closer) "$")
               (not (treesit-node-check closer 'missing)))
      (let ((end (treesit-node-start closer)))
        (when (or (= (point) end)
                  ;; Display equations have whitespace after the opener.
                  (and (< (point) end)
                       (memq (char-after (1+ (treesit-node-start math)))
                             '(?\s ?\t ?\r ?\n))
                       (save-excursion
                         (skip-chars-forward " \t\r\n" end)
                         (= (point) end))))
          (treesit-node-end closer))))))

(defun touchtypst--empty-math-p ()
  "Return non-nil if point is between the delimiters of an empty equation."
  (and (eq (char-before) ?$)
       (eq (char-after) ?$)
       (when-let* ((math (touchtypst-math-block)))
         (and (= (treesit-node-start math) (1- (point)))
              (= (treesit-node-end math) (1+ (point)))
              (equal (treesit-node-type (treesit-node-child math 0)) "$")
              (equal (treesit-node-type (treesit-node-child math -1)) "$")))))

(defun touchtypst--literal-dollar-p ()
  "Return non-nil if a dollar at point would be literal, not a math opener."
  (or (save-excursion
        (cl-oddp (skip-chars-backward "\\\\")))
      (let ((pos (point)))
        (cl-loop for node = (and (> pos (point-min)) (touchtypst--node-at (1- pos)))
                 then (treesit-node-parent node)
                 while node
                 thereis (touchtypst--literal-node-at-p node pos)))))

(defun touchtypst--call-callee-text (call-node)
  "Return text of CALL-NODE's callee child, or nil."
  (when-let* ((first (car (treesit-node-children call-node))))
    (treesit-node-text first t)))

(defun touchtypst-in-mat-row-p (&optional pos)
  "Return non-nil if POS (or point) is in a `mat(...)' of this equation."
  (let ((node (touchtypst--node-at (or pos (point))))
        (found nil))
    (while (and node (not found) (not (equal (treesit-node-type node) "math")))
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
      (let* ((leaf (touchtypst--node-at (1- p)))
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
  (when-let* ((parser (touchtypst--parser)))
    (treesit-parser-root-node parser)))

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
