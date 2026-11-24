;;; touchtypst-parse.el --- Typst math context and tab stops -*- lexical-binding: t -*-

;;; Commentary:

;; Use the Typst parse tree to find math expressions and tab stops.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)

;;;; Math context

(defvar-local touchtypst--context-function nil
  "Optional function for running syntax queries on an embedded Typst fragment.
Called as (POS OTHER-END RUN) in the source buffer.  OTHER-END is the
mark of an active region, or nil.  Return nil unless POS and any OTHER-END
are editable insertion positions in the same fragment.  Otherwise call
RUN with the Typst parser, at the position corresponding to POS in a
buffer whose accessible text is the verbatim fragment.  The fragment's
interior must be editable throughout.  Return its result
without changing the source buffer, its point, or its restriction.
When nil, use the current buffer and its existing Typst parser in place.")

(defvar touchtypst--query-context nil
  "Dynamically bound (BUFFER . PARSER) for the current syntax query.")

(defun touchtypst--parser ()
  "Return the selected Typst parser or the buffer's existing one, if any."
  (if (eq (current-buffer) (car touchtypst--query-context))
      (cdr touchtypst--query-context)
    (cl-find 'typst (treesit-parser-list) :key #'treesit-parser-language)))

(defun touchtypst--query (function &rest args)
  "Query Typst syntax at point with FUNCTION and source-buffer ARGS.
FUNCTION is read-only.  An integer result is a character position and is
translated back to the source buffer; other facts are returned unchanged.
Never return a tree node."
  (let* ((pos (point))
         (run (lambda (parser)
                (let* ((touchtypst--query-context (cons (current-buffer) parser))
                       (offset (- pos (point)))
                       (result (apply function args)))
                  (if (integerp result) (+ result offset) result)))))
    (if touchtypst--context-function
        (funcall touchtypst--context-function pos (and (use-region-p) (mark)) run)
      (funcall run (touchtypst--parser)))))

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

(defun touchtypst--unfinished-math-p ()
  "Return non-nil if the local parse error shows an open equation at point."
  (let ((left (save-excursion
                ;; Recovery nodes can end before trailing whitespace.
                (skip-chars-backward " \t\r\n")
                (when (> (point) (point-min))
                  (touchtypst--node-at (1- (point)))))))
    (cl-loop for child = left then parent
             for parent = (and child (treesit-node-parent child))
             while (and parent (not (equal (treesit-node-type child) "math")))
             when (equal (treesit-node-type parent) "ERROR")
             return
             ;; The opener is a direct token in the error, not a delimiter
             ;; inside a complete equation or part of a later expression.
             (cl-loop for token = child then (treesit-node-prev-sibling token)
                      for type = (treesit-node-type token)
                      while token
                      when (member type '("$" "#" "math"))
                      return (and (equal type "$")
                                  (not (treesit-node-check token 'missing))
                                  (<= (treesit-node-end token) (point)))))))

(defun touchtypst--call-callee-text (call-node)
  "Return text of CALL-NODE's callee child, or nil."
  (when-let* ((first (car (treesit-node-children call-node))))
    (treesit-node-text first t)))

(defun touchtypst-in-mat-row-p (&optional pos)
  "Return non-nil if insertion at POS (or point) is between `mat' parentheses."
  (let ((pos (or pos (point))))
    (cl-loop for node = (touchtypst--node-at pos) then (treesit-node-parent node)
             while (and node (not (equal (treesit-node-type node) "math")))
             thereis
             (when (and (member (treesit-node-type node) '("call" "apply"))
                        (equal (touchtypst--call-callee-text node) "mat"))
               (let ((open (treesit-node-child node 1))
                     (close (treesit-node-child node -1)))
                 (and (equal (treesit-node-text open t) "(")
                      (equal (treesit-node-text close t) ")")
                      (<= (treesit-node-end open) pos (treesit-node-start close))))))))

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

(defun touchtypst--previous-operand-start ()
  "Return the start of one math operand ending at point, or nil."
  (when-let* ((node (touchtypst-previous-node)))
    ;; Include a field's receiver and a code expression's `#'.  A code
    ;; call can have a separate argument group between the leaf and `code'.
    (cl-loop for parent = (treesit-node-parent node)
             while (and parent
                        (= (treesit-node-end parent) (point))
                        (member (treesit-node-type parent) '("field" "call" "code")))
             do (setq node parent))
    (treesit-node-start node)))

;;;; Tab stops

;; Stops are positions inside the argument slots of `call', `apply', and
;; `group' nodes -- one stop per slot, regardless of whether the slot is
;; empty.  Stops are derived fresh on every TAB; no buffer-local state.

(defun touchtypst--slot-stops (node has-callee)
  "Enumerate stop positions for slots inside NODE.
HAS-CALLEE means the first child is the callee (skip it)."
  (let ((apply-p (equal (treesit-node-type node) "apply"))
        (children (treesit-node-children node))
        stops
        prev-delim-end
        slot-start)
    (when has-callee (pop children))
    ;; An apply has one formula, not one node per slot.  Look at its
    ;; immediate children only: nested expressions and literals stay intact.
    (when apply-p
      (setq children (cl-loop for child in children
                              if (equal (treesit-node-type child) "formula")
                              append (treesit-node-children child)
                              else collect child)))
    (dolist (child children)
      (let ((type (treesit-node-type child)))
        (cond
         ((and (null prev-delim-end) (member type '("(" "[")))
          (setq prev-delim-end (treesit-node-end child)
                slot-start nil))
         ((or (member type '("," ";"))
              (and apply-p (equal type "symbol")
                   (equal (treesit-node-text child t) ",")))
          (push (or slot-start prev-delim-end) stops)
          (setq prev-delim-end (treesit-node-end child)
                slot-start nil))
         ((member type '(")" "]"))
          (push (or slot-start prev-delim-end) stops)
          (setq prev-delim-end nil))
         ((and (null slot-start)
               (or (member type '("formula" "tagged"))
                   (and apply-p (not (equal type "comment")))))
          (setq slot-start (treesit-node-start child))))))
    (nreverse stops)))

(defun touchtypst--node-stops (node)
  "Return list of stop positions contributed by NODE itself."
  (pcase (treesit-node-type node)
    ((or "call" "apply") (touchtypst--slot-stops node t))
    ("group"             (touchtypst--slot-stops node nil))))

(defun touchtypst--collect-stops (node &optional beg end)
  "Recursively collect stops in subtree rooted at NODE, sorted ascending.
If BEG and END are given, only collect stops from nodes within that range."
  (let (stops)
    (cl-labels ((walk (n)
                  (when (or (not beg)
                            (and (< (treesit-node-start n) end)
                                 (< beg (treesit-node-end n))))
                    (when (or (not beg)
                              (<= beg (treesit-node-start n) (treesit-node-end n) end))
                      (setq stops (nconc (touchtypst--node-stops n) stops)))
                    (dolist (c (treesit-node-children n))
                      (walk c)))))
      (walk node))
    (sort stops #'<)))

(defun touchtypst--first-template-stop (width)
  "Return the first stop in WIDTH characters of inserted text before point."
  (let ((beg (- (point) width)))
    (when-let* (((<= (point-min) beg)) ; The insertion can change fragment boundaries.
                (block (touchtypst-math-block beg)))
      (car (touchtypst--collect-stops block beg (point))))))

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
  (let ((next (touchtypst--query #'touchtypst--next-stop-position)))
    (when next (goto-char next) t)))

(defun touchtypst-prev-stop ()
  "Move point to the previous tab stop in the current math block."
  (interactive)
  (let ((prev (touchtypst--query #'touchtypst--prev-stop-position)))
    (when prev (goto-char prev) t)))

(provide 'touchtypst-parse)
;;; touchtypst-parse.el ends here
