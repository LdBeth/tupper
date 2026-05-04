;;;; cse.lisp --- site assignment for Algorithm 3.2 (paper §12).
;;;; Walks the formula AST and tags each branch-cutting operator
;;;; occurrence with a :site keyword whose value is the low bit
;;;; position of the site group reserved for that occurrence.
;;;; Two occurrences with equal head and equal arg list share a
;;;; site (the "occurs more than once with the same arguments" CSE
;;;; condition from the paper).
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defparameter *branch-cutting-ops*
  '(floor ceiling round truncate sgn tan /)
  "Operator heads that consume a :site tag and emit branch-tagged
   pieces.  `mod` is NOT in this list -- it is desugared into a
   composition of (/ floor) before site assignment, so its
   inner ops pick up sites individually.")

(defparameter *ops-arity*
  '((floor . 1) (ceiling . 1) (round . 1) (truncate . 1)
    (/ . 1) (tan . 1) (sgn . 2))
  "Number of bit-positions reserved per occurrence.  Sgn needs 2
   bits to distinguish its three pieces (-1, 0, +1); the others
   produce at most 2 pieces and need 1 bit.")

(defun assign-sites (formula)
  "Walk FORMULA and rewrite every branch-cutting node to carry a
   trailing :site N tag.  Two nodes with equal head and equal arg
   list share a site (Algorithm 3.2 / paper §12: 'occurs more than
   once with the same arguments').  Returns
   (values annotated-formula site-count).  Idempotent under repeat
   application."
  (let ((table (make-hash-table :test 'equal))
        (next  0))
    (labels ((rewrite (node)
               (cond
                 ((atom node) node)
                 ;; mod desugaring: rewrite first, then recurse, so
                 ;; the inner floor and / each pick up their own sites.
                 ((eq (car node) 'mod)
                  (let ((a (rewrite (second node)))
                        (b (rewrite (third node))))
                    (rewrite `(- ,a (* ,b (floor (/ ,a ,b)))))))
                 (t
                  (let* ((head      (car node))
                         (clean     (nth-value 0 (%extract-site (cdr node))))
                         (rec-args  (mapcar #'rewrite clean)))
                    (cond
                      ((member head *branch-cutting-ops*)
                       (let* ((key  (cons head rec-args))
                              (site (or (gethash key table)
                                        (setf (gethash key table)
                                              (prog1 next
                                                (incf next
                                                      (cdr (assoc head *ops-arity*))))))))
                         `(,head ,@rec-args :site ,site)))
                      (t (cons head rec-args))))))))
      (values (rewrite formula) next))))
