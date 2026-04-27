;;;; interval-set.lisp --- list-of-ival utilities (Algorithm 3.1).
(in-package #:tupper)

(defparameter *max-set-size* 16)

(defun ivs-merge-pair (a b)
  "Merge two ivals into one whose value bound covers both, OR-ing flags."
  (make-ival :lo (min (ival-lo a) (ival-lo b))
             :hi (max (ival-hi a) (ival-hi b))
             :def-lo (and (ival-def-lo a) (ival-def-lo b))
             :def-hi (or  (ival-def-hi a) (ival-def-hi b))
             :cont-lo (and (ival-cont-lo a) (ival-cont-lo b))
             :cont-hi (or  (ival-cont-hi a) (ival-cont-hi b))))

(defun ivs-collapse (ivs)
  "Merge an interval set into one ival (precision loss)."
  (cond ((null ivs) (list (make-undefined)))
        ((null (cdr ivs)) ivs)
        (t (list (reduce #'ivs-merge-pair ivs)))))

(defun ivs-cap (ivs)
  (if (> (length ivs) *max-set-size*)
      (ivs-collapse ivs)
      ivs))

(defun ivs-apply-unary (op ivs)
  (ivs-cap (mapcan op ivs)))

(defun ivs-apply-binary (op as bs)
  (ivs-cap (mapcan (lambda (a)
                     (mapcan (lambda (b) (funcall op a b)) bs))
                   as)))

;;; Reduce an interval-set to one bool-iv.
(defun ivs-bool-or  (vs) (bool-or* vs))
(defun ivs-bool-and (vs) (bool-and* vs))
