;;;; interval-set.lisp --- list-of-ival utilities (Algorithm 3.1).
(in-package #:tupper)

(defparameter *max-set-size* 32
  "Cap on the size of an interval set before merge/collapse fallback.")

(defparameter *interval-set-overflows* 0
  "Counter incremented whenever ivs-cap had to fall back to a hull collapse
   even after merging adjacent elements.  Reset before a graph run; check
   afterwards for diagnostic information.")

;;; Branch tag (Algorithm 3.2, paper §12): partial function from
;;; branch-cut sites to chosen branches.  Representation:
;;;   NIL                       == empty partial function (leaf eval)
;;;   (cut . chosen)            == cons of two non-negative integers;
;;;                                bit i of `cut` is set iff site i has
;;;                                been visited; bit positions of `chosen`
;;;                                hold the choice taken at each visited
;;;                                site.
;;; Combination is union with agreement (paper's IsAFunction):
;;;   m = a.cut & b.cut                         (sites both visited)
;;;   compatible iff (a.chosen & m) = (b.chosen & m)
;;; If incompatible, returns :conflict; callers (ivs-apply via
;;; branches-all-compatible-p) drop the operand tuple.
;;; Inside operator bodies the :conflict return is unreachable because
;;; ivs-apply pre-filters; treat it as a programmer error there.

(defun combine-branches (a b)
  (cond ((null a) b)
        ((null b) a)
        (t (let* ((ac (car a)) (ach (cdr a))
                  (bc (car b)) (bch (cdr b))
                  (m  (logand ac bc)))
             (if (= (logand ach m) (logand bch m))
                 (cons (logior ac bc) (logior ach bch))
                 :conflict)))))

(defun branches-all-compatible-p (ivs)
  "T iff folding combine-branches across operand branches never
   yields :conflict.  NIL operands are skipped (identity)."
  (let ((acc nil))
    (dolist (iv ivs t)
      (let ((b (ival-branch iv)))
        (unless (null b)
          (setf acc (if (null acc) b (combine-branches acc b)))
          (when (eq acc :conflict) (return nil)))))))

(defun ivs-merge-pair (a b)
  "Merge two ivals into one whose value bound covers both, OR-ing flags."
  (make-ival :lo (min (ival-lo a) (ival-lo b))
             :hi (max (ival-hi a) (ival-hi b))
             :def-lo (and (ival-def-lo a) (ival-def-lo b))
             :def-hi (or  (ival-def-hi a) (ival-def-hi b))
             :cont-lo (and (ival-cont-lo a) (ival-cont-lo b))
             :cont-hi (or  (ival-cont-hi a) (ival-cont-hi b))
             :branch (combine-branches (ival-branch a) (ival-branch b))))

(defun ivs-collapse (ivs)
  "Merge an interval set into one ival (precision loss)."
  (if (null ivs)
      (list (make-undefined))
      (list (reduce #'ivs-merge-pair ivs))))

(defun %flags-equal-p (a b)
  (and (eq (ival-def-lo  a) (ival-def-lo  b))
       (eq (ival-def-hi  a) (ival-def-hi  b))
       (eq (ival-cont-lo a) (ival-cont-lo b))
       (eq (ival-cont-hi a) (ival-cont-hi b))))

(defun ivs-merge-adjacent (ivs)
  "Sort by lo and merge any two ivals whose ranges overlap or touch within
   one ULP and whose flags match exactly.  Returns a fresh list."
  (cond ((or (null ivs) (null (cdr ivs))) ivs)
        (t
         (let* ((sorted (sort (copy-list ivs) #'< :key #'ival-lo))
                (out (list (first sorted))))
           (dolist (iv (rest sorted))
             (let ((top (first out)))
               (if (and (%flags-equal-p top iv)
                        (<= (ival-lo iv) (inc-ulp (ival-hi top))))
                   (setf (first out) (ivs-merge-pair top iv))
                   (push iv out))))
           (nreverse out)))))

(defun ivs-cap (ivs)
  (cond ((<= (length ivs) *max-set-size*) ivs)
        (t (let ((merged (ivs-merge-adjacent ivs)))
             (cond ((<= (length merged) *max-set-size*) merged)
                   (t (incf *interval-set-overflows*)
                      (ivs-collapse merged)))))))

(defun ivs-apply (op &rest sets)
  "Cross-product application: call OP once per (a, b, c, ...) tuple drawn
   from SETS in lexicographic order, concatenating the result lists.  All
   ivs-apply-{unary,binary,ternary} are special cases.  Tuples with
   incompatible branch tags (Algorithm 3.2) are dropped before evaluation."
  (ivs-cap
   ;; ACC is built front-to-back via nconc-of-fresh-conses; the inner
   ;; (cons x '()) cell is shared across siblings so nreverse would corrupt
   ;; it.  reverse is non-destructive.
   (labels ((rec (sets acc)
              (if (null sets)
                  (let ((operands (reverse acc)))
                    (if (branches-all-compatible-p operands)
                        (apply op operands)
                        '()))
                  (mapcan (lambda (x) (rec (cdr sets) (cons x acc)))
                          (car sets)))))
     (rec sets '()))))

(declaim (inline ivs-apply-unary ivs-apply-binary ivs-apply-ternary))
(defun ivs-apply-unary   (op as)       (ivs-apply op as))
(defun ivs-apply-binary  (op as bs)    (ivs-apply op as bs))
(defun ivs-apply-ternary (op as bs cs) (ivs-apply op as bs cs))

;;; Reduce an interval-set to one bool-iv.
(defun ivs-bool-or  (vs) (bool-or* vs))
(defun ivs-bool-and (vs) (bool-and* vs))
