;;;; boolean-interval.lisp
(in-package #:tupper)

;;; Boolean intervals. Three values:  :ff, :ft, :tt.
;;; (:ff means definitely false; :tt definitely true; :ft undecided.)

(deftype bool-iv () '(member :ff :ft :tt))

(declaim (inline bool-from-bounds))
(defun bool-from-bounds (lo hi)
  (cond ((and (not lo) (not hi)) :ff)
        ((and lo hi)             :tt)
        (t                        :ft)))

(defun bool-lo (b) (eq b :tt))
(defun bool-hi (b) (not (eq b :ff)))

(defun bool-and (a b)
  (bool-from-bounds (and (bool-lo a) (bool-lo b))
                    (and (bool-hi a) (bool-hi b))))

(defun bool-or (a b)
  (bool-from-bounds (or (bool-lo a) (bool-lo b))
                    (or (bool-hi a) (bool-hi b))))

(defun bool-not (a)
  (case a (:ff :tt) (:tt :ff) (:ft :ft)))

(defun bool-or* (lst)
  (reduce #'bool-or lst :initial-value :ff))
(defun bool-and* (lst)
  (reduce #'bool-and lst :initial-value :tt))
