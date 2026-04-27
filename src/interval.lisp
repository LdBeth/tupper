;;;; interval.lisp --- the core interval type with definedness + continuity.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defstruct ival
  (lo 0d0 :type double-float)
  (hi 0d0 :type double-float)
  ;; def in [def-lo, def-hi]; cont in [cont-lo, cont-hi]
  (def-lo nil :type boolean)
  (def-hi t   :type boolean)
  (cont-lo nil :type boolean)
  (cont-hi t   :type boolean)
  ;; branch slot reserved for Algorithm 3.2 (NIL = all branches).
  (branch nil))

(defun make-defined-cont (lo hi)
  (declare (type double-float lo hi))
  (make-ival :lo lo :hi hi
             :def-lo t :def-hi t
             :cont-lo t :cont-hi t))

(defun make-undefined ()
  (make-ival :lo +pos-inf+ :hi +neg-inf+
             :def-lo nil :def-hi nil
             :cont-lo nil :cont-hi nil))

(defun make-constant-iv (x)
  (declare (type double-float x))
  (make-defined-cont x x))

(defun ival-empty-p (iv)
  (or (not (ival-def-hi iv))
      (> (ival-lo iv) (ival-hi iv))))

(defun ival-contains-zero-p (iv)
  (and (<= (ival-lo iv) 0d0) (>= (ival-hi iv) 0d0)))

(defun ival-print (iv &optional (stream t))
  (format stream "[~,6f, ~,6f] def<~a,~a> cont<~a,~a>"
          (ival-lo iv) (ival-hi iv)
          (if (ival-def-lo iv) "T" "F") (if (ival-def-hi iv) "T" "F")
          (if (ival-cont-lo iv) "T" "F") (if (ival-cont-hi iv) "T" "F")))

(defun ival-totally-defined-p (iv)
  (and (ival-def-lo iv) (ival-def-hi iv)))
(defun ival-totally-undefined-p (iv)
  (and (not (ival-def-lo iv)) (not (ival-def-hi iv))))
(defun ival-continuous-p (iv)
  (and (ival-cont-lo iv) (ival-cont-hi iv)))
