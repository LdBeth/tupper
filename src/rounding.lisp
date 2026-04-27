;;;; rounding.lisp --- IEEE 754 directed rounding helpers.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defconstant +pos-inf+ sb-ext:double-float-positive-infinity)
(defconstant +neg-inf+ sb-ext:double-float-negative-infinity)

(defmacro with-rounding (mode &body body)
  "MODE is one of :nearest :positive-infinity :negative-infinity :zero."
  (let ((saved (gensym "ROUND-")))
    `(let ((,saved (getf (sb-int:get-floating-point-modes) :rounding-mode)))
       (unwind-protect
            (progn (sb-int:set-floating-point-modes :rounding-mode ,mode)
                   ,@body)
         (sb-int:set-floating-point-modes :rounding-mode ,saved)))))

(declaim (inline round-down round-up))
(defun round-down (thunk) (with-rounding :negative-infinity (funcall thunk)))
(defun round-up   (thunk) (with-rounding :positive-infinity (funcall thunk)))

;;; ULP helpers --- bitwise nudge by one floating-point step using the
;;; 64-bit IEEE-754 encoding.  For positive x, incrementing the bit pattern
;;; gives the next representable double; for negative x, decrementing it does.
(declaim (inline %bits->double %double->bits))
(defun %double->bits (x)
  (declare (type double-float x))
  (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32)
          (ldb (byte 32 0) (sb-kernel:double-float-low-bits x))))
(defun %bits->double (bits)
  (declare (type (unsigned-byte 64) bits))
  (let* ((h (ldb (byte 32 32) bits))
         (l (ldb (byte 32 0)  bits))
         (sh (if (>= h #x80000000) (- h #x100000000) h)))
    (sb-kernel:make-double-float sh l)))

(defun inc-ulp (x)
  (declare (type double-float x))
  (cond ((sb-ext:float-nan-p x) x)
        ((= x +pos-inf+) x)
        ((zerop x) least-positive-double-float)
        (t (let ((bits (%double->bits x)))
             (%bits->double (if (plusp x) (1+ bits) (1- bits)))))))

(defun dec-ulp (x)
  (declare (type double-float x))
  (cond ((sb-ext:float-nan-p x) x)
        ((= x +neg-inf+) x)
        ((zerop x) least-negative-double-float)
        (t (let ((bits (%double->bits x)))
             (%bits->double (if (plusp x) (1- bits) (1+ bits)))))))

;;; Basic directed-rounded primitives.  Arithmetic ops on constant arguments
;;; would otherwise be folded by the compiler at the *default* rounding mode;
;;; declare NOTINLINE on +/-/etc. to force runtime evaluation.
(defun add-down (a b)
  (declare (type double-float a b) (notinline +))
  (with-rounding :negative-infinity (+ a b)))
(defun add-up (a b)
  (declare (type double-float a b) (notinline +))
  (with-rounding :positive-infinity (+ a b)))
(defun sub-down (a b)
  (declare (type double-float a b) (notinline -))
  (with-rounding :negative-infinity (- a b)))
(defun sub-up (a b)
  (declare (type double-float a b) (notinline -))
  (with-rounding :positive-infinity (- a b)))
(defun mul-down (a b)
  (declare (type double-float a b) (notinline *))
  (with-rounding :negative-infinity (* a b)))
(defun mul-up (a b)
  (declare (type double-float a b) (notinline *))
  (with-rounding :positive-infinity (* a b)))
(defun div-down (a b)
  (declare (type double-float a b) (notinline /))
  (with-rounding :negative-infinity (/ a b)))
(defun div-up (a b)
  (declare (type double-float a b) (notinline /))
  (with-rounding :positive-infinity (/ a b)))
(defun sqrt-down (a)
  (declare (type double-float a))
  (dec-ulp (sqrt a)))     ; libm sqrt may not honor rounding; widen by 1 ULP.
(defun sqrt-up (a)
  (declare (type double-float a))
  (inc-ulp (sqrt a)))

;;; Widened transcendentals: compute, then nudge ±1 ULP for safety.
(declaim (inline trans-down trans-up))
(defun trans-down (val)
  (declare (type double-float val))
  (dec-ulp val))
(defun trans-up (val)
  (declare (type double-float val))
  (inc-ulp val))
