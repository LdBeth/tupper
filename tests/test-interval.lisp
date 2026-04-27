;;;; tests/test-interval.lisp --- minimal unit checks for interval arithmetic.
;;;; Load with (load "tests/test-interval.lisp") after (ql:quickload :tupper).
(in-package :tupper)

(defparameter *fail-count* 0)

(defmacro check (label expr)
  `(let ((result ,expr))
     (cond (result (format t "  ok  ~a~%" ,label))
           (t (incf *fail-count*)
              (format t "  FAIL ~a~%" ,label)))))

(defun run-tests ()
  (setf *fail-count* 0)
  (format t "~&== rounding ==~%")
  (check "add-up > add-down for 0.1+0.2"
         (> (add-up 0.1d0 0.2d0) (add-down 0.1d0 0.2d0)))
  (check "inc-ulp positive"
         (> (inc-ulp 1d0) 1d0))
  (check "dec-ulp positive"
         (< (dec-ulp 1d0) 1d0))
  (check "inc-ulp negative"
         (> (inc-ulp -1d0) -1d0))
  (format t "~&== ival ==~%")
  (check "constant ival is defined"
         (ival-totally-defined-p (make-constant-iv 1d0)))
  (check "iv-add of [1,2]+[3,4] = [4,6]"
         (let ((r (car (iv-add (make-defined-cont 1d0 2d0)
                               (make-defined-cont 3d0 4d0)))))
           (and (<= (ival-lo r) 4d0) (>= (ival-hi r) 6d0))))
  (check "iv-mul of [-1,2]*[-3,4] covers 8"
         (let ((r (car (iv-mul (make-defined-cont -1d0 2d0)
                               (make-defined-cont -3d0 4d0)))))
           (and (<= (ival-lo r) -6d0) (>= (ival-hi r) 8d0))))
  (check "iv-div by zero-straddler returns >=2 intervals"
         (let ((r (iv-div (make-defined-cont 1d0 1d0)
                          (make-defined-cont -1d0 1d0))))
           (>= (length r) 2)))
  (check "iv-sqrt of [-1,4] is [0, ~2]"
         (let ((r (car (iv-sqrt (make-defined-cont -1d0 4d0)))))
           (and (= (ival-lo r) 0d0) (>= (ival-hi r) 2d0)
                (not (ival-def-lo r)))))
  (check "iv-sin of [0, pi] hits 1"
         (let* ((pi-d (coerce pi 'double-float))
                (r (car (iv-sin (make-defined-cont 0d0 pi-d)))))
           (>= (ival-hi r) 1d0)))
  (format t "~&== formula eval ==~%")
  (check "1 < 2 -> :tt"
         (eq :tt (eval-formula '(< 1 2)
                               (make-constant-iv 0d0)
                               (make-constant-iv 0d0))))
  (check "y < sqrt(x) at x=-1 -> :ff (undefined rule)"
         (eq :ff (eval-formula '(< y (sqrt x))
                               (make-defined-cont -1d0 -0.5d0)
                               (make-defined-cont 0d0 1d0))))
  (format t "~&Failures: ~a~%" *fail-count*)
  (zerop *fail-count*))
