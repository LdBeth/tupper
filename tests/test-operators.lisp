;;;; tests/test-operators.lisp --- unit checks for the Day-1 operator extensions.
;;;; Load with (load "tests/test-operators.lisp") after (ql:quickload :tupper).
(in-package :tupper)

(unless (boundp '*fail-count*)
  (defparameter *fail-count* 0))

(unless (macro-function 'check)
  (defmacro check (label expr)
    `(let ((result ,expr))
       (cond (result (format t "  ok  ~a~%" ,label))
             (t (incf *fail-count*)
                (format t "  FAIL ~a~%" ,label))))))

(defun run-operator-tests ()
  (setf *fail-count* 0)

  (format t "~&== min / max ==~%")
  (check "min([1,3], [2,5]) = [1,3]"
         (let ((r (car (iv-min (make-defined-cont 1d0 3d0)
                               (make-defined-cont 2d0 5d0)))))
           (and (= (ival-lo r) 1d0) (= (ival-hi r) 3d0))))
  (check "max([1,3], [2,5]) = [2,5]"
         (let ((r (car (iv-max (make-defined-cont 1d0 3d0)
                               (make-defined-cont 2d0 5d0)))))
           (and (= (ival-lo r) 2d0) (= (ival-hi r) 5d0))))
  (check "min preserves cont"
         (let ((r (car (iv-min (make-defined-cont 0d0 1d0)
                               (make-defined-cont 0d0 1d0)))))
           (ival-cont-lo r)))

  (format t "~&== median ==~%")
  (check "median(1, 2, 3) = 2"
         (let ((r (car (iv-median (make-defined-cont 1d0 1d0)
                                  (make-defined-cont 2d0 2d0)
                                  (make-defined-cont 3d0 3d0)))))
           (and (= (ival-lo r) 2d0) (= (ival-hi r) 2d0))))
  (check "median([0,2], [1,1], [3,3]) = [1, 2]"
         ;; corner medians: med(0,1,3)=1, med(2,1,3)=2 -> hull [1,2]
         (let ((r (car (iv-median (make-defined-cont 0d0 2d0)
                                  (make-defined-cont 1d0 1d0)
                                  (make-defined-cont 3d0 3d0)))))
           (and (= (ival-lo r) 1d0) (= (ival-hi r) 2d0))))

  (format t "~&== floor ==~%")
  (check "floor([1.5, 2.7]) splits into {1, 2}"
         (let ((r (iv-floor (make-defined-cont 1.5d0 2.7d0))))
           (and (= (length r) 2)
                (= (ival-lo (first r)) 1d0)
                (= (ival-lo (second r)) 2d0))))
  (check "floor([1.2, 1.8]) = singleton {1}"
         (let ((r (iv-floor (make-defined-cont 1.2d0 1.8d0))))
           (and (= (length r) 1)
                (= (ival-lo (first r)) 1d0)
                (= (ival-hi (first r)) 1d0)
                (ival-cont-lo (first r)))))
  (check "floor([1.2, 4.7]) = hull [1, 4] with cont-lo=nil"
         (let ((r (iv-floor (make-defined-cont 1.2d0 4.7d0))))
           (and (= (length r) 1)
                (= (ival-lo (first r)) 1d0)
                (= (ival-hi (first r)) 4d0)
                (not (ival-cont-lo (first r))))))
  (check "floor of singleton integer is singleton, cont preserved"
         (let ((r (iv-floor (make-defined-cont 3d0 3d0))))
           (and (= (length r) 1)
                (= (ival-lo (first r)) 3d0)
                (ival-cont-lo (first r)))))
  (check "floor straddling step has cont-lo nil on each piece"
         (let ((r (iv-floor (make-defined-cont 0.5d0 1.5d0))))
           (and (= (length r) 2)
                (notany #'ival-cont-lo r))))

  (format t "~&== ceil / round / trunc ==~%")
  (check "ceil([1.2, 1.8]) = singleton {2}"
         (let ((r (iv-ceil (make-defined-cont 1.2d0 1.8d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) 2d0))))
  (check "round([1.2, 1.8]) splits {1, 2}"
         (let ((r (iv-round (make-defined-cont 1.2d0 1.8d0))))
           (= (length r) 2)))
  (check "trunc([-0.5, 0.5]) = singleton {0} (no step at 0)"
         (let ((r (iv-trunc (make-defined-cont -0.5d0 0.5d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) 0d0))))
  (check "trunc([-1.5, 1.5]) covers {-1, 0, 1} via hull [-1, 1]"
         (let ((r (iv-trunc (make-defined-cont -1.5d0 1.5d0))))
           (and (<= (ival-lo (first r)) -1d0)
                (>= (ival-hi (car (last r))) 1d0))))

  (format t "~&== sgn ==~%")
  (check "sgn([1, 2]) = {1}"
         (let ((r (iv-sgn (make-defined-cont 1d0 2d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) 1d0))))
  (check "sgn([-2, -1]) = {-1}"
         (let ((r (iv-sgn (make-defined-cont -2d0 -1d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) -1d0))))
  (check "sgn([0, 0]) = {0}, cont preserved"
         (let ((r (iv-sgn (make-defined-cont 0d0 0d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) 0d0)
                (ival-cont-lo (first r)))))
  (check "sgn([-1, 1]) splits into {-1, 0, 1}, all cont-lo nil"
         (let ((r (iv-sgn (make-defined-cont -1d0 1d0))))
           (and (= (length r) 3)
                (notany #'ival-cont-lo r))))

  (format t "~&== mod ==~%")
  (check "mod(3.5, 2) = 1.5"
         (let* ((r (iv-mod (make-defined-cont 3.5d0 3.5d0)
                           (make-defined-cont 2d0 2d0)))
                (iv (first r)))
           (and (= (length r) 1)
                (< (abs (- (ival-lo iv) 1.5d0)) 1d-9)
                (< (abs (- (ival-hi iv) 1.5d0)) 1d-9))))
  (check "mod(x, 1) over [0.2, 0.8] is [0.2, 0.8] (no wrap)"
         (let* ((r (iv-mod (make-defined-cont 0.2d0 0.8d0)
                           (make-defined-cont 1d0 1d0)))
                (iv (first r)))
           (and (= (length r) 1)
                (<= (ival-lo iv) 0.2d0)
                (>= (ival-hi iv) 0.8d0))))
  (check "mod(x, 1) over [0.5, 1.5] returns >=2 ivals (wrap at 1)"
         (>= (length (iv-mod (make-defined-cont 0.5d0 1.5d0)
                             (make-defined-cont 1d0 1d0)))
             2))

  (format t "~&== formula dispatch ==~%")
  (check "(floor x) at x=1.7 -> {1}"
         (let ((r (eval-expr '(floor x)
                             (make-defined-cont 1.7d0 1.7d0)
                             (make-defined-cont 0d0 0d0))))
           (and (= (length r) 1) (= (ival-lo (first r)) 1d0))))
  (check "(min x y) over x=[0,2], y=[1,1] -> [0,1]"
         (let ((r (eval-expr '(min x y)
                             (make-defined-cont 0d0 2d0)
                             (make-defined-cont 1d0 1d0))))
           (and (= (ival-lo (first r)) 0d0) (= (ival-hi (first r)) 1d0))))
  (check "(median x y 1) at x=0.5, y=0.5 -> 0.5"
         (let ((r (eval-expr '(median x y 1)
                             (make-defined-cont 0.5d0 0.5d0)
                             (make-defined-cont 0.5d0 0.5d0))))
           (= (ival-lo (first r)) 0.5d0)))

  (format t "~&Failures: ~a~%" *fail-count*)
  (zerop *fail-count*))
