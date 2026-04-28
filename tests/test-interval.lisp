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
  (check "iv-neg of [1,3] = [-3,-1]"
         (let ((r (car (iv-neg (make-defined-cont 1d0 3d0)))))
           (and (= (ival-lo r) -3d0) (= (ival-hi r) -1d0))))
  (check "iv-sub of [3,4]-[1,2] covers [1,3]"
         (let ((r (car (iv-sub (make-defined-cont 3d0 4d0)
                               (make-defined-cont 1d0 2d0)))))
           (and (<= (ival-lo r) 1d0) (>= (ival-hi r) 3d0))))
  (check "iv-abs of [-2,3] = [0,3]"
         (let ((r (car (iv-abs (make-defined-cont -2d0 3d0)))))
           (and (= (ival-lo r) 0d0) (= (ival-hi r) 3d0))))
  (check "iv-abs of [-3,-1] = [1,3]"
         (let ((r (car (iv-abs (make-defined-cont -3d0 -1d0)))))
           (and (= (ival-lo r) 1d0) (= (ival-hi r) 3d0))))
  (format t "~&== exp / log ==~%")
  (check "iv-exp of [0,1] covers [1, e]"
         (let ((r (car (iv-exp (make-defined-cont 0d0 1d0)))))
           (and (<= (ival-lo r) 1d0) (>= (ival-hi r) (exp 1d0)))))
  (check "iv-log of [1,e] covers [0,1]"
         (let* ((e-d (exp 1d0))
                (r (car (iv-log (make-defined-cont 1d0 e-d)))))
           (and (<= (ival-lo r) 0d0) (>= (ival-hi r) 1d0))))
  (check "iv-log of [-1,4] has def-lo nil (straddles 0)"
         (let ((r (car (iv-log (make-defined-cont -1d0 4d0)))))
           (not (ival-def-lo r))))
  (check "iv-log of [-2,-1] is undefined"
         (let ((r (car (iv-log (make-defined-cont -2d0 -1d0)))))
           (ival-totally-undefined-p r)))
  (check "iv-sin of [0, pi] hits 1"
         (let* ((pi-d (coerce pi 'double-float))
                (r (car (iv-sin (make-defined-cont 0d0 pi-d)))))
           (>= (ival-hi r) 1d0)))
  ;; Regression: wide-interval sin/cos/tan must short-circuit to the full
  ;; range without iterating over every k*pi/2 in [lo, hi].  Before the fix,
  ;; iv-sin on [0, 1e8] looped ~6e7 times per call and made the
  ;; exp-sin-over-[-10,10] graph take ~25 minutes.
  (format t "~&== sin/cos/tan wide-interval regression ==~%")
  (check "iv-sin on [0, 1d8] is [-1,1] (and fast)"
         (let* ((t0 (get-internal-real-time))
                (r (car (iv-sin (make-defined-cont 0d0 1d8))))
                (dt (/ (- (get-internal-real-time) t0)
                       (float internal-time-units-per-second))))
           (and (= (ival-lo r) -1d0) (= (ival-hi r) 1d0)
                (< dt 0.05d0))))
  (check "iv-cos on [-1d10, 1d10] is [-1,1] (and fast)"
         (let* ((t0 (get-internal-real-time))
                (r (car (iv-cos (make-defined-cont -1d10 1d10))))
                (dt (/ (- (get-internal-real-time) t0)
                       (float internal-time-units-per-second))))
           (and (= (ival-lo r) -1d0) (= (ival-hi r) 1d0)
                (< dt 0.05d0))))
  (check "iv-tan on [0, 1d6] is unbounded (and fast)"
         (let* ((t0 (get-internal-real-time))
                (r (car (iv-tan (make-defined-cont 0d0 1d6))))
                (dt (/ (- (get-internal-real-time) t0)
                       (float internal-time-units-per-second))))
           (and (= (ival-lo r) +neg-inf+) (= (ival-hi r) +pos-inf+)
                (< dt 0.05d0))))
  (check "narrow-interval sin still uses critical-point detection"
         ;; Sanity: the early-out must NOT fire for narrow intervals.
         ;; sin over [0, pi] (~3.14, less than 2pi) must still return [0, 1].
         (let* ((pi-d (coerce pi 'double-float))
                (r (car (iv-sin (make-defined-cont 0d0 pi-d)))))
           (and (<= (ival-lo r) 0d0) (>= (ival-hi r) 1d0)
                ;; NOT widened all the way to -1 (that would mean the
                ;; period guard fired wrongly).
                (> (ival-lo r) -0.5d0))))
  (format t "~&== formula eval ==~%")
  (check "1 < 2 -> :tt"
         (eq :tt (eval-formula '(< 1 2)
                               (make-constant-iv 0d0)
                               (make-constant-iv 0d0))))
  (check "y < sqrt(x) at x=-1 -> :ff (undefined rule)"
         (eq :ff (eval-formula '(< y (sqrt x))
                               (make-defined-cont -1d0 -0.5d0)
                               (make-defined-cont 0d0 1d0))))
  ;; End-to-end reliability invariants on rendered pixmaps.
  (format t "~&== end-to-end reliability ==~%")
  (check "y < sqrt(x): zero black pixels in x<0"
         ;; The whole point of the def<F,*> propagation: undefined operands
         ;; must make comparisons FF, so the left half must be all white/red.
         (let ((pm (graph-formula '(< y (sqrt x)) -1d0 1d0 -1d0 1d0 64 64
                                  :max-subpixel-depth 3))
               (nb 0))
           (dotimes (y 64) (dotimes (x 32)
             (when (eq (aref pm y x) :black) (incf nb))))
           (zerop nb)))
  (check "y = 1/x: zero black pixels on the column straddling x=0"
         ;; The interval-set return from iv-div for zero-straddling
         ;; denominators must keep the discontinuity column clean.
         (let* ((pm (graph-formula '(= y (/ 1 x)) -4d0 7d0 -4d0 7d0 128 128
                                   :max-subpixel-depth 3))
                (col (floor (* 128 (/ 4d0 11d0))))   ; column where x=0
                (nb 0))
           (dotimes (y 128)
             (when (eq (aref pm y col) :black) (incf nb)))
           (zerop nb)))
  (check "exp(sin x + cos y) = sin(exp(x+y)) on [-10,10] finishes fast"
         ;; Regression for the wide-sin/cos pathology: this graph at 64x64
         ;; ran ~3 minutes before the fix; should now be sub-second.
         (let* ((t0 (get-internal-real-time))
                (pm (graph-formula
                     '(= (exp (+ (sin x) (cos y))) (sin (exp (+ x y))))
                     -10d0 10d0 -10d0 10d0 64 64
                     :max-subpixel-depth 3))
                (dt (/ (- (get-internal-real-time) t0)
                       (float internal-time-units-per-second))))
           (declare (ignore pm))
           (< dt 5d0)))
  (format t "~&Failures: ~a~%" *fail-count*)
  (zerop *fail-count*))
