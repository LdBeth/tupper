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

  (format t "~&== mod (desugared via assign-sites) ==~%")
  (check "mod(3.5, 2) = 1.5 via eval-expr on desugared form"
         (let* ((ann (assign-sites '(mod 3.5 2)))
                (r (eval-expr ann
                              (make-defined-cont 0d0 0d0)
                              (make-defined-cont 0d0 0d0)))
                (iv (first r)))
           (and (= (length r) 1)
                (< (abs (- (ival-lo iv) 1.5d0)) 1d-9)
                (< (abs (- (ival-hi iv) 1.5d0)) 1d-9))))
  (check "mod(x, 1) over [0.2, 0.8] is [0.2, 0.8] (no wrap)"
         (let* ((ann (assign-sites '(mod x 1)))
                (r (eval-expr ann
                              (make-defined-cont 0.2d0 0.8d0)
                              (make-defined-cont 0d0 0d0)))
                (iv (first r)))
           (and (= (length r) 1)
                (<= (ival-lo iv) 0.2d0)
                (>= (ival-hi iv) 0.8d0))))
  (check "mod(x, 1) over [0.5, 1.5] returns >=2 ivals (wrap at 1)"
         (let* ((ann (assign-sites '(mod x 1)))
                (r (eval-expr ann
                              (make-defined-cont 0.5d0 1.5d0)
                              (make-defined-cont 0d0 0d0))))
           (>= (length r) 2)))

  (format t "~&== pow ==~%")
  (check "[2,3]^2 covers [4,9] (positive base, even)"
         (let ((r (car (iv-pow (make-defined-cont 2d0 3d0)
                               (make-defined-cont 2d0 2d0)))))
           (and (<= (ival-lo r) 4d0) (>= (ival-hi r) 9d0)
                (>= (ival-lo r) 0d0))))
  (check "[-3,-2]^2 covers [4,9] (negative base, even)"
         (let ((r (car (iv-pow (make-defined-cont -3d0 -2d0)
                               (make-defined-cont 2d0 2d0)))))
           (and (<= (ival-lo r) 4d0) (>= (ival-hi r) 9d0)
                (>= (ival-lo r) 0d0))))
  (check "[-2,3]^2: lo<=0 and hi>=9 (zero-crossing + even)"
         (let ((r (car (iv-pow (make-defined-cont -2d0 3d0)
                               (make-defined-cont 2d0 2d0)))))
           (and (<= (ival-lo r) 0d0) (>= (ival-hi r) 9d0))))
  (check "[-3,-2]^3 covers [-27,-8] (odd on negative base)"
         (let ((r (car (iv-pow (make-defined-cont -3d0 -2d0)
                               (make-defined-cont 3d0 3d0)))))
           (and (<= (ival-lo r) -27d0) (>= (ival-hi r) -8d0))))
  (check "x^0 = [1,1]"
         (let ((r (car (iv-pow (make-defined-cont -2d0 3d0)
                               (make-defined-cont 0d0 0d0)))))
           (and (= (ival-lo r) 1d0) (= (ival-hi r) 1d0))))
  (check "[2,4]^(-1) covers [0.25,0.5]"
         (let ((r (car (iv-pow (make-defined-cont 2d0 4d0)
                               (make-defined-cont -1d0 -1d0)))))
           (and (<= (ival-lo r) 0.25d0) (>= (ival-hi r) 0.5d0))))
  (check "positive base real exponent [4,9]^0.5 covers [2,3]"
         (let ((r (car (iv-pow (make-defined-cont 4d0 9d0)
                               (make-defined-cont 0.5d0 0.5d0)))))
           (and (<= (ival-lo r) 2d0) (>= (ival-hi r) 3d0))))
  (check "negative base + non-integer exp widens to [-inf,+inf]"
         (let ((r (car (iv-pow (make-defined-cont -2d0 3d0)
                               (make-defined-cont 0.5d0 0.5d0)))))
           (and (= (ival-lo r) +neg-inf+) (= (ival-hi r) +pos-inf+))))

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
  (check "(min x y 0) over x=[1,2], y=[3,4] -> lo=0 (n-ary)"
         (let ((r (eval-expr '(min x y 0)
                             (make-defined-cont 1d0 2d0)
                             (make-defined-cont 3d0 4d0))))
           (= (ival-lo (first r)) 0d0)))
  (check "(max x y 10) over x=[1,2], y=[3,4] -> hi=10 (n-ary)"
         (let ((r (eval-expr '(max x y 10)
                             (make-defined-cont 1d0 2d0)
                             (make-defined-cont 3d0 4d0))))
           (= (ival-hi (first r)) 10d0)))
  (check "(^ x 2) at x=[-2,3] -> lo<=0, hi>=9"
         (let ((r (eval-expr '(^ x 2)
                             (make-defined-cont -2d0 3d0)
                             (make-defined-cont 0d0 0d0))))
           (and (<= (ival-lo (first r)) 0d0) (>= (ival-hi (first r)) 9d0))))
  (check "(median x y 1) at x=0.5, y=0.5 -> 0.5"
         (let ((r (eval-expr '(median x y 1)
                             (make-defined-cont 0.5d0 0.5d0)
                             (make-defined-cont 0.5d0 0.5d0))))
           (= (ival-lo (first r)) 0.5d0)))

  ;;; --- Algorithm 3.2: paper-faithful branch rep ----------------------

  (format t "~&== combine-branches ==~%")
  (check "combine-branches NIL+NIL = NIL"
         (null (combine-branches nil nil)))
  (check "combine-branches NIL is identity"
         (and (equal '(5 . 0) (combine-branches nil '(5 . 0)))
              (equal '(5 . 0) (combine-branches '(5 . 0) nil))))
  (check "combine-branches agrees on shared site (both choose 0)"
         ;; cut={site 0}, chosen=0 ; cut={site 0}, chosen=0 -> agree
         (equal '(1 . 0) (combine-branches '(1 . 0) '(1 . 0))))
  (check "combine-branches agrees on shared site (both choose 1)"
         (equal '(1 . 1) (combine-branches '(1 . 1) '(1 . 1))))
  (check "combine-branches conflicts when shared site disagrees"
         (eq :conflict (combine-branches '(1 . 0) '(1 . 1))))
  (check "combine-branches unions disjoint sites"
         ;; cut={0}, chosen={0->1}  ;  cut={1}, chosen={1->1}
         (equal '(3 . 3) (combine-branches '(1 . 1) '(2 . 2))))
  (check "combine-branches with overlap + new site (agreement)"
         ;; A: cut={0,1}, chosen={0->1, 1->0}  =>  cut=3, chosen=1
         ;; B: cut={1,2}, chosen={1->0, 2->1}  =>  cut=6, chosen=4
         ;; overlap at site 1; both chose 0 there -> ok
         ;; combined: cut=7, chosen=5
         (equal '(7 . 5) (combine-branches '(3 . 1) '(6 . 4))))

  (format t "~&== branches-all-compatible-p ==~%")
  (check "all-NIL trio is compatible"
         (branches-all-compatible-p
          (list (make-defined-cont 0d0 1d0)
                (make-defined-cont 0d0 1d0)
                (make-defined-cont 0d0 1d0))))
  (check "shared-site agreement passes"
         (let ((a (make-defined-cont 0d0 1d0))
               (b (make-defined-cont 0d0 1d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(1 . 0))
           (branches-all-compatible-p (list a b))))
  (check "shared-site disagreement fails"
         (let ((a (make-defined-cont 0d0 1d0))
               (b (make-defined-cont 0d0 1d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(1 . 1))
           (not (branches-all-compatible-p (list a b)))))
  (check "fold catches three-way conflict that pairwise misses"
         ;; A: site 0 -> 0     (cut=1, chosen=0)
         ;; B: site 1 -> 0     (cut=2, chosen=0)  -- compatible with A
         ;; C: site 0 -> 1     (cut=1, chosen=1)  -- compatible with B alone, conflicts with A
         (let ((a (make-defined-cont 0d0 1d0))
               (b (make-defined-cont 0d0 1d0))
               (c (make-defined-cont 0d0 1d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(2 . 0)
                 (ival-branch c) '(1 . 1))
           (not (branches-all-compatible-p (list a b c)))))

  (format t "~&== branch threading via operators ==~%")
  (check "iv-add propagates union when operands agree"
         (let ((a (make-defined-cont 1d0 2d0))
               (b (make-defined-cont 3d0 4d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(2 . 0))
           (equal '(3 . 0) (ival-branch (car (iv-add a b))))))
  (check "iv-neg preserves branch (unary inherit)"
         (let ((a (make-defined-cont 1d0 2d0)))
           (setf (ival-branch a) '(1 . 1))
           (equal '(1 . 1) (ival-branch (car (iv-neg a))))))
  (check "ivs-apply-binary drops conflicting tuple"
         (let ((a (make-defined-cont 1d0 1d0))
               (b (make-defined-cont 2d0 2d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(1 . 1))
           (null (ivs-apply-binary #'iv-add (list a) (list b)))))
  (check "ivs-apply-binary keeps agreeing tuple"
         (let ((a (make-defined-cont 1d0 1d0))
               (b (make-defined-cont 2d0 2d0)))
           (setf (ival-branch a) '(1 . 0)
                 (ival-branch b) '(1 . 0))
           (= 1 (length (ivs-apply-binary #'iv-add (list a) (list b))))))
  (check "ivs-apply with all-NIL branches matches pre-hook behavior"
         (= 4 (length (ivs-apply-binary
                       #'iv-add
                       (list (make-defined-cont 1d0 1d0)
                             (make-defined-cont 2d0 2d0))
                       (list (make-defined-cont 10d0 10d0)
                             (make-defined-cont 20d0 20d0))))))

  ;;; ---------------------------------------------------------------------
  ;;; Day-2 extension tests (Groups C + D + F)
  ;;; ---------------------------------------------------------------------

  (format t "~&== nth-root ==~%")
  (check "iv-nth-root 3 [8, 27] covers [2, 3]"
         (let ((r (iv-nth-root 3 (make-defined-cont 8d0 27d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) 2d0)
                (>= (ival-hi (first r)) 3d0))))
  (check "iv-nth-root 3 [-27, -8] covers [-3, -2] (odd, sign-preserving)"
         (let ((r (iv-nth-root 3 (make-defined-cont -27d0 -8d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) -3d0)
                (>= (ival-hi (first r)) -2d0))))
  (check "iv-nth-root 3 [-8, 8] spans [-2, 2] (zero-crossing odd root)"
         (let ((r (iv-nth-root 3 (make-defined-cont -8d0 8d0))))
           (and (<= (ival-lo (first r)) -2d0)
                (>= (ival-hi (first r))  2d0))))
  (check "iv-nth-root 2 [4, 9] covers [2, 3]"
         (let ((r (iv-nth-root 2 (make-defined-cont 4d0 9d0))))
           (and (<= (ival-lo (first r)) 2d0)
                (>= (ival-hi (first r)) 3d0))))
  (check "iv-nth-root 2 [-4, -1] is undefined"
         (let ((r (iv-nth-root 2 (make-defined-cont -4d0 -1d0))))
           (and (= 1 (length r))
                (ival-totally-undefined-p (first r)))))
  (check "iv-nth-root 2 [-1, 4] partial: def<F,T>, lo>=0"
         (let ((r (iv-nth-root 2 (make-defined-cont -1d0 4d0))))
           (and (= 1 (length r))
                (>= (ival-lo (first r)) 0d0)
                (not (ival-def-lo (first r)))
                (ival-def-hi (first r)))))

  (format t "~&== rational pow (parity) ==~%")
  (check "(^ -8 1/3) = -2 (negative base, odd den, odd num)"
         (let ((r (eval-expr '(^ x 1/3)
                             (make-defined-cont -8d0 -8d0)
                             (make-defined-cont 0d0 0d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) -2d0)
                (>= (ival-hi (first r)) -2d0))))
  (check "(^ -8 2/3) covers 4 (negative base, odd den, even num)"
         (let ((r (eval-expr '(^ x 2/3)
                             (make-defined-cont -8d0 -8d0)
                             (make-defined-cont 0d0 0d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) 4d0)
                (>= (ival-hi (first r)) 4d0))))
  (check "(^ x 1/3) at x=[-8,8] spans [-2, 2]"
         (let* ((rs (eval-expr '(^ x 1/3)
                              (make-defined-cont -8d0 8d0)
                              (make-defined-cont 0d0 0d0)))
                (lo (reduce #'min rs :key #'ival-lo))
                (hi (reduce #'max rs :key #'ival-hi)))
           (and (<= lo -2d0) (>= hi 2d0))))
  (check "(^ x (/ 1 3)) parses parity tag"
         ;; (/ 1 3) is the explicit-division form.
         (let ((r (eval-expr '(^ x (/ 1 3))
                             (make-defined-cont -8d0 -8d0)
                             (make-defined-cont 0d0 0d0))))
           (and (<= (ival-lo (first r)) -2d0)
                (>= (ival-hi (first r)) -2d0))))

  (format t "~&== inverse trig ==~%")
  (check "iv-arcsin([0, 1]) = [0, pi/2]"
         (let ((r (iv-arcsin (make-defined-cont 0d0 1d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) 0d0)
                (>= (ival-hi (first r))
                    (* 0.5d0 (coerce pi 'double-float))))))
  (check "iv-arcsin([-2, 0.5]) clips: def<F,T>, hi >= asin(0.5)"
         (let ((r (iv-arcsin (make-defined-cont -2d0 0.5d0))))
           (and (= 1 (length r))
                (not (ival-def-lo (first r)))
                (>= (ival-hi (first r)) (asin 0.5d0)))))
  (check "iv-arcsin([2, 3]) wholly outside domain -> undefined"
         (let ((r (iv-arcsin (make-defined-cont 2d0 3d0))))
           (and (= 1 (length r))
                (ival-totally-undefined-p (first r)))))
  (check "iv-arccos([0, 1]) = [0, pi/2]"
         (let ((r (iv-arccos (make-defined-cont 0d0 1d0))))
           (and (<= (ival-lo (first r)) 0d0)
                (>= (ival-hi (first r))
                    (* 0.5d0 (coerce pi 'double-float))))))
  (check "iv-arccos([-1, 1]) covers [0, pi]"
         (let ((r (iv-arccos (make-defined-cont -1d0 1d0))))
           (and (<= (ival-lo (first r)) 0d0)
                (>= (ival-hi (first r)) (coerce pi 'double-float)))))
  (check "iv-arctan([0, 1]) covers [0, pi/4]"
         (let ((r (iv-arctan (make-defined-cont 0d0 1d0))))
           (and (<= (ival-lo (first r)) 0d0)
                (>= (ival-hi (first r))
                    (atan 1d0)))))
  (check "iv-arctan(R) bounded by [-pi/2, pi/2]"
         (let ((r (iv-arctan (make-ival :lo +neg-inf+ :hi +pos-inf+
                                        :def-lo t :def-hi t
                                        :cont-lo t :cont-hi t)))
               (half-pi (+ (* 0.5d0 (coerce pi 'double-float)) 1d-9)))
           (and (>= (ival-lo (first r)) (- half-pi))
                (<= (ival-hi (first r)) half-pi)
                (<= (ival-lo (first r)) 0d0)
                (>= (ival-hi (first r)) 0d0))))
  (check "iv-arccot R has lo>=0, hi<=pi"
         (let ((r (iv-arccot (make-defined-cont -100d0 100d0))))
           (and (>= (ival-lo (first r)) 0d0)
                (<= (ival-hi (first r))
                    (+ (coerce pi 'double-float) 1d-9)))))
  (check "iv-arccsc([2, 5]) covers arcsin(1/5)..arcsin(1/2)"
         (let* ((rs (iv-arccsc (make-defined-cont 2d0 5d0)))
                (lo (reduce #'min rs :key #'ival-lo))
                (hi (reduce #'max rs :key #'ival-hi)))
           (and (<= lo (asin (/ 1d0 5d0)))
                (>= hi (asin 0.5d0)))))
  (check "(arctan x) dispatch via formula"
         (let ((r (eval-expr '(arctan x)
                             (make-defined-cont 1d0 1d0)
                             (make-defined-cont 0d0 0d0))))
           (and (<= (ival-lo (first r)) (atan 1d0))
                (>= (ival-hi (first r)) (atan 1d0)))))

  (format t "~&== gamma / factorial ==~%")
  (check "gamma([1, 2]) covers [gamma_min, 1]"
         ;; gamma is in [~0.8856, 1] on [1, 2]; min near x=1.4616.
         (let ((r (iv-gamma (make-defined-cont 1d0 2d0))))
           (and (= 1 (length r))
                (<= (ival-lo (first r)) 0.8857d0)
                (>= (ival-hi (first r)) 1d0))))
  (check "gamma(5) covers 24"
         (let ((r (iv-gamma (make-defined-cont 5d0 5d0))))
           (and (<= (ival-lo (first r)) 24d0)
                (>= (ival-hi (first r)) 24d0))))
  (check "gamma straddling pole at 0 returns widened"
         (let ((r (iv-gamma (make-defined-cont -0.5d0 0.5d0))))
           (and (= (ival-lo (first r)) +neg-inf+)
                (= (ival-hi (first r)) +pos-inf+)
                (not (ival-def-lo (first r)))
                (not (ival-cont-lo (first r))))))
  (check "gamma straddling pole at -1 returns widened"
         (let ((r (iv-gamma (make-defined-cont -1.5d0 -0.5d0))))
           (and (= (ival-lo (first r)) +neg-inf+)
                (= (ival-hi (first r)) +pos-inf+)
                (not (ival-def-lo (first r))))))
  (check "factorial(5) covers 120"
         (let ((r (iv-factorial (make-defined-cont 5d0 5d0))))
           (and (<= (ival-lo (first r)) 120d0)
                (>= (ival-hi (first r)) 120d0))))
  (check "factorial of non-integer-bounds marks def-lo nil"
         (let ((r (iv-factorial (make-defined-cont 1d0 2d0))))
           (notany #'ival-def-lo r)))
  (check "(gamma x) dispatch at x=2 covers 1"
         (let ((r (eval-expr '(gamma x)
                             (make-defined-cont 2d0 2d0)
                             (make-defined-cont 0d0 0d0))))
           (and (<= (ival-lo (first r)) 1d0)
                (>= (ival-hi (first r)) 1d0))))
  (check "(! x) dispatch at x=4 covers 24"
         (let ((r (eval-expr '(! x)
                             (make-defined-cont 4d0 4d0)
                             (make-defined-cont 0d0 0d0))))
           (and (<= (ival-lo (first r)) 24d0)
                (>= (ival-hi (first r)) 24d0))))

  ;;; --- Algorithm 3.2: site-assignment pre-pass ------------------------

  (format t "~&== assign-sites ==~%")
  (check "lone (floor x) gets :site 0"
         (multiple-value-bind (f n) (assign-sites '(= y (floor x)))
           (declare (ignore n))
           (equal f '(= y (floor x :site 0)))))
  (check "site-count for one floor occurrence is 1"
         (multiple-value-bind (f n) (assign-sites '(= y (floor x)))
           (declare (ignore f))
           (= n 1)))
  (check "two equal floor occurrences share a site (CSE)"
         (multiple-value-bind (f n)
             (assign-sites '(= (+ y (floor x)) (+ 1/3 (floor x))))
           (declare (ignore n))
           (equal f '(= (+ y (floor x :site 0))
                        (+ 1/3 (floor x :site 0))))))
  (check "two-floor formula reports site-count 1"
         (multiple-value-bind (f n)
             (assign-sites '(= (+ y (floor x)) (+ 1/3 (floor x))))
           (declare (ignore f))
           (= n 1)))
  (check "different floor args get different sites"
         (multiple-value-bind (f n)
             (assign-sites '(= (floor x) (floor (+ x 1))))
           (declare (ignore n))
           (equal f '(= (floor x :site 0)
                        (floor (+ x 1) :site 1)))))
  (check "(mod x 1) is desugared into (- x (* 1 (floor (/ x 1))))"
         (multiple-value-bind (f n) (assign-sites '(= y (mod x 1)))
           (declare (ignore n))
           (equal f '(= y (- x (* 1 (floor (/ x 1 :site 0)
                                          :site 1)))))))
  (check "sgn allocates 2 sites (next site = 2)"
         (multiple-value-bind (f n)
             (assign-sites '(= y (+ (sgn x) (floor x))))
           (declare (ignore f))
           (= n 3)))
  (check "tan gets a site"
         (multiple-value-bind (f n) (assign-sites '(= y (tan x)))
           (declare (ignore n))
           (equal f '(= y (tan x :site 0)))))
  (check "/ gets a site"
         (multiple-value-bind (f n) (assign-sites '(= y (/ 1 x)))
           (declare (ignore n))
           (equal f '(= y (/ 1 x :site 0)))))
  (check "non-branch-cutting ops are unannotated"
         (multiple-value-bind (f n)
             (assign-sites '(= y (+ (sin x) (* x x))))
           (declare (ignore n))
           (equal f '(= y (+ (sin x) (* x x))))))
  (check "assign-sites is idempotent"
         (let* ((once (assign-sites '(= (+ y (floor x))
                                        (+ 1/3 (floor x)))))
                (twice (assign-sites once)))
           (equal once twice)))

  (format t "~&== %iv-step site tagging ==~%")
  (check "iv-floor [1.5,2.7] :site 0 -> piece-0 chosen=0, piece-1 chosen=1"
         (let ((r (iv-floor (make-defined-cont 1.5d0 2.7d0) 0)))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "iv-floor :site 3 places bits at position 3"
         (let ((r (iv-floor (make-defined-cont 1.5d0 2.7d0) 3)))
           (and (equal '(8 . 0) (ival-branch (first  r)))
                (equal '(8 . 8) (ival-branch (second r))))))
  (check "iv-floor with no site keeps branch NIL (back-compat)"
         (let ((r (iv-floor (make-defined-cont 1.5d0 2.7d0))))
           (and (= 2 (length r))
                (every (lambda (iv) (null (ival-branch iv))) r))))
  (check "iv-floor singleton-step result is unsited even when site is given"
         (let ((r (iv-floor (make-defined-cont 1.2d0 1.8d0) 0)))
           (and (= 1 (length r))
                (null (ival-branch (first r))))))
  (check "iv-floor hull case (>1 step) is unsited even when site is given"
         (let ((r (iv-floor (make-defined-cont 1.2d0 4.7d0) 0)))
           (and (= 1 (length r))
                (null (ival-branch (first r))))))
  (check "iv-ceil :site 0 produces both pieces tagged"
         (let ((r (iv-ceil (make-defined-cont 1.5d0 2.7d0) 0)))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "iv-round :site 0 produces both pieces tagged"
         (let ((r (iv-round (make-defined-cont 1.2d0 1.8d0) 0)))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "iv-trunc :site 0 produces both pieces tagged for [0.5,1.5]"
         (let ((r (iv-trunc (make-defined-cont 0.5d0 1.5d0) 0)))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))

  (format t "~&== iv-div site tagging ==~%")
  (check "iv-div 1/[-1,1] :site 0 tags both pieces"
         (let ((r (iv-div (make-defined-cont 1d0 1d0)
                          (make-defined-cont -1d0 1d0)
                          0)))
           (and (= 2 (length r))
                ;; Pieces are pushed onto a list and then reversed; the
                ;; ordering is negative-half then positive-half.
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "iv-div with non-straddling denominator does not tag (no cut)"
         (let ((r (iv-div (make-defined-cont 1d0 1d0)
                          (make-defined-cont 2d0 3d0)
                          0)))
           (and (= 1 (length r))
                (null (ival-branch (first r))))))
  (check "iv-div with no site keeps NIL branch (back-compat)"
         (let ((r (iv-div (make-defined-cont 1d0 1d0)
                          (make-defined-cont -1d0 1d0))))
           (every (lambda (iv) (null (ival-branch iv))) r)))

  (format t "~&== iv-tan site tagging ==~%")
  (check "iv-tan with range crossing pi/2 tags both segments"
         ;; Range straddling pi/2 (~1.5708): use [1.0, 2.0]
         (let ((r (iv-tan (make-defined-cont 1d0 2d0) 0)))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "iv-tan with range entirely between asymptotes is unsited"
         (let ((r (iv-tan (make-defined-cont 0.1d0 1.4d0) 0)))
           (and (= 1 (length r))
                (null (ival-branch (first r))))))
  (check "iv-tan with no site keeps NIL branch (back-compat)"
         (let ((r (iv-tan (make-defined-cont 1d0 2d0))))
           (every (lambda (iv) (null (ival-branch iv))) r)))

  (format t "~&== iv-sgn site tagging ==~%")
  (check "iv-sgn [-1,1] :site 0 tags 3 pieces with chosen 0/1/2"
         (let ((r (iv-sgn (make-defined-cont -1d0 1d0) 0)))
           (and (= 3 (length r))
                (equal '(3 . 0) (ival-branch (first  r)))   ; -1
                (equal '(3 . 1) (ival-branch (second r)))   ;  0
                (equal '(3 . 2) (ival-branch (third  r)))))) ; +1
  (check "iv-sgn [-1,0] :site 0 tags 2 pieces with chosen 0/1"
         (let ((r (iv-sgn (make-defined-cont -1d0 0d0) 0)))
           (and (= 2 (length r))
                (equal '(3 . 0) (ival-branch (first  r)))
                (equal '(3 . 1) (ival-branch (second r))))))
  (check "iv-sgn [0,1] :site 0 tags 2 pieces with chosen 1/2"
         (let ((r (iv-sgn (make-defined-cont 0d0 1d0) 0)))
           (and (= 2 (length r))
                (equal '(3 . 1) (ival-branch (first  r)))
                (equal '(3 . 2) (ival-branch (second r))))))
  (check "iv-sgn fully positive is unsited (single piece, no cut)"
         (let ((r (iv-sgn (make-defined-cont 1d0 2d0) 0)))
           (and (= 1 (length r))
                (null (ival-branch (first r))))))
  (check "iv-sgn :site 3 places bits at positions 3,4 (mask=24)"
         (let ((r (iv-sgn (make-defined-cont -1d0 1d0) 3)))
           (and (equal '(24 . 0)  (ival-branch (first  r)))
                (equal '(24 . 8)  (ival-branch (second r)))   ; chosen=01<<3=8
                (equal '(24 . 16) (ival-branch (third  r)))))) ; chosen=10<<3=16
  (check "iv-sgn with no site keeps NIL branch (back-compat)"
         (let ((r (iv-sgn (make-defined-cont -1d0 1d0))))
           (every (lambda (iv) (null (ival-branch iv))) r)))

  (format t "~&== eval-expr :site dispatch ==~%")
  (check "(floor x :site 0) at x=[1.5,2.7] returns two branch-tagged pieces"
         (let ((r (eval-expr '(floor x :site 0)
                             (make-defined-cont 1.5d0 2.7d0)
                             (make-defined-cont 0d0 0d0))))
           (and (= 2 (length r))
                (equal '(1 . 0) (ival-branch (first  r)))
                (equal '(1 . 1) (ival-branch (second r))))))
  (check "annotated formula evaluation: LHS/RHS floor pieces share site"
         ;; Manually annotate (= (+ y (floor x)) (+ 1/3 (floor x))) so both
         ;; floors carry :site 0.  Evaluate L and R; they share matching
         ;; tags so cmp-sets only pairs (L_i, R_i) with equal i.
         (let* ((annotated (assign-sites
                            '(= (+ y (floor x)) (+ 1/3 (floor x)))))
                (lhs (second annotated))
                (rhs (third  annotated))
                (xi (make-defined-cont 0.99d0 1.0d0))
                (yi (make-defined-cont 1.3d0  1.4d0))
                (lvs (eval-expr lhs xi yi))
                (rvs (eval-expr rhs xi yi)))
           ;; Each side has 2 pieces, with branch tags (1 . 0) and (1 . 1).
           (and (= 2 (length lvs)) (= 2 (length rvs))
                (every (lambda (iv) (member (ival-branch iv)
                                            '((1 . 0) (1 . 1)) :test #'equal))
                       (append lvs rvs)))))

  (format t "~&== Algorithm 3.2 acceptance: Figure 11(a) ==~%")
  (check "y + floor(x) = 1/3 + floor(x) finishes (zero red) at 128x128 d=4"
         (let* ((pixmap (graph-formula
                         '(= (+ y (floor x)) (+ 1/3 (floor x)))
                         -10d0 10d0 -10d0 10d0 128 128
                         :max-subpixel-depth 4))
                (red 0))
           (loop for py from 0 below 128 do
             (loop for px from 0 below 128 do
               (when (= (aref pixmap py px) +pixel-red+)
                 (incf red))))
           (format t "~&    [fig11a] red pixels: ~a~%" red)
           (zerop red)))

  (format t "~&Failures: ~a~%" *fail-count*)
  (zerop *fail-count*))
