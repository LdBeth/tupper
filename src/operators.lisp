;;;; operators.lisp --- per-operator interval routines.
;;;; Each operator returns a *list* of ivals (Step 5 design).
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

;;; --- helpers -------------------------------------------------------------
;;; Every binary op AND's the def and cont-lo flags of its inputs and forces
;;; cont-hi := T (the post-op upper-cont is always permissive).  Bundle that
;;; into one helper returning three values.
(defun combine-flags (&rest ivs)
  (values (every #'ival-def-lo  ivs)
          (every #'ival-def-hi  ivs)
          (every #'ival-cont-lo ivs)
          (reduce #'combine-branches ivs
                  :key #'ival-branch :initial-value nil)))

(defmacro with-combined-flags ((dl dh cl br) ivs &body body)
  `(multiple-value-bind (,dl ,dh ,cl ,br) (apply #'combine-flags ,ivs)
     ,@body))

(defun min4 (a b c d) (min a b c d))
(defun max4 (a b c d) (max a b c d))

;;; Widened-to-infinity result with cleared cont-lo: the canonical "after a
;;; discontinuity / undefined region" shape, used by iv-div (zero denom),
;;; iv-tan (asymptote), iv-log (straddles 0), iv-pow (negative-base fallback),
;;; and the step-function infinity guard.
(defun make-widened (def-hi)
  (make-ival :lo +neg-inf+ :hi +pos-inf+
             :def-lo nil :def-hi def-hi
             :cont-lo nil :cont-hi t))

;;; Defined-cont singleton at V (used by sgn).
(defun make-singleton (v dl dh &optional branch)
  (declare (type double-float v))
  (make-ival :lo v :hi v
             :def-lo dl :def-hi dh
             :cont-lo dl :cont-hi dh
             :branch branch))

;;; preserve-flags: a -> ival with the same def/cont/branch flags as A.
(defmacro %preserve (a lo hi)
  (let ((g (gensym)))
    `(let ((,g ,a))
       (make-ival :lo ,lo :hi ,hi
                  :def-lo (ival-def-lo ,g) :def-hi (ival-def-hi ,g)
                  :cont-lo (ival-cont-lo ,g) :cont-hi (ival-cont-hi ,g)
                  :branch (ival-branch ,g)))))

;;; --- unary negation ------------------------------------------------------

(defun iv-neg (a)
  (list (%preserve a (- (ival-hi a)) (- (ival-lo a)))))

;;; --- add / sub / mul (binary, share the flag-combining boilerplate) ------

(defmacro %binary-result (a b lo hi)
  `(with-combined-flags (dl dh cl br) (list ,a ,b)
     (list (make-ival :lo ,lo :hi ,hi
                      :def-lo dl :def-hi dh
                      :cont-lo cl :cont-hi t
                      :branch br))))

(defun iv-add (a b)
  (%binary-result a b
                  (add-down (ival-lo a) (ival-lo b))
                  (add-up   (ival-hi a) (ival-hi b))))

(defun iv-sub (a b)
  (%binary-result a b
                  (sub-down (ival-lo a) (ival-hi b))
                  (sub-up   (ival-hi a) (ival-lo b))))

(defun %corner-bounds (down-op up-op al ah bl bh)
  "Classic four-corner min/max for monotone-corner ops (mul, div)."
  (declare (type double-float al ah bl bh))
  (values
   (min4 (funcall down-op al bl) (funcall down-op al bh)
         (funcall down-op ah bl) (funcall down-op ah bh))
   (max4 (funcall up-op al bl) (funcall up-op al bh)
         (funcall up-op ah bl) (funcall up-op ah bh))))

(defun iv-mul (a b)
  (multiple-value-bind (lo hi)
      (%corner-bounds #'mul-down #'mul-up
                      (ival-lo a) (ival-hi a) (ival-lo b) (ival-hi b))
    (%binary-result a b lo hi)))

;;; --- divide --------------------------------------------------------------

(defun %iv-div-nozero (a b)
  "B does NOT contain 0; classical four-corner div."
  (multiple-value-bind (lo hi)
      (%corner-bounds #'div-down #'div-up
                      (ival-lo a) (ival-hi a) (ival-lo b) (ival-hi b))
    (with-combined-flags (dl dh cl br) (list a b)
      (make-ival :lo lo :hi hi
                 :def-lo dl :def-hi dh
                 :cont-lo cl :cont-hi t
                 :branch br))))

(defun iv-div (a b &optional site)
  (cond
    ;; Denominator is exactly {0}: undefined everywhere.
    ((and (zerop (ival-lo b)) (zerop (ival-hi b)))
     (list (make-undefined)))
    ;; Denominator strictly avoids 0: single interval, no cut.
    ((not (ival-contains-zero-p b))
     (list (%iv-div-nozero a b)))
    ;; Denominator straddles 0 -> two intervals (interval set).
    (t
     (let* ((bl (ival-lo b))
            (bh (ival-hi b))
            (eps least-positive-normalized-double-float)
            (mask (and site (ash 1 site)))
            (results '()))
       (when (< bl 0d0)
         (let ((b- (make-ival :lo bl :hi (- eps)
                              :def-lo (ival-def-lo b) :def-hi (ival-def-hi b)
                              :cont-lo nil :cont-hi t
                              :branch (ival-branch b))))
           (push (%iv-div-nozero a b-) results)))
       (when (> bh 0d0)
         (let ((b+ (make-ival :lo eps :hi bh
                              :def-lo (ival-def-lo b) :def-hi (ival-def-hi b)
                              :cont-lo nil :cont-hi t
                              :branch (ival-branch b))))
           (push (%iv-div-nozero a b+) results)))
       ;; Denominator straddling 0 means the result is undefined exactly
       ;; at that point: stamp def-lo/cont-lo accordingly, then tag with
       ;; site bits (negative-half = chosen 0; positive-half = chosen 1).
       (let* ((ordered (nreverse results))
              (n (length ordered)))
         (loop for iv in ordered
               for i from 0
               do (setf (ival-def-lo iv) nil
                        (ival-cont-lo iv) nil)
                  (when (and site (= n 2))
                    (let ((br (combine-branches
                               (ival-branch iv)
                               (cons mask (if (zerop i) 0 mask)))))
                      (setf (ival-branch iv) br))))
         ordered)))))

;;; --- sqrt ----------------------------------------------------------------

(defun iv-sqrt (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((< ah 0d0) (list (make-undefined)))
      ((>= al 0d0)
       (list (%preserve a (max 0d0 (sqrt-down al)) (sqrt-up ah))))
      (t
       (list (make-ival :lo 0d0 :hi (sqrt-up ah)
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                        :branch (ival-branch a)))))))

;;; --- abs -----------------------------------------------------------------

(defun iv-abs (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (let ((lo (cond ((>= al 0d0) al)
                    ((<= ah 0d0) (- ah))
                    (t 0d0)))
          (hi (max (abs al) (abs ah))))
      (list (%preserve a lo hi)))))

;;; --- exp / log -----------------------------------------------------------

(defun iv-exp (a)
  (list (%preserve a
                   (trans-down (exp (ival-lo a)))
                   (trans-up   (exp (ival-hi a))))))

(defun iv-log (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((<= ah 0d0) (list (make-undefined)))
      ((> al 0d0)
       (list (%preserve a (trans-down (log al)) (trans-up (log ah)))))
      (t
       (list (make-ival :lo +neg-inf+
                        :hi (trans-up (log ah))
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t
                        :branch (ival-branch a)))))))

;;; --- sin / cos / tan -----------------------------------------------------
;;; sin and cos share a scaffold: a period-guard, endpoint init, and a scan
;;; over k in [ceil(lo/(pi/2)), floor(hi/(pi/2))] checking whether (mod k 4)
;;; matches a residue that pins a critical extremum.  Parameterize by which
;;; residue triggers which extremum.

(defconstant +2pi+  (* 2d0 (coerce pi 'double-float)))
(defconstant +pi-d+ (coerce pi 'double-float))
(defconstant +pi/2+ (* 0.5d0 (coerce pi 'double-float)))

(defun %trig-bounds (fn min-residues max-residues lo hi)
  "Return (values vmin vmax) of FN over [lo, hi].  MIN-RESIDUES and
   MAX-RESIDUES are lists of (mod k 4) values where a critical point pins
   the min to -1 or the max to +1, respectively."
  (when (>= (- hi lo) +2pi+)
    (return-from %trig-bounds (values -1d0 1d0)))
  (let* ((v1 (funcall fn lo)) (v2 (funcall fn hi))
         (vmin (min v1 v2)) (vmax (max v1 v2)))
    (loop for k from (ceiling (/ lo +pi/2+)) to (floor (/ hi +pi/2+))
          for s = (mod k 4) do
            (when (member s min-residues) (setf vmin -1d0))
            (when (member s max-residues) (setf vmax  1d0)))
    (values (max -1d0 (trans-down vmin))
            (min  1d0 (trans-up   vmax)))))

(defun %sin-bounds (lo hi)
  ;; sin: residue 1 -> +1 (max), 3 -> -1 (min); 0,2 are zeros (endpoints handle).
  (%trig-bounds #'sin '(3) '(1) lo hi))

(defun %cos-bounds (lo hi)
  ;; cos: residue 0 -> +1 (max), 2 -> -1 (min).
  (%trig-bounds #'cos '(2) '(0) lo hi))

(defun %iv-sinusoid (a bounds-fn)
  "Shared shell for iv-sin / iv-cos."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (multiple-value-bind (lo hi)
        (if (or (= al +neg-inf+) (= ah +pos-inf+))
            (values -1d0 1d0)
            (funcall bounds-fn al ah))
      (list (%preserve a lo hi)))))

(defun iv-sin (a) (%iv-sinusoid a #'%sin-bounds))
(defun iv-cos (a) (%iv-sinusoid a #'%cos-bounds))

;;; tan: detect crossings of pi/2 + k*pi -> return interval set.
;;; Range >= pi already widens (single ival), so the post-tagging code
;;; sees at most 2 pieces; one bit per occurrence is enough.
(defun iv-tan (a &optional site)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((or (= al +neg-inf+) (= ah +pos-inf+)
           (>= (- ah al) +pi-d+))
       (list (make-widened (ival-def-hi a))))
      (t
       (let* ((k-lo (ceiling (- (/ al +pi-d+) 0.5d0)))
              (k-hi (floor   (- (/ ah +pi-d+) 0.5d0))))
         (cond
           ((> k-lo k-hi)
            (list (%preserve a (trans-down (tan al)) (trans-up (tan ah)))))
           (t
            (let ((segments '())
                  (left al))
              (loop for k from k-lo to k-hi
                    for asym = (* (+ k 0.5d0) +pi-d+) do
                      (push (cons left asym) segments)
                      (setf left asym))
              (push (cons left ah) segments)
              (let* ((ordered (nreverse segments))
                     (mask (and site (ash 1 site)))
                     (pieces
                       (loop for seg in ordered
                             for i from 0
                             collect
                             (let ((iv (make-ival
                                        :lo (trans-down (tan (car seg)))
                                        :hi (trans-up   (tan (cdr seg)))
                                        :def-lo nil :def-hi (ival-def-hi a)
                                        :cont-lo nil :cont-hi t
                                        :branch (ival-branch a)))
                                   (br-add (when (and site
                                                      (>= (length ordered) 2))
                                             (cons mask
                                                   (if (zerop i) 0 mask)))))
                               (when br-add
                                 (setf (ival-branch iv)
                                       (combine-branches
                                        (ival-branch iv) br-add)))
                               iv))))
                pieces)))))))))

;;; --- pow -----------------------------------------------------------------
;;; Integer exponents (any base) and real exponents on positive bases handled
;;; tightly.  For negative base + literal rational p/q exponent with odd q,
;;; the parser-side parity tag (Algorithm 3.3 lite) routes through %iv-rat-pow
;;; (defined further down with iv-nth-root); see iv-pow below.  All other
;;; cases widen to [-inf,+inf] with def<F,T>.

(defun %iv-int-pow (a n)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah)
             (type integer n))
    (cond
      ((zerop n) (make-defined-cont 1d0 1d0))
      ((minusp n)
       (%iv-div-nozero (make-defined-cont 1d0 1d0) (%iv-int-pow a (- n))))
      ((evenp n)
       ;;; mag-lo = min|x| over [al,ah]; mag-hi = max|x| over [al,ah].
       ;;; Even n makes x^n strictly increasing in |x|, so mag-lo^n ≤ mag-hi^n
       ;;; always — the min/max guard around vlo/vhi is an identity and drops out.
       (let* ((mag-lo (cond ((>= al 0d0) al) ((<= ah 0d0) (- ah)) (t 0d0)))
              (mag-hi (max (abs al) (abs ah))))
         (%preserve a
                    (trans-down (expt mag-lo n))
                    (trans-up   (expt mag-hi n)))))
      (t                                ; odd, monotone
       (%preserve a (trans-down (expt al n)) (trans-up (expt ah n)))))))

;;; --- min / max -----------------------------------------------------------
;;; Both monotone in each argument: the bound is just min/max of corresponding
;;; endpoints.  def/cont are AND'd; cont-hi forced T (consistent with other
;;; binary ops).

(defun iv-min (a b)
  (with-combined-flags (dl dh cl br) (list a b)
    (list (make-ival :lo (min (ival-lo a) (ival-lo b))
                     :hi (min (ival-hi a) (ival-hi b))
                     :def-lo dl :def-hi dh
                     :cont-lo cl :cont-hi t
                     :branch br))))

(defun iv-max (a b)
  (with-combined-flags (dl dh cl br) (list a b)
    (list (make-ival :lo (max (ival-lo a) (ival-lo b))
                     :hi (max (ival-hi a) (ival-hi b))
                     :def-lo dl :def-hi dh
                     :cont-lo cl :cont-hi t
                     :branch br))))

;;; --- median (3-arg) ------------------------------------------------------
;;; median(a,b,c) = a + b + c - min(a,b,c) - max(a,b,c).  The bound is the
;;; hull of the medians at the 8 corner combinations.

(declaim (inline %median3))
(defun %median3 (a b c)
  (declare (type double-float a b c))
  (- (+ a b c) (min a b c) (max a b c)))

(defun iv-median (a b c)
  (let ((vmin +pos-inf+)
        (vmax +neg-inf+))
    (declare (type double-float vmin vmax))
    (dolist (xa (list (ival-lo a) (ival-hi a)))
      (dolist (xb (list (ival-lo b) (ival-hi b)))
        (dolist (xc (list (ival-lo c) (ival-hi c)))
          (let ((m (%median3 xa xb xc)))
            (when (< m vmin) (setf vmin m))
            (when (> m vmax) (setf vmax m))))))
    (with-combined-flags (dl dh cl br) (list a b c)
      (list (make-ival :lo vmin :hi vmax
                       :def-lo dl :def-hi dh
                       :cont-lo cl :cont-hi t
                       :branch br)))))

;;; --- floor / ceil / round / trunc ---------------------------------------
;;; All four are step functions; the implementation pattern is from the paper
;;; (section 11): inspect the integer-step count between f(lo) and f(hi).
;;; Zero steps  -> single defined-cont singleton at that integer.
;;; One step    -> two singleton intervals (the discontinuity straddles).
;;; >1 step     -> hull [f(lo), f(hi)] with cont-lo := nil.

(defun %iv-step (iv step-fn &optional site)
  (let ((lo (ival-lo iv))
        (hi (ival-hi iv))
        (dl (ival-def-lo iv))
        (dh (ival-def-hi iv))
        (br (ival-branch iv)))
    (declare (type double-float lo hi))
    (cond
      ((or (= lo +neg-inf+) (= hi +pos-inf+))
       (list (make-widened dh)))
      (t
       (let ((flo (funcall step-fn lo))
             (fhi (funcall step-fn hi)))
         (declare (type double-float flo fhi))
         (cond
           ((= flo fhi)
            ;; Singleton-step case: no cut performed; preserve input branch.
            (list (make-ival :lo flo :hi flo
                             :def-lo dl :def-hi dh
                             :cont-lo dl :cont-hi dh
                             :branch br)))
           ((= (- fhi flo) 1d0)
            ;; Two-piece cut: tag piece 0 with chosen=0, piece 1 with
            ;; chosen=(ash 1 site).  Untagged when SITE is NIL.
            ;; Each piece is well-defined on its subdomain; the branch
            ;; tag (not the def flag) encodes the domain restriction.
            ;; cmp-sets filters by branch compatibility to prevent
            ;; cross-branch false positives.
            (let* ((mask (and site (ash 1 site)))
                   (br0  (if site (combine-branches br (cons mask 0))    br))
                   (br1  (if site (combine-branches br (cons mask mask)) br)))
              (list (make-ival :lo flo :hi flo
                               :def-lo dl :def-hi dh
                               :cont-lo nil :cont-hi dh
                               :branch br0)
                    (make-ival :lo fhi :hi fhi
                               :def-lo dl :def-hi dh
                               :cont-lo nil :cont-hi dh
                               :branch br1))))
           (t
            ;; Hull case (>1 step): result spans many cuts; conservatively
            ;; preserve input branch -- no per-piece distinction available.
            (list (make-ival :lo flo :hi fhi
                             :def-lo dl :def-hi dh
                             :cont-lo nil :cont-hi dh
                             :branch br)))))))))

(defun %ffloor1 (x) (declare (type double-float x)) (values (ffloor x)))
(defun %fceil1  (x) (declare (type double-float x)) (values (fceiling x)))
(defun %fround1 (x) (declare (type double-float x)) (values (fround x)))
(defun %ftrunc1 (x) (declare (type double-float x)) (values (ftruncate x)))

(defun iv-floor (iv &optional site) (%iv-step iv #'%ffloor1 site))
(defun iv-ceil  (iv &optional site) (%iv-step iv #'%fceil1  site))
;;; round: CL's fround is round-half-to-even (banker's rounding).
(defun iv-round (iv &optional site) (%iv-step iv #'%fround1 site))
;;; trunc: ftruncate is floor for x>=0, ceil for x<0.  The step pattern at
;;; every integer is the same as floor's, so the shared scaffold suffices.
(defun iv-trunc (iv &optional site) (%iv-step iv #'%ftrunc1 site))

;;; --- sgn -----------------------------------------------------------------
;;; Three-valued: -1, 0, +1.  When the input straddles 0 the result is an
;;; interval set covering the relevant subset of {-1, 0, 1}, with cont-lo
;;; cleared (sgn is discontinuous at 0).

;;; sgn: three-valued: -1, 0, +1.  When the input straddles 0 the result
;;; is an interval set covering the relevant subset of {-1, 0, 1}, with
;;; cont-lo cleared (sgn is discontinuous at 0).  Branch tagging
;;; (Algorithm 3.2) reserves 2 bit-positions per occurrence (site, site+1)
;;; encoding pieces -1, 0, +1 as chosen-bits 00, 01, 10 respectively.

(defun iv-sgn (iv &optional site)
  (let ((lo (ival-lo iv))
        (hi (ival-hi iv))
        (dl (ival-def-lo iv))
        (dh (ival-def-hi iv))
        (br (ival-branch iv)))
    (declare (type double-float lo hi))
    (cond
      ((< hi 0d0) (list (make-singleton -1d0 dl dh br)))
      ((> lo 0d0) (list (make-singleton  1d0 dl dh br)))
      ((and (= lo 0d0) (= hi 0d0)) (list (make-singleton 0d0 dl dh br)))
      (t
       (let ((mask (and site (ash 3 site)))
             (pieces '()))
         (when (< lo 0d0)
           (let ((iv (make-singleton -1d0 nil dh br)))
             (when site
               (setf (ival-branch iv)
                     (combine-branches br (cons mask 0))))
             (push iv pieces)))
         ;; The 0 piece is always included in the straddle case.
         (let ((iv (make-singleton 0d0 nil dh br)))
           (when site
             (setf (ival-branch iv)
                   (combine-branches br (cons mask (ash 1 site)))))
           (push iv pieces))
         (when (> hi 0d0)
           (let ((iv (make-singleton 1d0 nil dh br)))
             (when site
               (setf (ival-branch iv)
                     (combine-branches br (cons mask (ash 2 site)))))
             (push iv pieces)))
         (nreverse pieces))))))

;;; --- nth-root ------------------------------------------------------------
;;; iv-nth-root N X: real nth-root.  N is a positive integer.
;;;   - odd N  -> total function on R; sign-preserving and monotone increasing.
;;;   - even N -> domain {x >= 0}; clip / mark def<F,T> on partial overlap.
;;; Reliable evaluation uses libm's `expt` on the magnitude, widened by 1 ULP.

(defun %nth-root-pos (x n)
  "x >= 0, n >= 1.  Returns (values lo hi), the widened nth-root bracket."
  (declare (type double-float x) (type integer n))
  (let ((v (expt x (/ 1d0 (float n 1d0)))))
    (declare (type double-float v))
    (values (max 0d0 (dec-ulp v)) (inc-ulp v))))

(defun iv-nth-root (n a)
  (declare (type integer n))
  (when (or (not (integerp n)) (<= n 0))
    (error "iv-nth-root: N must be a positive integer, got ~a" n))
  (cond
    ((= n 1) (list a))
    ((oddp n)
     (let* ((al (ival-lo a)) (ah (ival-hi a)))
       (declare (type double-float al ah))
       ;; Odd nth-root is monotone increasing on all of R.  Compute via the
       ;; non-negative magnitude routine, then sign-flip when input < 0.
       ;;   For x > 0: lo = root_pos_down(x), hi = root_pos_up(x).
       ;;   For x < 0: nth-root(x) = -nth-root(-x); to round DOWN (toward
       ;;   -inf) we want the most-negative result, i.e. -root_pos_up(-x);
       ;;   to round UP we want the least-negative result, i.e. -root_pos_down(-x).
       (flet ((root-up (x)
                (declare (type double-float x))
                (cond ((zerop x) 0d0)
                      ((plusp x)
                       (multiple-value-bind (lo hi) (%nth-root-pos x n)
                         (declare (ignore lo)) hi))
                      (t
                       (multiple-value-bind (lo hi) (%nth-root-pos (- x) n)
                         (declare (ignore hi)) (- lo)))))
              (root-down (x)
                (declare (type double-float x))
                (cond ((zerop x) 0d0)
                      ((plusp x)
                       (multiple-value-bind (lo hi) (%nth-root-pos x n)
                         (declare (ignore hi)) lo))
                      (t
                       (multiple-value-bind (lo hi) (%nth-root-pos (- x) n)
                         (declare (ignore lo)) (- hi))))))
         (list (%preserve a (root-down al) (root-up ah))))))
    (t
     ;; Even n: domain x >= 0.  Monotone increasing on [0, +inf).
     (let ((al (ival-lo a)) (ah (ival-hi a)))
       (declare (type double-float al ah))
       (cond
         ((< ah 0d0) (list (make-undefined)))
         ((>= al 0d0)
          (multiple-value-bind (lo-l lo-h) (%nth-root-pos al n)
            (declare (ignore lo-h))
            (multiple-value-bind (hi-l hi-h) (%nth-root-pos ah n)
              (declare (ignore hi-l))
              (list (%preserve a lo-l hi-h)))))
         (t
          (multiple-value-bind (hi-l hi-h) (%nth-root-pos ah n)
            (declare (ignore hi-l))
            (list (make-ival :lo 0d0 :hi hi-h
                             :def-lo nil :def-hi (ival-def-hi a)
                             :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                             :branch (ival-branch a))))))))))

;;; --- pow extension: rational exponent on negative base ------------------
;;; %iv-rat-pow A NUM DEN PARITY: A^(NUM/DEN) where DEN is odd (so the
;;; expression is real for any real base).  PARITY is (:num-odd O? :den-odd O?).
;;; Result sign: positive if NUM even; sign of base if NUM odd.
;;; Implementation: compute |a|^(num/den) via %nth-root then integer power.

(defun %iv-rat-pow-pos-base (a num den)
  "A is non-negative, DEN >= 1; compute A^(num/den) for any signed integer NUM."
  (declare (type integer num den))
  (let ((root (car (iv-nth-root den a))))
    (cond
      ((zerop num) (list (make-defined-cont 1d0 1d0)))
      ((plusp num)
       (list (%iv-int-pow root num)))
      (t
       (list (%iv-div-nozero (make-defined-cont 1d0 1d0)
                             (%iv-int-pow root (- num))))))))

(defun %iv-rat-pow (a num den)
  "Rational power A^(NUM/DEN) where DEN is odd (real-valued for any A)."
  (declare (type integer num den))
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((>= al 0d0)
       (%iv-rat-pow-pos-base a num den))
      ((<= ah 0d0)
       ;; A is non-positive: a^(num/den) = sign * |a|^(num/den)
       ;; sign = +1 if num even, -1 if num odd (since den odd).
       (let* ((mag-iv (car (iv-neg a)))           ; [-ah, -al], non-negative
              (root-set (%iv-rat-pow-pos-base mag-iv num den)))
         (cond
           ((evenp num) root-set)
           (t (ivs-apply-unary #'iv-neg root-set)))))
      (t
       ;; A straddles 0.  Split: [al, 0] and [0, ah]; recurse.
       (cond
         ((minusp num)
          ;; Negative exponent: pole at 0, return widened with cont<F,T>.
          (list (make-widened (ival-def-hi a))))
         (t
          (let* ((neg-part (make-ival :lo al :hi 0d0
                                      :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                                      :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                                      :branch (ival-branch a)))
                 (pos-part (make-ival :lo 0d0 :hi ah
                                      :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                                      :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                                      :branch (ival-branch a))))
            (append (%iv-rat-pow neg-part num den)
                    (%iv-rat-pow pos-part num den)))))))))

;;; Replace iv-pow with a parity-aware version.  PARITY (optional 3rd arg)
;;; is a plist `(:num-odd ON :den-odd OD :num N :den D)` filled in by the
;;; parser when the exponent literal is a recognized rational p/q.

(defun iv-pow (a b &optional parity)
  "Power: integer exponent (any base), positive base (any real exponent),
   or rational p/q with odd q on negative base (via PARITY tag).  Otherwise
   widens to [-inf,+inf] and marks def<F,T>."
  (let ((bl (ival-lo b)) (bh (ival-hi b)))
    (cond
      ((and (= bl bh)
            (= bl (float (round bl) 1d0))
            (ival-totally-defined-p b))
       (list (%iv-int-pow a (round bl))))
      ((> (ival-lo a) 0d0)
       (let* ((log-a (car (iv-log a)))
              (b*log (car (iv-mul b log-a))))
         (iv-exp b*log)))
      ;; Negative base + literal rational p/q with odd q -> exact via parity.
      ((and parity
            (getf parity :den-odd)
            (= bl bh)
            (let ((num (getf parity :num))
                  (den (getf parity :den)))
              (and (integerp num) (integerp den)
                   (plusp den) (oddp den)
                   ;; sanity: ensure bl ~ num/den
                   (< (abs (- bl (/ (float num 1d0) (float den 1d0))))
                      1d-12))))
       (let ((num (getf parity :num))
             (den (getf parity :den)))
         (%iv-rat-pow a num den)))
      (t
       (list (make-widened (ival-def-hi a)))))))

;;; --- inverse trig (principal branches) ----------------------------------
;;; Domain handling: if input fully outside domain -> undefined; if partial
;;; overlap -> def<F,T> with value clamped to the valid sub-range.  libm
;;; results widened by 1 ULP via dec-ulp/inc-ulp.

(defconstant +half-pi-up+   (inc-ulp +pi/2+))
(defconstant +half-pi-down+ (dec-ulp +pi/2+))
(defconstant +pi-up+   (inc-ulp +pi-d+))
(defconstant +pi-down+ (dec-ulp +pi-d+))

(defun %asin-down (x)
  (declare (type double-float x))
  (max (- +half-pi-up+) (dec-ulp (asin x))))
(defun %asin-up (x)
  (declare (type double-float x))
  (min +half-pi-up+ (inc-ulp (asin x))))
(defun %acos-down (x)
  (declare (type double-float x))
  (max 0d0 (dec-ulp (acos x))))
(defun %acos-up (x)
  (declare (type double-float x))
  (min +pi-up+ (inc-ulp (acos x))))
(defun %atan-down (x)
  (declare (type double-float x))
  (max (- +half-pi-up+) (dec-ulp (atan x))))
(defun %atan-up (x)
  (declare (type double-float x))
  (min +half-pi-up+ (inc-ulp (atan x))))

(defun iv-arcsin (a)
  "Arcsin: domain [-1, 1], range [-pi/2, pi/2].  Monotone increasing."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ;; Entire input outside [-1, 1] -> undefined.
      ((or (< ah -1d0) (> al 1d0))
       (list (make-undefined)))
      ;; Fully inside [-1, 1] -> total, monotone.
      ((and (>= al -1d0) (<= ah 1d0))
       (list (%preserve a (%asin-down al) (%asin-up ah))))
      (t
       ;; Partial overlap: clip to [-1, 1] and mark def<F,T>.
       (let* ((cl (max al -1d0))
              (ch (min ah  1d0)))
         (list (make-ival :lo (%asin-down cl)
                          :hi (%asin-up ch)
                          :def-lo nil :def-hi (ival-def-hi a)
                          :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                          :branch (ival-branch a))))))))

(defun iv-arccos (a)
  "Arccos: domain [-1, 1], range [0, pi].  Monotone decreasing."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((or (< ah -1d0) (> al 1d0))
       (list (make-undefined)))
      ((and (>= al -1d0) (<= ah 1d0))
       ;; decreasing: lo of result = acos(ah); hi = acos(al)
       (list (%preserve a (%acos-down ah) (%acos-up al))))
      (t
       (let* ((cl (max al -1d0))
              (ch (min ah  1d0)))
         (list (make-ival :lo (%acos-down ch)
                          :hi (%acos-up cl)
                          :def-lo nil :def-hi (ival-def-hi a)
                          :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                          :branch (ival-branch a))))))))

(defun iv-arctan (a)
  "Arctan: domain (-inf, inf), range (-pi/2, pi/2).  Monotone increasing."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (let ((lo (cond ((= al +neg-inf+) (- +half-pi-up+))
                    (t (%atan-down al))))
          (hi (cond ((= ah +pos-inf+) +half-pi-up+)
                    (t (%atan-up ah)))))
      (list (%preserve a lo hi)))))

(defun iv-arccsc (a)
  "Arccsc(x) = Arcsin(1/x).  Domain (-inf, -1] U [1, inf)."
  (let ((reciprocal (iv-div (make-defined-cont 1d0 1d0) a)))
    (ivs-apply-unary #'iv-arcsin reciprocal)))

(defun iv-arcsec (a)
  "Arcsec(x) = Arccos(1/x).  Domain (-inf, -1] U [1, inf)."
  (let ((reciprocal (iv-div (make-defined-cont 1d0 1d0) a)))
    (ivs-apply-unary #'iv-arccos reciprocal)))

(defun iv-arccot (a)
  "Arccot: range (0, pi).  Convention: arccot(x) = pi/2 - arctan(x).
   Total and monotone decreasing on R."
  (let* ((at (car (iv-arctan a)))
         (al (ival-lo at)) (ah (ival-hi at)))
    (declare (type double-float al ah))
    ;; pi/2 - [al, ah] = [pi/2 - ah, pi/2 - al]; widen ULP at each end.
    (let ((lo (max 0d0 (sub-down +half-pi-down+ ah)))
          (hi (min +pi-up+ (sub-up +half-pi-up+ al))))
      (list (%preserve a lo hi)))))

;;; --- gamma / factorial --------------------------------------------------
;;; Use libm tgamma via SB-ALIEN; widen by 1 ULP each side.  Poles at the
;;; non-positive integers 0, -1, -2, ...  When the input straddles a pole,
;;; split at each pole; on each piece, gamma is finite and monotone on
;;; large enough sub-ranges.  For safety we just return [-inf, +inf] with
;;; def<F,T> cont<F,T> when a pole is touched.

(sb-alien:define-alien-routine ("tgamma" %c-tgamma) sb-alien:double
  (x sb-alien:double))

(declaim (inline %tgamma))
(defun %tgamma (x)
  (declare (type double-float x))
  (%c-tgamma x))

(defconstant +gamma-xmin+ 1.4616321449683623d0)
(defvar +gamma-ymin+ (load-time-value (%c-tgamma 1.4616321449683623d0)))

(defun %integer-in-range-p (lo hi)
  "T iff there is an integer k with lo <= k <= hi (closed interval)."
  (declare (type double-float lo hi))
  (<= (ceiling lo) (floor hi)))

(defun iv-gamma (a)
  "Gamma function via libm tgamma + 1-ULP widening.  Poles at non-positive
   integers force a widened result with def/cont = <F, T>."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ;; Infinity in the input -> bail to widened.
      ((or (= al +neg-inf+) (= ah +pos-inf+))
       (list (make-widened (ival-def-hi a))))
      ;; Strictly positive: gamma is finite and continuous on (0, +inf);
      ;; non-monotone (minimum near x ~= 1.4616).  Conservatively scan
      ;; endpoints + the minimum if it falls inside [al, ah].
      ((> al 0d0)
       (let* ((vlo (%tgamma al))
              (vhi (%tgamma ah))
              (lo (min vlo vhi))
              (hi (max vlo vhi)))
         (when (and (<= al +gamma-xmin+) (>= ah +gamma-xmin+))
           (setf lo (min lo +gamma-ymin+)))
         (list (%preserve a (dec-ulp lo) (inc-ulp hi)))))
      ;; Any pole (non-positive integer) in [al, min(ah, 0)] -> widen
      ;; to [-inf, +inf] with def/cont = <F, T>.  This subsumes the
      ;; al <= 0 <= ah straddle case (0 is the integer hit).
      ((%integer-in-range-p al (min ah 0d0))
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t
                        :branch (ival-branch a))))
      (t
       ;; ah < 0 and no integer in [al, ah]: gamma is continuous and finite
       ;; on this open segment between two consecutive negative-integer
       ;; poles.  It is non-monotone (one extremum per segment) and grows
       ;; unboundedly toward the poles, so a tight hull would need the
       ;; segment's extremum.  Conservative: widen to [-inf, +inf] while
       ;; preserving def/cont (no pole touched here).
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)
                        :branch (ival-branch a)))))))

(defun iv-factorial (a)
  "Factorial = gamma(x + 1).  For non-integer-bound input mark def<F,T>."
  (let* ((shifted (car (iv-add a (make-defined-cont 1d0 1d0))))
         (g (iv-gamma shifted)))
    ;; Factorial is conventionally only defined on non-negative integers;
    ;; if either endpoint is non-integer, mark def-lo := nil.
    (let* ((al (ival-lo a)) (ah (ival-hi a))
           (al-int (= al (float (round al) 1d0)))
           (ah-int (= ah (float (round ah) 1d0)))
           (all-int (and al-int ah-int (= al ah))))
      (cond
        (all-int g)
        (t (mapcar (lambda (iv)
                     (make-ival :lo (ival-lo iv) :hi (ival-hi iv)
                                :def-lo nil :def-hi (ival-def-hi iv)
                                :cont-lo (ival-cont-lo iv)
                                :cont-hi (ival-cont-hi iv)
                                :branch (ival-branch iv)))
                   g))))))
