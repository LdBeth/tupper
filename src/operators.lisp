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

(defun iv-div (a b)
  (cond
    ;; Denominator is exactly {0}: undefined everywhere.
    ((and (zerop (ival-lo b)) (zerop (ival-hi b)))
     (list (make-undefined)))
    ;; Denominator strictly avoids 0: single interval.
    ((not (ival-contains-zero-p b))
     (list (%iv-div-nozero a b)))
    ;; Denominator straddles 0 -> two intervals (interval set).
    (t
     (let* ((bl (ival-lo b))
            (bh (ival-hi b))
            (eps least-positive-normalized-double-float)
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
       ;; at that point: stamp def-lo/cont-lo accordingly.
       (mapcar (lambda (iv)
                 (setf (ival-def-lo iv) nil
                       (ival-cont-lo iv) nil)
                 iv)
               (nreverse results))))))

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

(defconstant +2pi+ (* 2d0 (coerce pi 'double-float)))
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
(defun iv-tan (a)
  (let ((al (ival-lo a)) (ah (ival-hi a))
        (pi-d (coerce pi 'double-float)))
    (declare (type double-float al ah))
    (cond
      ((or (= al +neg-inf+) (= ah +pos-inf+)
           ;; if width covers a full period, at least one asymptote is hit
           (>= (- ah al) pi-d))
       (list (make-widened (ival-def-hi a))))
      (t
       (let* ((k-lo (ceiling (- (/ al pi-d) 0.5d0)))
              (k-hi (floor   (- (/ ah pi-d) 0.5d0))))
         (cond
           ((> k-lo k-hi)
            (list (%preserve a (trans-down (tan al)) (trans-up (tan ah)))))
           (t
            (let ((segments '())
                  (left al))
              (loop for k from k-lo to k-hi
                    for asym = (* (+ k 0.5d0) pi-d) do
                      (push (cons left asym) segments)
                      (setf left asym))
              (push (cons left ah) segments)
              (mapcar (lambda (seg)
                        (make-ival :lo (trans-down (tan (car seg)))
                                   :hi (trans-up   (tan (cdr seg)))
                                   :def-lo nil :def-hi (ival-def-hi a)
                                   :cont-lo nil :cont-hi t
                                   :branch (ival-branch a)))
                      (nreverse segments))))))))))

;;; --- pow -----------------------------------------------------------------
;;; Limited support: integer exponents (any base) and real exponents on
;;; positive bases.  Algorithm 3.3 (parity tagging) is left unimplemented;
;;; see plan.md Group C.

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

(defun iv-pow (a b)
  "Power: handles integer exponent (constant interval [n,n], n integer)
   or positive base; otherwise widens conservatively (Algorithm 3.3 hook)."
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
      (t
       (list (make-widened (ival-def-hi a)))))))

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

(defun %iv-step (iv step-fn)
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
            (list (make-ival :lo flo :hi flo
                             :def-lo dl :def-hi dh
                             :cont-lo dl :cont-hi dh
                             :branch br)))
           ((= (- fhi flo) 1d0)
            (list (make-ival :lo flo :hi flo
                             :def-lo nil :def-hi dh
                             :cont-lo nil :cont-hi dh
                             :branch br)
                  (make-ival :lo fhi :hi fhi
                             :def-lo nil :def-hi dh
                             :cont-lo nil :cont-hi dh
                             :branch br)))
           (t
            (list (make-ival :lo flo :hi fhi
                             :def-lo dl :def-hi dh
                             :cont-lo nil :cont-hi dh
                             :branch br)))))))))

(defun %ffloor1 (x) (declare (type double-float x)) (values (ffloor x)))
(defun %fceil1  (x) (declare (type double-float x)) (values (fceiling x)))
(defun %fround1 (x) (declare (type double-float x)) (values (fround x)))
(defun %ftrunc1 (x) (declare (type double-float x)) (values (ftruncate x)))

(defun iv-floor (iv) (%iv-step iv #'%ffloor1))
(defun iv-ceil  (iv) (%iv-step iv #'%fceil1))
;;; round: CL's fround is round-half-to-even (banker's rounding).
(defun iv-round (iv) (%iv-step iv #'%fround1))
;;; trunc: ftruncate is floor for x>=0, ceil for x<0.  The step pattern at
;;; every integer is the same as floor's, so the shared scaffold suffices.
(defun iv-trunc (iv) (%iv-step iv #'%ftrunc1))

;;; --- sgn -----------------------------------------------------------------
;;; Three-valued: -1, 0, +1.  When the input straddles 0 the result is an
;;; interval set covering the relevant subset of {-1, 0, 1}, with cont-lo
;;; cleared (sgn is discontinuous at 0).

(defun iv-sgn (iv)
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
       ;;; make-singleton v nil dh expands to the same make-ival call; use it.
       (loop for (include val) in `((,(< lo 0d0) -1d0)
                                    (t            0d0)
                                    (,(> hi 0d0)  1d0))
             when include collect (make-singleton val nil dh br))))))

;;; --- mod -----------------------------------------------------------------
;;; iv-mod a b = a - b * floor(a/b), composed via the existing set-aware
;;; primitives.  Discontinuities at every wraparound flow naturally out of
;;; the iv-floor split.

(defun iv-mod (a b)
  (let* ((quot (ivs-apply-binary #'iv-div  (list a) (list b)))
         (fl   (ivs-apply-unary  #'iv-floor quot))
         (prod (ivs-apply-binary #'iv-mul   (list b) fl)))
    (ivs-apply-binary #'iv-sub (list a) prod)))
