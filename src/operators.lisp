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
          (every #'ival-cont-lo ivs)))

(defmacro with-combined-flags ((dl dh cl) ivs &body body)
  `(multiple-value-bind (,dl ,dh ,cl) (apply #'combine-flags ,ivs)
     ,@body))

(defun min4 (a b c d) (min a b c d))
(defun max4 (a b c d) (max a b c d))

;;; preserve-flags: a -> ival with the same def/cont flags as A.
(defmacro %preserve (a lo hi)
  (let ((g (gensym)))
    `(let ((,g ,a))
       (make-ival :lo ,lo :hi ,hi
                  :def-lo (ival-def-lo ,g) :def-hi (ival-def-hi ,g)
                  :cont-lo (ival-cont-lo ,g) :cont-hi (ival-cont-hi ,g)))))

;;; --- unary negation ------------------------------------------------------

(defun iv-neg (a)
  (list (%preserve a (- (ival-hi a)) (- (ival-lo a)))))

;;; --- add / sub / mul (binary, share the flag-combining boilerplate) ------

(defmacro %binary-result (a b lo hi)
  `(with-combined-flags (dl dh cl) (list ,a ,b)
     (list (make-ival :lo ,lo :hi ,hi
                      :def-lo dl :def-hi dh
                      :cont-lo cl :cont-hi t))))

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
    (with-combined-flags (dl dh cl) (list a b)
      (make-ival :lo lo :hi hi
                 :def-lo dl :def-hi dh
                 :cont-lo cl :cont-hi t))))

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
                              :cont-lo nil :cont-hi t)))
           (push (%iv-div-nozero a b-) results)))
       (when (> bh 0d0)
         (let ((b+ (make-ival :lo eps :hi bh
                              :def-lo (ival-def-lo b) :def-hi (ival-def-hi b)
                              :cont-lo nil :cont-hi t)))
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
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)))))))

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
                        :cont-lo nil :cont-hi t))))))

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
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t)))
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
                                   :cont-lo nil :cont-hi t))
                      (nreverse segments))))))))))

;;; --- pow -----------------------------------------------------------------
;;; Limited support: integer exponents (any base) and real exponents on
;;; positive bases.  Algorithm 3.3 (parity tagging) is left unimplemented.

(defun %iv-int-pow (a n)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah)
             (type integer n))
    (cond
      ((zerop n) (make-defined-cont 1d0 1d0))
      ((minusp n)
       (%iv-div-nozero (make-defined-cont 1d0 1d0) (%iv-int-pow a (- n))))
      ((evenp n)
       (let ((lo (cond ((>= al 0d0) al)
                       ((<= ah 0d0) ah)
                       (t 0d0)))
             (hi (if (>= (abs ah) (abs al)) ah al)))
         (let ((vlo (expt (abs lo) n))
               (vhi (expt (abs hi) n)))
           (%preserve a
                      (trans-down (min vlo vhi))
                      (trans-up   (max vlo vhi))))))
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
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t))))))
