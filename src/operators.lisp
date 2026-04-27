;;;; operators.lisp --- per-operator interval routines.
;;;; Each operator returns a *list* of ivals (Step 5 design).
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

;;; --- helpers -------------------------------------------------------------

(defun and-defs (&rest ivs)
  "Combine def flags via AND."
  (values (every #'ival-def-lo ivs)
          (every #'ival-def-hi ivs)))

(defun and-conts (&rest ivs)
  (values (every #'ival-cont-lo ivs)
          (every #'ival-cont-hi ivs)))

(defun min4 (a b c d) (min a b c d))
(defun max4 (a b c d) (max a b c d))

;;; --- unary negation ------------------------------------------------------

(defun iv-neg (a)
  (list (make-ival :lo (- (ival-hi a))
                   :hi (- (ival-lo a))
                   :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                   :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))

;;; --- add / sub -----------------------------------------------------------

(defun iv-add (a b)
  (multiple-value-bind (dl dh) (and-defs a b)
    (multiple-value-bind (cl ch) (and-conts a b)
      (declare (ignore ch))
      (list (make-ival :lo (add-down (ival-lo a) (ival-lo b))
                       :hi (add-up   (ival-hi a) (ival-hi b))
                       :def-lo dl :def-hi dh
                       :cont-lo cl :cont-hi t)))))

(defun iv-sub (a b)
  (multiple-value-bind (dl dh) (and-defs a b)
    (multiple-value-bind (cl ch) (and-conts a b)
      (declare (ignore ch))
      (list (make-ival :lo (sub-down (ival-lo a) (ival-hi b))
                       :hi (sub-up   (ival-hi a) (ival-lo b))
                       :def-lo dl :def-hi dh
                       :cont-lo cl :cont-hi t)))))

;;; --- multiply ------------------------------------------------------------

(defun iv-mul (a b)
  (let ((al (ival-lo a)) (ah (ival-hi a))
        (bl (ival-lo b)) (bh (ival-hi b)))
    (declare (type double-float al ah bl bh))
    (let ((lo (min4 (mul-down al bl) (mul-down al bh)
                    (mul-down ah bl) (mul-down ah bh)))
          (hi (max4 (mul-up al bl) (mul-up al bh)
                    (mul-up ah bl) (mul-up ah bh))))
      (multiple-value-bind (dl dh) (and-defs a b)
        (multiple-value-bind (cl ch) (and-conts a b)
          (declare (ignore ch))
          (list (make-ival :lo lo :hi hi
                           :def-lo dl :def-hi dh
                           :cont-lo cl :cont-hi t)))))))

;;; --- divide --------------------------------------------------------------

(defun %iv-div-nozero (a b)
  "B does NOT contain 0; classical four-corner div."
  (let ((al (ival-lo a)) (ah (ival-hi a))
        (bl (ival-lo b)) (bh (ival-hi b)))
    (declare (type double-float al ah bl bh))
    (let ((lo (min4 (div-down al bl) (div-down al bh)
                    (div-down ah bl) (div-down ah bh)))
          (hi (max4 (div-up al bl) (div-up al bh)
                    (div-up ah bl) (div-up ah bh))))
      (multiple-value-bind (dl dh) (and-defs a b)
        (multiple-value-bind (cl ch) (and-conts a b)
          (declare (ignore ch))
          (make-ival :lo lo :hi hi
                     :def-lo dl :def-hi dh
                     :cont-lo cl :cont-hi t))))))

(defun iv-div (a b)
  (cond
    ;; Denominator is exactly {0}: undefined everywhere.
    ((and (zerop (ival-lo b)) (zerop (ival-hi b)))
     (list (make-undefined)))
    ;; Denominator strictly avoids 0: single interval, defined+continuous.
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
       ;; If the denominator interval reaches the actual point 0 from inside,
       ;; the result is undefined exactly at that spot.  Mark def-lo nil and
       ;; cont-lo nil on each piece.
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
       (list (make-ival :lo (max 0d0 (sqrt-down al))
                        :hi (sqrt-up ah)
                        :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))
      (t
       ;; straddles 0: defined only on [0, ah], so def-lo := nil
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
      (list (make-ival :lo lo :hi hi
                       :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                       :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))))

;;; --- exp / log -----------------------------------------------------------

(defun iv-exp (a)
  (list (make-ival :lo (trans-down (exp (ival-lo a)))
                   :hi (trans-up   (exp (ival-hi a)))
                   :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                   :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))

(defun iv-log (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((<= ah 0d0) (list (make-undefined)))
      ((> al 0d0)
       (list (make-ival :lo (trans-down (log al))
                        :hi (trans-up   (log ah))
                        :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))
      (t
       (list (make-ival :lo +neg-inf+
                        :hi (trans-up (log ah))
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t))))))

;;; --- sin / cos -----------------------------------------------------------
;;; A critical-point-aware implementation: scan integer multiples of pi/2
;;; that fall in [lo, hi] and clamp to ±1 when warranted.

(defconstant +2pi+ (* 2d0 (coerce pi 'double-float)))
(defconstant +pi/2+ (* 0.5d0 (coerce pi 'double-float)))

(defun %sin-bounds (lo hi)
  "Return (values smin smax) for sin over [lo,hi] (lo <= hi)."
  (let* ((s1 (sin lo)) (s2 (sin hi))
         (smin (min s1 s2))
         (smax (max s1 s2))
         (pi/2 +pi/2+))
    ;; Scan integers k such that k*pi/2 in (lo, hi) and check sin parity.
    (let ((k-lo (ceiling (/ lo pi/2)))
          (k-hi (floor   (/ hi pi/2))))
      (loop for k from k-lo to k-hi
            for s = (mod k 4) do
              (case s
                (1 (setf smax 1d0))   ; pi/2 + 2k pi -> +1
                (3 (setf smin -1d0))  ; 3pi/2 + 2k pi -> -1
                ;; 0 and 2 are zeros of sin -> already covered by endpoints
                )))
    (values (max -1d0 (trans-down smin))
            (min  1d0 (trans-up   smax)))))

(defun %cos-bounds (lo hi)
  "Return (values cmin cmax) for cos over [lo,hi]."
  (let* ((c1 (cos lo)) (c2 (cos hi))
         (cmin (min c1 c2))
         (cmax (max c1 c2))
         (pi/2 +pi/2+))
    (let ((k-lo (ceiling (/ lo pi/2)))
          (k-hi (floor   (/ hi pi/2))))
      (loop for k from k-lo to k-hi
            for s = (mod k 4) do
              (case s
                (0 (setf cmax 1d0))    ; cos(2k pi) = +1
                (2 (setf cmin -1d0)))))
    (values (max -1d0 (trans-down cmin))
            (min  1d0 (trans-up   cmax)))))

(defun iv-sin (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((or (= al +neg-inf+) (= ah +pos-inf+))
       (list (make-ival :lo -1d0 :hi 1d0
                        :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))
      (t (multiple-value-bind (lo hi) (%sin-bounds al ah)
           (list (make-ival :lo lo :hi hi
                            :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                            :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))))))

(defun iv-cos (a)
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah))
    (cond
      ((or (= al +neg-inf+) (= ah +pos-inf+))
       (list (make-ival :lo -1d0 :hi 1d0
                        :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                        :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))
      (t (multiple-value-bind (lo hi) (%cos-bounds al ah)
           (list (make-ival :lo lo :hi hi
                            :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                            :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))))))

;;; tan: detect crossings of pi/2 + k*pi -> return interval set.
(defun iv-tan (a)
  (let ((al (ival-lo a)) (ah (ival-hi a))
        (pi/2 +pi/2+) (pi-d (coerce pi 'double-float)))
    (declare (type double-float al ah))
    (cond
      ((or (= al +neg-inf+) (= ah +pos-inf+))
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t)))
      (t
       ;; integer k with (k+1/2)*pi in (al, ah)?
       (let* ((k-lo (ceiling (- (/ al pi-d) 0.5d0)))
              (k-hi (floor   (- (/ ah pi-d) 0.5d0))))
         (cond
           ((> k-lo k-hi)
            ;; no asymptote crossed
            (list (make-ival :lo (trans-down (tan al))
                             :hi (trans-up   (tan ah))
                             :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                             :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))
           (t
            ;; conservative: split at each asymptote.
            (let ((segments '())
                  (left al))
              (loop for k from k-lo to k-hi
                    for asym = (* (+ k 0.5d0) pi-d) do
                      (push (cons left asym) segments)
                      (setf left asym))
              (push (cons left ah) segments)
              (mapcar (lambda (seg)
                        (let ((sl (car seg)) (sh (cdr seg)))
                          (make-ival :lo (trans-down (tan sl))
                                     :hi (trans-up   (tan sh))
                                     :def-lo nil :def-hi (ival-def-hi a)
                                     :cont-lo nil :cont-hi t)))
                      (nreverse segments))))))))))

;;; --- pow -----------------------------------------------------------------
;;; Limited support: integer exponents (any base) and real exponents on
;;; positive bases.  Algorithm 3.3 (parity tagging) is left unimplemented.

(defun %iv-int-pow (a n)
  "A^n, n integer."
  (let ((al (ival-lo a)) (ah (ival-hi a)))
    (declare (type double-float al ah)
             (type integer n))
    (cond
      ((zerop n)
       (make-defined-cont 1d0 1d0))
      ((minusp n)
       (let ((p (%iv-int-pow a (- n))))
         (%iv-div-nozero (make-defined-cont 1d0 1d0) p)))
      ((evenp n)
       (let ((lo (cond ((>= al 0d0) al)
                       ((<= ah 0d0) ah)
                       (t 0d0)))
             (hi (if (>= (abs ah) (abs al)) ah al)))
         (let ((vlo (expt (abs lo) n))
               (vhi (expt (abs hi) n)))
           (make-ival :lo (trans-down (min vlo vhi))
                      :hi (trans-up   (max vlo vhi))
                      :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                      :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a)))))
      (t                              ; odd, monotone
       (make-ival :lo (trans-down (expt al n))
                  :hi (trans-up   (expt ah n))
                  :def-lo (ival-def-lo a) :def-hi (ival-def-hi a)
                  :cont-lo (ival-cont-lo a) :cont-hi (ival-cont-hi a))))))

(defun iv-pow (a b)
  "Power: handles integer exponent (constant interval [n,n], n integer)
   or positive base; otherwise widens conservatively (Algorithm 3.3 hook)."
  (let ((bl (ival-lo b)) (bh (ival-hi b)))
    (cond
      ;; Integer exponent (constant interval whose endpoints are equal int).
      ((and (= bl bh)
            (= bl (float (round bl) 1d0))
            (ival-totally-defined-p b))
       (list (%iv-int-pow a (round bl))))
      ;; Positive base: a^b = exp(b * log a).
      ((> (ival-lo a) 0d0)
       (let* ((log-a (car (iv-log a)))
              (b*log (car (iv-mul b log-a))))
         (iv-exp b*log)))
      ;; Otherwise: conservative widening with possibly-undefined.
      (t
       (list (make-ival :lo +neg-inf+ :hi +pos-inf+
                        :def-lo nil :def-hi (ival-def-hi a)
                        :cont-lo nil :cont-hi t))))))
