;;;; graph.lisp --- the Graph / RefinePixels / RefineSubpixel drivers.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun pixel-bounds-x (px width L R)
  "Outer x-bounds for pixel column PX.  Returns (values lo hi) as double."
  (declare (type fixnum px width)
           (type double-float L R))
  (let ((w (coerce width 'double-float))
        (pf  (coerce px 'double-float))
        (pf+ (coerce (1+ px) 'double-float)))
    (let ((lo (with-rounding :negative-infinity
                (+ L (* (- R L) (/ pf w)))))
          (hi (with-rounding :positive-infinity
                (+ L (* (- R L) (/ pf+ w))))))
      (values lo hi))))

(defun pixel-bounds-y (py height B T*)
  "Outer y-bounds for pixel row PY (PY=0 is bottom)."
  (declare (type fixnum py height)
           (type double-float B T*))
  (let ((h (coerce height 'double-float))
        (pf  (coerce py 'double-float))
        (pf+ (coerce (1+ py) 'double-float)))
    (let ((lo (with-rounding :negative-infinity
                (+ B (* (- T* B) (/ pf h)))))
          (hi (with-rounding :positive-infinity
                (+ B (* (- T* B) (/ pf+ h))))))
      (values lo hi))))

(defun block-bounds-x (px-lo px-hi width L R)
  "Outer x-bounds covering pixel columns [PX-LO, PX-HI]."
  (multiple-value-bind (lo _hi) (pixel-bounds-x px-lo width L R)
    (declare (ignore _hi))
    (multiple-value-bind (_lo hi) (pixel-bounds-x (1- px-hi) width L R)
      (declare (ignore _lo))
      (values lo hi))))
(defun block-bounds-y (py-lo py-hi height B T*)
  (multiple-value-bind (lo _hi) (pixel-bounds-y py-lo height B T*)
    (declare (ignore _hi))
    (multiple-value-bind (_lo hi) (pixel-bounds-y (1- py-hi) height B T*)
      (declare (ignore _lo))
      (values lo hi))))

(defun fill-block (pixmap x-lo x-hi y-lo y-hi color)
  (loop for py from y-lo below y-hi do
    (loop for px from x-lo below x-hi do
      (setf (aref pixmap py px) color))))

(defun graph-formula (formula L R B Top width height
                      &key (max-subpixel-depth 4))
  "Returns a (height x width) pixmap of :black/:white/:red."
  (declare (type fixnum width height))
  (let ((Ld (coerce L 'double-float))
        (Rd (coerce R 'double-float))
        (Bd (coerce B 'double-float))
        (Td (coerce Top 'double-float))
        (pixmap (make-array (list height width) :initial-element :red)))
    ;; Step 1: pyramid of pixel blocks, top-down.  Block size = power of 2.
    (let ((bsz (let ((s 1)) (loop while (and (<= (* s 2) width)
                                             (<= (* s 2) height))
                                  do (setf s (* 2 s)))
                  s)))
      (refine-blocks formula pixmap Ld Rd Bd Td width height bsz))
    ;; Step 2: per-pixel subpixel refinement on remaining red pixels.
    (refine-subpixels formula pixmap Ld Rd Bd Td width height
                      max-subpixel-depth)
    pixmap))

(defun refine-blocks (formula pixmap L R B Top w h bsz)
  "Coarse-to-fine: process all blocks at size BSZ, then BSZ/2, etc., down to 1."
  (let ((blocks (initial-blocks w h bsz)))
    (loop while blocks do
      (let ((next '()))
        (dolist (blk blocks)
          (destructuring-bind (px-lo py-lo size) blk
            (let* ((px-hi (min w (+ px-lo size)))
                   (py-hi (min h (+ py-lo size))))
              ;; Skip if entire block already decided.
              (unless (block-all-decided-p pixmap px-lo py-lo px-hi py-hi)
                (multiple-value-bind (xl xh) (block-bounds-x px-lo px-hi w L R)
                  (multiple-value-bind (yl yh) (block-bounds-y py-lo py-hi h B Top)
                    (let* ((xi (make-defined-cont xl xh))
                           (yi (make-defined-cont yl yh))
                           (b  (eval-formula formula xi yi)))
                      (cond
                        ((eq b :tt) (fill-block pixmap px-lo px-hi py-lo py-hi :black))
                        ((eq b :ff) (fill-block pixmap px-lo px-hi py-lo py-hi :white))
                        ((and (formula-equation-p formula)
                              (ivt-proves-equation-p formula xl xh yl yh))
                         ;; IVT proof: at least one solution in the block.
                         ;; We can't paint the whole block black, but we'll
                         ;; let subpixel pass localize it; mark for refining.
                         (when (> size 1)
                           (push (list px-lo py-lo (max 1 (ash size -1))) next)
                           (let ((s/2 (ash size -1)))
                             (when (> s/2 0)
                               (push (list (+ px-lo s/2) py-lo s/2) next)
                               (push (list px-lo (+ py-lo s/2) s/2) next)
                               (push (list (+ px-lo s/2) (+ py-lo s/2) s/2) next)))))
                        (t
                         (when (> size 1)
                           (let ((s/2 (ash size -1)))
                             (push (list px-lo py-lo s/2) next)
                             (when (< (+ px-lo s/2) w)
                               (push (list (+ px-lo s/2) py-lo s/2) next))
                             (when (< (+ py-lo s/2) h)
                               (push (list px-lo (+ py-lo s/2) s/2) next))
                             (when (and (< (+ px-lo s/2) w)
                                        (< (+ py-lo s/2) h))
                               (push (list (+ px-lo s/2) (+ py-lo s/2) s/2) next)))))))))))))
        (setf blocks next)))))

(defun initial-blocks (w h bsz)
  (let ((out '()))
    (loop for py from 0 below h by bsz do
      (loop for px from 0 below w by bsz do
        (push (list px py bsz) out)))
    out))

(defun block-all-decided-p (pixmap x-lo y-lo x-hi y-hi)
  (loop for py from y-lo below y-hi
        always (loop for px from x-lo below x-hi
                     always (not (eq (aref pixmap py px) :red)))))

;;; --- subpixel refinement -------------------------------------------------

(defun refine-subpixels (formula pixmap L R B Top w h max-depth)
  (loop for py from 0 below h do
    (loop for px from 0 below w do
      (when (eq (aref pixmap py px) :red)
        (multiple-value-bind (xl xh) (pixel-bounds-x px w L R)
          (multiple-value-bind (yl yh) (pixel-bounds-y py h B Top)
            (setf (aref pixmap py px)
                  (subpixel-decide formula xl xh yl yh max-depth))))))))

(defun subpixel-decide (formula xl xh yl yh depth)
  "Recursively probe; returns :black/:white/:red."
  (let* ((xi (make-defined-cont xl xh))
         (yi (make-defined-cont yl yh))
         (b (eval-formula formula xi yi)))
    (cond
      ((eq b :tt) :black)
      ((eq b :ff) :white)
      ((and (formula-equation-p formula)
            (ivt-proves-equation-p formula xl xh yl yh))
       :black)
      ((<= depth 0) :red)
      (t
       ;; Subdivide 2x2; pixel is :white iff all 4 sub-cells are :white,
       ;; :black if any sub-cell is :black, else :red.
       (let* ((xm (* 0.5d0 (+ xl xh)))
              (ym (* 0.5d0 (+ yl yh)))
              (results
               (list (subpixel-decide formula xl xm yl ym (1- depth))
                     (subpixel-decide formula xm xh yl ym (1- depth))
                     (subpixel-decide formula xl xm ym yh (1- depth))
                     (subpixel-decide formula xm xh ym yh (1- depth)))))
         (cond ((some (lambda (c) (eq c :black)) results) :black)
               ((every (lambda (c) (eq c :white)) results) :white)
               (t :red)))))))
