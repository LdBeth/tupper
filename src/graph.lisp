;;;; graph.lisp --- the Graph / RefinePixels / RefineSubpixel drivers.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

;;; Pixel colour constants stored in the (unsigned-byte 8) pixmap.
(defconstant +pixel-black+ 0)
(defconstant +pixel-white+ 1)
(defconstant +pixel-red+   2)

;;; --- pixel / block coordinate bounds ------------------------------------
;;; A pixel column PX of WIDTH spans real-x = L + (R-L) * PX / WIDTH (lo edge)
;;; up to PX+1 (hi edge).  Each edge is computed once with directed rounding
;;; so the rectangle definitely contains the true pixel region.

(defun pixel-edge-x (px width L R rounding)
  (declare (type fixnum px width)
           (type double-float L R))
  (let ((w (coerce width 'double-float))
        (pf (coerce px 'double-float)))
    (with-rounding rounding (+ L (* (- R L) (/ pf w))))))

(defun pixel-edge-y (py height B T* rounding)
  (declare (type fixnum py height)
           (type double-float B T*))
  (let ((h (coerce height 'double-float))
        (pf (coerce py 'double-float)))
    (with-rounding rounding (+ B (* (- T* B) (/ pf h))))))

(defun pixel-bounds-x (px width L R)
  (values (pixel-edge-x px       width L R :negative-infinity)
          (pixel-edge-x (1+ px)  width L R :positive-infinity)))

(defun pixel-bounds-y (py height B T*)
  (values (pixel-edge-y py       height B T* :negative-infinity)
          (pixel-edge-y (1+ py)  height B T* :positive-infinity)))

(defun block-bounds-x (px-lo px-hi width L R)
  "Outer x-bounds covering pixel columns [PX-LO, PX-HI)."
  (values (pixel-edge-x px-lo width L R :negative-infinity)
          (pixel-edge-x px-hi width L R :positive-infinity)))

(defun block-bounds-y (py-lo py-hi height B T*)
  (values (pixel-edge-y py-lo height B T* :negative-infinity)
          (pixel-edge-y py-hi height B T* :positive-infinity)))

(defun fill-block (pixmap x-lo x-hi y-lo y-hi color)
  (declare (type (unsigned-byte 8) color))
  (loop for py from y-lo below y-hi do
    (loop for px from x-lo below x-hi do
      (setf (aref pixmap py px) color))))

;;; --- shared decision kernel ---------------------------------------------
;;; Given a real-coordinate rectangle, evaluate the formula and classify the
;;; rectangle as :black, :white, :ivt (existence proof - solution somewhere
;;; inside but not localized), or :undecided.  Both the block and subpixel
;;; passes use this same kernel; they differ only in how they treat :ivt
;;; (block: refine; subpixel: paint :black).  Algorithm 3.4 (CSE +
;;; axis-only subexpression caching) would attach here; see plan.md.

(defun decide-cell (formula xl xh yl yh)
  (let* ((xi (make-defined-cont xl xh))
         (yi (make-defined-cont yl yh))
         (b  (eval-formula formula xi yi)))
    (cond ((eq b :tt) :black)
          ((eq b :ff) :white)
          ((and (formula-equation-p formula)
                (ivt-proves-equation-p formula xl xh yl yh))
           :ivt)
          (t :undecided))))

;;; --- top-level driver ----------------------------------------------------

(defun graph-formula (formula L R B Top width height
                      &key (max-subpixel-depth 4))
  "Returns a (HEIGHT x WIDTH) pixmap of :black / :white / :red."
  (declare (type fixnum width height)
           (type double-float L R B Top))
  (let ((pixmap (make-array (list height width)
                            :element-type '(unsigned-byte 8)
                            :initial-element +pixel-red+)))
    ;; Step 1: pyramid of pixel blocks, top-down.  Block size = power of 2.
    (let ((bsz (let ((s 1)) (loop while (and (<= (* s 2) width)
                                             (<= (* s 2) height))
                                  do (setf s (* 2 s)))
                  s)))
      (refine-blocks formula pixmap L R B Top width height bsz))
    ;; Step 2: per-pixel subpixel refinement on remaining red pixels.
    (refine-subpixels formula pixmap L R B Top width height
                      max-subpixel-depth)
    pixmap))

;;; --- block pass ----------------------------------------------------------

(defun initial-blocks (w h bsz)
  (loop for py from 0 below h by bsz
        nconc (loop for px from 0 below w by bsz collect (list px py bsz))))

(defun block-all-decided-p (pixmap x-lo y-lo x-hi y-hi)
  (loop for py from y-lo below y-hi
        always (loop for px from x-lo below x-hi
                     always (/= (aref pixmap py px) +pixel-red+))))

(defun %quadrant-children (px-lo py-lo size w h)
  "Return the list of <(x y size/2)> sub-block tuples for SUBDIVIDE."
  (when (> size 1)
    (let ((s/2 (ash size -1))
          (out (list (list px-lo py-lo (max 1 (ash size -1))))))
      (when (< (+ px-lo s/2) w)
        (push (list (+ px-lo s/2) py-lo s/2) out))
      (when (< (+ py-lo s/2) h)
        (push (list px-lo (+ py-lo s/2) s/2) out))
      (when (and (< (+ px-lo s/2) w) (< (+ py-lo s/2) h))
        (push (list (+ px-lo s/2) (+ py-lo s/2) s/2) out))
      out)))

(defun refine-blocks (formula pixmap L R B Top w h bsz)
  "Coarse-to-fine: process all blocks at size BSZ, then BSZ/2, etc., to 1."
  (loop for blocks = (initial-blocks w h bsz)
                   then (loop for (px-lo py-lo size) in blocks
                              nconc (refine-one-block formula pixmap
                                                      px-lo py-lo size
                                                      L R B Top w h))
        while blocks))

(defun refine-one-block (formula pixmap px-lo py-lo size L R B Top w h)
  "Process one block; return a list of children to refine next, or NIL."
  (let ((px-hi (min w (+ px-lo size)))
        (py-hi (min h (+ py-lo size))))
    (when (block-all-decided-p pixmap px-lo py-lo px-hi py-hi)
      (return-from refine-one-block nil))
    (multiple-value-bind (xl xh) (block-bounds-x px-lo px-hi w L R)
      (multiple-value-bind (yl yh) (block-bounds-y py-lo py-hi h B Top)
        (case (decide-cell formula xl xh yl yh)
          (:black (fill-block pixmap px-lo px-hi py-lo py-hi +pixel-black+) nil)
          (:white (fill-block pixmap px-lo px-hi py-lo py-hi +pixel-white+) nil)
          (otherwise (%quadrant-children px-lo py-lo size w h)))))))

;;; --- subpixel pass -------------------------------------------------------

(defun refine-subpixels (formula pixmap L R B Top w h max-depth)
  (loop for py from 0 below h do
    (loop for px from 0 below w do
      (when (= (aref pixmap py px) +pixel-red+)
        (multiple-value-bind (xl xh) (pixel-bounds-x px w L R)
          (multiple-value-bind (yl yh) (pixel-bounds-y py h B Top)
            (setf (aref pixmap py px)
                  (subpixel-decide formula xl xh yl yh max-depth))))))))

(defun subpixel-decide (formula xl xh yl yh depth)
  "Recursively probe a single pixel; returns +pixel-black+ / +pixel-white+ / +pixel-red+.
   At the subpixel level an IVT existence proof IS sufficient to colour the
   pixel black (the rectangle is small enough to localize the solution)."
  (let ((decision (decide-cell formula xl xh yl yh)))
    (case decision
      (:black +pixel-black+)
      (:white +pixel-white+)
      (:ivt   +pixel-black+)
      (otherwise
       (if (<= depth 0)
           +pixel-red+
           (let* ((xm (* 0.5d0 (+ xl xh)))
                  (ym (* 0.5d0 (+ yl yh)))
                  (results
                    (list (subpixel-decide formula xl xm yl ym (1- depth))
                          (subpixel-decide formula xm xh yl ym (1- depth))
                          (subpixel-decide formula xl xm ym yh (1- depth))
                          (subpixel-decide formula xm xh ym yh (1- depth)))))
             (cond ((some (lambda (c) (= c +pixel-black+)) results) +pixel-black+)
                   ((every (lambda (c) (= c +pixel-white+)) results) +pixel-white+)
                   (t +pixel-red+))))))))
