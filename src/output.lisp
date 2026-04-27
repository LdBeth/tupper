;;;; output.lisp --- write the pixmap as a PPM via cl-netpbm.
(in-package #:tupper)

(defun pixel-rgb (byte)
  "Map a +pixel-*+ byte to an (R G B) vector."
  (declare (type (unsigned-byte 8) byte))
  (cond ((= byte +pixel-black+) (vector   0   0   0))
        ((= byte +pixel-white+) (vector 255 255 255))
        ((= byte +pixel-red+)   (vector 220  60  60))
        (t                      (vector 128 128 128))))

(defun save-ppm (pixmap path)
  "PIXMAP is (H x W) (unsigned-byte 8) array.  Writes PPM with y-up flip."
  (let* ((h (array-dimension pixmap 0))
         (w (array-dimension pixmap 1))
         (rgb (make-array (list h w))))
    (dotimes (y h)
      (dotimes (x w)
        ;; y-up convention -> flip: PPM row 0 is top.
        (setf (aref rgb (- h 1 y) x) (pixel-rgb (aref pixmap y x)))))
    (netpbm:write-to-file path rgb
                          :format :ppm
                          :encoding :binary
                          :maximum-value 255)
    path))
