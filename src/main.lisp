;;;; main.lisp --- CLI entry point + canonical examples.
(in-package #:tupper)

(defparameter *examples*
  '(("parabola"  (= y (- (^ x 2) (/ 1 3)))
                 -1d0 1d0 -1d0 1d0  256 256 0)
    ("sqrt-ineq" (< y (sqrt x))
                 -1d0 1d0 -1d0 1d0  256 256 0)
    ("recip"     (= y (/ 1 x))
                 -4d0 7d0 -4d0 7d0  384 384 0)
    ("tupper-eq" (> (* (- y 5) (cos (* 4 (sqrt (+ (^ (- x 4) 2) (^ y 2))))))
                    (* x (sin (* 2 (sqrt (+ (^ x 2) (^ y 2)))))))
                 -10d0 10d0 -10d0 10d0  512 512 4)
    ("sin=cos"   (= (sin x) (cos y))
                 -10d0 10d0 -10d0 10d0  256 256 0)
    ("fig5a"     (> (- (cos (cos (min (+ (sin x) y) (+ x (sin y)))))
                       (cos (sin (max (+ (sin y) x) (+ y (sin x))))))
                  0)
     -10d0 10d0 -10d0 10d0 512 512 0)
    ("wve"       (= (cos (- 2 (^ y 2)))
                  (* x y (sin (- 1 (^ x 2)))
                   (sin (- 2 (^ y 2)))))
    -10d0 10d0 -10d0 10d0 512 512 4)
    ;; Day-1 extension demos (groups A + B):
    ("floor"     (= y (floor x))
                 -4d0 4d0 -4d0 4d0  256 256 0)
    ("sgn-sin"   (= y (sgn (sin x)))
                 -10d0 10d0 -2d0 2d0  256 128 0)
    ("min-eq"    (= (min x y) 1)
                 -3d0 3d0 -3d0 3d0  256 256 0)
    ;; Day-2 extension demos (groups C + D + F):
    ("arctan"    (= y (arctan x))
                 -5d0 5d0 -5d0 5d0  256 256 0)
    ("cuberoot"  (= y (^ x 1/3))
                 -2d0 3d0 -2d0 3d0  256 256 0)
    ("gamma"     (= y (gamma x))
                 -4d0 5d0 -4d0 5d0  256 256 0)))

(defun run-example (entry &key (out-dir "out/"))
  (destructuring-bind (name formula L R B Top w h depth) entry
    (ensure-directories-exist out-dir)
    (format t "~&[tupper] rendering ~a (~ax~a) ..." name w h)
    (force-output)
    (let* ((t0 (get-internal-real-time))
           (pixmap (graph-formula formula L R B Top w h
                                  :max-subpixel-depth depth))
           (path (format nil "~a~a.ppm" out-dir name)))
      (save-ppm pixmap path)
      (format t " done in ~,2fs -> ~a~%"
              (/ (- (get-internal-real-time) t0)
                 (float internal-time-units-per-second))
              path))))

(defun demo (&key (out-dir "out/"))
  "Run all canonical examples; write PPMs to OUT-DIR."
  (dolist (ex *examples*)
    (run-example ex :out-dir out-dir))
  (format t "~&[tupper] all examples complete.~%"))
