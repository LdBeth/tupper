;;;; package.lisp
(defpackage #:tupper
  (:use #:cl)
  (:export #:demo
           #:graph-formula
           #:save-ppm
           #:ival #:make-ival
           #:eval-formula
           #:assign-sites
           #:+pixel-black+
           #:+pixel-white+
           #:+pixel-red+
           #:^
           #:x
           #:y))
