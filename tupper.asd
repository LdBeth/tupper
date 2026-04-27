;;;; tupper.asd
(asdf:defsystem #:tupper
  :description "Tupper's reliable 2D graphing algorithm (SIGGRAPH 2001) in SBCL."
  :author "plotter"
  :license "MIT"
  :depends-on (#:cl-netpbm)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "rounding")
                             (:file "interval")
                             (:file "boolean-interval")
                             (:file "interval-set")
                             (:file "operators")
                             (:file "formula")
                             (:file "graph")
                             (:file "output")
                             (:file "main")))))
