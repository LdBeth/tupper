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
                             (:file "cse")
                             (:file "graph")
                             (:file "output")
                             (:file "main")))))

(asdf:defsystem #:tupper/tests
  :description "Tests for tupper."
  :depends-on (#:tupper)
  :serial t
  :components ((:module "tests"
                :components ((:file "test-interval")
                             (:file "test-operators"))))
  :perform (asdf:test-op (op c)
             (let ((a (funcall (read-from-string "tupper::run-tests")))
                   (b (funcall (read-from-string "tupper::run-operator-tests"))))
               (unless (and a b)
                 (error "Tests failed.")))))
