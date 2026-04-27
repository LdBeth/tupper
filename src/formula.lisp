;;;; formula.lisp --- AST evaluator for formulas.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun number->iv (n)
  (make-constant-iv (coerce n 'double-float)))

(defun %n-ary (binop ident args x y)
  "Fold an n-ary arithmetic op: (op a b c ...) = a `binop` b `binop` c ..."
  (cond ((null args) (list (number->iv ident)))
        ((null (cdr args)) (eval-expr (car args) x y))
        (t (reduce (lambda (acc e)
                     (ivs-apply-binary binop acc (eval-expr e x y)))
                   (cdr args)
                   :initial-value (eval-expr (car args) x y)))))

(defun eval-expr (expr x y)
  "EXPR is an s-expression (or X/Y/number).  Returns a list of ivals."
  (cond
    ((numberp expr) (list (number->iv expr)))
    ((eq expr 'x) (list x))
    ((eq expr 'y) (list y))
    ((symbolp expr)
     (error "Unknown variable in expression: ~a" expr))
    ((consp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         (+ (%n-ary #'iv-add 0 args x y))
         (* (%n-ary #'iv-mul 1 args x y))
         (- (cond ((null args) (error "(-) with no args"))
                  ((null (cdr args))
                   (ivs-apply-unary #'iv-neg (eval-expr (car args) x y)))
                  (t (%n-ary #'iv-sub 0 args x y))))
         (/ (ivs-apply-binary #'iv-div
                              (eval-expr (first args) x y)
                              (eval-expr (second args) x y)))
         (^ (ivs-apply-binary #'iv-pow
                              (eval-expr (first args) x y)
                              (eval-expr (second args) x y)))
         (sqrt (ivs-apply-unary #'iv-sqrt (eval-expr (first args) x y)))
         (abs  (ivs-apply-unary #'iv-abs  (eval-expr (first args) x y)))
         (sin  (ivs-apply-unary #'iv-sin  (eval-expr (first args) x y)))
         (cos  (ivs-apply-unary #'iv-cos  (eval-expr (first args) x y)))
         (tan  (ivs-apply-unary #'iv-tan  (eval-expr (first args) x y)))
         (log  (ivs-apply-unary #'iv-log  (eval-expr (first args) x y)))
         (exp  (ivs-apply-unary #'iv-exp  (eval-expr (first args) x y)))
         (t (error "Unknown operator: ~a" op)))))
    (t (error "Bad expression: ~a" expr))))

;;; --- comparison of two interval sets -------------------------------------

(defun cmp-pair (op li ri)
  "Compare two ivals.  Returns a bool-iv.  An undefined operand makes the
   comparison FF (per Tupper's semantics)."
  (cond
    ((or (ival-totally-undefined-p li) (ival-totally-undefined-p ri))
     :ff)
    ((or (not (ival-def-lo li)) (not (ival-def-lo ri)))
     ;; possibly undefined -> upper bound only
     (let ((upper (cmp-pair-bool op li ri)))
       (case upper (:ff :ff) (t :ft))))
    (t (cmp-pair-bool op li ri))))

(defun cmp-pair-bool (op li ls)
  (let ((al (ival-lo li)) (ah (ival-hi li))
        (bl (ival-lo ls)) (bh (ival-hi ls)))
    (case op
      ((<)  (cond ((< ah bl) :tt)
                  ((>= al bh) :ff)
                  (t :ft)))
      ((<=) (cond ((<= ah bl) :tt)
                  ((>  al bh) :ff)
                  (t :ft)))
      ((>)  (cond ((> al bh) :tt)
                  ((<= ah bl) :ff)
                  (t :ft)))
      ((>=) (cond ((>= al bh) :tt)
                  ((<  ah bl) :ff)
                  (t :ft)))
      ((=)  (cond ((and (= al ah) (= bl bh) (= al bl)) :tt)
                  ((or (< ah bl) (> al bh)) :ff)
                  (t :ft)))
      ((/=) (cond ((or (< ah bl) (> al bh)) :tt)
                  ((and (= al ah) (= bl bh) (= al bl)) :ff)
                  (t :ft))))))

(defun cmp-sets (op as bs)
  "Compare interval-sets pairwise, OR-ing the bool-iv across all pairs."
  (let ((acc :ff))
    (dolist (a as)
      (dolist (b bs)
        (setf acc (bool-or acc (cmp-pair op a b)))))
    acc))

;;; --- formula evaluator ---------------------------------------------------

(defun eval-formula (formula x y)
  "FORMULA is (op LHS RHS), (and ...), (or ...), (not f).  Returns bool-iv."
  (cond
    ((not (consp formula))
     (error "Bad formula: ~a" formula))
    (t
     (let ((op (car formula)))
       (case op
         ((< <= > >= = /=)
          (cmp-sets op
                    (eval-expr (second formula) x y)
                    (eval-expr (third  formula) x y)))
         ((and)
          (reduce #'bool-and (mapcar (lambda (f) (eval-formula f x y))
                                     (cdr formula))
                  :initial-value :tt))
         ((or)
          (reduce #'bool-or (mapcar (lambda (f) (eval-formula f x y))
                                    (cdr formula))
                  :initial-value :ff))
         ((not)
          (bool-not (eval-formula (second formula) x y)))
         (t (error "Unknown formula head: ~a" op)))))))

;;; --- equation IVT proof helpers (Step 8) --------------------------------
;;; For a formula of form (= L R), evaluate (L - R) at sample points; if
;;; any pair has opposite signs and both are well-defined, return T.

(defun formula-equation-p (formula)
  (and (consp formula) (eq (car formula) '=)))

(defun eval-expr-point (expr x y)
  "Evaluate EXPR at the *single point* (x, y) by passing degenerate ivals.
   Returns a single double-float or NIL if undefined/non-finite."
  (let* ((xi (make-constant-iv x))
         (yi (make-constant-iv y))
         (ivs (handler-case (eval-expr expr xi yi)
                (error () nil))))
    (when (and ivs (= (length ivs) 1))
      (let ((iv (car ivs)))
        (when (and (ival-totally-defined-p iv)
                   (not (= (ival-lo iv) +neg-inf+))
                   (not (= (ival-hi iv) +pos-inf+)))
          (* 0.5d0 (+ (ival-lo iv) (ival-hi iv))))))))

(defun ivt-proves-equation-p (formula x-lo x-hi y-lo y-hi)
  "Sample (L - R) at the four corners of the box; if signs differ and both
   evaluations are well-defined, IVT guarantees a zero in the box."
  (when (formula-equation-p formula)
    (let* ((diff `(- ,(second formula) ,(third formula)))
           (samples
            (loop for (xx yy) in (list (list x-lo y-lo) (list x-lo y-hi)
                                       (list x-hi y-lo) (list x-hi y-hi))
                  for v = (eval-expr-point diff xx yy)
                  when v collect v)))
      (and (>= (length samples) 2)
           (some #'plusp samples)
           (some #'minusp samples)))))
