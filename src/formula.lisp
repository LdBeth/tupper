;;;; formula.lisp --- AST evaluator for formulas.
(in-package #:tupper)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun number->iv (n)
  (make-constant-iv (coerce n 'double-float)))

(defun %n-ary (binop ident args x y)
  "Fold an n-ary op left-to-right.  IDENT is the value of (op) with no args;
   pass NIL to require at least one arg (used by min/max which have no
   identity element on intervals)."
  (cond ((null args)
         (if ident
             (list (number->iv ident))
             (error "n-ary op with no identity needs >=1 arg")))
        ((null (cdr args)) (eval-expr (car args) x y))
        (t (reduce (lambda (acc e)
                     (ivs-apply-binary binop acc (eval-expr e x y)))
                   (cdr args)
                   :initial-value (eval-expr (car args) x y)))))

;;; Operator dispatch tables.  Each entry is (SYMBOL . #'iv-fn).  Adding a
;;; new operator typically means appending one row to the appropriate table.
(defparameter *unary-ops*
  `((sqrt     . ,#'iv-sqrt)  (abs      . ,#'iv-abs)
    (sin      . ,#'iv-sin)   (cos      . ,#'iv-cos)   (tan . ,#'iv-tan)
    (log      . ,#'iv-log)   (exp      . ,#'iv-exp)
    (floor    . ,#'iv-floor) (ceiling  . ,#'iv-ceil)
    (round    . ,#'iv-round) (truncate . ,#'iv-trunc)
    (sgn      . ,#'iv-sgn)
    ;; Group D: inverse trig principal branches; a*/arc* are aliases.
    (arcsin   . ,#'iv-arcsin) (arccos . ,#'iv-arccos)
    (arctan   . ,#'iv-arctan) (arccsc . ,#'iv-arccsc)
    (arcsec   . ,#'iv-arcsec) (arccot . ,#'iv-arccot)
    (asin     . ,#'iv-arcsin) (acos   . ,#'iv-arccos)
    (atan     . ,#'iv-arctan) (acsc   . ,#'iv-arccsc)
    (asec     . ,#'iv-arcsec) (acot   . ,#'iv-arccot)
    ;; Group F: special functions
    (gamma    . ,#'iv-gamma)
    (factorial . ,#'iv-factorial)
    (!        . ,#'iv-factorial)))

(defparameter *binary-ops*
  `((/        . ,#'iv-div)
    (mod      . ,#'iv-mod)))

;;; ^ / expt and nth-root use a special dispatch path below: the parser
;;; peeks at the literal exponent / N to extract parity / integer info
;;; before routing through ivs-apply-binary.

(defun %literal-rational-parity (expr)
  "If EXPR is a rational literal or (/ P Q) form, return plist
   (:num P :den Q :num-odd ON :den-odd OD); else NIL."
  (cond
    ((and (rationalp expr) (not (floatp expr)))
     (let ((num (numerator expr)) (den (denominator expr)))
       (list :num num :den den
             :num-odd (oddp num) :den-odd (oddp den))))
    ((and (consp expr) (eq (car expr) '/)
          (= 3 (length expr))
          (integerp (second expr)) (integerp (third expr))
          (not (zerop (third expr))))
     (let ((num (second expr)) (den (third expr)))
       (list :num num :den den
             :num-odd (oddp num) :den-odd (oddp den))))
    (t nil)))

(defun eval-expr (expr x y)
  "EXPR is an s-expression (or X/Y/number).  Returns a list of ivals."
  (cond
    ((numberp expr) (list (number->iv expr)))
    ((eq expr 'x) (list x))
    ((eq expr 'y) (list y))
    ((symbolp expr)
     (error "Unknown variable in expression: ~a" expr))
    ((consp expr)
     (let* ((op (car expr)) (args (cdr expr))
            (u (cdr (assoc op *unary-ops*)))
            (b (cdr (assoc op *binary-ops*))))
       (cond
         (u (ivs-apply-unary u (eval-expr (first args) x y)))
         (b (ivs-apply-binary b
                              (eval-expr (first args) x y)
                              (eval-expr (second args) x y)))
         (t
          (case op
            (+ (%n-ary #'iv-add 0 args x y))
            (* (%n-ary #'iv-mul 1 args x y))
            (- (cond ((null args) (error "(-) with no args"))
                     ((null (cdr args))
                      (ivs-apply-unary #'iv-neg (eval-expr (car args) x y)))
                     (t (%n-ary #'iv-sub 0 args x y))))
            (min (%n-ary #'iv-min nil args x y))
            (max (%n-ary #'iv-max nil args x y))
            (median (unless (= 3 (length args))
                      (error "median expects exactly 3 args, got ~a"
                             (length args)))
                    (ivs-apply-ternary #'iv-median
                                       (eval-expr (first args) x y)
                                       (eval-expr (second args) x y)
                                       (eval-expr (third args) x y)))
            ;; ^ / expt: parser-side parity tag for rational exponents.  If
            ;; the exponent is a literal *non-integer* rational p/q the
            ;; AST-side helper extracts (:num :den :num-odd :den-odd);
            ;; iv-pow uses that to evaluate exactly on negative bases
            ;; (Algorithm 3.3 lite).  For integer exponents we fall through
            ;; to the existing iv-pow path (parity ignored) -- this preserves
            ;; bit-identity for the reference examples.
            ((^ expt)
             (let* ((base-set (eval-expr (first args)  x y))
                    (exp-set  (eval-expr (second args) x y))
                    (parity   (%literal-rational-parity (second args))))
               (cond
                 ((and parity (> (getf parity :den) 1))
                  (ivs-apply-binary
                   (lambda (a b) (iv-pow a b parity))
                   base-set exp-set))
                 (t
                  (ivs-apply-binary #'iv-pow base-set exp-set)))))
            ;; nth-root takes a literal integer N and one ival X.
            (nth-root
             (unless (and (= 2 (length args)) (integerp (first args)))
               (error "nth-root expects (nth-root N X) with integer N"))
             (ivs-apply-unary
              (let ((n (first args)))
                (lambda (iv) (iv-nth-root n iv)))
              (eval-expr (second args) x y)))
            (t (error "Unknown operator: ~a" op)))))))
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
          (bool-and* (mapcar (lambda (f) (eval-formula f x y))
                             (cdr formula))))
         ((or)
          (bool-or* (mapcar (lambda (f) (eval-formula f x y))
                            (cdr formula))))
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
