# tupper

A Common Lisp / SBCL implementation of Jeff Tupper's *Reliable Two-Dimensional
Graphing Methods for Mathematical Formulae with Two Free Variables*
(SIGGRAPH 2001), through Algorithm 3.1 (subpixel computation + continuity
tracking + interval sets).

Every output pixel is **black**, **white**, or **red** with these
guarantees:

- **white** — formula has no solutions in the pixel
- **black** — formula has at least one solution in the pixel
- **red** — undecided

## Install

Drop the `tupper/` directory under `~/quicklisp/local-projects/` (or
`~/.common-lisp/quicklisp/local-projects/`) and:

```lisp
(ql:quickload :tupper)
(tupper:demo)            ;; renders the 5 examples to ./out/*.ppm
```

## Formula syntax

S-expressions:

```
(= LHS RHS) (< ...) (<= ...) (> ...) (>= ...) (/= ...)
(and F1 F2 ...) (or F1 F2 ...) (not F)
```

Expressions: `x`, `y`, numeric literals, `+ - * / ^`, `sqrt`, `abs`, `sin`,
`cos`, `tan`, `log`, `exp`.

```lisp
(tupper:graph-formula '(= y (- (^ x 2) 1/3))
                      -1d0 1d0 -1d0 1d0  256 256)
```

## Notes

- Implements through Algorithm 3.1.  Branch-cut tracking (3.2) and
  exponentiation parity tagging (3.3) are stubbed.
- Uses SBCL's IEEE 754 directed-rounding controls; transcendental results
  are widened by 1 ULP for safety since libm rounding is not guaranteed.
- The Step 8 IVT-based existence proof is included for equations.

## Tests

```lisp
(ql:quickload :tupper)
(load "tests/test-interval.lisp")
(tupper::run-tests)
```
