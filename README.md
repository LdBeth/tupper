# tupper

A Common Lisp / SBCL implementation of Jeff Tupper's *Reliable Two-Dimensional
Graphing Methods for Mathematical Formulae with Two Free Variables*
(SIGGRAPH 2001). Implements Algorithm 3.1 (subpixel computation + continuity
tracking + interval sets) and Algorithm 3.2 (branch-cut tracking), including
the paper's Figure 11(a) acceptance target.

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
(tupper:demo)            ;; renders the example gallery to ./out/*.ppm
```

## Formula syntax

S-expressions:

```
(= LHS RHS) (< ...) (<= ...) (> ...) (>= ...) (/= ...)
(and F1 F2 ...) (or F1 F2 ...) (not F)
```

Expressions: `x`, `y`, numeric literals, and:

- arithmetic `+ - * / ^`, `sqrt`, `abs`, `exp`, `log`;
- trig `sin`, `cos`, `tan` and inverse trig `arcsin`/`arccos`/`arctan`
  (also `arccsc`/`arcsec`/`arccot`, with `asin`/… aliases);
- `min`, `max` (n-ary), `median` (3-arg), `floor`, `ceiling`, `round`,
  `truncate`, `sgn`, `mod`;
- `nth-root`, `gamma`, `!`/`factorial`.

```lisp
(tupper:graph-formula '(= y (- (^ x 2) 1/3))
                      -1d0 1d0 -1d0 1d0  256 256)
```

## Notes

- Implements Algorithms 3.1 and 3.2. Exponentiation parity tagging (3.3)
  and common-subexpression elimination (3.4) are hooked but not yet filled
  in; halftoning (paper §13) is deferred as it is not needed for correctness.
- Uses SBCL's IEEE 754 directed-rounding controls; transcendental results
  are widened by 1 ULP for safety since libm rounding is not guaranteed.
- The Step 8 IVT-based existence proof is included for equations.

## Tests

The `tupper/tests` system runs both the core interval / end-to-end reliability
checks and the operator-extension tests:

```sh
sbcl --noinform --non-interactive \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(ql:quickload :tupper/tests :silent t)' \
  --eval '(asdf:test-system :tupper/tests)'
```
