;;;; This inlcude wraps functions provided by the Python math module, which
;;;; in turn provide access to the mathematical functions defined by the
;;;; C standard.
;;;;
;;;; These functions cannot be used with complex numbers; use the functions
;;;; of the same name from the cmath module if you require support for
;;;; complex numbers. The distinction between functions which support
;;;; complex numbers and those which don’t is made since most users do not
;;;; want to learn quite as much mathematics as required to understand
;;;; complex numbers. Receiving an exception instead of a complex result
;;;; allows earlier detection of the unexpected complex number used as a
;;;; parameter, so that the programmer can determine how and why it was
;;;; generated in the first place.
;;;;
;;;; The following functions are provided by this module. Except when
;;;; explicitly noted otherwise, all return values are floats.
(eval-when-compile

  (defun get-math-funcs ()
    '(;; Number-theoretic and representation functions
      (ceil 1)
      (copysign 2)
      (fabs 1)
      (factorial 1)
      (floor 1)
      (fmod 2)
      (frexp 1)
      (fsum 1)
      (isfinite 1)
      (isinf 1)
      (isnan 1)
      (ldexp 2)
      (modf 1)
      (trunc 1)
      ;; Power and logarithmic functions
      (exp 1)
      (expm1 1)
      (log 1) (log 2)
      (log1p 1)
      (log2 1)
      (log10 1)
      (pow 2)
      (sqrt 1)
      ;; Trigonometric functions
      (acos 1)
      (asin 1)
      (atan 1)
      (atan2 2)
      (cos 1)
      (hypot 2)
      (sin 1)
      (tan 1)
      ;; Angular conversion
      (degrees 1)
      (radians 1)
      ;; Hyperbolic functions
      (acosh 1)
      (asinh 1)
      (atanh 1)
      (cosh 1)
      (sinh 1)
      (tanh 1)
      ;; Special functions
      (erf 1)
      (erfc 1)
      (gamma 1)
      (lgamma 1)))

  (defun get-builtins-funcs ()
    '((abs 1)
      (divmod 2)
      (float 1)
      (min 1)
      (max 1)
      (sum 1)
      (pow 3)
      (round 1)
      (round 2)))

  (defun get-operator-funcs ()
    '((add 2)
      (mul 2)
      (sub 2)
      (truediv 2)
      (floordiv 2)
      (mod 2)
      (neg 1)
      (pos 1))))

(defmacro generate-math-api ()
  `(progn ,@(lsci-util:make-funcs (get-math-funcs) 'math)))

(defmacro generate-builtins-api ()
  `(progn ,@(lsci-util:make-funcs (get-builtins-funcs) 'builtins)))

(defmacro generate-operator-api ()
  `(progn ,@(lsci-util:make-funcs (get-operator-funcs) 'operator)))

(generate-math-api)
(generate-builtins-api)
(generate-operator-api)

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'loaded)
