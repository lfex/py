;;;; This inlcude wraps functions provided by the Python cmath module, which
;;;; in turn provide access to mathematical functions for complex numbers.
;;;; The functions in this module accept integers, floating-point numbers
;;;; or complex numbers as arguments. They will also accept any Python
;;;; object that has either a __complex__() or a __float__() method: these
;;;; methods are used to convert the object to a complex or floating-point
;;;; number, respectively, and the function is then applied to the result
;;;; of the conversion.
(eval-when-compile

  (defun get-cmath-funcs ()
    '(;; Conversions to and from polar coordinates
      (phase 1)
      (polar 1)
      (rect 2)
      ;; Power and logarithmic functions
      (exp 1)
      (log 1) (log 2)
      (log10 1)
      (sqrt 1)
      ;; Trigonometric functions
      (acos 1)
      (asin 1)
      (atan 1)
      (cos 1)
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
      ;; Classification functions
      (isfinite 1)
      (isinf 1)
      (isnan 1)))

  (defun get-builtins-funcs ()
    '((complex 1)
      (complex 2))))

(defmacro generate-cmath-api ()
  `(progn ,@(lsci-util:make-funcs (get-cmath-funcs) 'cmath)))

(defmacro generate-builtins-api ()
  `(progn ,@(lsci-util:make-funcs (get-builtins-funcs) 'builtins)))

(generate-cmath-api)
(generate-builtins-api)

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
