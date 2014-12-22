;;;; The functions wrapped in this inlcude are from the following source:
;;;;
;;;;  * http://docs.scipy.org/doc/numpy/reference/routines.html
;;;;
(eval-when-compile

  (defun get-np-create-funcs ()
    '(;; Array creation routines - Ones and zeros
      (empty 1) (empty 2) (empty 3)
      (empty_like 1) (empty_like 2) (empty_like 3) (empty_like 4)
      (eye 1) (eye 2) (eye 3) (eye 4)
      (identity 1) (identity 2)
      (ones 1) (ones 2) (ones 3)
      (ones_like 1) (ones_like 2) (ones_like 3) (ones_like 4)
      (zeros 1) (zeros 2) (zeros 3)
      (zeros_like 1) (zeros_like 2) (zeros_like 3) (zeros_like 4)
      (full 2) (full 3)
      (full_like 2) (full_like 3) (full_like 4) (full_like 5)
      ;; Array creation routines - From existing data
      (array 1) (array 2) (array 3) (array 4) (array 5) (array 6)
      (asarray 1) (asarray 2) (asarray 3)
      (asanyarray 1) (asanyarray 2) (asanyarray 3)
      (ascontiguousarray 1) (ascontiguousarray 2)
      (asmatrix 1) (asmatrix 2)
      (copy 1) (copy 2)
      (frombuffer 1) (frombuffer 2) (frombuffer 3) (frombuffer 4)
      (fromfile 1) (fromfile 2) (fromfile 3) (fromfile 4)
      (fromfunction 2) (fromfunction 3)
      (fromiter 2) (fromiter 3)
      (fromstring 1) (fromstring 2) (fromstring 3) (fromstring 4)
      ;; Array creation routines - Creating record arrays
      ;; Array creation routines - Creating character arrays
      ;; Array creation routines - Numerical ranges
      (arange 1) (arange 2) (arange 3) (arange 4)
      (linspace 2)
      ;; Array creation routines - Building matrices
      ;; Array creation routines - The Matrix class
      ))

  (defun get-np-manip-funcs ()
    '(;; Array manipulation routines - Changing array shape
      ;; Array manipulation routines - Transpose-like operations
      ;; Array manipulation routines - Changing number of dimensions
      ;; Array manipulation routines - Changing kind of array
      ;; Array manipulation routines - Joining arrays
      ;; Array manipulation routines - Splitting arrays
      ;; Array manipulation routines - Tiling arrays
      ;; Array manipulation routines - Adding and removing elements
      ;; Array manipulation routines - Rearranging elements
      ))

  (defun get-np-math-funcs ()
    '(;; Mathematical functions - Trigonometric functions
      (sin 1) (sin 2)
      (cos 1) (cos 2)
      (tan 1) (tan 2)
      (arcsin 1) (arcsin 2)
      (arccos 1) (arccos 2)
      (arctan 1) (arctan 2)
      (arctan2 2) (arctan2 3)
      (hypot 2) (hypot 3)
      (degrees 1) (degrees 2)
      (radians 1) (radians 2)
      (unwrap 1) (unwrap 2) (unwrap 3)
      (deg2rad 1) (deg2rad 2)
      (rad2deg 1) (rad2deg 2)
      ;; Mathematical functions - Hyperbolic functions
      ;; Mathematical functions - Rounding
      ;; Mathematical functions - Sums, products, differences
      ;; Mathematical functions - Exponents and logarithms
      (exp 1) (exp 2)
      (expm1 1) (expm1 2)
      (exp2 1) (exp2 2)
      (log 1) (log 2)
      (log10 1) (log10 2)
      (log2 1) (log2 2)
      (log1p 1) (log1p 2)
      (logaddexp 2) (logaddexp 3)
      (logaddexp2 2) (logaddexp2 3)
      ;; Mathematical functions - Other special functions
      (i0 1)
      (sinc 1)
      ;; Mathematical functions - Floating point routines
      ;; Mathematical functions - Arithmetic operations
      (add 2) (add 3)
      (reciprocal 1) (reciprocal 2)
      (negative 1) (negative 2)
      (multiply 2) (multiply 3)
      (divide 2) (divide 3)
      (power 2) (power 3)
      (subtract 2) (subtract 3)
      (true_divide 2) (true_divide 3)
      (floor_divide 2) (floor_divide 3)
      (fmod 2) (fmod 3)
      (mod 2) (mod 3)
      (modf 1) (modf 3)
      (remainder 2) (remainder 3)
      ;; Mathematical functions - Handling complex numbers
      ;; Mathematical functions - Miscellaneous
      ))

  (defun get-np-other-funcs ()
    '(;; Binary operations
      ;; String operations
      ;; C-Types Foreign Function Interface
      ;; Datetime Support Functions
      ;; Data type routines
      ;; Optionally Scipy-accelerated routines (numpy.dual)
      ;; Mathematical functions with automatic domain (numpy.emath)
      ;; Floating point error handling
      ;; Discrete Fourier Transform (numpy.fft)
      ;; Financial functions
      ;; Functional programming
      ;; Numpy-specific help functions
      ;; Indexing routines
      ;; Input and output
      (load 1) (load 2)
      (save 2)
      (loadtxt 1) (loadtxt 2)
      (savetxt 2) (savetxt 3)
      ;; Linear algebra (numpy.linalg)
      ;; Logic functions
      ;; Masked array operations
      ;; Matrix library (numpy.matlib)
      ;; Miscellaneous routines
      ;; Padding Arrays
      ;; Polynomials
      (polyfit 3)
      (poly1d 1)
      ;; Random sampling (numpy.random)
      ;; Set routines
      ;; Sorting, searching, and counting
      ;; Statistics
      )))

(defmacro generate-np-create-api ()
  `(progn ,@(lsci-util:make-funcs (get-np-create-funcs) 'numpy)))

(defmacro generate-np-manip-api ()
  `(progn ,@(lsci-util:make-funcs (get-np-manip-funcs) 'numpy)))

(defmacro generate-np-math-api ()
  `(progn ,@(lsci-util:make-funcs (get-np-math-funcs) 'numpy)))

(defmacro generate-np-other-api ()
  `(progn ,@(lsci-util:make-funcs (get-np-other-funcs) 'numpy)))

(generate-np-create-api)
(generate-np-manip-api)
(generate-np-math-api)
(generate-np-other-api)

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
