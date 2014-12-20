;;;; The functions wrapped in this inlcude are from the following source:
;;;;
;;;;  * http://docs.scipy.org/doc/scipy-dev/reference/index.html#reference
;;;;
(eval-when-compile

  (defun get-sp-funcs ()
    '(;; scipy.cluster
      ;; scipy.constants
      ;; Discrete Fourier transforms (scipy.fftpack)
      ;; Integration and ODEs (scipy.integrate)
      ;; Interpolation (scipy.interpolate)
      ;; Input and output (scipy.io)
      ;; Linear algebra (scipy.linalg)
      ;; Miscellaneous routines (scipy.misc)
      ;; Multi-dimensional image processing (scipy.ndimage)
      ;; Orthogonal distance regression (scipy.odr)
      ;; Optimization and root finding (scipy.optimize)
      ;; Signal processing (scipy.signal)
      ;; Sparse matrices (scipy.sparse)
      ;; Sparse linear algebra (scipy.sparse.linalg)
      ;; Compressed Sparse Graph Routines (scipy.sparse.csgraph)
      ;; Spatial algorithms and data structures (scipy.spatial)
      ;; Special functions (scipy.special)
      ;; Statistical functions (scipy.stats)
      ;; Statistical functions for masked arrays (scipy.stats.mstats)
      ;; C/C++ integration (scipy.weave)
      )))

(defmacro generate-sp-api ()
  `(progn ,@(lsci-util:make-funcs (get-sp-funcs) 'scipy)))

(generate-sp-api)

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
