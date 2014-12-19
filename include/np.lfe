;;;; The functions wrapped in this inlcude are from the following sources:
;;;;
;;;;  * http://docs.scipy.org/doc/numpy/reference/routines.array-creation.html
;;;;  *
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
      ;; Array creation routines - Creating record arrays
      ;; Array creation routines - Creating character arrays
      ;; Array creation routines - Numerical ranges
      ;; Array creation routines - Building matrices
      ;; Array creation routines - The Matrix class
      )))

(defmacro generate-np-create-api ()
  `(progn ,@(lsci-util:make-funcs (get-np-create-funcs) 'numpy)))

(generate-np-create-api)

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'loaded)
