(defmodule lsci-sp
  (export all))

(include-lib "lsci/include/sp.lfe")

(defun version ()
  (lsci-py:const 'scipy '__version__ 'str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrappers - the following functions couldn't be simply wrapped with the
;;;            same macros that wrap most of the other SciPy functions due to
;;;            the fact that these needed some sort of special handling.
;;;

;; Interpolation
;;
(defun interpolate.interp1d (x y)
  (lsci-py:func-call 'scipy.interpolate
                     'interp1d
                     `(,x ,y)
                     '()))

(defun interpolate.interp1d (x y kwargs)
  (lsci-py:func-call 'scipy.interpolate
                     'interp1d
                     `(,x ,y)
                     kwargs))
