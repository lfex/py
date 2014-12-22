(defmodule lsci-cmath
  (export all))

(include-lib "lsci/include/cmath.lfe")

(defun pi ()
  (lsci-py:const 'cmath 'pi 'float))

(defun e ()
  (lsci-py:const 'cmath 'e 'float))

(defun real (complex-number)
  (lsci-py:attr complex-number 'real))

(defun imag (complex-number)
  (lsci-py:attr complex-number 'imag))

(defun conjugate (complex-number)
  (lsci-py:method complex-number 'conjugate))

(defun conj (complex-number)
  (conjugate complex-number))

(defun ->str (complex-number)
  (lsci-py:const complex-number 'str))
