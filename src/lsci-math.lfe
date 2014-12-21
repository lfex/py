(defmodule lsci-math
  (export all))

(include-lib "lsci/include/math.lfe")

(defun pi ()
  (lsci-py:const 'math 'pi 'float))

(defun e ()
  (lsci-py:const 'math 'e 'float))

(defun phi (x)
  "Cumulative distribution function for the standard normal distribution."
  (/ (+ (erf (/ x (sqrt 2.0)))
        1.0)
     2.0))

(defun div (a b)
  (truediv a b))
