(defmodule lsci-cmath
  (export all))

(include-lib "lsci/include/cmath.lfe")

(defun pi ()
  (lsci:py-const 'cmath 'pi 'float))

(defun e ()
  (lsci:py-const 'cmath 'e 'float))
