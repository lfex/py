(defmodule lsci-sp
  (export all))

(include-lib "lsci/include/sp.lfe")

(defun version ()
  (lsci:py-const 'scipy '__version__ 'str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
