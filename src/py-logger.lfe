(defmodule py-logger
  (export all))

(include-lib "logjam/include/logjam.lfe")

(defun setup ()
  (logjam:setup))
