(defmodule py-sched
  (export all))

(defun get-next-pid ()
  (car (py:get-pids)))
