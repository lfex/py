(defmodule py-sched
  (export all))

(defun get-next-pid ()
  (py-config:call-scheduler))

(defun get-first ()
  "King of imbecile schedulers.

  Always get the first PID. Don't even bother with the others. Ever."
  (car (py-sup:get-children-pids)))

(defun round-robin ()
  'noop)

(defun least-memory ()
  'noop)

(defun any-not-running ()
  'noop)
