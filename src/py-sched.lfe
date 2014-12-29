;;;; Schedulers for selecting the Python server on which to execute the next
;;;; call.
;;;;
;;;; Note there is an implicit "protocol" in this module (as well as the
;;;; py-sup module functions that are called here, e.g., (get-child-* ...)):
;;;;
;;;; * All calls that get children return a list of 2-tuples where the
;;;;   first element is the data queried and the second element is the
;;;;   pid of the child (python server process). This makes meaningful
;;;;   sorting trivial.
;;;;
;;;; * Sorting should place the desired data point in the 1st position
;;;;   of the resultant list.
;;;;
;;;; * If the above are followed, the function (select-pid data), where
;;;;   data is the list of tuples, should work without issue.
;;;;
;;;; * The function (get-next-pid) calls the configured scheduler and it
;;;;   expects that the scheduler will return the name for a process, which
;;;;   it then uses to lookup the PID.
;;;;
(defmodule py-sched
  (export all))

(defun get-next-pid ()
  (whereis (py-config:call-scheduler)))

(defun get-first ()
  "Always get the first PID.

  Don't even bother with the others. Ever. King of imbecile schedulers."
  (select-pid (py-sup:get-children)))

(defun random ()
  "Select a random Python server PID."
  (let ((index (random:uniform (py-config:get-worker-count))))
    (select-pid `(,(lists:nth index (py-sup:get-children))))))

(defun least-reductions ()
  "Select the Python server PID with the least reductions."
  (select-pid (lists:sort (py-sup:get-child-reductions))))

(defun least-messages-in ()
  "Select the Python server PID with the least messages in."
  (select-pid (lists:sort (py-sup:get-child-messages-in))))

(defun youngest ()
  "Select the Python server PID that has been running for the shortest time."
  (select-pid (lists:reverse (lists:sort (py-sup:get-child-start-time)))))

(defun oldest ()
  "Select the Python server PID that has been running for the longest time."
  (select-pid (lists:sort (py-sup:get-child-start-time))))

(defun select-pid (data)
  (element 2 (car data)))
