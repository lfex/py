(defmodule py-sup
  (behaviour e2_task_supervisor)
  (export all))

(defun start_link ()
  (e2_task_supervisor:start_link
    (MODULE) 'mydb-client-handler '(registered)))

(defun start_handler (socket)
  (e2_task_supervisor:start_task (MODULE) `(,socket)))
